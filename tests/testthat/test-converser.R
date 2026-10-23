test_that("word-boundary object matching prevents false-positive substring matches", {
  test_env <- rlang::new_environment(list(
    x = 1:100,
    i = 42L,
    m = matrix(1:9, 3, 3),
    in_var = "test",
    survey_df = data.frame(age = c(25, 30, NA), score = c(88.5, 92.0, 79.1))
  ))

  res_false_pos <- ConverseR:::fetch_env_context(
    selection = "ggplot(aes(year, revenue)) + geom_point()",
    input = "modify this plot and export data",
    env = test_env
  )
  expect_identical(res_false_pos, character(0))

  res_true_pos <- ConverseR:::fetch_env_context(
    selection = "",
    input = "fit a linear model on survey_df predicting score from age",
    env = test_env
  )
  expect_true(any(grepl("survey_df", res_true_pos, fixed = TRUE)))
  expect_true(any(grepl("Missing values (NAs): age=1", res_true_pos, fixed = TRUE)))
})

test_that("windowed code context truncates huge files while preserving top YAML frontmatter and library() calls", {
  fake_lines <- c(
    "---",
    "title: \"Social Science Study\"",
    "format: html",
    "---",
    "library(tidyverse)",
    "library(fixest)",
    paste0("# line ", 7:500)
  )

  windowed <- ConverseR:::window_document_contents(
    contents = fake_lines,
    start_row = 250L,
    end_row = 255L,
    window_lines = 20L
  )

  expect_lt(length(windowed), 65L)
  expect_true(any(grepl("title: \"Social Science Study\"", windowed, fixed = TRUE)))
  expect_true(any(grepl("library(tidyverse)", windowed, fixed = TRUE)))
  expect_true(any(grepl("library(fixest)", windowed, fixed = TRUE)))
  expect_true(any(grepl("# line 250", windowed, fixed = TRUE)))
})

test_that("S3 describe_object summarizes data.frames (including survey labels & factor levels), lm/glm models, and htest objects cleanly", {
  df <- data.frame(
    a = c(1, 2, NA),
    party = factor(c("Dem", "Ind", "Rep"), levels = c("Dem", "Ind", "Rep"))
  )
  attr(df$a, "label") <- "Respondent Trust Score (1-7)"
  attr(df$a, "labels") <- c("Low Trust" = 1, "High Trust" = 7)

  df_desc <- describe_object(df, "my_df")
  expect_true(any(grepl("my_df: <data.frame [3 rows x 2 cols]>", df_desc, fixed = TRUE)))
  expect_true(any(grepl("Missing values (NAs): a=1", df_desc, fixed = TRUE)))
  expect_true(any(grepl("Factor Levels:", df_desc, fixed = TRUE)))
  expect_true(any(grepl("party: ['Dem', 'Ind', 'Rep']", df_desc, fixed = TRUE)))
  expect_true(any(grepl("Survey Variable & Value Labels", df_desc, fixed = TRUE)))
  expect_true(any(grepl("Respondent Trust Score (1-7)", df_desc, fixed = TRUE)))
  expect_true(any(grepl("1='Low Trust'", df_desc, fixed = TRUE)))

  fit <- lm(mpg ~ wt + hp, data = mtcars)
  fit_desc <- describe_object(fit, "mpg_model")
  expect_true(any(grepl("mpg_model: <lm> formula: mpg ~ wt + hp", fit_desc, fixed = TRUE)))
  expect_true(any(grepl("wt", fit_desc, fixed = TRUE)))

  glm_fit <- glm(am ~ wt, data = mtcars, family = binomial(link = "logit"))
  glm_desc <- describe_object(glm_fit, "am_logit")
  expect_true(any(grepl("family: binomial(link = 'logit')", glm_desc, fixed = TRUE)))

  ttest_obj <- t.test(mpg ~ am, data = mtcars)
  ttest_desc <- describe_object(ttest_obj, "mpg_ttest")
  expect_true(any(grepl("mpg_ttest: <htest>", ttest_desc, fixed = TRUE)))
  expect_true(any(grepl("Welch Two Sample t-test", ttest_desc, fixed = TRUE)))
})

test_that("capture_error_context captures base R errors like iris[1:232] without being overwritten by rlang::last_error()", {
  try(iris[1:232], silent = TRUE)
  err_ctx1 <- ConverseR:::capture_error_context()
  err_ctx2 <- ConverseR:::capture_error_context()

  expect_true(grepl("undefined columns selected", err_ctx1, fixed = TRUE))
  expect_true(grepl("undefined columns selected", err_ctx2, fixed = TRUE))
  expect_false(grepl("Can't show last error", err_ctx2, fixed = TRUE))
})

test_that("sanitize_llm_output strips <thinking> blocks and fixes Quarto chunks", {
  raw_ollama <- "<thinking>\nThe user wants to load mtcars as a tibble.\n</thinking>\n\n```r\ndata <- as_tibble(mtcars)\n```"
  cleaned_r <- sanitize_llm_output(raw_ollama, ext = "r")
  expect_equal(trimws(cleaned_r), "data <- as_tibble(mtcars)")

  raw_qmd <- "Here is the chunk:\n```r\nsummary(mtcars)\n```"
  cleaned_qmd <- sanitize_llm_output(raw_qmd, ext = "qmd", strip_fences = FALSE)
  expect_true(grepl("```{r}", cleaned_qmd, fixed = TRUE))
})

test_that("sanitize_llm_output strips all reasoning tags, thought blocks, and conversational preambles", {
  # 1. <thought> and <reasoning> tags
  raw_thought <- "<thought>\nLet's calculate summary stats.\n</thought>\nres <- summary(df)"
  expect_equal(trimws(sanitize_llm_output(raw_thought, ext = "r")), "res <- summary(df)")

  raw_reasoning <- "<reasoning>\nStep 1: fit model.\n</reasoning>\nfit <- lm(y ~ x, data = d)"
  expect_equal(trimws(sanitize_llm_output(raw_reasoning, ext = "r")), "fit <- lm(y ~ x, data = d)")

  # 2. Markdown fenced reasoning blocks
  raw_fence <- "```thought\nAnalyzing the columns\n```\ndf |> select(a, b)"
  expect_equal(trimws(sanitize_llm_output(raw_fence, ext = "r")), "df |> select(a, b)")

  # 3. Unclosed streaming reasoning tags (suppressed mid-stream)
  unclosed_stream <- "<thought>\nThinking midway through generation..."
  expect_equal(sanitize_llm_output(unclosed_stream, ext = "r"), "")

  # 4. Markdown thought headings & conversational preambles with code fences in pure code mode
  raw_preamble <- "Here is the updated code:\n\n```r\nx <- 1:10\ny <- x * 2\n```\n\nHope that helps!"
  expect_equal(trimws(sanitize_llm_output(raw_preamble, ext = "r", strip_fences = TRUE)), "x <- 1:10\ny <- x * 2")

  # 5. Markdown thought process heading without fences
  raw_md_heading <- "**Thinking Process:**\n1. Use dplyr mutate.\n\ndf |> mutate(z = x + y)"
  expect_equal(trimws(sanitize_llm_output(raw_md_heading, ext = "r", strip_fences = TRUE)), "df |> mutate(z = x + y)")

  # 6. Unclosed streaming code block extracts code after opening fence
  raw_stream_fence <- "Sure! Here is the code:\n```r\np + geom_smooth()"
  expect_equal(trimws(sanitize_llm_output(raw_stream_fence, ext = "r", strip_fences = TRUE)), "p + geom_smooth()")
})

test_that("converser_use_openrouter passes exclude = TRUE for reasoning suppression", {
  opt1 <- converser_use_openrouter(api_key = "test_key", model = "deepseek/deepseek-v4.1-flash", reasoning_effort = "low", persist = FALSE)
  prov1 <- opt1$get_provider()
  expect_equal(prov1@extra_args$reasoning$effort, "low")
  expect_true(isTRUE(prov1@extra_args$reasoning$exclude))

  opt2 <- converser_use_openrouter(api_key = "test_key", model = "deepseek/deepseek-v4.1-flash", reasoning_effort = "none", persist = FALSE)
  prov2 <- opt2$get_provider()
  expect_equal(prov2@extra_args$reasoning$effort, "none")
  expect_true(isTRUE(prov2@extra_args$reasoning$exclude))
})

test_that("extract_code_from_markdown and render_code_diff_html work accurately", {
  md <- "Here is the updated code:\n\n```r\nx <- 1:10\ny <- x * 2\n```\n\nHope that helps!"
  extracted <- extract_code_from_markdown(md)
  expect_equal(extracted, "x <- 1:10\ny <- x * 2")

  diff_html <- as.character(render_code_diff_html("x <- 1:5\ny <- x * 2", extracted))
  expect_true(grepl("rc-diff-del", diff_html, fixed = TRUE))
  expect_true(grepl("rc-diff-add", diff_html, fixed = TRUE))
})

test_that("strip_chunk_fences and is_cursor_inside_chunk prevent breaking chunks", {
  # strip_chunk_fences handles named chunks, chunk options, and nested fences
  expect_equal(strip_chunk_fences("```{r test}\nx <- 1:10\n```"), "x <- 1:10")
  expect_equal(strip_chunk_fences("```{r test, fig.width=7}\nx <- 1:10\n```"), "x <- 1:10")
  expect_equal(strip_chunk_fences("```{r, echo=FALSE}\nx <- 1:10\n```"), "x <- 1:10")
  expect_equal(strip_chunk_fences("```r\nx <- 1:10\n```"), "x <- 1:10")
  expect_equal(strip_chunk_fences("x <- 1:10"), "x <- 1:10")
  expect_equal(strip_chunk_fences("```r\n```{r test}\nx <- 1:10\n```\n```"), "x <- 1:10")

  # extract_code_from_markdown strips nested chunk fences from model output
  nested_md <- "```r\n```{r test}\nx <- 1:10\ny <- x * 2\n```\n```"
  expect_equal(extract_code_from_markdown(nested_md), "x <- 1:10\ny <- x * 2")

  # is_cursor_inside_chunk accurately detects whether row is inside an existing chunk
  doc_lines <- c(
    "# Title",
    "",
    "Prose line 3",
    "```{r chunk1}",
    "x <- 1",
    "y <- 2",
    "```",
    "Prose line 8",
    "```{r chunk2, eval=FALSE}",
    "plot(x, y)",
    "```"
  )

  expect_false(is_cursor_inside_chunk(doc_lines, 3))
  expect_true(is_cursor_inside_chunk(doc_lines, 5))
  expect_true(is_cursor_inside_chunk(doc_lines, 6))
  expect_false(is_cursor_inside_chunk(doc_lines, 8))
  expect_true(is_cursor_inside_chunk(doc_lines, 10))
})

test_that("SKILL.md and bundled .zip skill archives are extracted and injected into system prompt", {
  old_skill <- getOption("converser.skill_text")
  old_file <- getOption("converser.skill_file")
  on.exit(options(converser.skill_text = old_skill, converser.skill_file = old_file), add = TRUE)

  bundle_src <- file.path(tempdir(), "test_skill_bundle")
  dir.create(file.path(bundle_src, "references"), recursive = TRUE, showWarnings = FALSE)
  writeLines("# Lab Statistical Skill\nUse clean regression helpers.", file.path(bundle_src, "SKILL.md"))
  writeLines("clean_lm <- function(df) lm(y ~ x, data = df)", file.path(bundle_src, "references", "helpers.R"))

  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(c(tmp_zip, bundle_src), recursive = TRUE), add = TRUE)

  old_wd <- setwd(bundle_src)
  utils::zip(tmp_zip, files = c("SKILL.md", "references/helpers.R"), flags = "-q")
  setwd(old_wd)

  converser_config(skill_file = tmp_zip)
  sys_prompt <- ConverseR:::construct_system_prompt(context = NULL, mode = "sidebar")

  expect_true(grepl("Lab Statistical Skill", sys_prompt, fixed = TRUE))
  expect_true(grepl("clean_lm <- function", sys_prompt, fixed = TRUE))
  expect_true(grepl("references/helpers.R", sys_prompt, fixed = TRUE))
})

test_that("converser_inspect_config prints and returns structured configuration", {
  old_chat <- getOption("converser.chat")
  on.exit(options(converser.chat = old_chat), add = TRUE)

  dummy_chat <- ellmer::chat_openrouter(
    model = "deepseek/deepseek-v4.1-flash",
    api_key = "dummy_key",
    api_args = list(
      provider = list(data_collection = "deny", zdr = TRUE),
      reasoning = list(effort = "high")
    )
  )
  options(converser.chat = dummy_chat)

  out_lines <- capture.output(cfg <- converser_inspect_config())
  expect_equal(cfg$model, "deepseek/deepseek-v4.1-flash")
  expect_equal(cfg$api_args$reasoning$effort, "high")
  expect_true(any(grepl("deepseek/deepseek-v4.1-flash", out_lines, fixed = TRUE)))
})

test_that("map_interface_mode maps Insert Before and Insert After accurately", {
  expect_equal(ConverseR:::map_interface_mode("Replace"), "replace")
  expect_equal(ConverseR:::map_interface_mode("Insert Before"), "prefix")
  expect_equal(ConverseR:::map_interface_mode("Insert After"), "suffix")
})

test_that("live introspection tools execute safely", {
  help_out <- ConverseR:::tool_search_help("mean", "base")
  expect_true(grepl("Arithmetic Mean", help_out, fixed = TRUE))

  tools_list <- converser_tools()
  expect_length(tools_list, 5L)
})

test_that("non-blocking httpuv sidebar server serves 200 OK HTML, Top 20 OpenRouter Models, Stop button, Help button in Settings, and JSON API", {
  on.exit(converser_stop_sidebar(), add = TRUE)
  url <- ConverseR:::ensure_nonblocking_server()
  expect_true(grepl("^http://127\\.0\\.0\\.1:[0-9]+", url))

  res_html <- ConverseR:::handle_sidebar_http_request(list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET"
  ))
  expect_equal(res_html$status, 200L)
  expect_true(grepl("ConverseR", res_html$body, fixed = TRUE))
  expect_true(grepl("Top 20 OpenRouter Models", res_html$body, fixed = TRUE))
  expect_true(grepl("deepseek/deepseek-v4.1-flash", res_html$body, fixed = TRUE))
  expect_true(grepl("openGuide()", res_html$body, fixed = TRUE))
  expect_true(grepl(">Help</button>", res_html$body, fixed = TRUE))
  expect_true(grepl("stop_btn", res_html$body, fixed = TRUE))
  expect_true(grepl("btn-stop-square", res_html$body, fixed = TRUE))
  expect_false(grepl("cfg_provider_order", res_html$body, fixed = TRUE))
  expect_false(grepl("cfg_style", res_html$body, fixed = TRUE))
  expect_true(grepl("Save as default across R sessions", res_html$body, fixed = TRUE))
  expect_true(grepl("Share Plot", res_html$body, fixed = TRUE))
  expect_true(grepl("Preview Difference", res_html$body, fixed = TRUE))
  expect_true(grepl("Run in Console", res_html$body, fixed = TRUE))
  expect_true(grepl("targetRect.top - boxRect.top + box.scrollTop", res_html$body, fixed = TRUE))
  expect_false(grepl("renderMessages[^{]*\\{[^}]*box\\.scrollTop = box\\.scrollHeight;", res_html$body))
  expect_true(grepl("shouldAutoOpenLastDiff", res_html$body, fixed = TRUE))
  expect_true(grepl("rc-msg-assistant:last-child", res_html$body, fixed = TRUE))

  sys_prompt <- ConverseR:::construct_system_prompt(context = NULL, mode = "sidebar")
  expect_true(grepl("SINGLE ```yaml ... ``` code chunk", sys_prompt, fixed = TRUE))

  res_state <- ConverseR:::handle_sidebar_http_request(list(
    PATH_INFO = "/api/state",
    REQUEST_METHOD = "GET"
  ))
  expect_equal(res_state$status, 200L)
  parsed <- jsonlite::fromJSON(res_state$body)
  expect_true(isTRUE(parsed$ok))
})

test_that("open_nonblocking_diff_review and api_post_chat isolate auto_open_diff to only newest preview diff turn", {
  # 1. Simulate an inline preview diff
  ConverseR:::open_nonblocking_diff_review(
    original = "x <- 1",
    proposed = "x <- 2",
    interface = "replace",
    prompt = "Change 1 to 2"
  )
  ui_history <- ConverseR:::converser_env[["chat_history_ui"]]
  last_turn <- ui_history[[length(ui_history)]]
  expect_true(isTRUE(last_turn$auto_open_diff))

  # 2. Simulate subsequent preview diff clears the older turn's auto_open_diff
  ConverseR:::open_nonblocking_diff_review(
    original = "y <- 10",
    proposed = "y <- 20",
    interface = "replace",
    prompt = "Change 10 to 20"
  )
  ui_history2 <- ConverseR:::converser_env[["chat_history_ui"]]
  expect_null(ui_history2[[2]]$auto_open_diff)
  expect_true(isTRUE(ui_history2[[length(ui_history2)]]$auto_open_diff))

  # 3. Simulate asking a question in sidebar via api_post_chat clears all legacy auto_open_diff flags
  res_chat <- ConverseR:::api_post_chat(list(prompt = "How does this code work?"))
  # Note: chat response will succeed or return error message, but history is updated
  msgs <- ConverseR:::converser_env[["chat_history_ui"]]
  # All older turns should have auto_open_diff NULL
  prior_flags <- unlist(lapply(msgs[seq_len(length(msgs) - 1L)], function(m) m$auto_open_diff))
  expect_length(prior_flags, 0L)
})

test_that("expanded whole-document context (10,000 lines) and .GlobalEnv manifest/objects use session hash caching across turns", {
  ConverseR:::converser_clear_context_cache()
  on.exit(ConverseR:::converser_clear_context_cache(), add = TRUE)

  # 1. Default window_lines = 5000L sends a 3,500-line document in full without truncation
  large_doc <- paste0("# figure code line ", seq_len(3500))
  full_out <- ConverseR:::window_document_contents(
    contents = large_doc,
    start_row = 100L,
    end_row = 100L
  )
  expect_length(full_out, 3500L)

  # 2. Expanded .GlobalEnv mode (include_all = TRUE) + hash caching (cache_context = TRUE)
  test_env <- rlang::new_environment(list(
    survey_df = data.frame(age = c(25, 30, NA), trust = c(1, 5, 7)),
    ols_fit = lm(mpg ~ wt, data = mtcars),
    study_n = 1200L
  ))

  # Turn 1: Even when the user asks a general question without naming any objects,
  # all objects in .GlobalEnv are included in the manifest and described in full!
  env_turn1 <- ConverseR:::fetch_env_context(
    selection = "",
    input = "Are there any missing values in my datasets, and what is the R-squared of my models?",
    env = test_env,
    include_all = TRUE,
    cache_context = TRUE
  )
  env_turn1_str <- paste(env_turn1, collapse = "\n")
  expect_true(grepl("Global Environment (.GlobalEnv) Manifest (3 total objects):", env_turn1_str, fixed = TRUE))
  expect_true(grepl("survey_df: <data.frame [3 rows x 2 cols]>", env_turn1_str, fixed = TRUE))
  expect_true(grepl("ols_fit: <lm> formula: mpg ~ wt", env_turn1_str, fixed = TRUE))

  # Turn 2: When nothing in .GlobalEnv has changed, the manifest and object summaries are cached!
  env_turn2 <- ConverseR:::fetch_env_context(
    selection = "",
    input = "Now write code to plot the residuals.",
    env = test_env,
    include_all = TRUE,
    cache_context = TRUE
  )
  env_turn2_str <- paste(env_turn2, collapse = "\n")
  expect_true(grepl("manifest (3 objects) is unchanged from earlier in this conversation", env_turn2_str, fixed = TRUE))
  expect_true(grepl("Unchanged .GlobalEnv objects already described in full earlier in this conversation (3)", env_turn2_str, fixed = TRUE))
  expect_false(grepl("survey_df: <data.frame [3 rows x 2 cols]>", env_turn2_str, fixed = TRUE))

  # Turn 3: If one object (`survey_df`) is modified in .GlobalEnv, ONLY `survey_df` is re-described!
  test_env$survey_df$new_col <- c(10, 20, 30)
  env_turn3 <- ConverseR:::fetch_env_context(
    selection = "",
    input = "Check the updated dataset.",
    env = test_env,
    include_all = TRUE,
    cache_context = TRUE
  )
  env_turn3_str <- paste(env_turn3, collapse = "\n")
  expect_true(grepl("survey_df: <data.frame [3 rows x 3 cols]>", env_turn3_str, fixed = TRUE))
  expect_true(grepl("Unchanged .GlobalEnv objects already described in full earlier in this conversation (2)", env_turn3_str, fixed = TRUE))
  expect_false(grepl("ols_fit: <lm> formula: mpg ~ wt", env_turn3_str, fixed = TRUE))

  # 3. Incremental document delta: editing 1 line in a 3,500-line script sends only the changed hunk (~7 lines)
  edited_doc <- large_doc
  edited_doc[142] <- "xlab(\"Year (2010-2024)\")"
  delta_out <- ConverseR:::compute_document_delta(large_doc, edited_doc, context_lines = 2L)
  expect_lt(length(delta_out), 10L)
  expect_true(any(grepl("- [old L142] # figure code line 142", delta_out, fixed = TRUE)))
  expect_true(any(grepl("+ [new L142] xlab(\"Year (2010-2024)\")", delta_out, fixed = TRUE)))
})

test_that("detect_whole_document_intent accurately identifies whole-document audit prompts", {
  # Positive matches
  expect_true(ConverseR:::detect_whole_document_intent("Please audit this entire document for styling."))
  expect_true(ConverseR:::detect_whole_document_intent("Check across the manuscript if all figures are consistent."))
  expect_true(ConverseR:::detect_whole_document_intent("Are all models using the same covariates throughout the script?"))
  expect_true(ConverseR:::detect_whole_document_intent("Review all chunks in this file"))
  expect_true(ConverseR:::detect_whole_document_intent("Ensure theme_minimal is consistent across all plots"))

  # Negative matches (localized queries)
  expect_false(ConverseR:::detect_whole_document_intent("How do I filter rows where x > 5?"))
  expect_false(ConverseR:::detect_whole_document_intent("Fix the syntax error on line 42"))
  expect_false(ConverseR:::detect_whole_document_intent("What is the p-value of this test?"))
  expect_false(ConverseR:::detect_whole_document_intent(""))
  expect_false(ConverseR:::detect_whole_document_intent(NULL))
})

test_that("extract_active_chunk_or_window extracts enclosing chunk in Quarto/Rmd and 250-line window in .R", {
  qmd_doc <- c(
    "---",
    "title: \"Research Paper\"",
    "author: \"Jane Doe\"",
    "---",
    "",
    "library(ggplot2)",
    "library(dplyr)",
    "",
    "# Section 1",
    "```{r chunk-setup}",
    "data_path <- 'raw_data.csv'",
    "```",
    "",
    "# Section 2",
    "```{r chunk-model}",
    "# Cursor is here on line 16",
    "fit <- lm(mpg ~ wt + hp, data = mtcars)",
    "summary(fit)",
    "```",
    "",
    "# Section 3",
    "```{r chunk-plot}",
    "ggplot(mtcars, aes(wt, mpg)) + geom_point()",
    "```"
  )

  # When cursor is on line 16 (inside chunk-model)
  extracted <- ConverseR:::extract_active_chunk_or_window(qmd_doc, cursor_line = 16L, ext = "qmd")
  extracted_str <- paste(extracted, collapse = "\n")

  # Includes YAML frontmatter and imports from top of document
  expect_true(grepl("title: \"Research Paper\"", extracted_str, fixed = TRUE))
  expect_true(grepl("library(ggplot2)", extracted_str, fixed = TRUE))
  expect_true(grepl("library(dplyr)", extracted_str, fixed = TRUE))

  # Includes active chunk-model
  expect_true(grepl("```{r chunk-model}", extracted_str, fixed = TRUE))
  expect_true(grepl("fit <- lm(mpg ~ wt + hp, data = mtcars)", extracted_str, fixed = TRUE))
  expect_true(grepl("summary(fit)", extracted_str, fixed = TRUE))

  # Does NOT include other chunks
  expect_false(grepl("data_path <- 'raw_data.csv'", extracted_str, fixed = TRUE))
  expect_false(grepl("ggplot(mtcars, aes(wt, mpg))", extracted_str, fixed = TRUE))

  # In plain .R file with 500 lines: window centered at 300 with backup_window = 125 gives 251 lines + 2 omission markers
  r_script <- paste0("line_", seq_len(500))
  r_extracted <- ConverseR:::extract_active_chunk_or_window(r_script, cursor_line = 300L, ext = "r", backup_window = 125L)
  expect_length(r_extracted, 253L)
  expect_equal(r_extracted[1], "# [... omitted lines 1 to 174 ...]")
  expect_equal(r_extracted[2], "line_175")
  expect_equal(r_extracted[252], "line_425")
  expect_equal(r_extracted[253], "# [... omitted lines 426 to 500 ...]")
})

test_that("fetch_env_context caps unmentioned objects to top 2-3 active datasets/models (Option E1)", {
  ConverseR:::converser_clear_context_cache()
  on.exit(ConverseR:::converser_clear_context_cache(), add = TRUE)

  # Create an environment with 6 objects: 4 data frames, 1 model, 1 scalar
  test_env <- rlang::new_environment(list(
    df1 = data.frame(a = 1:5),
    df2 = data.frame(b = 1:5),
    df3 = data.frame(c = 1:5),
    df4 = data.frame(d = 1:5),
    m1 = lm(mpg ~ wt, data = mtcars),
    n_sample = 500L
  ))

  # Query that does NOT mention any specific object:
  # Full manifest should list all 6 objects
  # But detailed summaries should be capped at at most 3 unmentioned objects!
  ctx <- ConverseR:::fetch_env_context(
    selection = "",
    input = "Can you help me write some code?",
    env = test_env,
    include_all = TRUE,
    cache_context = FALSE
  )
  ctx_str <- paste(ctx, collapse = "\n")

  # Full manifest lists all 6
  expect_true(grepl("Global Environment (.GlobalEnv) Manifest (6 total objects):", ctx_str, fixed = TRUE))
  expect_true(grepl("# - df1: <data.frame", ctx_str, fixed = TRUE))
  expect_true(grepl("# - df2: <data.frame", ctx_str, fixed = TRUE))
  expect_true(grepl("# - df3: <data.frame", ctx_str, fixed = TRUE))
  expect_true(grepl("# - df4: <data.frame", ctx_str, fixed = TRUE))
  expect_true(grepl("# - m1: <lm>", ctx_str, fixed = TRUE))
  expect_true(grepl("# - n_sample: <integer (len=1)>", ctx_str, fixed = TRUE))

  # Count detailed object descriptions (which start with '# name: <', unlike manifest lines '# - name: <')
  detailed_lines <- grep("^# [a-zA-Z0-9_]+: <", ctx, value = TRUE)
  expect_lte(length(detailed_lines), 3L)

  # When the user explicitly mentions df4:
  ctx_mention <- ConverseR:::fetch_env_context(
    selection = "",
    input = "Can you clean df4 for me?",
    env = test_env,
    include_all = TRUE,
    cache_context = FALSE
  )
  ctx_mention_str <- paste(ctx_mention, collapse = "\n")
  expect_true(grepl("# df4: <data.frame [5 rows x 1 cols]>", ctx_mention_str, fixed = TRUE))
})

test_that("extract_clean_user_prompt extracts the clean user question from heavy turn payloads", {
  # 1. Standard prompt with code context and environment
  raw_payload <- paste(
    "The user is currently working in this .R file (5000 lines):",
    "`````",
    paste(rep("x <- rnorm(100)", 100), collapse = "\n"),
    "`````",
    "",
    "The user made the following request:",
    "",
    "> How do I do a simple lmer() model",
    "",
    "Objects in the user's R Global Environment (.GlobalEnv):",
    "`````",
    "# df: <data.frame [100 x 5]>",
    "`````",
    sep = "\n"
  )

  clean <- ConverseR:::extract_clean_user_prompt(raw_payload)
  expect_identical(clean, "How do I do a simple lmer() model")

  # 2. Multiline user prompt with highlighted selection
  multiline_payload <- paste(
    "The user made the following request:",
    "",
    "> Line 1 of request",
    "Line 2 of request",
    "",
    "The user has highlighted the following selection to apply the request to:",
    "`````",
    "selection_code()",
    "`````",
    sep = "\n"
  )

  clean_multi <- ConverseR:::extract_clean_user_prompt(multiline_payload)
  expect_identical(clean_multi, "Line 1 of request\nLine 2 of request")

  # 3. Plain string already clean
  expect_identical(ConverseR:::extract_clean_user_prompt("Plain text question"), "Plain text question")
})

test_that("prune_chat_turn_payloads strips past turn payloads while keeping user dialogue intact (Option #3)", {
  skip_if_not_installed("ellmer")

  chat <- ellmer::chat_openrouter(api_key = "fake_test_key")

  heavy_turn_payload <- paste(
    "The user is currently working in this .R file (3500 lines):",
    "`````",
    paste(rep("x <- 1", 500), collapse = "\n"),
    "`````",
    "",
    "The user made the following request:",
    "",
    "> Fit an OLS regression",
    "",
    "Objects in the user's R Global Environment (.GlobalEnv):",
    "`````",
    "# - dat: <data.frame [50 x 3]>",
    "`````",
    sep = "\n"
  )

  t1 <- ellmer:::Turn(
    role = "user",
    contents = list(ellmer:::ContentText(heavy_turn_payload))
  )
  t2 <- ellmer:::Turn(
    role = "assistant",
    contents = list(ellmer:::ContentText("fit <- lm(y ~ x, data = dat)"))
  )
  t3 <- ellmer:::Turn(
    role = "user",
    contents = list(ellmer:::ContentText("The user is currently working...\n\nThe user made the following request:\n\n> Now summarize fit\n\nObjects in .GlobalEnv:"))
  )
  t4 <- ellmer:::Turn(
    role = "assistant",
    contents = list(ellmer:::ContentText("summary(fit)"))
  )

  chat$set_turns(list(t1, t2, t3, t4))

  # Prior to pruning, Turn 1 has thousands of characters
  expect_gt(nchar(chat$get_turns()[[1]]@text), 1000L)

  # Execute pruning: cleans Turn 1 via extract_clean_user_prompt() and Turn 3 via explicit prompt string
  ConverseR:::prune_chat_turn_payloads(chat, latest_clean_prompt = "Now summarize fit")

  turns_after <- chat$get_turns()
  expect_length(turns_after, 4L)
  expect_identical(turns_after[[1]]@text, "Fit an OLS regression")
  expect_identical(turns_after[[2]]@text, "fit <- lm(y ~ x, data = dat)")
  expect_identical(turns_after[[3]]@text, "Now summarize fit")
  expect_identical(turns_after[[4]]@text, "summary(fit)")
})

test_that("open_nonblocking_diff_review eliminates phantom empty code blocks from pre-fenced code", {
  ConverseR:::converser_reset()

  # Simulate an LLM output that already had executable chunk fences (e.g. from Quarto/Rmd)
  fenced_proposed <- "```{r}\nfit <- lmer(y ~ x + (1|group), data = df)\n```"

  # Call open_nonblocking_diff_review
  ConverseR:::open_nonblocking_diff_review(
    original = "old_code()",
    proposed = fenced_proposed,
    interface = "replace",
    prompt = "Fit a mixed model"
  )

  msgs <- ConverseR:::converser_env[["chat_history_ui"]]
  expect_length(msgs, 2L)

  assistant_msg <- msgs[[2]]
  expect_true(isTRUE(assistant_msg$auto_open_diff))

  # The assistant HTML must contain EXACTLY ONE <pre> block, never an extra empty <pre><code></code></pre>
  pre_count <- length(gregexpr("<pre>", assistant_msg$html)[[1]])
  expect_identical(pre_count, 1L)

  # Verify the code block does not duplicate backtick fences
  expect_false(grepl("<pre><code[^>]*>```", assistant_msg$html))
  expect_true(grepl("fit &lt;- lmer", assistant_msg$html, fixed = TRUE))
})

test_that("viewer guard and deferred task callback alert when another package occupies the Viewer pane", {
  orig_opt_v <- getOption("viewer")
  on.exit(options(viewer = orig_opt_v), add = TRUE)

  mock_viewer_called <- character(0)
  options(viewer = function(url, height = NULL) {
    mock_viewer_called <<- c(mock_viewer_called, url)
  })

  converser_url <- "http://127.0.0.1:5678/"
  ConverseR:::install_viewer_guard(converser_url)

  # 1. Opening ConverseR itself should not flag viewer as overwritten
  getOption("viewer")(converser_url)
  expect_false(isTRUE(ConverseR:::converser_env[["viewer_overwritten"]]))
  expect_true("converser_viewer_guard" %in% getTaskCallbackNames())

  # 2. When another package (like gtsummary/gt) opens an HTML table via getOption("viewer"):
  getOption("viewer")("file:///tmp/gtsummary_table.html")
  expect_true(isTRUE(ConverseR:::converser_env[["viewer_overwritten"]]))

  # 3. Synchronous task callback execution (now = TRUE):
  expect_message(
    res_cb <- ConverseR:::converser_viewer_callback_handler(NULL, NULL, TRUE, TRUE, now = TRUE),
    "The Viewer tab is now displaying HTML output"
  )
  expect_true(isTRUE(res_cb))
  expect_false(isTRUE(ConverseR:::converser_env[["viewer_overwritten"]]))

  # 4. Debounced deferred delivery at end of command sequence (now = FALSE):
  getOption("viewer")("file:///tmp/gtsummary_table.html")
  expect_true(isTRUE(ConverseR:::converser_env[["viewer_overwritten"]]))
  # Task callback runs after intermediate statement in a chunk/script:
  ConverseR:::converser_viewer_callback_handler(NULL, NULL, TRUE, TRUE, now = FALSE)
  # Still marked overwritten, alert deferred
  expect_true(isTRUE(ConverseR:::converser_env[["viewer_overwritten"]]))
  # Let debounce timer elapse and process idle event loop
  Sys.sleep(0.4)
  expect_message(
    later::run_now(0),
    "The Viewer tab is now displaying HTML output"
  )
  expect_false(isTRUE(ConverseR:::converser_env[["viewer_overwritten"]]))

  # 5. When another package calls rstudioapi::viewer directly:
  if (rstudioapi::isAvailable() || !is.null(ConverseR:::converser_env[["rstudioapi_viewer_orig"]])) {
    rstudioapi::viewer("file:///tmp/another_table.html")
    expect_true(isTRUE(ConverseR:::converser_env[["viewer_overwritten"]]))
    expect_message(
      res_cb2 <- ConverseR:::converser_viewer_callback_handler(NULL, NULL, TRUE, TRUE, now = TRUE),
      "The Viewer tab is now displaying HTML output"
    )
    expect_true(isTRUE(res_cb2))
    expect_false(isTRUE(ConverseR:::converser_env[["viewer_overwritten"]]))
  }

  # 6. Uninstall restores original viewer and cleans callbacks
  ConverseR:::uninstall_viewer_guard()
  expect_false("converser_viewer_guard" %in% getTaskCallbackNames())
})

test_that("Model Consultation: detect_model_function recognizes all 12 model families and rejects non-models", {
  # 1. Linear & ANOVA
  expect_equal(ConverseR:::detect_model_function("fit <- lm(y ~ x, data = df)"), "lm")
  expect_equal(ConverseR:::detect_model_function("res <- aov(yield ~ block + N*P, data = npk)"), "aov")
  expect_equal(ConverseR:::detect_model_function("anova(mod1, mod2)"), "anova")
  expect_equal(ConverseR:::detect_model_function("car::Anova(fit, type = 'III')"), "anova")
  expect_equal(ConverseR:::detect_model_function("manova(cbind(y1, y2) ~ group)"), "manova")

  # 2. Generalized Linear & Count Models
  expect_equal(ConverseR:::detect_model_function("glm(y ~ x, family = binomial, data = d)"), "glm")
  expect_equal(ConverseR:::detect_model_function("MASS::glm.nb(days ~ age, data = quine)"), "glm.nb")
  expect_equal(ConverseR:::detect_model_function("zeroinfl(art ~ fem + mar | fem, data = bio)"), "zeroinfl")
  expect_equal(ConverseR:::detect_model_function("pscl::hurdle(count ~ x | z)"), "hurdle")

  # 3. Mixed-Effects & Multilevel
  expect_equal(ConverseR:::detect_model_function("lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)"), "lmer")
  expect_equal(ConverseR:::detect_model_function("glmer(cbind(incidence, size - incidence) ~ period + (1 | herd), family = binomial)"), "glmer")
  expect_equal(ConverseR:::detect_model_function("nlmer(circumference ~ SSlogis(age, Asym, xmid, scal) ~ Asym|Tree)"), "nlmer")
  expect_equal(ConverseR:::detect_model_function("nlme::lme(distance ~ age, random = ~1 | Subject)"), "lme")
  expect_equal(ConverseR:::detect_model_function("glmmTMB::glmmTMB(count ~ spp + (1|site), family = nbinom2)"), "glmmtmb")

  # 4. Econometric & Fixed Effects
  expect_equal(ConverseR:::detect_model_function("fixest::feols(y ~ x1 + x2 | id + year, data = base)"), "feols")
  expect_equal(ConverseR:::detect_model_function("feglm(y ~ x | id, family = 'poisson')"), "feglm")
  expect_equal(ConverseR:::detect_model_function("fepois(trade ~ dist | exp + imp)"), "fepois")
  expect_equal(ConverseR:::detect_model_function("fenegbin(y ~ x | id)"), "fenegbin")
  expect_equal(ConverseR:::detect_model_function("feNmlm(y ~ x | id)"), "fenmlm")
  expect_equal(ConverseR:::detect_model_function("plm::plm(inv ~ value + capital, data = Grunfeld, model = 'within')"), "plm")
  expect_equal(ConverseR:::detect_model_function("ivreg(packs ~ price | tax, data = CigarettesSW)"), "ivreg")

  # 5. Bayesian Models
  expect_equal(ConverseR:::detect_model_function("brms::brm(count ~ zAge + (1|patient), family = poisson())"), "brm")
  expect_equal(ConverseR:::detect_model_function("rstanarm::stan_glm(y ~ x, data = d)"), "stan_glm")
  expect_equal(ConverseR:::detect_model_function("stan_glmer(y ~ x + (1|g), family = binomial)"), "stan_glmer")
  expect_equal(ConverseR:::detect_model_function("stan_lmer(Reaction ~ Days + (1|Subject))"), "stan_lmer")

  # 6. Generalized Additive Models (GAMs)
  expect_equal(ConverseR:::detect_model_function("mgcv::gam(y ~ s(x0) + s(x1), data = dat)"), "gam")
  expect_equal(ConverseR:::detect_model_function("bam(y ~ s(x0) + s(x1), data = big_data)"), "bam")

  # 7. Survival Analysis
  expect_equal(ConverseR:::detect_model_function("survival::coxph(Surv(time, status) ~ age + sex, lung)"), "coxph")
  expect_equal(ConverseR:::detect_model_function("survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian)"), "survreg")

  # 8. Ordinal & Categorical
  expect_equal(ConverseR:::detect_model_function("MASS::polr(Sat ~ Infl + Type, data = housing)"), "polr")
  expect_equal(ConverseR:::detect_model_function("nnet::multinom(prog ~ ses + write, data = hsbdemo)"), "multinom")
  expect_equal(ConverseR:::detect_model_function("ordinal::clm(rating ~ temp + contact, data = wine)"), "clm")
  expect_equal(ConverseR:::detect_model_function("clmm(rating ~ temp + (1|judge), data = wine)"), "clmm")

  # 9. Structural Equation Modeling (SEM)
  expect_equal(ConverseR:::detect_model_function("lavaan::sem(model_syntax, data = HolzingerSwineford1939)"), "sem")
  expect_equal(ConverseR:::detect_model_function("cfa(model, data = dat)"), "cfa")
  expect_equal(ConverseR:::detect_model_function("growth(model, data = demo)"), "growth")

  # 10. Robust & Quantile
  expect_equal(ConverseR:::detect_model_function("MASS::rlm(stack.loss ~ ., data = stackloss)"), "rlm")
  expect_equal(ConverseR:::detect_model_function("robustbase::lmrob(y ~ x)"), "lmrob")
  expect_equal(ConverseR:::detect_model_function("glmrob(y ~ x, family = binomial)"), "glmrob")
  expect_equal(ConverseR:::detect_model_function("quantreg::rq(foodexp ~ income, tau = 0.5)"), "rq")

  # 11. GEE & Longitudinal
  expect_equal(ConverseR:::detect_model_function("geepack::geeglm(response ~ time, id = id, family = binomial)"), "geeglm")
  expect_equal(ConverseR:::detect_model_function("gee(y ~ x, id = id)"), "gee")

  # 12. Meta-Analysis
  expect_equal(ConverseR:::detect_model_function("metafor::rma(yi, vi, data = dat)"), "rma")
  expect_equal(ConverseR:::detect_model_function("rma.uni(yi, vi)"), "rma.uni")
  expect_equal(ConverseR:::detect_model_function("rma.mv(yi, vi, random = ~ 1 | district)"), "rma.mv")

  # Negative cases
  expect_null(ConverseR:::detect_model_function("calmer <- 5"))
  expect_null(ConverseR:::detect_model_function("filter(df, a > 1)"))
  expect_null(ConverseR:::detect_model_function("ggplot(df, aes(x, y)) + geom_point()"))
  expect_null(ConverseR:::detect_model_function("library(lme4)"))
  expect_null(ConverseR:::detect_model_function(""))
  expect_null(ConverseR:::detect_model_function(NULL))
  expect_null(ConverseR:::detect_model_function(character(0)))
})

test_that("Model Consultation: system prompt, turn payload, and sidebar UI wiring", {
  # 1. System prompt includes non-jargony rules when is_consult = TRUE
  prompt_consult <- ConverseR:::construct_system_prompt(mode = "sidebar", is_consult = TRUE)
  expect_true(grepl("ACTIVE WORKFLOW: STATISTICAL MODEL CONSULTATION", prompt_consult, fixed = TRUE))
  expect_true(grepl("STRICT NON-JARGONY COMMUNICATION", prompt_consult, fixed = TRUE))
  expect_true(grepl("STRUCTURED 3-STEP CONSULTATION", prompt_consult, fixed = TRUE))

  # Default prompt excludes consult section
  prompt_default <- ConverseR:::construct_system_prompt(mode = "sidebar", is_consult = FALSE)
  expect_false(grepl("ACTIVE WORKFLOW: STATISTICAL MODEL CONSULTATION", prompt_default, fixed = TRUE))

  # 2. construct_turn prepends Model Consultation Request and enables whole doc
  fake_ctx <- structure(
    list(
      id = "doc_test",
      path = "model_test.R",
      contents = c(
        "library(lme4)",
        "fit <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)",
        "summary(fit)"
      ),
      selection = structure(
        list(list(
          range = list(start = c(row = 2L, column = 1L), end = c(row = 2L, column = 67L)),
          text = "fit <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)"
        )),
        class = "document_selection"
      )
    ),
    class = "document_context"
  )
  turn_consult <- ConverseR:::construct_turn(
    user_prompt = "Evaluate this model",
    context = fake_ctx,
    is_consult = TRUE
  )
  expect_true(grepl("\\[Model Consultation Request\\]", turn_consult))
  expect_true(grepl("fit <- lmer", turn_consult, fixed = TRUE))

  # 3. Sidebar HTML and router integration
  html_page <- ConverseR:::sidebar_single_page_html()
  expect_true(grepl("rc-pill-consult", html_page, fixed = TRUE))
  expect_true(grepl("id=\"consult_btn\"", html_page, fixed = TRUE))
  expect_true(grepl("Model Consultation", html_page, fixed = TRUE))
  expect_true(grepl("quickAction('consult')", html_page, fixed = TRUE))

  # 4. State API returns has_model and model_fn
  res_state <- ConverseR:::handle_sidebar_http_request(list(
    PATH_INFO = "/api/state",
    REQUEST_METHOD = "GET"
  ))
  parsed_state <- jsonlite::fromJSON(res_state$body)
  expect_true("has_model" %in% names(parsed_state))
  expect_true("model_fn" %in% names(parsed_state))

  # 5. Selection API returns has_model and model_fn
  res_sel <- ConverseR:::handle_sidebar_http_request(list(
    PATH_INFO = "/api/selection",
    REQUEST_METHOD = "GET"
  ))
  parsed_sel <- jsonlite::fromJSON(res_sel$body)
  expect_true(isTRUE(parsed_sel$ok))
  expect_true("has_model" %in% names(parsed_sel))
  expect_true("model_fn" %in% names(parsed_sel))
})

test_that("Fix Error accurately identifies failing line of code, line number, and error context", {
  # 1. find_expression_ending_at handles single-line and multiline statements
  script_lines <- c(
    "x <- 10",
    "fit <- lm(",
    "  mpg ~ cyl,",
    "  data = mtcars",
    ")",
    "summary(fit)",
    "bad_syntax <- lm(mpg ~"
  )
  
  # Single line ending at row 1
  s1 <- ConverseR:::find_expression_ending_at(script_lines, 1)
  expect_equal(s1$start, 1)
  expect_equal(s1$end, 1)
  expect_equal(s1$code, "x <- 10")

  # Multiline ending at row 5 (cursor at row 6)
  s_multi <- ConverseR:::find_expression_ending_at(script_lines, 5)
  expect_equal(s_multi$start, 2)
  expect_equal(s_multi$end, 5)
  expect_true(grepl("fit <- lm", s_multi$code, fixed = TRUE))

  # Syntax error fallback
  s_err <- ConverseR:::find_expression_ending_at(script_lines, 7)
  expect_equal(s_err$start, 7)
  expect_equal(s_err$end, 7)
  expect_equal(s_err$code, "bad_syntax <- lm(mpg ~")

  # 2. extract_call_from_errmsg
  call_single <- ConverseR:::extract_call_from_errmsg("Error in lm(mpg ~ bad, data = mtcars) : object 'bad' not found")
  expect_equal(call_single, "lm(mpg ~ bad, data = mtcars)")

  call_multi <- ConverseR:::extract_call_from_errmsg("Error in lm(mpg ~ bad,\n    data = mtcars) :\n  object 'bad' not found")
  expect_true(grepl("lm(mpg ~ bad", call_multi, fixed = TRUE))

  call_none <- ConverseR:::extract_call_from_errmsg("Error: object 'x' not found")
  expect_null(call_none)

  # 3. extract_failing_code with active highlighted selection (Tier 1)
  ctx_sel <- structure(
    list(
      path = "analysis.R",
      contents = script_lines,
      selection = list(list(
        text = "fit <- lm(mpg ~ cyl, data = mtcars)",
        range = list(start = list(row = 2, column = 1), end = list(row = 2, column = 35))
      ))
    ),
    class = "document_context"
  )
  res_sel <- ConverseR:::extract_failing_code(ctx_sel, "Error in eval(...) : bad variable")
  expect_equal(res_sel$code, "fit <- lm(mpg ~ cyl, data = mtcars)")
  expect_true(grepl("Highlighted Selection", res_sel$source))
  expect_equal(res_sel$line_start, 2)

  # 4. extract_failing_code with Ctrl+Enter cursor advance (cursor on row 3, failed on row 2)
  ctx_adv <- structure(
    list(
      path = "analysis.R",
      contents = c("data <- mtcars", "fit <- lm(mpg ~ nonexistent, data = data)", "summary(fit)"),
      selection = list(list(
        text = "",
        range = list(start = list(row = 3, column = 1), end = list(row = 3, column = 1))
      ))
    ),
    class = "document_context"
  )
  res_adv <- ConverseR:::extract_failing_code(ctx_adv, "Error in eval(predvars, data, env) : object 'nonexistent' not found")
  expect_equal(res_adv$code, "fit <- lm(mpg ~ nonexistent, data = data)")
  expect_equal(res_adv$line_start, 2)
  expect_equal(res_adv$source, "Line 2 of analysis.R")

  # 5. extract_failing_code with multiline piped block ending at row 4, cursor on row 5
  ctx_pipe <- structure(
    list(
      path = "clean.R",
      contents = c(
        "mtcars |>",
        "  filter(cyl == 4) |>",
        "  mutate(bad = missing_var * 2) |>",
        "  summarise(m = mean(bad))",
        "print('done')"
      ),
      selection = list(list(
        text = "",
        range = list(start = list(row = 5, column = 1), end = list(row = 5, column = 1))
      ))
    ),
    class = "document_context"
  )
  res_pipe <- ConverseR:::extract_failing_code(ctx_pipe, "Error in mutate() : object 'missing_var' not found")
  expect_equal(res_pipe$line_start, 1)
  expect_equal(res_pipe$line_end, 4)
  expect_equal(res_pipe$source, "Lines 1-4 of clean.R")
  expect_true(grepl("mutate(bad = missing_var * 2)", res_pipe$code, fixed = TRUE))

  # 6. extract_failing_code from console / base R error message
  res_console <- ConverseR:::extract_failing_code(NULL, "Error in read.csv('missing_file.csv') : cannot open the connection")
  expect_equal(res_console$code, "read.csv('missing_file.csv')")
  expect_equal(res_console$source, "Failed Call")

  # 7. capture_error_context formats code block and line numbers
  try(stop("Intentional test error"), silent = TRUE)
  err_ctx <- ConverseR:::capture_error_context(context = ctx_adv)
  expect_true(grepl("Code line/statement that led to the error", err_ctx, fixed = TRUE))
  expect_true(grepl("Line 2 of analysis.R", err_ctx, fixed = TRUE))
  expect_true(grepl("fit <- lm(mpg ~ nonexistent, data = data)", err_ctx, fixed = TRUE))

  # 8. construct_turn with include_error = TRUE includes failing code and prompt framing
  turn_err <- ConverseR:::construct_turn(
    user_prompt = "Diagnose my error",
    context = ctx_adv,
    include_error = TRUE
  )
  expect_true(grepl("\\[Error Diagnosis Request\\]", turn_err))
  expect_true(grepl("Line 2 of analysis.R", turn_err, fixed = TRUE))
  expect_true(grepl("fit <- lm(mpg ~ nonexistent, data = data)", turn_err, fixed = TRUE))
})


