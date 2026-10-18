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

test_that("extract_code_from_markdown and render_code_diff_html work accurately", {
  md <- "Here is the updated code:\n\n```r\nx <- 1:10\ny <- x * 2\n```\n\nHope that helps!"
  extracted <- extract_code_from_markdown(md)
  expect_equal(extracted, "x <- 1:10\ny <- x * 2")

  diff_html <- as.character(render_code_diff_html("x <- 1:5\ny <- x * 2", extracted))
  expect_true(grepl("rc-diff-del", diff_html, fixed = TRUE))
  expect_true(grepl("rc-diff-add", diff_html, fixed = TRUE))
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
