# Prompt Construction & Sanitization Engine ------------------------------------

#' Initialize a ConverseR assistant for inline or sidebar interaction
#'
#' @param context RStudio document context.
#' @param mode Either `"inline"` (returns pure replacement code without backticks)
#'   or `"sidebar"` (returns rich markdown explanations with fenced code blocks).
#' @param has_selection Logical; whether the user highlighted a selection.
#' @param continue_chat Logical; whether to reuse the previous inline chat object
#'   for multi-turn follow-up memory.
#' @param chat Optional pre-initialized `ellmer` `Chat` object.
#' @keywords internal
initialize_assistant <- function(
  context,
  mode = c("inline", "sidebar"),
  has_selection = FALSE,
  continue_chat = FALSE,
  chat = NULL
) {
  mode <- arg_match(mode)

  if (isTRUE(continue_chat) && !is.null(converser_env[["last_inline_chat"]])) {
    assistant <- converser_env[["last_inline_chat"]]
  } else {
    assistant <- chat %||% get_converser_chat(register_tools = TRUE)
  }

  if (is.null(assistant)) {
    return(NULL)
  }

  sys_prompt <- construct_system_prompt(
    context = context,
    mode = mode,
    has_selection = has_selection
  )
  assistant$set_system_prompt(sys_prompt)

  assistant
}

construct_system_prompt <- function(
  context = NULL,
  mode = c("inline", "sidebar"),
  has_selection = FALSE
) {
  mode <- arg_match(mode)
  ext <- if (!is.null(context)) file_extension(context$path) else "r"

  custom_sys <- getOption("converser.system_prompt")
  base_persona <- if (!is.null(custom_sys) && is_string(custom_sys) && nzchar(trimws(custom_sys))) {
    trimws(custom_sys)
  } else {
    paste0(
      "You are ConverseR, an expert R data scientist, statistician, and academic ",
      "research assistant integrated directly into the user's RStudio/Positron IDE. ",
      "You have access to tools that can inspect the user's live R global environment, ",
      "statistical models, data frames, and installed package documentation."
    )
  }

  style_guide <- trimws(get_converser_style())
  style_section <- if (nzchar(style_guide)) {
    paste0("\n\n", style_guide)
  } else {
    ""
  }

  skill_md <- get_active_skill_text()
  skill_section <- if (nzchar(trimws(skill_md))) {
    paste0(
      "\n\n## Active User Skill / Reference Instructions (SKILL.md)\n\n",
      trimws(skill_md)
    )
  } else {
    ""
  }

  if (identical(mode, "inline")) {
    format_rules <- if (identical(ext, "r")) {
      paste0(
        "CRITICAL OUTPUT FORMAT: You are streaming directly into an `.R` script buffer. ",
        "Respond ONLY with valid, executable R code. Do NOT include markdown code fences ",
        "(no ```r or ```), do NOT include conversational exposition, and do NOT include ",
        "<thinking> tags in your final output. Any necessary brief notes must be R comments (#)."
      )
    } else if (ext %in% c("qmd", "rmd")) {
      paste0(
        "CRITICAL OUTPUT FORMAT: You are streaming directly into a Quarto/RMarkdown (.",
        ext, ") document. When generating executable R code blocks outside an existing ",
        "chunk, use standard executable chunk syntax ` ```{r} ` (with curly braces), ",
        "NOT ` ```r `. Provide only the requested content with no conversational wrapper."
      )
    } else {
      paste0(
        "CRITICAL OUTPUT FORMAT: Provide only the requested code or text with no ",
        "conversational exposition and no surrounding markdown fences unless explicitly asked."
      )
    }

    selection_rule <- if (has_selection) {
      paste0(
        "\n\nYour task is to apply the user's request to their selected text. ",
        "Return ONLY the replacement text for the selection-no surrounding context, ",
        "no explanation, no outer fences."
      )
    } else {
      ""
    }

    paste0(base_persona, style_section, skill_section, "\n\n", format_rules, selection_rule)
  } else {
    paste0(
      base_persona,
      style_section,
      skill_section, "\n\n",
      "When providing code that modifies or replaces the user's active selection or script, ",
      "always place the complete, ready-to-run R code inside a standard fenced code block ",
      "(```r ... ```) so the user can one-click Run in Console, Preview Difference, Insert, or Replace. ",
      "CRITICAL RULE FOR SKILL.MD GENERATION: Whenever the user asks you to generate, create, or draft ",
      "a `SKILL.md` file (or coding skill specification), you MUST output the ENTIRE skill inside a ",
      "SINGLE ```yaml ... ``` code chunk. Format the entire skill as clean YAML (using YAML keys like ",
      "`name`, `description`, `coding_style`, `packages`, `modeling_conventions`, `plotting_conventions`, ",
      "and `code_templates` with indented `|` block scalars for any R code snippets). NEVER place triple ",
      "backticks (```) inside the YAML block, and do not split the output into multiple code blocks, so ",
      "the user can copy the entire SKILL.md from a single code block in one click. ",
      "Be concise, rigorous, and statistically sound."
    )
  }
}

#' Construct a contextualized user turn for the LLM
#'
#' @param user_prompt User's natural language request.
#' @param context RStudio document context.
#' @param include_env Logical; whether to automatically attach matched `.GlobalEnv` summaries.
#' @param include_error Logical; whether to attach the most recent R error & backtrace.
#' @param window_lines Number of surrounding code lines to include.
#' @keywords internal
construct_turn <- function(
  user_prompt,
  context = NULL,
  include_env = TRUE,
  include_error = FALSE,
  window_lines = get_converser_window()
) {
  code_ctx <- fetch_code_context(context, window_lines = window_lines)
  selection_raw <- code_ctx[["selection_raw"]] %||% ""

  env_ctx <- if (isTRUE(include_env)) {
    fetch_env_context(selection_raw, user_prompt, env = global_env())
  } else {
    character(0)
  }

  err_ctx <- if (isTRUE(include_error)) {
    capture_error_context()
  } else {
    character(0)
  }

  construct_turn_impl(
    user_prompt = user_prompt,
    code_context = code_ctx,
    env_context = env_ctx,
    error_context = err_ctx
  )
}

construct_turn_impl <- function(
  user_prompt,
  code_context,
  env_context = character(0),
  error_context = character(0)
) {
  res <- character(0)

  file_contents <- code_context[["file_contents"]]
  selection <- code_context[["selection"]]
  selection_raw <- code_context[["selection_raw"]] %||% ""
  ext <- code_context[["ext"]] %||% "r"

  if (length(file_contents) > 0) {
    res <- c(
      sprintf("The user is currently working in this `.%s` file (windowed around line %d):", ext, code_context[["cursor_line"]] %||% 1L),
      "",
      file_contents
    )
  }

  res <- c(res, "", "The user made the following request:", "")
  res <- c(res, paste0("> ", user_prompt))

  if (length(selection) > 0) {
    res <- c(
      res, "",
      "The user has highlighted the following selection to apply the request to:",
      "",
      selection
    )
  }

  if (length(env_context) > 0 && !identical(env_context, character(0))) {
    res <- c(
      res, "",
      "Relevant objects detected in the user's R Global Environment (.GlobalEnv):",
      "",
      env_context
    )
  }

  if (length(error_context) > 0 && nzchar(paste(error_context, collapse = ""))) {
    res <- c(
      res, "",
      "Recent R Console Error / Backtrace:",
      "`````",
      error_context,
      "`````"
    )
  }

  if (nzchar(selection_raw)) {
    if (grepl("`", selection_raw, fixed = TRUE)) {
      res <- c(
        res, "",
        paste0(
          "The five-backtick fences (`````) above frame the prompt sections. ",
          "Preserve any internal backticks that belong to the selection itself, ",
          "but never output the five-backtick delimiter fences."
        )
      )
    }
  }

  paste0(res, collapse = "\n")
}

# Output Sanitization (Fixes <thinking> leakage and markdown fence bugs) --------

#' Strip reasoning `<thinking>` blocks and unwanted markdown fences from model output
#'
#' @description
#' Strips `<thinking>...</thinking>` blocks and stray markdown code fences.
#'
#' @param text Character string generated by the LLM.
#' @param ext File extension (e.g. `"r"`, `"qmd"`, `"rmd"`).
#' @param strip_fences Logical; whether to strip outer ` ```r ... ``` ` fences.
#' @returns Cleaned character string.
#' @export
sanitize_llm_output <- function(text, ext = "r", strip_fences = (identical(ext, "r"))) {
  if (is.null(text) || length(text) == 0) {
    return("")
  }
  out <- paste0(text, collapse = "")

  # 1. Strip complete <thinking>...</thinking> or <think>...</think> blocks (dotall)
  out <- gsub("(?s)<think(?:ing)?>.*?</think(?:ing)?>\\s*", "", out, perl = TRUE)

  # 2. If streaming is mid-way inside an unclosed <thinking> block, hide the partial thinking text
  if (grepl("<think(?:ing)?>", out, perl = TRUE) && !grepl("</think(?:ing)?>", out, perl = TRUE)) {
    out <- sub("(?s)<think(?:ing)?>.*$", "", out, perl = TRUE)
  }

  # 3. Strip five-backtick delimiter fences if the model echoed them
  out <- gsub("^\\s*`````[a-zA-Z0-9_{}]*\\n?", "", out)
  out <- gsub("\\n?`````\\s*$", "", out)

  # 4. In .R files (or when strip_fences = TRUE), strip outer triple-backtick code fences
  if (isTRUE(strip_fences)) {
    out <- gsub("^\\s*```[a-zA-Z0-9_{}]*\\s*\r?\n", "", out)
    out <- gsub("\r?\n```\\s*$", "", out)
    out <- gsub("^\\s*```\\s*$", "", out)
  } else if (ext %in% c("qmd", "rmd")) {
    out <- gsub("(?m)^(\\s*)```r\\s*$", "\\1```{r}", out, perl = TRUE)
  }

  out
}

#' Extract the primary R code block from a Markdown response (for Sidebar Diff/Apply)
#'
#' @param markdown_text Full markdown text from an assistant response.
#' @returns Character string containing the extracted code, or the sanitized text if no fences exist.
#' @export
extract_code_from_markdown <- function(markdown_text) {
  if (is.null(markdown_text) || !nzchar(trimws(markdown_text))) {
    return("")
  }

  cleaned <- sanitize_llm_output(markdown_text, ext = "md", strip_fences = FALSE)

  pattern <- "(?s)```(?:\\{r[^}]*\\}|[a-zA-Z0-9_-]*)\\s*\r?\n(.*?)\r?\n```"
  matches <- regmatches(cleaned, gregexpr(pattern, cleaned, perl = TRUE))[[1]]

  if (length(matches) == 0) {
    return(trimws(sanitize_llm_output(cleaned, ext = "r", strip_fences = TRUE)))
  }

  blocks <- vapply(
    matches,
    function(m) {
      sub("(?s)^```(?:\\{r[^}]*\\}|[a-zA-Z0-9_-]*)\\s*\r?\n(.*?)\r?\n```$", "\\1", m, perl = TRUE)
    },
    character(1)
  )

  paste(blocks, collapse = "\n\n")
}
