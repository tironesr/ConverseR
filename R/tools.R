# Live R Session Introspection Tools for ellmer --------------------------------

#' Register ConverseR live introspection tools on an ellmer Chat object
#'
#' @description
#' Equips an `ellmer` `Chat` object with safe, read-only introspection tools so
#' the LLM can dynamically inspect your global environment, data frames,
#' statistical models, installed package documentation, and recent console
#' errors without dumping everything into the initial prompt.
#'
#' @param chat An `ellmer` `Chat` object.
#' @returns Invisibly returns the modified `Chat` object.
#' @export
register_converser_tools <- function(chat) {
  if (!inherits(chat, "Chat")) {
    cli_abort("{.arg chat} must be an {.pkg ellmer} {.cls Chat} object.")
  }

  tools <- converser_tools()
  for (tl in tools) {
    chat$register_tool(tl)
  }

  invisible(chat)
}

#' Get the list of ConverseR ellmer tool definitions
#'
#' @returns A list of `ellmer` tool definitions.
#' @export
converser_tools <- function() {
  list(
    ellmer::tool(
      tool_list_env,
      description = "List all objects currently in the user's R Global Environment (.GlobalEnv) along with their class, dimensions/length, and memory size.",
      name = "list_env_objects",
      arguments = list()
    ),
    ellmer::tool(
      tool_inspect_data,
      description = "Inspect a data.frame, tibble, data.table, or object in the user's R Global Environment. Returns column names, types, missing value (NA) counts, factor levels, summary statistics, and sample rows.",
      name = "inspect_data",
      arguments = list(
        name = ellmer::type_string("The exact name of the object in .GlobalEnv to inspect."),
        n_rows = ellmer::type_integer("Number of sample rows to preview (default 5).", required = FALSE)
      )
    ),
    ellmer::tool(
      tool_inspect_model,
      description = "Inspect a fitted statistical or machine learning model or hypothesis test object (e.g., lm, glm, fixest, lmerMod/glmerMod, coxph, lavaan, htest) in the user's R Global Environment. Returns formula, family/link, coefficient table with 95% confidence intervals (broom::tidy), random effects variances, and fit statistics (broom::glance).",
      name = "inspect_model",
      arguments = list(
        name = ellmer::type_string("The exact name of the model or hypothesis test object in .GlobalEnv.")
      )
    ),
    ellmer::tool(
      tool_search_help,
      description = "Read official R documentation (.Rd help file) and function signature for an installed R function or package topic. Use this to verify exact argument names and avoid hallucinating deprecated syntax.",
      name = "search_r_help",
      arguments = list(
        topic = ellmer::type_string("Function or topic name (e.g., 'across' or 'dplyr::across')."),
        pkg = ellmer::type_string("Optional package name (e.g., 'dplyr').", required = FALSE)
      )
    ),
    ellmer::tool(
      tool_last_error,
      description = "Retrieve the most recent R error message and rlang backtrace from the user's R session.",
      name = "get_last_r_error",
      arguments = list()
    )
  )
}

# Tool Implementations ---------------------------------------------------------

tool_list_env <- function() {
  env <- global_env()
  objs <- ls(envir = env, all.names = FALSE)
  if (length(objs) == 0) {
    return("The user's .GlobalEnv is currently empty.")
  }

  info <- vapply(
    objs,
    function(nm) {
      tryCatch(
        {
          x <- get(nm, envir = env)
          cls <- paste(class(x), collapse = "/")
          dims <- if (!is.null(dim(x))) {
            paste0("[", paste(dim(x), collapse = " x "), "]")
          } else {
            paste0("(len=", length(x), ")")
          }
          sprintf("- %s: <%s> %s", nm, cls, dims)
        },
        error = function(e) sprintf("- %s: <uninspectable>", nm)
      )
    },
    character(1)
  )

  paste(c("Objects in .GlobalEnv:", unname(info)), collapse = "\n")
}

tool_inspect_data <- function(name, n_rows = 5L) {
  env <- global_env()
  if (!exists(name, envir = env, inherits = FALSE)) {
    return(sprintf("Object '%s' does not exist in .GlobalEnv.", name))
  }

  x <- get(name, envir = env)
  n_rows <- max(1L, min(as.integer(n_rows %||% 5L), 25L))

  old_dims <- getOption("converser.dims")
  on.exit(options(converser.dims = old_dims), add = TRUE)
  options(converser.dims = c(n_rows, 100L))

  desc <- describe_object(x, name = name)

  if (is.data.frame(x) && ncol(x) <= 30) {
    sum_out <- tryCatch(
      utils::capture.output(print(summary(x))),
      error = function(e) character(0)
    )
    if (length(sum_out) > 0) {
      desc <- c(desc, "", "# Summary Statistics:", paste0("#> ", sum_out))
    }
  }

  paste(desc, collapse = "\n")
}

tool_inspect_model <- function(name) {
  env <- global_env()
  if (!exists(name, envir = env, inherits = FALSE)) {
    return(sprintf("Model object '%s' does not exist in .GlobalEnv.", name))
  }

  x <- get(name, envir = env)
  if (inherits(x, "htest")) {
    desc <- describe_object.htest(x, name = name)
  } else {
    desc <- describe_model_object(x, name = name)
  }

  paste(desc, collapse = "\n")
}

tool_search_help <- function(topic, pkg = NULL) {
  if (grepl("::", topic, fixed = TRUE)) {
    parts <- strsplit(topic, "::", fixed = TRUE)[[1]]
    pkg <- parts[1]
    topic <- parts[2]
  }

  if (!is.null(pkg) && !nzchar(trimws(pkg))) {
    pkg <- NULL
  }

  sig_line <- tryCatch(
    {
      fn <- if (!is.null(pkg)) {
        getExportedValue(pkg, topic)
      } else {
        get(topic, mode = "function")
      }
      paste0("Function Signature: ", topic, "(", paste(names(formals(fn)), collapse = ", "), ")")
    },
    error = function(e) NULL
  )

  help_text <- tryCatch(
    {
      h <- if (!is.null(pkg)) {
        utils::help((topic), package = (pkg), help_type = "text")
      } else {
        utils::help((topic), help_type = "text")
      }

      if (length(h) == 0) {
        return(paste(c(sig_line, sprintf("No help file found for topic '%s'.", topic)), collapse = "\n"))
      }

      rd <- utils:::.getHelpFile(as.character(h[1]))
      txt <- utils::capture.output(tools::Rd2txt(rd, out = stdout(), options = list(underline_titles = FALSE)))
      if (length(txt) > 120) {
        txt <- c(txt[1:120], "... [help file truncated for brevity]")
      }
      paste(txt, collapse = "\n")
    },
    error = function(e) {
      sprintf("Could not read Rd file for '%s': %s", topic, conditionMessage(e))
    }
  )

  paste(c(sig_line, help_text), collapse = "\n\n")
}

tool_last_error <- function() {
  capture_error_context(context = get_safe_editor_context())
}

find_expression_ending_at <- function(contents, end_line, max_lines = 35L) {
  if (is.null(contents) || length(contents) == 0L || is.null(end_line) || is.na(end_line)) {
    return(NULL)
  }
  end_line <- min(as.integer(end_line), length(contents))
  if (end_line < 1L) return(NULL)

  # Skip trailing blank lines or comments if end_line itself is blank/comment
  while (end_line >= 1L && (!nzchar(trimws(contents[end_line])) || grepl("^\\s*#", contents[end_line]))) {
    end_line <- end_line - 1L
  }
  if (end_line < 1L) return(NULL)

  min_line <- max(1L, end_line - max_lines + 1L)
  best_stmt <- NULL

  for (start_line in seq(end_line, min_line, by = -1L)) {
    raw_sub <- paste(contents[start_line:end_line], collapse = "\n")
    p <- tryCatch(parse(text = raw_sub, keep.source = FALSE), error = function(e) NULL)
    if (!is.null(p) && length(p) == 1L) {
      best_stmt <- list(
        start = start_line,
        end = end_line,
        code = paste(contents[start_line:end_line], collapse = "\n")
      )
    } else if (!is.null(best_stmt) && !is.null(p) && length(p) > 1L) {
      break
    }
  }

  if (is.null(best_stmt)) {
    best_stmt <- list(
      start = end_line,
      end = end_line,
      code = contents[end_line]
    )
  }
  best_stmt
}

extract_call_from_errmsg <- function(err_msg) {
  if (is.null(err_msg) || !nzchar(err_msg)) return(NULL)
  m <- regexec("(?s)^\\s*Error in (.*?)\\s*:\\s*(.*)$", err_msg, perl = TRUE)
  matches <- regmatches(err_msg, m)[[1]]
  if (length(matches) >= 2L && nzchar(trimws(matches[2]))) {
    return(trimws(matches[2]))
  }
  NULL
}

#' Extract the code line or statement that caused the most recent error
#' @param context Editor context from RStudio.
#' @param err_msg Error message string from `geterrmessage()`.
#' @param rlang_err Optional error object from `rlang::last_error()`.
#' @keywords internal
extract_failing_code <- function(context = NULL, err_msg = "", rlang_err = NULL) {
  err_msg <- trimws(err_msg %||% "")
  err_call <- extract_call_from_errmsg(err_msg)

  rlang_call <- NULL
  if (!is.null(rlang_err)) {
    rlang_call <- tryCatch({
      c_obj <- rlang_err$call %||% rlang_err$trace$call
      if (!is.null(c_obj)) paste(deparse(c_obj), collapse = "\n") else NULL
    }, error = function(e) NULL)
  }

  # Tier 1: User has an active selection in the editor
  if (!is.null(context) && !is.null(context$selection) && length(context$selection) > 0L) {
    sel_text <- trimws(context$selection[[1]]$text %||% "")
    if (nzchar(sel_text)) {
      fname <- if (!is.null(context$path) && nzchar(context$path)) basename(context$path) else "script"
      r_start <- tryCatch(as.integer(context$selection[[1]]$range$start[["row"]]), error = function(e) NA_integer_)
      r_end <- tryCatch(as.integer(context$selection[[1]]$range$end[["row"]]), error = function(e) NA_integer_)

      source_desc <- if (!is.na(r_start) && !is.na(r_end) && r_start == r_end) {
        sprintf("Line %d of %s (Highlighted Selection)", r_start, fname)
      } else if (!is.na(r_start) && !is.na(r_end)) {
        sprintf("Lines %d-%d of %s (Highlighted Selection)", r_start, r_end, fname)
      } else {
        sprintf("%s (Highlighted Selection)", fname)
      }
      return(list(code = sel_text, source = source_desc, line_start = r_start, line_end = r_end))
    }
  }

  # Tier 2: Editor script around cursor position
  if (!is.null(context) && !is.null(context$contents) && length(context$contents) > 0L) {
    contents <- context$contents
    fname <- if (!is.null(context$path) && nzchar(context$path)) basename(context$path) else "script"
    cursor_row <- tryCatch(as.integer(context$selection[[1]]$range$start[["row"]]), error = function(e) NA_integer_)

    if (!is.na(cursor_row) && cursor_row >= 1L) {
      cand_a <- if (cursor_row > 1L) find_expression_ending_at(contents, cursor_row - 1L) else NULL
      cand_b <- find_expression_ending_at(contents, cursor_row)

      matches_err <- function(cand) {
        if (is.null(cand) || !nzchar(cand$code)) return(FALSE)
        if (!is.null(err_call)) {
          if (grepl(err_call, cand$code, fixed = TRUE)) return(TRUE)
          fn_name <- sub("\\(.*$", "", err_call)
          if (nzchar(fn_name)) {
            if (grepl("^[a-zA-Z0-9_.]+$", fn_name)) {
              if (tryCatch(grepl(paste0("\\b", fn_name, "\\b"), cand$code), error = function(e) FALSE)) return(TRUE)
            }
            if (grepl(fn_name, cand$code, fixed = TRUE)) return(TRUE)
          }
        }
        if (!is.null(rlang_call)) {
          if (grepl(rlang_call, cand$code, fixed = TRUE)) return(TRUE)
          fn_name <- sub("\\(.*$", "", rlang_call)
          if (nzchar(fn_name)) {
            if (grepl("^[a-zA-Z0-9_.]+$", fn_name)) {
              if (tryCatch(grepl(paste0("\\b", fn_name, "\\b"), cand$code), error = function(e) FALSE)) return(TRUE)
            }
            if (grepl(fn_name, cand$code, fixed = TRUE)) return(TRUE)
          }
        }
        if (nzchar(err_msg)) {
          quoted_tokens <- regmatches(err_msg, gregexpr("['\"][a-zA-Z0-9_.]+['\"]", err_msg))[[1]]
          if (length(quoted_tokens) > 0L) {
            clean_tokens <- gsub("['\"]", "", quoted_tokens)
            clean_tokens <- clean_tokens[nchar(clean_tokens) > 1L]
            for (tok in clean_tokens) {
              if (grepl(paste0("\\b", tok, "\\b"), cand$code)) return(TRUE)
            }
          }
        }
        FALSE
      }

      chosen <- NULL
      if (!is.null(cand_a) && matches_err(cand_a)) {
        chosen <- cand_a
      } else if (!is.null(cand_b) && matches_err(cand_b)) {
        chosen <- cand_b
      } else if (!is.null(cand_a) && nzchar(trimws(cand_a$code))) {
        # Default to cursor_row - 1 because Ctrl+Enter advances cursor in RStudio
        chosen <- cand_a
      } else if (!is.null(cand_b) && nzchar(trimws(cand_b$code))) {
        chosen <- cand_b
      }

      if (!is.null(chosen)) {
        source_desc <- if (chosen$start == chosen$end) {
          sprintf("Line %d of %s", chosen$start, fname)
        } else {
          sprintf("Lines %d-%d of %s", chosen$start, chosen$end, fname)
        }
        return(list(code = chosen$code, source = source_desc, line_start = chosen$start, line_end = chosen$end))
      }
    }
  }

  # Tier 3: Console expressions & error calls
  last_err_cmd <- tryCatch(converser_env[["last_error_code"]], error = function(e) NULL)
  if (!is.null(last_err_cmd) && nzchar(trimws(last_err_cmd))) {
    return(list(code = trimws(last_err_cmd), source = "R Console", line_start = NA_integer_, line_end = NA_integer_))
  }

  last_console <- tryCatch(converser_env[["last_console_code"]], error = function(e) NULL)
  if (!is.null(last_console) && nzchar(trimws(last_console))) {
    if (!is.null(err_call)) {
      fn_name <- sub("\\(.*$", "", err_call)
      if (grepl(err_call, last_console, fixed = TRUE) ||
          (nzchar(fn_name) && grepl(fn_name, last_console, fixed = TRUE))) {
        return(list(code = trimws(last_console), source = "R Console", line_start = NA_integer_, line_end = NA_integer_))
      }
    }
  }

  if (!is.null(rlang_call) && nzchar(trimws(rlang_call))) {
    return(list(code = trimws(rlang_call), source = "Failed Call (rlang)", line_start = NA_integer_, line_end = NA_integer_))
  }

  if (!is.null(err_call) && nzchar(trimws(err_call))) {
    return(list(code = trimws(err_call), source = "Failed Call", line_start = NA_integer_, line_end = NA_integer_))
  }

  if (!is.null(last_console) && nzchar(trimws(last_console))) {
    return(list(code = trimws(last_console), source = "R Console", line_start = NA_integer_, line_end = NA_integer_))
  }

  NULL
}

#' Capture the most recent R error message, failing code, and rlang backtrace
#' @param context Optional editor context (as returned by `get_safe_editor_context()`).
#' @keywords internal
capture_error_context <- function(context = NULL) {
  if (is.null(context)) {
    context <- get_safe_editor_context()
  }

  raw_err <- trimws(geterrmessage())

  # Guard: calling `rlang::last_error()` when no rlang error has occurred
  # throws an internal error that overwrites R's `geterrmessage()` buffer with
  # "Can't show last error because no error was recorded yet".
  # Preserve any genuine base-R error message in `converser_env` before checking rlang!
  if (nzchar(raw_err) && !grepl("Can't show last error because no error was recorded", raw_err, fixed = TRUE)) {
    converser_env[["last_real_errmessage"]] <- raw_err
  }

  err_msg <- converser_env[["last_real_errmessage"]] %||% ""
  if (grepl("Can't show last error because no error was recorded", err_msg, fixed = TRUE)) {
    err_msg <- ""
  }

  rlang_err <- tryCatch(rlang::last_error(), error = function(e) NULL)

  if (!nzchar(err_msg) && is.null(rlang_err) && is.null(converser_env[["last_error_code"]])) {
    return("No recent R error recorded in this session.")
  }

  # Extract failing code line/block
  failing <- extract_failing_code(context = context, err_msg = err_msg, rlang_err = rlang_err)

  res <- character(0)
  if (!is.null(failing) && nzchar(trimws(failing$code))) {
    src_label <- if (nzchar(failing$source %||% "")) sprintf(" [%s]", failing$source) else ""
    res <- c(
      sprintf("Code line/statement that led to the error%s:", src_label),
      "```r",
      failing$code,
      "```",
      ""
    )
  }

  if (nzchar(err_msg)) {
    res <- c(
      res,
      "Last R error message (geterrmessage()):",
      err_msg
    )
  }

  if (!is.null(rlang_err)) {
    bt <- tryCatch(utils::capture.output(print(rlang_err)), error = function(e) character(0))
    if (length(bt) > 0) {
      res <- c(
        res,
        "",
        "Last rlang error / backtrace:",
        bt
      )
    }
  }

  if (length(res) == 0) {
    return("No recent R error recorded in this session.")
  }

  paste(res, collapse = "\n")
}
