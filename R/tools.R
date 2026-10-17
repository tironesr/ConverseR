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
      description = "Inspect a fitted statistical or machine learning model object (e.g., lm, glm, fixest, lmerMod, coxph) in the user's R Global Environment. Returns formula, coefficient table (broom::tidy), and fit statistics (broom::glance).",
      name = "inspect_model",
      arguments = list(
        name = ellmer::type_string("The exact name of the model object in .GlobalEnv.")
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
  if (inherits(x, "lm") || requireNamespace("broom", quietly = TRUE)) {
    desc <- describe_object.lm(x, name = name)
  } else {
    desc <- utils::capture.output(print(summary(x)))
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
  capture_error_context()
}

#' Capture the most recent R error message and rlang backtrace
#' @keywords internal
capture_error_context <- function() {
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

  res <- character(0)
  if (nzchar(err_msg)) {
    res <- c(
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
