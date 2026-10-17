# Throttled, Undo-Safe Document Streaming & Diff Utilities ----------------------

#' Stream an LLM generator into an RStudio/Positron document without IPC thrashing
#'
#' @description
#' Replaces `streamy::stream()` with three major architectural improvements:
#' 1. **Pre-Edit Snapshot (`converser_undo()`)**: Saves the full document state
#'    prior to editing so you can revert the entire AI generation in one step.
#' 2. **Time-Throttled Buffer Writes**: Instead of calling `rstudioapi::modifyRange()`
#'    on every single token (which causes severe UI flicker and IPC lag on fast models),
#'    batches editor updates at a smooth 100ms frame interval (`throttle_sec = 0.10`).
#' 3. **Real-Time `<thinking>` Tag & Fence Filtering**: Continuously sanitizes
#'    `<thinking>...</thinking>` blocks and stray markdown code fences before
#'    writing to the document buffer.
#'
#' @param generator A `coro` generator returned by `chat$stream(...)`.
#' @param context RStudio document context from `rstudioapi::getActiveDocumentContext()`.
#' @param interface One of `"replace"`, `"prefix"`, or `"suffix"`.
#' @param throttle_sec Minimum seconds between intermediate `rstudioapi::modifyRange()`
#'   calls during streaming. Default is `0.10` (10 FPS).
#' @returns Invisibly returns the final inserted/replacement text.
#' @export
stream_to_editor <- function(
  generator,
  context,
  interface = c("replace", "prefix", "suffix"),
  throttle_sec = 0.10
) {
  interface <- arg_match(interface)
  ext <- file_extension(context$path)

  # Save full document snapshot for one-step `converser_undo()`
  primary_sel <- rstudioapi::primary_selection(context)
  converser_env[["last_edit"]] <- list(
    doc_id = context$id,
    full_contents = context$contents,
    original_range = primary_sel$range,
    original_text = primary_sel$text
  )

  # Prepare target range based on interface
  prep <- prepare_stream_range(context, interface)
  active_range <- prep$range
  remainder <- prep$remainder
  n_lines_orig <- prep$n_lines_orig

  raw_accum <- ""
  last_update_time <- as.numeric(Sys.time())

  tryCatch(
    {
      if (is_positron()) {
        # Positron's modifyRange shim cannot interleave rapid updates; buffer with CLI spinner
        raw_accum <- collect_generator_with_spinner(generator)
      } else {
        coro::loop(
          for (chunk in generator) {
            if (is.null(chunk) || identical(chunk, "")) next
            raw_accum <- paste0(raw_accum, chunk)

            now <- as.numeric(Sys.time())
            if ((now - last_update_time) >= throttle_sec) {
              clean_so_far <- sanitize_llm_output(raw_accum, ext = ext)
              if (nzchar(clean_so_far)) {
                active_range <- write_buffered_chunk(
                  text = clean_so_far,
                  range = active_range,
                  doc_id = context$id,
                  n_lines_orig = n_lines_orig
                )
              }
              last_update_time <- now
            }
          }
        )
      }

      # Final clean write once streaming completes
      final_text <- sanitize_llm_output(raw_accum, ext = ext)
      final_payload <- sub("\n$", "", paste0(final_text, remainder))

      rstudioapi::modifyRange(
        location = active_range,
        text = final_payload,
        id = context$id
      )

      # Update end row to match final output
      n_final_lines <- nchar(gsub("[^\n]+", "", final_payload))
      active_range$end[["row"]] <- active_range$start[["row"]] + n_final_lines
      active_range$end[["column"]] <- 100000L

      rstudioapi::setSelectionRanges(active_range, id = context$id)
      tryCatch(rstudioapi::executeCommand("reindent"), error = function(e) NULL)

      if (!identical(interface, "replace")) {
        rstudioapi::setCursorPosition(active_range$start, id = context$id)
      }

      invisible(final_text)
    },
    error = function(e) {
      if (rstudioapi::isAvailable()) {
        rstudioapi::showDialog(
          "ConverseR Error",
          paste("The assistant encountered an issue while streaming:\n", conditionMessage(e))
        )
      } else {
        cli_warn("Streaming error: {conditionMessage(e)}")
      }
      invisible(raw_accum)
    }
  )
}

prepare_stream_range <- function(context, interface) {
  sel <- rstudioapi::primary_selection(context)
  range <- sel$range
  remainder <- ""

  if (identical(interface, "replace")) {
    if (range$end[["column"]] == 1L && range$end[["row"]] > range$start[["row"]]) {
      range$end[["row"]] <- range$end[["row"]] - 1L
    }
    sel_text <- sel[["text"]]
    range$end[["column"]] <- 100000L

    rstudioapi::setSelectionRanges(range, id = context$id)
    full_line_text <- rstudioapi::primary_selection(
      rstudioapi::getSourceEditorContext(id = context$id)
    )$text

    if (nzchar(sel_text)) {
      remainder <- gsub(gsub("\n$", "", sel_text), "", full_line_text, fixed = TRUE)
      remainder <- gsub("\n", "", remainder, fixed = TRUE)
    }

    n_lines_orig <- max(1L, range$end[["row"]] - range$start[["row"]] + 1L)
    empty_lines <- paste0(rep("\n", n_lines_orig - 1L), collapse = "")
    rstudioapi::modifyRange(range, empty_lines, context$id)
    rstudioapi::setCursorPosition(range$start, context$id)

    list(range = range, remainder = remainder, n_lines_orig = n_lines_orig)
  } else if (identical(interface, "prefix")) {
    whole_lines <- range
    whole_lines$start[["column"]] <- 1L
    whole_lines$end[["column"]] <- 100000L

    existing <- extract_lines_in_range(whole_lines, context$contents)
    rstudioapi::modifyRange(
      whole_lines,
      paste0(c("", existing), collapse = "\n"),
      context$id
    )

    target_range <- whole_lines
    target_range$end[["row"]] <- range$start[["row"]]
    target_range$end[["column"]] <- 100000L
    rstudioapi::setCursorPosition(target_range$start, context$id)

    list(range = target_range, remainder = "", n_lines_orig = 1L)
  } else {
    # suffix
    rstudioapi::modifyRange(
      range,
      paste0(sel[["text"]], "\n"),
      context$id
    )
    target_range <- range
    target_range$start[["row"]] <- range$end[["row"]] + 1L
    target_range$end[["row"]] <- range$end[["row"]] + 1L
    target_range$start[["column"]] <- 1L
    target_range$end[["column"]] <- 100000L
    rstudioapi::setCursorPosition(target_range$start, context$id)

    list(range = target_range, remainder = "", n_lines_orig = 1L)
  }
}

write_buffered_chunk <- function(text, range, doc_id, n_lines_orig) {
  clean_no_trail <- gsub("\n+$", "", text)
  n_lines <- nchar(gsub("[^\n]+", "", clean_no_trail)) + 1L
  pad_count <- max(n_lines_orig - n_lines, 0L)
  padded <- paste0(clean_no_trail, paste0(rep("\n", pad_count), collapse = ""))

  rstudioapi::modifyRange(range, padded, doc_id)

  n_lines_res <- nchar(gsub("[^\n]+", "", padded))
  range$end[["row"]] <- active_end_row <- range$start[["row"]] + n_lines_res
  range$end[["column"]] <- 100000L
  range
}

extract_lines_in_range <- function(range, contents) {
  s_row <- range$start[[1]]
  e_row <- range$end[[1]]
  s_col <- range$start[[2]]
  e_col <- range$end[[2]]

  if (s_row == e_row) {
    substr(contents[s_row], s_col, e_col)
  } else {
    lines <- contents[s_row:e_row]
    lines[1] <- substr(lines[1], s_col, nchar(lines[1]))
    lines[length(lines)] <- substr(lines[length(lines)], 1, e_col)
    lines
  }
}

collect_generator_with_spinner <- function(generator) {
  rlang::local_options(cli.progress_show_after = 0)
  cli::cli_progress_bar(
    total = NA,
    format = "{cli::pb_spin} ConverseR generating... {cli::col_grey(paste0('[', {cli::pb_elapsed}, ']'))}",
    clear = FALSE
  )
  chunks <- character(0)
  coro::loop(
    for (chunk in generator) {
      chunks <- c(chunks, chunk)
      cli::cli_progress_update()
    }
  )
  cli::cli_progress_done()
  paste0(chunks, collapse = "")
}

# Unified Diff Generator for Safe Previewing ------------------------------------

#' Compute a line-by-line unified diff HTML between original and proposed code
#'
#' @param original Character string of original selected code.
#' @param proposed Character string of proposed replacement code.
#' @returns An HTML string (`shiny::HTML`) rendering additions in green and removals in red.
#' @export
render_code_diff_html <- function(original, proposed) {
  orig_lines <- if (is.null(original) || !nzchar(original)) {
    character(0)
  } else {
    strsplit(gsub("\r\n", "\n", original), "\n", fixed = TRUE)[[1]]
  }

  prop_lines <- if (is.null(proposed) || !nzchar(proposed)) {
    character(0)
  } else {
    strsplit(gsub("\r\n", "\n", proposed), "\n", fixed = TRUE)[[1]]
  }

  diff_ops <- compute_line_diff(orig_lines, prop_lines)

  html_rows <- vapply(
    diff_ops,
    function(op) {
      escaped <- htmltools::htmlEscape(op$line)
      if (identical(op$type, "add")) {
        sprintf(
          "<div class='rc-diff-line rc-diff-add'><span class='rc-diff-sign'>+</span><code>%s</code></div>",
          escaped
        )
      } else if (identical(op$type, "del")) {
        sprintf(
          "<div class='rc-diff-line rc-diff-del'><span class='rc-diff-sign'>-</span><code>%s</code></div>",
          escaped
        )
      } else {
        sprintf(
          "<div class='rc-diff-line rc-diff-ctx'><span class='rc-diff-sign'> </span><code>%s</code></div>",
          escaped
        )
      }
    },
    character(1)
  )

  shiny::HTML(paste0("<div class='rc-diff-container'>", paste(html_rows, collapse = ""), "</div>"))
}

# Lightweight LCS (Longest Common Subsequence) line diff in pure R
compute_line_diff <- function(a, b) {
  n <- length(a)
  m <- length(b)

  if (n == 0 && m == 0) return(list())
  if (n == 0) return(lapply(b, function(l) list(type = "add", line = l)))
  if (m == 0) return(lapply(a, function(l) list(type = "del", line = l)))

  # Guard for huge diffs (>400 lines)
  if (n * m > 160000) {
    return(c(
      lapply(a, function(l) list(type = "del", line = l)),
      lapply(b, function(l) list(type = "add", line = l))
    ))
  }

  dp <- matrix(0L, nrow = n + 1L, ncol = m + 1L)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      if (identical(a[i], b[j])) {
        dp[i + 1L, j + 1L] <- dp[i, j] + 1L
      } else {
        dp[i + 1L, j + 1L] <- max(dp[i, j + 1L], dp[i + 1L, j])
      }
    }
  }

  ops <- list()
  i <- n
  j <- m
  while (i > 0L || j > 0L) {
    if (i > 0L && j > 0L && identical(a[i], b[j])) {
      ops <- c(list(list(type = "ctx", line = a[i])), ops)
      i <- i - 1L
      j <- j - 1L
    } else if (j > 0L && (i == 0L || dp[i + 1L, j] >= dp[i, j + 1L])) {
      ops <- c(list(list(type = "add", line = b[j])), ops)
      j <- j - 1L
    } else {
      ops <- c(list(list(type = "del", line = a[i])), ops)
      i <- i - 1L
    }
  }

  ops
}
