# Minimalist Inline Assistant with Non-Blocking Diff Preview -------------------

#' Run the minimalist ConverseR Inline Assistant
#'
#' @description
#' Opens a sleek, compact keyboard-driven prompt bar in RStudio/Positron.
#' Automatically includes windowed script context and smart `.GlobalEnv`
#' object summaries (`data.frame`, `lm`/`glm`, `ggplot`).
#'
#' When you have code highlighted, you can choose **Replace**, **Insert Before**,
#' or **Insert After**, and optionally check **Preview Difference first** to
#' inspect a side-by-side red/green diff in the non-blocking Viewer pane before
#' modifying your document (avoiding any nested Shiny modal `"Terminate R"`
#' warning).
#'
#' @returns Invisibly returns the underlying `ellmer` `Chat` object.
#' @export
converser_inline <- function() {
  if (!rstudioapi::isAvailable()) {
    cli_abort("{.fn converser_inline} requires an active RStudio or Positron session.")
  }

  context <- tryCatch(
    rstudioapi::getSourceEditorContext(),
    error = function(e) rstudioapi::getActiveDocumentContext()
  )

  if (is.null(context)) {
    cli_inform(c("!" = "Please open an R script or Quarto/RMarkdown document in the Source Editor first."))
    return(invisible(NULL))
  }

  chat_check <- get_converser_chat(register_tools = FALSE)
  if (is.null(chat_check)) {
    return(invisible(NULL))
  }

  sel_text <- rstudioapi::primary_selection(context)$text %||% ""
  has_selection <- nzchar(sel_text)

  converser_env[["inline_submitted_input"]] <- NULL

  tryCatch(
    suppressMessages(
      converser_inline_gadget(has_selection = has_selection)
    ),
    error = function(cnd) NULL
  )

  input <- converser_env[["inline_submitted_input"]]

  if (is.null(input) || !nzchar(trimws(input$text %||% ""))) {
    return(invisible(NULL))
  }

  fresh_ctx <- tryCatch(
    rstudioapi::getSourceEditorContext(),
    error = function(e) NULL
  )
  if (!is.null(fresh_ctx) && nzchar(fresh_ctx$id %||% "")) {
    context <- fresh_ctx
  }

  assistant <- initialize_assistant(
    context = context,
    mode = "inline",
    has_selection = has_selection,
    continue_chat = FALSE,
    chat = get_converser_chat(register_tools = FALSE)
  )

  turn <- construct_turn(
    user_prompt = input$text,
    context = context,
    include_env = TRUE,
    include_error = FALSE
  )

  # Map user-friendly labels ("Replace", "Insert Before", "Insert After") to internal streamy modes
  raw_iface <- input$interface %||% "Replace"
  internal_iface <- map_interface_mode(raw_iface)

  if (isTRUE(input$preview_diff) && has_selection) {
    cli_inform(c("i" = "ConverseR: Generating code for Preview Difference..."))
    raw_resp <- invoke_chat_with_fallback(assistant, turn, context = context, mode = "inline", has_selection = has_selection)
    proposed_code <- sanitize_llm_output(raw_resp, ext = file_extension(context$path))

    # Open the non-blocking Viewer pane diff reviewer instead of a nested runGadget()!
    # This completely eliminates RStudio's "Terminate R" modal popup bug.
    open_nonblocking_diff_review(
      original = sel_text,
      proposed = proposed_code,
      interface = internal_iface,
      prompt = input$text
    )
  } else {
    cli_inform(c("i" = "ConverseR: Streaming response into active document..."))
    stream_to_editor(
      generator = invoke_stream_with_fallback(assistant, turn, context = context, mode = "inline", has_selection = has_selection),
      context = context,
      interface = internal_iface
    )
    cli_inform(c("v" = "ConverseR: Done! (Run {.fn converser_undo} to revert in one step.)"))
  }

  converser_env[["last_inline_chat"]] <- assistant
  converser_env[["last_call_chat"]] <- assistant
  converser_env[["active_chat"]] <- assistant

  invisible(assistant)
}

map_interface_mode <- function(label) {
  lbl <- tolower(trimws(label %||% "replace"))
  if (lbl %in% c("insert before", "prefix")) return("prefix")
  if (lbl %in% c("insert after", "suffix")) return("suffix")
  "replace"
}

# Graceful fallback if a model rejects OpenRouter `reasoning` parameters
invoke_chat_with_fallback <- function(assistant, turn, context, mode, has_selection) {
  tryCatch(
    assistant$chat(turn, echo = "none"),
    error = function(e) {
      # Strip reasoning from extra_args and retry once in case model doesn't support reasoning effort
      strip_reasoning_from_chat(assistant)
      assistant$chat(turn, echo = "none")
    }
  )
}

invoke_stream_with_fallback <- function(assistant, turn, context, mode, has_selection) {
  tryCatch(
    assistant$stream(turn),
    error = function(e) {
      strip_reasoning_from_chat(assistant)
      assistant$stream(turn)
    }
  )
}

strip_reasoning_from_chat <- function(chat) {
  tryCatch(
    {
      prov <- chat$get_provider()
      if (!is.null(prov@extra_args$reasoning)) {
        prov@extra_args$reasoning <- NULL
      }
    },
    error = function(e) NULL
  )
}

converser_inline_gadget <- function(has_selection) {
  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        html, body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
          padding: 8px 12px; margin: 0; background: #ffffff; overflow-x: hidden;
        }
        .form-group { margin-bottom: 8px; width: 100%; }
        .rc-bar-row {
          display: flex; justify-content: space-between; align-items: center;
          gap: 10px; margin-top: 6px; width: 100%; flex-wrap: nowrap;
        }
        .rc-left-controls {
          display: flex; align-items: center; gap: 12px; font-size: 12.5px;
          color: #334155; flex-wrap: nowrap; min-width: 0;
        }
        .rc-left-controls .form-group { margin-bottom: 0; width: auto; }
        .rc-left-controls .checkbox { margin-top: 0; margin-bottom: 0; white-space: nowrap; }
        .rc-submit-btn { white-space: nowrap; flex-shrink: 0; font-weight: 600; }
      ")),
      shiny::tags$script(shiny::HTML("
        $(document).on('keyup', function(e) {
          if (e.key === 'Enter' && !e.shiftKey) {
            Shiny.setInputValue('done_enter', Math.random(), {priority: 'event'});
          } else if (e.key === 'Escape') {
            Shiny.setInputValue('cancel_esc', Math.random(), {priority: 'event'});
          }
        });
        $(document).ready(function() {
          $('#prompt_text').focus();
        });
      "))
    ),
    miniUI::miniContentPanel(
      padding = 4,
      shiny::textInput(
        "prompt_text",
        label = NULL,
        placeholder = "What would you like to do? (Press Enter to run)",
        width = "100%"
      ),
      shiny::div(
        class = "rc-bar-row",
        if (has_selection) {
          shiny::div(
            class = "rc-left-controls",
            shiny::selectInput(
              "interface",
              label = NULL,
              choices = c("Replace", "Insert Before", "Insert After"),
              selected = converser_env[["last_interface"]] %||% "Replace",
              width = "135px"
            ),
            shiny::checkboxInput(
              "preview_diff",
              "Preview Difference first",
              value = isTRUE(converser_env[["last_preview_diff"]])
            )
          )
        } else {
          shiny::span(style = "font-size: 12px; color: #64748b;", "Inserts at cursor (Press Esc to cancel)")
        },
        shiny::actionButton("done_btn", "Generate", class = "btn-primary btn-sm rc-submit-btn")
      )
    )
  )

  server <- function(input, output, session) {
    handle_submit <- function() {
      iface <- if (has_selection) (input$interface %||% "Replace") else "Insert Before"
      converser_env[["last_interface"]] <- iface
      if (has_selection) {
        converser_env[["last_preview_diff"]] <- isTRUE(input$preview_diff)
      }
      res <- list(
        text = input$prompt_text %||% "",
        interface = iface,
        preview_diff = isTRUE(input$preview_diff)
      )
      converser_env[["inline_submitted_input"]] <- res
      shiny::stopApp(returnValue = res)
    }

    shiny::observeEvent(input$done_enter, handle_submit())
    shiny::observeEvent(input$done_btn, handle_submit())
    shiny::observeEvent(input$cancel_esc, {
      converser_env[["inline_submitted_input"]] <- NULL
      shiny::stopApp(returnValue = NULL)
    })
  }

  viewer <- shiny::dialogViewer(
    "ConverseR Inline Edit",
    width = 520,
    height = if (has_selection) 135 else 115
  )
  shiny::runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}

open_nonblocking_diff_review <- function(original, proposed, interface = "replace", prompt = "") {
  # Append a special diff review card into the non-blocking sidebar and open it!
  msgs <- converser_env[["chat_history_ui"]] %||% list()

  user_html <- commonmark::markdown_html(paste0("*(Inline Edit Preview)*: ", prompt), extensions = TRUE)
  code_md <- paste0("```r\n", proposed, "\n```")
  assistant_html <- commonmark::markdown_html(code_md, extensions = TRUE)

  msgs <- c(
    msgs,
    list(
      list(role = "user", html = user_html),
      list(role = "assistant", html = assistant_html, auto_open_diff = TRUE)
    )
  )
  converser_env[["chat_history_ui"]] <- msgs

  url <- ensure_nonblocking_server()
  if (rstudioapi::isAvailable() && rstudioapi::hasFun("viewer")) {
    rstudioapi::viewer(url)
    cli_inform(c(
      "v" = "ConverseR: Opened Preview Difference in the Viewer pane.",
      "i" = "Review the red/green diff and click 'Replace', 'Insert', or 'Run in Console' when ready."
    ))
  } else {
    utils::browseURL(url)
  }
}
