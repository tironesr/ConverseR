# Clean, Single-View Non-Blocking ConverseR Sidebar with Settings & SKILL.md --

#' Open the ConverseR Sidebar in the RStudio Viewer Pane
#'
#' @description
#' Launches the clean, single-view, **non-blocking** ConverseR chat sidebar in
#' the RStudio Viewer pane (`converser()` or `converser_sidebar()`).
#'
#' - **Zero console blocking & respects your layout**: Runs in the background via
#'   `httpuv` and opens in the Viewer pane *without* minimizing your Console.
#' - **Persistent Cross-Session Settings**: Click the model badge or `Settings`
#'   button in the header to set your OpenRouter API key, choose from the top 20
#'   OpenRouter models (defaulting to `deepseek/deepseek-v4.1-flash`), configure
#'   Zero Data Retention (`ZDR`), reasoning effort (`none`, `minimal`, `low`,
#'   `medium`, `high`, `default`), upload/edit a `SKILL.md` file or `.zip` skill
#'   bundle, and save them as your persistent defaults across R sessions.
#' - **Share Plot & Fix Error**: Toggle **`Share Plot`** to attach your active
#'   RStudio plot to any question, or click **`Fix Error`** to diagnose R errors
#'   and either **`Run in Console`**, **`Replace`**, **`Insert`**, or **`Copy`**.
#'
#' @param viewer `"pane"` (RStudio Viewer pane, default) or `"browser"`.
#' @export
converser <- function(viewer = c("pane", "browser")) {
  converser_sidebar(viewer = viewer)
}

#' @rdname converser
#' @export
converser_sidebar <- function(viewer = c("pane", "browser")) {
  viewer <- arg_match(viewer)

  chat <- converser_env[["active_chat"]] %||% get_converser_chat(register_tools = TRUE)
  if (!is.null(chat)) {
    ctx <- get_safe_editor_context()
    chat$set_system_prompt(construct_system_prompt(context = ctx, mode = "sidebar"))
    converser_env[["active_chat"]] <- chat
  }

  if (is.null(converser_env[["chat_history_ui"]])) {
    converser_env[["chat_history_ui"]] <- list()
  }

  url <- ensure_nonblocking_server()

  if (identical(viewer, "pane") && rstudioapi::isAvailable() && rstudioapi::hasFun("viewer")) {
    # Do NOT pass height = "maximize" so RStudio never minimizes the pane above the Viewer!
    rstudioapi::viewer(url)
    install_viewer_guard(url)
    cli_inform(c(
      "v" = "ConverseR is open in the Viewer pane.",
      "i" = "Your R console is free: keep coding while you chat!"
    ))
  } else {
    uninstall_viewer_guard()
    utils::browseURL(url)
    cli_inform(c("v" = "ConverseR opened in browser ({.url {url}})."))
  }

  invisible(url)
}

#' Stop the background ConverseR Sidebar server
#' @export
converser_stop_sidebar <- function() {
  uninstall_viewer_guard()
  srv <- converser_env[["httpuv_server"]]
  if (!is.null(srv)) {
    tryCatch(httpuv::stopServer(srv), error = function(e) NULL)
    converser_env[["httpuv_server"]] <- NULL
    converser_env[["httpuv_port"]] <- NULL
    cli_inform(c("v" = "Stopped ConverseR background sidebar server."))
  }
  invisible(NULL)
}

# Viewer Pane Replacement Interception & Deferred Task Callback ---------------

install_viewer_guard <- function(converser_url) {
  converser_env[["sidebar_active_url"]] <- converser_url
  converser_env[["viewer_overwritten"]] <- FALSE
  ensure_viewer_task_callback()

  # 1. Wrap getOption("viewer")
  orig_viewer <- converser_env[["rstudio_viewer_orig"]]
  if (is.null(orig_viewer)) {
    orig_viewer <- getOption("viewer")
    if (is.null(orig_viewer)) {
      orig_viewer <- function(url, height = NULL) {
        if (rstudioapi::isAvailable() && rstudioapi::hasFun("viewer")) {
          rstudioapi::viewer(url, height = height)
        } else {
          utils::browseURL(url)
        }
      }
    }
    converser_env[["rstudio_viewer_orig"]] <- orig_viewer
  }

  wrapped_viewer <- function(url, height = NULL) {
    active_url <- converser_env[["sidebar_active_url"]] %||% ""
    clean_host <- sub("^https?://", "", active_url)
    is_converser <- nzchar(clean_host) && grepl(clean_host, url, fixed = TRUE)

    res <- tryCatch(
      if (is.null(height)) orig_viewer(url) else orig_viewer(url, height = height),
      error = function(e) NULL
    )

    if (!is_converser) {
      converser_env[["viewer_overwritten"]] <- TRUE
    } else {
      converser_env[["viewer_overwritten"]] <- FALSE
    }

    invisible(res)
  }

  options(viewer = wrapped_viewer)

  # 2. Wrap rstudioapi::viewer in namespace if available
  if (is.null(converser_env[["rstudioapi_viewer_orig"]])) {
    orig_api_viewer <- tryCatch(rstudioapi::viewer, error = function(e) NULL)
    if (!is.null(orig_api_viewer)) {
      converser_env[["rstudioapi_viewer_orig"]] <- orig_api_viewer
      wrapped_api_viewer <- function(url, height = NULL) {
        active_url <- converser_env[["sidebar_active_url"]] %||% ""
        clean_host <- sub("^https?://", "", active_url)
        is_converser <- nzchar(clean_host) && grepl(clean_host, url, fixed = TRUE)

        res <- tryCatch(
          if (is.null(height)) orig_api_viewer(url) else orig_api_viewer(url, height = height),
          error = function(e) NULL
        )

        if (!is_converser) {
          converser_env[["viewer_overwritten"]] <- TRUE
        } else {
          converser_env[["viewer_overwritten"]] <- FALSE
        }

        invisible(res)
      }

      tryCatch({
        unlockBinding("viewer", asNamespace("rstudioapi"))
        assign("viewer", wrapped_api_viewer, envir = asNamespace("rstudioapi"))
      }, error = function(e) NULL)
    }
  }

  # 3. Wrap .rs.api.viewer in tools:rstudio if present
  if ("tools:rstudio" %in% search() && is.null(converser_env[["tools_rs_viewer_orig"]])) {
    tools_env <- as.environment("tools:rstudio")
    if (exists(".rs.api.viewer", envir = tools_env, inherits = FALSE)) {
      orig_rs_viewer <- get(".rs.api.viewer", envir = tools_env)
      converser_env[["tools_rs_viewer_orig"]] <- orig_rs_viewer
      wrapped_rs_viewer <- function(url, height = NULL) {
        active_url <- converser_env[["sidebar_active_url"]] %||% ""
        clean_host <- sub("^https?://", "", active_url)
        is_converser <- nzchar(clean_host) && grepl(clean_host, url, fixed = TRUE)

        res <- tryCatch(
          if (is.null(height)) orig_rs_viewer(url) else orig_rs_viewer(url, height = height),
          error = function(e) NULL
        )

        if (!is_converser) {
          converser_env[["viewer_overwritten"]] <- TRUE
        } else {
          converser_env[["viewer_overwritten"]] <- FALSE
        }

        invisible(res)
      }

      tryCatch({
        if (bindingIsLocked(".rs.api.viewer", tools_env)) {
          unlockBinding(".rs.api.viewer", tools_env)
        }
        assign(".rs.api.viewer", wrapped_rs_viewer, envir = tools_env)
      }, error = function(e) NULL)
    }
  }

  invisible(NULL)
}

uninstall_viewer_guard <- function() {
  remove_viewer_task_callback()
  orig_viewer <- converser_env[["rstudio_viewer_orig"]]
  if (!is.null(orig_viewer)) {
    options(viewer = orig_viewer)
    converser_env[["rstudio_viewer_orig"]] <- NULL
  }

  orig_api_viewer <- converser_env[["rstudioapi_viewer_orig"]]
  if (!is.null(orig_api_viewer)) {
    tryCatch({
      unlockBinding("viewer", asNamespace("rstudioapi"))
      assign("viewer", orig_api_viewer, envir = asNamespace("rstudioapi"))
      converser_env[["rstudioapi_viewer_orig"]] <- NULL
    }, error = function(e) NULL)
  }

  orig_rs_viewer <- converser_env[["tools_rs_viewer_orig"]]
  if (!is.null(orig_rs_viewer) && "tools:rstudio" %in% search()) {
    tryCatch({
      tools_env <- as.environment("tools:rstudio")
      if (bindingIsLocked(".rs.api.viewer", tools_env)) {
        unlockBinding(".rs.api.viewer", tools_env)
      }
      assign(".rs.api.viewer", orig_rs_viewer, envir = tools_env)
      converser_env[["tools_rs_viewer_orig"]] <- NULL
    }, error = function(e) NULL)
  }

  converser_env[["viewer_overwritten"]] <- FALSE
  invisible(NULL)
}

ensure_viewer_task_callback <- function() {
  cb_names <- getTaskCallbackNames()
  if (!("converser_viewer_guard" %in% cb_names)) {
    addTaskCallback(converser_viewer_callback_handler, name = "converser_viewer_guard")
  }
  invisible(NULL)
}

remove_viewer_task_callback <- function() {
  cb_names <- getTaskCallbackNames()
  if ("converser_viewer_guard" %in% cb_names) {
    tryCatch(removeTaskCallback("converser_viewer_guard"), error = function(e) NULL)
  }
  invisible(NULL)
}

converser_viewer_callback_handler <- function(expr, value, ok, visible) {
  if (isTRUE(converser_env[["viewer_overwritten"]])) {
    converser_env[["viewer_overwritten"]] <- FALSE
    tryCatch(
      cli_inform(c(
        "i" = "The Viewer tab is now displaying HTML output.",
        "i" = "Click {.run [converser()](ConverseR::converser())} to restore your ConverseR chat."
      )),
      error = function(e) {
        message("[ConverseR] The Viewer tab is now displaying HTML output.\nType converser() to restore your ConverseR chat.")
      }
    )
  }
  TRUE
}

ensure_nonblocking_server <- function() {
  existing_srv <- converser_env[["httpuv_server"]]
  existing_port <- converser_env[["httpuv_port"]]
  if (!is.null(existing_srv) && !is.null(existing_port)) {
    still_running <- tryCatch(existing_srv$isRunning(), error = function(e) FALSE)
    if (isTRUE(still_running)) {
      return(sprintf("http://127.0.0.1:%d/", existing_port))
    }
    tryCatch(httpuv::stopServer(existing_srv), error = function(e) NULL)
  }

  port <- httpuv::randomPort(min = 4100L, max = 8900L)
  srv <- httpuv::startServer(
    host = "127.0.0.1",
    port = port,
    app = list(call = handle_sidebar_http_request)
  )

  converser_env[["httpuv_server"]] <- srv
  converser_env[["httpuv_port"]] <- port

  sprintf("http://127.0.0.1:%d/", port)
}

get_safe_editor_context <- function() {
  if (!rstudioapi::isAvailable()) return(NULL)
  tryCatch(
    rstudioapi::getSourceEditorContext(),
    error = function(e) {
      tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e2) NULL)
    }
  )
}

has_active_selection <- function() {
  ctx <- get_safe_editor_context()
  if (is.null(ctx)) return(FALSE)
  sel_text <- tryCatch(rstudioapi::primary_selection(ctx)$text %||% "", error = function(e) "")
  nzchar(sel_text)
}

# HTTP Request Router (`httpuv`) -----------------------------------------------

handle_sidebar_http_request <- function(req) {
  # Snapshot geterrmessage() immediately upon any incoming request before any
  # tryCatch() in the HTTP router can touch R's internal error message buffer!
  raw_err <- trimws(geterrmessage())
  if (nzchar(raw_err) && !grepl("Can't show last error because no error was recorded", raw_err, fixed = TRUE)) {
    converser_env[["last_real_errmessage"]] <- raw_err
  }

  path <- req$PATH_INFO %||% "/"
  method <- req$REQUEST_METHOD %||% "GET"

  if (identical(method, "OPTIONS")) {
    return(list(
      status = 200L,
      headers = list(
        "Access-Control-Allow-Origin" = "*",
        "Access-Control-Allow-Methods" = "GET, POST, OPTIONS",
        "Access-Control-Allow-Headers" = "Content-Type"
      ),
      body = ""
    ))
  }

  if (identical(method, "GET") && (identical(path, "/") || identical(path, "/index.html"))) {
    return(list(
      status = 200L,
      headers = list(
        "Content-Type" = "text/html; charset=utf-8",
        "Cache-Control" = "no-store"
      ),
      body = sidebar_single_page_html()
    ))
  }

  body_data <- list()
  if (identical(method, "POST") && !is.null(req$rook.input)) {
    raw_lines <- tryCatch(req$rook.input$read_lines(), error = function(e) character(0))
    raw_str <- paste(raw_lines, collapse = "\n")
    if (nzchar(trimws(raw_str))) {
      body_data <- tryCatch(
        jsonlite::fromJSON(raw_str, simplifyVector = TRUE),
        error = function(e) list()
      )
    }
  }

  res_payload <- tryCatch(
    dispatch_api_route(path, body_data),
    error = function(e) list(ok = FALSE, error = conditionMessage(e))
  )

  list(
    status = 200L,
    headers = list(
      "Content-Type" = "application/json; charset=utf-8",
      "Cache-Control" = "no-store"
    ),
    body = jsonlite::toJSON(res_payload, auto_unbox = TRUE, null = "null")
  )
}

dispatch_api_route <- function(path, body) {
  if (identical(path, "/api/state")) {
    return(api_get_state())
  }
  if (identical(path, "/api/selection")) {
    return(list(ok = TRUE, has_selection = has_active_selection()))
  }
  if (identical(path, "/api/settings")) {
    return(list(ok = TRUE, settings = inspect_current_settings()))
  }
  if (identical(path, "/api/save_settings")) {
    return(api_post_save_settings(body))
  }
  if (identical(path, "/api/save_skill_file")) {
    return(api_post_save_skill_file(body))
  }
  if (identical(path, "/api/load_skill_path")) {
    return(api_post_load_skill_path(body))
  }
  if (identical(path, "/api/upload_skill_zip")) {
    return(api_post_upload_skill_zip(body))
  }
  if (identical(path, "/api/chat")) {
    return(api_post_chat(body))
  }
  if (identical(path, "/api/diff")) {
    return(api_post_diff(body))
  }
  if (identical(path, "/api/apply")) {
    return(api_post_apply(body))
  }
  if (identical(path, "/api/console")) {
    return(api_post_console(body))
  }
  if (identical(path, "/api/copy")) {
    return(api_post_copy(body))
  }
  if (identical(path, "/api/guide")) {
    converser_guide(browser = TRUE)
    return(list(ok = TRUE, message = "Opened Getting Started Guide in your browser"))
  }
  if (identical(path, "/api/undo")) {
    converser_undo()
    return(list(ok = TRUE, message = "Reverted last edit."))
  }
  if (identical(path, "/api/reset")) {
    converser_reset()
    chat <- get_converser_chat(register_tools = TRUE)
    if (!is.null(chat)) {
      chat$set_system_prompt(construct_system_prompt(context = get_safe_editor_context(), mode = "sidebar"))
      converser_env[["active_chat"]] <- chat
    }
    return(list(ok = TRUE))
  }

  list(ok = FALSE, error = paste("Unknown route:", path))
}

# API Endpoints ----------------------------------------------------------------

api_get_state <- function() {
  chat <- converser_env[["active_chat"]] %||% get_converser_chat(register_tools = TRUE)
  model_name <- if (!is.null(chat)) {
    tryCatch(chat$get_model(), error = function(e) "deepseek-v4.1-flash")
  } else {
    "deepseek-v4.1-flash"
  }

  model_short <- sub("^[^/]+/", "", model_name)
  msgs <- converser_env[["chat_history_ui"]] %||% list()

  list(
    ok = TRUE,
    model = model_short,
    full_model = model_name,
    has_selection = has_active_selection(),
    has_skill = nzchar(trimws(get_active_skill_text())),
    settings = inspect_current_settings(),
    messages = msgs
  )
}

api_post_save_settings <- function(body) {
  api_key <- trimws(body$openrouter_api_key %||% "")

  model <- trimws(body$model %||% "deepseek/deepseek-v4.1-flash")
  if (!nzchar(model)) model <- "deepseek/deepseek-v4.1-flash"
  reasoning_effort <- body$reasoning_effort %||% "low"
  zdr <- isTRUE(body$zdr)
  data_collection <- if (identical(body$data_collection, "allow")) "allow" else "deny"
  allow_fallbacks <- if (is.null(body$allow_fallbacks)) TRUE else isTRUE(body$allow_fallbacks)
  system_prompt <- body$system_prompt %||% ""
  skill_text <- body$skill_text %||% ""
  skill_file <- trimws(body$skill_file %||% "")
  persist <- if (is.null(body$persist)) TRUE else isTRUE(body$persist)

  options(
    converser.style = "",
    converser.system_prompt = system_prompt,
    converser.skill_text = skill_text,
    converser.skill_file = if (nzchar(skill_file)) skill_file else NULL
  )

  chat <- converser_use_openrouter(
    model = model,
    reasoning_effort = reasoning_effort,
    zdr = zdr,
    data_collection = data_collection,
    allow_fallbacks = allow_fallbacks,
    system_prompt = system_prompt,
    skill_text = skill_text,
    skill_file = if (nzchar(skill_file)) skill_file else NULL,
    api_key = api_key,
    persist = persist
  )

  ctx <- get_safe_editor_context()
  chat_with_tools <- get_converser_chat(register_tools = TRUE)
  if (!is.null(chat_with_tools)) {
    chat_with_tools$set_system_prompt(construct_system_prompt(context = ctx, mode = "sidebar"))
    converser_env[["active_chat"]] <- chat_with_tools
    converser_env[["last_call_chat"]] <- chat_with_tools
  }

  model_short <- sub("^[^/]+/", "", model)
  persist_note <- if (persist) " (saved as default)" else " (session only)"
  list(
    ok = TRUE,
    model = model_short,
    full_model = model,
    has_skill = nzchar(trimws(skill_text)),
    message = sprintf("Active: %s | reasoning: %s%s", model_short, reasoning_effort, persist_note)
  )
}

api_post_save_skill_file <- function(body) {
  target_path <- trimws(body$path %||% "SKILL.md")
  if (!nzchar(target_path)) target_path <- "SKILL.md"
  target_exp <- path.expand(target_path)
  content <- body$skill_text %||% ""

  writeLines(content, con = target_exp, useBytes = TRUE)
  options(
    converser.skill_file = target_exp,
    converser.skill_text = content
  )

  if (!is.null(converser_env[["active_chat"]])) {
    ctx <- get_safe_editor_context()
    converser_env[["active_chat"]]$set_system_prompt(
      construct_system_prompt(context = ctx, mode = "sidebar")
    )
  }

  list(
    ok = TRUE,
    path = normalizePath(target_exp, winslash = "/", mustWork = FALSE),
    message = sprintf("Saved SKILL.md to %s", basename(target_exp))
  )
}

api_post_load_skill_path <- function(body) {
  target_path <- trimws(body$path %||% "")
  if (!nzchar(target_path)) {
    return(list(ok = FALSE, error = "Please enter a path to SKILL.md, a folder, or a .zip bundle."))
  }
  target_exp <- path.expand(target_path)
  if (!file.exists(target_exp)) {
    return(list(ok = FALSE, error = sprintf("File or folder not found: %s", target_exp)))
  }
  txt <- read_skill_files(target_exp)
  options(
    converser.skill_file = target_exp,
    converser.skill_text = txt
  )
  bundled <- converser_env[["skill_bundle_files"]] %||% character(0)
  list(
    ok = TRUE,
    skill_text = txt,
    bundled_files = bundled,
    path = normalizePath(target_exp, winslash = "/", mustWork = FALSE),
    message = sprintf("Loaded %s (%d files, %d chars)", basename(target_exp), max(1L, length(bundled)), nchar(txt))
  )
}

api_post_upload_skill_zip <- function(body) {
  b64 <- body$base64 %||% ""
  fname <- basename(body$filename %||% "skill_bundle.zip")
  if (!nzchar(b64)) {
    return(list(ok = FALSE, error = "Empty ZIP upload payload."))
  }

  # Strip data URI prefix if present
  b64_clean <- sub("^data:[^;]+;base64,", "", b64)
  raw_bytes <- jsonlite::base64_dec(b64_clean)

  # Store uploaded skill zip inside the persistent config folder so it remains available across sessions
  cfg_dir <- dirname(converser_config_path())
  dir.create(cfg_dir, recursive = TRUE, showWarnings = FALSE)
  saved_zip <- file.path(cfg_dir, fname)
  writeBin(raw_bytes, saved_zip)

  txt <- read_skill_files(saved_zip)
  options(
    converser.skill_file = saved_zip,
    converser.skill_text = txt
  )
  bundled <- converser_env[["skill_bundle_files"]] %||% character(0)

  list(
    ok = TRUE,
    skill_text = txt,
    bundled_files = bundled,
    path = normalizePath(saved_zip, winslash = "/", mustWork = FALSE),
    message = sprintf("Extracted ZIP bundle '%s' (%d files, %d chars)", fname, length(bundled), nchar(txt))
  )
}

api_post_chat <- function(body) {
  prompt_str <- trimws(body$prompt %||% "")
  if (!nzchar(prompt_str)) {
    return(list(ok = FALSE, error = "Empty prompt."))
  }

  inc_err <- isTRUE(body$inc_err)
  inc_plot <- isTRUE(body$inc_plot)
  force_whole_doc <- isTRUE(body$force_whole_doc)

  ctx <- get_safe_editor_context()
  chat <- converser_env[["active_chat"]] %||% get_converser_chat(register_tools = TRUE)
  if (is.null(chat)) {
    return(list(ok = FALSE, error = "No OpenRouter API key configured yet. Click Settings in the top bar to enter your key and save your defaults."))
  }

  chat$set_system_prompt(construct_system_prompt(context = ctx, mode = "sidebar"))
  prune_chat_turn_payloads(chat)
  converser_env[["active_chat"]] <- chat

  # If this chat object has 0 prior turns, ensure the context cache starts clean
  existing_turns <- tryCatch(chat$get_turns(), error = function(e) list())
  if (length(existing_turns) == 0L) {
    converser_clear_context_cache()
  }

  # Snapshot cache hashes in case the LLM request errors out or is aborted
  prev_doc_hash <- converser_env[["cached_doc_hash"]]
  prev_doc_path <- converser_env[["cached_doc_path"]]
  prev_doc_lines <- converser_env[["cached_doc_lines"]]
  prev_manifest_hash <- converser_env[["cached_manifest_hash"]]
  prev_env_hashes <- converser_env[["cached_env_hashes"]]

  plot_data_uri <- NULL
  plot_content_obj <- NULL
  if (inc_plot) {
    plot_content_obj <- tryCatch(
      ellmer::content_image_plot(width = 700, height = 480),
      error = function(e) NULL
    )
    plot_data_uri <- tryCatch(
      capture_active_plot_data_uri(),
      error = function(e) NULL
    )
  }

  msgs <- converser_env[["chat_history_ui"]] %||% list()
  user_html <- commonmark::markdown_html(prompt_str, extensions = TRUE)
  if (!is.null(plot_data_uri)) {
    user_html <- paste0(
      user_html,
      sprintf("<div style='margin-top:6px;'><img src='%s' style='max-width:100%%; border-radius:6px; border:1px solid #cbd5e1;'/></div>", plot_data_uri)
    )
  }

  msgs <- c(msgs, list(list(role = "user", html = user_html)))
  converser_env[["chat_history_ui"]] <- msgs

  turn_payload <- construct_turn(
    user_prompt = prompt_str,
    context = ctx,
    include_env = TRUE,
    include_error = inc_err,
    force_whole_doc = force_whole_doc,
    cache_context = TRUE
  )

  resp_text <- tryCatch(
    {
      if (!is.null(plot_content_obj)) {
        tryCatch(
          chat$chat(plot_content_obj, turn_payload, echo = "none"),
          error = function(vis_err) {
            chat$chat(
              paste0(
                "[Note: The active model could not process raw image bytes (",
                conditionMessage(vis_err),
                "). Please respond based on the ggplot2/graphics code and environment objects below.]\n\n",
                turn_payload
              ),
              echo = "none"
            )
          }
        )
      } else {
        chat$chat(turn_payload, echo = "none")
      }
    },
    error = function(e) {
      # First try stripping reasoning effort in case a non-reasoning model rejected it
      tryCatch(
        {
          strip_reasoning_from_chat(chat)
          chat$chat(turn_payload, echo = "none")
        },
        error = function(e_reason) {
          tryCatch(
            {
              fallback_chat <- get_converser_chat(register_tools = FALSE)
              strip_reasoning_from_chat(fallback_chat)
              fallback_chat$set_system_prompt(construct_system_prompt(context = ctx, mode = "sidebar"))
              converser_clear_context_cache()
              fresh_turn <- construct_turn(
                user_prompt = prompt_str,
                context = ctx,
                include_env = TRUE,
                include_error = inc_err,
                force_whole_doc = force_whole_doc,
                cache_context = TRUE
              )
              res_fb <- fallback_chat$chat(fresh_turn, echo = "none")
              chat <<- fallback_chat
              res_fb
            },
            error = function(e2) {
              # Restore cache state if the turn failed completely
              converser_env[["cached_doc_hash"]] <- prev_doc_hash
              converser_env[["cached_doc_path"]] <- prev_doc_path
              converser_env[["cached_doc_lines"]] <- prev_doc_lines
              converser_env[["cached_manifest_hash"]] <- prev_manifest_hash
              converser_env[["cached_env_hashes"]] <- prev_env_hashes
              paste0("**Error communicating with LLM:** ", conditionMessage(e))
            }
          )
        }
      )
    }
  )

  if (!grepl("^\\*\\*Error communicating with LLM:\\*\\*", resp_text)) {
    prune_chat_turn_payloads(chat, latest_clean_prompt = prompt_str)
  }

  converser_env[["active_chat"]] <- chat
  converser_env[["last_call_chat"]] <- chat

  clean_md <- sanitize_llm_output(resp_text, ext = "md", strip_fences = FALSE)
  assistant_html <- commonmark::markdown_html(clean_md, extensions = TRUE)

  msgs <- c(msgs, list(list(role = "assistant", html = assistant_html, raw_md = clean_md)))
  converser_env[["chat_history_ui"]] <- msgs

  list(
    ok = TRUE,
    has_selection = has_active_selection(),
    messages = msgs
  )
}

api_post_diff <- function(body) {
  ctx <- get_safe_editor_context()
  sel_text <- if (!is.null(ctx)) (rstudioapi::primary_selection(ctx)$text %||% "") else ""
  proposed <- body$code %||% ""

  diff_html <- as.character(render_code_diff_html(sel_text, proposed))
  list(
    ok = TRUE,
    has_selection = nzchar(sel_text),
    diff_html = diff_html
  )
}

api_post_apply <- function(body) {
  mode <- body$mode %||% "replace"
  code_to_write <- body$code %||% ""
  if (!nzchar(code_to_write)) {
    return(list(ok = FALSE, error = "No code to apply."))
  }

  ctx <- get_safe_editor_context()
  if (is.null(ctx)) {
    return(list(ok = FALSE, error = "Please click inside an R script in the Source Editor first (or click 'Run in Console')."))
  }

  sel <- rstudioapi::primary_selection(ctx)
  if (identical(mode, "replace") && !nzchar(sel$text %||% "")) {
    return(list(ok = FALSE, error = "Nothing is highlighted in the script to replace. Highlight code first, click 'Run in Console', or click 'Insert'."))
  }

  converser_env[["last_edit"]] <- list(
    doc_id = ctx$id,
    full_contents = ctx$contents,
    original_range = sel$range,
    original_text = sel$text
  )

  if (identical(mode, "replace")) {
    rstudioapi::modifyRange(location = sel$range, text = code_to_write, id = ctx$id)
    msg <- "Replaced selection in script (Undo available)"
  } else if (identical(mode, "prefix")) {
    insert_pos <- sel$range$start
    rstudioapi::insertText(location = insert_pos, text = paste0(code_to_write, "\n"), id = ctx$id)
    msg <- "Inserted before selection in script (Undo available)"
  } else {
    insert_pos <- sel$range$end
    rstudioapi::insertText(location = insert_pos, text = paste0("\n", code_to_write), id = ctx$id)
    msg <- "Inserted into script (Undo available)"
  }

  list(ok = TRUE, message = msg, has_selection = has_active_selection())
}

# Execute code directly in the R Console without touching the active script
api_post_console <- function(body) {
  code_to_run <- body$code %||% ""
  execute_now <- if (is.null(body$execute)) TRUE else isTRUE(body$execute)

  if (!nzchar(trimws(code_to_run))) {
    return(list(ok = FALSE, error = "No code to send to the R Console."))
  }

  if (rstudioapi::isAvailable() && rstudioapi::hasFun("sendToConsole")) {
    rstudioapi::sendToConsole(code_to_run, execute = execute_now, echo = TRUE)
    msg <- if (execute_now) {
      "Executed directly in R Console (script untouched)"
    } else {
      "Pasted into R Console input line"
    }
    return(list(ok = TRUE, message = msg))
  }

  list(ok = FALSE, error = "RStudio Console API is not available in this session.")
}

# Copy text to the system clipboard via R (fallback for RStudio Viewer iframe restrictions)
api_post_copy <- function(body) {
  txt <- body$text %||% ""
  if (!nzchar(txt)) {
    return(list(ok = FALSE, error = "Nothing to copy."))
  }
  copied <- FALSE
  if (.Platform$OS.type == "windows" && exists("writeClipboard", where = asNamespace("utils"), mode = "function")) {
    tryCatch(
      {
        utils::writeClipboard(txt, format = 1L)
        copied <- TRUE
      },
      error = function(e) NULL
    )
  }
  list(ok = copied, message = "Copied to clipboard")
}

capture_active_plot_data_uri <- function() {
  img_obj <- ellmer::content_image_plot(width = 650, height = 420)
  b64 <- tryCatch(img_obj@data, error = function(e) NULL)
  mime <- tryCatch(img_obj@type, error = function(e) "image/png")
  if (!is.null(b64) && nzchar(b64)) {
    return(paste0("data:", mime, ";base64,", b64))
  }
  NULL
}

# Clean Single-View Sidebar UI + Slide-Over Settings / SKILL.md Drawer ---------

sidebar_single_page_html <- function() {
  '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8"/>
<meta name="viewport" content="width=device-width, initial-scale=1"/>
<title>ConverseR</title>
<style>
  :root {
    --primary: #2563eb;
    --primary-hover: #1d4ed8;
    --bg: #ffffff;
    --surface: #f8fafc;
    --border: #e2e8f0;
    --text: #0f172a;
    --muted: #64748b;
  }
  * { box-sizing: border-box; }
  body {
    margin: 0; padding: 8px 10px;
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    font-size: 13px; background: var(--bg); color: var(--text);
    display: flex; flex-direction: column; height: 100vh; position: relative;
  }
  /* Compact Header */
  .rc-header {
    display: flex; justify-content: space-between; align-items: center;
    padding-bottom: 6px; border-bottom: 1px solid var(--border); flex-shrink: 0;
  }
  .rc-title { font-weight: 700; font-size: 13.5px; display: flex; align-items: center; gap: 5px; }
  .rc-badge {
    font-size: 10px; background: #eff6ff; color: #1d4ed8; border: 1px solid #bfdbfe;
    padding: 2px 7px; border-radius: 999px; font-weight: 600; cursor: pointer;
    max-width: 140px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
  }
  .rc-badge:hover { background: #dbeafe; }
  .rc-skill-dot {
    font-size: 9.5px; background: #dcfce7; color: #166534; border: 1px solid #86efac;
    padding: 1px 5px; border-radius: 999px; font-weight: 600; display: none; cursor: pointer;
  }
  .rc-skill-dot.active { display: inline-block; }
  .btn-icon {
    border: 1px solid var(--border); background: #fff; color: var(--muted);
    padding: 2px 7px; border-radius: 5px; font-size: 11px; cursor: pointer;
  }
  .btn-icon:hover { background: var(--surface); color: var(--text); }

  /* Main Chat Stream */
  .rc-chat-box {
    flex: 1; overflow-y: auto; padding: 8px 2px; min-height: 0; scroll-behavior: smooth;
  }
  .rc-empty {
    color: var(--muted); text-align: center; padding: 48px 16px; line-height: 1.5;
  }

  /* Chat Messages */
  .rc-msg { margin-bottom: 10px; padding: 8px 10px; border-radius: 8px; line-height: 1.45; overflow-wrap: anywhere; position: relative; }
  .rc-msg-user { background: #eff6ff; color: #1e3a8a; border-bottom-right-radius: 2px; }
  .rc-msg-assistant { background: var(--surface); border: 1px solid var(--border); border-bottom-left-radius: 2px; }
  .rc-msg p { margin: 4px 0; }
  .rc-msg p:first-child { margin-top: 0; }
  .rc-msg p:last-child { margin-bottom: 0; }

  /* Interactive Code Blocks with Built-In Console, Diff & Apply Toolbar */
  .rc-code-wrapper {
    margin: 8px 0; border: 1px solid #cbd5e1; border-radius: 6px; overflow: hidden; background: #0f172a;
  }
  .rc-code-toolbar {
    display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 4px;
    background: #1e293b; padding: 4px 8px; border-bottom: 1px solid #334155;
  }
  .rc-code-lang { font-size: 10px; color: #94a3b8; font-family: Consolas, monospace; text-transform: uppercase; font-weight: 600; }
  .rc-code-actions { display: flex; gap: 4px; flex-wrap: wrap; }
  .rc-code-btn {
    border: 1px solid #475569; background: #334155; color: #f8fafc;
    padding: 2px 7px; border-radius: 4px; font-size: 10.5px; cursor: pointer; font-weight: 500;
    transition: opacity 0.15s, background 0.15s;
  }
  .rc-code-btn:hover:not(:disabled) { background: #475569; }
  .rc-code-btn-primary { background: #2563eb; border-color: #3b82f6; }
  .rc-code-btn-primary:hover:not(:disabled) { background: #1d4ed8; }
  .rc-code-btn-console { background: #4f46e5; border-color: #6366f1; }
  .rc-code-btn-console:hover:not(:disabled) { background: #4338ca; }
  .rc-code-btn-diff { background: #0f766e; border-color: #14b8a6; }
  .rc-code-btn-diff:hover:not(:disabled) { background: #115e59; }
  .rc-code-btn:disabled {
    background: #1e293b !important;
    border-color: #334155 !important;
    color: #64748b !important;
    cursor: not-allowed !important;
    opacity: 0.65;
  }
  .rc-msg pre {
    margin: 0; padding: 8px 10px; color: #f8fafc; font-size: 11.5px;
    overflow-x: auto; font-family: Consolas, monospace; background: transparent;
  }
  .rc-msg code { font-family: Consolas, monospace; font-size: 11.5px; }

  /* Inline Diff Drawer inside Code Block */
  .rc-inline-diff {
    display: none; background: #ffffff; border-top: 2px solid #14b8a6; padding: 6px;
    font-family: Consolas, monospace; font-size: 11.5px; max-height: 220px; overflow: auto;
  }
  .rc-inline-diff.open { display: block; }
  .rc-diff-header {
    display: flex; justify-content: space-between; align-items: center;
    font-family: -apple-system, sans-serif; font-size: 11px; color: var(--muted);
    padding-bottom: 4px; margin-bottom: 4px; border-bottom: 1px solid var(--border);
  }
  .rc-diff-line { white-space: pre-wrap; padding: 1px 4px; line-height: 1.35; }
  .rc-diff-add { background-color: #dcfce7; color: #166534; }
  .rc-diff-del { background-color: #fee2e2; color: #991b1b; text-decoration: line-through; }
  .rc-diff-ctx { color: #64748b; }
  .rc-diff-sign { display: inline-block; width: 16px; user-select: none; font-weight: bold; }

  /* Compact Bottom Input Area */
  .rc-bottom {
    border-top: 1px solid var(--border); padding-top: 6px; flex-shrink: 0;
  }
  .rc-action-strip {
    display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;
  }
  .rc-pills { display: flex; gap: 5px; align-items: center; }
  .rc-pill-btn {
    border: 1px solid var(--border); background: var(--surface); color: #334155;
    padding: 2px 8px; border-radius: 999px; font-size: 11px; cursor: pointer; font-weight: 500;
    transition: all 0.12s;
  }
  .rc-pill-btn:hover { background: #eff6ff; border-color: #93c5fd; color: var(--primary); }
  .rc-pill-btn.active {
    background: #2563eb; border-color: #1d4ed8; color: #ffffff;
  }
  .rc-input-row { display: flex; gap: 6px; align-items: flex-end; }
  textarea, input[type="text"], input[type="password"], select {
    border: 1px solid #cbd5e1; border-radius: 6px;
    padding: 5px 8px; font-family: inherit; font-size: 12px; width: 100%;
  }
  textarea:focus, input[type="text"]:focus, input[type="password"]:focus, select:focus {
    outline: 2px solid #93c5fd; border-color: var(--primary);
  }
  .btn-send {
    background: var(--primary); color: #fff; border: none; border-radius: 6px;
    padding: 8px 12px; font-size: 12px; font-weight: 600; cursor: pointer; height: 36px;
    white-space: nowrap;
  }
  .btn-send:hover { background: var(--primary-hover); }
  .btn-stop {
    display: none; align-items: center; justify-content: center; gap: 5px;
    background: #dc2626; color: #ffffff; border: 1px solid #b91c1c; border-radius: 6px;
    padding: 8px 12px; font-size: 12px; font-weight: 600; cursor: pointer; height: 36px;
    white-space: nowrap;
  }
  .btn-stop:hover { background: #b91c1c; }
  .btn-stop-square {
    display: inline-block; width: 9px; height: 9px; background: #ffffff; border-radius: 1.5px;
  }
  .rc-status { font-size: 10.5px; color: var(--muted); }

  /* Slide-Over Settings & SKILL.md Modal Overlay */
  .rc-modal-overlay {
    display: none; position: fixed; inset: 0; background: #ffffff; z-index: 100;
    flex-direction: column; padding: 8px 10px;
  }
  .rc-modal-overlay.open { display: flex; }
  .rc-modal-body { flex: 1; overflow-y: auto; padding: 6px 2px; }
  .rc-section-card {
    background: var(--surface); border: 1px solid var(--border); border-radius: 7px;
    padding: 8px 10px; margin-bottom: 8px;
  }
  .rc-section-title {
    font-weight: 700; font-size: 11.5px; text-transform: uppercase; letter-spacing: 0.03em;
    color: #334155; margin-bottom: 6px; display: flex; justify-content: space-between; align-items: center;
  }
  .rc-field { margin-bottom: 6px; }
  .rc-field label { display: block; font-size: 11px; font-weight: 600; color: #475569; margin-bottom: 2px; }
  .rc-row-2 { display: grid; grid-template-columns: 1fr 1fr; gap: 6px; }
  .rc-check-row { display: flex; gap: 12px; align-items: center; font-size: 11.5px; margin-top: 4px; }
  .rc-check-row label { display: flex; align-items: center; gap: 4px; cursor: pointer; font-weight: 500; }

  .rc-spinner {
    display: inline-block; width: 11px; height: 11px;
    border: 2px solid #93c5fd; border-top-color: var(--primary);
    border-radius: 50%; animation: spin 0.8s linear infinite; margin-right: 4px; vertical-align: -1px;
  }
  @keyframes spin { to { transform: rotate(360deg); } }
</style>
</head>
<body>

<!-- TOP HEADER -->
<div class="rc-header">
  <div class="rc-title">
    <span>ConverseR</span>
    <span id="model_badge" class="rc-badge" onclick="toggleSettings()" title="Click to configure OpenRouter key, model, reasoning &amp; SKILL.md">deepseek-v4.1-flash</span>
    <span id="skill_badge" class="rc-skill-dot" onclick="toggleSettings()" title="SKILL.md / ZIP bundle is active in system prompt">SKILL</span>
  </div>
  <div style="display:flex; gap:4px;">
    <button class="btn-icon" onclick="toggleSettings()" title="Configure OpenRouter key, model, ZDR, reasoning &amp; SKILL.md">Settings</button>
    <button class="btn-icon" onclick="undoLastEdit()" title="Undo last edit in script">Undo</button>
    <button class="btn-icon" onclick="resetChat()" title="Clear conversation">Clear</button>
  </div>
</div>

<!-- MAIN CHAT STREAM -->
<div class="rc-chat-box" id="chat_box"></div>

<!-- BOTTOM INPUT BAR -->
<div class="rc-bottom">
  <div class="rc-action-strip">
    <div class="rc-pills">
      <button class="rc-pill-btn" id="full_doc_btn" onclick="toggleFullDoc()" title="Click to force sending the entire document (or mention keywords like in this document or all figures)">Full Script</button>
      <button class="rc-pill-btn" id="share_plot_btn" onclick="toggleSharePlot()" title="Toggle to attach your active R plot image to your next message">Share Plot</button>
      <button class="rc-pill-btn" onclick="quickAction(\'error\')" title="Diagnose &amp; fix the last R error">Fix Error</button>
    </div>
    <span class="rc-status" id="status_text">Ready</span>
  </div>
  <div class="rc-input-row">
    <textarea id="prompt_input" rows="2" style="resize:none;" placeholder="Select code in your script and/or ask a question (Enter to send)..."></textarea>
    <button class="btn-send" id="send_btn" onclick="sendChat()">Send</button>
    <button class="btn-stop" id="stop_btn" onclick="stopChat()" title="Stop generation"><span class="btn-stop-square"></span>Stop</button>
  </div>
</div>

<!-- SLIDE-OVER SETTINGS & SKILL.MD BUILDER OVERLAY -->
<div class="rc-modal-overlay" id="settings_modal">
  <div class="rc-header">
    <div class="rc-title">Model, OpenRouter &amp; SKILL.md Settings</div>
    <div style="display:flex; gap:6px; align-items:center;">
      <button class="btn-icon" onclick="openGuide()" title="Open the Getting Started Guide in your browser">Help</button>
      <button class="btn-icon" onclick="toggleSettings()">Close &#10005;</button>
    </div>
  </div>

  <div class="rc-modal-body">
    <!-- SECTION 1: OPENROUTER API KEY, MODEL & PRIVACY -->
    <div class="rc-section-card">
      <div class="rc-section-title">
        <span>OpenRouter API Key &amp; Model</span>
        <button class="btn-icon" onclick="fetchOpenRouterModels(true)" id="fetch_models_btn">Show All Models</button>
      </div>

      <div class="rc-field">
        <label>OpenRouter API Key (<code>OPENROUTER_API_KEY</code>):</label>
        <div style="display:flex; gap:4px;">
          <input type="password" id="cfg_api_key" placeholder="sk-or-v1-..." autocomplete="off"/>
          <button class="btn-icon" style="white-space:nowrap;" onclick="toggleApiKeyVisibility()" id="toggle_key_btn">Show</button>
        </div>
      </div>

      <div class="rc-field">
        <label>Select from Top 20 OpenRouter Models (or type any Model ID below):</label>
        <select id="cfg_model_preset" onchange="onPresetSelect(this.value)" style="margin-bottom:4px;">
          <option value="">-- Top 20 OpenRouter Models --</option>
          <option value="deepseek/deepseek-v4.1-flash">deepseek/deepseek-v4.1-flash (Default - DeepSeek V4.1 Flash)</option>
          <option value="anthropic/claude-sonnet-5.5">anthropic/claude-sonnet-5.5 (Anthropic: Claude Sonnet 5.5)</option>
          <option value="anthropic/claude-opus-5.5">anthropic/claude-opus-5.5 (Anthropic: Claude Opus 5.5)</option>
          <option value="anthropic/claude-fable-5.1">anthropic/claude-fable-5.1 (Anthropic: Claude Fable 5.1)</option>
          <option value="anthropic/claude-haiku-5.5">anthropic/claude-haiku-5.5 (Anthropic: Claude Haiku 5.5)</option>
          <option value="anthropic/claude-opus-5">anthropic/claude-opus-5 (Anthropic: Claude Opus 5)</option>
          <option value="openai/gpt-6.1-sol-pro">openai/gpt-6.1-sol-pro (OpenAI: GPT-6.1 Sol Pro)</option>
          <option value="openai/gpt-6.1-sol">openai/gpt-6.1-sol (OpenAI: GPT-6.1 Sol)</option>
          <option value="openai/gpt-6-astra-pro">openai/gpt-6-astra-pro (OpenAI: GPT-6 Astra Pro)</option>
          <option value="openai/gpt-6-astra">openai/gpt-6-astra (OpenAI: GPT-6 Astra)</option>
          <option value="openai/gpt-5.6-sol">openai/gpt-5.6-sol (OpenAI: GPT-5.6 Sol)</option>
          <option value="google/gemini-4-argon">google/gemini-4-argon (Google: Gemini 4 Argon)</option>
          <option value="google/gemini-3.8-flash">google/gemini-3.8-flash (Google: Gemini 3.8 Flash)</option>
          <option value="google/gemini-3.7-flash">google/gemini-3.7-flash (Google: Gemini 3.7 Flash)</option>
          <option value="deepseek/deepseek-v4-flash-vision-exp">deepseek/deepseek-v4-flash-vision-exp (DeepSeek: V4 Flash Vision)</option>
          <option value="qwen/qwen3.8-max-prime">qwen/qwen3.8-max-prime (Qwen: Qwen3.8 Max Prime)</option>
          <option value="qwen/qwen3.8-flash">qwen/qwen3.8-flash (Qwen: Qwen3.8 Flash)</option>
          <option value="x-ai/grok-4.7">x-ai/grok-4.7 (xAI: Grok 4.7)</option>
          <option value="mistralai/mistral-large-4-0">mistralai/mistral-large-4-0 (Mistral: Mistral Large 4)</option>
          <option value="moonshotai/kimi-k3">moonshotai/kimi-k3 (MoonshotAI: Kimi K3)</option>
        </select>
        <input type="text" id="cfg_model" list="openrouter_datalist" value="deepseek/deepseek-v4.1-flash" placeholder="e.g. deepseek/deepseek-v4.1-flash"/>
        <datalist id="openrouter_datalist"></datalist>
      </div>

      <div class="rc-row-2">
        <div class="rc-field">
          <label title="OpenRouter normalizes effort across OpenAI, Anthropic, Gemini &amp; DeepSeek">Thinking / Reasoning Effort:</label>
          <select id="cfg_reasoning">
            <option value="default">Model Default (Auto)</option>
            <option value="none">None (Exclude reasoning)</option>
            <option value="minimal">Minimal (~10% budget)</option>
            <option value="low" selected>Low (~20% budget)</option>
            <option value="medium">Medium (~50% budget)</option>
            <option value="high">High (~80% budget)</option>
          </select>
        </div>
        <div class="rc-field">
          <label>Data Collection Policy:</label>
          <select id="cfg_data_collection">
            <option value="deny">Deny (Block training hosts)</option>
            <option value="allow">Allow (All providers)</option>
          </select>
        </div>
      </div>

      <div class="rc-check-row">
        <label title="Restrict routing exclusively to Zero Data Retention endpoints"><input type="checkbox" id="cfg_zdr" checked/> Zero Data Retention (ZDR only)</label>
        <label title="Allow OpenRouter to fall back if primary host is unavailable"><input type="checkbox" id="cfg_fallbacks" checked/> Allow Provider Fallbacks</label>
      </div>
    </div>

    <!-- SECTION 2: SYSTEM PROMPT -->
    <div class="rc-section-card">
      <div class="rc-section-title">Base Persona / System Prompt</div>
      <div class="rc-field" style="margin-bottom:0;">
        <textarea id="cfg_system_prompt" rows="2" placeholder="Optional custom base persona (leave blank to use default ConverseR persona)..."></textarea>
      </div>
    </div>

    <!-- SECTION 3: SKILL.MD & ZIP BUNDLE LOADER -->
    <div class="rc-section-card" style="margin-bottom:4px;">
      <div class="rc-section-title">
        <span>SKILL.md &amp; ZIP Bundle Loader</span>
        <div style="display:flex; gap:4px;">
          <button class="btn-icon" onclick="insertSkillTemplate()">Insert Template</button>
          <label class="btn-icon" style="margin:0; cursor:pointer;" title="Upload a SKILL.md file OR a .zip archive containing SKILL.md + resource files">
            Upload .md or .zip
            <input type="file" id="skill_file_upload" accept=".md,.txt,.Rmd,.qmd,.zip" style="display:none;" onchange="handleSkillUpload(event)"/>
          </label>
        </div>
      </div>

      <div style="display:flex; gap:4px; margin-bottom:6px;">
        <input type="text" id="cfg_skill_path" placeholder="Path to SKILL.md, folder, or .zip bundle"/>
        <button class="btn-icon" style="white-space:nowrap;" onclick="loadSkillFromPath()">Load Path</button>
        <button class="btn-icon" style="white-space:nowrap;" onclick="saveSkillToDisk()">Save .md</button>
      </div>

      <div id="bundled_files_info" style="font-size:11px; color:#166534; margin-bottom:4px; display:none;"></div>

      <div class="rc-field" style="margin-bottom:0;">
        <label>Active SKILL.md &amp; Bundled Resources (appended to system prompt):</label>
        <textarea id="cfg_skill_text" rows="6" style="font-family:Consolas,monospace; font-size:11.5px;" placeholder="# Domain Skill &amp; Coding Conventions\nUpload a SKILL.md or .zip bundle (containing SKILL.md + reference/helper files), or write custom instructions here..."></textarea>
      </div>
    </div>
  </div>

  <div style="border-top:1px solid var(--border); padding-top:6px; display:flex; justify-content:space-between; align-items:center; gap:8px;">
    <label style="display:flex; align-items:center; gap:5px; font-size:11.5px; font-weight:600; color:#1e293b; cursor:pointer;" title="Saves your API key, model, reasoning level, and SKILL.md so they load automatically in every future R session without editing .Rprofile or .Renviron">
      <input type="checkbox" id="cfg_persist" checked/> Save as default across R sessions
    </label>
    <div style="display:flex; align-items:center; gap:8px;">
      <span class="rc-status" id="settings_status"></span>
      <button class="btn-send" style="height:30px; padding:4px 14px;" onclick="saveAllSettings()">Save &amp; Apply</button>
    </div>
  </div>
</div>

<script>
let isSending = false;
let hasEditorSelection = false;
let sharePlotAttached = false;
let currentAbortController = null;

function setStatus(html) {
  document.getElementById("status_text").innerHTML = html;
}

async function openGuide() {
  try {
    const res = await fetch("/api/guide", { method: "POST" });
    const data = await res.json();
    if (data.ok) setStatus(data.message);
  } catch (e) {}
}

// Robust clipboard helper that works inside RStudio Viewer iframes and browsers
async function copyTextRobust(text, btnEl) {
  let copied = false;
  try {
    const ta = document.createElement("textarea");
    ta.value = text;
    ta.setAttribute("readonly", "");
    ta.style.position = "fixed";
    ta.style.top = "-9999px";
    ta.style.left = "-9999px";
    document.body.appendChild(ta);
    ta.focus();
    ta.select();
    copied = document.execCommand("copy");
    document.body.removeChild(ta);
  } catch (e) {}

  if (!copied && navigator.clipboard && navigator.clipboard.writeText) {
    try {
      await navigator.clipboard.writeText(text);
      copied = true;
    } catch (e2) {}
  }

  // Also invoke R-side clipboard copy so Windows/RStudio clipboard is guaranteed populated
  try {
    const res = await fetch("/api/copy", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ text: text })
    });
    const data = await res.json();
    if (data && data.ok) copied = true;
  } catch (e3) {}

  if (btnEl) {
    const orig = btnEl.getAttribute("data-orig-label") || btnEl.textContent;
    btnEl.setAttribute("data-orig-label", orig);
    btnEl.textContent = "Copied!";
    setTimeout(() => { btnEl.textContent = orig; }, 1400);
  }
  setStatus("Copied to clipboard");
}

function toggleApiKeyVisibility() {
  const inp = document.getElementById("cfg_api_key");
  const btn = document.getElementById("toggle_key_btn");
  if (inp.type === "password") {
    inp.type = "text";
    btn.textContent = "Hide";
  } else {
    inp.type = "password";
    btn.textContent = "Show";
  }
}

let fullDocAttached = false;
function toggleFullDoc() {
  fullDocAttached = !fullDocAttached;
  const btn = document.getElementById("full_doc_btn");
  if (btn) btn.classList.toggle("active", fullDocAttached);
  if (fullDocAttached) {
    setStatus("Full script mode active for your next message");
  } else {
    setStatus("Ready");
  }
}

function toggleSharePlot() {
  sharePlotAttached = !sharePlotAttached;
  const btn = document.getElementById("share_plot_btn");
  btn.classList.toggle("active", sharePlotAttached);
  const input = document.getElementById("prompt_input");
  if (sharePlotAttached) {
    btn.textContent = "Plot Attached (Active)";
    setStatus("Active plot will be shared with your next message");
    input.placeholder = "Ask anything about your attached plot (e.g., how to improve it, change labels, etc.)...";
    input.focus();
  } else {
    btn.textContent = "Share Plot";
    setStatus("Ready");
    input.placeholder = "Select code in your script and/or ask a question (Enter to send)...";
  }
}

function updateReplaceButtonsState(hasSel) {
  hasEditorSelection = !!hasSel;
  document.querySelectorAll(\'[data-mode="replace"]\').forEach(btn => {
    btn.disabled = !hasEditorSelection;
    btn.title = hasEditorSelection
      ? "Replace highlighted selection in active script"
      : "Highlight code in your RStudio script to enable Replace (or use Run in Console)";
  });
}

let isCheckingSelection = false;
let lastSelectionCheckMs = 0;

async function refreshSelectionState() {
  if (isSending || isCheckingSelection || document.visibilityState === "hidden") return;
  const now = Date.now();
  if (now - lastSelectionCheckMs < 800) return;
  lastSelectionCheckMs = now;
  isCheckingSelection = true;
  try {
    const res = await fetch("/api/selection");
    const data = await res.json();
    if (data.ok && data.has_selection !== hasEditorSelection) {
      updateReplaceButtonsState(data.has_selection);
    }
  } catch (e) {
  } finally {
    isCheckingSelection = false;
  }
}

function renderMessages(messages) {
  const box = document.getElementById("chat_box");
  if (!messages || messages.length === 0) {
    box.innerHTML = `<div class="rc-empty">
      <div style="font-weight:600; color:#334155; font-size:13.5px; margin-bottom:6px;">Select code in your script and/or ask a question below.</div>
      <div style="font-size:12px; color:#64748b;">
        ConverseR automatically incorporates your active script context and relevant objects from <code>.GlobalEnv</code>.
      </div>
    </div>`;
    return;
  }

  let autoOpenLastDiff = false;
  box.innerHTML = messages.map((m) => {
    if (m.auto_open_diff) autoOpenLastDiff = true;
    const cls = m.role === "user" ? "rc-msg rc-msg-user" : "rc-msg rc-msg-assistant";
    const prefAttr = m.preferred_mode ? ` data-preferred-mode="${m.preferred_mode}"` : "";
    return `<div class="${cls}"${prefAttr}>${m.html}</div>`;
  }).join("");

  enhanceCodeBlocks(autoOpenLastDiff);
  updateReplaceButtonsState(hasEditorSelection);
  box.scrollTop = box.scrollHeight;
}

function enhanceCodeBlocks(autoOpenLastDiff = false) {
  const pres = Array.from(document.querySelectorAll(".rc-msg-assistant pre"));
  let diffAutoOpened = false;
  pres.forEach((pre, idx) => {
    if (pre.parentElement && pre.parentElement.classList.contains("rc-code-wrapper")) return;
    const codeEl = pre.querySelector("code");
    const rawCode = codeEl ? codeEl.innerText : pre.innerText;
    if (!rawCode || !rawCode.trim()) {
      pre.remove();
      return;
    }
    const clsName = (codeEl && codeEl.className) ? codeEl.className.toLowerCase() : "";
    const isYaml = clsName.includes("yaml");
    const langLabel = isYaml ? "YAML (SKILL.md)" : "R Code";
    const msgContainer = pre.closest(".rc-msg-assistant");
    const preferredMode = msgContainer ? (msgContainer.getAttribute("data-preferred-mode") || "") : "";

    const wrapper = document.createElement("div");
    wrapper.className = "rc-code-wrapper";

    const toolbar = document.createElement("div");
    toolbar.className = "rc-code-toolbar";
    if (isYaml) {
      toolbar.innerHTML = `
        <span class="rc-code-lang">${langLabel}</span>
        <div class="rc-code-actions">
          <button class="rc-code-btn rc-code-btn-primary" data-action="apply_skill" data-idx="${idx}" title="Load this YAML directly into your SKILL.md Settings editor">Load into SKILL.md</button>
          <button class="rc-code-btn" data-action="copy" data-idx="${idx}" title="Copy YAML to clipboard">Copy</button>
        </div>
      `;
    } else {
      const insertBeforeBtn = preferredMode === "prefix"
        ? `<button class="rc-code-btn rc-code-btn-primary" data-mode="prefix" data-idx="${idx}" title="Insert before highlighted selection in active script">Insert Before</button>`
        : "";
      const insertAfterClass = preferredMode === "suffix" ? "rc-code-btn rc-code-btn-primary" : "rc-code-btn";
      const insertAfterLabel = preferredMode === "suffix" ? "Insert After" : "Insert in Script";
      const replaceClass = (preferredMode === "prefix" || preferredMode === "suffix")
        ? "rc-code-btn"
        : "rc-code-btn rc-code-btn-primary";
      toolbar.innerHTML = `
        <span class="rc-code-lang">${langLabel}</span>
        <div class="rc-code-actions">
          <button class="rc-code-btn rc-code-btn-console" data-action="console" data-idx="${idx}" title="Execute directly in the R Console without modifying your script">Run in Console</button>
          <button class="rc-code-btn rc-code-btn-diff" data-idx="${idx}" title="Compare proposed code against highlighted selection">Preview Difference</button>
          ${insertBeforeBtn}
          <button class="${replaceClass}" data-mode="replace" data-idx="${idx}">Replace</button>
          <button class="${insertAfterClass}" data-mode="insert" data-idx="${idx}" title="Insert after cursor/selection in active script">${insertAfterLabel}</button>
          <button class="rc-code-btn" data-action="copy" data-idx="${idx}" title="Copy code to clipboard">Copy</button>
        </div>
      `;
    }
    toolbar.addEventListener("mouseenter", refreshSelectionState);

    const diffDrawer = document.createElement("div");
    diffDrawer.className = "rc-inline-diff";
    diffDrawer.id = "diff_drawer_" + idx;

    pre.parentNode.insertBefore(wrapper, pre);
    wrapper.appendChild(toolbar);
    wrapper.appendChild(pre);
    wrapper.appendChild(diffDrawer);

    const diffBtn = toolbar.querySelector(".rc-code-btn-diff");
    if (diffBtn) {
      diffBtn.addEventListener("click", async () => {
        if (diffDrawer.classList.contains("open")) {
          diffDrawer.classList.remove("open");
          return;
        }
        diffDrawer.innerHTML = `<div style="color:#64748b; padding:4px;">Computing difference against active selection...</div>`;
        diffDrawer.classList.add("open");
        diffDrawer.scrollIntoView({ behavior: "smooth", block: "nearest" });

        const res = await fetch("/api/diff", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ code: rawCode.trim() })
        });
        const data = await res.json();
        if (data.ok) {
          updateReplaceButtonsState(data.has_selection);
          const hdr = data.has_selection
            ? `<span>Red = Current Selection | Green = Proposed</span>`
            : `<span>Nothing highlighted in editor (showing proposed code)</span>`;
          diffDrawer.innerHTML = `
            <div class="rc-diff-header">
              ${hdr}
              <button class="btn-icon" onclick="this.closest(\'.rc-inline-diff\').classList.remove(\'open\')">Close</button>
            </div>
            ${data.diff_html}
          `;
          setTimeout(() => {
            diffDrawer.scrollIntoView({ behavior: "smooth", block: "center" });
          }, 40);
        }
      });
    }

    // Load into SKILL.md button handler for YAML blocks
    const skillBtn = toolbar.querySelector(\'[data-action="apply_skill"]\');
    if (skillBtn) {
      skillBtn.addEventListener("click", async () => {
        await toggleSettings();
        const box = document.getElementById("cfg_skill_text");
        if (box) {
          box.value = rawCode.trim();
          box.focus();
        }
        const st = document.getElementById("settings_status");
        if (st) {
          st.textContent = "Loaded YAML into SKILL.md! Click Save & Apply (or Save to Disk).";
        }
      });
    }

    // Run in Console button handler
    const consoleBtn = toolbar.querySelector(\'[data-action="console"]\');
    if (consoleBtn) {
      consoleBtn.addEventListener("click", async () => {
        await runInConsole(rawCode.trim(), consoleBtn);
      });
    }

    // Copy button handler (uses robust textarea + R clipboard fallback)
    const copyBtn = toolbar.querySelector(\'[data-action="copy"]\');
    if (copyBtn) {
      copyBtn.addEventListener("click", async () => {
        await copyTextRobust(rawCode.trim(), copyBtn);
      });
    }

    // Script Replace / Insert handlers
    toolbar.querySelectorAll("[data-mode]").forEach(btn => {
      btn.addEventListener("click", async () => {
        if (btn.disabled) return;
        const mode = btn.getAttribute("data-mode");
        await applyCode(mode, rawCode.trim());
      });
    });

    // If triggered from converser_inline() with "Preview Difference first", auto-open the diff drawer on the valid code block!
    if (autoOpenLastDiff && !diffAutoOpened && diffBtn) {
      diffAutoOpened = true;
      setTimeout(() => diffBtn.click(), 80);
    }
  });
}

async function runInConsole(code, btnEl) {
  const origLabel = btnEl ? btnEl.textContent : "Run in Console";
  if (btnEl) btnEl.textContent = "Running...";
  try {
    const res = await fetch("/api/console", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ code: code, execute: true })
    });
    const data = await res.json();
    if (data.ok) {
      setStatus(data.message);
      if (btnEl) {
        btnEl.textContent = "Ran in Console!";
        setTimeout(() => { btnEl.textContent = origLabel; }, 1600);
      }
    } else {
      setStatus(data.error || "Could not send to console");
      if (btnEl) btnEl.textContent = origLabel;
    }
  } catch (e) {
    setStatus("Error sending to console");
    if (btnEl) btnEl.textContent = origLabel;
  }
}

// Settings & SKILL.md / ZIP Modal Functions ------------------------------------

function populateSettingsForm(s) {
  if (!s) return;
  document.getElementById("cfg_api_key").value = s.openrouter_api_key || "";
  document.getElementById("cfg_model").value = s.model || "deepseek/deepseek-v4.1-flash";
  document.getElementById("cfg_reasoning").value = s.reasoning_effort || "low";
  document.getElementById("cfg_data_collection").value = s.data_collection || "deny";
  document.getElementById("cfg_zdr").checked = s.zdr !== false;
  document.getElementById("cfg_fallbacks").checked = s.allow_fallbacks !== false;
  document.getElementById("cfg_system_prompt").value = s.system_prompt || "";
  document.getElementById("cfg_skill_path").value = s.skill_file || "";
  document.getElementById("cfg_skill_text").value = s.skill_text || "";
  showBundledFilesBadge(s.bundled_files || []);
}

function showBundledFilesBadge(files) {
  const el = document.getElementById("bundled_files_info");
  if (files && files.length > 0) {
    el.style.display = "block";
    el.textContent = "Bundled files loaded (" + files.length + "): " + files.join(", ");
  } else {
    el.style.display = "none";
  }
}

async function toggleSettings() {
  const modal = document.getElementById("settings_modal");
  if (modal.classList.contains("open")) {
    modal.classList.remove("open");
    return;
  }
  try {
    const res = await fetch("/api/settings");
    const data = await res.json();
    if (data.ok) populateSettingsForm(data.settings);
  } catch (e) {}
  modal.classList.add("open");
}

function onPresetSelect(val) {
  if (val) {
    document.getElementById("cfg_model").value = val;
  }
}

async function fetchOpenRouterModels(showAll = false) {
  const btn = document.getElementById("fetch_models_btn");
  if (btn) btn.textContent = "Fetching...";
  try {
    const res = await fetch("https://openrouter.ai/api/v1/models");
    const json = await res.json();
    if (json && Array.isArray(json.data)) {
      const dl = document.getElementById("openrouter_datalist");
      const preset = document.getElementById("cfg_model_preset");

      const cleanModels = json.data.filter(m => m.id && !m.id.includes(":batch") && !m.id.includes(":free") && !m.id.startsWith("~"));
      const allIds = cleanModels.map(m => m.id).sort();
      dl.innerHTML = allIds.map(id => `<option value="${id}"></option>`).join("");

      if (showAll) {
        preset.innerHTML = `<option value="">-- Select from ${allIds.length} OpenRouter Models --</option>` +
          allIds.map(id => `<option value="${id}">${id}</option>`).join("");
        document.getElementById("settings_status").textContent = `Loaded all ${allIds.length} models`;
      } else {
        // Rank top 20 models by OpenRouter coding/intelligence benchmark or newest flagship, keeping deepseek/deepseek-v4.1-flash at #1
        const scored = [...cleanModels].sort((a, b) => {
          if (a.id === "deepseek/deepseek-v4.1-flash") return -1;
          if (b.id === "deepseek/deepseek-v4.1-flash") return 1;
          const ca = (a.benchmarks && a.benchmarks.artificial_analysis && a.benchmarks.artificial_analysis.coding_index) || 0;
          const cb = (b.benchmarks && b.benchmarks.artificial_analysis && b.benchmarks.artificial_analysis.coding_index) || 0;
          if (cb !== ca) return cb - ca;
          return (b.created || 0) - (a.created || 0);
        });
        const top20 = scored.slice(0, 20);
        preset.innerHTML = `<option value="">-- Top 20 OpenRouter Models --</option>` +
          top20.map(m => `<option value="${m.id}">${m.id} (${m.name || m.id})</option>`).join("");
      }
    }
  } catch (e) {
    if (showAll) {
      document.getElementById("settings_status").textContent = "Could not fetch model list.";
    }
  } finally {
    if (btn) btn.textContent = "Show All Models";
  }
}

function insertSkillTemplate() {
  const box = document.getElementById("cfg_skill_text");
  if (box.value.trim() && !confirm("Replace current SKILL.md editor text with the starter YAML template?")) {
    return;
  }
  box.value = [
    "name: r-data-science-skill",
    "description: Custom R coding, statistical modeling, and plotting conventions",
    "coding_style:",
    "  - Prefer tidyverse pipelines (|>, dplyr, tidyr, purrr) and ggplot2.",
    "  - Keep code modular, reproducible, and commented only where non-obvious.",
    "packages:",
    "  wrangling: [dplyr, tidyr, purrr, readr]",
    "  modeling: [stats, broom, fixest, lme4]",
    "  plotting: [ggplot2, scales]",
    "modeling_conventions:",
    "  - Report effect sizes, standard errors, and 95% confidence intervals.",
    "  - Use broom::tidy(model, conf.int = TRUE) when summarizing models.",
    "plotting_conventions:",
    "  - Use theme_minimal(base_size = 12) as the default ggplot2 theme.",
    "code_templates:",
    "  summary_model: |",
    "    fit <- lm(outcome ~ predictor, data = df)",
    "    broom::tidy(fit, conf.int = TRUE)"
  ].join("\\n");
}

function handleSkillUpload(event) {
  const file = event.target.files[0];
  if (!file) return;

  if (file.name.toLowerCase().endsWith(".zip")) {
    const reader = new FileReader();
    reader.onload = async function(e) {
      document.getElementById("settings_status").textContent = "Extracting ZIP...";
      const res = await fetch("/api/upload_skill_zip", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ filename: file.name, base64: e.target.result })
      });
      const data = await res.json();
      if (data.ok) {
        document.getElementById("cfg_skill_text").value = data.skill_text || "";
        document.getElementById("cfg_skill_path").value = data.path || file.name;
        showBundledFilesBadge(data.bundled_files || []);
        document.getElementById("settings_status").textContent = data.message;
        document.getElementById("skill_badge").classList.add("active");
      } else {
        document.getElementById("settings_status").textContent = data.error || "Failed to unpack ZIP.";
      }
    };
    reader.readAsDataURL(file);
  } else {
    const reader = new FileReader();
    reader.onload = function(e) {
      document.getElementById("cfg_skill_text").value = e.target.result || "";
      document.getElementById("cfg_skill_path").value = file.name;
      showBundledFilesBadge([file.name]);
      document.getElementById("settings_status").textContent = `Uploaded ${file.name}. Click Save & Apply!`;
    };
    reader.readAsText(file);
  }
}

async function loadSkillFromPath() {
  const path = document.getElementById("cfg_skill_path").value.trim();
  const res = await fetch("/api/load_skill_path", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ path: path })
  });
  const data = await res.json();
  if (data.ok) {
    document.getElementById("cfg_skill_text").value = data.skill_text || "";
    document.getElementById("cfg_skill_path").value = data.path || path;
    showBundledFilesBadge(data.bundled_files || []);
    document.getElementById("settings_status").textContent = data.message;
  } else {
    document.getElementById("settings_status").textContent = data.error || "Failed to load file.";
  }
}

async function saveSkillToDisk() {
  const path = document.getElementById("cfg_skill_path").value.trim() || "SKILL.md";
  const text = document.getElementById("cfg_skill_text").value;
  const res = await fetch("/api/save_skill_file", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ path: path, skill_text: text })
  });
  const data = await res.json();
  if (data.ok) {
    document.getElementById("cfg_skill_path").value = data.path;
    document.getElementById("settings_status").textContent = data.message;
    document.getElementById("skill_badge").classList.toggle("active", !!text.trim());
  } else {
    document.getElementById("settings_status").textContent = data.error || "Could not save file.";
  }
}

async function saveAllSettings() {
  const statusEl = document.getElementById("settings_status");
  statusEl.innerHTML = \'<span class="rc-spinner"></span>Saving...\';
  const payload = {
    openrouter_api_key: document.getElementById("cfg_api_key").value.trim(),
    model: document.getElementById("cfg_model").value.trim() || "deepseek/deepseek-v4.1-flash",
    reasoning_effort: document.getElementById("cfg_reasoning").value,
    data_collection: document.getElementById("cfg_data_collection").value,
    zdr: document.getElementById("cfg_zdr").checked,
    allow_fallbacks: document.getElementById("cfg_fallbacks").checked,
    system_prompt: document.getElementById("cfg_system_prompt").value,
    skill_file: document.getElementById("cfg_skill_path").value.trim(),
    skill_text: document.getElementById("cfg_skill_text").value,
    persist: document.getElementById("cfg_persist").checked
  };
  try {
    const res = await fetch("/api/save_settings", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload)
    });
    const data = await res.json();
    if (data.ok) {
      document.getElementById("model_badge").textContent = data.model;
      document.getElementById("skill_badge").classList.toggle("active", !!data.has_skill);
      statusEl.textContent = "Saved!";
      setStatus(data.message);
      setTimeout(() => {
        document.getElementById("settings_modal").classList.remove("open");
      }, 350);
    } else {
      statusEl.textContent = "Error: " + (data.error || "Failed to apply settings");
    }
  } catch (e) {
    statusEl.textContent = "Error: " + e.message;
  }
}

async function loadState() {
  try {
    const res = await fetch("/api/state");
    const data = await res.json();
    if (!data.ok) return;
    document.getElementById("model_badge").textContent = data.model || "deepseek-v4.1-flash";
    document.getElementById("skill_badge").classList.toggle("active", !!data.has_skill);
    hasEditorSelection = !!data.has_selection;
    if (data.settings) populateSettingsForm(data.settings);
    renderMessages(data.messages || []);
    fetchOpenRouterModels(false);
  } catch (e) {
    setStatus("Offline");
  }
}

function setThinkingUI(thinking) {
  isSending = thinking;
  document.getElementById("send_btn").style.display = thinking ? "none" : "inline-block";
  document.getElementById("stop_btn").style.display = thinking ? "inline-flex" : "none";
}

function stopChat() {
  if (currentAbortController) {
    currentAbortController.abort();
    currentAbortController = null;
  }
  const tempEl = document.getElementById("temp_thinking");
  if (tempEl) {
    tempEl.innerHTML = "<em style=\'color:#dc2626;\'>Stopped by user.</em>";
    tempEl.removeAttribute("id");
  }
  setThinkingUI(false);
  setStatus("Stopped");
}

async function sendChat() {
  const input = document.getElementById("prompt_input");
  const text = input.value.trim();
  if (!text || isSending) return;
  input.value = "";
  const attachPlot = sharePlotAttached;
  if (sharePlotAttached) {
    toggleSharePlot();
  }
  const forceDoc = fullDocAttached;
  if (fullDocAttached) {
    toggleFullDoc();
  }
  await sendPromptInternal(text, false, attachPlot, forceDoc);
}

async function sendPromptInternal(promptText, incErr, incPlot, forceDoc = false) {
  if (isSending) return;
  setThinkingUI(true);
  currentAbortController = new AbortController();

  const box = document.getElementById("chat_box");
  const emptyEl = box.querySelector(".rc-empty");
  if (emptyEl) emptyEl.remove();

  box.insertAdjacentHTML("beforeend", `<div class="rc-msg rc-msg-user"><p>${escapeHtml(promptText)}</p></div>`);
  box.insertAdjacentHTML("beforeend", `<div class="rc-msg rc-msg-assistant" id="temp_thinking"><span class="rc-spinner"></span>Thinking...</div>`);
  box.scrollTop = box.scrollHeight;
  setStatus(\'<span class="rc-spinner"></span>Thinking...\');

  try {
    const res = await fetch("/api/chat", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        prompt: promptText,
        inc_err: !!incErr,
        inc_plot: !!incPlot,
        force_whole_doc: !!forceDoc
      }),
      signal: currentAbortController.signal
    });
    const data = await res.json();
    if (data.ok) {
      hasEditorSelection = !!data.has_selection;
      renderMessages(data.messages || []);
      setStatus("Ready");
    } else {
      const errMsg = data.error || "An error occurred while communicating with OpenRouter.";
      const tempEl = document.getElementById("temp_thinking");
      if (tempEl) {
        const needsKey = errMsg.toLowerCase().includes("api key") || errMsg.toLowerCase().includes("settings");
        const keyBtn = needsKey
          ? ` <button class="btn-icon" style="margin-left:6px; border-color:#dc2626; color:#dc2626;" onclick="toggleSettings()">Open Settings</button>`
          : "";
        tempEl.innerHTML = `<div style="color:#dc2626; font-weight:500;">${escapeHtml(errMsg)}${keyBtn}</div>`;
        tempEl.removeAttribute("id");
      }
      setStatus(errMsg);
    }
  } catch (e) {
    if (e && e.name === "AbortError") {
      setStatus("Stopped");
    } else {
      const tempEl = document.getElementById("temp_thinking");
      if (tempEl) {
        tempEl.innerHTML = `<div style="color:#dc2626;">Connection error: ${escapeHtml((e && e.message) ? e.message : "Could not reach R server.")}</div>`;
        tempEl.removeAttribute("id");
      }
      setStatus("Error");
    }
  } finally {
    currentAbortController = null;
    setThinkingUI(false);
  }
}

function quickAction(type) {
  if (type === "error") {
    sendPromptInternal("Diagnose my last R error and provide the corrected R code.", true, false);
  }
}

async function applyCode(mode, code) {
  const res = await fetch("/api/apply", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ mode: mode, code: code })
  });
  const data = await res.json();
  if (data.ok) {
    setStatus(data.message);
    if (typeof data.has_selection !== "undefined") {
      updateReplaceButtonsState(data.has_selection);
    }
  } else {
    setStatus(data.error || "Could not apply");
  }
}

async function undoLastEdit() {
  const res = await fetch("/api/undo", { method: "POST" });
  const data = await res.json();
  setStatus(data.message || "Reverted");
}

async function resetChat() {
  await fetch("/api/reset", { method: "POST" });
  renderMessages([]);
  setStatus("Cleared");
}

function escapeHtml(s) {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}

document.getElementById("prompt_input").addEventListener("keydown", function(e) {
  if (e.key === "Enter" && !e.shiftKey) {
    e.preventDefault();
    sendChat();
  }
});

loadState();
</script>
</body>
</html>'
}
