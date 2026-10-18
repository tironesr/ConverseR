# Internal package state environment
converser_env <- rlang::new_environment()

# Package Startup Hooks (.onLoad & .onAttach) ----------------------------------

.onLoad <- function(libname, pkgname) {
  # Muffle RStudio 2026's internal `.Call("rs_getBrowserEnv")` NULL-pointer warning in R 4.4+
  tryCatch(
    globalCallingHandlers(warning = function(w) {
      msg <- conditionMessage(w)
      call_str <- paste(deparse(conditionCall(w)), collapse = " ")
      if (grepl("rs_getBrowserEnv", call_str, fixed = TRUE) ||
          (grepl("converting NULL pointer to R NULL", msg, fixed = TRUE) && grepl("rs_", call_str, fixed = TRUE))) {
        tryInvokeRestart("muffleWarning")
      }
    }),
    error = function(e) NULL
  )

  # Load saved JSON settings into options/env without invoking RStudio/cli hooks
  suppressWarnings(
    tryCatch(
      converser_load_saved_config(quiet = TRUE, init_chat = FALSE),
      error = function(e) NULL
    )
  )
}

.onAttach <- function(libname, pkgname) {
  # Wrap cli::format_message in suppressWarnings() so RStudio renders clickable
  # `.run` links while suppressing RStudio's internal `rs_getBrowserEnv` warning.
  msg <- suppressWarnings(
    tryCatch(
      cli::format_message(c(
        "v" = "{.pkg ConverseR} loaded.",
        "i" = "Getting Started Guide : {.run [converser_guide()](ConverseR::converser_guide())}",
        "i" = "Open Sidebar          : {.run [converser()](ConverseR::converser())}",
        "i" = "Inline Assistant      : {.run [converser_inline()](ConverseR::converser_inline())}"
      )),
      error = function(e) {
        paste0(
          "ConverseR loaded.\n",
          "  * Getting Started Guide : converser_guide()\n",
          "  * Open Sidebar          : converser()\n",
          "  * Inline Assistant      : converser_inline()"
        )
      }
    )
  )
  suppressWarnings(packageStartupMessage(msg))
}

# Persistent Cross-Session Configuration Storage -------------------------------

#' Path to the persistent ConverseR user configuration file
#'
#' @returns Character string path to `settings.json` inside `tools::R_user_dir("ConverseR", "config")`.
#' @export
converser_config_path <- function() {
  cfg_dir <- tools::R_user_dir("ConverseR", which = "config")
  file.path(cfg_dir, "settings.json")
}

#' Save current ConverseR settings (model, OpenRouter API key, skills) across R sessions
#'
#' @description
#' Saves your OpenRouter API key, selected model, reasoning effort, privacy/ZDR
#' settings, system prompt, and `SKILL.md` / `.zip` skill bundle to a persistent
#' user configuration file (`converser_config_path()`).
#'
#' Saved settings are automatically loaded whenever `library(ConverseR)` starts
#' in a new R session—no manual editing of `.Rprofile` or `.Renviron` required.
#'
#' @param settings Optional named list of settings to save. Defaults to the
#'   currently active ConverseR settings.
#' @param api_key Optional OpenRouter API key string (`"sk-or-v1-..."`) to store
#'   persistently across sessions.
#' @returns Invisibly returns the path to the saved configuration file.
#' @export
converser_save_config <- function(settings = NULL, api_key = NULL) {
  current <- inspect_current_settings(include_api_key = TRUE)
  if (is.list(settings)) {
    for (nm in names(settings)) {
      if (!is.null(settings[[nm]])) {
        current[[nm]] <- settings[[nm]]
      }
    }
  }
  if (!is.null(api_key)) {
    clean_key <- trimws(api_key)
    current$openrouter_api_key <- clean_key
    converser_env[["saved_api_key"]] <- clean_key
    if (nzchar(clean_key)) {
      Sys.setenv(OPENROUTER_API_KEY = clean_key)
    } else {
      Sys.unsetenv("OPENROUTER_API_KEY")
    }
  }

  cfg_file <- converser_config_path()
  dir.create(dirname(cfg_file), recursive = TRUE, showWarnings = FALSE)

  to_write <- list(
    openrouter_api_key = current$openrouter_api_key %||% "",
    model = current$model %||% "deepseek/deepseek-v4.1-flash",
    reasoning_effort = current$reasoning_effort %||% "low",
    zdr = isTRUE(current$zdr),
    data_collection = current$data_collection %||% "deny",
    allow_fallbacks = isTRUE(current$allow_fallbacks),
    system_prompt = current$system_prompt %||% "",
    style = "",
    skill_file = current$skill_file %||% "",
    skill_text = current$skill_text %||% ""
  )

  jsonlite::write_json(to_write, path = cfg_file, auto_unbox = TRUE, pretty = TRUE)
  invisible(cfg_file)
}

#' Load saved ConverseR configuration from disk
#'
#' @param quiet Logical; whether to suppress informational messages.
#' @param init_chat Logical; whether to immediately instantiate the `ellmer`
#'   `Chat` object (set to `FALSE` during `.onLoad` so chat initialization
#'   happens lazily on first use).
#' @returns Invisibly returns `TRUE` if a saved configuration was loaded, `FALSE` otherwise.
#' @export
converser_load_saved_config <- function(quiet = FALSE, init_chat = TRUE) {
  cfg_file <- converser_config_path()
  if (!file.exists(cfg_file)) {
    return(invisible(FALSE))
  }

  saved <- tryCatch(
    jsonlite::read_json(cfg_file, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (!is.list(saved)) {
    return(invisible(FALSE))
  }

  saved_key <- trimws(saved$openrouter_api_key %||% "")
  converser_env[["saved_api_key"]] <- saved_key
  if (nzchar(saved_key)) {
    Sys.setenv(OPENROUTER_API_KEY = saved_key)
  }

  options(converser.style = "")
  if (!is.null(saved$system_prompt)) {
    options(converser.system_prompt = saved$system_prompt)
  }
  if (!is.null(saved$skill_file)) {
    options(converser.skill_file = saved$skill_file)
  }
  if (!is.null(saved$skill_text)) {
    options(converser.skill_text = saved$skill_text)
  }
  if (!nzchar(trimws(saved$skill_text %||% "")) && !is.null(saved$skill_file) && nzchar(trimws(saved$skill_file)) && file.exists(path.expand(saved$skill_file))) {
    options(converser.skill_text = read_skill_files(saved$skill_file))
  }

  converser_env[["openrouter_settings"]] <- list(
    model = saved$model %||% "deepseek/deepseek-v4.1-flash",
    reasoning_effort = saved$reasoning_effort %||% "low",
    zdr = if (is.null(saved$zdr)) TRUE else isTRUE(saved$zdr),
    data_collection = saved$data_collection %||% "deny",
    allow_fallbacks = if (is.null(saved$allow_fallbacks)) TRUE else isTRUE(saved$allow_fallbacks)
  )

  if (isTRUE(init_chat) && nzchar(saved_key)) {
    suppressWarnings(
      tryCatch(
        converser_use_openrouter(
          model = saved$model %||% "deepseek/deepseek-v4.1-flash",
          reasoning_effort = saved$reasoning_effort %||% "low",
          zdr = if (is.null(saved$zdr)) TRUE else isTRUE(saved$zdr),
          data_collection = saved$data_collection %||% "deny",
          allow_fallbacks = if (is.null(saved$allow_fallbacks)) TRUE else isTRUE(saved$allow_fallbacks),
          system_prompt = saved$system_prompt,
          skill_text = getOption("converser.skill_text"),
          skill_file = saved$skill_file,
          api_key = saved_key,
          persist = FALSE
        ),
        error = function(e) NULL
      )
    )
  }

  if (!isTRUE(quiet)) {
    cli_inform(c("v" = "Loaded saved ConverseR settings from {.file {cfg_file}}."))
  }

  invisible(TRUE)
}

#' Configure ConverseR options, OpenRouter settings, and SKILL.md / ZIP bundles
#'
#' @description
#' Helper function to configure ConverseR's underlying `ellmer` Chat object,
#' custom system prompt, `SKILL.md` file(s) or `.zip` skill bundles,
#' context window size, and data frame preview dimensions.
#'
#' @param chat An `ellmer` `Chat` object (e.g., `ellmer::chat_openrouter()`).
#' @param style Optional character string for additional style instructions (defaults to `""`).
#' @param system_prompt Optional custom base system prompt string.
#' @param skill_file Optional path (or character vector of paths) to a markdown
#'   skill file (`"SKILL.md"`) OR a bundled skill archive (`"my_skill.zip"`)
#'   containing `SKILL.md` plus companion resource files (`references/`,
#'   `examples/`, `resources/`, `scripts/`).
#' @param skill_text Optional raw markdown text for a skill to append directly
#'   to the system prompt.
#' @param openrouter_api_key Optional OpenRouter API key (`"sk-or-v1-..."`).
#' @param dims Integer vector of length 2 `c(rows, cols)` controlling how many
#'   rows and columns of data frames are previewed in context. Default is `c(5L, 50L)`.
#' @param window_lines Integer number of lines above and below the cursor/selection
#'   to include as code context. Default is `5000L` (sends the entire document for
#'   files up to 10,000 lines). Set to `Inf` for unlimited lines.
#' @param max_env_objects Integer maximum number of `.GlobalEnv` objects to include
#'   with full `describe_object()` summaries in the context (in addition to the
#'   complete `.GlobalEnv` manifest). Default is `50L`.
#' @param enable_tools Logical; whether to register live R session introspection
#'   tools (`inspect_data`, `inspect_model`, `search_r_help`, `list_env_objects`)
#'   on the chat. Default is `TRUE`.
#' @param persist Logical; if `TRUE`, saves the configuration persistently across
#'   R sessions (`converser_save_config()`). Default is `FALSE`.
#'
#' @returns Invisibly returns the previous options list.
#' @export
converser_config <- function(
  chat = NULL,
  style = NULL,
  system_prompt = NULL,
  skill_file = NULL,
  skill_text = NULL,
  openrouter_api_key = NULL,
  dims = NULL,
  window_lines = NULL,
  max_env_objects = NULL,
  enable_tools = NULL,
  persist = FALSE
) {
  if (!is.null(openrouter_api_key)) {
    clean_key <- trimws(openrouter_api_key)
    converser_env[["saved_api_key"]] <- clean_key
    if (nzchar(clean_key)) {
      Sys.setenv(OPENROUTER_API_KEY = clean_key)
    } else {
      Sys.unsetenv("OPENROUTER_API_KEY")
    }
  }

  opts <- list()
  if (!is.null(chat)) {
    if (!inherits(chat, "Chat")) {
      cli_abort("{.arg chat} must be an {.pkg ellmer} {.cls Chat} object.")
    }
    opts[["converser.chat"]] <- chat
    converser_env[["active_chat"]] <- NULL
    converser_env[["last_inline_chat"]] <- NULL
    converser_clear_context_cache()
  }
  if (!is.null(style)) {
    if (!is_string(style)) {
      cli_abort("{.arg style} must be a single character string.")
    }
    opts[["converser.style"]] <- style
  }
  if (!is.null(system_prompt)) {
    opts[["converser.system_prompt"]] <- system_prompt
  }
  if (!is.null(skill_file)) {
    opts[["converser.skill_file"]] <- skill_file
    loaded <- read_skill_files(skill_file)
    if (nzchar(loaded)) {
      opts[["converser.skill_text"]] <- loaded
    }
  }
  if (!is.null(skill_text)) {
    opts[["converser.skill_text"]] <- skill_text
  }
  if (!is.null(dims)) {
    if (length(dims) != 2L || !is.numeric(dims)) {
      cli_abort("{.arg dims} must be a numeric vector of length 2: {.code c(rows, cols)}.")
    }
    opts[["converser.dims"]] <- as.integer(dims)
  }
  if (!is.null(window_lines)) {
    if (length(window_lines) != 1L || !is.numeric(window_lines)) {
      cli_abort("{.arg window_lines} must be a single positive number or {.code Inf}.")
    }
    opts[["converser.window_lines"]] <- window_lines
  }
  if (!is.null(max_env_objects)) {
    if (length(max_env_objects) != 1L || !is.numeric(max_env_objects) || max_env_objects < 1) {
      cli_abort("{.arg max_env_objects} must be a positive integer or {.code Inf}.")
    }
    opts[["converser.max_env_objects"]] <- max_env_objects
  }
  if (!is.null(enable_tools)) {
    if (!is_bool(enable_tools)) {
      cli_abort("{.arg enable_tools} must be {.code TRUE} or {.code FALSE}.")
    }
    opts[["converser.enable_tools"]] <- enable_tools
  }

  res <- options(opts)

  if (isTRUE(persist)) {
    converser_save_config(api_key = openrouter_api_key)
  }

  invisible(res)
}

#' Configure an OpenRouter model with privacy (ZDR) and reasoning settings
#'
#' @description
#' Convenience helper (also powering the in-Sidebar **Settings** modal) that
#' builds an `ellmer::chat_openrouter()` object with OpenRouter's
#' Zero Data Retention (`zdr`), `data_collection`, and unified `reasoning`
#' parameters (`effort` / `exclude`), and sets it as the active ConverseR model.
#'
#' Pass `persist = TRUE` to save your settings (including `api_key`, `model`,
#' `reasoning_effort`, and `SKILL.md` / `.zip` bundle) as your persistent default
#' across future R sessions without manually editing `.Rprofile` or `.Renviron`.
#'
#' @param model OpenRouter model ID. Defaults to `"deepseek/deepseek-v4.1-flash"`.
#' @param reasoning_effort Reasoning effort level: `"default"` (omit `reasoning`
#'   parameter so non-reasoning and reasoning models use their native default),
#'   `"none"` (sets `reasoning = list(exclude = TRUE)`), `"minimal"`, `"low"`,
#'   `"medium"`, or `"high"`.
#' @param zdr Logical; if `TRUE` (default), restricts routing exclusively to
#'   Zero Data Retention endpoints (`zdr = TRUE`).
#' @param data_collection Either `"deny"` (default, blocks any provider that trains
#'   on data) or `"allow"`.
#' @param allow_fallbacks Logical; whether OpenRouter may fall back to other providers
#'   if the primary provider is down. Default `TRUE`.
#' @param system_prompt Optional custom base system prompt.
#' @param skill_text Optional `SKILL.md` markdown content to append to the system prompt.
#' @param skill_file Optional path to a `SKILL.md` file or `.zip` skill bundle on disk.
#' @param api_key Optional OpenRouter API key (`"sk-or-v1-..."`). If provided,
#'   sets `OPENROUTER_API_KEY` for the session (and saves it across sessions if `persist = TRUE`).
#' @param persist Logical; if `TRUE`, saves these settings to `converser_config_path()`
#'   so they load automatically in future R sessions. Default is `FALSE`.
#' @returns Invisibly returns the newly configured `ellmer` `Chat` object.
#' @export
converser_use_openrouter <- function(
  model = "deepseek/deepseek-v4.1-flash",
  reasoning_effort = "low",
  zdr = TRUE,
  data_collection = c("deny", "allow"),
  allow_fallbacks = TRUE,
  system_prompt = getOption("converser.system_prompt"),
  skill_text = getOption("converser.skill_text"),
  skill_file = getOption("converser.skill_file"),
  api_key = NULL,
  persist = FALSE
) {
  data_collection <- arg_match(data_collection)

  if (!is.null(api_key)) {
    clean_key <- trimws(api_key)
    converser_env[["saved_api_key"]] <- clean_key
    if (nzchar(clean_key)) {
      Sys.setenv(OPENROUTER_API_KEY = clean_key)
    } else {
      Sys.unsetenv("OPENROUTER_API_KEY")
    }
  }

  provider_cfg <- list(
    data_collection = data_collection,
    zdr = isTRUE(zdr),
    allow_fallbacks = isTRUE(allow_fallbacks)
  )

  api_args <- list(provider = provider_cfg)

  if (!is.null(reasoning_effort) && nzchar(trimws(reasoning_effort)) && !identical(reasoning_effort, "default")) {
    if (identical(reasoning_effort, "none")) {
      api_args$reasoning <- list(exclude = TRUE)
    } else {
      api_args$reasoning <- list(effort = trimws(reasoning_effort))
    }
  }

  if (!is.null(skill_file) && nzchar(trimws(paste(skill_file, collapse = "")))) {
    options(converser.skill_file = skill_file)
    if (is.null(skill_text) || !nzchar(trimws(skill_text))) {
      skill_text <- read_skill_files(skill_file)
    }
  }
  if (!is.null(skill_text)) {
    options(converser.skill_text = skill_text)
  }
  if (!is.null(system_prompt)) {
    options(converser.system_prompt = system_prompt)
  }

  converser_env[["openrouter_settings"]] <- list(
    model = model,
    reasoning_effort = reasoning_effort %||% "default",
    zdr = isTRUE(zdr),
    data_collection = data_collection,
    allow_fallbacks = isTRUE(allow_fallbacks)
  )

  active_key <- converser_env[["saved_api_key"]] %||% ""
  chat <- NULL
  if (nzchar(active_key)) {
    chat <- suppressWarnings(
      ellmer::chat_openrouter(
        model = model,
        api_key = active_key,
        api_args = api_args,
        system_prompt = if (!is.null(system_prompt) && nzchar(trimws(system_prompt))) system_prompt else "You are an expert R programmer."
      )
    )
  }

  options(converser.chat = chat, gander.chat = chat)
  converser_env[["active_chat"]] <- NULL
  converser_env[["last_inline_chat"]] <- NULL
  converser_clear_context_cache()

  if (isTRUE(persist)) {
    converser_save_config(api_key = api_key)
  }

  invisible(chat)
}

#' Print and verify the active or last-used OpenRouter / LLM configuration
#'
#' @description
#' Prints a clear, structured summary of the model, provider URL, OpenRouter
#' `api_args` (including `reasoning$effort`, `provider$zdr`, and `provider$data_collection`),
#' active `SKILL.md` / ZIP bundle status, and token usage from the most recent
#' ConverseR call (or currently configured chat) so you can verify that model
#' and reasoning changes took effect.
#'
#' @returns Invisibly returns a structured list of the configuration details.
#' @export
converser_inspect_config <- function() {
  last_chat <- converser_env[["last_call_chat"]] %||%
    converser_env[["active_chat"]] %||%
    converser_env[["last_inline_chat"]] %||%
    getOption("converser.chat", getOption("gander.chat", getOption(".gander_chat")))

  if (is.null(last_chat) || !inherits(last_chat, "Chat")) {
    cli_inform(c("!" = "No active or configured {.cls Chat} object found."))
    return(invisible(NULL))
  }

  prov <- tryCatch(last_chat$get_provider(), error = function(e) NULL)
  model <- tryCatch(last_chat$get_model(), error = function(e) "<unknown>")
  prov_name <- tryCatch(prov@name, error = function(e) class(prov)[1])
  base_url <- tryCatch(prov@base_url, error = function(e) "<default>")
  extra_args <- tryCatch(prov@extra_args, error = function(e) list())
  sys_prompt <- tryCatch(last_chat$get_system_prompt(), error = function(e) "")
  turns <- tryCatch(last_chat$get_turns(), error = function(e) list())
  tokens <- tryCatch(last_chat$get_tokens(), error = function(e) NULL)

  skill_txt <- get_active_skill_text()
  bundle_files <- converser_env[["skill_bundle_files"]] %||% character(0)
  cfg_path <- converser_config_path()

  cat("── ConverseR Active / Last-Call LLM Configuration ───────────────────────\n")
  cat(sprintf("• Provider        : %s (%s)\n", prov_name, base_url))
  cat(sprintf("• Model           : %s\n", model))

  if (length(extra_args) > 0) {
    cat("• API Extra Args  :\n")
    json_str <- jsonlite::toJSON(extra_args, auto_unbox = TRUE, pretty = TRUE)
    cat(paste0("    ", strsplit(json_str, "\n")[[1]], collapse = "\n"), "\n")
  } else {
    cat("• API Extra Args  : <none / model defaults>\n")
  }

  cat(sprintf(
    "• Active SKILL.md : %s\n",
    if (nzchar(trimws(skill_txt))) sprintf("Yes (%d chars, %d bundled files)", nchar(skill_txt), length(bundle_files)) else "None"
  ))
  if (length(bundle_files) > 0) {
    cat(sprintf("• Bundled Files   : %s\n", paste(bundle_files, collapse = ", ")))
  }
  cat(sprintf("• Saved Defaults  : %s\n", if (file.exists(cfg_path)) cfg_path else "Not saved yet"))
  cat(sprintf("• System Prompt   : %d characters\n", nchar(sys_prompt %||% "")))
  cat(sprintf("• Recorded Turns  : %d turn(s) in this Chat instance\n", length(turns)))

  if (is.data.frame(tokens) && nrow(tokens) > 0) {
    cat(sprintf(
      "• Token Usage     : %d input / %d output tokens\n",
      sum(tokens$input, na.rm = TRUE),
      sum(tokens$output, na.rm = TRUE)
    ))
  }
  cat("─────────────────────────────────────────────────────────────────────────\n")

  invisible(list(
    provider = prov_name,
    base_url = base_url,
    model = model,
    api_args = extra_args,
    has_skill = nzchar(trimws(skill_txt)),
    bundled_files = bundle_files,
    system_prompt_chars = nchar(sys_prompt %||% ""),
    turns = length(turns)
  ))
}

#' Open the interactive HTML Getting Started Guide for ConverseR
#'
#' @description
#' Opens the interactive HTML Getting Started Guide for `ConverseR` in your
#' default web browser or the RStudio Viewer pane. Covers OpenRouter setup,
#' saving persistent API keys and model settings, binding keyboard shortcuts
#' for `converser_inline()`, creating `SKILL.md` and `.zip` skill bundles from
#' your existing R code, and using Console vs. Script actions.
#'
#' @param browser Logical; if `TRUE` (default), opens in your default web browser
#'   for full-screen reading; if `FALSE`, opens in the RStudio Viewer pane.
#' @export
converser_guide <- function(browser = TRUE) {
  guide_url <- "https://tironesr.github.io/ConverseR/ConverseR-guide.html"

  if (!isTRUE(browser) && rstudioapi::isAvailable() && rstudioapi::hasFun("viewer")) {
    rstudioapi::viewer(guide_url)
  } else {
    utils::browseURL(guide_url)
  }
  invisible(guide_url)
}

# Read SKILL.md or unpack a bundled Skill .zip archive -------------------------

read_skill_files <- function(paths) {
  if (is.null(paths) || length(paths) == 0) return("")
  blocks <- character(0)
  all_bundled <- character(0)

  for (p in paths) {
    p_exp <- path.expand(trimws(p))
    if (!nzchar(p_exp) || !file.exists(p_exp)) next

    if (grepl("\\.zip$", tolower(p_exp))) {
      zip_res <- extract_skill_zip_bundle(p_exp)
      if (nzchar(zip_res$text)) {
        blocks <- c(blocks, zip_res$text)
        all_bundled <- c(all_bundled, zip_res$files)
      }
    } else if (dir.exists(p_exp)) {
      dir_res <- read_skill_directory(p_exp)
      if (nzchar(dir_res$text)) {
        blocks <- c(blocks, dir_res$text)
        all_bundled <- c(all_bundled, dir_res$files)
      }
    } else {
      lines <- tryCatch(readLines(p_exp, warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
      if (length(lines) > 0) {
        blocks <- c(blocks, paste(lines, collapse = "\n"))
        all_bundled <- c(all_bundled, basename(p_exp))
      }
    }
  }

  converser_env[["skill_bundle_files"]] <- unique(all_bundled)
  paste(blocks, collapse = "\n\n")
}

#' Extract a bundled Skill `.zip` archive containing `SKILL.md` and companion resources
#' @keywords internal
extract_skill_zip_bundle <- function(zip_path) {
  dest_dir <- file.path(tempdir(), paste0("converser_skill_", as.integer(Sys.time())))
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  utils::unzip(zip_path, exdir = dest_dir)
  converser_env[["skill_bundle_dir"]] <- dest_dir

  read_skill_directory(dest_dir)
}

read_skill_directory <- function(root_dir) {
  all_files <- list.files(root_dir, recursive = TRUE, full.names = TRUE)
  all_files <- all_files[!grepl("(__MACOSX|/\\.|\\\\\\.)", all_files)]

  if (length(all_files) == 0) {
    return(list(text = "", files = character(0)))
  }

  rel_names <- sub(
    paste0("^", gsub("\\\\", "/", normalizePath(root_dir, winslash = "/", mustWork = FALSE)), "/?"),
    "",
    gsub("\\\\", "/", normalizePath(all_files, winslash = "/", mustWork = FALSE))
  )

  is_skill_md <- tolower(basename(all_files)) == "skill.md"
  text_exts <- "\\.(md|r|rmd|qmd|txt|json|ya?ml|sql|py|sh|csv)$"
  is_text_res <- !is_skill_md & grepl(text_exts, tolower(all_files))

  sections <- character(0)

  for (idx in which(is_skill_md)) {
    lines <- tryCatch(readLines(all_files[idx], warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
    if (length(lines) > 0) {
      sections <- c(sections, paste(lines, collapse = "\n"))
    }
  }

  if (any(is_text_res)) {
    sections <- c(sections, "\n### Bundled Skill Reference & Resource Files")
    for (idx in which(is_text_res)) {
      lines <- tryCatch(readLines(all_files[idx], warn = FALSE, encoding = "UTF-8"), error = function(e) character(0))
      if (length(lines) == 0) next
      max_l <- if (grepl("\\.csv$", tolower(all_files[idx]))) 40L else 250L
      if (length(lines) > max_l) {
        lines <- c(lines[1:max_l], sprintf("# [... truncated %d additional lines in %s ...]", length(lines) - max_l, rel_names[idx]))
      }
      sections <- c(
        sections,
        sprintf("\n#### Resource File: `%s` (local path: `%s`)\n`````\n%s\n`````",
                rel_names[idx],
                gsub("\\\\", "/", all_files[idx]),
                paste(lines, collapse = "\n"))
      )
    }
  }

  other_idx <- which(!is_skill_md & !is_text_res)
  if (length(other_idx) > 0) {
    bin_lines <- vapply(
      other_idx,
      function(i) sprintf("- `%s` (extracted to: `%s`)", rel_names[i], gsub("\\\\", "/", all_files[i])),
      character(1)
    )
    sections <- c(
      sections,
      "\n### Additional Bundled Data/Binary Files (Available on Disk):",
      bin_lines
    )
  }

  list(
    text = paste(sections, collapse = "\n\n"),
    files = rel_names
  )
}

get_active_skill_text <- function() {
  txt <- getOption("converser.skill_text", "")
  if (nzchar(trimws(txt))) {
    return(txt)
  }
  f <- getOption("converser.skill_file")
  if (!is.null(f)) {
    return(read_skill_files(f))
  }
  ""
}

inspect_current_settings <- function(include_api_key = TRUE) {
  saved <- converser_env[["openrouter_settings"]]
  chat_opt <- getOption("converser.chat", getOption("gander.chat", getOption(".gander_chat")))

  model <- "deepseek/deepseek-v4.1-flash"
  zdr <- TRUE
  data_collection <- "deny"
  reasoning_effort <- "low"
  allow_fallbacks <- TRUE
  base_sys_prompt <- getOption("converser.system_prompt", "")

  if (!is.null(chat_opt) && inherits(chat_opt, "Chat")) {
    model <- tryCatch(chat_opt$get_model(), error = function(e) model)
    prov <- tryCatch(chat_opt$get_provider(), error = function(e) NULL)
    if (!is.null(prov)) {
      extra <- tryCatch(prov@extra_args, error = function(e) list())
      if (!is.null(extra$provider)) {
        if (!is.null(extra$provider$zdr)) zdr <- isTRUE(extra$provider$zdr)
        if (!is.null(extra$provider$data_collection)) data_collection <- as.character(extra$provider$data_collection)
        if (!is.null(extra$provider$allow_fallbacks)) allow_fallbacks <- isTRUE(extra$provider$allow_fallbacks)
      }
      if (!is.null(extra$reasoning)) {
        if (isTRUE(extra$reasoning$exclude)) {
          reasoning_effort <- "none"
        } else if (!is.null(extra$reasoning$effort)) {
          reasoning_effort <- as.character(extra$reasoning$effort)
        }
      } else {
        reasoning_effort <- "default"
      }
    }
    if (!nzchar(base_sys_prompt)) {
      orig_sys <- tryCatch(chat_opt$get_system_prompt(), error = function(e) "")
      if (nzchar(orig_sys) && !grepl("^You are (ConverseR|an expert R programmer)", orig_sys)) {
        base_sys_prompt <- orig_sys
      }
    }
  } else if (!is.null(saved)) {
    model <- saved$model %||% model
    zdr <- isTRUE(saved$zdr)
    data_collection <- saved$data_collection %||% data_collection
    reasoning_effort <- saved$reasoning_effort %||% reasoning_effort
    allow_fallbacks <- isTRUE(saved$allow_fallbacks)
  }

  explicit_key <- converser_env[["saved_api_key"]] %||% ""
  api_key_val <- if (isTRUE(include_api_key)) explicit_key else ""

  list(
    openrouter_api_key = api_key_val,
    has_api_key = nzchar(explicit_key),
    model = model,
    reasoning_effort = reasoning_effort,
    zdr = zdr,
    data_collection = data_collection,
    allow_fallbacks = allow_fallbacks,
    system_prompt = base_sys_prompt %||% "",
    skill_file = paste(getOption("converser.skill_file", ""), collapse = ", "),
    skill_text = get_active_skill_text(),
    bundled_files = converser_env[["skill_bundle_files"]] %||% character(0),
    has_saved_config = file.exists(converser_config_path())
  )
}

#' Inspect or retrieve the active ConverseR Chat session
#'
#' @description
#' Returns the `ellmer` `Chat` object from the most recent inline or sidebar
#' interaction (`converser_peek()`), or clears the persistent multi-turn history
#' (`converser_reset()`).
#'
#' @returns The `ellmer` `Chat` object, or `NULL` if no session has occurred yet.
#' @export
converser_peek <- function() {
  converser_env[["last_call_chat"]] %||% converser_env[["active_chat"]] %||% converser_env[["last_inline_chat"]]
}

#' @rdname converser_peek
#' @export
converser_reset <- function() {
  converser_env[["active_chat"]] <- NULL
  converser_env[["last_inline_chat"]] <- NULL
  converser_env[["last_call_chat"]] <- NULL
  converser_env[["last_edit"]] <- NULL
  converser_env[["chat_history_ui"]] <- list()
  converser_clear_context_cache()
  cli_inform(c("v" = "ConverseR multi-turn chat history and state have been reset."))
  invisible(NULL)
}

converser_clear_context_cache <- function() {
  converser_env[["cached_doc_hash"]] <- NULL
  converser_env[["cached_manifest_hash"]] <- NULL
  converser_env[["cached_env_hashes"]] <- list()
  invisible(NULL)
}

#' Undo the last ConverseR inline edit in a single step
#'
#' @description
#' Snapshots the document state before applying edits so you can revert the
#' entire AI generation cleanly in one step.
#'
#' @export
converser_undo <- function() {
  last_edit <- converser_env[["last_edit"]]
  if (is.null(last_edit)) {
    cli_inform(c("!" = "No recent ConverseR inline edit found to undo."))
    return(invisible(NULL))
  }

  if (rstudioapi::isAvailable() && !is.null(last_edit$doc_id) && !is.null(last_edit$full_contents)) {
    ctx <- rstudioapi::getSourceEditorContext(id = last_edit$doc_id)
    full_range <- rstudioapi::document_range(
      rstudioapi::document_position(1, 1),
      rstudioapi::document_position(length(ctx$contents) + 1, 100000)
    )
    rstudioapi::modifyRange(
      location = full_range,
      text = paste0(last_edit$full_contents, collapse = "\n"),
      id = last_edit$doc_id
    )
    if (!is.null(last_edit$original_range)) {
      rstudioapi::setSelectionRanges(last_edit$original_range, id = last_edit$doc_id)
    }
    converser_env[["last_edit"]] <- NULL
    cli_inform(c("v" = "Reverted document to state prior to last ConverseR edit."))
  }
  invisible(NULL)
}

get_converser_chat <- function(register_tools = TRUE) {
  chat_opt <- getOption(
    "converser.chat",
    getOption("gander.chat", getOption(".gander_chat"))
  )

  # If no chat option is set yet, try loading saved user config
  if (is.null(chat_opt)) {
    converser_load_saved_config(quiet = TRUE, init_chat = TRUE)
    chat_opt <- getOption(
      "converser.chat",
      getOption("gander.chat", getOption(".gander_chat"))
    )
  }

  explicit_key <- converser_env[["saved_api_key"]] %||% ""
  if (is.null(chat_opt) && nzchar(explicit_key)) {
    saved <- converser_env[["openrouter_settings"]] %||% list()
    chat_opt <- tryCatch(
      converser_use_openrouter(
        model = saved$model %||% "deepseek/deepseek-v4.1-flash",
        reasoning_effort = saved$reasoning_effort %||% "low",
        zdr = if (is.null(saved$zdr)) TRUE else isTRUE(saved$zdr),
        data_collection = saved$data_collection %||% "deny",
        allow_fallbacks = if (is.null(saved$allow_fallbacks)) TRUE else isTRUE(saved$allow_fallbacks),
        api_key = explicit_key
      ),
      error = function(e) NULL
    )
  }

  if (is.null(chat_opt)) {
    cli_inform(
      c(
        "!" = "{.pkg ConverseR} requires an OpenRouter API key and model configuration.",
        "i" = "Open {.code converser()} and click {.strong Settings} to enter your OpenRouter API key and save your defaults.",
        "i" = "Or run {.code converser_guide()} for the Getting Started Guide."
      ),
      call = NULL
    )
    return(NULL)
  }

  if (!inherits(chat_opt, "Chat")) {
    cli_inform(
      c(
        "!" = "The configured chat option must be an {.pkg ellmer} {.cls Chat} object, not {.obj_type_friendly {chat_opt}}."
      ),
      call = NULL
    )
    return(NULL)
  }

  chat <- chat_opt$clone()

  enable_tools <- getOption("converser.enable_tools", TRUE)
  if (isTRUE(enable_tools) && isTRUE(register_tools)) {
    tryCatch(
      register_converser_tools(chat),
      error = function(e) NULL
    )
  }

  chat
}

get_converser_style <- function() {
  res <- getOption("converser.style", "")
  if (!is.null(res) && is_string(res)) {
    return(res)
  }
  ""
}

get_converser_dims <- function() {
  dims <- getOption(
    "converser.dims",
    getOption("gander.dims", getOption(".gander_dims"))
  )
  if (is.null(dims) || length(dims) != 2L || !is.numeric(dims)) {
    return(c(5L, 50L))
  }
  as.integer(dims)
}

get_converser_window <- function() {
  win <- getOption("converser.window_lines", 5000L)
  if (!is.numeric(win) || length(win) != 1L || win < 1) {
    return(5000L)
  }
  win
}

get_converser_max_env_objects <- function() {
  mx <- getOption("converser.max_env_objects", 50L)
  if (!is.numeric(mx) || length(mx) != 1L || mx < 1) {
    return(50L)
  }
  mx
}

file_extension <- function(file_path) {
  if (is.null(file_path) || !is_character(file_path) || length(file_path) == 0 || identical(file_path, "")) {
    return("r")
  }
  ext <- strsplit(basename(file_path), "\\.")[[1]]
  if (length(ext) <= 1) {
    return("r")
  }
  tolower(ext[length(ext)])
}

is_positron <- function() {
  Sys.getenv("POSITRON") == "1"
}

global_env <- function() {
  .GlobalEnv
}
