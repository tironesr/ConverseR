# Code & Environment Context Extraction (Expanded + Hash-Cached) --------------

#' Extract full/windowed code context from an RStudio/Positron document with hash caching
#'
#' @description
#' Extracts the active document context (sending the entire document by default for
#' files up to 10,000 lines via `window_lines = 5000L`, and preserving YAML frontmatter
#' and `library()`/`require()` calls if a massive file is windowed).
#' When `cache_context = TRUE` (used in multi-turn sidebar conversations), if the
#' document contents and path have not changed since an earlier turn in the same
#' conversation, `fetch_code_context()` marks `doc_cached = TRUE` so the full
#' document is not redundantly re-transmitted on every message.
#'
#' @param context An RStudio document context from `rstudioapi::getActiveDocumentContext()`.
#' @param window_lines Number of lines above and below the cursor/selection to include
#'   (default `5000L`, which includes the entire file for scripts up to 10,000 lines).
#' @param cache_context Logical; whether to deduplicate unchanged document contents
#'   across multi-turn chat calls using `rlang::hash()`.
#' @returns A list containing `file_contents`, `raw_contents`, `doc_cached`,
#'   `total_lines`, `selection`, `selection_raw`, `file_path`, `ext`, and `cursor_line`.
#' @keywords internal
fetch_code_context <- function(context, window_lines = get_converser_window(), cache_context = FALSE) {
  if (is.null(context)) {
    return(list(
      file_contents = character(0),
      raw_contents = character(0),
      doc_cached = FALSE,
      total_lines = 0L,
      selection = character(0),
      selection_raw = "",
      file_path = "",
      ext = "r",
      cursor_line = 1L
    ))
  }

  contents <- context$contents %||% character(0)
  primary_sel <- rstudioapi::primary_selection(context)
  selection_text <- primary_sel[["text"]] %||% ""

  start_row <- primary_sel$range$start[["row"]] %||% 1L
  end_row <- primary_sel$range$end[["row"]] %||% start_row
  file_path <- context$path %||% ""
  ext <- file_extension(file_path)
  has_nonempty_doc <- length(contents) > 0 && !(length(contents) == 1L && identical(contents, ""))

  windowed_contents <- window_document_contents(
    contents = contents,
    start_row = start_row,
    end_row = end_row,
    window_lines = window_lines
  )

  doc_cached <- FALSE
  if (isTRUE(cache_context) && has_nonempty_doc) {
    doc_hash <- rlang::hash(list(path = file_path, contents = windowed_contents))
    if (identical(doc_hash, converser_env[["cached_doc_hash"]])) {
      doc_cached <- TRUE
    } else {
      converser_env[["cached_doc_hash"]] <- doc_hash
    }
  }

  list(
    file_contents = if (doc_cached) character(0) else fence_block(windowed_contents),
    raw_contents = contents,
    doc_cached = doc_cached,
    total_lines = length(contents),
    selection = fence_block(selection_text),
    selection_raw = selection_text,
    file_path = file_path,
    ext = ext,
    cursor_line = start_row
  )
}

window_document_contents <- function(contents, start_row, end_row, window_lines = 5000L) {
  n_total <- length(contents)
  if (n_total == 0 || (n_total == 1 && identical(contents, ""))) {
    return(character(0))
  }

  if (is.infinite(window_lines) || n_total <= (window_lines * 2L + (end_row - start_row + 1L))) {
    return(contents)
  }

  win_start <- max(1L, start_row - as.integer(window_lines))
  win_end <- min(n_total, end_row + as.integer(window_lines))

  header_lines <- character(0)
  if (win_start > 1L) {
    pre_lines <- contents[seq_len(win_start - 1L)]

    # Preserve YAML frontmatter if the document begins with `---`
    yaml_lines <- character(0)
    if (grepl("^---\\s*$", pre_lines[1])) {
      end_yaml <- which(grepl("^(---|\\.\\.\\.)\\s*$", pre_lines[-1]))
      if (length(end_yaml) > 0 && (end_yaml[1] + 1L) <= min(length(pre_lines), 40L)) {
        yaml_lines <- pre_lines[seq_len(end_yaml[1] + 1L)]
      }
    }

    pkg_lines <- grep("^\\s*(library|require|source|box::use)\\s*\\(", pre_lines, value = TRUE)
    preserved <- c(yaml_lines, unique(pkg_lines))
    if (length(preserved) > 0) {
      header_lines <- c(
        "# [Document Header / Packages loaded earlier in file]:",
        preserved,
        sprintf("# [... omitted lines 1 to %d ...]", win_start - 1L)
      )
    } else {
      header_lines <- sprintf("# [... omitted lines 1 to %d ...]", win_start - 1L)
    }
  }

  footer_lines <- character(0)
  if (win_end < n_total) {
    footer_lines <- sprintf("# [... omitted lines %d to %d ...]", win_end + 1L, n_total)
  }

  c(header_lines, contents[win_start:win_end], footer_lines)
}

fence_block <- function(x) {
  if (length(x) == 0 || identical(x, "")) {
    return(character(0))
  }
  c("`````", x, "`````")
}

# Environment Inspection (Manifest + Expanded S3 Summaries + Hash Caching) -----

#' Extract R environment context with expanded `.GlobalEnv` coverage and hash caching
#'
#' @param selection Selected code string.
#' @param input User prompt string.
#' @param env Environment to inspect (defaults to `.GlobalEnv`).
#' @param document_contents Optional character vector of active document lines.
#' @param include_all Logical; if `TRUE` (used by `construct_turn()`), includes a
#'   manifest of all `.GlobalEnv` objects (up to 200) plus full `describe_object()`
#'   summaries for up to `max_objects` (default 50) objects in `.GlobalEnv` even if
#'   not explicitly named in the prompt. If `FALSE`, only describes objects explicitly
#'   mentioned in `selection` or `input`.
#' @param cache_context Logical; if `TRUE`, tracks `rlang::hash()` per object in
#'   `converser_env[["cached_env_hashes"]]` so unchanged `.GlobalEnv` objects are
#'   not re-transmitted on subsequent turns of the same conversation.
#' @param max_objects Maximum number of objects to describe in full (default `50L`).
#' @returns Character vector of formatted environment descriptions.
#' @keywords internal
fetch_env_context <- function(
  selection,
  input,
  env = global_env(),
  document_contents = character(0),
  include_all = FALSE,
  cache_context = FALSE,
  max_objects = get_converser_max_env_objects()
) {
  obj_names <- ls(envir = env, all.names = FALSE)
  if (length(obj_names) == 0) {
    return(character(0))
  }

  sel_vars <- extract_identifiers(selection)
  prompt_vars <- extract_mentioned_objects(input, obj_names)
  mentioned <- intersect(obj_names, unique(c(sel_vars, prompt_vars)))

  if (!isTRUE(include_all)) {
    if (length(mentioned) == 0) {
      return(character(0))
    }
    res <- describe_variables(mentioned, env)
    if (identical(res, character(0))) {
      return(character(0))
    }
    return(c("`````", res, "`````"))
  }

  # Expanded Mode (Option B1):
  # 1. Build a compact manifest of all objects in .GlobalEnv (up to 200 objects)
  manifest_lines <- build_env_manifest(obj_names, env = env, max_manifest = 200L)
  manifest_section <- character(0)
  if (length(manifest_lines) > 0) {
    m_hash <- rlang::hash(manifest_lines)
    if (isTRUE(cache_context) && identical(m_hash, converser_env[["cached_manifest_hash"]])) {
      manifest_section <- sprintf(
        "# [Global Environment (.GlobalEnv) manifest (%d objects) is unchanged from earlier in this conversation]",
        length(obj_names)
      )
    } else {
      if (isTRUE(cache_context)) {
        converser_env[["cached_manifest_hash"]] <- m_hash
      }
      manifest_section <- c(
        sprintf("# Global Environment (.GlobalEnv) Manifest (%d total objects):", length(obj_names)),
        manifest_lines,
        ""
      )
    }
  }

  # 2. Prioritize objects for full S3 `describe_object()` summaries up to `max_objects` (default 50)
  doc_vars <- intersect(obj_names, extract_identifiers(document_contents))

  # Classify remaining objects so data frames, models, hypothesis tests, and plots come before functions
  remaining <- setdiff(obj_names, c(mentioned, doc_vars))
  high_priority_rem <- character(0)
  med_priority_rem <- character(0)
  low_priority_rem <- character(0)

  for (nm in remaining) {
    obj <- tryCatch(env_get(env, nm), error = function(e) NULL)
    if (is.null(obj)) next
    if (is.data.frame(obj) || is_model_like(obj) || inherits(obj, c("htest", "ggplot"))) {
      high_priority_rem <- c(high_priority_rem, nm)
    } else if (!is.function(obj)) {
      med_priority_rem <- c(med_priority_rem, nm)
    } else {
      low_priority_rem <- c(low_priority_rem, nm)
    }
  }

  ordered_candidates <- unique(c(
    mentioned,
    doc_vars,
    high_priority_rem,
    med_priority_rem,
    low_priority_rem
  ))

  if (length(ordered_candidates) > max_objects) {
    ordered_candidates <- ordered_candidates[seq_len(as.integer(max_objects))]
  }

  # 3. Describe candidate objects with per-object hash caching
  cached_hashes <- converser_env[["cached_env_hashes"]] %||% list()
  described_blocks <- character(0)
  already_cached_names <- character(0)

  for (nm in ordered_candidates) {
    if (!env_has(env, nm)) next
    obj <- tryCatch(env_get(env, nm), error = function(e) NULL)
    if (is.null(obj)) next

    obj_hash <- tryCatch(rlang::hash(obj), error = function(e) NA_character_)
    if (isTRUE(cache_context) && !is.na(obj_hash) && identical(cached_hashes[[nm]], obj_hash)) {
      already_cached_names <- c(already_cached_names, nm)
      next
    }

    obj_desc <- tryCatch(
      describe_object(obj, nm),
      error = function(e) paste0("# ", nm, ": <error inspecting object: ", conditionMessage(e), ">")
    )
    described_blocks <- c(described_blocks, obj_desc, "")
    if (isTRUE(cache_context) && !is.na(obj_hash)) {
      cached_hashes[[nm]] <- obj_hash
    }
  }

  if (isTRUE(cache_context)) {
    converser_env[["cached_env_hashes"]] <- cached_hashes
  }

  cache_note <- character(0)
  if (length(already_cached_names) > 0) {
    cache_note <- sprintf(
      "# [Unchanged .GlobalEnv objects already described in full earlier in this conversation (%d): %s]",
      length(already_cached_names),
      paste(already_cached_names, collapse = ", ")
    )
  }

  combined <- c(manifest_section, described_blocks, cache_note)
  if (length(combined) == 0) {
    return(character(0))
  }

  c("`````", combined, "`````")
}

build_env_manifest <- function(obj_names, env = global_env(), max_manifest = 200L) {
  if (length(obj_names) == 0) return(character(0))
  shown_names <- if (length(obj_names) > max_manifest) obj_names[seq_len(max_manifest)] else obj_names

  lines <- vapply(
    shown_names,
    function(nm) {
      tryCatch(
        {
          x <- env_get(env, nm)
          cls <- paste(class(x), collapse = "/")
          if (is.data.frame(x)) {
            cols <- names(x)
            col_preview <- if (length(cols) > 25L) {
              paste0(paste(cols[1:25], collapse = ", "), sprintf(", ... (+%d more)", length(cols) - 25L))
            } else {
              paste(cols, collapse = ", ")
            }
            sprintf("# - %s: <%s [%d x %d]> cols: %s", nm, cls, nrow(x), ncol(x), col_preview)
          } else if (is_model_like(x)) {
            fml <- tryCatch(deparse1(stats::formula(x)), error = function(e) "")
            sprintf("# - %s: <%s> %s", nm, cls, fml)
          } else if (!is.null(dim(x))) {
            sprintf("# - %s: <%s [%s]>", nm, cls, paste(dim(x), collapse = " x "))
          } else if (is.function(x)) {
            sprintf("# - %s: <function(%s)>", nm, paste(names(formals(x)), collapse = ", "))
          } else {
            sprintf("# - %s: <%s (len=%d)>", nm, cls, length(x))
          }
        },
        error = function(e) sprintf("# - %s: <uninspectable>", nm)
      )
    },
    character(1)
  )

  if (length(obj_names) > max_manifest) {
    lines <- c(lines, sprintf("# - ... and %d additional objects in .GlobalEnv", length(obj_names) - max_manifest))
  }
  unname(lines)
}

#' Extract syntactically valid R identifiers using treesitter.r with safe fallback
#' @keywords internal
extract_identifiers <- function(code_text) {
  if (is.null(code_text) || length(code_text) == 0) {
    return(character(0))
  }
  code_str <- paste(code_text, collapse = "\n")
  if (!nzchar(trimws(code_str))) {
    return(character(0))
  }

  tryCatch(
    {
      language <- treesitter.r::language()
      parser <- treesitter::parser(language)
      tree <- treesitter::parser_parse(parser, code_str)
      root <- treesitter::tree_root_node(tree)

      query_source <- '
        (identifier) @id
        (call function: (identifier) @func)
      '
      query <- treesitter::query(language, query_source)
      captures <- treesitter::query_captures(query, root)

      if (length(captures$node) == 0) {
        return(tokenize_identifiers_fallback(code_str))
      }

      ids <- unique(vapply(captures$node, treesitter::node_text, character(1)))
      unique(c(ids, tokenize_identifiers_fallback(code_str)))
    },
    error = function(e) {
      tokenize_identifiers_fallback(code_str)
    }
  )
}

tokenize_identifiers_fallback <- function(text) {
  if (!nzchar(text)) return(character(0))
  tokens <- unlist(regmatches(text, gregexpr("[A-Za-z.][A-Za-z0-9._]*", text)))
  unique(tokens)
}

#' Match object names in natural language input using strict token boundaries
#' @keywords internal
extract_mentioned_objects <- function(input, obj_names) {
  if (is.null(input) || !nzchar(trimws(input)) || length(obj_names) == 0) {
    return(character(0))
  }

  prompt_tokens <- tokenize_identifiers_fallback(input)

  backtick_matches <- unlist(regmatches(input, gregexpr("`([^`]+)`", input)))
  if (length(backtick_matches) > 0) {
    backtick_clean <- gsub("^`|`$", "", backtick_matches)
    prompt_tokens <- unique(c(prompt_tokens, backtick_clean))
  }

  english_stopwords <- c(
    "a", "an", "as", "at", "be", "by", "do", "for", "if", "in", "is", "it",
    "me", "my", "no", "of", "on", "or", "so", "to", "up", "us", "we"
  )

  matched <- character(0)
  for (nm in obj_names) {
    if (tolower(nm) %in% english_stopwords) {
      if (paste0("`", nm, "`") %in% backtick_matches) {
        matched <- c(matched, nm)
      }
      next
    }

    if (nm %in% prompt_tokens) {
      matched <- c(matched, nm)
    }
  }

  unique(matched)
}

describe_variables <- function(variables, env) {
  if (length(variables) == 0) {
    return(character(0))
  }

  res <- character(0)
  for (variable in variables) {
    if (!env_has(env, variable)) next
    obj_desc <- tryCatch(
      {
        obj <- env_get(env, variable)
        describe_object(obj, variable)
      },
      error = function(e) {
        paste0("# ", variable, ": <error inspecting object: ", conditionMessage(e), ">")
      }
    )
    res <- c(res, obj_desc, "")
  }

  res
}

#' S3 Generic for concise, token-efficient, non-blocking R object descriptions
#'
#' @param x An R object.
#' @param name Name of the object in the environment.
#' @export
describe_object <- function(x, name = "object") {
  UseMethod("describe_object")
}

#' @export
describe_object.data.frame <- function(x, name = "data.frame") {
  dims <- get_converser_dims()
  total_rows <- nrow(x)
  total_cols <- ncol(x)
  n_row <- min(dims[1], total_rows)
  n_col <- min(dims[2], total_cols)

  header <- sprintf(
    "# %s: <%s [%d rows x %d cols]> (showing first %d rows, %d cols)",
    name, paste(class(x), collapse = "/"), total_rows, total_cols, n_row, n_col
  )

  # 1. Detect active grouping variables on grouped_df (dplyr::group_by)
  group_line <- character(0)
  grp_attr <- attr(x, "groups", exact = TRUE)
  if (inherits(x, "grouped_df") && is.data.frame(grp_attr) && ncol(grp_attr) > 1L) {
    grp_vars <- setdiff(names(grp_attr), ".rows")
    if (length(grp_vars) > 0) {
      group_line <- sprintf("#> Active Groups (%d groups): %s", nrow(grp_attr), paste(grp_vars, collapse = ", "))
    }
  }

  sub_x <- x[seq_len(n_row), seq_len(n_col), drop = FALSE]
  str_out <- utils::capture.output(
    utils::str(sub_x, vec.len = 4, give.attr = FALSE, strict.width = "cut", width = 100)
  )

  cols_subset <- x[, seq_len(n_col), drop = FALSE]

  # 2. Missing value (NA) counts
  na_counts <- vapply(cols_subset, function(col) sum(is.na(col)), integer(1))
  na_cols <- na_counts[na_counts > 0]
  na_line <- character(0)
  if (length(na_cols) > 0) {
    na_summary <- paste(names(na_cols), na_cols, sep = "=", collapse = ", ")
    na_line <- paste0("#> Missing values (NAs): ", na_summary)
  }

  # 3. Complete Factor Levels (up to 10 levels per factor column so str() doesn't truncate them)
  factor_lines <- character(0)
  is_fac <- vapply(cols_subset, is.factor, logical(1))
  if (any(is_fac)) {
    fac_summaries <- vapply(
      names(cols_subset)[is_fac],
      function(cn) {
        lvls <- levels(cols_subset[[cn]])
        if (length(lvls) > 10) {
          lvl_str <- paste0(paste(paste0("'", lvls[1:10], "'"), collapse = ", "), sprintf(", ... (%d total)", length(lvls)))
        } else {
          lvl_str <- paste(paste0("'", lvls, "'"), collapse = ", ")
        }
        sprintf("%s: [%s]", cn, lvl_str)
      },
      character(1)
    )
    factor_lines <- c("#> Factor Levels:", paste0("#>   ", unname(fac_summaries)))
  }

  # 4. Survey Variable & Value Labels (`haven_labelled` / SPSS / Stata / SAS / Qualtrics)
  label_lines <- character(0)
  for (cn in names(cols_subset)) {
    col <- cols_subset[[cn]]
    var_lbl <- attr(col, "label", exact = TRUE)
    val_lbls <- attr(col, "labels", exact = TRUE)
    has_var_lbl <- !is.null(var_lbl) && is.character(var_lbl) && length(var_lbl) == 1L && nzchar(trimws(var_lbl))
    has_val_lbls <- !is.null(val_lbls) && length(val_lbls) > 0L

    if (has_var_lbl || has_val_lbls) {
      entry <- paste0("#>   ", cn, ": ")
      if (has_var_lbl) {
        entry <- paste0(entry, "'", trimws(var_lbl), "'")
      }
      if (has_val_lbls) {
        vl_sub <- utils::head(val_lbls, 8L)
        vl_str <- paste(paste0(unname(vl_sub), "='", names(vl_sub), "'"), collapse = ", ")
        if (length(val_lbls) > 8L) {
          vl_str <- paste0(vl_str, sprintf(", ... (%d codes)", length(val_lbls)))
        }
        entry <- paste0(entry, if (has_var_lbl) " " else "", "{", vl_str, "}")
      }
      label_lines <- c(label_lines, entry)
    }
  }
  if (length(label_lines) > 0) {
    label_lines <- c("#> Survey Variable & Value Labels (haven/labelled):", label_lines)
  }

  c(
    header,
    group_line,
    paste0("#> ", gsub("\t.*$", "", str_out)),
    na_line,
    factor_lines,
    label_lines
  )
}

is_model_like <- function(x) {
  if (inherits(x, c("lm", "glm", "fixest", "merMod", "lmerMod", "glmerMod", "coxph", "lavaan", "nls", "gam", "polr", "multinom", "rq", "svyglm"))) {
    return(TRUE)
  }
  if (!is.data.frame(x) && !is.function(x) && (!is.list(x) || !identical(class(x), "list"))) {
    has_fml <- !is.null(tryCatch(stats::formula(x), error = function(e) NULL))
    if (has_fml) return(TRUE)
  }
  FALSE
}

#' Shared helper to format statistical models (`lm`, `glm`, `fixest`, `merMod`, `coxph`, `lavaan`, etc.)
#' @keywords internal
describe_model_object <- function(x, name = "model") {
  cls <- paste(class(x), collapse = "/")
  fml <- tryCatch(deparse1(stats::formula(x)), error = function(e) "<unknown>")
  n_obs <- tryCatch(stats::nobs(x), error = function(e) NA_integer_)

  fam_info <- ""
  if (inherits(x, "glm")) {
    fam_obj <- tryCatch(stats::family(x), error = function(e) NULL)
    if (!is.null(fam_obj) && !is.null(fam_obj$family)) {
      fam_info <- sprintf(" | family: %s(link = '%s')", fam_obj$family, fam_obj$link %||% "")
    }
  }

  lines <- c(
    sprintf("# %s: <%s> formula: %s%s (n = %s)", name, cls, fml, fam_info, as.character(n_obs))
  )

  tidy_df <- NULL
  glance_df <- NULL

  if (requireNamespace("broom", quietly = TRUE)) {
    tidy_df <- tryCatch(
      as.data.frame(broom::tidy(x, conf.int = TRUE)),
      error = function(e) tryCatch(as.data.frame(broom::tidy(x)), error = function(e2) NULL)
    )
    glance_df <- tryCatch(as.data.frame(broom::glance(x)), error = function(e) NULL)
  }

  if (is.null(tidy_df) && requireNamespace("broom.mixed", quietly = TRUE)) {
    tidy_df <- tryCatch(
      as.data.frame(broom.mixed::tidy(x, conf.int = TRUE)),
      error = function(e) tryCatch(as.data.frame(broom.mixed::tidy(x)), error = function(e2) NULL)
    )
    if (is.null(glance_df)) {
      glance_df <- tryCatch(as.data.frame(broom.mixed::glance(x)), error = function(e) NULL)
    }
  }

  if (!is.null(tidy_df) && nrow(tidy_df) > 0) {
    num_cols <- vapply(tidy_df, is.numeric, logical(1))
    tidy_df[num_cols] <- lapply(tidy_df[num_cols], round, 4)
    lines <- c(lines, "#> Coefficients (tidy):", paste0("#>   ", utils::capture.output(print(tidy_df, row.names = FALSE))))
  } else {
    coefs <- tryCatch(stats::coef(summary(x)), error = function(e) NULL)
    if (!is.null(coefs) && (is.matrix(coefs) || is.data.frame(coefs))) {
      if (is.numeric(coefs)) coefs <- round(coefs, 4)
      lines <- c(lines, "#> Coefficients:", paste0("#>   ", utils::capture.output(print(coefs))))
    }
  }

  # Include random-effects variance components for mixed models (`lme4::merMod`)
  if (inherits(x, "merMod")) {
    vc_out <- tryCatch(utils::capture.output(print(summary(x)$varcor)), error = function(e) character(0))
    if (length(vc_out) > 0) {
      lines <- c(lines, "#> Random Effects (VarCorr):", paste0("#>   ", vc_out))
    }
  }

  if (!is.null(glance_df) && nrow(glance_df) > 0) {
    num_cols <- vapply(glance_df, is.numeric, logical(1))
    glance_df[num_cols] <- lapply(glance_df[num_cols], round, 4)
    lines <- c(lines, "#> Fit statistics (broom::glance):", paste0("#>   ", utils::capture.output(print(glance_df, row.names = FALSE))))
  }

  lines
}

#' @export
describe_object.lm <- function(x, name = "model") {
  describe_model_object(x, name = name)
}

#' @export
describe_object.fixest <- function(x, name = "model") {
  describe_model_object(x, name = name)
}

#' @export
describe_object.merMod <- function(x, name = "model") {
  describe_model_object(x, name = name)
}

#' @export
describe_object.coxph <- function(x, name = "model") {
  describe_model_object(x, name = name)
}

#' @export
describe_object.lavaan <- function(x, name = "model") {
  describe_model_object(x, name = name)
}

#' @export
describe_object.htest <- function(x, name = "htest") {
  method_str <- x$method %||% "Hypothesis Test"
  data_str <- x$data.name %||% "<unknown>"
  lines <- sprintf("# %s: <htest> %s (data: %s)", name, method_str, data_str)

  if (requireNamespace("broom", quietly = TRUE)) {
    tidy_df <- tryCatch(as.data.frame(broom::tidy(x)), error = function(e) NULL)
    if (!is.null(tidy_df) && nrow(tidy_df) > 0) {
      num_cols <- vapply(tidy_df, is.numeric, logical(1))
      tidy_df[num_cols] <- lapply(tidy_df[num_cols], round, 4)
      lines <- c(lines, "#> Test results (broom::tidy):", paste0("#>   ", utils::capture.output(print(tidy_df, row.names = FALSE))))
      return(lines)
    }
  }

  out <- utils::capture.output(print(x))
  c(lines, paste0("#> ", out[nzchar(trimws(out))]))
}

#' @export
describe_object.ggplot <- function(x, name = "plot") {
  data_info <- if (!is.null(x$data) && is.data.frame(x$data)) {
    sprintf("data.frame [%d x %d] (%s)", nrow(x$data), ncol(x$data), paste(utils::head(names(x$data), 8), collapse = ", "))
  } else {
    "<inherited/none>"
  }

  mapping_info <- tryCatch(
    paste(names(x$mapping), vapply(x$mapping, rlang::as_label, character(1)), sep = " = ", collapse = ", "),
    error = function(e) "<none>"
  )

  layers_info <- tryCatch(
    vapply(x$layers, function(l) class(l$geom)[1], character(1)),
    error = function(e) character(0)
  )

  c(
    sprintf("# %s: <ggplot2 object>", name),
    sprintf("#> Data: %s", data_info),
    sprintf("#> Aesthetics: aes(%s)", mapping_info),
    sprintf("#> Geoms/Layers: %s", if (length(layers_info) > 0) paste(layers_info, collapse = " + ") else "<none>")
  )
}

#' @export
describe_object.function <- function(x, name = "fn") {
  fn_lines <- deparse(x)
  if (length(fn_lines) <= 30) {
    c(sprintf("# %s: <function>", name), paste0("#> ", fn_lines))
  } else {
    sig <- deparse(args(x))[1]
    c(sprintf("# %s: <function (%d lines)>", name, length(fn_lines)), paste0("#> Signature: ", sig))
  }
}

#' @export
describe_object.default <- function(x, name = "object") {
  # If x is a statistical model object that has a formula or broom::tidy method, summarize it as a model
  if (!is.list(x) || !identical(class(x), "list")) {
    has_fml <- !is.null(tryCatch(stats::formula(x), error = function(e) NULL))
    if (has_fml && requireNamespace("broom", quietly = TRUE)) {
      tidy_try <- tryCatch(broom::tidy(x), error = function(e) NULL)
      if (!is.null(tidy_try) && is.data.frame(tidy_try) && nrow(tidy_try) > 0) {
        return(describe_model_object(x, name = name))
      }
    }
  }

  obj_size <- tryCatch(as.numeric(utils::object.size(x)), error = function(e) 0)
  cls <- paste(class(x), collapse = "/")

  str_out <- utils::capture.output(
    utils::str(
      x,
      max.level = 2,
      list.len = 10,
      vec.len = 5,
      give.attr = FALSE,
      strict.width = "cut",
      width = 90
    )
  )

  if (length(str_out) > 25) {
    str_out <- c(str_out[1:25], sprintf("... [truncated %d additional lines of str()]", length(str_out) - 25))
  }

  c(
    sprintf("# %s: <%s> (%s bytes)", name, cls, format(obj_size, big.mark = ",")),
    paste0("#> ", gsub("\t.*$", "", str_out))
  )
}
