# Classify the origin independently from severity. This keeps established
# status values backward compatible while making upstream drift auditable.
.datasus_dictionary_issue_class <- function(error) {
  if (inherits(error, "microdatasus_dictionary_missing_error")) {
    return("upstream_archive_missing")
  }
  if (inherits(error, "microdatasus_dictionary_ambiguous_error")) {
    return("archive_ambiguity")
  }
  if (inherits(error, "microdatasus_dictionary_invalid_error")) {
    return("upstream_content_invalid")
  }
  if (inherits(error, "microdatasus_dictionary_relation_error")) {
    return("relation_io_or_parser")
  }
  "internal_or_unknown"
}

# Preserve relation failures as data so one malformed optional file does not
# hide either the remaining metadata or the reason that labels are absent.
.datasus_dictionary_conversion <- function(dictionary, definition) {
  tryCatch({
    conversion <- .tabwin_read_conversion(dictionary, definition)
    symbolic <- !is.null(conversion$ranges) && nrow(conversion$ranges) > 0L
    definition_fallback <- isTRUE(conversion$fallback_label)
    recovered_label <- isTRUE(conversion$recovered_label)
    conflicting_keys <- if (is.null(conversion$conflicting_key_count)) {
      0L
    } else {
      conversion$conflicting_key_count
    }
    duplicate_fallback <- conflicting_keys > 0L
    count_mismatch <- isTRUE(conversion$category_count_mismatch)
    fallback <- definition_fallback || duplicate_fallback || count_mismatch
    list(
      conversion = conversion,
      status = if (fallback) {
        "fallback"
      } else if (symbolic) {
        "non_enumerable"
      } else {
        "ok"
      },
      issue_class = if (duplicate_fallback) {
        "upstream_duplicate_keys"
      } else if (count_mismatch) {
        "upstream_category_count_mismatch"
      } else if (definition_fallback) {
        "definition_drift"
      } else if (symbolic) {
        "analytical_range"
      } else {
        NA_character_
      },
      message = if (duplicate_fallback) {
        paste0(
          "The related DBF has ", conflicting_keys,
          " key(s) with conflicting labels; the last physical record was ",
          "used following TabWin precedence."
        )
      } else if (count_mismatch) {
        paste0(
          "The CNV declares ", conversion$category_count,
          if (conversion$category_count == 1L) " category" else " categories",
          " but defines ", conversion$observed_category_count,
          if (conversion$observed_category_count == 1L) {
            " category"
          } else {
            " categories"
          },
          "; all physical categories were retained."
        )
      } else if (definition_fallback) {
        if (recovered_label) {
          paste0(
            "The DEF repeats key field ", conversion$requested_label_field,
            " as its label; audited official DBF field ",
            conversion$label_field, " was used."
          )
        } else {
          paste0(
            "The DEF requests label field ",
            conversion$requested_label_field,
            "; the only non-key DBF field ", conversion$label_field,
            " was used."
          )
        }
      } else if (symbolic) {
        "One or more analytical ranges are represented symbolically."
      } else {
        NA_character_
      }
    )
  }, error = function(error) {
    message <- conditionMessage(error)
    status <- if (inherits(
      error, "microdatasus_dictionary_missing_error"
    )) {
      "missing"
    } else if (inherits(error, c(
      "microdatasus_dictionary_invalid_error",
      "microdatasus_dictionary_ambiguous_error"
    ))) {
      "invalid"
    } else {
      "error"
    }
    list(
      conversion = NULL, status = status, message = message,
      issue_class = .datasus_dictionary_issue_class(error)
    )
  })
}

# Extract all usable relations with one pass over the ZIP. Individual parsing
# remains fault tolerant because official DEF files can reference retired CNVs.
.datasus_prefetch_dictionary_relations <- function(dictionary, definitions) {
  if (isTRUE(dictionary$extracted_all) || !nrow(definitions)) {
    return(invisible(NULL))
  }
  files <- unique(definitions$file)
  entries <- vapply(files, function(file) {
    definition_dir <- dictionary$definition_dir
    relative_file <- if (definition_dir %in% c("", ".", "/")) {
      file
    } else {
      paste0(definition_dir, "/", file)
    }
    tryCatch(
      .tabwin_find_entry(dictionary$entries, relative_file),
      error = function(error) NA_character_
    )
  }, character(1))
  entries <- unique(entries[!is.na(entries)])
  destinations <- file.path(dictionary$cache_dir, basename(entries))
  pending <- !file.exists(destinations) | file.size(destinations) == 0
  if (!any(pending)) {
    return(invisible(NULL))
  }
  # If batch extraction fails, the existing lazy extractor gets another chance
  # for each relation and preserves its more specific diagnostic messages.
  tryCatch(
    zip::unzip(
      zipfile = dictionary$archive,
      files = entries[pending],
      exdir = dictionary$cache_dir,
      junkpaths = TRUE,
      overwrite = TRUE
    ),
    error = function(error) NULL
  )
  invisible(NULL)
}

.datasus_validate_fields <- function(fields) {
  if (is.null(fields)) return(NULL)
  if (!is.character(fields) || anyNA(fields) || any(!nzchar(fields))) {
    cli::cli_abort("{.arg fields} must be NULL or a character vector of field names.")
  }
  unique(toupper(fields))
}

.datasus_dictionary_field_view <- function(result) {
  groups <- split(result, result$field)
  status_rank <- c(ok = 1L, fallback = 2L, not_requested = 3L, non_enumerable = 4L,
                   missing = 5L, invalid = 6L, error = 7L)
  tibble::tibble(
    information_system = vapply(groups, function(x) x$information_system[[1L]],
                                character(1)),
    field = names(groups),
    type = vapply(groups, function(x) x$type[[1L]], character(1)),
    definitions_count = vapply(groups, nrow, integer(1)),
    status = vapply(groups, function(x) {
      ranks <- unname(status_rank[x$status])
      x$status[[which.max(replace(ranks, is.na(ranks), 0L))]]
    }, character(1)),
    definitions = unname(groups)
  )
}

#' Consult variables in an official DataSUS dictionary
#'
#' Downloads or reuses a TabWin dictionary and presents its variable metadata
#' and code-label maps as a rectangular lookup table.
#'
#' @param information_system A value accepted by fetch_tabwin_dictionary().
#' @param include_labels Logical scalar. If TRUE, parse CNV and DBF relations
#'   and include their code-label tables in the labels list-column.
#' @param fields Optional character vector restricting the returned fields.
#' @param view Either `"definitions"`, with one row per DEF declaration, or
#'   `"fields"`, with repeated declarations grouped in a list-column.
#' @param include_ranges Logical scalar. If TRUE, include symbolic CNV interval
#'   rules in the `ranges` list-column.
#' @inheritParams fetch_tabwin_dictionary
#'
#' @details Large analytical CNV ranges are retained as symbolic rules instead
#'   of being expanded into millions of rows. `status` distinguishes complete,
#'   fallback, non-enumerable, missing, invalid, and failed relations. A
#'   `issue_class` independently identifies upstream absence/content drift,
#'   archive ambiguity, parser/I/O errors, analytical ranges, or definition
#'   fallback, so severity is not confused with origin.
#'   Fallback is reported when an official two-column DBF renamed its sole
#'   description field while the DEF retained the previous name. DBFs with
#'   duplicate keys and conflicting labels are also explicit fallbacks: the
#'   last physical record is retained, following TabWin precedence. A CNV whose
#'   declared category count differs from its physical definitions is reported
#'   as an upstream fallback, while every physical category is retained. Parsed
#'   relations persist on disk when `cache_dir` is set, and completed result
#'   tables are reused during the R session.
#'
#' @return A tibble with one row per categorical definition or numeric field.
#'
#' @examplesIf interactive() && curl::has_internet()
#' variables <- datasus_variables("SIM-DO", include_labels = FALSE)
#' variables[, c("field", "type", "description")]
#'
#' @export
datasus_variables <- function(
  information_system,
  include_labels = TRUE,
  timeout = 240,
  refresh = FALSE,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL),
  fields = NULL,
  view = c("definitions", "fields"),
  include_ranges = TRUE
) {
  .datasus_assert_flag(include_labels, "include_labels")
  .datasus_assert_flag(include_ranges, "include_ranges")
  fields <- .datasus_validate_fields(fields)
  view <- match.arg(view)
  dictionary <- fetch_tabwin_dictionary(
    information_system = information_system,
    timeout = timeout,
    refresh = refresh,
    quiet = quiet,
    cache_dir = cache_dir
  )
  definitions <- dictionary$definitions
  if (!is.null(fields)) {
    available <- unique(c(definitions$field, dictionary$numeric_fields))
    unknown <- setdiff(fields, available)
    if (length(unknown)) {
      cli::cli_abort("Fields not declared by this dictionary: {.field {unknown}}.")
    }
    definitions <- definitions[definitions$field %in% fields, , drop = FALSE]
  }
  # Cache the fully assembled lookup table separately from parsed relations.
  table_cache_key <- paste(
    "..datasus_variables", include_labels, include_ranges,
    paste(sort(unique(fields)), collapse = ","), sep = "::"
  )
  if (!refresh && exists(
    table_cache_key, envir = dictionary$conversions, inherits = FALSE
  )) {
    result <- get(table_cache_key, envir = dictionary$conversions,
                  inherits = FALSE)
    if (identical(view, "fields")) {
      return(.datasus_dictionary_field_view(result))
    }
    return(result)
  }
  if (include_labels) {
    if (!quiet) {
      cli::cli_alert_info(
        "Preparing the DataSUS variable labels for {.val {information_system}}..."
      )
    }
    .datasus_prefetch_dictionary_relations(dictionary, definitions)
  }
  conversions <- lapply(seq_len(nrow(definitions)), function(index) {
    if (!include_labels) {
      return(list(
        conversion = NULL, status = "not_requested",
        message = NA_character_, issue_class = NA_character_
      ))
    }
    .datasus_dictionary_conversion(
      dictionary, definitions[index, , drop = FALSE]
    )
  })
  parsed <- lapply(conversions, `[[`, "conversion")
  label_tables <- lapply(parsed, function(conversion) {
    if (is.null(conversion)) {
      return(tibble::tibble(code = character(), label = character()))
    }
    tibble::tibble(code = names(conversion$map),
                   label = unname(conversion$map))
  })
  range_tables <- lapply(parsed, function(conversion) {
    if (!include_ranges || is.null(conversion) || is.null(conversion$ranges)) {
      return(tibble::tibble(
        token = character(), lower = numeric(), upper = numeric(),
        label = character(), priority = integer()
      ))
    }
    tibble::as_tibble(conversion$ranges[c(
      "token", "lower", "upper", "label", "priority"
    )])
  })
  result <- tibble::tibble(
    information_system = dictionary$information_system,
    definition = dictionary$definition,
    source = dictionary$source,
    downloaded_at = dictionary$downloaded_at,
    archive_checksum = dictionary$archive_checksum,
    archive_checksum_algorithm = if (is.null(dictionary$archive_checksum_algorithm)) "md5" else dictionary$archive_checksum_algorithm,
    field = definitions$field,
    description = definitions$description,
    type = "categorical",
    command = definitions$command,
    relation = definitions$extension,
    file = definitions$file,
    position = definitions$position,
    code_width = vapply(
      parsed,
      function(conversion) {
        if (is.null(conversion)) NA_integer_ else conversion$code_width
      },
      integer(1)
    ),
    categories = vapply(
      parsed,
      function(conversion) {
        if (is.null(conversion)) NA_integer_ else length(conversion$map)
      },
      integer(1)
    ),
    range_rules = vapply(
      parsed,
      function(conversion) {
        if (is.null(conversion) || is.null(conversion$ranges)) 0L else nrow(conversion$ranges)
      },
      integer(1)
    ),
    status = vapply(conversions, `[[`, character(1), "status"),
    message = vapply(conversions, function(value) {
      if (is.null(value$message)) NA_character_ else value$message
    }, character(1)),
    issue_class = vapply(conversions, function(value) {
      if (is.null(value$issue_class)) NA_character_ else value$issue_class
    }, character(1)),
    labels_complete = vapply(
      conversions, function(value) value$status %in% c("ok", "fallback"),
      logical(1)
    ),
    labels = label_tables,
    ranges = range_tables
  )
  numeric <- setdiff(dictionary$numeric_fields, definitions$field)
  if (!is.null(fields)) numeric <- intersect(numeric, fields)
  if (length(numeric)) {
    empty_labels <- rep(
      list(tibble::tibble(code = character(), label = character())),
      length(numeric)
    )
    numeric_rows <- tibble::tibble(
      information_system = dictionary$information_system,
      definition = dictionary$definition,
      source = dictionary$source,
      downloaded_at = dictionary$downloaded_at,
      archive_checksum = dictionary$archive_checksum,
      archive_checksum_algorithm = if (is.null(dictionary$archive_checksum_algorithm)) "md5" else dictionary$archive_checksum_algorithm,
      field = numeric,
      description = NA_character_,
      type = "numeric",
      command = "I",
      relation = NA_character_,
      file = NA_character_,
      position = NA_integer_,
      code_width = NA_integer_,
      categories = NA_integer_,
      range_rules = 0L,
      status = "ok",
      message = NA_character_,
      issue_class = NA_character_,
      labels_complete = TRUE,
      labels = empty_labels,
      ranges = rep(list(tibble::tibble(
        token = character(), lower = numeric(), upper = numeric(),
        label = character(), priority = integer()
      )), length(numeric))
    )
    result <- rbind(result, numeric_rows)
  }
  if (include_labels && !quiet) {
    cli::cli_alert_success(
      "Prepared the DataSUS variable labels for {.val {information_system}}."
    )
  }
  # The environment is shared by copies of the cached dictionary object.
  assign(table_cache_key, result, envir = dictionary$conversions)
  if (identical(view, "fields")) {
    return(.datasus_dictionary_field_view(result))
  }
  result
}
