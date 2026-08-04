# Preserve relation failures as data so one malformed optional file does not
# hide either the remaining metadata or the reason that labels are absent.
.datasus_dictionary_conversion <- function(dictionary, definition) {
  tryCatch({
    conversion <- .tabwin_read_conversion(dictionary, definition)
    symbolic <- !is.null(conversion$ranges) && nrow(conversion$ranges) > 0L
    list(
      conversion = conversion,
      status = if (symbolic) "non_enumerable" else "ok",
      message = if (symbolic) {
        "One or more analytical ranges are represented symbolically."
      } else {
        NA_character_
      }
    )
  }, error = function(error) {
    message <- conditionMessage(error)
    status <- if (grepl("missing|exactly one file matching|empty", message,
                         ignore.case = TRUE)) {
      "missing"
    } else if (grepl("invalid|no usable|no field|cannot be used", message,
                      ignore.case = TRUE)) {
      "invalid"
    } else {
      "error"
    }
    list(conversion = NULL, status = status, message = message)
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
  status_rank <- c(ok = 1L, not_requested = 2L, non_enumerable = 3L,
                   missing = 4L, invalid = 5L, error = 6L)
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
#'   non-enumerable, missing, invalid, and failed relations. Parsed relations
#'   persist on disk when `cache_dir` is set, and completed result tables are
#'   reused during the R session.
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
      return(list(conversion = NULL, status = "not_requested",
                  message = NA_character_))
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
    labels_complete = vapply(
      conversions, function(value) identical(value$status, "ok"), logical(1)
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
