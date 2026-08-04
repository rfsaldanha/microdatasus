# A malformed optional relation should not hide the remaining dictionary metadata.
.datasus_dictionary_conversion <- function(dictionary, definition) {
  tryCatch(
    .tabwin_read_conversion(dictionary, definition),
    error = function(error) NULL
  )
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

#' Consult variables in an official DataSUS dictionary
#'
#' Downloads or reuses a TabWin dictionary and presents its variable metadata
#' and code-label maps as a rectangular lookup table.
#'
#' @param information_system A value accepted by fetch_tabwin_dictionary().
#' @param include_labels Logical scalar. If TRUE, parse CNV and DBF relations
#'   and include their code-label tables in the labels list-column.
#' @inheritParams fetch_tabwin_dictionary
#'
#' @details Analytical CNV ranges that are too large to enumerate are kept as
#'   variable metadata, with `NA` in `categories` and an empty `labels` table.
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
  cache_dir = getOption("microdatasus.cache_dir", NULL)
) {
  .datasus_assert_flag(include_labels, "include_labels")
  dictionary <- fetch_tabwin_dictionary(
    information_system = information_system,
    timeout = timeout,
    refresh = refresh,
    quiet = quiet,
    cache_dir = cache_dir
  )
  definitions <- dictionary$definitions
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
      return(NULL)
    }
    .datasus_dictionary_conversion(
      dictionary,
      definitions[index, , drop = FALSE]
    )
  })
  label_tables <- lapply(conversions, function(conversion) {
    if (is.null(conversion)) {
      return(tibble::tibble(code = character(), label = character()))
    }
    tibble::tibble(
      code = names(conversion$map),
      label = unname(conversion$map)
    )
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
      conversions,
      function(conversion) {
        if (is.null(conversion)) NA_integer_ else conversion$code_width
      },
      integer(1)
    ),
    categories = vapply(
      conversions,
      function(conversion) {
        if (is.null(conversion)) NA_integer_ else length(conversion$map)
      },
      integer(1)
    ),
    labels = label_tables
  )
  numeric <- setdiff(dictionary$numeric_fields, definitions$field)
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
      labels = empty_labels
    )
    result <- rbind(result, numeric_rows)
  }
  if (include_labels && !quiet) {
    cli::cli_alert_success(
      "Prepared the DataSUS variable labels for {.val {information_system}}."
    )
  }
  result
}
