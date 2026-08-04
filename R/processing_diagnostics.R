.process_validate_options <- function(labels, diagnostics) {
  labels <- match.arg(labels, c("factor", "character", "none"))
  .datasus_assert_flag(diagnostics, "diagnostics")
  list(labels = labels, diagnostics = diagnostics)
}

# A mutable collector avoids copying diagnostic results through every
# intermediate tibble and is only allocated when the user requests a report.
.process_diagnostic_collector <- function(
  enabled,
  information_system,
  input
) {
  if (!enabled) {
    return(NULL)
  }
  collector <- new.env(parent = emptyenv())
  collector$information_system <- information_system
  collector$input_fields <- names(input)
  collector$input_rows <- nrow(input)
  collector$mapped_fields <- character()
  collector$unmapped_fields <- character()
  collector$unknown <- list()
  collector$coercion_failures <- list()
  collector$dictionaries <- list()
  collector$reference_tables <- list()
  collector$expected_fields <- character()
  collector
}

.process_record_dictionary <- function(collector, dictionary) {
  if (is.null(collector)) return(invisible(NULL))
  fallback <- function(value, default) {
    if (is.null(value) || !length(value)) default else value[[1L]]
  }
  key <- fallback(dictionary$information_system, collector$information_system)
  collector$dictionaries[[key]] <- data.frame(
    information_system = key,
    definition = fallback(dictionary$definition, NA_character_),
    archive_checksum = fallback(dictionary$archive_checksum, NA_character_),
    archive_checksum_algorithm = fallback(dictionary$archive_checksum_algorithm, "md5"),
    archive_path = fallback(dictionary$archive, NA_character_),
    source = fallback(dictionary$source, NA_character_),
    stringsAsFactors = FALSE
  )
  definitions <- dictionary$definitions
  declared <- if (is.null(definitions) || !"field" %in% names(definitions)) {
    character()
  } else {
    definitions$field
  }
  collector$expected_fields <- unique(c(
    collector$expected_fields, declared, dictionary$numeric_fields
  ))
  invisible(NULL)
}

.process_record_coercion <- function(collector, field, target, source, result,
                                     missing = character()) {
  if (is.null(collector)) return(invisible(NULL))
  source <- trimws(as.character(source))
  meaningful <- !is.na(source) & nzchar(source) & !source %in% missing
  invalid <- meaningful & is.na(result)
  if (any(invalid)) {
    counts <- sort(table(source[invalid]), decreasing = TRUE)
    collector$coercion_failures[[length(collector$coercion_failures) + 1L]] <-
      data.frame(
        field = field, target = target, value = names(counts),
        n = as.integer(counts), stringsAsFactors = FALSE
      )
  }
  invisible(NULL)
}

.process_record_unmapped_field <- function(collector, field) {
  if (!is.null(collector)) {
    collector$unmapped_fields <- unique(c(collector$unmapped_fields, field))
  }
  invisible(NULL)
}

.process_record_dictionary_diagnostics <- function(
  collector,
  field,
  dictionary,
  source,
  selected
) {
  if (is.null(collector)) {
    return(invisible(NULL))
  }
  collector$mapped_fields <- unique(c(collector$mapped_fields, field))
  source <- trimws(as.character(source))
  lookup <- source
  conversion <- selected$conversion
  if (identical(conversion$type, "cnv")) {
    definition <- selected$definition
    lookup <- substring(
      lookup,
      definition$position,
      definition$position + conversion$code_width - 1L
    )
    lookup <- .tabwin_normalize_code(lookup, conversion$code_width)
  }
  # Membership in the official map is more precise than comparing labels with
  # source text, because a legitimate label can equal its source code.
  unknown <- !is.na(source) & nzchar(source) &
    is.na(.tabwin_conversion_labels(lookup, conversion))
  if (any(unknown)) {
    counts <- sort(table(source[unknown]), decreasing = TRUE)
    collector$unknown[[length(collector$unknown) + 1L]] <- data.frame(
      field = field,
      dictionary = dictionary,
      code = names(counts),
      n = as.integer(counts),
      stringsAsFactors = FALSE
    )
  }
  invisible(NULL)
}

.process_finalize <- function(data, collector = NULL) {
  result <- tibble::as_tibble(data)
  if (is.null(collector)) {
    return(result)
  }
  unknown <- if (length(collector$unknown)) {
    do.call(rbind, collector$unknown)
  } else {
    data.frame(
      field = character(),
      dictionary = character(),
      code = character(),
      n = integer(),
      stringsAsFactors = FALSE
    )
  }
  coercion_failures <- if (length(collector$coercion_failures)) {
    tibble::as_tibble(do.call(rbind, collector$coercion_failures))
  } else {
    tibble::tibble(field = character(), target = character(),
                   value = character(), n = integer())
  }
  dictionaries <- if (length(collector$dictionaries)) {
    tibble::as_tibble(do.call(rbind, collector$dictionaries))
  } else {
    tibble::tibble(
      information_system = character(), definition = character(),
      archive_checksum = character(), archive_checksum_algorithm = character(),
      archive_path = character(),
      source = character()
    )
  }
  report <- structure(
    list(
      information_system = collector$information_system,
      package_version = as.character(utils::packageVersion("microdatasus")),
      input_rows = collector$input_rows,
      output_rows = nrow(result),
      input_fields = collector$input_fields,
      output_fields = names(result),
      added_fields = setdiff(names(result), collector$input_fields),
      mapped_fields = unique(collector$mapped_fields),
      unmapped_fields = collector$unmapped_fields,
      expected_fields = collector$expected_fields,
      missing_expected_fields = setdiff(
        collector$expected_fields, toupper(collector$input_fields)
      ),
      dictionaries = dictionaries,
      reference_tables = if (length(collector$reference_tables)) {
        tibble::as_tibble(do.call(rbind, collector$reference_tables))
      } else {
        .datasus_reference_table_metadata()
      },
      coercion_failures = coercion_failures,
      unknown_codes = tibble::as_tibble(unknown)
    ),
    class = "microdatasus_processing_diagnostics"
  )
  attr(result, "microdatasus_diagnostics") <- report
  result
}

#' Extract processing diagnostics
#'
#' Returns the optional report attached by a processing function called with
#' `diagnostics = TRUE`. The report includes input/output fields, dictionary
#' provenance and checksum, expected and unmapped fields, unknown codes, and
#' failed numeric or date coercions.
#'
#' @param x An object returned by a microdatasus processing function.
#'
#' @return A `microdatasus_processing_diagnostics` list, or `NULL` when
#'   diagnostics were not requested.
#'
#' @examplesIf interactive() && curl::has_internet()
#' processed <- process_sim(
#'   sim_do_sample,
#'   municipality_data = FALSE,
#'   labels = "none",
#'   diagnostics = TRUE
#' )
#' processing_diagnostics(processed)
#'
#' @export
processing_diagnostics <- function(x) {
  attr(x, "microdatasus_diagnostics", exact = TRUE)
}
