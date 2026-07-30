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
  collector$unknown <- list()
  collector
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
    is.na(unname(conversion$map[lookup]))
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
  report <- structure(
    list(
      information_system = collector$information_system,
      input_rows = collector$input_rows,
      output_rows = nrow(result),
      input_fields = collector$input_fields,
      output_fields = names(result),
      added_fields = setdiff(names(result), collector$input_fields),
      mapped_fields = unique(collector$mapped_fields),
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
#' diagnostics = TRUE.
#'
#' @param x An object returned by a microdatasus processing function.
#'
#' @return A microdatasus_processing_diagnostics list, or NULL when diagnostics
#' were not requested.
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
