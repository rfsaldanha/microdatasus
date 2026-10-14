# Raw downloads retain each physical layout independently of public source
# tracking. Fingerprints reject stale context after external table edits.
.datasus_source_fingerprints <- function(data) {
  vapply(data, digest::digest, character(1), algo = "xxhash64")
}

.datasus_bind_raw <- function(parts, information_system) {
  physical <- c("dbf_field_types", "dbf_field_widths", "dbf_field_decimals",
                "dbf_language_driver", "dbc_encoding", "dbc_column_encodings")
  sources <- lapply(parts, function(part) {
    list(rows = nrow(part), fields = names(part),
         physical = attributes(part)[intersect(physical, names(attributes(part)))],
         fingerprints = .datasus_source_fingerprints(part))
  })
  result <- tibble::as_tibble(data.table::rbindlist(parts, use.names = TRUE, fill = TRUE))
  attr(result, "microdatasus_sources") <- list(
    version = 1L, information_system = information_system,
    fields = names(result), sources = sources
  )
  result
}

.datasus_bind_processed <- function(parts) {
  # Table-level execution attributes belong to the combined result, never to
  # whichever part happened to come first. Preserve every column attribute.
  parts <- lapply(parts, function(part) {
    attributes(part) <- list(names = names(part),
                            row.names = .set_row_names(nrow(part)), class = "data.frame")
    part
  })
  tibble::as_tibble(dplyr::bind_rows(parts))
}

.process_source_context <- function(data, information_system, process_args) {
  context <- attr(data, "microdatasus_sources", exact = TRUE)
  if (is.null(context)) return(NULL)
  invalid <- function() cli::cli_abort(c(
    "The downloaded table's source context no longer matches its data.",
    "i" = "Process the original download before editing or combining its rows or columns."
  ), class = "microdatasus_source_context_error")
  if (!identical(context$version, 1L) ||
      !identical(context$information_system, information_system) ||
      !identical(context$fields, names(data)) || !length(context$sources)) invalid()
  counts <- vapply(context$sources, function(source) source$rows, integer(1))
  if (anyNA(counts) || any(counts < 0L) || sum(counts) != nrow(data)) invalid()
  offset <- 0L
  parts <- reports <- vector("list", length(context$sources))
  for (i in seq_along(context$sources)) {
    source <- context$sources[[i]]
    if (!all(source$fields %in% names(data))) invalid()
    rows <- offset + seq_len(counts[[i]])
    part <- data[rows, source$fields, drop = FALSE]
    attr(part, "microdatasus_sources") <- NULL
    if (!identical(.datasus_source_fingerprints(part), source$fingerprints)) invalid()
    for (attribute in names(source$physical)) {
      attr(part, attribute) <- source$physical[[attribute]]
    }
    parts[[i]] <- .datasus_process_file(part, information_system, process_args)
    reports[[i]] <- processing_diagnostics(parts[[i]])
    offset <- offset + counts[[i]]
  }
  result <- .datasus_bind_processed(parts)
  for (attribute in c("microdatasus_provenance", "microdatasus_request")) {
    attr(result, attribute) <- attr(data, attribute, exact = TRUE)
  }
  if (isTRUE(process_args$diagnostics)) {
    collector <- .process_diagnostic_collector(TRUE, information_system, data)
    for (report in reports) {
      for (field in c("mapped_fields", "unmapped_fields", "expected_fields")) {
        collector[[field]] <- unique(c(collector[[field]], report[[field]]))
      }
      collector$unknown <- c(collector$unknown, list(report$unknown_codes))
      collector$coercion_failures <- c(collector$coercion_failures, list(report$coercion_failures))
      collector$dictionaries <- c(collector$dictionaries, list(report$dictionaries))
      collector$reference_tables <- c(collector$reference_tables, list(report$reference_tables))
    }
    result <- .process_finalize(result, collector)
    report <- processing_diagnostics(result)
    report$dictionaries <- unique(report$dictionaries)
    report$reference_tables <- unique(report$reference_tables)
    for (field in c("unknown_codes", "coercion_failures")) {
      table <- report[[field]]
      if (nrow(table)) {
        keys <- setdiff(names(table), "n")
        grouped <- dplyr::group_by(table, dplyr::across(dplyr::all_of(keys)))
        report[[field]] <- dplyr::summarise(
          grouped, dplyr::across(dplyr::all_of("n"), sum), .groups = "drop"
        )
      }
    }
    attr(result, "microdatasus_diagnostics") <- report
  }
  result
}
