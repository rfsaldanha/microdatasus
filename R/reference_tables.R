# Metadata makes legacy packaged lookups auditable instead of silently treating
# them as current official dictionaries. The objects remain exported for
# backward compatibility, but only tabMun is still used as a spatial fallback.
.datasus_reference_specs <- function() {
  data.frame(
    table = c("tabMun", "tabCBO", "tabNaturalidade", "tabOcupacao",
              "sigtab", "equipe", "paisnet"),
    source = rep(
      "Legacy packaged snapshot; exact source archive and date unavailable",
      7L
    ),
    source_date = as.Date(rep(NA_character_, 7L)),
    role = c("municipality_fallback", rep("legacy_compatibility", 6L)),
    used_in_processing = c(TRUE, rep(FALSE, 6L)),
    stringsAsFactors = FALSE
  )
}

#' Inspect packaged reference-table provenance
#'
#' Lists the legacy lookup objects retained for backward compatibility and
#' identifies whether each one is still used internally. Missing source dates
#' are reported explicitly rather than inferred.
#'
#' @return A tibble with source, dimensions, role, and a SHA-256 checksum of
#'   each serialized table.
#' @export
datasus_reference_tables <- function() {
  .datasus_reference_table_metadata(.datasus_reference_specs()$table)
}

.datasus_reference_table_metadata <- function(tables = character()) {
  specs <- .datasus_reference_specs()
  specs <- specs[match(tables, specs$table, nomatch = 0L), , drop = FALSE]
  objects <- lapply(specs$table, function(name) {
    get(name, envir = asNamespace("microdatasus"))
  })
  tibble::tibble(
    table = specs$table, source = specs$source,
    source_date = specs$source_date, role = specs$role,
    used_in_processing = specs$used_in_processing,
    rows = vapply(objects, nrow, integer(1)),
    columns = vapply(objects, ncol, integer(1)),
    checksum = vapply(objects, digest::digest, character(1),
                      algo = "sha256"),
    checksum_algorithm = "sha256"
  )
}

.process_record_reference <- function(collector, table) {
  if (is.null(collector)) return(invisible(NULL))
  collector$reference_tables[[table]] <-
    .datasus_reference_table_metadata(table)
  invisible(NULL)
}
