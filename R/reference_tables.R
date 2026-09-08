# Metadata distinguishes a verified fixed municipal snapshot from the remaining
# legacy lookup objects. Only tabMun is used as a spatial fallback.
.datasus_reference_specs <- function() {
  data.frame(
    table = c("tabMun", "tabCBO", "tabNaturalidade", "tabOcupacao",
              "sigtab", "equipe", "paisnet"),
    source = c(
      "DataSUS territorial base 2023; pinned TB_MUNICIP/TB_UF TXT members",
      rep("Legacy packaged snapshot; exact source archive and date unavailable", 6L)
    ),
    source_date = as.Date(c("2022-05-16", rep(NA_character_, 6L))),
    source_date_basis = c("ZIP member modification date; not territorial validity", rep(NA_character_, 6L)),
    source_version = c("datasus-territorio-2023-txt-20220516", rep(NA_character_, 6L)),
    transformation = c("legacy-compatible-v1", rep(NA_character_, 6L)),
    source_archive = c(
      "ftp://ftp.datasus.gov.br/territorio/tabelas/2023/base_territorial_2023.zip",
      rep(NA_character_, 6L)
    ),
    source_archive_sha256 = c(
      "798be2f62a53dd1af8e335a44a1916154f36ee3a3051a7375864a74ef47c3bc4",
      rep(NA_character_, 6L)
    ),
    role = c("municipality_fallback", rep("legacy_compatibility", 6L)),
    used_in_processing = c(TRUE, rep(FALSE, 6L)),
    stringsAsFactors = FALSE
  )
}

#' Inspect packaged reference-table provenance
#'
#' Lists packaged lookup objects and identifies whether each is still used
#' internally. The municipal reference has a pinned official source and an
#' explicit snapshot version. The other six legacy objects retain unknown
#' source dates rather than inferred provenance.
#'
#' For `tabMun`, `source_date` is the TXT members' modification date inside the
#' official ZIP, not the validity date of all territorial attributes or the
#' package's original extraction date. Processors apply this fixed snapshot;
#' they do not select territorial boundaries from observation dates.
#' The original TXT members are supplied in
#' `system.file("extdata", "territory", "tabmun-source.zip", package = "microdatasus")`.
#' See [tabMun] for the documented compatibility transformations.
#'
#' @return A tibble with source, date basis, source version and archive checksum
#'   where known, dimensions, role, and a SHA-256 checksum of each serialized table.
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
                      algo = "sha256", serializeVersion = 2),
    checksum_algorithm = "sha256",
    source_date_basis = specs$source_date_basis,
    source_version = specs$source_version,
    transformation = specs$transformation,
    source_archive = specs$source_archive,
    source_archive_sha256 = specs$source_archive_sha256
  )
}

.process_record_reference <- function(collector, table) {
  if (is.null(collector)) return(invisible(NULL))
  collector$reference_tables[[table]] <-
    .datasus_reference_table_metadata(table)
  invisible(NULL)
}
