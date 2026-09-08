#' Fixed municipal and special territorial-code reference
#'
#' Reconstructed from the exact `TB_MUNICIP` and `TB_UF` TXT members of the
#' official DataSUS territorial base 2023 archive. Their modification date in
#' that ZIP is 2022-05-16; it does not establish a single validity date for all
#' attributes. The snapshot version is `datasus-territorio-2023-txt-20220516`.
#'
#' This reconstruction preserves the existing table's values, classes, and row
#' order. It includes extinct, transferred, and unknown territorial codes, so
#' the row count is not the number of active municipalities. Processors use the
#' same fixed reference for every observation period when territorial enrichment
#' is enabled; they do not harmonize historical boundaries automatically.
#'
#' The `legacy-compatible-v1` transformation converts decimal commas, replaces
#' zero geographical placeholders with `NA` for unknown units and former
#' territory `200010`, and retains two historical UF-display conventions:
#' empty text for code `0`, and successor state `Pernambuco` for `200010`.
#' The latter retains status `TRANSF` and type `TERRIT`; the original UF name is
#' available in the shipped source TXT. Genuine zero values elsewhere remain.
#' [datasus_reference_tables()] reports the version and source checksum.
#'
#' @source DataSUS, [territorial base 2023](ftp://ftp.datasus.gov.br/territorio/tabelas/2023/base_territorial_2023.zip).
#' The four unmodified TXT/layout members and reconstruction notes are supplied
#' under `system.file("extdata", "territory", package = "microdatasus")`.
#'
#' @format A data frame with 5659 rows and 9 variables:
#' \describe{
#'   \item{munResCod}{Municipality IBGE code with 6 numbers}
#'   \item{munResStatus}{Status}
#'   \item{munResTipo}{Type}
#'   \item{munResNome}{Name}
#'   \item{munResUf}{UF (state)}
#'   \item{munResLat}{Latitude}
#'   \item{munResLon}{Longitude}
#'   \item{munResAlt}{Altitude}
#'   \item{munResArea}{Area}
#'
#' }
"tabMun"
