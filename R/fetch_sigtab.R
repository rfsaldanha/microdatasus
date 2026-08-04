#' Download the current SIGTAB table
#'
#' Downloads and reads the current SIA procedure table distributed by DataSUS.
#' This standalone table is useful when procedure metadata is needed outside
#' [process_sia()], which now reads the tables declared by each TabWin DEF.
#'
#' @param timeout A positive numeric scalar. Download and connection timeout,
#'   in seconds.
#' @param cache_dir Optional persistent cache root. The default uses the
#'   `microdatasus.cache_dir` option when set.
#' @param refresh Logical scalar. If `TRUE`, redownload the ZIP archive.
#' @param quiet Logical scalar. If `TRUE`, suppress progress messages.
#'
#' @return A data frame with character columns `COD` (procedure code) and
#'   `nome_proced` (procedure name).
#'
#' @section Network access:
#' This function downloads the current `TAB_SIA.zip` archive from DataSUS.
#' Transfer progress is displayed by default. Without `cache_dir`, the archive and
#' extracted files are removed before return; persistent cache entries are validated.
#'
#' @references
#' Saldanha, R. F. (2026). [SIA -- Sistema de Informações Ambulatoriais do
#' SUS](https://rfsaldanha.github.io/sis/sia.html).
#'
#' @seealso [process_sia()], [fetch_datasus()]
#' @export
fetch_sigtab <- function(
  timeout = 240,
  cache_dir = getOption("microdatasus.cache_dir", NULL),
  refresh = FALSE,
  quiet = FALSE
) {
  sigtab_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Auxiliar/TAB_SIA.zip"
  sigtab_file <- file.path("DBF/TB_SIGTAW.dbf")

  if (!quiet) {
    cli::cli_alert_info(
      "Downloading DataSUS auxiliary table {.strong SIGTAB}..."
    )
  }
  tmp <- .datasus_fetch_zip_dbf(
    sigtab_url, sigtab_file, timeout, cache_dir, refresh, quiet, "SIGTAB"
  )
  if (ncol(tmp) != 2L) {
    cli::cli_abort(
      "The downloaded SIGTAB table must contain exactly two columns; found {ncol(tmp)}."
    )
  }
  colnames(tmp) <- c("COD", "nome_proced")
  tmp$COD <- as.character(tmp$COD)
  tmp$nome_proced <- stringi::stri_enc_toutf8(tmp$nome_proced)

  if (!quiet) {
    cli::cli_alert_success(
      "Downloaded and read DataSUS auxiliary table {.strong SIGTAB}."
    )
  }
  tmp
}
