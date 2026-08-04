#' Download the current CADGER table
#'
#' Downloads and reads the current CNES establishment-name table distributed by
#' DataSUS. This remains available as a standalone helper; [process_cnes()]
#' reads the same DBF through its session-cached TabWin dictionary.
#'
#' @param timeout A positive numeric scalar. Download and connection timeout,
#'   in seconds.
#' @param cache_dir Optional persistent cache root. The default uses the
#'   `microdatasus.cache_dir` option when set.
#' @param refresh Logical scalar. If `TRUE`, redownload the ZIP archive.
#' @param quiet Logical scalar. If `TRUE`, suppress progress messages.
#'
#' @return A data frame with character columns `CNES` (establishment code) and
#'   `FANTASIA` (trade name).
#'
#' @section Network access:
#' This function downloads the current `TAB_CNES.zip` archive from DataSUS.
#' Transfer progress is displayed by default. Without `cache_dir`, the archive and
#' extracted files are removed before return; persistent cache entries are validated.
#'
#' @references
#' Saldanha, R. F. (2026). [CNES -- Cadastro Nacional de Estabelecimentos de
#' Saúde](https://rfsaldanha.github.io/sis/cnes.html).
#'
#' @seealso [process_cnes()], [fetch_datasus()]
#' @export
fetch_cadger <- function(
  timeout = 240,
  cache_dir = getOption("microdatasus.cache_dir", NULL),
  refresh = FALSE,
  quiet = FALSE
) {
  cadger_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Auxiliar/TAB_CNES.zip"
  cadger_file <- file.path("DBF/CADGERBR.dbf")

  if (!quiet) {
    cli::cli_alert_info(
      "Downloading DataSUS auxiliary table {.strong CADGER}..."
    )
  }
  tmp <- .datasus_fetch_zip_dbf(
    cadger_url, cadger_file, timeout, cache_dir, refresh, quiet, "CADGER"
  )
  required <- c("CNES", "FANTASIA")
  missing <- setdiff(required, names(tmp))
  if (length(missing)) {
    cli::cli_abort(
      "The downloaded CADGER table is missing required column{?s}: {.field {missing}}."
    )
  }
  provenance <- attr(tmp, "microdatasus_provenance", exact = TRUE)
  request <- attr(tmp, "microdatasus_request", exact = TRUE)
  tmp <- tmp[, required, drop = FALSE]
  tmp$CNES <- as.character(tmp$CNES)
  tmp$FANTASIA <- stringi::stri_enc_toutf8(str = tmp$FANTASIA)
  attr(tmp, "microdatasus_provenance") <- provenance
  attr(tmp, "microdatasus_request") <- request

  if (!quiet) {
    cli::cli_alert_success(
      "Downloaded and read DataSUS auxiliary table {.strong CADGER}."
    )
  }
  tmp
}
