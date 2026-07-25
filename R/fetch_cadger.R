#' Download the current CADGER table
#'
#' Downloads and reads the current CNES establishment-name table distributed by
#' DataSUS. [process_cnes()] can use this table to add establishment names.
#'
#' @param timeout A positive numeric scalar. Download and connection timeout,
#'   in seconds.
#'
#' @return A data frame with character columns `CNES` (establishment code) and
#'   `FANTASIA` (trade name).
#'
#' @section Network access:
#' This function downloads the current `TAB_CNES.zip` archive from DataSUS.
#' The temporary archive and extracted files are removed before the function
#' returns or aborts.
#'
#' @references
#' Saldanha, R. F. (2026). [CNES -- Cadastro Nacional de Estabelecimentos de
#' Saúde](https://rfsaldanha.github.io/sis/cnes.html).
#'
#' @seealso [process_cnes()], [fetch_datasus()]
#' @export
fetch_cadger <- function(timeout = 240) {
  cadger_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Auxiliar/TAB_CNES.zip"
  cadger_file <- file.path("DBF/CADGERBR.dbf")

  cli::cli_alert_info("Downloading the current CADGER table from DataSUS...")
  tmp <- .datasus_fetch_zip_dbf(cadger_url, cadger_file, timeout)
  required <- c("CNES", "FANTASIA")
  missing <- setdiff(required, names(tmp))
  if (length(missing)) {
    cli::cli_abort(
      "CADGER is missing required column{?s}: {paste(missing, collapse = ', ')}."
    )
  }
  tmp <- tmp[, required, drop = FALSE]
  tmp$CNES <- as.character(tmp$CNES)
  tmp$FANTASIA <- stringi::stri_enc_toutf8(str = tmp$FANTASIA)

  tmp
}
