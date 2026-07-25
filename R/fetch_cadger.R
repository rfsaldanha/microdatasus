#' Fetch CADGER table
#'
#' Downloads the current CADGER establishment table from DataSUS.
#'
#' @param timeout A positive numeric scalar. Download and connection timeout,
#'   in seconds.
#'
#' @return A data frame with `CNES` and `FANTASIA` columns.
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
