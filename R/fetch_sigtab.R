#' Fetch SIGTAB table
#'
#' Downloads the current SIGTAB procedure table from DataSUS.
#'
#' @param timeout A positive numeric scalar. Download and connection timeout,
#'   in seconds.
#'
#' @return A data frame with `COD` and `nome_proced` columns.
#' @export
fetch_sigtab <- function(timeout = 240) {
  sigtab_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Auxiliar/TAB_SIA.zip"
  sigtab_file <- file.path("DBF/TB_SIGTAW.dbf")

  cli::cli_alert_info("Downloading the current SIGTAB table from DataSUS...")
  tmp <- .datasus_fetch_zip_dbf(sigtab_url, sigtab_file, timeout)
  if (ncol(tmp) != 2L) {
    cli::cli_abort(
      "SIGTAB must contain exactly two columns; found {ncol(tmp)}."
    )
  }
  colnames(tmp) <- c("COD", "nome_proced")
  tmp$COD <- as.character(tmp$COD)
  tmp$nome_proced <- stringi::stri_enc_toutf8(tmp$nome_proced)

  tmp
}
