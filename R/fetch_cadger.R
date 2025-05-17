#' Fetch CADGER table
#'
#' This function fetchs an updated CADGER table from DataSUS
#'
#' @param timeout sets the download timeout reference. Defaults to 240
#'
#' @return a data.frame
#' @export
fetch_cadger <- function(timeout = 240){
  # Resets original timeout option on function exit
  original_time_option <- getOption("timeout")
  on.exit(options(timeout = original_time_option))

  # Set new timeout
  options(timeout = timeout)

  cadger_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Auxiliar/TAB_CNES.zip"

  # Temporary file and dir
  temp_file <- tempfile()
  temp_dir <- tempdir()

  # Check local Internet connection
  local_internet <- curl::has_internet()
  if(local_internet == TRUE){
    message("Your local Internet connection seems to be ok.")
  } else {
    stop("It appears that your local Internet connection is not working. Can you check?")
  }

  # Check DataSUS FTP server
  remote_file_is_availabe <- RCurl::url.exists("ftp.datasus.gov.br")
  if(remote_file_is_availabe == TRUE){
    message("DataSUS FTP server seems to be up. Starting download...")
  } else {
    message("It appears that DataSUS FTP is down. I will try to download the files anyway...")
  }

  # Try to download file
  tryCatch({
    utils::download.file(cadger_url, temp_file, mode = "wb", method = "libcurl")
  },
  error=function(cond) {
    message(paste("Something went wrong while trying to download CADGER table. URL:", file))
  })

  # sigtab file address
  cadger_file <- file.path("DBF/CADGERBR.dbf")

  # Unzip
  zip::unzip(zipfile = temp_file, exdir = temp_dir, overwrite = TRUE, files = cadger_file)

  # Read file
  tmp <- foreign::read.dbf(file = file.path(temp_dir, cadger_file))
  tmp <- dplyr::select(tmp, "CNES", "FANTASIA")
  tmp$FANTASIA <- stringi::stri_enc_toutf8(str = tmp$FANTASIA)

  # Return sigtab data frame
  return(tmp)
}
