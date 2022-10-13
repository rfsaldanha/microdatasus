#' Fetch and read microdata files from DataSUS
#'
#' \code{fetch_datasus} downloads microdata (DBC) files from DataSUS and reads them.
#'
#' This function downloads DBC files from DataSUS following parameters about start date, end date, UF and health information system abbreviation. After the download process, the files are merged into a unique \code{data.frame} object.
#'
#' A specific UF or a vector of UFs can be informed using the following abbreviations: "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO".
#'
#' The following systems are implemented: "SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER", "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT", "SINASC", "CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR", "CNES-HB", "CNES-PF", "CNES-EP", "CNES-RC", "CNES-IN", "CNES-EE", "CNES-EF", "CNES-GM", "SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD", "SIA-AN", "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", "SIA-PS", "SIA-SAD", "SINAN-DENGUE-FINAL", "SINAN-DENGUE-PRELIMINAR", "SINAN-CHIKUNGUNYA-FINAL", "SINAN-CHIKUNGUNYA-PRELIMINAR", "SINAN-ZIKA-FINAL", "SINAN-ZIKA-PRELIMINAR", "SINAN-MALARIA-FINAL", "SINAN-MALARIA-PRELIMINAR".
#'
#' @param year_start,year_end numeric. Start and end year of files in the format yyyy.
#' @param month_start,month_end numeric. Start and end month in the format mm. Those parameters are only used with the healh information systems SIH, CNES and SIA. There parameter are ignored if the information health system is SIM or SINASC.
#' @param uf an optional string or a vector of strings. By default all UFs ("Unidades Federativas") are download. See \emph{Details}.
#' @param information_system string. The abbreviation of the health information system to be accessed. See \emph{Details}.
#' @param vars an optional string or a vector of strings. By default, all variables read and stored, unless a list of desired variables is informed by this parameter.
#' @param stop_on_error logical. If TRUE, the download process will be stopped if an error occurs.
#'
#' @section Warning:
#' A Internet connection is needed to use this function.
#'
#' The year and month used to download the files regards the processing month and year of the cases by DataSUS.
#'
#' The UF regards where the cases were processed by DataSUS.
#'
#' The files are downloaded to a temporary folder and deleted after the reading process.
#'
#' @examples \dontrun{
#' fetch_datasus(year_start = 2010, year_end = 2011,
#'               information_system = "SIM-DO")
#'
#' fetch_datasus(year_start = 2014, year_end = 2014,
#'               information_system = "SIM-DO",
#'               vars = c("CODMUNRES", "DTOBITO", "CAUSABAS"))
#'
#' fetch_datasus(year_start = 2014, month_start = 1,
#'               year_end = 2014, month_end = 2,
#'               uf = c("RJ", "MG", "SP", "ES"),
#'               information_system = "SIH-RD")
#' }
#' @export

fetch_datasus <- function(year_start, month_start, year_end, month_end, uf = "all", information_system, vars = NULL, stop_on_error = FALSE){
  # Verify health information system
  sisSIH <- c("SIH-RD","SIH-RJ","SIH-SP","SIH-ER")
  sisSIM <- c("SIM-DO", "SIM-DOFET","SIM-DOEXT","SIM-DOINF","SIM-DOMAT")
  sisSINASC <- c("SINASC")
  sisCNES <- c("CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR", "CNES-HB","CNES-PF","CNES-EP","CNES-RC","CNES-IN","CNES-EE","CNES-EF","CNES-GM")
  sisSIA <- c("SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD", "SIA-AN", "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", "SIA-PS", "SIA-SAD")
  sisSINAN <- c("SINAN-DENGUE-FINAL", "SINAN-DENGUE-PRELIMINAR", "SINAN-CHIKUNGUNYA-FINAL", "SINAN-CHIKUNGUNYA-PRELIMINAR", "SINAN-ZIKA-FINAL", "SINAN-ZIKA-PRELIMINAR", "SINAN-MALARIA-FINAL", "SINAN-MALARIA-PRELIMINAR")
  available_information_system <- c(sisSIH, sisSIM, sisSINASC, sisCNES, sisSIA, sisSINAN)
  if(!(information_system %in% available_information_system)) stop("Health informaton system unknown.")

  # Create dates for verification
  if(substr(information_system,1,3) == "SIH" | substr(information_system,1,4) == "CNES" | substr(information_system,1,3) == "SIA"){
    date_start <- as.Date(paste0(year_start,"-",formatC(month_start, width = 2, format = "d", flag = "0"),"-","01"))
    date_end <- as.Date(paste0(year_end,"-",formatC(month_end, width = 2, format = "d", flag = "0"),"-","01"))
  } else if(substr(information_system,1,3) == "SIM" | information_system == "SINASC" | information_system == "SINAN-DENGUE-FINAL" | information_system == "SINAN-DENGUE-PRELIMINAR" | information_system == "SINAN-CHIKUNGUNYA-FINAL" | information_system == "SINAN-CHIKUNGUNYA-PRELIMINAR" | information_system == "SINAN-ZIKA-FINAL" | information_system == "SINAN-ZIKA-PRELIMINAR" | information_system == "SINAN-MALARIA-FINAL" | information_system == "SINAN-MALARIA-PRELIMINAR"){
    date_start <- as.Date(paste0(year_start,"-01-01"))
    date_end <- as.Date(paste0(year_end,"-01-01"))
  }

  # Check dates
  if(date_start > date_end) stop("Start date must be greather than end date.")

  # Create sequence of dates
  if(substr(information_system,1,3) == "SIH" | substr(information_system,1,4) == "CNES" | substr(information_system,1,3) == "SIA"){
    dates <- seq(date_start, date_end, by = "month")
    dates <- paste0(substr(lubridate::year(dates),3,4),formatC(lubridate::month(dates), width = 2, format = "d", flag = "0"))
  } else if(substr(information_system,1,3) == "SIM" | information_system == "SINASC"){
    dates <- seq(date_start, date_end, by = "year")
    dates <- lubridate::year(dates)
  } else if(information_system == "SINAN-DENGUE-FINAL" | information_system == "SINAN-DENGUE-PRELIMINAR" | information_system == "SINAN-CHIKUNGUNYA-FINAL" | information_system == "SINAN-CHIKUNGUNYA-PRELIMINAR" | information_system == "SINAN-ZIKA-FINAL" | information_system == "SINAN-ZIKA-PRELIMINAR" | information_system == "SINAN-MALARIA-FINAL" | information_system == "SINAN-MALARIA-PRELIMINAR"){
    dates <- seq(date_start, date_end, by = "year")
    dates <- lubridate::year(dates)
    dates <- substr(dates, 3, 4)
  }


  # Check UF
  ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
  if(!all((uf %in% c("all",ufs)))) stop("UF unknown.")

  # Check UF for SINAN files
  if(information_system %in% sisSINAN & uf != "all"){
    message("SINAN files are not available per UF. Ignoring argument 'uf' and downloading data.")
  }

  # Create download sequence by system, UF and date
  if(information_system %in% sisSIM[2:length(sisSIM)]){
    file_extension <- as.vector(paste0(substr(dates, 3,4),".dbc"))
  } else if (information_system %in% sisSINAN){
    file_extension <- as.vector(sapply("BR", paste0, dates,".dbc"))
  } else if (all(uf == "all")) {
    file_extension <- as.vector(sapply(ufs, paste0, dates,".dbc"))
  } else if(information_system %in% sisSIA & "SP" %in% uf){
    uf_sia <- uf
    file_extension <- as.vector(sapply(uf_sia, paste0, dates,".dbc"))

    file_extension_sp_a <- as.vector(sapply("SP", paste0, dates,"a.dbc"))
    file_extension_sp_b <- as.vector(sapply("SP", paste0, dates,"b.dbc"))
    file_extension_sp_c <- as.vector(sapply("SP", paste0, dates,"c.dbc"))
    file_extension_sp_d <- as.vector(sapply("SP", paste0, dates,"d.dbc"))
    file_extension_sp_e <- as.vector(sapply("SP", paste0, dates,"e.dbc"))

    file_extension <- c(file_extension, file_extension_sp_a, file_extension_sp_b, file_extension_sp_c, file_extension_sp_d, file_extension_sp_e)
  } else {
    file_extension <- as.vector(sapply(uf, paste0, dates,".dbc"))
  }

  # Create files list for download
  if(information_system == "SIM-DO") {
    # Available dates
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DORES/"
    avail_geral <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = geral_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), start = 5, stop = 8))
    avail_prelim <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = prelim_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), start = 5, stop = 8))

    # Check if required dates are available
    if(!all(dates %in% c(avail_geral, avail_prelim))){
      message(paste0("The following dates are not availabe at DataSUS: ", paste0(dates[!dates %in% c(avail_geral, avail_prelim)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_prelim)){
      message(paste0("The following dates are preliminar: ", paste0(valid_dates[valid_dates %in% avail_prelim], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_geral,
      yes = paste0(geral_url,"DO", file_extension),
      no = paste0(prelim_url,"DO", file_extension)
    )
  } else if (information_system == "SIM-DOFET") {
    # Available dates
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DOFET/"

    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOFET", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", tmp))))

    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOFET", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", tmp))))

    # Check if required dates are available
    if(!all(dates %in% c(avail_geral, avail_prelim))){
      message(paste0("The following dates are not availabe at DataSUS: ", paste0(dates[!dates %in% c(avail_geral, avail_prelim)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_prelim)){
      message(paste0("The following dates are preliminar: ", paste0(valid_dates[valid_dates %in% avail_prelim], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_geral,
      yes = paste0(geral_url,"DOFET", file_extension),
      no = paste0(prelim_url,"DOFET", file_extension)
    )
  } else if (information_system == "SIM-DOEXT") {
    # Available dates
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DOFET/"

    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOEXT", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", tmp))))

    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOEXT", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", tmp))))

    # Check if required dates are available
    if(!all(dates %in% c(avail_geral, avail_prelim))){
      message(paste0("The following dates are not availabe at DataSUS: ", paste0(dates[!dates %in% c(avail_geral, avail_prelim)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_prelim)){
      message(paste0("The following dates are preliminar: ", paste0(valid_dates[valid_dates %in% avail_prelim], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_geral,
      yes = paste0(geral_url,"DOEXT", file_extension),
      no = paste0(prelim_url,"DOEXT", file_extension)
    )
  } else if (information_system == "SIM-DOINF") {
    # Available dates
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DOFET/"

    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOINF", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", tmp))))

    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOINF", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", tmp))))

    # Check if required dates are available
    if(!all(dates %in% c(avail_geral, avail_prelim))){
      message(paste0("The following dates are not availabe at DataSUS: ", paste0(dates[!dates %in% c(avail_geral, avail_prelim)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_prelim)){
      message(paste0("The following dates are preliminar: ", paste0(valid_dates[valid_dates %in% avail_prelim], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_geral,
      yes = paste0(geral_url,"DOINF", file_extension),
      no = paste0(prelim_url,"DOINF", file_extension)
    )
  } else if (information_system == "SIM-DOMAT") {
    # Available dates
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DOFET/"

    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOMAT", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", tmp))))

    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOMAT", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", tmp))))

    # Check if required dates are available
    if(!all(dates %in% c(avail_geral, avail_prelim))){
      message(paste0("The following dates are not availabe at DataSUS: ", paste0(dates[!dates %in% c(avail_geral, avail_prelim)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_prelim)){
      message(paste0("The following dates are preliminar: ", paste0(valid_dates[valid_dates %in% avail_prelim], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_geral,
      yes = paste0(geral_url,"DOMAT", file_extension),
      no = paste0(prelim_url,"DOMAT", file_extension)
    )
  } else if(information_system == "SIH-RD"){
    # Available dates
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"

    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("RD", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))

    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("RD", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))

    # Check if required dates are available
    if(!all(dates %in% c(avail_atual, avail_antigo))){
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", paste0(dates[!dates %in% c(avail_atual, avail_antigo)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_antigo)){
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", paste0(valid_dates[valid_dates %in% avail_antigo], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_atual,
      yes = paste0(atual_url,"RD", file_extension),
      no = paste0(antigo_url,"RD", file_extension)
    )
  } else if(information_system == "SIH-RJ") {
    # Available dates
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"

    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("RJ", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))

    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("RJ", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))

    # Check if required dates are available
    if(!all(dates %in% c(avail_atual, avail_antigo))){
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", paste0(dates[!dates %in% c(avail_atual, avail_antigo)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_antigo)){
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", paste0(valid_dates[valid_dates %in% avail_antigo], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_atual,
      yes = paste0(atual_url,"RJ", file_extension),
      no = paste0(antigo_url,"RJ", file_extension)
    )
  } else if(information_system == "SIH-SP") {
    # Available dates
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"

    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("SP", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))

    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("SP", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))

    # Check if required dates are available
    if(!all(dates %in% c(avail_atual, avail_antigo))){
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", paste0(dates[!dates %in% c(avail_atual, avail_antigo)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_antigo)){
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", paste0(valid_dates[valid_dates %in% avail_antigo], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_atual,
      yes = paste0(atual_url,"SP", file_extension),
      no = paste0(antigo_url,"SP", file_extension)
    )
  } else if(information_system == "SIH-ER") {
    # Available dates
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"

    tmp <- unlist(strsplit(x = RCurl::getURL(url = atual_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ER", tmp)]
    avail_atual <- unique(substr(x = tmp, start = 5, stop = 8))

    tmp <- unlist(strsplit(x = RCurl::getURL(url = antigo_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("ER", tmp)]
    avail_antigo <- unique(substr(x = tmp, start = 5, stop = 8))

    # Check if required dates are available
    if(!all(dates %in% c(avail_atual, avail_antigo))){
      message(paste0("The following dates are not availabe at DataSUS (yymm): ", paste0(dates[!dates %in% c(avail_atual, avail_antigo)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_atual, avail_antigo)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_antigo)){
      message(paste0("The following dates (yymm) are from old folders and may contain incompatible codes (including old ICD codes): ", paste0(valid_dates[valid_dates %in% avail_antigo], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_atual,
      yes = paste0(atual_url,"ER", file_extension),
      no = paste0(antigo_url,"ER", file_extension)
    )
  } else if(information_system == "SINASC") {
    # Available dates
    antigo_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1994_1995/Dados/DNRES/"
    atual_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/PRELIM/DNRES/"
    avail_antigo <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = antigo_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), start = 6, stop = 9))
    avail_atual <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = atual_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), start = 5, stop = 8))
    avail_prelim <- unique(substr(x = unlist(strsplit(x = RCurl::getURL(url = prelim_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n")), start = 5, stop = 8))

    # Check if required dates are available
    if(!all(dates %in% c(avail_antigo, avail_atual, avail_prelim))){
      message(paste0("The following dates are not availabe at DataSUS: ", paste0(dates[!dates %in% c(avail_antigo, avail_atual, avail_prelim)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_antigo, avail_atual, avail_prelim)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_prelim)){
      message(paste0("The following dates are preliminar: ", paste0(valid_dates[valid_dates %in% avail_prelim], collapse = ", "), "."))
    }

    # Message about old data
    if(any(valid_dates %in% avail_antigo)){
      message(paste0("The following dates are from old folders and and may contain incompatible codes (including old ICD codes): ", paste0(valid_dates[valid_dates %in% avail_antigo], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_atual,
      yes = paste0(atual_url,"DO", file_extension),
      no = ifelse(
        test = valid_dates %in% avail_antigo,
        yes = paste0(antigo_url,"DO", file_extension),
        no = paste0(prelim_url,"DO", file_extension)
      )
    )
  } else if (information_system == "SIM-DOFET") {
    # Available dates
    geral_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    prelim_url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/DOFET/"

    tmp <- unlist(strsplit(x = RCurl::getURL(url = geral_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOFET", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_geral <- sort(as.numeric(ifelse(test = substr(tmp, 0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", tmp))))

    tmp <- unlist(strsplit(x = RCurl::getURL(url = prelim_url, ftp.use.epsv = TRUE, dirlistonly = TRUE), split = "\n"))
    tmp <- tmp[grep("DOFET", tmp)]
    tmp <- unique(substr(x = tmp, start = 6, stop = 7))
    avail_prelim <- sort(as.numeric(ifelse(test = substr(tmp, 0, 1) == "9", yes = paste0("19", tmp), no = paste0("20", tmp))))

    # Check if required dates are available
    if(!all(dates %in% c(avail_geral, avail_prelim))){
      message(paste0("The following dates are not availabe at DataSUS: ", paste0(dates[!dates %in% c(avail_geral, avail_prelim)], collapse = ", "), ". Only the available dates will be downloaded."))
    }
    valid_dates <- dates[dates %in% c(avail_geral, avail_prelim)]

    # Message about preliminary data
    if(any(valid_dates %in% avail_prelim)){
      message(paste0("The following dates are preliminar: ", paste0(valid_dates[valid_dates %in% avail_prelim], collapse = ", "), "."))
    }

    # File list
    files_list <- ifelse(
      test = valid_dates %in% avail_geral,
      yes = paste0(geral_url,"DOFET", file_extension),
      no = paste0(prelim_url,"DOFET", file_extension)
    )

    # url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/"
    # files_list <- paste0(url,"DN", file_extension)
  } else if(information_system == "CNES-LT"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/LT/"
    files_list <- paste0(url,"LT", file_extension)
  } else if(information_system == "CNES-ST"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/"
    files_list <- paste0(url,"ST", file_extension)
  } else if(information_system == "CNES-DC"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/DC/"
    files_list <- paste0(url,"DC", file_extension)
  } else if(information_system == "CNES-EQ"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EQ/"
    files_list <- paste0(url,"EQ", file_extension)
  } else if(information_system == "CNES-SR"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/SR/"
    files_list <- paste0(url,"SR", file_extension)
  } else if(information_system == "CNES-HB"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/HB/"
    files_list <- paste0(url,"HB", file_extension)
  } else if(information_system == "CNES-PF"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/"
    files_list <- paste0(url,"PF", file_extension)
  } else if(information_system == "CNES-EP"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EP/"
    files_list <- paste0(url,"EP", file_extension)
  } else if(information_system == "CNES-RC"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/RC/"
    files_list <- paste0(url,"RC", file_extension)
  } else if(information_system == "CNES-IN"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/IN/"
    files_list <- paste0(url,"IN", file_extension)
  } else if(information_system == "CNES-EE"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EE/"
    files_list <- paste0(url,"EE", file_extension)
  } else if(information_system == "CNES-EF"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EF/"
    files_list <- paste0(url,"EF", file_extension)
  } else if(information_system == "CNES-GM"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/GM/"
    files_list <- paste0(url,"GM", file_extension)
  } else if(information_system == "SIA-AB"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"AB", file_extension)
  } else if(information_system == "SIA-ABO"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"ABO", file_extension)
  } else if(information_system == "SIA-ACF"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"ACF", file_extension)
  } else if(information_system == "SIA-AD"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"AD", file_extension)
  } else if(information_system == "SIA-AN"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"AN", file_extension)
  } else if(information_system == "SIA-AM"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"AM", file_extension)
  } else if(information_system == "SIA-AQ"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"AQ", file_extension)
  } else if(information_system == "SIA-AR"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"AR", file_extension)
  } else if(information_system == "SIA-ATD"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"ATD", file_extension)
  } else if(information_system == "SIA-PA"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"PA", file_extension)
  } else if(information_system == "SIA-PS"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"PS", file_extension)
  } else if(information_system == "SIA-SAD"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    files_list <- paste0(url,"SAD", file_extension)
  } else if(information_system == "SINAN-DENGUE-FINAL"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    files_list <- paste0(url,"DENG", file_extension)
  } else if(information_system == "SINAN-DENGUE-PRELIMINAR"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
    files_list <- paste0(url,"DENG", file_extension)
  } else if(information_system == "SINAN-CHIKUNGUNYA-FINAL"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    files_list <- paste0(url,"CHIK", file_extension)
  } else if(information_system == "SINAN-CHIKUNGUNYA-PRELIMINAR"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
    files_list <- paste0(url,"CHIK", file_extension)
  } else if(information_system == "SINAN-ZIKA-FINAL"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    files_list <- paste0(url,"ZIKA", file_extension)
  } else if(information_system == "SINAN-ZIKA-PRELIMINAR"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
    files_list <- paste0(url,"ZIKA", file_extension)
  } else if(information_system == "SINAN-MALARIA-FINAL"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
    files_list <- paste0(url,"MALA", file_extension)
  } else if(information_system == "SINAN-MALARIA-PRELIMINAR"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"
    files_list <- paste0(url,"MALA", file_extension)
  }

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

  # Dowload files
  data <- NULL
  for(file in files_list){
    # Temporary file
    temp <- tempfile()

    # Empty data.frame
    partial <- data.frame()

    # Try to download file
    tryCatch({
      utils::download.file(file, temp, mode = "wb", method = "libcurl")
      partial <- read.dbc::read.dbc(temp)
      file.remove(temp)
    },
    error=function(cond) {
      message(paste("Something went wrong with this URL:", file))
      message("This can be a problem with the Internet or the file does not exist yet.")

      if(stop_on_error == TRUE){
        stop("Stopping download.")
      }
    })

    # Merge files
    if(nrow(partial) > 0){
      if(!all(vars %in% names(partial))) stop("One or more variables names are unknown. Typo?")
      if(is.null(vars)){
        data <- dplyr::bind_rows(data, partial)
      } else {
        data <- dplyr::bind_rows(data, subset(partial, select = vars))
      }
    }

  }

  # Return
  return(data)
}
