#' Fetch and read microdata files from DataSUS
#' 
#' \code{fetch_datasus} downloads microdata (DBC) files from DataSUS and reads them.
#' 
#' This function downloads DBC files from DataSUS following parameters about start date, end date, UF and health information system abbreviation. After the download process, the files are merged into a unique \code{data.frame} object.
#' 
#' A specific UF or a vector of UFs can be informed using the following abbreviations: "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO".
#' 
#' The following systems are implemented: "SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER", "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT", "SINASC", "CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR", "CNES-HB", "CNES-PF", "CNES-EP", "CNES-RC", "CNES-IN", "CNES-EE", "CNES-EF", "CNES-GM", "SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD", "SIA-AN", "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", "SIA-PS", "SIA-SAD".
#' 
#' @param year_start,year_end numeric. Start and end year of files in the format yyyy.
#' @param month_start,month_end Numeric. Start and end month in the format mm. Those parameters are only used with the healh information systems SIH, CNES and SIA. There parameter are ignored if the information health system is SIM or SINASC.
#' @param uf an optional string or a vector of strings. By default all UFs ("Unidades Federativas") are download. See \emph{Details}.
#' @param information_system string. The abbreviation of the health information system to be accessed. See \emph{Details}.
#' @param vars an optional string or a vector of strings. By default, all variables read and stored, unless a list of desired variables is informed by this parameter.
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
#' @examples 
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

fetch_datasus <- function(year_start, month_start, year_end, month_end, uf="all", information_system, vars=NULL){
  # Verify health information system
  sisSIH <- c("SIH-RD","SIH-RJ","SIH-SP","SIH-ER")
  sisSIM <- c("SIM-DO","SIM-DOFET","SIM-DOEXT","SIM-DOINF","SIM-DOMAT")
  sisSINASC <- c("SINASC")
  sisCNES <- c("CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR", "CNES-HB","CNES-PF","CNES-EP","CNES-RC","CNES-IN","CNES-EE","CNES-EF","CNES-GM")
  sisSIA <- c("SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD", "SIA-AN", "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", "SIA-PS", "SIA-SAD")
  available_information_system <- c(sisSIH, sisSIM, sisSINASC, sisCNES, sisSIA)
  if(!(information_system %in% available_information_system)) stop("Health informaton system unknown.")

  # Create dates for verification
  if(substr(information_system,1,3) == "SIH" | substr(information_system,1,4) == "CNES" | substr(information_system,1,3) == "SIA"){
    date_start <- as.Date(paste0(year_start,"-",formatC(month_start, width = 2, format = "d", flag = "0"),"-","01"))
    date_end <- as.Date(paste0(year_end,"-",formatC(month_end, width = 2, format = "d", flag = "0"),"-","01"))
  } else if(substr(information_system,1,3) == "SIM" | information_system == "SINASC"){
    date_start <- as.Date(paste0(year_start,"-01-01"))
    date_end <- as.Date(paste0(year_end,"-01-01"))
  }

  # Check dates
  if(date_start > date_end) stop("Start date must be before end date.")

  # Create sequence of dates
  if(substr(information_system,1,3) == "SIH" | substr(information_system,1,4) == "CNES" | substr(information_system,1,3) == "SIA"){
    dates <- seq(date_start, date_end, by = "month")
    dates <- paste0(substr(lubridate::year(dates),3,4),formatC(lubridate::month(dates), width = 2, format = "d", flag = "0"))
  } else if(substr(information_system,1,3) == "SIM" | information_system == "SINASC"){
    dates <- seq(date_start, date_end, by = "year")
    dates <- lubridate::year(dates)
  }


  # Check UF
  ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
  if(!all((uf %in% c("all",ufs)))) stop("UF unknow.")

  # Create download sequence by system, UF and date
  if(information_system %in% sisSIM[2:length(sisSIM)]){
    file_extension <- as.vector(paste0(substr(dates, 3,4),".dbc"))
  } else if (all(uf == "all")) {
    file_extension <- as.vector(sapply(ufs, paste0, dates,".dbc"))
  } else if(information_system %in% sisSIA & "SP" %in% uf){
    uf_sia <- uf[!uf=="SP"]
    file_extension <- as.vector(sapply(uf_sia, paste0, dates,".dbc"))
    
    file_extension_sp_a <- as.vector(sapply("SP", paste0, dates,"a.dbc"))
    file_extension_sp_b <- as.vector(sapply("SP", paste0, dates,"b.dbc"))
    
    file_extension <- c(file_extension, file_extension_sp_a, file_extension_sp_b)
  } else {
    file_extension <- as.vector(sapply(uf, paste0, dates,".dbc"))
  }

  # Create files list for download
  if(information_system == "SIM-DO") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"
    files_list <- paste0(url,"DO", file_extension)
  } else if (information_system == "SIM-DOFET") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    files_list <- paste0(url,"DOFET", file_extension)
  } else if (information_system == "SIM-DOEXT") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    files_list <- paste0(url,"DOEXT", file_extension)
  } else if (information_system == "SIM-DOINF") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    files_list <- paste0(url,"DOINF", file_extension)
  } else if (information_system == "SIM-DOMAT") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    files_list <- paste0(url,"DOMAT", file_extension)
  } else if(information_system == "SIH-RD"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/"
    files_list <- paste0(url,"RD", file_extension)
  } else if(information_system == "SIH-RJ") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/"
    files_list <- paste0(url,"RJ", file_extension)
  } else if(information_system == "SIH-SP") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/"
    files_list <- paste0(url,"SP", file_extension)
  } else if(information_system == "SIH-ER") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/"
    files_list <- paste0(url,"ER", file_extension)
  } else if(information_system == "SINASC") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/"
    files_list <- paste0(url,"DN", file_extension)
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
  }

  # Dowload files
  data <- NULL
  for(file in files_list){
    temp <- tempfile()

    # Try to dowload and read files
    tryCatch({
      download.file(file, temp, mode = "wb")
      partial <- read.dbc::read.dbc(temp)
    },
    error=function(cond) {
      message(paste("Something went wrong with this URL:", file))
      message("This can be a problem with the Internet or the file does not exist yet.")
    })

    # Merge files
    if(!all(vars %in% names(partial))) stop("One or more variables names are unknow. Typo?")
    if(is.null(vars)){
      data <- plyr::rbind.fill(data, partial)
    } else {
      data <- plyr::rbind.fill(data, subset(partial, select = vars))
    }

  }

  # Return
  return(data)
}
