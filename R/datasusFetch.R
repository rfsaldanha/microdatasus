# funDonwload.R
# Download de arquivos DBC do DataSUS

datasusFetch <- function(anoIni, mesIni, anoFim, mesFim, uf="all", sistema="SIM-DO", vars=NULL){
  # Verifica sistema
  sisSIH <- c("SIH-RD","SIH-RJ","SIH-SP","SIH-ER")
  sisSIM <- c("SIM-DO","SIM-DOFET","SIM-DOEXT","SIM-DOINF","SIM-DOMAT")
  sisSINASC <- c("SINASC")
  sisCNES <- c("CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR", "CNES-HB","CNES-PF","CNES-EP","CNES-RC","CNES-IN","CNES-EE","CNES-EF","CNES-GM")
  sisSIA <- c("SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD", "SIA-AN", "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", "SIA-PS", "SIA-SAD")
  sistemas <- c(sisSIH, sisSIM, sisSINASC, sisCNES, sisSIA)
  if(!(sistema %in% sistemas)) stop("Sistema de informação desconhecido ou não implementado.")

  # Cria datas para verificação
  if(substr(sistema,1,3) == "SIH" | substr(sistema,1,4) == "CNES" | substr(sistema,1,3) == "SIA"){
    dataIni <- as.Date(paste0(anoIni,"-",formatC(mesIni, width = 2, format = "d", flag = "0"),"-","01"))
    dataFim <- as.Date(paste0(anoFim,"-",formatC(mesFim, width = 2, format = "d", flag = "0"),"-","01"))
  } else if(substr(sistema,1,3) == "SIM" | sistema == "SINASC"){
    dataIni <- as.Date(paste0(anoIni,"-01-01"))
    dataFim <- as.Date(paste0(anoFim,"-01-01"))
  }

  # Verifica data
  if(dataIni > dataFim) stop("Período inicial maior que período final.")

  # Cria sequência de datas
  if(substr(sistema,1,3) == "SIH" | substr(sistema,1,4) == "CNES" | substr(sistema,1,3) == "SIA"){
    datas <- seq(dataIni, dataFim, by = "month")
    datas <- paste0(substr(lubridate::year(datas),3,4),formatC(lubridate::month(datas), width = 2, format = "d", flag = "0"))
  } else if(substr(sistema,1,3) == "SIM" | sistema == "SINASC"){
    datas <- seq(dataIni, dataFim, by = "year")
    datas <- lubridate::year(datas)
  }


  # Verifica sigla estados
  ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
  if(!all((uf %in% c("all",ufs)))) stop("Sigla de UF incorreta.")

  # Cria sequência de download baseado no sistema, UF e data
  if(sistema %in% sisSIM[2:length(sisSIM)]){
    extensao <- as.vector(paste0(substr(datas, 3,4),".dbc"))
  } else if (all(uf == "all")) {
    extensao <- as.vector(sapply(ufs, paste0, datas,".dbc"))
  } else {
    extensao <- as.vector(sapply(uf, paste0, datas,".dbc"))
  }

  # Inicia download
  if(sistema == "SIM-DO") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"
    listaArquivos <- paste0(url,"DO", extensao)
  } else if (sistema == "SIM-DOFET") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    listaArquivos <- paste0(url,"DOFET", extensao)
  } else if (sistema == "SIM-DOEXT") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    listaArquivos <- paste0(url,"DOEXT", extensao)
  } else if (sistema == "SIM-DOINF") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    listaArquivos <- paste0(url,"DOINF", extensao)
  } else if (sistema == "SIM-DOMAT") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DOFET/"
    listaArquivos <- paste0(url,"DOMAT", extensao)
  } else if(sistema == "SIH-RD"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/"
    listaArquivos <- paste0(url,"RD", extensao)
  } else if(sistema == "SIH-RJ") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/"
    listaArquivos <- paste0(url,"RJ", extensao)
  } else if(sistema == "SIH-SP") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/"
    listaArquivos <- paste0(url,"SP", extensao)
  } else if(sistema == "SIH-ER") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/dados/"
    listaArquivos <- paste0(url,"ER", extensao)
  } else if(sistema == "SINASC") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/"
    listaArquivos <- paste0(url,"DN", extensao)
  } else if(sistema == "CNES-LT"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/LT/"
    listaArquivos <- paste0(url,"LT", extensao)
  } else if(sistema == "CNES-ST"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST/"
    listaArquivos <- paste0(url,"ST", extensao)
  } else if(sistema == "CNES-DC"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/DC/"
    listaArquivos <- paste0(url,"DC", extensao)
  } else if(sistema == "CNES-EQ"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EQ/"
    listaArquivos <- paste0(url,"EQ", extensao)
  } else if(sistema == "CNES-SR"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/SR/"
    listaArquivos <- paste0(url,"SR", extensao)
  } else if(sistema == "CNES-HB"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/HB/"
    listaArquivos <- paste0(url,"HB", extensao)
  } else if(sistema == "CNES-PF"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/PF/"
    listaArquivos <- paste0(url,"PF", extensao)
  } else if(sistema == "CNES-EP"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EP/"
    listaArquivos <- paste0(url,"EP", extensao)
  } else if(sistema == "CNES-RC"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/RC/"
    listaArquivos <- paste0(url,"RC", extensao)
  } else if(sistema == "CNES-IN"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/IN/"
    listaArquivos <- paste0(url,"IN", extensao)
  } else if(sistema == "CNES-EE"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EE/"
    listaArquivos <- paste0(url,"EE", extensao)
  } else if(sistema == "CNES-EF"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/EF/"
    listaArquivos <- paste0(url,"EF", extensao)
  } else if(sistema == "CNES-GM"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/GM/"
    listaArquivos <- paste0(url,"GM", extensao)
  } else if(sistema == "SIA-AB"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"AB", extensao)
  } else if(sistema == "SIA-ABO"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"ABO", extensao)
  } else if(sistema == "SIA-ACF"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"ACF", extensao)
  } else if(sistema == "SIA-AD"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"AD", extensao)
  } else if(sistema == "SIA-AN"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"AN", extensao)
  } else if(sistema == "SIA-AM"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"AM", extensao)
  } else if(sistema == "SIA-AQ"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"AQ", extensao)
  } else if(sistema == "SIA-AR"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"AR", extensao)
  } else if(sistema == "SIA-ATD"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"ATD", extensao)
  } else if(sistema == "SIA-PA"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"PA", extensao)
  } else if(sistema == "SIA-PS"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"PS", extensao)
  } else if(sistema == "SIA-SAD"){
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
    listaArquivos <- paste0(url,"SAD", extensao)
  }

  # Baixa arquivos
  dados <- NULL
  for(arquivo in listaArquivos){
    temp <- tempfile()

    # Tenta baixar e ler arquivos
    tryCatch({
      download.file(arquivo, temp, mode = "wb")
      parcela <- read.dbc::read.dbc(temp)
    },
    error=function(cond) {
      message(paste("Algo deu errado com a URL:", arquivo))
      message("Pode ser um problema na Internet ou o arquivo ainda não existe no site do DataSUS. Os dados deste arquivo não foram inclusos no resultado final.")
    })

    # Junta arquivo baixado ao conjunto
    if(!all(vars %in% names(parcela))) stop("Uma ou mais variáveis são desconhecidas. Grafia incorreta?")
    if(is.null(vars)){
      dados <- plyr::rbind.fill(dados, parcela)
    } else {
      dados <- plyr::rbind.fill(dados, subset(parcela, select = vars))
    }

  }
  # Retorna objeto
  return(dados)
}
