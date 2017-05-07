# funDonwload.R
# Download de arquivos DBC do DataSUS

datasusFetch <- function(anoIni=year(today()), mesIni, anoFim=year(today()), mesFim, uf="all", sistema="SIM", vars=NULL){
  # Verifica sistema
  sistemas <- c("SIH-RD","SIH-RJ","SIH-SP","SIH-ER","SIM","SINASC")
  if(!(sistema %in% sistemas)) stop("Sistema de informação desconhecido ou não implementado.")

  # Cria datas para verificação
  if(substr(sistema,1,3) == "SIH"){
    dataIni <- as.Date(paste0(anoIni,"-",formatC(mesIni, width = 2, format = "d", flag = "0"),"-","01"))
    dataFim <- as.Date(paste0(anoFim,"-",formatC(mesFim, width = 2, format = "d", flag = "0"),"-","01"))
  } else if(substr(sistema,1,3) == "SIM" | sistema == "SINASC"){
    dataIni <- as.Date(paste0(anoIni,"-01-01"))
    dataFim <- as.Date(paste0(anoFim,"-01-01"))
  }

  # Verifica data
  if(dataIni > dataFim) stop("Período inicial maior que período final.")

  # Cria sequência de datas
  if(substr(sistema,1,3) == "SIH"){
    datas <- seq(dataIni, dataFim, by = "month")
    datas <- paste0(substr(lubridate::year(datas),3,4),formatC(lubridate::month(datas), width = 2, format = "d", flag = "0"))
  } else if(substr(sistema,1,3) == "SIM" | sistema == "SINASC"){
    datas <- seq(dataIni, dataFim, by = "year")
    datas <- lubridate::year(datas)
  }


  # Verifica sigla estados
  ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
  if(!all((uf %in% c("all",ufs)))) stop("Sigla de UF incorreta.")

  # Cria sequência de download baseado na data e UF
  if(all(uf == "all")) {
    extensao <- as.vector(sapply(ufs, paste0, datas,".dbc"))
  } else {
    extensao <- as.vector(sapply(uf, paste0, datas,".dbc"))
  }

  # Inicia download
  if(sistema == "SIH-RD"){
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
  } else if(sistema == "SIM") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/"
    listaArquivos <- paste0(url,"DO", extensao)
  } else if(sistema == "SINASC") {
    url <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/DNRES/"
    listaArquivos <- paste0(url,"DN", extensao)
  }

  # Baixa arquivos
  dados <- NULL
  for(arquivo in listaArquivos){
    temp <- tempfile()

    # Tenta baixar e ler arquivos
    tryCatch({
      download.file(arquivo, temp)
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
