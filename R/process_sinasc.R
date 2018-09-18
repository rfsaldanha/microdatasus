#' Process SINASC variables from DataSUS
#' 
#' \code{process_sinasc} processes SINASC variables retrieved by \code{fetch_datasus()}.
#' 
#' This function processes SINASC variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#' 
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#' 
#' @examples 
#' df <- fetch_datasus(year_start = 2010, year_end = 2010,
#'                     uf = "RJ",
#'                     information_system = "SINASC")
#' df_a <- process_sinasc(df)
#' df_b <- process_sinasc(df, municipality_data = FALSE)

process_sinasc <- function(data, municipality_data = TRUE) {
  # Variables names
  variables_names <- names(data)
  
  # NUMERODN
  if("NUMERODN" %in% variables_names){
    data$NUMERODN <- as.integer(data$NUMERODN)
  }
  
  # CODINST
  if("CODINST" %in% variables_names){
    data$CODINST <- as.integer(data$CODINST)
  }
  
  # ORIGEM
  if("ORIGEM" %in% variables_names){
    data$ORIGEM <- as.integer(data$ORIGEM)
  }
  
  # NUMERODV
  if("NUMERODV" %in% variables_names){
    data$NUMERODV <- as.integer(data$NUMERODV)
  }
  
  # PREFIXODN
  if("PREFIXODN" %in% variables_names){
    data$PREFIXODN <- as.integer(data$PREFIXODN)
  }
  
  # CODESTAB
  if("CODESTAB" %in% variables_names){
    data$CODESTAB <- as.character(data$CODESTAB)
  }
  
  # CODMUNNASC
  if("CODMUNNASC" %in% variables_names){
    data$CODMUNNASC <- as.integer(data$CODMUNNASC)
  }
  
  # LOCNASC
  if("LOCNASC" %in% variables_names){
    data$LOCNASC <- as.numeric(levels(data$LOCNASC))[data$LOCNASC]
    data$LOCNASC[data$LOCNASC==0] <- NA
    data$LOCNASC[data$LOCNASC==1] <- "Hospital"
    data$LOCNASC[data$LOCNASC==2] <- "Outro estabelecimento de saúde"
    data$LOCNASC[data$LOCNASC==3] <- "Domicílio"
    data$LOCNASC[data$LOCNASC==4] <- "Outros"
    data$LOCNASC[data$LOCNASC==5] <- NA
    data$LOCNASC[data$LOCNASC==6] <- NA
    data$LOCNASC[data$LOCNASC==7] <- NA
    data$LOCNASC[data$LOCNASC==8] <- NA
    data$LOCNASC[data$LOCNASC==9] <- NA
    data$LOCNASC <- factor(data$LOCNASC)
  }
  
  # IDADEMAE
  if("IDADEMAE" %in% variables_names){
    data$IDADEMAE <- as.numeric(data$IDADEMAE)
    data$IDADEMAE[data$IDADEMAE==0] <- NA
    data$IDADEMAE[data$IDADEMAE==99] <- NA
  }
  
  # ESTCIVMAE
  if("ESTCIVMAE" %in% variables_names){
    data$ESTCIVMAE <- as.numeric(levels(data$ESTCIVMAE))[data$ESTCIVMAE]
    data$ESTCIVMAE[data$ESTCIVMAE==0] <- NA
    data$ESTCIVMAE[data$ESTCIVMAE==1] <- "Solteira"
    data$ESTCIVMAE[data$ESTCIVMAE==2] <- "Casada"
    data$ESTCIVMAE[data$ESTCIVMAE==3] <- "Viúva"
    data$ESTCIVMAE[data$ESTCIVMAE==4] <- "Separada judicialmente"
    data$ESTCIVMAE[data$ESTCIVMAE==5] <- "União consensual"
    data$ESTCIVMAE[data$ESTCIVMAE==6] <- NA
    data$ESTCIVMAE[data$ESTCIVMAE==7] <- NA
    data$ESTCIVMAE[data$ESTCIVMAE==8] <- NA
    data$ESTCIVMAE[data$ESTCIVMAE==9] <- NA
    data$ESTCIVMAE <- factor(data$ESTCIVMAE)
  }
  
  # ESCMAE
  if("ESCMAE" %in% variables_names){
    data$ESCMAE <- as.numeric(levels(data$ESCMAE))[data$ESCMAE]
    data$ESCMAE[data$ESCMAE==1] <- "Nenhum"
    data$ESCMAE[data$ESCMAE==2] <- "1 a 3 anos"
    data$ESCMAE[data$ESCMAE==3] <- "4 a 7 anos"
    data$ESCMAE[data$ESCMAE==4] <- "8 a 11 anos"
    data$ESCMAE[data$ESCMAE==5] <- "12 anos ou mais"
    data$ESCMAE[data$ESCMAE==6] <- NA
    data$ESCMAE[data$ESCMAE==7] <- NA
    data$ESCMAE[data$ESCMAE==8] <- "9 a 11 anos"
    data$ESCMAE[data$ESCMAE==9] <- NA
    data$ESCMAE <- factor(data$ESCMAE)
  }
  
  # DTNASC
  if("DTNASC" %in% variables_names){
    data$DTNASC <- as.character(data$DTNASC)
    data$DTNASC <- as.Date(data$DTNASC, format = "%d%m%Y")
  }
  
  # CODOCUPMAE
  if ("CODOCUPMAE" %in% variables_names) {
    if (!("DTNASC" %in% variables_names))
      stop("The variable DTNASC is needed to preprocess the variable CODOCUPMAE")
    data$CODOCUPMAE <- as.character(data$CODOCUPMAE)
    colnames(tabOcupacao)[1] <- "CODOCUPMAE"
    colnames(tabCBO)[1] <- "CODOCUPMAE"
    ano <- lubridate::year(data$DTNASC)
    data$CODOCUPMAE <-
      factor(ifelse(
        ano <= 2005,
        plyr::join(data, tabOcupacao, by = "CODOCUPMAE", match = "first")$nome,
        dplyr::left_join(data, tabCBO, by = "CODOCUPMAE")$nome
      ))
  }
  
  
  # QTDFILVIVO
  if("QTDFILVIVO" %in% variables_names){
    data$QTDFILVIVO <- as.integer(data$QTDFILVIVO)
    data$QTDFILVIVO[data$QTDFILVIVO==99] <- NA
  }
  
  # QTDFILMORT
  if("QTDFILMORT" %in% variables_names){
    data$QTDFILMORT <- as.integer(data$QTDFILMORT)
    data$QTDFILMORT[data$QTDFILMORT==99] <- NA
  }
  
  # CODMUNRES
  if("CODMUNRES" %in% variables_names & municipality_data == TRUE){
    data$CODMUNRES <- as.integer(as.character(data$CODMUNRES))
    colnames(tabMun)[1] <- "CODMUNRES"
    data <- dplyr::left_join(data, tabMun, by = "CODMUNRES")
  } else {
    data$CODMUNRES <- as.integer(as.character(data$CODMUNRES))
  }
  
  # GESTACAO
  if("GESTACAO" %in% variables_names){
    data$GESTACAO <- as.numeric(levels(data$GESTACAO))[data$GESTACAO]
    data$GESTACAO[data$GESTACAO==0] <- NA
    data$GESTACAO[data$GESTACAO==1] <- "Menos de 22 semanas"
    data$GESTACAO[data$GESTACAO==2] <- "22 a 27 semanas"
    data$GESTACAO[data$GESTACAO==3] <- "28 a 31 semanas"
    data$GESTACAO[data$GESTACAO==4] <- "32 a 36 semanas"
    data$GESTACAO[data$GESTACAO==5] <- "37 a 41 semanas"
    data$GESTACAO[data$GESTACAO==6] <- "42 semanas ou mais"
    data$GESTACAO[data$GESTACAO==7] <- NA
    data$GESTACAO[data$GESTACAO==8] <- NA
    data$GESTACAO[data$GESTACAO==9] <- NA
    data$GESTACAO <- factor(data$GESTACAO)
  }
  
  # GRAVIDEZ
  if("GRAVIDEZ" %in% variables_names){
    data$GRAVIDEZ <- as.numeric(levels(data$GRAVIDEZ))[data$GRAVIDEZ]
    data$GRAVIDEZ[data$GRAVIDEZ==0] <- NA
    data$GRAVIDEZ[data$GRAVIDEZ==1] <- "Única"
    data$GRAVIDEZ[data$GRAVIDEZ==2] <- "Dupla"
    data$GRAVIDEZ[data$GRAVIDEZ==3] <- "Tripla e mais"
    data$GRAVIDEZ[data$GRAVIDEZ==4] <- NA
    data$GRAVIDEZ[data$GRAVIDEZ==5] <- NA
    data$GRAVIDEZ[data$GRAVIDEZ==6] <- NA
    data$GRAVIDEZ[data$GRAVIDEZ==7] <- NA
    data$GRAVIDEZ[data$GRAVIDEZ==8] <- NA
    data$GRAVIDEZ[data$GRAVIDEZ==9] <- NA
    data$GRAVIDEZ <- factor(data$GRAVIDEZ)
  }
  
  # PARTO
  if("PARTO" %in% variables_names){
    data$PARTO <- as.numeric(levels(data$PARTO))[data$PARTO]
    data$PARTO[data$PARTO==0] <- NA
    data$PARTO[data$PARTO==1] <- "Vaginal"
    data$PARTO[data$PARTO==2] <- "Cesáreo"
    data$PARTO[data$PARTO==3] <- NA
    data$PARTO[data$PARTO==4] <- NA
    data$PARTO[data$PARTO==5] <- NA
    data$PARTO[data$PARTO==6] <- NA
    data$PARTO[data$PARTO==7] <- NA
    data$PARTO[data$PARTO==8] <- NA
    data$PARTO[data$PARTO==9] <- NA
    data$PARTO <- factor(data$PARTO)
  }
  
  # CONSULTAS
  if("CONSULTAS" %in% variables_names){
    data$CONSULTAS <- as.numeric(levels(data$CONSULTAS))[data$CONSULTAS]
    data$CONSULTAS[data$CONSULTAS==0] <- NA
    data$CONSULTAS[data$CONSULTAS==1] <- "Nenhuma"
    data$CONSULTAS[data$CONSULTAS==2] <- "1 a 3 vezes"
    data$CONSULTAS[data$CONSULTAS==3] <- "4 a 6 vezes"
    data$CONSULTAS[data$CONSULTAS==4] <- "7 ou mais vezes"
    data$CONSULTAS[data$CONSULTAS==5] <- NA
    data$CONSULTAS[data$CONSULTAS==6] <- NA
    data$CONSULTAS[data$CONSULTAS==7] <- NA
    data$CONSULTAS[data$CONSULTAS==8] <- NA
    data$CONSULTAS[data$CONSULTAS==9] <- NA
    data$CONSULTAS <- factor(data$CONSULTAS)
  }
  
  # HORANASC
  if("HORANASC" %in% variables_names){
    data$HORANASC <- as.character(data$HORANASC)
  }
  
  # SEXO
  if("SEXO" %in% variables_names){
    data$SEXO <- as.numeric(levels(data$SEXO))[data$SEXO]
    data$SEXO[data$SEXO==0] <- NA
    data$SEXO[data$SEXO==1] <- "Masculino"
    data$SEXO[data$SEXO==2] <- "Feminino"
    data$SEXO[data$SEXO==9] <- NA
    data$SEXO <- factor(data$SEXO)
  }
  
  # APGAR1
  if("APGAR1" %in% variables_names){
    data$APGAR1 <- as.numeric(data$APGAR1)
    data$APGAR1[data$APGAR1==99] <- NA
  }
  
  # APGAR5
  if("APGAR5" %in% variables_names){
    data$APGAR5 <- as.numeric(data$APGAR5)
    data$APGAR5[data$APGAR5==99] <- NA
  }
  
  # RACACOR
  if("RACACOR" %in% variables_names){
    data$RACACOR <- as.numeric(levels(data$RACACOR))[data$RACACOR]
    data$RACACOR[data$RACACOR==1] <- "Branca"
    data$RACACOR[data$RACACOR==2] <- "Preta"
    data$RACACOR[data$RACACOR==3] <- "Amarela"
    data$RACACOR[data$RACACOR==4] <- "Parda"
    data$RACACOR[data$RACACOR==5] <- "Indígena"
    data$RACACOR <- factor(data$RACACOR)
  }
  
  # PESO
  if("PESO" %in% variables_names){
    data$PESO <- as.numeric(as.character(data$PESO))
    data$PESO[data$PESO==0] <- NA
    data$PESO[data$PESO==9999] <- NA
  }
  
  # IDANOMAL
  if("IDANOMAL" %in% variables_names){
    data$IDANOMAL <- as.numeric(levels(data$IDANOMAL))[data$IDANOMAL]
    data$IDANOMAL[data$IDANOMAL==1] <- "Sim"
    data$IDANOMAL[data$IDANOMAL==2] <- "Não"
    data$IDANOMAL[data$IDANOMAL==3] <- NA
    data$IDANOMAL[data$IDANOMAL==4] <- NA
    data$IDANOMAL[data$IDANOMAL==5] <- NA
    data$IDANOMAL[data$IDANOMAL==6] <- NA
    data$IDANOMAL[data$IDANOMAL==7] <- NA
    data$IDANOMAL[data$IDANOMAL==8] <- NA
    data$IDANOMAL[data$IDANOMAL==9] <- NA
    data$IDANOMAL <- factor(data$IDANOMAL)
  }
  
  # DTCADASTRO
  if("DTCADASTRO" %in% variables_names){
    data$DTCADASTRO <- as.character(data$DTCADASTRO)
    data$DTCADASTRO <- as.Date(data$DTCADASTRO, format = "%d%m%Y")
  }
  
  # CODANOMAL
  if("CODANOMAL" %in% variables_names){
    data$CODANOMAL <- as.character(data$CODANOMAL)
  }
  
  # NUMEROLOTE
  if("NUMEROLOTE" %in% variables_names){
    data$NUMEROLOTE <- as.character(data$NUMEROLOTE)
  }
  
  # VERSAOSIST
  if("VERSAOSIST" %in% variables_names){
    data$VERSAOSIST <- as.character(data$VERSAOSIST)
  }
  
  # DTRECEBIM
  if("DTRECEBIM" %in% variables_names){
    data$DTRECEBIM <- as.character(data$DTRECEBIM)
    data$DTRECEBIM <- as.Date(data$DTRECEBIM, format = "%d%m%Y")
  }
  
  # DIFDATA
  if("DIFDATA" %in% variables_names){
    data$DIFDATA <- as.integer(data$DIFDATA)
  }
  
  # DTRECORIG
  if("DTRECORIG" %in% variables_names){
    data$DTRECORIG <- as.character(data$DTRECORIG)
    data$DTRECORIG <- as.Date(data$DTRECORIG, format = "%d%m%Y")
  }
  
  # NATURALMAE
  if("NATURALMAE" %in% variables_names){
    data$NATURALMAE <- as.integer(data$NATURALMAE)
  }
  
  # CODMUNNATU
  if("CODMUNNATU" %in% variables_names){
    data$CODMUNNATU <- as.integer(data$CODMUNNATU)
  }
  
  # CODUFNATU
  if("CODUFNATU" %in% variables_names){
    data$CODUFNATU <- as.integer(data$CODUFNATU)
  }
  
  # ESCMAE2010
  if("CODUFNATU" %in% variables_names){
    data$CODUFNATU <- as.integer(data$CODUFNATU)
  }
  
  # SERIESCMAE
  if("SERIESCMAE" %in% variables_names){
    data$SERIESCMAE <- as.integer(data$SERIESCMAE)
  }
  
  # DTNASCMAE
  if("DTNASCMAE" %in% variables_names){
    data$DTNASCMAE <- as.character(data$DTNASCMAE)
    data$DTNASCMAE <- as.Date(data$DTNASCMAE, format = "%d%m%Y")
  }
  
  # RACACORMAE
  if("RACACORMAE" %in% variables_names){
    data$RACACORMAE <- as.numeric(levels(data$RACACORMAE))[data$RACACORMAE]
    data$RACACORMAE[data$RACACORMAE==1] <- "Branca"
    data$RACACORMAE[data$RACACORMAE==2] <- "Preta"
    data$RACACORMAE[data$RACACORMAE==3] <- "Amarela"
    data$RACACORMAE[data$RACACORMAE==4] <- "Parda"
    data$RACACORMAE[data$RACACORMAE==5] <- "Indígena"
    data$RACACORMAE <- factor(data$RACACORMAE)
  }
  
  # QTDGESTANT
  if("QTDGESTANT" %in% variables_names){
    data$QTDGESTANT <- as.integer(data$QTDGESTANT)
  }
  
  # QTDPARTNOR
  if("QTDPARTNOR" %in% variables_names){
    data$QTDPARTNOR <- as.integer(data$QTDPARTNOR)
  }
  
  # QTDPARTCES
  if("QTDPARTCES" %in% variables_names){
    data$QTDPARTCES <- as.integer(data$QTDPARTCES)
  }
  
  # IDADEPAI
  if("IDADEPAI" %in% variables_names){
    data$IDADEPAI <- as.integer(data$IDADEPAI)
  }
  
  # DTULTMENST
  if("DTULTMENST" %in% variables_names){
    data$DTULTMENST <- as.character(data$DTULTMENST)
    data$DTULTMENST <- as.Date(data$DTULTMENST, format = "%d%m%Y")
  }
  
  # SEMAGESTAC
  if("SEMAGESTAC" %in% variables_names){
    data$SEMAGESTAC <- as.numeric(as.character(data$SEMAGESTAC))
  }
  
  # TPMETESTIM
  if("TPMETESTIM" %in% variables_names){
    data$TPMETESTIM <- as.integer(data$TPMETESTIM)
  }
  
  # CONSPRENAT
  if("CONSPRENAT" %in% variables_names){
    data$CONSPRENAT <- as.integer(data$CONSPRENAT)
  }
  
  # MESPRENAT
  if("MESPRENAT" %in% variables_names){
    data$MESPRENAT <- as.integer(data$MESPRENAT)
  }
  
  # TPAPRESENT
  if("TPAPRESENT" %in% variables_names){
    data$TPAPRESENT <- as.integer(data$TPAPRESENT)
  }
  
  # STTRABPART
  if("STTRABPART" %in% variables_names){
    data$STTRABPART <- as.integer(data$STTRABPART)
  }
  
  # STCESPARTO
  if("STCESPARTO" %in% variables_names){
    data$STCESPARTO <- as.integer(data$STCESPARTO)
  }
  
  # TPNASCASSI
  if("TPNASCASSI" %in% variables_names){
    data$TPNASCASSI <- as.integer(data$TPNASCASSI)
  }
  
  # TPFUNCRESP
  if("TPFUNCRESP" %in% variables_names){
    data$TPFUNCRESP <- as.integer(data$TPFUNCRESP)
  }
  
  # TPDOCRESP
  if("TPDOCRESP" %in% variables_names){
    data$TPDOCRESP <- as.integer(data$TPDOCRESP)
  }
  
  # DTDECLARAC
  if("DTDECLARAC" %in% variables_names){
    data$DTDECLARAC <- as.character(data$DTDECLARAC)
    data$DTDECLARAC <- as.Date(data$DTDECLARAC, format = "%d%m%Y")
  }
  
  # ESCMAEAGR1
  if("ESCMAEAGR1" %in% variables_names){
    data$ESCMAEAGR1 <- as.integer(data$ESCMAEAGR1)
  }
  
  # TPROBSON
  if("TPROBSON" %in% variables_names){
    data$TPROBSON <- as.integer(data$TPROBSON)
  }
  
  # STDNEPIDEM
  if("STDNEPIDEM" %in% variables_names){
    data$STDNEPIDEM <- as.integer(data$STDNEPIDEM)
  }
  
  # STDNNOVA
  if("STDNNOVA" %in% variables_names){
    data$STDNNOVA <- as.integer(data$STDNNOVA)
  }
  
  # CODPAISRES
  if("CODPAISRES" %in% variables_names){
    data$CODPAISRES <- as.integer(data$CODPAISRES)
  }
  
  # PARIDADE
  if("PARIDADE" %in% variables_names){
    data$PARIDADE <- as.integer(data$PARIDADE)
  }
  
  # Purge levels
  data <- droplevels(data)
  
  # Return
  return(data)
}