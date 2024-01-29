#' Process SIM variables from DataSUS
#'
#' \code{process_sim} processes SIM variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SIM variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples \dontrun{
#' df <- fetch_datasus(year_start = 2010, year_end = 2010,
#'                     uf = "RJ",
#'                     information_system = "SIM-DO")
#' df_a <- process_sim(df)
#' df_b <- process_sim(df, municipality_data = FALSE)
#' }
#' @export

process_sim <- function(data, municipality_data = TRUE) {
  # Variables names
  variables_names <- names(data)

  # Declara objetos
  ano <- NULL
  unidade <- NULL

  # CODINST
  if ("CODINST" %in% variables_names) {
    data$CODINST[data$CODINST == "E"] <- "Estadual"
    data$CODINST[data$CODINST == "R"] <- "Regional"
    data$CODINST[data$CODINST == "M"] <- "Municipal"
    data$CODINST <- factor(data$CODINST)
  }

  # TIPOBITO
  if ("TIPOBITO" %in% variables_names) {
    data$TIPOBITO <- as.numeric(data$TIPOBITO)
    data$TIPOBITO[data$TIPOBITO == 0] <- NA
    data$TIPOBITO[data$TIPOBITO == 9] <- NA
    data$TIPOBITO[data$TIPOBITO == 1] <- "Fetal"
    data$TIPOBITO[data$TIPOBITO == 2] <- "N\\u00e3o Fetal"
    data$TIPOBITO <- factor(data$TIPOBITO)
  }

  # DTOBITO
  if ("DTOBITO" %in% variables_names) {
    data$DTOBITO <- as.Date(data$DTOBITO, format = "%d%m%Y")
  }

  # NATURAL
  if ("NATURAL" %in% variables_names) {
    colnames(tabNaturalidade)[1] <- "NATURAL"
    data$NATURAL <- factor(dplyr::left_join(data, tabNaturalidade, by = "NATURAL")$nome)
  }

  # DTNASC
  if ("DTNASC" %in% variables_names) {
    data$DTNASC <- as.Date(data$DTNASC, format = "%d%m%Y")
  }

  # IDADE
  if ("IDADE" %in% variables_names) {
    data$IDADE[data$IDADE == "000" | data$IDADE == "999"] <- NA
    unidade <- substr(data$IDADE, 1, 1)
    # Minutos
    data$IDADEminutos <-
      as.numeric(ifelse(unidade == 0, substr(data$IDADE, 2, 3), NA))
    # Horas
    data$IDADEhoras <-
      as.numeric(ifelse(unidade == 1, substr(data$IDADE, 2, 3), NA))
    # Dias
    data$IDADEdias <-
      as.numeric(ifelse(unidade == 2, substr(data$IDADE, 2, 3), NA))
    # Meses
    data$IDADEmeses <-
      as.numeric(ifelse(unidade == 3, substr(data$IDADE, 2, 3), NA))
    # Anos
    data$IDADEanos <-
      as.numeric(ifelse(
        unidade == 4,
        substr(data$IDADE, 2, 3),
        ifelse(unidade == 5, 100 + as.numeric(substr(data$IDADE, 2, 3)), NA)
      ))
    # Apaga campo original
    data$IDADE <- NULL
  }

  # SEXO
  if ("SEXO" %in% variables_names) {
    data$SEXO <- as.numeric(data$SEXO)
    data$SEXO[data$SEXO == 0] <- NA
    data$SEXO[data$SEXO == 9] <- NA
    data$SEXO[data$SEXO == 1] <- "Masculino"
    data$SEXO[data$SEXO == 2] <- "Feminino"
    data$SEXO <- factor(data$SEXO)
  }

  # RACACOR
  if ("RACACOR" %in% variables_names) {
    data$RACACOR <- as.numeric(data$RACACOR)
    data$RACACOR[data$RACACOR == 0] <- NA
    data$RACACOR[data$RACACOR == 1] <- "Branca"
    data$RACACOR[data$RACACOR == 2] <- "Preta"
    data$RACACOR[data$RACACOR == 3] <- "Amarela"
    data$RACACOR[data$RACACOR == 4] <- "Parda"
    data$RACACOR[data$RACACOR == 5] <- "Ind\\u00edgena"
    data$RACACOR[data$RACACOR == 6] <- NA
    data$RACACOR[data$RACACOR == 7] <- NA
    data$RACACOR[data$RACACOR == 8] <- NA
    data$RACACOR[data$RACACOR == 9] <- NA
    data$RACACOR <- factor(data$RACACOR)
  }

  # ESTCIV
  if ("ESTCIV" %in% variables_names) {
    data$ESTCIV <- as.numeric(data$ESTCIV)
    data$ESTCIV[data$ESTCIV == 0] <- NA
    data$ESTCIV[data$ESTCIV == 1] <- "Solteiro"
    data$ESTCIV[data$ESTCIV == 2] <- "Casado"
    data$ESTCIV[data$ESTCIV == 3] <- "Vi\\u00favo"
    data$ESTCIV[data$ESTCIV == 4] <- "Separado judicialmente"
    data$ESTCIV[data$ESTCIV == 5] <- "Uni\\u00e3o consensual"
    data$ESTCIV[data$ESTCIV == 6] <- NA
    data$ESTCIV[data$ESTCIV == 7] <- NA
    data$ESTCIV[data$ESTCIV == 8] <- NA
    data$ESTCIV[data$ESTCIV == 9] <- NA
    data$ESTCIV <- factor(data$ESTCIV)
  }

  # ESC
  if ("ESC" %in% variables_names) {
    data$ESC[data$ESC == "0"] <- NA
    data$ESC[data$ESC == "6"] <- NA
    data$ESC[data$ESC == "7"] <- NA
    data$ESC[data$ESC == "9"] <- NA
    data$ESC[data$ESC == "A"] <- NA
    data$ESC[data$ESC == "1"] <- "Nenhuma"
    data$ESC[data$ESC == "2"] <- "1 a 3 anos"
    data$ESC[data$ESC == "3"] <- "4 a 7 anos"
    data$ESC[data$ESC == "4"] <- "8 a 11 anos"
    data$ESC[data$ESC == "5"] <- "12 anos ou mais"
    data$ESC[data$ESC == "8"] <- "9 a 11 anos"
    data$ESC[data$ESC == "9"] <- NA
    data$ESC <- factor(data$ESC)
  }

  # ESC2010
  if ("ESC2010" %in% variables_names) {
    data$ESC2010 <- as.character(data$ESC2010)
  }

  # SERIESCFAL
  if ("SERIESCFAL" %in% variables_names) {
    data$SERIESCFAL <- as.character(data$SERIESCFAL)
  }

  # OCUP
  if ("OCUP" %in% variables_names) {
    if (!("DTOBITO" %in% variables_names))
      stop("The variable DTOBITO is needed to preprocess the variable OCUP.")

    colnames(tabOcupacao)[1] <- "OCUP"
    tabOcupacao$OCUP = as.character(tabOcupacao$OCUP)

    colnames(tabCBO)[1] <- "OCUP"
    tabCBO$OCUP = as.character(tabCBO$OCUP)

    ano <- lubridate::year(data$DTOBITO)

    data$OCUP <-
      factor(ifelse(
        ano <= 2005,
        dplyr::left_join(data, tabOcupacao, by = "OCUP")$nome,
        dplyr::left_join(data, tabCBO, by = "OCUP")$nome
      ))
  }

  # CODMUNRES
  if ("CODMUNRES" %in% variables_names){
    if(nchar(data$CODMUNRES[1]) == 7){
      data$CODMUNRES <- substr(data$CODMUNRES, 0, 6)
    }
  } else if ("CODMUNRES" %in% variables_names & municipality_data == TRUE) {
    colnames(tabMun)[1] <- "CODMUNRES"
    tabMun$CODMUNRES <- as.character(tabMun$CODMUNRES)
    data <- dplyr::left_join(data, tabMun, by = "CODMUNRES")
  }

  # CODMUNOCOR
  if ("CODMUNOCOR" %in% variables_names){
    if(nchar(data$CODMUNOCOR[1]) == 7){
      data$CODMUNOCOR <- substr(data$CODMUNOCOR, 0, 6)
    }
  }

  # LOCOCOR
  if ("LOCOCOR" %in% variables_names) {
    data$LOCOCOR <- as.numeric(data$LOCOCOR)
    data$LOCOCOR[data$LOCOCOR == 1] <- "Hospital"
    data$LOCOCOR[data$LOCOCOR == 2] <- "Outro estabelecimento de sa\\u00fade"
    data$LOCOCOR[data$LOCOCOR == 3] <- "Domic\\u00edlio"
    data$LOCOCOR[data$LOCOCOR == 4] <- "Via p\\u00fablica"
    data$LOCOCOR[data$LOCOCOR == 5] <- "Outros"
    data$LOCOCOR[data$LOCOCOR == 9] <- NA
    data$LOCOCOR <- factor(data$LOCOCOR)
  }

  # IDADEMAE
  if ("IDADEMAE" %in% variables_names) {
    data$IDADEMAE <- as.numeric(data$IDADEMAE)
    data$IDADEMAE[data$IDADEMAE == 0] <- NA
  }

  # ESCMAE
  if ("ESCMAE" %in% variables_names) {
    data$ESCMAE[data$ESCMAE == "0"] <- NA
    data$ESCMAE[data$ESCMAE == "6"] <- NA
    data$ESCMAE[data$ESCMAE == "7"] <- NA
    data$ESCMAE[data$ESCMAE == "9"] <- NA
    data$ESCMAE[data$ESCMAE == "A"] <- NA
    data$ESCMAE[data$ESCMAE == "1"] <- "Nenhuma"
    data$ESCMAE[data$ESCMAE == "2"] <- "1 a 3 anos"
    data$ESCMAE[data$ESCMAE == "3"] <- "4 a 7 anos"
    data$ESCMAE[data$ESCMAE == "4"] <- "8 a 11 anos"
    data$ESCMAE[data$ESCMAE == "5"] <- "12 anos ou mais"
    data$ESCMAE[data$ESCMAE == "8"] <- "9 a 11 anos"
    data$ESCMAE[data$ESCMAE == "9"] <- NA
    data$ESCMAE <- factor(data$ESCMAE)
  }

  # OCUPMAE
  if ("OCUPMAE" %in% variables_names) {
    if (!("DTOBITO" %in% variables_names))
      stop("The variable DTOBITO is needed to preprocess the variable OCUPMAE.")

    colnames(tabOcupacao)[1] <- "OCUPMAE"
    colnames(tabCBO)[1] <- "OCUPMAE"
    ano <- lubridate::year(data$DTOBITO)
    data$OCUPMAE <-
      factor(ifelse(
        ano <= 2005,
        dplyr::left_join(data, tabOcupacao, by = "OCUPMAE")$nome,
        dplyr::left_join(data, tabCBO, by = "OCUPMAE")$nome
      ))
  }

  # QTDFILVIVO
  if ("QTDFILVIVO" %in% variables_names) {
    data$QTDFILVIVO <- as.numeric(data$QTDFILVIVO)
  }

  # QTDFILMORT
  if ("QTDFILMORT" %in% variables_names) {
    data$QTDFILMORT <- as.numeric(data$QTDFILMORT)
  }

  # GRAVIDEZ
  if ("GRAVIDEZ" %in% variables_names) {
    data$GRAVIDEZ <- as.numeric(data$GRAVIDEZ)
    data$GRAVIDEZ[data$GRAVIDEZ == 1] <- "\\u00fanica"
    data$GRAVIDEZ[data$GRAVIDEZ == 2] <- "Dupla"
    data$GRAVIDEZ[data$GRAVIDEZ == 3] <- "Tr\\u00edplice e mais"
    data$GRAVIDEZ[data$GRAVIDEZ == 9] <- NA
    data$GRAVIDEZ <- factor(data$GRAVIDEZ)
  }

  # GESTACAO
  if ("GESTACAO" %in% variables_names) {
    data$GESTACAO[data$GESTACAO == "0"] <- NA
    data$GESTACAO[data$GESTACAO == "A"] <- "21 a 27 semanas"
    data$GESTACAO[data$GESTACAO == "1"] <- "Menos de 22 semanas"
    data$GESTACAO[data$GESTACAO == "2"] <- "22 a 27 semanas"
    data$GESTACAO[data$GESTACAO == "3"] <- "28 a 31 semanas"
    data$GESTACAO[data$GESTACAO == "4"] <- "32 a 36 semanas"
    data$GESTACAO[data$GESTACAO == "5"] <- "37 a 41 semanas"
    data$GESTACAO[data$GESTACAO == "6"] <- "42 semanas e mais"
    data$GESTACAO[data$GESTACAO == "7"] <- "28 semanas e mais"
    data$GESTACAO[data$GESTACAO == "8"] <- "28 a 36 semanas"
    data$GESTACAO[data$GESTACAO == "9"] <- NA
    data$GESTACAO <- factor(data$GESTACAO)
  }

  # PARTO
  if ("PARTO" %in% variables_names) {
    data$PARTO <- as.numeric(data$PARTO)
    data$PARTO[data$PARTO == 0] <- NA
    data$PARTO[data$PARTO == 1] <- "Vaginal"
    data$PARTO[data$PARTO == 2] <- "Ces\\u00e1reo"
    data$PARTO[data$PARTO == 3] <- NA
    data$PARTO[data$PARTO == 4] <- NA
    data$PARTO[data$PARTO == 5] <- NA
    data$PARTO[data$PARTO == 6] <- NA
    data$PARTO[data$PARTO == 7] <- NA
    data$PARTO[data$PARTO == 8] <- NA
    data$PARTO[data$PARTO == 9] <- NA
    data$PARTO <- factor(data$PARTO)
  }

  # OBITOPARTO
  if ("OBITOPARTO" %in% variables_names) {
    data$OBITOPARTO <- as.numeric(data$OBITOPARTO)
    data$OBITOPARTO[data$OBITOPARTO == 0] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 1] <- "Antes"
    data$OBITOPARTO[data$OBITOPARTO == 2] <- "Durante"
    data$OBITOPARTO[data$OBITOPARTO == 3] <- "Depois"
    data$OBITOPARTO[data$OBITOPARTO == 4] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 5] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 6] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 7] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 8] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 9] <- NA
    data$OBITOPARTO <- factor(data$OBITOPARTO)
  }

  # PESO
  if ("PESO" %in% variables_names) {
    data$PESO <- as.numeric(data$PESO)
    data$PESO[data$PESO == 0] <- NA
  }

  # OBITOGRAV
  if ("OBITOGRAV" %in% variables_names) {
    data$OBITOGRAV <- as.numeric(data$OBITOGRAV)
    data$OBITOGRAV[data$OBITOGRAV == 1] <- "Sim"
    data$OBITOGRAV[data$OBITOGRAV == 2] <- "N\\u00e3o"
    data$OBITOGRAV[data$OBITOGRAV == 3] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 4] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 5] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 6] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 7] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 8] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 9] <- NA
    data$OBITOGRAV <- factor(data$OBITOGRAV)
  }

  # OBITOPUERP
  if ("OBITOPUERP" %in% variables_names) {
    data$OBITOPUERP <- as.numeric(data$OBITOPUERP)
    data$OBITOPUERP[data$OBITOPUERP == 1] <- "De 0 a 42 dias"
    data$OBITOPUERP[data$OBITOPUERP == 2] <- "De 43 dias a 1 ano"
    data$OBITOPUERP[data$OBITOPUERP == 3] <- "N\\u00e3o"
    data$OBITOPUERP[data$OBITOPUERP == 4] <- NA
    data$OBITOPUERP[data$OBITOPUERP == 5] <- NA
    data$OBITOPUERP[data$OBITOPUERP == 6] <- NA
    data$OBITOPUERP[data$OBITOPUERP == 7] <- NA
    data$OBITOPUERP[data$OBITOPUERP == 8] <- NA
    data$OBITOPUERP[data$OBITOPUERP == 9] <- NA
    data$OBITOPUERP <- factor(data$OBITOPUERP)
  }

  # ASSISTMED
  if ("ASSISTMED" %in% variables_names) {
    data$ASSISTMED <- as.numeric(data$ASSISTMED)
    data$ASSISTMED[data$ASSISTMED == 1] <- "Sim"
    data$ASSISTMED[data$ASSISTMED == 2] <- "N\\u00e3o"
    data$ASSISTMED[data$ASSISTMED == 9] <- NA
    data$ASSISTMED <- factor(data$ASSISTMED)
  }

  # EXAME
  if ("EXAME" %in% variables_names) {
    data$EXAME <- as.numeric(data$EXAME)
    data$EXAME[data$EXAME == 1] <- "Sim"
    data$EXAME[data$EXAME == 2] <- "N\\u00e3o"
    data$EXAME[data$EXAME == 9] <- NA
    data$EXAME <- factor(data$EXAME)
  }

  # CIRURGIA
  if ("CIRURGIA" %in% variables_names) {
    data$CIRURGIA <- as.numeric(data$CIRURGIA)
    data$CIRURGIA[data$CIRURGIA == 1] <- "Sim"
    data$CIRURGIA[data$CIRURGIA == 2] <- "N\\u00e3o"
    data$CIRURGIA[data$CIRURGIA == 9] <- NA
    data$CIRURGIA <- factor(data$CIRURGIA)
  }

  # NECROPSIA
  if ("NECROPSIA" %in% variables_names) {
    data$NECROPSIA <- as.numeric(data$NECROPSIA)
    data$NECROPSIA[data$NECROPSIA == 1] <- "Sim"
    data$NECROPSIA[data$NECROPSIA == 2] <- "N\\u00e3o"
    data$NECROPSIA[data$NECROPSIA == 9] <- NA
    data$NECROPSIA <- factor(data$NECROPSIA)
  }

  # DTATESTADO
  if ("DTATESTADO" %in% variables_names) {
    data$DTATESTADO <- as.Date(data$DTATESTADO, format = "%d%m%Y")
  }

  # CIRCOBITO
  if ("CIRCOBITO" %in% variables_names) {
    data$CIRCOBITO <- as.numeric(data$CIRCOBITO)
    data$CIRCOBITO[data$CIRCOBITO == 0] <- NA
    data$CIRCOBITO[data$CIRCOBITO == 1] <- "Acidente"
    data$CIRCOBITO[data$CIRCOBITO == 2] <- "Suic\\u00eddio"
    data$CIRCOBITO[data$CIRCOBITO == 3] <- "Homic\\u00eddio"
    data$CIRCOBITO[data$CIRCOBITO == 4] <- "Outro"
    data$CIRCOBITO[data$CIRCOBITO == 5] <- NA
    data$CIRCOBITO[data$CIRCOBITO == 6] <- NA
    data$CIRCOBITO[data$CIRCOBITO == 7] <- NA
    data$CIRCOBITO[data$CIRCOBITO == 8] <- NA
    data$CIRCOBITO[data$CIRCOBITO == 9] <- NA
    data$CIRCOBITO <- factor(data$CIRCOBITO)
  }

  # ACIDTRAB
  if ("ACIDTRAB" %in% variables_names) {
    data$ACIDTRAB <- as.numeric(data$ACIDTRAB)
    data$ACIDTRAB[data$ACIDTRAB == 1] <- "Sim"
    data$ACIDTRAB[data$ACIDTRAB == 2] <- "N\\u00e3o"
    data$ACIDTRAB[data$ACIDTRAB == 9] <- NA
    data$ACIDTRAB <- factor(data$ACIDTRAB)
  }

  # FONTE
  if ("FONTE" %in% variables_names) {
    data$FONTE <- as.numeric(data$FONTE)
    data$FONTE[data$FONTE == 1] <- "Boletim de Ocorr\\u00eancia"
    data$FONTE[data$FONTE == 2] <- "Hospital"
    data$FONTE[data$FONTE == 3] <- "Fam\\u00edlia"
    data$FONTE[data$FONTE == 4] <- "Outro"
    data$FONTE[data$FONTE == 9] <- NA
    data$FONTE <- factor(data$FONTE)
  }

  # TPPOS
  if ("TPPOS" %in% variables_names) {
    data$TPPOS[data$TPPOS == "N"] <- "N\\u00e3o investigado"
    data$TPPOS[data$TPPOS == "S"] <- "Investigado"
  }

  # DTINVESTIG
  if ("DTINVESTIG" %in% variables_names) {
    data$DTINVESTIG <- as.Date(data$DTINVESTIG, format = "%d%m%Y")
  }

  # DTCADASTRO
  if ("DTCADASTRO" %in% variables_names) {
    data$DTCADASTRO <- as.Date(data$DTCADASTRO, format = "%d%m%Y")
  }

  # ATESTANTE
  if ("ATESTANTE" %in% variables_names) {
    data$ATESTANTE <- as.numeric(data$ATESTANTE)
    data$ATESTANTE[data$ATESTANTE == 0] <- NA
    data$ATESTANTE[data$ATESTANTE == 1] <- "Sim"
    data$ATESTANTE[data$ATESTANTE == 2] <- "Substituto"
    data$ATESTANTE[data$ATESTANTE == 3] <- "IML"
    data$ATESTANTE[data$ATESTANTE == 4] <- "SVO"
    data$ATESTANTE[data$ATESTANTE == 5] <- "Outro"
    data$ATESTANTE[data$ATESTANTE == 6] <- NA
    data$ATESTANTE[data$ATESTANTE == 7] <- NA
    data$ATESTANTE[data$ATESTANTE == 8] <- NA
    data$ATESTANTE[data$ATESTANTE == 9] <- NA
    data$ATESTANTE <- factor(data$ATESTANTE)
  }

  # FONTEINV
  if ("FONTEINV" %in% variables_names) {
    data$FONTEINV <- as.numeric(data$FONTEINV)
    data$FONTEINV[data$FONTEINV == 1] <- "Comit\\u00ea de Mortalidade Materna e/ou Infantil"
    data$FONTEINV[data$FONTEINV == 2] <- "Visita familiar / Entrevista fam\\u00edlia"
    data$FONTEINV[data$FONTEINV == 3] <- "Estabelecimento de sa\\u00fade / Prontu\\u00e1rio"
    data$FONTEINV[data$FONTEINV == 4] <- "Relacionamento com outros bancos de dados"
    data$FONTEINV[data$FONTEINV == 5] <- "SVO"
    data$FONTEINV[data$FONTEINV == 6] <- "IML"
    data$FONTEINV[data$FONTEINV == 7] <- "Outra fonte"
    data$FONTEINV[data$FONTEINV == 8] <- "M\\u00faltiplas fontes"
    data$FONTEINV[data$FONTEINV == 9] <- NA
    data$FONTEINV <- factor(data$FONTEINV)
  }

  # DTRECEBIM
  if ("DTRECEBIM" %in% variables_names) {
    data$DTRECEBIM <- as.Date(data$DTRECEBIM, format = "%d%m%Y")
  }

  # DTRECORIGA
  if ("DTRECORIGA" %in% variables_names) {
    data$DTRECORIGA <- as.Date(data$DTRECORIGA, format = "%d%m%Y")
  }

  # Purge levels
  data <- droplevels(data)

  # Remove objects
  rm(ano, unidade)

  # Unescape unicode characters
  data <- as.data.frame(lapply(X = data, FUN = stringi::stri_unescape_unicode))

  # Return
  return(data)

}
