#' Process SINAN Chikungunya variables from DataSUS
#'
#' \code{process_sinan_chikungunya} processes SINAN Chikungunya variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SINAN Chikungunya variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples \dontrun{
#' df <- fetch_datasus(year_start = 2016, year_end = 2016,
#' uf = "RJ", information_system = "SINAN-CHIKUNGUNYA-FINAL")
#' df_a <- process_sinan_chikungunya(df)
#' df_b <- process_sinan_chikungunya(df, municipality_data = FALSE)
#' }
#' @export

process_sinan_chikungunya <- function(data, municipality_data = TRUE){
  # Variables names
  variables_names <- names(data)

  # Declara objetos
  ano <- NULL
  unidade <- NULL

  # TP_NOT
  if ("TP_NOT" %in% variables_names) {
    data$TP_NOT <- as.numeric(data$TP_NOT)
    data$TP_NOT[data$TP_NOT == 1] <- "Negativa"
    data$TP_NOT[data$TP_NOT == 2] <- "Individual"
    data$TP_NOT[data$TP_NOT == 3] <- "Surto"
    data$TP_NOT[data$TP_NOT == 4] <- "Agregado"
    data$TP_NOT <- factor(data$TP_NOT)
  }

  # DT_NOTIFIC
  if ("DT_NOTIFIC" %in% variables_names) {
    data$DT_NOTIFIC <- as.Date(data$DT_NOTIFIC)
  }

  # SG_UF_NOT
  if ("SG_UF_NOT" %in% variables_names) {
    data$SG_UF_NOT <- as.numeric(data$SG_UF_NOT)
    data$SG_UF_NOT[data$SG_UF_NOT == 0] <- "Ignorado"
    data$SG_UF_NOT[data$SG_UF_NOT == 99] <- "Ignorado"
    data$SG_UF_NOT[data$SG_UF_NOT == 11] <- "Rond\u00f4nia"
    data$SG_UF_NOT[data$SG_UF_NOT == 12] <- "Acre"
    data$SG_UF_NOT[data$SG_UF_NOT == 13] <- "Amazonas"
    data$SG_UF_NOT[data$SG_UF_NOT == 14] <- "Roraima"
    data$SG_UF_NOT[data$SG_UF_NOT == 15] <- "Par\u00e1"
    data$SG_UF_NOT[data$SG_UF_NOT == 16] <- "Amap\u00e1"
    data$SG_UF_NOT[data$SG_UF_NOT == 17] <- "Tocantins"
    data$SG_UF_NOT[data$SG_UF_NOT == 21] <- "Maranh\u00e3o"
    data$SG_UF_NOT[data$SG_UF_NOT == 22] <- "Piau\u00ed"
    data$SG_UF_NOT[data$SG_UF_NOT == 23] <- "Cear\u00e1"
    data$SG_UF_NOT[data$SG_UF_NOT == 24] <- "Rio Grande do Norte"
    data$SG_UF_NOT[data$SG_UF_NOT == 25] <- "Para\u00edba"
    data$SG_UF_NOT[data$SG_UF_NOT == 26] <- "Pernambuco"
    data$SG_UF_NOT[data$SG_UF_NOT == 27] <- "Alagoas"
    data$SG_UF_NOT[data$SG_UF_NOT == 28] <- "Sergipe"
    data$SG_UF_NOT[data$SG_UF_NOT == 29] <- "Bahia"
    data$SG_UF_NOT[data$SG_UF_NOT == 31] <- "Minas Gerais"
    data$SG_UF_NOT[data$SG_UF_NOT == 32] <- "Esp\u00edrito Santo"
    data$SG_UF_NOT[data$SG_UF_NOT == 33] <- "Rio de Janeiro"
    data$SG_UF_NOT[data$SG_UF_NOT == 35] <- "S\u00e3o Paulo"
    data$SG_UF_NOT[data$SG_UF_NOT == 41] <- "Paran\u00e1"
    data$SG_UF_NOT[data$SG_UF_NOT == 42] <- "Santa Catarina"
    data$SG_UF_NOT[data$SG_UF_NOT == 43] <- "Rio Grande do Sul"
    data$SG_UF_NOT[data$SG_UF_NOT == 50] <- "Mato Grosso do Sul"
    data$SG_UF_NOT[data$SG_UF_NOT == 51] <- "Mato Grosso"
    data$SG_UF_NOT[data$SG_UF_NOT == 52] <- "Goi\u00e1s"
    data$SG_UF_NOT[data$SG_UF_NOT == 53] <- "Distrito Federal"
    data$SG_UF_NOT <- factor(data$SG_UF_NOT)
  }

  # IDADE
  if ("NU_IDADE_N" %in% variables_names) {
    data$NU_IDADE_N[data$NU_IDADE_N == "000" | data$NU_IDADE_N == "999"] <- NA
    unidade <- substr(data$NU_IDADE_N, 1, 1)
    # Minutos
    data$IDADEminutos <-
      as.numeric(ifelse(unidade == 0, substr(data$IDADE, 2, 3), NA))
    # Horas
    data$IDADEhoras <-
      as.numeric(ifelse(unidade == 1, substr(data$NU_IDADE_N, 2, 3), NA))
    # Dias
    data$IDADEdias <-
      as.numeric(ifelse(unidade == 2, substr(data$NU_IDADE_N, 2, 3), NA))
    # Meses
    data$IDADEmeses <-
      as.numeric(ifelse(unidade == 3, substr(data$NU_IDADE_N, 2, 3), NA))
    # Anos
    data$IDADEanos <-
      as.numeric(ifelse(
        unidade == 4,
        substr(data$NU_IDADE_N, 2, 3),
        ifelse(unidade == 5, 100 + as.numeric(substr(data$NU_IDADE_N, 2, 3)), NA)
      ))
    # Apaga campo original
    data$NU_IDADE_N <- NULL
  }

  # CS_SEXO
  if ("CS_SEXO" %in% variables_names) {
    data$CS_SEXO <- as.character(data$CS_SEXO)
    data$CS_SEXO[data$CS_SEXO == "M"] <- "Masculino"
    data$CS_SEXO[data$CS_SEXO == "F"] <- "Feminino"
    data$CS_SEXO[data$CS_SEXO == "I"] <- "Ignorado"
    data$CS_SEXO <- factor(data$CS_SEXO)
  }

  # CS_GESTANT
  if ("CS_GESTANT" %in% variables_names) {
    data$CS_GESTANT[data$CS_GESTANT == "1"] <- "1o trimestre"
    data$CS_GESTANT[data$CS_GESTANT == "2"] <- "2o trimestre"
    data$CS_GESTANT[data$CS_GESTANT == "3"] <- "3o trimestre"
    data$CS_GESTANT[data$CS_GESTANT == "4"] <- "Idade gestacional ignorada"
    data$CS_GESTANT[data$CS_GESTANT == "5"] <- "N\u00e3o"
    data$CS_GESTANT[data$CS_GESTANT == "6"] <- "N\u00e3o se aplica"
    data$CS_GESTANT[data$CS_GESTANT == "9"] <- "Ignorado"
    data$CS_GESTANT <- factor(data$CS_GESTANT)
  }

  # CS_RACA
  if ("CS_RACA" %in% variables_names) {
    data$CS_RACA <- as.numeric(data$CS_RACA)
    data$CS_RACA[data$CS_RACA == 1] <- "Branca"
    data$CS_RACA[data$CS_RACA == 2] <- "Preta"
    data$CS_RACA[data$CS_RACA == 3] <- "Amarela"
    data$CS_RACA[data$CS_RACA == 4] <- "Parda"
    data$CS_RACA[data$CS_RACA == 5] <- "Ind\u00edgena"
    data$CS_RACA[data$CS_RACA == 9] <- "Ignorado"
    data$CS_RACA <- factor(data$CS_RACA)
  }

  # CS_ESCOL_N
  if ("CS_ESCOL_N" %in% variables_names) {
    data$CS_ESCOL_N <- as.numeric(data$CS_ESCOL_N)
    data$CS_ESCOL_N[data$CS_ESCOL_N == 1] <- "1a a 4a s\u00e9rie incompleta do EF"
    data$CS_ESCOL_N[data$CS_ESCOL_N == 2] <- "4a s\u00e9rie completa do EF (antigo 1o grau)"
    data$CS_ESCOL_N[data$CS_ESCOL_N == 3] <- "5a \u00e0 8a s\u00e9rie incompleta do EF (antigo gin\u00e1sio ou 1o grau)"
    data$CS_ESCOL_N[data$CS_ESCOL_N == 4] <- "Ensino fundamental completo (antigo gin\u00e1sio ou 1o grau)"
    data$CS_ESCOL_N[data$CS_ESCOL_N == 5] <- "Ensino m\u00e9dio incompleto (antigo colegial ou 2o grau)"
    data$CS_ESCOL_N[data$CS_ESCOL_N == 6] <- "Ensino m\u00e9dio completo (antigo colegial ou 2o grau)"
    data$CS_ESCOL_N[data$CS_ESCOL_N == 7] <- "Educa\u00e7\u00e3o superior incompleta"
    data$CS_ESCOL_N[data$CS_ESCOL_N == 8] <- "Educa\u00e7\u00e3o superior completa"
    data$CS_ESCOL_N[data$CS_ESCOL_N == 9] <- "Ignorado"
    data$CS_ESCOL_N[data$CS_ESCOL_N == 10] <- "N\u00e3o se aplica"
    data$CS_ESCOL_N <- factor(data$CS_ESCOL_N)
  }

  # SG_UF
  if ("SG_UF" %in% variables_names) {
    data$SG_UF <- as.numeric(data$SG_UF)
    data$SG_UF[data$SG_UF == 0] <- "Ignorado"
    data$SG_UF[data$SG_UF == 99] <- "Ignorado"
    data$SG_UF[data$SG_UF == 11] <- "Rond\u00f4nia"
    data$SG_UF[data$SG_UF == 12] <- "Acre"
    data$SG_UF[data$SG_UF == 13] <- "Amazonas"
    data$SG_UF[data$SG_UF == 14] <- "Roraima"
    data$SG_UF[data$SG_UF == 15] <- "Par\u00e1"
    data$SG_UF[data$SG_UF == 16] <- "Amap\u00e1"
    data$SG_UF[data$SG_UF == 17] <- "Tocantins"
    data$SG_UF[data$SG_UF == 21] <- "Maranh\u00e3o"
    data$SG_UF[data$SG_UF == 22] <- "Piau\u00ed"
    data$SG_UF[data$SG_UF == 23] <- "Cear\u00e1"
    data$SG_UF[data$SG_UF == 24] <- "Rio Grande do Norte"
    data$SG_UF[data$SG_UF == 25] <- "Para\u00edba"
    data$SG_UF[data$SG_UF == 26] <- "Pernambuco"
    data$SG_UF[data$SG_UF == 27] <- "Alagoas"
    data$SG_UF[data$SG_UF == 28] <- "Sergipe"
    data$SG_UF[data$SG_UF == 29] <- "Bahia"
    data$SG_UF[data$SG_UF == 31] <- "Minas Gerais"
    data$SG_UF[data$SG_UF == 32] <- "Esp\u00edrito Santo"
    data$SG_UF[data$SG_UF == 33] <- "Rio de Janeiro"
    data$SG_UF[data$SG_UF == 35] <- "S\u00e3o Paulo"
    data$SG_UF[data$SG_UF == 41] <- "Paran\u00e1"
    data$SG_UF[data$SG_UF == 42] <- "Santa Catarina"
    data$SG_UF[data$SG_UF == 43] <- "Rio Grande do Sul"
    data$SG_UF[data$SG_UF == 50] <- "Mato Grosso do Sul"
    data$SG_UF[data$SG_UF == 51] <- "Mato Grosso"
    data$SG_UF[data$SG_UF == 52] <- "Goi\u00e1s"
    data$SG_UF[data$SG_UF == 53] <- "Distrito Federal"
    data$SG_UF <- factor(data$SG_UF)
  }

  # ID_PAIS
  if ("ID_PAIS" %in% variables_names) {
    data$ID_PAIS <- dplyr::left_join(data, microdatasus::paisnet, by = c("ID_PAIS" = "ID_PAIS"))$NM_PAIS
  }

  # ID_OCUPA_N
  if ("ID_OCUPA_N" %in% variables_names) {
    data$ID_OCUPA_N <- factor(dplyr::left_join(data, microdatasus::tabCBO, by = c("ID_OCUPA_N" = "cod"))$nome)
  }

  # CLASSI_FIN
  if ("CLASSI_FIN" %in% variables_names) {
    data$CLASSI_FIN <- as.numeric(data$CLASSI_FIN)
    data$CLASSI_FIN[data$CLASSI_FIN == 5] <- "Descartado"
    data$CLASSI_FIN[data$CLASSI_FIN == 8] <- "Inconclusivo"
    data$CLASSI_FIN[data$CLASSI_FIN == 10] <- "Dengue"
    data$CLASSI_FIN[data$CLASSI_FIN == 11] <- "Dengue com sinais de alarme"
    data$CLASSI_FIN[data$CLASSI_FIN == 12] <- "Dengue grave"
    data$CLASSI_FIN[data$CLASSI_FIN == 13] <- "Chikungunya"
    data$CLASSI_FIN <- factor(data$CLASSI_FIN)
  }

  # CRITERIO
  if ("CRITERIO" %in% variables_names) {
    data$CRITERIO <- as.numeric(data$CRITERIO)
    data$CRITERIO[data$CRITERIO == 1] <- "Laborat\u00f3rio"
    data$CRITERIO[data$CRITERIO == 2] <- "Cl\u00ednico epidemiol\u00f3gico"
    data$CRITERIO[data$CRITERIO == 3] <- "Em investiga\u00e7\u00e3o"
    data$CRITERIO <- factor(data$CRITERIO)
  }

  # TPAUTOCTO
  if ("TPAUTOCTO" %in% variables_names) {
    data$TPAUTOCTO <- as.numeric(data$TPAUTOCTO)
    data$TPAUTOCTO[data$TPAUTOCTO == 1] <- "Sim"
    data$TPAUTOCTO[data$TPAUTOCTO == 2] <- "N\u00e3o"
    data$TPAUTOCTO[data$TPAUTOCTO == 3] <- "Indeterminado"
    data$TPAUTOCTO <- factor(data$TPAUTOCTO)
  }

  # COUFINF
  if ("COUFINF" %in% variables_names) {
    data$COUFINF <- as.numeric(data$COUFINF)
    data$COUFINF[data$COUFINF == 0] <- "Ignorado"
    data$COUFINF[data$COUFINF == 99] <- "Ignorado"
    data$COUFINF[data$COUFINF == 11] <- "Rond\u00f4nia"
    data$COUFINF[data$COUFINF == 12] <- "Acre"
    data$COUFINF[data$COUFINF == 13] <- "Amazonas"
    data$COUFINF[data$COUFINF == 14] <- "Roraima"
    data$COUFINF[data$COUFINF == 15] <- "Par\u00e1"
    data$COUFINF[data$COUFINF == 16] <- "Amap\u00e1"
    data$COUFINF[data$COUFINF == 17] <- "Tocantins"
    data$COUFINF[data$COUFINF == 21] <- "Maranh\u00e3o"
    data$COUFINF[data$COUFINF == 22] <- "Piau\u00ed"
    data$COUFINF[data$COUFINF == 23] <- "Cear\u00e1"
    data$COUFINF[data$COUFINF == 24] <- "Rio Grande do Norte"
    data$COUFINF[data$COUFINF == 25] <- "Para\u00edba"
    data$COUFINF[data$COUFINF == 26] <- "Pernambuco"
    data$COUFINF[data$COUFINF == 27] <- "Alagoas"
    data$COUFINF[data$COUFINF == 28] <- "Sergipe"
    data$COUFINF[data$COUFINF == 29] <- "Bahia"
    data$COUFINF[data$COUFINF == 31] <- "Minas Gerais"
    data$COUFINF[data$COUFINF == 32] <- "Esp\u00edrito Santo"
    data$COUFINF[data$COUFINF == 33] <- "Rio de Janeiro"
    data$COUFINF[data$COUFINF == 35] <- "S\u00e3o Paulo"
    data$COUFINF[data$COUFINF == 41] <- "Paran\u00e1"
    data$COUFINF[data$COUFINF == 42] <- "Santa Catarina"
    data$COUFINF[data$COUFINF == 43] <- "Rio Grande do Sul"
    data$COUFINF[data$COUFINF == 50] <- "Mato Grosso do Sul"
    data$COUFINF[data$COUFINF == 51] <- "Mato Grosso"
    data$COUFINF[data$COUFINF == 52] <- "Goi\u00e1s"
    data$COUFINF[data$COUFINF == 53] <- "Distrito Federal"
    data$COUFINF <- factor(data$COUFINF)
  }

  # COPAISINF
  if ("COPAISINF" %in% variables_names) {
    data$COPAISINF <- dplyr::left_join(data, microdatasus::paisnet, by = c("COPAISINF" = "COPAISINF"))$NM_PAIS
  }

  # DOENCA_TRA
  if ("DOENCA_TRA" %in% variables_names) {
    data$DOENCA_TRA <- as.numeric(data$DOENCA_TRA)
    data$DOENCA_TRA[data$DOENCA_TRA == 1] <- "Sim"
    data$DOENCA_TRA[data$DOENCA_TRA == 2] <- "N\u00e3o"
    data$DOENCA_TRA[data$DOENCA_TRA == 9] <- "Ignorado"
    data$DOENCA_TRA <- factor(data$DOENCA_TRA)
  }

  # EVOLUCAO
  if ("EVOLUCAO" %in% variables_names) {
    data$EVOLUCAO <- as.numeric(data$EVOLUCAO)
    data$EVOLUCAO[data$EVOLUCAO == 1] <- "Cura"
    data$EVOLUCAO[data$EVOLUCAO == 2] <- "\u00d3bito por dengue"
    data$EVOLUCAO[data$EVOLUCAO == 3] <- "\u00d3bito por outras causas"
    data$EVOLUCAO[data$EVOLUCAO == 4] <- "\u00d3bito em investiga\u00e7\u00e3o"
    data$EVOLUCAO[data$EVOLUCAO == 9] <- "Ignorado"
    data$EVOLUCAO <- factor(data$EVOLUCAO)
  }

  # DT_OBITO
  if ("DT_OBITO" %in% variables_names) {
    data$DT_OBITO <- as.Date(data$DT_OBITO)
  }

  # DT_ENCERRA
  if ("DT_ENCERRA" %in% variables_names) {
    data$DT_ENCERRA <- as.Date(data$DT_ENCERRA)
  }

  # CS_FLXRET
  if ("CS_FLXRET" %in% variables_names) {
    data$CS_FLXRET <- as.numeric(data$CS_FLXRET)
    data$CS_FLXRET[data$CS_FLXRET == 0] <- "N\u00e3o"
    data$CS_FLXRET[data$CS_FLXRET == 1] <- "Habilitado para envio"
    data$CS_FLXRET[data$CS_FLXRET == 2] <- "Enviado"
    data$CS_FLXRET <- factor(data$CS_FLXRET)
  }

  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  data <- as.data.frame(lapply(X = data, FUN = stringi::stri_unescape_unicode))

  # Return
  return(data)
}
