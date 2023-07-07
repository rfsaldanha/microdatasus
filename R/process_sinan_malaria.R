#' Process SINAN Malaria variables from DataSUS
#'
#' \code{process_sinan_malaria} processes SINAN Malaria variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SINAN Malaria variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples \dontrun{
#' df <- fetch_datasus(year_start = 2016, year_end = 2016,
#' uf = "RJ", information_system = "SINAN-MALARIA-FINAL")
#' df_a <- process_sinan_malaria(df)
#' df_b <- process_sinan_malaria(df, municipality_data = FALSE)
#' }
#' @export

process_sinan_malaria <- function(data, municipality_data = TRUE){
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
    data$SG_UF_NOT[data$SG_UF_NOT == 17] <- "Tocantis"
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
    data$CS_SEXO[data$CS_SEXO == "M"] <- "Masculino"
    data$CS_SEXO[data$CS_SEXO == "F"] <- "Feminino"
    data$CS_SEXO[data$CS_SEXO == "I"] <- "Ignorado"
    data$CS_SEXO <- factor(data$CS_SEXO)
  }

  # CS_GESTANT
  if ("CS_GESTANT" %in% variables_names) {
    data$CS_GESTANT <- as.numeric(data$CS_GESTANT)
    data$CS_GESTANT[data$CS_GESTANT == 1] <- "1o trimestre"
    data$CS_GESTANT[data$CS_GESTANT == 2] <- "2o trimestre"
    data$CS_GESTANT[data$CS_GESTANT == 3] <- "3o trimestre"
    data$CS_GESTANT[data$CS_GESTANT == 4] <- "Idade gestacional ignorada"
    data$CS_GESTANT[data$CS_GESTANT == 5] <- "N\u00e3o"
    data$CS_GESTANT[data$CS_GESTANT == 6] <- "N\u00e3o se aplica"
    data$CS_GESTANT[data$CS_GESTANT == 9] <- "Ignorado"
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
    data$SG_UF[data$SG_UF == 17] <- "Tocantis"
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
    data$CLASSI_FIN[data$CLASSI_FIN == 1] <- "Confirmado"
    data$CLASSI_FIN[data$CLASSI_FIN == 2] <- "Descartado"
    data$CLASSI_FIN <- factor(data$CLASSI_FIN)
  }

  # AT_ATIVIDA
  if ("AT_ATIVIDA" %in% variables_names) {
    data$AT_ATIVIDA <- as.numeric(data$AT_ATIVIDA)
    data$AT_ATIVIDA[data$AT_ATIVIDA == 1] <- "Agricultura"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 2] <- "Pecu\u00e1ria"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 3] <- "Dom\u00e9stica"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 4] <- "Turismo"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 5] <- "Garimpagem"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 6] <- "Explora\u00e7\u00e3o vegetal"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 7] <- "Ca\u00e7a/Pesca"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 8] <- "Construtor de estradas/barragens"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 9] <- "Minera\u00e7\u00e3o"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 10] <- "Viajante"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 11] <- "Outros"
    data$AT_ATIVIDA[data$AT_ATIVIDA == 99] <- "Ignorado"
    data$AT_ATIVIDA <- factor(data$AT_ATIVIDA)
  }

  # AT_LAMINA
  if ("AT_LAMINA" %in% variables_names) {
    data$AT_LAMINA <- as.numeric(data$AT_LAMINA)
    data$AT_LAMINA[data$AT_LAMINA == 1] <- "BP"
    data$AT_LAMINA[data$AT_LAMINA == 2] <- "BA"
    data$AT_LAMINA[data$AT_LAMINA == 3] <- "LVC"
    data$AT_LAMINA <- factor(data$AT_LAMINA)
  }

  # AT_SINTOMA
  if ("AT_SINTOMA" %in% variables_names) {
    data$AT_SINTOMA <- as.numeric(data$AT_SINTOMA)
    data$AT_SINTOMA[data$AT_SINTOMA == 1] <- "Com sintomas"
    data$AT_SINTOMA[data$AT_SINTOMA == 2] <- "Sem sintomas"
    data$AT_SINTOMA <- factor(data$AT_SINTOMA)
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
    data$COUFINF[data$COUFINF == 17] <- "Tocantis"
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

  # RESULT
  if ("RESULT" %in% variables_names) {
    data$RESULT <- as.numeric(data$RESULT)
    data$RESULT[data$RESULT == 1] <- "Negativo"
    data$RESULT[data$RESULT == 2] <- "F"
    data$RESULT[data$RESULT == 3] <- "F + FG"
    data$RESULT[data$RESULT == 4] <- "V"
    data$RESULT[data$RESULT == 5] <- "F + V"
    data$RESULT[data$RESULT == 6] <- "V + FG"
    data$RESULT[data$RESULT == 7] <- "FG"
    data$RESULT[data$RESULT == 8] <- "M"
    data$RESULT[data$RESULT == 9] <- "F + M"
    data$RESULT[data$RESULT == 10] <- "O"
    data$RESULT <- factor(data$RESULT)
  }

  # PCRUZ
  if ("PCRUZ" %in% variables_names) {
    data$PCRUZ <- as.numeric(data$PCRUZ)
    data$PCRUZ[data$PCRUZ == 1] <- "Menor que meia cruz"
    data$PCRUZ[data$PCRUZ == 2] <- "Meia cruz"
    data$PCRUZ[data$PCRUZ == 3] <- "Uma cruz"
    data$PCRUZ[data$PCRUZ == 4] <- "Duas cruzes"
    data$PCRUZ[data$PCRUZ == 5] <- "Tr\u00eas cruzes"
    data$PCRUZ[data$PCRUZ == 6] <- "Quatro cruzes"
    data$PCRUZ <- factor(data$PCRUZ)
  }

  # TRA_ESQUEM
  if ("TRA_ESQUEM" %in% variables_names) {
    data$TRA_ESQUEM <- as.numeric(data$TRA_ESQUEM)
    data$TRA_ESQUEM[data$TRA_ESQUEM == 1] <- "Infec\u00e7\u00f5es por Pv com Cloroquina em 3 dias e Primaquina em 7 dias"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 2] <- "Infec\u00e7\u00f5es por Pf com Quinina em 3 dias + Doxiciclina em 5 dias + primaquina no 6o dia"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 3] <- "Infec\u00e7\u00f5es mistas por Pv + Pf com Mefloquina em dose \u00fanica e primaquina em 7 dias"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 4] <- "Infec\u00e7\u00f5es por Pm com cloroquina em 3 dias"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 5] <- "Infec\u00e7\u00f5es por Pv em crian\u00e7as apresentando v\u00f4mitos, com c\u00e1psulas retais de artesunato em 4 dias e Primaquina em 7 dias"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 6] <- "Infec\u00e7\u00f5es por Pf com Mefloquina em dose \u00fanica e primaquina no segundo dia"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 7] <- "Infec\u00e7\u00f5es por Pf com Quinina em 7 dias"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 8] <- "Infec\u00e7\u00f5es por Pf de crian\u00e7as com c\u00e1psulas retais de artesunato em 4 dias e dose \u00fanica de Mefloquina no 3o dia e Primaquina no 5o dia"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 9] <- "Infec\u00e7\u00f5es mistas por Pv + Pf com Quinina em 3 dias, doxiciclina em 5 dias e Primaquina em 7 dias"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 10] <- "Preven\u00e7\u00e3o de reca\u00edda da mal\u00e1ria por Pv com Cloroquina em dose \u00fanica semanal durante 3 meses"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 11] <- "Mal\u00e1ria grave e complicada"
    data$TRA_ESQUEM[data$TRA_ESQUEM == 99] <- "Outro esquema utilizado (por m\u00e9dico)"
    data$TRA_ESQUEM <- factor(data$TRA_ESQUEM)
  }

  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  data <- as.data.frame(lapply(X = data, FUN = stringi::stri_unescape_unicode))

  # Return
  return(data)
}
