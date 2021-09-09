#' Process SINAN Dengue variables from DataSUS
#'
#' \code{process_sinan_dengue} processes SINAN Dengue variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SINAN Dengue variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples \dontrun{
#' df <- process_sinan_dengue(year_start = 2016, year_end = 2016, uf = "RJ")
#' df_a <- process_sinan_dengue(df)
#' df_b <- process_sinan_dengue(df, municipality_data = FALSE)
#' }
#' @export

process_sinan_dengue <- function(data, municipality_data = TRUE){
  # Variables names
  variables_names <- names(data)

  # Declara objetos
  ano <- NULL
  unidade <- NULL

  # TP_NOT
  if ("TP_NOT" %in% variables_names) {
    data$TP_NOT <- as.numeric(levels(data$TP_NOT))[data$TP_NOT]
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
    data$SG_UF_NOT <- as.numeric(levels(data$SG_UF_NOT))[data$SG_UF_NOT]
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
    data$NU_IDADE_N <- as.character(data$NU_IDADE_N)
    data$NU_IDADE_N[data$NU_IDADE_N == "000" | data$NU_IDADE_N == "999"] <- NA
    unidade <- substr(data$NU_IDADE_N, 1, 1)
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
    data$CS_SEXO <- as.character(levels(data$CS_SEXO))[data$CS_SEXO]
    data$CS_SEXO[data$CS_SEXO == "M"] <- "Masculino"
    data$CS_SEXO[data$CS_SEXO == "F"] <- "Feminino"
    data$CS_SEXO[data$CS_SEXO == "I"] <- "Ignorado"
    data$CS_SEXO <- factor(data$CS_SEXO)
  }

  # CS_GESTANT
  if ("CS_GESTANT" %in% variables_names) {
    data$CS_GESTANT <- as.numeric(levels(data$CS_GESTANT))[data$CS_GESTANT]
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
    data$CS_RACA <- as.numeric(levels(data$CS_RACA))[data$CS_RACA]
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
    data$CS_ESCOL_N <- as.numeric(levels(data$CS_ESCOL_N))[data$CS_ESCOL_N]
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
    data$SG_UF <- as.numeric(levels(data$SG_UF))[data$SG_UF]
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
    data$ID_PAIS <- as.character(levels(data$ID_PAIS))[data$ID_PAIS]
    data$ID_PAIS <- dplyr::left_join(data, microdatasus::paisnet, by = c("ID_PAIS" = "ID_PAIS"))$NM_PAIS
  }

  # ID_OCUPA_N
  if ("ID_OCUPA_N" %in% variables_names) {
    data$ID_OCUPA_N <- as.character(levels(data$ID_OCUPA_N))[data$ID_OCUPA_N]
    data$ID_OCUPA_N <- factor(dplyr::left_join(data, microdatasus::tabCBO, by = c("ID_OCUPA_N" = "cod"))$nome)
  }

  # FEBRE
  if ("FEBRE" %in% variables_names) {
    data$FEBRE <- as.numeric(levels(data$FEBRE))[data$FEBRE]
    data$FEBRE[data$FEBRE == 1] <- "Sim"
    data$FEBRE[data$FEBRE == 2] <- "N\u00e3o"
    data$FEBRE <- factor(data$FEBRE)
  }

  # MIALGIA
  if ("MIALGIA" %in% variables_names) {
    data$MIALGIA <- as.numeric(levels(data$MIALGIA))[data$MIALGIA]
    data$MIALGIA[data$MIALGIA == 1] <- "Sim"
    data$MIALGIA[data$MIALGIA == 2] <- "N\u00e3o"
    data$MIALGIA <- factor(data$MIALGIA)
  }

  # CEFALEIA
  if ("CEFALEIA" %in% variables_names) {
    data$CEFALEIA <- as.numeric(levels(data$CEFALEIA))[data$CEFALEIA]
    data$CEFALEIA[data$CEFALEIA == 1] <- "Sim"
    data$CEFALEIA[data$CEFALEIA == 2] <- "N\u00e3o"
    data$CEFALEIA <- factor(data$CEFALEIA)
  }

  # EXANTEMA
  if ("EXANTEMA" %in% variables_names) {
    data$EXANTEMA <- as.numeric(levels(data$EXANTEMA))[data$EXANTEMA]
    data$EXANTEMA[data$EXANTEMA == 1] <- "Sim"
    data$EXANTEMA[data$EXANTEMA == 2] <- "N\u00e3o"
    data$EXANTEMA <- factor(data$EXANTEMA)
  }

  # VOMITO
  if ("VOMITO" %in% variables_names) {
    data$VOMITO <- as.numeric(levels(data$VOMITO))[data$VOMITO]
    data$VOMITO[data$VOMITO == 1] <- "Sim"
    data$VOMITO[data$VOMITO == 2] <- "N\u00e3o"
    data$VOMITO <- factor(data$VOMITO)
  }

  # NAUSEA
  if ("NAUSEA" %in% variables_names) {
    data$NAUSEA <- as.numeric(levels(data$NAUSEA))[data$NAUSEA]
    data$NAUSEA[data$NAUSEA == 1] <- "Sim"
    data$NAUSEA[data$NAUSEA == 2] <- "N\u00e3o"
    data$NAUSEA <- factor(data$NAUSEA)
  }

  # DOR_COSTAS
  if ("DOR_COSTAS" %in% variables_names) {
    data$DOR_COSTAS <- as.numeric(levels(data$DOR_COSTAS))[data$DOR_COSTAS]
    data$DOR_COSTAS[data$DOR_COSTAS == 1] <- "Sim"
    data$DOR_COSTAS[data$DOR_COSTAS == 2] <- "N\u00e3o"
    data$DOR_COSTAS <- factor(data$DOR_COSTAS)
  }

  # CONJUNTVIT
  if ("CONJUNTVIT" %in% variables_names) {
    data$CONJUNTVIT <- as.numeric(levels(data$CONJUNTVIT))[data$CONJUNTVIT]
    data$CONJUNTVIT[data$CONJUNTVIT == 1] <- "Sim"
    data$CONJUNTVIT[data$CONJUNTVIT == 2] <- "N\u00e3o"
    data$CONJUNTVIT <- factor(data$CONJUNTVIT)
  }

  # ARTRITE
  if ("ARTRITE" %in% variables_names) {
    data$ARTRITE <- as.numeric(levels(data$ARTRITE))[data$ARTRITE]
    data$ARTRITE[data$ARTRITE == 1] <- "Sim"
    data$ARTRITE[data$ARTRITE == 2] <- "N\u00e3o"
    data$ARTRITE <- factor(data$ARTRITE)
  }

  # ARTRALGIA
  if ("ARTRALGIA" %in% variables_names) {
    data$ARTRALGIA <- as.numeric(levels(data$ARTRALGIA))[data$ARTRALGIA]
    data$ARTRALGIA[data$ARTRALGIA == 1] <- "Sim"
    data$ARTRALGIA[data$ARTRALGIA == 2] <- "N\u00e3o"
    data$ARTRALGIA <- factor(data$ARTRALGIA)
  }

  # PETEQUIA_N
  if ("PETEQUIA_N" %in% variables_names) {
    data$PETEQUIA_N <- as.numeric(levels(data$PETEQUIA_N))[data$PETEQUIA_N]
    data$PETEQUIA_N[data$PETEQUIA_N == 1] <- "Sim"
    data$PETEQUIA_N[data$PETEQUIA_N == 2] <- "N\u00e3o"
    data$PETEQUIA_N <- factor(data$PETEQUIA_N)
  }

  # LEUCOPENIA
  if ("LEUCOPENIA" %in% variables_names) {
    data$LEUCOPENIA <- as.numeric(levels(data$LEUCOPENIA))[data$LEUCOPENIA]
    data$LEUCOPENIA[data$LEUCOPENIA == 1] <- "Sim"
    data$LEUCOPENIA[data$LEUCOPENIA == 2] <- "N\u00e3o"
    data$LEUCOPENIA <- factor(data$LEUCOPENIA)
  }

  # LACO
  if ("LACO" %in% variables_names) {
    data$LACO <- as.numeric(levels(data$LACO))[data$LACO]
    data$LACO[data$LACO == 1] <- "Sim"
    data$LACO[data$LACO == 2] <- "N\u00e3o"
    data$LACO <- factor(data$LACO)
  }

  # DOR_RETRO
  if ("DOR_RETRO" %in% variables_names) {
    data$DOR_RETRO <- as.numeric(levels(data$DOR_RETRO))[data$DOR_RETRO]
    data$DOR_RETRO[data$DOR_RETRO == 1] <- "Sim"
    data$DOR_RETRO[data$DOR_RETRO == 2] <- "N\u00e3o"
    data$DOR_RETRO <- factor(data$DOR_RETRO)
  }

  # DIABETES
  if ("DIABETES" %in% variables_names) {
    data$DIABETES <- as.numeric(levels(data$DIABETES))[data$DIABETES]
    data$DIABETES[data$DIABETES == 1] <- "Sim"
    data$DIABETES[data$DIABETES == 2] <- "N\u00e3o"
    data$DIABETES <- factor(data$DIABETES)
  }

  # HEMATOLOG
  if ("HEMATOLOG" %in% variables_names) {
    data$HEMATOLOG <- as.numeric(levels(data$HEMATOLOG))[data$HEMATOLOG]
    data$HEMATOLOG[data$HEMATOLOG == 1] <- "Sim"
    data$HEMATOLOG[data$HEMATOLOG == 2] <- "N\u00e3o"
    data$HEMATOLOG <- factor(data$HEMATOLOG)
  }

  # HEPATOPAT
  if ("HEPATOPAT" %in% variables_names) {
    data$HEPATOPAT <- as.numeric(levels(data$HEPATOPAT))[data$HEPATOPAT]
    data$HEPATOPAT[data$HEPATOPAT == 1] <- "Sim"
    data$HEPATOPAT[data$HEPATOPAT == 2] <- "N\u00e3o"
    data$HEPATOPAT <- factor(data$HEPATOPAT)
  }

  # RENAL
  if ("RENAL" %in% variables_names) {
    data$RENAL <- as.numeric(levels(data$RENAL))[data$RENAL]
    data$RENAL[data$RENAL == 1] <- "Sim"
    data$RENAL[data$RENAL == 2] <- "N\u00e3o"
    data$RENAL <- factor(data$RENAL)
  }

  # HIPERTENSA
  if ("HIPERTENSA" %in% variables_names) {
    data$HIPERTENSA <- as.numeric(levels(data$HIPERTENSA))[data$HIPERTENSA]
    data$HIPERTENSA[data$HIPERTENSA == 1] <- "Sim"
    data$HIPERTENSA[data$HIPERTENSA == 2] <- "N\u00e3o"
    data$HIPERTENSA <- factor(data$HIPERTENSA)
  }

  # ACIDO_PEPT
  if ("ACIDO_PEPT" %in% variables_names) {
    data$ACIDO_PEPT <- as.numeric(levels(data$ACIDO_PEPT))[data$ACIDO_PEPT]
    data$ACIDO_PEPT[data$ACIDO_PEPT == 1] <- "Sim"
    data$ACIDO_PEPT[data$ACIDO_PEPT == 2] <- "N\u00e3o"
    data$ACIDO_PEPT <- factor(data$ACIDO_PEPT)
  }

  # AUTO_IMUNE
  if ("AUTO_IMUNE" %in% variables_names) {
    data$AUTO_IMUNE <- as.numeric(levels(data$AUTO_IMUNE))[data$AUTO_IMUNE]
    data$AUTO_IMUNE[data$AUTO_IMUNE == 1] <- "Sim"
    data$AUTO_IMUNE[data$AUTO_IMUNE == 2] <- "N\u00e3o"
    data$AUTO_IMUNE <- factor(data$AUTO_IMUNE)
  }

  # DT_CHIK_S1
  if ("DT_CHIK_S1" %in% variables_names) {
    data$DT_CHIK_S1 <- as.Date(data$DT_CHIK_S1)
  }

  # DT_CHIK_S2
  if ("DT_CHIK_S2" %in% variables_names) {
    data$DT_CHIK_S2 <- as.Date(data$DT_CHIK_S2)
  }

  # DT_PRNT
  if ("DT_PRNT" %in% variables_names) {
    data$DT_PRNT <- as.Date(data$DT_PRNT)
  }

  # RES_CHIKS1
  if ("RES_CHIKS1" %in% variables_names) {
    data$RES_CHIKS1 <- as.numeric(levels(data$RES_CHIKS1))[data$RES_CHIKS1]
    data$RES_CHIKS1[data$RES_CHIKS1 == 1] <- "Reagente"
    data$RES_CHIKS1[data$RES_CHIKS1 == 2] <- "N\u00e3o reagente"
    data$RES_CHIKS1[data$RES_CHIKS1 == 3] <- "Inconclusivo"
    data$RES_CHIKS1[data$RES_CHIKS1 == 4] <- "N\u00e3o realizado"
    data$RES_CHIKS1 <- factor(data$RES_CHIKS1)
  }

  # RES_CHIKS2
  if ("RES_CHIKS2" %in% variables_names) {
    data$RES_CHIKS2 <- as.numeric(levels(data$RES_CHIKS2))[data$RES_CHIKS2]
    data$RES_CHIKS2[data$RES_CHIKS2 == 1] <- "Reagente"
    data$RES_CHIKS2[data$RES_CHIKS2 == 2] <- "N\u00e3o reagente"
    data$RES_CHIKS2[data$RES_CHIKS2 == 3] <- "Inconclusivo"
    data$RES_CHIKS2[data$RES_CHIKS2 == 4] <- "N\u00e3o realizado"
    data$RES_CHIKS2 <- factor(data$RES_CHIKS2)
  }

  # RESUL_PRNT
  if ("RESUL_PRNT" %in% variables_names) {
    data$RESUL_PRNT <- as.numeric(levels(data$RESUL_PRNT))[data$RESUL_PRNT]
    data$RESUL_PRNT[data$RESUL_PRNT == 1] <- "Reagente"
    data$RESUL_PRNT[data$RESUL_PRNT == 2] <- "N\u00e3o reagente"
    data$RESUL_PRNT[data$RESUL_PRNT == 3] <- "Inconclusivo"
    data$RESUL_PRNT[data$RESUL_PRNT == 4] <- "N\u00e3o realizado"
    data$RESUL_PRNT <- factor(data$RESUL_PRNT)
  }

  # DT_SORO
  if ("DT_SORO" %in% variables_names) {
    data$DT_SORO <- as.Date(data$DT_SORO)
  }

  # RESUL_SORO
  if ("RESUL_SORO" %in% variables_names) {
    data$RESUL_SORO <- as.numeric(levels(data$RESUL_SORO))[data$RESUL_SORO]
    data$RESUL_SORO[data$RESUL_SORO == 1] <- "Reagente"
    data$RESUL_SORO[data$RESUL_SORO == 2] <- "N\u00e3o reagente"
    data$RESUL_SORO[data$RESUL_SORO == 3] <- "Inconclusivo"
    data$RESUL_SORO[data$RESUL_SORO == 4] <- "N\u00e3o realizado"
    data$RESUL_SORO <- factor(data$RESUL_SORO)
  }

  # DT_NS1
  if ("DT_NS1" %in% variables_names) {
    data$DT_NS1 <- as.Date(data$DT_NS1)
  }

  # RESUL_NS1
  if ("RESUL_NS1" %in% variables_names) {
    data$RESUL_NS1 <- as.numeric(levels(data$RESUL_NS1))[data$RESUL_NS1]
    data$RESUL_NS1[data$RESUL_NS1 == 1] <- "Positivo"
    data$RESUL_NS1[data$RESUL_NS1 == 2] <- "Negativo"
    data$RESUL_NS1[data$RESUL_NS1 == 3] <- "Inconclusivo"
    data$RESUL_NS1[data$RESUL_NS1 == 4] <- "N\u00e3o realizado"
    data$RESUL_NS1 <- factor(data$RESUL_NS1)
  }

  # DT_VIRAL
  if ("DT_VIRAL" %in% variables_names) {
    data$DT_VIRAL <- as.Date(data$DT_VIRAL)
  }

  # RESUL_VI_N
  if ("RESUL_VI_N" %in% variables_names) {
    data$RESUL_VI_N <- as.numeric(levels(data$RESUL_VI_N))[data$RESUL_VI_N]
    data$RESUL_VI_N[data$RESUL_VI_N == 1] <- "Positivo"
    data$RESUL_VI_N[data$RESUL_VI_N == 2] <- "Negativo"
    data$RESUL_VI_N[data$RESUL_VI_N == 3] <- "Inconclusivo"
    data$RESUL_VI_N[data$RESUL_VI_N == 4] <- "N\u00e3o realizado"
    data$RESUL_VI_N <- factor(data$RESUL_VI_N)
  }

  # DT_PCR
  if ("DT_PCR" %in% variables_names) {
    data$DT_PCR <- as.Date(data$DT_PCR)
  }

  # RESUL_PCR_
  if ("RESUL_PCR_" %in% variables_names) {
    data$RESUL_PCR_ <- as.numeric(levels(data$RESUL_PCR_))[data$RESUL_PCR_]
    data$RESUL_PCR_[data$RESUL_PCR_ == 1] <- "Positivo"
    data$RESUL_PCR_[data$RESUL_PCR_ == 2] <- "Negativo"
    data$RESUL_PCR_[data$RESUL_PCR_ == 3] <- "Inconclusivo"
    data$RESUL_PCR_[data$RESUL_PCR_ == 4] <- "N\u00e3o realizado"
    data$RESUL_PCR_ <- factor(data$RESUL_PCR_)
  }

  # SOROTIPO
  if ("SOROTIPO" %in% variables_names) {
    data$SOROTIPO <- as.numeric(levels(data$SOROTIPO))[data$SOROTIPO]
    data$SOROTIPO[data$SOROTIPO == 1] <- "DEN 1"
    data$SOROTIPO[data$SOROTIPO == 2] <- "DEN 2"
    data$SOROTIPO[data$SOROTIPO == 3] <- "DEN 3"
    data$SOROTIPO[data$SOROTIPO == 4] <- "DEN 4"
    data$SOROTIPO <- factor(data$SOROTIPO)
  }

  # HISTOPA_N
  if ("HISTOPA_N" %in% variables_names) {
    data$HISTOPA_N <- as.numeric(levels(data$HISTOPA_N))[data$HISTOPA_N]
    data$HISTOPA_N[data$HISTOPA_N == 1] <- "Positivo"
    data$HISTOPA_N[data$HISTOPA_N == 2] <- "Negativo"
    data$HISTOPA_N[data$HISTOPA_N == 3] <- "Inconclusivo"
    data$HISTOPA_N[data$HISTOPA_N == 4] <- "N\u00e3o realizado"
    data$HISTOPA_N <- factor(data$HISTOPA_N)
  }

  # IMUNOH_N
  if ("IMUNOH_N" %in% variables_names) {
    data$IMUNOH_N <- as.numeric(levels(data$IMUNOH_N))[data$IMUNOH_N]
    data$IMUNOH_N[data$IMUNOH_N == 1] <- "Positivo"
    data$IMUNOH_N[data$IMUNOH_N == 2] <- "Negativo"
    data$IMUNOH_N[data$IMUNOH_N == 3] <- "Inconclusivo"
    data$IMUNOH_N[data$IMUNOH_N == 4] <- "N\u00e3o realizado"
    data$IMUNOH_N <- factor(data$IMUNOH_N)
  }

  # HOSPITALIZ
  if ("HOSPITALIZ" %in% variables_names) {
    data$HOSPITALIZ <- as.numeric(levels(data$HOSPITALIZ))[data$HOSPITALIZ]
    data$HOSPITALIZ[data$HOSPITALIZ == 1] <- "Sim"
    data$HOSPITALIZ[data$HOSPITALIZ == 2] <- "N\u00e3o"
    data$HOSPITALIZ[data$HOSPITALIZ == 9] <- "Ignorado"
    data$HOSPITALIZ <- factor(data$HOSPITALIZ)
  }

  # DT_INTERNA
  if ("DT_INTERNA" %in% variables_names) {
    data$DT_INTERNA <- as.Date(data$DT_INTERNA)
  }

  # DT_INTERNA
  if ("DT_INTERNA" %in% variables_names) {
    data$DT_INTERNA <- as.Date(data$DT_INTERNA)
  }

  # UF
  if ("UF" %in% variables_names) {
    data$UF <- as.numeric(levels(data$UF))[data$UF]
    data$UF[data$UF == 0] <- "Ignorado"
    data$UF[data$UF == 99] <- "Ignorado"
    data$UF[data$UF == 11] <- "Rond\u00f4nia"
    data$UF[data$UF == 12] <- "Acre"
    data$UF[data$UF == 13] <- "Amazonas"
    data$UF[data$UF == 14] <- "Roraima"
    data$UF[data$UF == 15] <- "Par\u00e1"
    data$UF[data$UF == 16] <- "Amap\u00e1"
    data$UF[data$UF == 17] <- "Tocantis"
    data$UF[data$UF == 21] <- "Maranh\u00e3o"
    data$UF[data$UF == 22] <- "Piau\u00ed"
    data$UF[data$UF == 23] <- "Cear\u00e1"
    data$UF[data$UF == 24] <- "Rio Grande do Norte"
    data$UF[data$UF == 25] <- "Para\u00edba"
    data$UF[data$UF == 26] <- "Pernambuco"
    data$UF[data$UF == 27] <- "Alagoas"
    data$UF[data$UF == 28] <- "Sergipe"
    data$UF[data$UF == 29] <- "Bahia"
    data$UF[data$UF == 31] <- "Minas Gerais"
    data$UF[data$UF == 32] <- "Esp\u00edrito Santo"
    data$UF[data$UF == 33] <- "Rio de Janeiro"
    data$UF[data$UF == 35] <- "S\u00e3o Paulo"
    data$UF[data$UF == 41] <- "Paran\u00e1"
    data$UF[data$UF == 42] <- "Santa Catarina"
    data$UF[data$UF == 43] <- "Rio Grande do Sul"
    data$UF[data$UF == 50] <- "Mato Grosso do Sul"
    data$UF[data$UF == 51] <- "Mato Grosso"
    data$UF[data$UF == 52] <- "Goi\u00e1s"
    data$UF[data$UF == 53] <- "Distrito Federal"
    data$UF <- factor(data$UF)
  }

  # MUNICIPIO
  if("MUNICIPIO" %in% variables_names & municipality_data == TRUE){
    data$MUNICIPIO <- as.numeric(levels(data$MUNICIPIO))[data$MUNICIPIO]
    data <- dplyr::left_join(data, microdatasus::tabMun, by = c("MUNICIPIO" = "munResCod"))
  } else {
    data$MUNICIPIO <- as.numeric(levels(data$MUNICIPIO))[data$MUNICIPIO]
  }

  # TPAUTOCTO
  if ("TPAUTOCTO" %in% variables_names) {
    data$TPAUTOCTO <- as.numeric(levels(data$TPAUTOCTO))[data$TPAUTOCTO]
    data$TPAUTOCTO[data$TPAUTOCTO == 1] <- "Sim"
    data$TPAUTOCTO[data$TPAUTOCTO == 2] <- "N\u00e3o"
    data$TPAUTOCTO[data$TPAUTOCTO == 3] <- "Indeterminado"
    data$TPAUTOCTO <- factor(data$TPAUTOCTO)
  }

  # COUFINF
  if ("COUFINF" %in% variables_names) {
    data$COUFINF <- as.numeric(levels(data$COUFINF))[data$COUFINF]
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
    data$COPAISINF <- as.character(levels(data$COPAISINF))[data$COPAISINF]
    data$COPAISINF <- dplyr::left_join(data, microdatasus::paisnet, by = c("COPAISINF" = "COPAISINF"))$NM_PAIS
  }

  # CLASSI_FIN
  if ("CLASSI_FIN" %in% variables_names) {
    data$CLASSI_FIN <- as.numeric(levels(data$CLASSI_FIN))[data$CLASSI_FIN]
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
    data$CRITERIO <- as.numeric(levels(data$CRITERIO))[data$CRITERIO]
    data$CRITERIO[data$CRITERIO == 1] <- "Laborat\u00f3rio"
    data$CRITERIO[data$CRITERIO == 2] <- "Cl\u00ednico epidemiol\u00f3gico"
    data$CRITERIO[data$CRITERIO == 3] <- "Em investiga\u00e7\u00e3o"
    data$CRITERIO <- factor(data$CRITERIO)
  }

  # DOENCA_TRA
  if ("DOENCA_TRA" %in% variables_names) {
    data$DOENCA_TRA <- as.numeric(levels(data$DOENCA_TRA))[data$DOENCA_TRA]
    data$DOENCA_TRA[data$DOENCA_TRA == 1] <- "Sim"
    data$DOENCA_TRA[data$DOENCA_TRA == 2] <- "N\u00e3o"
    data$DOENCA_TRA[data$DOENCA_TRA == 9] <- "Ignorado"
    data$DOENCA_TRA <- factor(data$DOENCA_TRA)
  }

  # CLINC_CHIK
  if ("CLINC_CHIK" %in% variables_names) {
    data$CLINC_CHIK <- as.numeric(levels(data$CLINC_CHIK))[data$CLINC_CHIK]
    data$CLINC_CHIK[data$CLINC_CHIK == 1] <- "Aguga"
    data$CLINC_CHIK[data$CLINC_CHIK == 2] <- "Cr\u00f4nica"
    data$CLINC_CHIK <- factor(data$CLINC_CHIK)
  }

  # EVOLUCAO
  if ("EVOLUCAO" %in% variables_names) {
    data$EVOLUCAO <- as.numeric(levels(data$EVOLUCAO))[data$EVOLUCAO]
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

  # ALRM_HIPOT
  if ("ALRM_HIPOT" %in% variables_names) {
    data$ALRM_HIPOT <- as.numeric(levels(data$ALRM_HIPOT))[data$ALRM_HIPOT]
    data$ALRM_HIPOT[data$ALRM_HIPOT == 1] <- "Sim"
    data$ALRM_HIPOT[data$ALRM_HIPOT == 2] <- "N\u00e3o"
    data$ALRM_HIPOT <- factor(data$ALRM_HIPOT)
  }

  # ALRM_PLAQ
  if ("ALRM_PLAQ" %in% variables_names) {
    data$ALRM_PLAQ <- as.numeric(levels(data$ALRM_PLAQ))[data$ALRM_PLAQ]
    data$ALRM_PLAQ[data$ALRM_PLAQ == 1] <- "Sim"
    data$ALRM_PLAQ[data$ALRM_PLAQ == 2] <- "N\u00e3o"
    data$ALRM_PLAQ <- factor(data$ALRM_PLAQ)
  }

  # ALRM_VOM
  if ("ALRM_VOM" %in% variables_names) {
    data$ALRM_VOM <- as.numeric(levels(data$ALRM_VOM))[data$ALRM_VOM]
    data$ALRM_VOM[data$ALRM_VOM == 1] <- "Sim"
    data$ALRM_VOM[data$ALRM_VOM == 2] <- "N\u00e3o"
    data$ALRM_VOM <- factor(data$ALRM_VOM)
  }

  # ALRM_SANG
  if ("ALRM_SANG" %in% variables_names) {
    data$ALRM_SANG <- as.numeric(levels(data$ALRM_SANG))[data$ALRM_SANG]
    data$ALRM_SANG[data$ALRM_SANG == 1] <- "Sim"
    data$ALRM_SANG[data$ALRM_SANG == 2] <- "N\u00e3o"
    data$ALRM_SANG <- factor(data$ALRM_SANG)
  }

  # ALRM_HEMAT
  if ("ALRM_HEMAT" %in% variables_names) {
    data$ALRM_HEMAT <- as.numeric(levels(data$ALRM_HEMAT))[data$ALRM_HEMAT]
    data$ALRM_HEMAT[data$ALRM_HEMAT == 1] <- "Sim"
    data$ALRM_HEMAT[data$ALRM_HEMAT == 2] <- "N\u00e3o"
    data$ALRM_HEMAT <- factor(data$ALRM_HEMAT)
  }

  # ALRM_ABDOM
  if ("ALRM_ABDOM" %in% variables_names) {
    data$ALRM_ABDOM <- as.numeric(levels(data$ALRM_ABDOM))[data$ALRM_ABDOM]
    data$ALRM_ABDOM[data$ALRM_ABDOM == 1] <- "Sim"
    data$ALRM_ABDOM[data$ALRM_ABDOM == 2] <- "N\u00e3o"
    data$ALRM_ABDOM <- factor(data$ALRM_ABDOM)
  }

  # ALRM_LETAR
  if ("ALRM_LETAR" %in% variables_names) {
    data$ALRM_LETAR <- as.numeric(levels(data$ALRM_LETAR))[data$ALRM_LETAR]
    data$ALRM_LETAR[data$ALRM_LETAR == 1] <- "Sim"
    data$ALRM_LETAR[data$ALRM_LETAR == 2] <- "N\u00e3o"
    data$ALRM_LETAR <- factor(data$ALRM_LETAR)
  }

  # ALRM_HEPAT
  if ("ALRM_HEPAT" %in% variables_names) {
    data$ALRM_HEPAT <- as.numeric(levels(data$ALRM_HEPAT))[data$ALRM_HEPAT]
    data$ALRM_HEPAT[data$ALRM_HEPAT == 1] <- "Sim"
    data$ALRM_HEPAT[data$ALRM_HEPAT == 2] <- "N\u00e3o"
    data$ALRM_HEPAT <- factor(data$ALRM_HEPAT)
  }

  # ALRM_LIQ
  if ("ALRM_LIQ" %in% variables_names) {
    data$ALRM_LIQ <- as.numeric(levels(data$ALRM_LIQ))[data$ALRM_LIQ]
    data$ALRM_LIQ[data$ALRM_LIQ == 1] <- "Sim"
    data$ALRM_LIQ[data$ALRM_LIQ == 2] <- "N\u00e3o"
    data$ALRM_LIQ <- factor(data$ALRM_LIQ)
  }

  # DT_ALRM
  if ("DT_ALRM" %in% variables_names) {
    data$DT_ALRM <- as.Date(data$DT_ALRM)
  }

  # GRAV_PULSO
  if ("GRAV_PULSO" %in% variables_names) {
    data$GRAV_PULSO <- as.numeric(levels(data$GRAV_PULSO))[data$GRAV_PULSO]
    data$GRAV_PULSO[data$GRAV_PULSO == 1] <- "Sim"
    data$GRAV_PULSO[data$GRAV_PULSO == 2] <- "N\u00e3o"
    data$GRAV_PULSO <- factor(data$GRAV_PULSO)
  }

  # GRAV_CONV
  if ("GRAV_CONV" %in% variables_names) {
    data$GRAV_CONV <- as.numeric(levels(data$GRAV_CONV))[data$GRAV_CONV]
    data$GRAV_CONV[data$GRAV_CONV == 1] <- "Sim"
    data$GRAV_CONV[data$GRAV_CONV == 2] <- "N\u00e3o"
    data$GRAV_CONV <- factor(data$GRAV_CONV)
  }

  # GRAV_ENCH
  if ("GRAV_ENCH" %in% variables_names) {
    data$GRAV_ENCH <- as.numeric(levels(data$GRAV_ENCH))[data$GRAV_ENCH]
    data$GRAV_ENCH[data$GRAV_ENCH == 1] <- "Sim"
    data$GRAV_ENCH[data$GRAV_ENCH == 2] <- "N\u00e3o"
    data$GRAV_ENCH <- factor(data$GRAV_ENCH)
  }

  # GRAV_INSUF
  if ("GRAV_INSUF" %in% variables_names) {
    data$GRAV_INSUF <- as.numeric(levels(data$GRAV_INSUF))[data$GRAV_INSUF]
    data$GRAV_INSUF[data$GRAV_INSUF == 1] <- "Sim"
    data$GRAV_INSUF[data$GRAV_INSUF == 2] <- "N\u00e3o"
    data$GRAV_INSUF <- factor(data$GRAV_INSUF)
  }

  # GRAV_TAQUI
  if ("GRAV_TAQUI" %in% variables_names) {
    data$GRAV_TAQUI <- as.numeric(levels(data$GRAV_TAQUI))[data$GRAV_TAQUI]
    data$GRAV_TAQUI[data$GRAV_TAQUI == 1] <- "Sim"
    data$GRAV_TAQUI[data$GRAV_TAQUI == 2] <- "N\u00e3o"
    data$GRAV_TAQUI <- factor(data$GRAV_TAQUI)
  }

  # GRAV_EXTRE
  if ("GRAV_EXTRE" %in% variables_names) {
    data$GRAV_EXTRE <- as.numeric(levels(data$GRAV_EXTRE))[data$GRAV_EXTRE]
    data$GRAV_EXTRE[data$GRAV_EXTRE == 1] <- "Sim"
    data$GRAV_EXTRE[data$GRAV_EXTRE == 2] <- "N\u00e3o"
    data$GRAV_EXTRE <- factor(data$GRAV_EXTRE)
  }

  # GRAV_HIPOT
  if ("GRAV_HIPOT" %in% variables_names) {
    data$GRAV_HIPOT <- as.numeric(levels(data$GRAV_HIPOT))[data$GRAV_HIPOT]
    data$GRAV_HIPOT[data$GRAV_HIPOT == 1] <- "Sim"
    data$GRAV_HIPOT[data$GRAV_HIPOT == 2] <- "N\u00e3o"
    data$GRAV_HIPOT <- factor(data$GRAV_HIPOT)
  }

  # GRAV_HEMAT
  if ("GRAV_HEMAT" %in% variables_names) {
    data$GRAV_HEMAT <- as.numeric(levels(data$GRAV_HEMAT))[data$GRAV_HEMAT]
    data$GRAV_HEMAT[data$GRAV_HEMAT == 1] <- "Sim"
    data$GRAV_HEMAT[data$GRAV_HEMAT == 2] <- "N\u00e3o"
    data$GRAV_HEMAT <- factor(data$GRAV_HEMAT)
  }

  # GRAV_MELEN
  if ("GRAV_MELEN" %in% variables_names) {
    data$GRAV_MELEN <- as.numeric(levels(data$GRAV_MELEN))[data$GRAV_MELEN]
    data$GRAV_MELEN[data$GRAV_MELEN == 1] <- "Sim"
    data$GRAV_MELEN[data$GRAV_MELEN == 2] <- "N\u00e3o"
    data$GRAV_MELEN <- factor(data$GRAV_MELEN)
  }

  # GRAV_METRO
  if ("GRAV_METRO" %in% variables_names) {
    data$GRAV_METRO <- as.numeric(levels(data$GRAV_METRO))[data$GRAV_METRO]
    data$GRAV_METRO[data$GRAV_METRO == 1] <- "Sim"
    data$GRAV_METRO[data$GRAV_METRO == 2] <- "N\u00e3o"
    data$GRAV_METRO <- factor(data$GRAV_METRO)
  }

  # GRAV_SANG
  if ("GRAV_SANG" %in% variables_names) {
    data$GRAV_SANG <- as.numeric(levels(data$GRAV_SANG))[data$GRAV_SANG]
    data$GRAV_SANG[data$GRAV_SANG == 1] <- "Sim"
    data$GRAV_SANG[data$GRAV_SANG == 2] <- "N\u00e3o"
    data$GRAV_SANG <- factor(data$GRAV_SANG)
  }

  # GRAV_AST
  if ("GRAV_AST" %in% variables_names) {
    data$GRAV_AST <- as.numeric(levels(data$GRAV_AST))[data$GRAV_AST]
    data$GRAV_AST[data$GRAV_AST == 1] <- "Sim"
    data$GRAV_AST[data$GRAV_AST == 2] <- "N\u00e3o"
    data$GRAV_AST <- factor(data$GRAV_AST)
  }

  # GRAV_MIOC
  if ("GRAV_MIOC" %in% variables_names) {
    data$GRAV_MIOC <- as.numeric(levels(data$GRAV_MIOC))[data$GRAV_MIOC]
    data$GRAV_MIOC[data$GRAV_MIOC == 1] <- "Sim"
    data$GRAV_MIOC[data$GRAV_MIOC == 2] <- "N\u00e3o"
    data$GRAV_MIOC <- factor(data$GRAV_MIOC)
  }

  # GRAV_CONSC
  if ("GRAV_CONSC" %in% variables_names) {
    data$GRAV_CONSC <- as.numeric(levels(data$GRAV_CONSC))[data$GRAV_CONSC]
    data$GRAV_CONSC[data$GRAV_CONSC == 1] <- "Sim"
    data$GRAV_CONSC[data$GRAV_CONSC == 2] <- "N\u00e3o"
    data$GRAV_CONSC <- factor(data$GRAV_CONSC)
  }

  # GRAV_ORGAO
  if ("GRAV_ORGAO" %in% variables_names) {
    data$GRAV_ORGAO <- as.numeric(levels(data$GRAV_ORGAO))[data$GRAV_ORGAO]
    data$GRAV_ORGAO[data$GRAV_ORGAO == 1] <- "Sim"
    data$GRAV_ORGAO[data$GRAV_ORGAO == 2] <- "N\u00e3o"
    data$GRAV_ORGAO <- factor(data$GRAV_ORGAO)
  }

  # DT_GRAV
  if ("DT_GRAV" %in% variables_names) {
    data$DT_GRAV <- as.Date(data$DT_GRAV)
  }

  # MANI_HEMOR
  if ("MANI_HEMOR" %in% variables_names) {
    data$MANI_HEMOR <- as.numeric(levels(data$MANI_HEMOR))[data$MANI_HEMOR]
    data$MANI_HEMOR[data$MANI_HEMOR == 1] <- "Sim"
    data$MANI_HEMOR[data$MANI_HEMOR == 2] <- "N\u00e3o"
    data$MANI_HEMOR[data$MANI_HEMOR == 9] <- "Ignorado"
    data$MANI_HEMOR <- factor(data$MANI_HEMOR)
  }

  # EPISTAXE
  if ("EPISTAXE" %in% variables_names) {
    data$EPISTAXE <- as.numeric(levels(data$EPISTAXE))[data$EPISTAXE]
    data$EPISTAXE[data$EPISTAXE == 1] <- "Sim"
    data$EPISTAXE[data$EPISTAXE == 2] <- "N\u00e3o"
    data$EPISTAXE[data$EPISTAXE == 9] <- "Ignorado"
    data$EPISTAXE <- factor(data$EPISTAXE)
  }

  # GENGIVO
  if ("GENGIVO" %in% variables_names) {
    data$GENGIVO <- as.numeric(levels(data$GENGIVO))[data$GENGIVO]
    data$GENGIVO[data$GENGIVO == 1] <- "Sim"
    data$GENGIVO[data$GENGIVO == 2] <- "N\u00e3o"
    data$GENGIVO[data$GENGIVO == 9] <- "Ignorado"
    data$GENGIVO <- factor(data$GENGIVO)
  }

  # METRO
  if ("METRO" %in% variables_names) {
    data$METRO <- as.numeric(levels(data$METRO))[data$METRO]
    data$METRO[data$METRO == 1] <- "Sim"
    data$METRO[data$METRO == 2] <- "N\u00e3o"
    data$METRO[data$METRO == 9] <- "Ignorado"
    data$METRO <- factor(data$METRO)
  }

  # PETEQUIAS
  if ("PETEQUIAS" %in% variables_names) {
    data$PETEQUIAS <- as.numeric(levels(data$PETEQUIAS))[data$PETEQUIAS]
    data$PETEQUIAS[data$PETEQUIAS == 1] <- "Sim"
    data$PETEQUIAS[data$PETEQUIAS == 2] <- "N\u00e3o"
    data$PETEQUIAS[data$PETEQUIAS == 9] <- "Ignorado"
    data$PETEQUIAS <- factor(data$PETEQUIAS)
  }

  # HEMATURA
  if ("HEMATURA" %in% variables_names) {
    data$HEMATURA <- as.numeric(levels(data$HEMATURA))[data$HEMATURA]
    data$HEMATURA[data$HEMATURA == 1] <- "Sim"
    data$HEMATURA[data$HEMATURA == 2] <- "N\u00e3o"
    data$HEMATURA[data$HEMATURA == 9] <- "Ignorado"
    data$HEMATURA <- factor(data$HEMATURA)
  }

  # SANGRAM
  if ("SANGRAM" %in% variables_names) {
    data$SANGRAM <- as.numeric(levels(data$SANGRAM))[data$SANGRAM]
    data$SANGRAM[data$SANGRAM == 1] <- "Sim"
    data$SANGRAM[data$SANGRAM == 2] <- "N\u00e3o"
    data$SANGRAM[data$SANGRAM == 9] <- "Ignorado"
    data$SANGRAM <- factor(data$SANGRAM)
  }

  # LACO_N
  if ("LACO_N" %in% variables_names) {
    data$LACO_N <- as.numeric(levels(data$LACO_N))[data$LACO_N]
    data$LACO_N[data$LACO_N == 1] <- "Sim"
    data$LACO_N[data$LACO_N == 2] <- "N\u00e3o"
    data$LACO_N[data$LACO_N == 9] <- "Ignorado"
    data$LACO_N <- factor(data$LACO_N)
  }

  # PLASMATICO
  if ("PLASMATICO" %in% variables_names) {
    data$PLASMATICO <- as.numeric(levels(data$PLASMATICO))[data$PLASMATICO]
    data$PLASMATICO[data$PLASMATICO == 1] <- "Sim"
    data$PLASMATICO[data$PLASMATICO == 2] <- "N\u00e3o"
    data$PLASMATICO[data$PLASMATICO == 9] <- "Ignorado"
    data$PLASMATICO <- factor(data$PLASMATICO)
  }

  # EVIDENCIA
  if ("EVIDENCIA" %in% variables_names) {
    data$EVIDENCIA <- as.numeric(levels(data$EVIDENCIA))[data$EVIDENCIA]
    data$EVIDENCIA[data$EVIDENCIA == 1] <- "Hemoconcentra\u00e7\u00e3o"
    data$EVIDENCIA[data$EVIDENCIA == 2] <- "Derrames cavit\u00e1rios"
    data$EVIDENCIA[data$EVIDENCIA == 3] <- "Hipoproteinemia"
    data$EVIDENCIA <- factor(data$EVIDENCIA)
  }

  # CON_FHD
  if ("CON_FHD" %in% variables_names) {
    data$CON_FHD <- as.numeric(levels(data$CON_FHD))[data$CON_FHD]
    data$CON_FHD[data$CON_FHD == 1] <- "Grau I"
    data$CON_FHD[data$CON_FHD == 2] <- "Grau II"
    data$CON_FHD[data$CON_FHD == 3] <- "Grau III"
    data$CON_FHD[data$CON_FHD == 4] <- "Grau IV"
    data$CON_FHD <- factor(data$CON_FHD)
  }

  # COMPLICA
  if ("COMPLICA" %in% variables_names) {
    data$COMPLICA <- as.numeric(levels(data$COMPLICA))[data$COMPLICA]
    data$COMPLICA[data$COMPLICA == 1] <- "Altera\u00e7\u00f5es neurol\u00f3gicas"
    data$COMPLICA[data$COMPLICA == 2] <- "Disfun\u00e7\u00e3o cardiorespirat\u00f3ria"
    data$COMPLICA[data$COMPLICA == 3] <- "Insufici\u00eancia hep\u00e1tica"
    data$COMPLICA[data$COMPLICA == 4] <- "Plaquetas <50.000mm"
    data$COMPLICA[data$COMPLICA == 5] <- "Hemorragia digestiva"
    data$COMPLICA[data$COMPLICA == 6] <- "Derrames cavit\u00e1rios"
    data$COMPLICA[data$COMPLICA == 7] <- "Leucometria < 100"
    data$COMPLICA[data$COMPLICA == 8] <- "N\u00e3o se enquadra nos crit\u00e9rios de FHD"
    data$COMPLICA <- factor(data$COMPLICA)
  }

  # NDUPLIC_N
  if ("NDUPLIC_N" %in% variables_names) {
    data$NDUPLIC_N <- as.numeric(levels(data$NDUPLIC_N))[data$NDUPLIC_N]
    data$NDUPLIC_N[data$NDUPLIC_N == 0] <- "N\u00e3o identificado"
    data$NDUPLIC_N[data$NDUPLIC_N == ""] <- "N\u00e3o identificado"
    data$NDUPLIC_N[data$NDUPLIC_N == 1] <- "N\u00e3o \u00e9 duplicidade (n\u00e3o listar)"
    data$NDUPLIC_N[data$NDUPLIC_N == 2] <- "Duplicidade (n\u00e3o contar)"
    data$NDUPLIC_N <- factor(data$NDUPLIC_N)
  }

  # CS_FLXRET
  if ("CS_FLXRET" %in% variables_names) {
    data$CS_FLXRET <- as.numeric(levels(data$CS_FLXRET))[data$CS_FLXRET]
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
