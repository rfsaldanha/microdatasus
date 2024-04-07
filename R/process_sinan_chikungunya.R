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

  # Use dtplyr
  data <- dtplyr::lazy_dt(data)

  # TP_NOT
  if ("TP_NOT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(TP_NOT = dplyr::case_match(
        .data$TP_NOT,
        "1" ~ "Negativa",
        "2" ~ "Individual",
        "3" ~ "Surto",
        "4" ~ "Agregado",
        .default = .data$TP_NOT
      )) %>%
      dplyr::mutate(TP_NOT = as.factor(.data$TP_NOT))
  }

  # DT_NOTIFIC
  if ("DT_NOTIFIC" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_NOTIFIC = as.Date(.data$DT_NOTIFIC))
  }

  # SG_UF_NOT
  if ("SG_UF_NOT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(SG_UF_NOT = dplyr::case_match(
        .data$SG_UF_NOT,
        "0" ~ "Ignorado",
        "99" ~ "Ignorado",
        "11" ~ "Rond\u00f4nia",
        "12" ~ "Acre",
        "13" ~ "Amazonas",
        "14" ~ "Roraima",
        "15" ~ "Par\u00e1",
        "16" ~ "Amap\u00e1",
        "17" ~ "Tocantins",
        "21" ~ "Maranh\u00e3o",
        "22" ~ "Piau\u00ed",
        "23" ~ "Cear\u00e1",
        "24" ~ "Rio Grande do Norte",
        "25" ~ "Para\u00edba",
        "26" ~ "Pernambuco",
        "27" ~ "Alagoas",
        "28" ~ "Sergipe",
        "29" ~ "Bahia",
        "31" ~ "Minas Gerais",
        "32" ~ "Esp\u00edrito Santo",
        "33" ~ "Rio de Janeiro",
        "35" ~ "S\u00e3o Paulo",
        "41" ~ "Paran\u00e1",
        "42" ~ "Santa Catarina",
        "43" ~ "Rio Grande do Sul",
        "50" ~ "Mato Grosso do Sul",
        "51" ~ "Mato Grosso",
        "52" ~ "Goi\u00e1s",
        "53" ~ "Distrito Federal",
        .default = .data$SG_UF_NOT
      )) %>%
      dplyr::mutate(SG_UF_NOT = as.factor(.data$SG_UF_NOT))
  }

  # IDADE
  if ("NU_IDADE_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(NU_IDADE_N = dplyr::case_match(.data$NU_IDADE_N,
                                              999 ~ NA,
                                              .default = .data$NU_IDADE_N)) %>%
      # Codigo e valor
      dplyr::mutate(idade_cod = substr(.data$NU_IDADE_N, 1, 1),
                    idade_value = as.numeric(substr(.data$NU_IDADE_N, 2, 3)),) %>%
      dplyr::mutate(IDADEminutos = dplyr::case_match(.data$idade_cod,
                                                     "0" ~ idade_value,
                                                     .default = NA)) %>%
      dplyr::mutate(IDADEhoras = dplyr::case_match(.data$idade_cod,
                                                   "1" ~ idade_value,
                                                   .default = NA)) %>%
      dplyr::mutate(IDADEdias = dplyr::case_match(.data$idade_cod,
                                                  "2" ~ idade_value,
                                                  .default = NA)) %>%
      dplyr::mutate(IDADEmeses = dplyr::case_match(.data$idade_cod,
                                                   "3" ~ idade_value,
                                                   .default = NA)) %>%
      dplyr::mutate(
        IDADEanos = dplyr::case_match(
          .data$idade_cod,
          "4" ~ idade_value,
          "5" ~ idade_value + 100,
          .default = NA
        )
      ) %>%
      dplyr::select(-"idade_cod", -"idade_value")
  }

  # CS_SEXO
  if ("CS_SEXO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CS_SEXO = dplyr::case_match(
       .data$CS_SEXO,
       "M" ~ "Masculino",
       "F" ~ "Feminino",
       "I" ~ "Ignorado",
       .default = .data$CS_SEXO
      )) %>%
      dplyr::mutate(CS_SEXO = as.factor(.data$CS_SEXO))
  }

  # CS_GESTANT
  if ("CS_GESTANT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CS_GESTANT = dplyr::case_match(
        .data$CS_GESTANT,
        "1" ~ "1o trimestre",
        "2" ~ "2o trimestre",
        "3" ~ "3o trimestre",
        "4" ~ "Idade gestacional ignorada",
        "5" ~ "N\u00e3o",
        "6" ~ "N\u00e3o se aplica",
        "9" ~ "Ignorado",
        .default = .data$CS_GESTANT
      )) %>%
      dplyr::mutate(CS_GESTANT = as.factor(.data$CS_GESTANT))
  }

  # CS_RACA
  if ("CS_RACA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CS_RACA = dplyr::case_match(
        .data$CS_RACA,
        "1" ~ "Branca",
        "2" ~ "Preta",
        "3" ~ "Amarela",
        "4" ~ "Parda",
        "5" ~ "Ind\u00edgena",
        "9" ~ "Ignorado",
        .default = .data$CS_RACA
      )) %>%
      dplyr::mutate(CS_RACA = as.factor(.data$CS_RACA))
  }

  # CS_ESCOL_N
  if ("CS_ESCOL_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CS_ESCOL_N = dplyr::case_match(
        .data$CS_ESCOL_N,
        "1" ~ "1a a 4a s\u00e9rie incompleta do EF",
        "2" ~ "4a s\u00e9rie completa do EF (antigo 1o grau)",
        "3" ~ "5a \u00e0 8a s\u00e9rie incompleta do EF (antigo gin\u00e1sio ou 1o grau)",
        "4" ~ "Ensino fundamental completo (antigo gin\u00e1sio ou 1o grau)",
        "5" ~ "Ensino m\u00e9dio incompleto (antigo colegial ou 2o grau)",
        "6" ~ "Ensino m\u00e9dio completo (antigo colegial ou 2o grau)",
        "7" ~ "Educa\u00e7\u00e3o superior incompleta",
        "8" ~ "Educa\u00e7\u00e3o superior completa",
        "9" ~ "Ignorado",
        "10" ~  "N\u00e3o se aplica",
        .default = .data$CS_ESCOL_N
      )) %>%
      dplyr::mutate(CS_ESCOL_N <- as.factor(.data$CS_ESCOL_N))
  }

  # SG_UF
  if ("SG_UF" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(SG_UF = dplyr::case_match(
        .data$SG_UF,
        "0" ~ "Ignorado",
        "99" ~ "Ignorado",
        "11" ~ "Rond\u00f4nia",
        "12" ~ "Acre",
        "13" ~ "Amazonas",
        "14" ~ "Roraima",
        "15" ~ "Par\u00e1",
        "16" ~ "Amap\u00e1",
        "17" ~ "Tocantins",
        "21" ~ "Maranh\u00e3o",
        "22" ~ "Piau\u00ed",
        "23" ~ "Cear\u00e1",
        "24" ~ "Rio Grande do Norte",
        "25" ~ "Para\u00edba",
        "26" ~ "Pernambuco",
        "27" ~ "Alagoas",
        "28" ~ "Sergipe",
        "29" ~ "Bahia",
        "31" ~ "Minas Gerais",
        "32" ~ "Esp\u00edrito Santo",
        "33" ~ "Rio de Janeiro",
        "35" ~ "S\u00e3o Paulo",
        "41" ~ "Paran\u00e1",
        "42" ~ "Santa Catarina",
        "43" ~ "Rio Grande do Sul",
        "50" ~ "Mato Grosso do Sul",
        "51" ~ "Mato Grosso",
        "52" ~ "Goi\u00e1s",
        "53" ~ "Distrito Federal",
        .default = .data$SG_UF
      )) %>%
      dplyr::mutate(SG_UF = as.factor(.data$SG_UF))
  }

  # ID_PAIS
  if ("ID_PAIS" %in% variables_names) {
    data$ID_PAIS <- dplyr::left_join(data, microdatasus::paisnet, by = c("ID_PAIS" = "ID_PAIS"))$NM_PAIS
  }

  # ID_OCUPA_N
  if ("ID_OCUPA_N" %in% variables_names) {
    data$ID_OCUPA_N <- factor(dplyr::left_join(data, microdatasus::tabCBO, by = c("ID_OCUPA_N" = "cod"))$nome)
  }

  # FEBRE
  if ("FEBRE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(FEBRE = dplyr::case_match(
        .data$FEBRE,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$FEBRE
      )) %>%
      dplyr::mutate(FEBRE  = as.factor(.data$FEBRE))
  }

  # MIALGIA
  if ("MIALGIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(MIALGIA = dplyr::case_match(
        .data$MIALGIA,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$MIALGIA
      )) %>%
      dplyr::mutate(MIALGIA  = as.factor(.data$MIALGIA))
  }

  # CEFALEIA
  if ("CEFALEIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CEFALEIA = dplyr::case_match(
        .data$CEFALEIA,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$CEFALEIA
      )) %>%
      dplyr::mutate(CEFALEIA  = as.factor(.data$CEFALEIA))
  }

  # EXANTEMA
  if ("EXANTEMA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(EXANTEMA = dplyr::case_match(
        .data$EXANTEMA,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$EXANTEMA
      )) %>%
      dplyr::mutate(EXANTEMA  = as.factor(.data$EXANTEMA))
  }

  # VOMITO
  if ("VOMITO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(VOMITO = dplyr::case_match(
        .data$VOMITO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$VOMITO
      )) %>%
      dplyr::mutate(VOMITO  = as.factor(.data$VOMITO))
  }

  # NAUSEA
  if ("NAUSEA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(NAUSEA = dplyr::case_match(
        .data$NAUSEA,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$NAUSEA
      )) %>%
      dplyr::mutate(NAUSEA  = as.factor(.data$NAUSEA))
  }

  # DOR_COSTAS
  if ("DOR_COSTAS" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DOR_COSTAS = dplyr::case_match(
        .data$DOR_COSTAS,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$DOR_COSTAS
      )) %>%
      dplyr::mutate(DOR_COSTAS  = as.factor(.data$DOR_COSTAS))
  }

  # CONJUNTVIT
  if ("CONJUNTVIT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CONJUNTVIT = dplyr::case_match(
        .data$CONJUNTVIT,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$CONJUNTVIT
      )) %>%
      dplyr::mutate(CONJUNTVIT  = as.factor(.data$CONJUNTVIT))
  }

  # ARTRITE
  if ("ARTRITE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ARTRITE = dplyr::case_match(
        .data$ARTRITE,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ARTRITE
      )) %>%
      dplyr::mutate(ARTRITE  = as.factor(.data$ARTRITE))
  }

  # ARTRALGIA
  if ("ARTRALGIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ARTRALGIA = dplyr::case_match(
        .data$ARTRALGIA,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ARTRALGIA
      )) %>%
      dplyr::mutate(ARTRALGIA  = as.factor(.data$ARTRALGIA))
  }

  # PETEQUIA_N
  if ("PETEQUIA_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(PETEQUIA_N = dplyr::case_match(
        .data$PETEQUIA_N,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$PETEQUIA_N
      )) %>%
      dplyr::mutate(PETEQUIA_N  = as.factor(.data$PETEQUIA_N))
  }

  # LEUCOPENIA
  if ("LEUCOPENIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(LEUCOPENIA = dplyr::case_match(
        .data$LEUCOPENIA,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$LEUCOPENIA
      )) %>%
      dplyr::mutate(LEUCOPENIA  = as.factor(.data$LEUCOPENIA))
  }

  # LACO
  if ("LACO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(LACO = dplyr::case_match(
        .data$LACO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$LACO
      )) %>%
      dplyr::mutate(LACO  = as.factor(.data$LACO))
  }

  # DOR_RETRO
  if ("DOR_RETRO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DOR_RETRO = dplyr::case_match(
        .data$DOR_RETRO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$DOR_RETRO
      )) %>%
      dplyr::mutate(DOR_RETRO  = as.factor(.data$DOR_RETRO))
  }

  # DIABETES
  if ("DIABETES" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DIABETES = dplyr::case_match(
        .data$DIABETES,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$DIABETES
      )) %>%
      dplyr::mutate(DIABETES  = as.factor(.data$DIABETES))
  }

  # HEMATOLOG
  if ("HEMATOLOG" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(HEMATOLOG = dplyr::case_match(
        .data$HEMATOLOG,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$HEMATOLOG
      )) %>%
      dplyr::mutate(HEMATOLOG  = as.factor(.data$HEMATOLOG))
  }

  # HEPATOPAT
  if ("HEPATOPAT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(HEPATOPAT = dplyr::case_match(
        .data$HEPATOPAT,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$HEPATOPAT
      )) %>%
      dplyr::mutate(HEPATOPAT  = as.factor(.data$HEPATOPAT))
  }

  # RENAL
  if ("RENAL" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(RENAL = dplyr::case_match(
        .data$RENAL,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$RENAL
      )) %>%
      dplyr::mutate(RENAL  = as.factor(.data$RENAL))
  }

  # HIPERTENSA
  if ("HIPERTENSA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(HIPERTENSA = dplyr::case_match(
        .data$HIPERTENSA,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$HIPERTENSA
      )) %>%
      dplyr::mutate(HIPERTENSA  = as.factor(.data$HIPERTENSA))
  }

  # ACIDO_PEPT
  if ("ACIDO_PEPT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ACIDO_PEPT = dplyr::case_match(
        .data$ACIDO_PEPT,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ACIDO_PEPT
      )) %>%
      dplyr::mutate(ACIDO_PEPT  = as.factor(.data$ACIDO_PEPT))
  }

  # AUTO_IMUNE
  if ("AUTO_IMUNE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(AUTO_IMUNE = dplyr::case_match(
        .data$AUTO_IMUNE,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$AUTO_IMUNE
      )) %>%
      dplyr::mutate(AUTO_IMUNE  = as.factor(.data$AUTO_IMUNE))
  }

  # DT_CHIK_S1
  if ("DT_CHIK_S1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_CHIK_S1 = as.Date(.data$DT_CHIK_S1))
  }

  # DT_CHIK_S2
  if ("DT_CHIK_S2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_CHIK_S2 = as.Date(.data$DT_CHIK_S2))
  }

  # DT_PRNT
  if ("DT_PRNT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_PRNT = as.Date(.data$DT_PRNT))
  }

  # RES_CHIKS1
  if ("RES_CHIKS1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(RES_CHIKS1 = dplyr::case_match(
        .data$RES_CHIKS1,
        "1" ~ "Reagente",
        "2" ~ "N\u00e3o reagente",
        "3" ~ "Inconclusivo",
        "4" ~ "N\u00e3o realizado",
        .default = .data$RES_CHIKS1
      )) %>%
      dplyr::mutate(RES_CHIKS1 = as.factor(.data$RES_CHIKS1))
  }

  # RES_CHIKS2
  if ("RES_CHIKS2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(RES_CHIKS2 = dplyr::case_match(
        .data$RES_CHIKS2,
        "1" ~ "Reagente",
        "2" ~ "N\u00e3o reagente",
        "3" ~ "Inconclusivo",
        "4" ~ "N\u00e3o realizado",
        .default = .data$RES_CHIKS2
      )) %>%
      dplyr::mutate(RES_CHIKS2 = as.factor(.data$RES_CHIKS2))
  }

  # RESUL_PRNT
  if ("RESUL_PRNT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(RESUL_PRNT = dplyr::case_match(
        .data$RESUL_PRNT,
        "1" ~ "Reagente",
        "2" ~ "N\u00e3o reagente",
        "3" ~ "Inconclusivo",
        "4" ~ "N\u00e3o realizado",
        .default = .data$RESUL_PRNT
      )) %>%
      dplyr::mutate(RESUL_PRNT = as.factor(.data$RESUL_PRNT))
  }

  # DT_SORO
  if ("DT_SORO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_SORO = as.Date(.data$DT_SORO))
  }

  # RESUL_SORO
  if ("RESUL_SORO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(RESUL_SORO = dplyr::case_match(
        .data$RESUL_SORO,
        "1" ~ "Reagente",
        "2" ~ "N\u00e3o reagente",
        "3" ~ "Inconclusivo",
        "4" ~ "N\u00e3o realizado",
        .default = .data$RESUL_SORO
      )) %>%
      dplyr::mutate(RESUL_SORO = as.factor(.data$RESUL_SORO))
  }

  # DT_NS1
  if ("DT_NS1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_NS1 = as.Date(.data$DT_NS1))
  }

  # RESUL_NS1
  if ("RESUL_NS1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(RESUL_NS1 = dplyr::case_match(
        .data$RESUL_NS1,
        "1" ~ "Positivo",
        "2" ~ "Negativo",
        "3" ~ "Inconclusivo",
        "4" ~ "N\u00e3o realizado",
        .default = .data$RESUL_NS1
      )) %>%
      dplyr::mutate(RESUL_NS1 = as.factor(.data$RESUL_NS1))
  }

  # DT_VIRAL
  if ("DT_VIRAL" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_VIRAL = as.Date(.data$DT_VIRAL))
  }

  # RESUL_VI_N
  if ("RESUL_VI_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(RESUL_VI_N = dplyr::case_match(
        .data$RESUL_VI_N,
        "1" ~ "Positivo",
        "2" ~ "Negativo",
        "3" ~ "Inconclusivo",
        "4" ~ "N\u00e3o realizado",
        .default = .data$RESUL_VI_N
      )) %>%
      dplyr::mutate(RESUL_VI_N = as.factor(.data$RESUL_VI_N))
  }

  # DT_PCR
  if ("DT_PCR" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_PCR = as.Date(.data$DT_PCR))
  }

  # RESUL_PCR_
  if ("RESUL_PCR_" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(RESUL_PCR_ = dplyr::case_match(
        .data$RESUL_PCR_,
        "1" ~ "Positivo",
        "2" ~ "Negativo",
        "3" ~ "Inconclusivo",
        "4" ~ "N\u00e3o realizado",
        .default = .data$RESUL_PCR_
      )) %>%
      dplyr::mutate(RESUL_PCR_ = as.factor(.data$RESUL_PCR_))
  }

  # HISTOPA_N
  if ("HISTOPA_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(HISTOPA_N = dplyr::case_match(
        .data$HISTOPA_N,
        "1" ~ "Positivo",
        "2" ~ "Negativo",
        "3" ~ "Inconclusivo",
        "4" ~ "N\u00e3o realizado",
        .default = .data$HISTOPA_N
      )) %>%
      dplyr::mutate(HISTOPA_N = as.factor(.data$HISTOPA_N))
  }

  # IMUNOH_N
  if ("IMUNOH_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(IMUNOH_N = dplyr::case_match(
        .data$IMUNOH_N,
        "1" ~ "Positivo",
        "2" ~ "Negativo",
        "3" ~ "Inconclusivo",
        "4" ~ "N\u00e3o realizado",
        .default = .data$IMUNOH_N
      )) %>%
      dplyr::mutate(IMUNOH_N = as.factor(.data$IMUNOH_N))
  }

  # HOSPITALIZ
  if ("HOSPITALIZ" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(IMUNOH_N = dplyr::case_match(
        .data$HOSPITALIZ,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$HOSPITALIZ
      )) %>%
      dplyr::mutate(HOSPITALIZ = as.factor(.data$HOSPITALIZ))
  }

  # DT_INTERNA
  if ("DT_INTERNA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_INTERNA = as.Date(.data$DT_INTERNA))
  }

  # UF
  if ("UF" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(UF = dplyr::case_match(
        .data$UF,
        "0" ~ "Ignorado",
        "99" ~ "Ignorado",
        "11" ~ "Rond\u00f4nia",
        "12" ~ "Acre",
        "13" ~ "Amazonas",
        "14" ~ "Roraima",
        "15" ~ "Par\u00e1",
        "16" ~ "Amap\u00e1",
        "17" ~ "Tocantis",
        "21" ~ "Maranh\u00e3o",
        "22" ~ "Piau\u00ed",
        "23" ~ "Cear\u00e1",
        "24" ~ "Rio Grande do Norte",
        "25" ~ "Para\u00edba",
        "26" ~ "Pernambuco",
        "27" ~ "Alagoas",
        "28" ~ "Sergipe",
        "29" ~ "Bahia",
        "31" ~ "Minas Gerais",
        "32" ~ "Esp\u00edrito Santo",
        "33" ~ "Rio de Janeiro",
        "35" ~ "S\u00e3o Paulo",
        "41" ~ "Paran\u00e1",
        "42" ~ "Santa Catarina",
        "43" ~ "Rio Grande do Sul",
        "50" ~ "Mato Grosso do Sul",
        "51" ~ "Mato Grosso",
        "52" ~ "Goi\u00e1s",
        "53" ~ "Distrito Federal",
        .default = .data$UF
      )) %>%
      dplyr::mutate(UF = as.factor(.data$UF))
  }

  # MUNICIPIO
  if("MUNICIPIO" %in% variables_names & municipality_data == TRUE){
    data <- data %>%
      dplyr::mutate(MUNICIPIO = as.numeric(.data$MUNICIPIO)) %>%
      dplyr::left_join(microdatasus::tabMun, by = c("MUNICIPIO" = "munResCod"))
  } else if("MUNICIPIO" %in% variables_names){
    data <- data %>%
      dplyr::mutate(MUNICIPIO = as.numeric(.data$MUNICIPIO))
  }

  # TPAUTOCTO
  if ("TPAUTOCTO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(TPAUTOCTO = dplyr::case_match(
        .data$TPAUTOCTO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "3" ~ "Indeterminado",
        .default = .data$TPAUTOCTO
      )) %>%
      dplyr::mutate(TPAUTOCTO = as.factor(.data$TPAUTOCTO))
  }

  # COUFINF
  if ("COUFINF" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(IMUNOH_N = dplyr::case_match(
        "0" ~ "Ignorado",
        "99" ~ "Ignorado",
        "11" ~ "Rond\u00f4nia",
        "12" ~ "Acre",
        "13" ~ "Amazonas",
        "14" ~ "Roraima",
        "15" ~ "Par\u00e1",
        "16" ~ "Amap\u00e1",
        "17" ~ "Tocantis",
        "21" ~ "Maranh\u00e3o",
        "22" ~ "Piau\u00ed",
        "23" ~ "Cear\u00e1",
        "24" ~ "Rio Grande do Norte",
        "25" ~ "Para\u00edba",
        "26" ~ "Pernambuco",
        "27" ~ "Alagoas",
        "28" ~ "Sergipe",
        "29" ~ "Bahia",
        "31" ~ "Minas Gerais",
        "32" ~ "Esp\u00edrito Santo",
        "33" ~ "Rio de Janeiro",
        "35" ~ "S\u00e3o Paulo",
        "41" ~ "Paran\u00e1",
        "42" ~ "Santa Catarina",
        "43" ~ "Rio Grande do Sul",
        "50" ~ "Mato Grosso do Sul",
        "51" ~ "Mato Grosso",
        "52" ~ "Goi\u00e1s",
        "53" ~ "Distrito Federal",
        .default = .data$COUFINF
      )) %>%
      dplyr::mutate(COUFINF = as.factor(.data$COUFINF))
  }

  # COPAISINF
  if ("COPAISINF" %in% variables_names) {
    data$COPAISINF <- dplyr::left_join(data, microdatasus::paisnet, by = c("COPAISINF" = "COPAISINF"))$NM_PAIS
  }

  # CLASSI_FIN
  if ("CLASSI_FIN" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CLASSI_FIN = dplyr::case_match(
        .data$CLASSI_FIN,
        1 ~ "Dengue cl\u00e1ssico",
        2 ~ "Dengue com complica\u00e7\u00f5es",
        3 ~ "Febre hemorr\u00e1gica do dengue",
        4 ~ "S\u00edndrome do choque do dengue",
        5 ~ "Descartado",
        8 ~ "Inconclusivo",
        10 ~  "Dengue",
        11 ~  "Dengue com sinais de alarme",
        12 ~  "Dengue grave",
        13 ~  "Chikungunya",
        .default = .data$CLASSI_FIN
      )) %>%
      dplyr::mutate(CLASSI_FIN = as.factor(.data$CLASSI_FIN))
  }

  # CRITERIO
  if ("CRITERIO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CRITERIO = dplyr::case_match(
        .data$CRITERIO,
        "1" ~ "Laborat\u00f3rio",
        "2" ~ "Cl\u00ednico epidemiol\u00f3gico",
        "3" ~ "Em investiga\u00e7\u00e3o",
        .default = .data$CRITERIO
      )) %>%
      dplyr::mutate(CRITERIO = as.factor(.data$CRITERIO))
  }

  # CLINC_CHIK
  if ("CLINC_CHIK" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CLINC_CHIK = dplyr::case_match(
        .data$CLINC_CHIK,
        "1" ~"Aguda",
        "2" ~"Cr\u00f4nica"
        .default = .data$CLINC_CHIK
      )) %>%
      dplyr::mutate(CLINC_CHIK = as.factor(.data$CLINC_CHIK))
  }

  # TPAUTOCTO
  if ("TPAUTOCTO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(TPAUTOCTO = dplyr::case_match(
        .data$TPAUTOCTO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "3" ~ "Indeterminado",
        .default = .data$TPAUTOCTO
      )) %>%
      dplyr::mutate(TPAUTOCTO = as.factor(.data$TPAUTOCTO))
  }

  # COUFINF
  if ("COUFINF" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(COUFINF = dplyr::case_match(
        .data$COUFINF,
        "0" ~ "Ignorado",
        "99" ~ "Ignorado",
        "11" ~ "Rond\u00f4nia",
        "12" ~ "Acre",
        "13" ~ "Amazonas",
        "14" ~ "Roraima",
        "15" ~ "Par\u00e1",
        "16" ~ "Amap\u00e1",
        "17" ~ "Tocantins",
        "21" ~ "Maranh\u00e3o",
        "22" ~ "Piau\u00ed",
        "23" ~ "Cear\u00e1",
        "24" ~ "Rio Grande do Norte",
        "25" ~ "Para\u00edba",
        "26" ~ "Pernambuco",
        "27" ~ "Alagoas",
        "28" ~ "Sergipe",
        "29" ~ "Bahia",
        "31" ~ "Minas Gerais",
        "32" ~ "Esp\u00edrito Santo",
        "33" ~ "Rio de Janeiro",
        "35" ~ "S\u00e3o Paulo",
        "41" ~ "Paran\u00e1",
        "42" ~ "Santa Catarina",
        "43" ~ "Rio Grande do Sul",
        "50" ~ "Mato Grosso do Sul",
        "51" ~ "Mato Grosso",
        "52" ~ "Goi\u00e1s",
        "53" ~ "Distrito Federal",
        .default = .data$COUFINF
      )) %>%
      dplyr::mutate(COUFINF = as.factor(.data$COUFINF))
  }

  # COPAISINF
  if ("COPAISINF" %in% variables_names) {
    data$COPAISINF <- dplyr::left_join(data, microdatasus::paisnet, by = c("COPAISINF" = "COPAISINF"))$NM_PAIS
  }

  # DOENCA_TRA
  if ("DOENCA_TRA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DOENCA_TRA = dplyr::case_match(
        .data$DOENCA_TRA,
        "1" ~"Sim",
        "2" ~"N\u00e3o",
        "9" ~"Ignorado",
        .default = .data$DOENCA_TRA
      )) %>%
      dplyr::mutate(DOENCA_TRA = as.factor(.data$DOENCA_TRA))
  }

  # EVOLUCAO
  if ("EVOLUCAO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(EVOLUCAO = dplyr::case_match(
        .data$EVOLUCAO,
        "1" ~ "Cura",
        "2" ~ "\u00d3bito por dengue",
        "3" ~ "\u00d3bito por outras causas",
        "4" ~ "\u00d3bito em investiga\u00e7\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$EVOLUCAO
      )) %>%
      dplyr::mutate(EVOLUCAO = as.factor(.data$EVOLUCAO))
  }

  # DT_OBITO
  if ("DT_OBITO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_OBITO = as.Date(.data$DT_OBITO))
  }

  # DT_ENCERRA
  if ("DT_ENCERRA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_ENCERRA = as.Date(.data$DT_ENCERRA))
  }

  # CS_FLXRET
  if ("CS_FLXRET" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CS_FLXRET = dplyr::case_match(
        .data$CS_FLXRET,
        "0" ~ "N\u00e3o",
        "1" ~ "Habilitado para envio",
        "2" ~ "Enviado",
        .default = .data$CS_FLXRET
      )) %>%
      dplyr::mutate(CS_FLXRET = as.factor(.data$CS_FLXRET))
  }

  # From data.table to tibble
  data <- tibble::as_tibble(data)

  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  data <- suppressWarnings(tibble::as_tibble(lapply(X = data, FUN = stringi::stri_unescape_unicode)))

  # Return
  return(data)
}
