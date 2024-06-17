#' Process SINAN Chikungunya variables from DataSUS
#'
#' \code{process_sinan_chikungunya} processes SINAN Chikungunya variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SINAN Chikungunya variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples
#' process_sinan_chikungunya(sinan_chikungunya_sample)
#'
#' @return a \code{data.frame} with the processed data.
#'
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
    colnames(tabMun)[1] <- "MUNICIPIO"
    tabMun$MUNICIPIO <- as.character(tabMun$MUNICIPIO)

    data <- data %>%
      dplyr::left_join(tabMun, by = "MUNICIPIO")
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
        "1" ~ "Dengue cl\u00e1ssico",
        "2" ~ "Dengue com complica\u00e7\u00f5es",
        "3" ~ "Febre hemorr\u00e1gica do dengue",
        "4" ~ "S\u00edndrome do choque do dengue",
        "5" ~ "Descartado",
        "8" ~ "Inconclusivo",
        "10" ~  "Dengue",
        "11" ~  "Dengue com sinais de alarme",
        "12" ~  "Dengue grave",
        "13" ~  "Chikungunya",
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
        "2" ~"Cr\u00f4nica",
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

  # ALRM_HIPOT
  if ("ALRM_HIPOT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ALRM_HIPOT = dplyr::case_match(
        .data$ALRM_HIPOT,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ALRM_HIPOT
      )) %>%
      dplyr::mutate(ALRM_HIPOT = as.factor(.data$ALRM_HIPOT))
  }

  # ALRM_PLAQ
  if ("ALRM_PLAQ" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ALRM_PLAQ = dplyr::case_match(
        .data$ALRM_PLAQ,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ALRM_PLAQ
      )) %>%
      dplyr::mutate(ALRM_PLAQ = as.factor(.data$ALRM_PLAQ))
  }

  # ALRM_VOM
  if ("ALRM_VOM" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ALRM_VOM = dplyr::case_match(
        .data$ALRM_VOM,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ALRM_VOM
      )) %>%
      dplyr::mutate(ALRM_VOM = as.factor(.data$ALRM_VOM))
  }

  # ALRM_SANG
  if ("ALRM_SANG" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ALRM_SANG = dplyr::case_match(
        .data$ALRM_SANG,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ALRM_SANG
      )) %>%
      dplyr::mutate(ALRM_SANG = as.factor(.data$ALRM_SANG))
  }

  # ALRM_HEMAT
  if ("ALRM_HEMAT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ALRM_HEMAT = dplyr::case_match(
        .data$ALRM_HEMAT,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ALRM_HEMAT
      )) %>%
      dplyr::mutate(ALRM_HEMAT = as.factor(.data$ALRM_HEMAT))
  }

  # ALRM_ABDOM
  if ("ALRM_ABDOM" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ALRM_ABDOM = dplyr::case_match(
        .data$ALRM_ABDOM,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ALRM_ABDOM
      )) %>%
      dplyr::mutate(ALRM_ABDOM = as.factor(.data$ALRM_ABDOM))
  }

  # ALRM_LETAR
  if ("ALRM_LETAR" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ALRM_LETAR = dplyr::case_match(
        .data$ALRM_LETAR,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ALRM_LETAR
      )) %>%
      dplyr::mutate(ALRM_LETAR = as.factor(.data$ALRM_LETAR))
  }

  # ALRM_HEPAT
  if ("ALRM_HEPAT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ALRM_HEPAT = dplyr::case_match(
        .data$ALRM_HEPAT,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ALRM_HEPAT
      )) %>%
      dplyr::mutate(ALRM_HEPAT = as.factor(.data$ALRM_HEPAT))
  }

  # ALRM_LIQ
  if ("ALRM_LIQ" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ALRM_LIQ = dplyr::case_match(
        .data$ALRM_LIQ,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$ALRM_LIQ
      )) %>%
      dplyr::mutate(ALRM_LIQ = as.factor(.data$ALRM_LIQ))
  }

  # DT_ALRM
  if ("DT_ALRM" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_ALRM = as.Date(.data$DT_ALRM))
  }

  # GRAV_PULSO
  if ("GRAV_PULSO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_PULSO = dplyr::case_match(
        .data$GRAV_PULSO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_PULSO
      )) %>%
      dplyr::mutate(GRAV_PULSO = as.factor(.data$GRAV_PULSO))
  }

  # GRAV_CONV
  if ("GRAV_CONV" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_CONV = dplyr::case_match(
        .data$GRAV_CONV,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_CONV
      )) %>%
      dplyr::mutate(GRAV_CONV = as.factor(.data$GRAV_CONV))
  }

  # GRAV_ENCH
  if ("GRAV_ENCH" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_ENCH = dplyr::case_match(
        .data$GRAV_ENCH,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_ENCH
      )) %>%
      dplyr::mutate(GRAV_ENCH = as.factor(.data$GRAV_ENCH))
  }

  # GRAV_INSUF
  if ("GRAV_INSUF" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_INSUF = dplyr::case_match(
        .data$GRAV_INSUF,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_INSUF
      )) %>%
      dplyr::mutate(GRAV_INSUF = as.factor(.data$GRAV_INSUF))
  }

  # GRAV_TAQUI
  if ("GRAV_TAQUI" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_TAQUI = dplyr::case_match(
        .data$GRAV_TAQUI,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_TAQUI
      )) %>%
      dplyr::mutate(GRAV_TAQUI = as.factor(.data$GRAV_TAQUI))
  }

  # GRAV_EXTRE
  if ("GRAV_EXTRE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_EXTRE = dplyr::case_match(
        .data$GRAV_EXTRE,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_EXTRE
      )) %>%
      dplyr::mutate(GRAV_EXTRE = as.factor(.data$GRAV_EXTRE))
  }

  # GRAV_HIPOT
  if ("GRAV_HIPOT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_HIPOT = dplyr::case_match(
        .data$GRAV_HIPOT,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_HIPOT
      )) %>%
      dplyr::mutate(GRAV_HIPOT = as.factor(.data$GRAV_HIPOT))
  }

  # GRAV_HEMAT
  if ("GRAV_HEMAT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_HEMAT = dplyr::case_match(
        .data$GRAV_HEMAT,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_HEMAT
      )) %>%
      dplyr::mutate(GRAV_HEMAT = as.factor(.data$GRAV_HEMAT))
  }

  # GRAV_MELEN
  if ("GRAV_MELEN" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_MELEN = dplyr::case_match(
        .data$GRAV_MELEN,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_MELEN
      )) %>%
      dplyr::mutate(GRAV_MELEN = as.factor(.data$GRAV_MELEN))
  }

  # GRAV_METRO
  if ("GRAV_METRO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_METRO = dplyr::case_match(
        .data$GRAV_METRO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_METRO
      )) %>%
      dplyr::mutate(GRAV_METRO = as.factor(.data$GRAV_METRO))
  }

  # GRAV_SANG
  if ("GRAV_SANG" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_SANG = dplyr::case_match(
        .data$GRAV_SANG,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_SANG
      )) %>%
      dplyr::mutate(GRAV_SANG = as.factor(.data$GRAV_SANG))
  }

  # GRAV_AST
  if ("GRAV_AST" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_AST = dplyr::case_match(
        .data$GRAV_AST,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_AST
      )) %>%
      dplyr::mutate(GRAV_AST = as.factor(.data$GRAV_AST))
  }

  # GRAV_MIOC
  if ("GRAV_MIOC" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_MIOC = dplyr::case_match(
        .data$GRAV_MIOC,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_MIOC
      )) %>%
      dplyr::mutate(GRAV_MIOC = as.factor(.data$GRAV_MIOC))
  }

  # GRAV_CONSC
  if ("GRAV_CONSC" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_CONSC = dplyr::case_match(
        .data$GRAV_CONSC,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_CONSC
      )) %>%
      dplyr::mutate(GRAV_CONSC = as.factor(.data$GRAV_CONSC))
  }

  # GRAV_ORGAO
  if ("GRAV_ORGAO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GRAV_ORGAO = dplyr::case_match(
        .data$GRAV_ORGAO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        .default = .data$GRAV_ORGAO
      )) %>%
      dplyr::mutate(GRAV_ORGAO = as.factor(.data$GRAV_ORGAO))
  }

  # DT_GRAV
  if ("DT_GRAV" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_GRAV = as.Date(.data$DT_GRAV))
  }

  # MANI_HEMOR
  if ("MANI_HEMOR" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(MANI_HEMOR = dplyr::case_match(
        .data$MANI_HEMOR,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$MANI_HEMOR
      )) %>%
      dplyr::mutate(MANI_HEMOR = as.factor(.data$MANI_HEMOR))
  }

  # EPISTAXE
  if ("EPISTAXE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(EPISTAXE = dplyr::case_match(
        .data$EPISTAXE,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$EPISTAXE
      )) %>%
      dplyr::mutate(EPISTAXE = as.factor(.data$EPISTAXE))
  }

  # GENGIVO
  if ("GENGIVO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(GENGIVO = dplyr::case_match(
        .data$GENGIVO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$GENGIVO
      )) %>%
      dplyr::mutate(GENGIVO = as.factor(.data$GENGIVO))
  }

  # METRO
  if ("METRO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(METRO = dplyr::case_match(
        .data$METRO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$METRO
      )) %>%
      dplyr::mutate(METRO = as.factor(.data$METRO))
  }

  # PETEQUIAS
  if ("PETEQUIAS" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(PETEQUIAS = dplyr::case_match(
        .data$PETEQUIAS,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$PETEQUIAS
      )) %>%
      dplyr::mutate(PETEQUIAS = as.factor(.data$PETEQUIAS))
  }

  # HEMATURA
  if ("HEMATURA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(HEMATURA = dplyr::case_match(
        .data$HEMATURA,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$HEMATURA
      )) %>%
      dplyr::mutate(HEMATURA = as.factor(.data$HEMATURA))
  }

  # SANGRAM
  if ("SANGRAM" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(SANGRAM = dplyr::case_match(
        .data$SANGRAM,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$SANGRAM
      )) %>%
      dplyr::mutate(SANGRAM = as.factor(.data$SANGRAM))
  }

  # LACO_N
  if ("LACO_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(LACO_N = dplyr::case_match(
        .data$LACO_N,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$LACO_N
      )) %>%
      dplyr::mutate(LACO_N = as.factor(.data$LACO_N))
  }

  # PLASMATICO
  if ("PLASMATICO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(PLASMATICO = dplyr::case_match(
        .data$PLASMATICO,
        "1" ~ "Sim",
        "2" ~ "N\u00e3o",
        "9" ~ "Ignorado",
        .default = .data$PLASMATICO
      )) %>%
      dplyr::mutate(PLASMATICO = as.factor(.data$PLASMATICO))
  }

  # EVIDENCIA
  if ("EVIDENCIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(EVIDENCIA = dplyr::case_match(
        .data$EVIDENCIA,
        "1" ~ "Hemoconcentra\u00e7\u00e3o",
        "2" ~ "Derrames cavit\u00e1rios",
        "3" ~ "Hipoproteinemia",
        .default = .data$EVIDENCIA
      )) %>%
      dplyr::mutate(EVIDENCIA = as.factor(.data$EVIDENCIA))
  }

  # CON_FHD
  if ("CON_FHD" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CON_FHD = dplyr::case_match(
        .data$CON_FHD,
        "1" ~ "Grau I",
        "2" ~ "Grau II",
        "3" ~ "Grau III",
        "4" ~ "Grau IV",
        .default = .data$CON_FHD
      )) %>%
      dplyr::mutate(CON_FHD = as.factor(.data$CON_FHD))
  }

  # COMPLICA
  if ("COMPLICA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(COMPLICA = dplyr::case_match(
        .data$COMPLICA,
        "1" ~ "Altera\u00e7\u00f5es neurol\u00f3gicas",
        "2" ~ "Disfun\u00e7\u00e3o cardiorespirat\u00f3ria",
        "3" ~ "Insufici\u00eancia hep\u00e1tica",
        "4" ~ "Plaquetas <50.000mm",
        "5" ~ "Hemorragia digestiva",
        "6" ~ "Derrames cavit\u00e1rios",
        "7" ~ "Leucometria < 100",
        "8" ~ "N\u00e3o se enquadra nos crit\u00e9rios de FHD",
        .default = .data$COMPLICA
      )) %>%
      dplyr::mutate(COMPLICA = as.factor(.data$COMPLICA))
  }

  # NDUPLIC_N
  if ("NDUPLIC_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(NDUPLIC_N = dplyr::case_match(
        .data$NDUPLIC_N,
        "0" ~ "N\u00e3o identificado",
        "" ~ "N\u00e3o identificado",
        "1" ~ "N\u00e3o \u00e9 duplicidade (n\u00e3o listar)",
        "2" ~ "Duplicidade (n\u00e3o contar)",
        .default = .data$NDUPLIC_N
      )) %>%
      dplyr::mutate(NDUPLIC_N = as.factor(.data$NDUPLIC_N))
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
