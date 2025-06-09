#' Process SINAN Chagas variables from DataSUS
#'
#' \code{process_sinan_chagas} processes SINAN Chagas variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SINAN Chagas variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples
#' process_sinan_malaria(sinan_chagas_sample)
#'
#' @return a \code{data.frame} with the processed data.
#'
#' @export

process_sinan_chagas <- function(data, municipality_data = TRUE) {
  # Variables names
  variables_names <- names(data)

  # Use dtplyr
  data <- dtplyr::lazy_dt(data)

  # TP_NOT
  if ("TP_NOT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TP_NOT = dplyr::case_match(
          .data$TP_NOT,
          "1" ~ "Negativa",
          "2" ~ "Individual",
          "3" ~ "Surto",
          "4" ~ "Agregado",
          .default = .data$TP_NOT
        )
      ) %>%
      dplyr::mutate(TP_NOT = as.factor(.data$TP_NOT))
  }

  # DT_NOTIFIC
  if ("DT_NOTIFIC" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_NOTIFIC = as.Date(.data$DT_NOTIFIC))
  }

  # DT_SIN_PRI
  if ("DT_SIN_PRI" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_SIN_PRI = as.Date(.data$DT_SIN_PRI))
  }

  # DT_DIGITA
  if ("DT_DIGITA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_DIGITA = as.Date(.data$DT_DIGITA))
  }

  # DT_INVEST
  if ("DT_INVEST" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_INVEST = as.Date(.data$DT_INVEST))
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

  # DT_DESLC1
  if ("DT_DESLC1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_DESLC1 = as.Date(.data$DT_DESLC1))
  }

  # DT_DESLC2
  if ("DT_DESLC2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_DESLC2 = as.Date(.data$DT_DESLC2))
  }

  # DT_DESLC3
  if ("DT_DESLC3" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_DESLC3 = as.Date(.data$DT_DESLC3))
  }

  # DT_COL_DIR
  if ("DT_COL_DIR" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_COL_DIR = as.Date(.data$DT_COL_DIR))
  }

  # DT_COL_IND
  if ("DT_COL_IND" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_COL_IND = as.Date(.data$DT_COL_IND))
  }

  # DT_COL_S1
  if ("DT_COL_S1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_COL_S1 = as.Date(.data$DT_COL_S1))
  }

  # DT_COL_S2
  if ("DT_COL_S2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_COL_S2 = as.Date(.data$DT_COL_S2))
  }

  # DT_TRANSUS
  if ("DT_TRANSUS" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_TRANSUS = as.Date(.data$DT_TRANSUS))
  }

  # DT_TRANSDM
  if ("DT_TRANSDM" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_TRANSDM = as.Date(.data$DT_TRANSDM))
  }

  # DT_TRANSSM
  if ("DT_TRANSSM" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_TRANSSM = as.Date(.data$DT_TRANSSM))
  }

  # DT_TRANSRM
  if ("DT_TRANSRM" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_TRANSRM = as.Date(.data$DT_TRANSRM))
  }

  # DT_TRANSRS
  if ("DT_TRANSRS" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_TRANSRS = as.Date(.data$DT_TRANSRS))
  }

  # DT_TRANSSE
  if ("DT_TRANSSE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_TRANSSE = as.Date(.data$DT_TRANSSE))
  }

  # SEM_NOT
  if ("SEM_NOT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        SEM_NOT = dplyr::case_match(
          .data$SEM_NOT,
          "1" ~ "Semana 1",
          "2" ~ "Semana 2",
          "3" ~ "Semana 3",
          "4" ~ "Semana 4",
          "5" ~ "Semana 5",
          "6" ~ "Semana 6",
          "7" ~ "Semana 7",
          "8" ~ "Semana 8",
          "9" ~ "Semana 9",
          "10" ~ "Semana 10",
          "11" ~ "Semana 11",
          "12" ~ "Semana 12",
          "13" ~ "Semana 13",
          "14" ~ "Semana 14",
          "15" ~ "Semana 15",
          "16" ~ "Semana 16",
          "17" ~ "Semana 17",
          "18" ~ "Semana 18",
          "19" ~ "Semana 19",
          "20" ~ "Semana 20",
          "21" ~ "Semana 21",
          "22" ~ "Semana 22",
          "23" ~ "Semana 23",
          "24" ~ "Semana 24",
          "25" ~ "Semana 25",
          "26" ~ "Semana 26",
          "27" ~ "Semana 27",
          "28" ~ "Semana 28",
          "29" ~ "Semana 29",
          "30" ~ "Semana 30",
          "31" ~ "Semana 31",
          "32" ~ "Semana 32",
          "33" ~ "Semana 33",
          "34" ~ "Semana 34",
          "35" ~ "Semana 35",
          "36" ~ "Semana 36",
          "37" ~ "Semana 37",
          "38" ~ "Semana 38",
          "39" ~ "Semana 39",
          "40" ~ "Semana 40",
          "41" ~ "Semana 41",
          "42" ~ "Semana 42",
          "43" ~ "Semana 43",
          "44" ~ "Semana 44",
          "45" ~ "Semana 45",
          "46" ~ "Semana 46",
          "47" ~ "Semana 47",
          "48" ~ "Semana 48",
          "49" ~ "Semana 49",
          "50" ~ "Semana 50",
          "51" ~ "Semana 51",
          "52" ~ "Semana 52",
          "53" ~ "Semana 53",
          "54" ~ "Em branco",
          .default = .data$SEM_NOT
        )
      ) %>%
      dplyr::mutate(SEM_NOT = as.factor(.data$SEM_NOT))
  }

  # SG_UF_NOT
  if ("SG_UF_NOT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        SG_UF_NOT = dplyr::case_match(
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
        )
      ) %>%
      dplyr::mutate(SG_UF_NOT = as.factor(.data$SG_UF_NOT))
  }

  # SEM_PRI
  if ("SEM_PRI" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        SEM_PRI = dplyr::case_match(
          .data$SEM_PRI,
          "1" ~ "Semana 1",
          "2" ~ "Semana 2",
          "3" ~ "Semana 3",
          "4" ~ "Semana 4",
          "5" ~ "Semana 5",
          "6" ~ "Semana 6",
          "7" ~ "Semana 7",
          "8" ~ "Semana 8",
          "9" ~ "Semana 9",
          "10" ~ "Semana 10",
          "11" ~ "Semana 11",
          "12" ~ "Semana 12",
          "13" ~ "Semana 13",
          "14" ~ "Semana 14",
          "15" ~ "Semana 15",
          "16" ~ "Semana 16",
          "17" ~ "Semana 17",
          "18" ~ "Semana 18",
          "19" ~ "Semana 19",
          "20" ~ "Semana 20",
          "21" ~ "Semana 21",
          "22" ~ "Semana 22",
          "23" ~ "Semana 23",
          "24" ~ "Semana 24",
          "25" ~ "Semana 25",
          "26" ~ "Semana 26",
          "27" ~ "Semana 27",
          "28" ~ "Semana 28",
          "29" ~ "Semana 29",
          "30" ~ "Semana 30",
          "31" ~ "Semana 31",
          "32" ~ "Semana 32",
          "33" ~ "Semana 33",
          "34" ~ "Semana 34",
          "35" ~ "Semana 35",
          "36" ~ "Semana 36",
          "37" ~ "Semana 37",
          "38" ~ "Semana 38",
          "39" ~ "Semana 39",
          "40" ~ "Semana 40",
          "41" ~ "Semana 41",
          "42" ~ "Semana 42",
          "43" ~ "Semana 43",
          "44" ~ "Semana 44",
          "45" ~ "Semana 45",
          "46" ~ "Semana 46",
          "47" ~ "Semana 47",
          "48" ~ "Semana 48",
          "49" ~ "Semana 49",
          "50" ~ "Semana 50",
          "51" ~ "Semana 51",
          "52" ~ "Semana 52",
          "53" ~ "Semana 53",
          "54" ~ "Em branco",
          .default = .data$SEM_PRI
        )
      ) %>%
      dplyr::mutate(SEM_PRI = as.factor(.data$SEM_PRI))
  }

  # IDADE
  if ("NU_IDADE_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        NU_IDADE_N = dplyr::case_match(
          .data$NU_IDADE_N,
          999 ~ NA,
          .default = .data$NU_IDADE_N
        )
      ) %>%
      # Codigo e valor
      dplyr::mutate(
        idade_cod = substr(.data$NU_IDADE_N, 1, 1),
        idade_value = as.numeric(substr(.data$NU_IDADE_N, 2, 3)),
      ) %>%
      dplyr::mutate(
        IDADEminutos = dplyr::case_match(
          .data$idade_cod,
          "0" ~ idade_value,
          .default = NA
        )
      ) %>%
      dplyr::mutate(
        IDADEhoras = dplyr::case_match(
          .data$idade_cod,
          "1" ~ idade_value,
          .default = NA
        )
      ) %>%
      dplyr::mutate(
        IDADEdias = dplyr::case_match(
          .data$idade_cod,
          "2" ~ idade_value,
          .default = NA
        )
      ) %>%
      dplyr::mutate(
        IDADEmeses = dplyr::case_match(
          .data$idade_cod,
          "3" ~ idade_value,
          .default = NA
        )
      ) %>%
      dplyr::mutate(
        IDADEanos = dplyr::case_match(
          .data$idade_cod,
          "4" ~ idade_value,
          "5" ~ idade_value + 100,
          .default = NA
        )
      ) %>%
      dplyr::select(-"idade_cod", -"idade_value")
  } else if ("NU_IDADE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        idade_cod = substr(.data$NU_IDADE, 0, 1),
        idade_value = as.numeric(substr(.data$NU_IDADE, 2, 4)),
      ) %>%
      dplyr::mutate(
        IDADEdias = dplyr::case_match(
          .data$idade_cod,
          "D" ~ idade_value,
          .default = NA
        )
      ) %>%
      dplyr::mutate(
        IDADEmeses = dplyr::case_match(
          .data$idade_cod,
          "M" ~ idade_value,
          .default = NA
        )
      ) %>%
      dplyr::mutate(
        IDADEanos = dplyr::case_match(
          .data$idade_cod,
          "A" ~ idade_value,
          .default = NA
        )
      ) %>%
      dplyr::select(-"idade_cod", -"idade_value")
  }

  # CS_SEXO
  if ("CS_SEXO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CS_SEXO = dplyr::case_match(
          .data$CS_SEXO,
          "M" ~ "Masculino",
          "F" ~ "Feminino",
          "I" ~ "Ignorado",
          .default = .data$CS_SEXO
        )
      ) %>%
      dplyr::mutate(CS_SEXO = as.factor(.data$CS_SEXO))
  }

  # CS_GESTANT
  if ("CS_GESTANT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CS_GESTANT = dplyr::case_match(
          .data$CS_GESTANT,
          "1" ~ "1o trimestre",
          "2" ~ "2o trimestre",
          "3" ~ "3o trimestre",
          "4" ~ "Idade gestacional ignorada",
          "5" ~ "N\u00e3o",
          "6" ~ "N\u00e3o se aplica",
          "9" ~ "Ignorado",
          .default = .data$CS_GESTANT
        )
      ) %>%
      dplyr::mutate(CS_GESTANT = as.factor(.data$CS_GESTANT))
  }

  # CS_RACA
  if ("CS_RACA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CS_RACA = dplyr::case_match(
          .data$CS_RACA,
          "1" ~ "Branca",
          "2" ~ "Preta",
          "3" ~ "Amarela",
          "4" ~ "Parda",
          "5" ~ "Ind\u00edgena",
          "9" ~ "Ignorado",
          .default = .data$CS_RACA
        )
      ) %>%
      dplyr::mutate(CS_RACA = as.factor(.data$CS_RACA))
  }

  # CS_ESCOL_N
  if ("CS_ESCOL_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CS_ESCOL_N = dplyr::case_match(
          .data$CS_ESCOL_N,
          "1" ~ "1a a 4a s\u00e9rie incompleta do EF",
          "2" ~ "4a s\u00e9rie completa do EF (antigo 1o grau)",
          "3" ~
            "5a \u00e0 8a s\u00e9rie incompleta do EF (antigo gin\u00e1sio ou 1o grau)",
          "4" ~ "Ensino fundamental completo (antigo gin\u00e1sio ou 1o grau)",
          "5" ~ "Ensino m\u00e9dio incompleto (antigo colegial ou 2o grau)",
          "6" ~ "Ensino m\u00e9dio completo (antigo colegial ou 2o grau)",
          "7" ~ "Educa\u00e7\u00e3o superior incompleta",
          "8" ~ "Educa\u00e7\u00e3o superior completa",
          "9" ~ "Ignorado",
          "10" ~ "N\u00e3o se aplica",
          .default = .data$CS_ESCOL_N
        )
      ) %>%
      dplyr::mutate(CS_ESCOL_N = as.factor(.data$CS_ESCOL_N))
  }

  # SG_UF
  if ("SG_UF" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        SG_UF = dplyr::case_match(
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
        )
      ) %>%
      dplyr::mutate(SG_UF = as.factor(.data$SG_UF))
  }

  # ID_PAIS
  if ("ID_PAIS" %in% variables_names) {
    data$ID_PAIS <- dplyr::left_join(
      data,
      microdatasus::paisnet,
      by = c("ID_PAIS" = "ID_PAIS")
    )$NM_PAIS
  }

  # ID_OCUPA_N
  if ("ID_OCUPA_N" %in% variables_names) {
    data$ID_OCUPA_N <- factor(
      dplyr::left_join(
        data,
        microdatasus::tabCBO,
        by = c("ID_OCUPA_N" = "cod")
      )$nome
    )
  }

  # PRESENCA
  if ("PRESENCA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        PRESENCA = dplyr::case_match(
          .data$PRESENCA,
          "9" ~ NA,
          "1" ~ "Sim",
          "2" ~ "N\\u00e3o",
          "3" ~ "N\\u00e3o se aplica",
          .default = .data$PRESENCA
        )
      ) %>%
      dplyr::mutate(PRESENCA = as.factor(.data$PRESENCA))
  }

  # HISTORIA
  if ("HISTORIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        HISTORIA = dplyr::case_match(
          .data$HISTORIA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$HISTORIA
        )
      ) %>%
      dplyr::mutate(HISTORIA = as.factor(.data$HISTORIA))
  }

  # CONTROLE
  if ("CONTROLE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CONTROLE = dplyr::case_match(
          .data$CONTROLE,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$CONTROLE
        )
      ) %>%
      dplyr::mutate(CONTROLE = as.factor(.data$CONTROLE))
  }

  # MANIPULA
  if ("MANIPULA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        MANIPULA = dplyr::case_match(
          .data$MANIPULA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "3" ~ "N\u00e3o se aplica",
          "9" ~ NA,
          .default = .data$MANIPULA
        )
      ) %>%
      dplyr::mutate(MANIPULA = as.factor(.data$MANIPULA))
  }

  # MAECHAGA
  if ("MAECHAGA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        MAECHAGA = dplyr::case_match(
          .data$MAECHAGA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "3" ~ "N\u00e3o se aplica",
          "9" ~ NA,
          .default = .data$MAECHAGA
        )
      ) %>%
      dplyr::mutate(MAECHAGA = as.factor(.data$MAECHAGA))
  }

  # ORAL
  if ("ORAL" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ORAL = dplyr::case_match(
          .data$ORAL,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$ORAL
        )
      ) %>%
      dplyr::mutate(ORAL = as.factor(.data$ORAL))
  }

  # ASSINTOMA
  if ("ASSINTOMA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ASSINTOMA = dplyr::case_match(
          .data$ASSINTOMA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$ASSINTOMA
        )
      ) %>%
      dplyr::mutate(ASSINTOMA = as.factor(.data$ASSINTOMA))
  }

  # EDEMA
  if ("EDEMA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        EDEMA = dplyr::case_match(
          .data$EDEMA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$EDEMA
        )
      ) %>%
      dplyr::mutate(EDEMA = as.factor(.data$EDEMA))
  }

  # MENINGOE
  if ("MENINGOE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        MENINGOE = dplyr::case_match(
          .data$MENINGOE,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$MENINGOE
        )
      ) %>%
      dplyr::mutate(MENINGOE = as.factor(.data$MENINGOE))
  }

  # FEBRE
  if ("FEBRE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        FEBRE = dplyr::case_match(
          .data$FEBRE,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$FEBRE
        )
      ) %>%
      dplyr::mutate(FEBRE = as.factor(.data$FEBRE))
  }

  # HEPATOME
  if ("HEPATOME" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        HEPATOME = dplyr::case_match(
          .data$HEPATOME,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$HEPATOME
        )
      ) %>%
      dplyr::mutate(HEPATOME = as.factor(.data$HEPATOME))
  }

  # SINAIS_ICC
  if ("SINAIS_ICC" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        SINAIS_ICC = dplyr::case_match(
          .data$SINAIS_ICC,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$SINAIS_ICC
        )
      ) %>%
      dplyr::mutate(SINAIS_ICC = as.factor(.data$SINAIS_ICC))
  }

  # ARRITMIAS
  if ("ARRITMIAS" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ARRITMIAS = dplyr::case_match(
          .data$ARRITMIAS,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$ARRITMIAS
        )
      ) %>%
      dplyr::mutate(ARRITMIAS = as.factor(.data$ARRITMIAS))
  }

  # ASTENIA
  if ("ASTENIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ASTENIA = dplyr::case_match(
          .data$ASTENIA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$ASTENIA
        )
      ) %>%
      dplyr::mutate(ASTENIA = as.factor(.data$ASTENIA))
  }

  # ESPLENOM
  if ("ESPLENOM" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ESPLENOM = dplyr::case_match(
          .data$ESPLENOM,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$ESPLENOM
        )
      ) %>%
      dplyr::mutate(ESPLENOM = as.factor(.data$ESPLENOM))
  }

  # CHAGOMA
  if ("CHAGOMA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CHAGOMA = dplyr::case_match(
          .data$CHAGOMA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$CHAGOMA
        )
      ) %>%
      dplyr::mutate(CHAGOMA = as.factor(.data$CHAGOMA))
  }

  # EXAME
  if ("EXAME" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        EXAME = dplyr::case_match(
          .data$EXAME,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "3" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$EXAME
        )
      ) %>%
      dplyr::mutate(EXAME = as.factor(.data$EXAME))
  }

  # MICRO_HEMA
  if ("MICRO_HEMA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        MICRO_HEMA = dplyr::case_match(
          .data$MICRO_HEMA,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "3" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$MICRO_HEMA
        )
      ) %>%
      dplyr::mutate(MICRO_HEMA = as.factor(.data$MICRO_HEMA))
  }

  # OUTRO
  if ("OUTRO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        OUTRO = dplyr::case_match(
          .data$OUTRO,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "3" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$OUTRO
        )
      ) %>%
      dplyr::mutate(OUTRO = as.factor(.data$OUTRO))
  }

  # XENODIAG
  if ("XENODIAG" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        XENODIAG = dplyr::case_match(
          .data$XENODIAG,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "3" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$XENODIAG
        )
      ) %>%
      dplyr::mutate(XENODIAG = as.factor(.data$XENODIAG))
  }

  # HEMOCULT
  if ("HEMOCULT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        HEMOCULT = dplyr::case_match(
          .data$HEMOCULT,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "3" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$HEMOCULT
        )
      ) %>%
      dplyr::mutate(HEMOCULT = as.factor(.data$HEMOCULT))
  }

  # ELI_IGM_S1
  if ("ELI_IGM_S1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ELI_IGM_S1 = dplyr::case_match(
          .data$ELI_IGM_S1,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$ELI_IGM_S1
        )
      ) %>%
      dplyr::mutate(ELI_IGM_S1 = as.factor(.data$ELI_IGM_S1))
  }

  # ELI_IGG_S1
  if ("ELI_IGG_S1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ELI_IGG_S1 = dplyr::case_match(
          .data$ELI_IGG_S1,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$ELI_IGG_S1
        )
      ) %>%
      dplyr::mutate(ELI_IGG_S1 = as.factor(.data$ELI_IGG_S1))
  }

  # ELI_IGM_S2
  if ("ELI_IGM_S2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ELI_IGM_S2 = dplyr::case_match(
          .data$ELI_IGM_S2,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$ELI_IGM_S2
        )
      ) %>%
      dplyr::mutate(ELI_IGM_S2 = as.factor(.data$ELI_IGM_S2))
  }

  # ELI_IGG_S2
  if ("ELI_IGG_S2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ELI_IGG_S2 = dplyr::case_match(
          .data$ELI_IGG_S2,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$ELI_IGG_S2
        )
      ) %>%
      dplyr::mutate(ELI_IGG_S2 = as.factor(.data$ELI_IGG_S2))
  }

  # HEM_IGM_S1
  if ("HEM_IGM_S1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        HEM_IGM_S1 = dplyr::case_match(
          .data$HEM_IGM_S1,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$HEM_IGM_S1
        )
      ) %>%
      dplyr::mutate(HEM_IGM_S1 = as.factor(.data$HEM_IGM_S1))
  }

  # HEM_IGG_S1
  if ("HEM_IGG_S1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        HEM_IGG_S1 = dplyr::case_match(
          .data$HEM_IGG_S1,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$HEM_IGM_S1
        )
      ) %>%
      dplyr::mutate(HEM_IGG_S1 = as.factor(.data$HEM_IGG_S1))
  }

  # HEM_IGM_S2
  if ("HEM_IGM_S2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        HEM_IGM_S2 = dplyr::case_match(
          .data$HEM_IGM_S2,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$HEM_IGM_S1
        )
      ) %>%
      dplyr::mutate(HEM_IGM_S2 = as.factor(.data$HEM_IGM_S2))
  }

  # HEM_IGG_S2
  if ("HEM_IGG_S2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        HEM_IGG_S2 = dplyr::case_match(
          .data$HEM_IGG_S2,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$HEM_IGM_S1
        )
      ) %>%
      dplyr::mutate(HEM_IGG_S2 = as.factor(.data$HEM_IGG_S2))
  }

  # IMU_IGM_S1
  if ("IMU_IGM_S1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        IMU_IGM_S1 = dplyr::case_match(
          .data$IMU_IGM_S1,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$IMU_IGM_S1
        )
      ) %>%
      dplyr::mutate(IMU_IGM_S1 = as.factor(.data$IMU_IGM_S1))
  }

  # TIT_IGM_S1
  if ("TIT_IGM_S1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TIT_IGM_S1 = dplyr::case_match(
          .data$TIT_IGM_S1,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$TIT_IGM_S1
        )
      ) %>%
      dplyr::mutate(TIT_IGM_S1 = as.factor(.data$TIT_IGM_S1))
  }

  # IMU_IGM_S2
  if ("IMU_IGM_S2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        IMU_IGM_S2 = dplyr::case_match(
          .data$IMU_IGM_S2,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$IMU_IGM_S2
        )
      ) %>%
      dplyr::mutate(IMU_IGM_S2 = as.factor(.data$IMU_IGM_S2))
  }

  # TIT_IGM_S2
  if ("TIT_IGM_S2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TIT_IGM_S2 = dplyr::case_match(
          .data$TIT_IGM_S2,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$TIT_IGM_S2
        )
      ) %>%
      dplyr::mutate(TIT_IGM_S2 = as.factor(.data$TIT_IGM_S2))
  }

  # IMU_IGG_S1
  if ("IMU_IGG_S1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        IMU_IGG_S1 = dplyr::case_match(
          .data$IMU_IGG_S1,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$IMU_IGG_S1
        )
      ) %>%
      dplyr::mutate(IMU_IGG_S1 = as.factor(.data$IMU_IGG_S1))
  }

  # TIT_IGG_S1
  if ("TIT_IGG_S1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TIT_IGG_S1 = dplyr::case_match(
          .data$TIT_IGG_S1,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$TIT_IGG_S1
        )
      ) %>%
      dplyr::mutate(TIT_IGG_S1 = as.factor(.data$TIT_IGG_S1))
  }

  # IMU_IGG_S2
  if ("IMU_IGG_S2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        IMU_IGG_S2 = dplyr::case_match(
          .data$IMU_IGG_S2,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$IMU_IGG_S2
        )
      ) %>%
      dplyr::mutate(IMU_IGG_S2 = as.factor(.data$IMU_IGG_S2))
  }

  # TIT_IGG_S2
  if ("TIT_IGG_S2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TIT_IGG_S2 = dplyr::case_match(
          .data$TIT_IGG_S2,
          "1" ~ "Reagente",
          "2" ~ "N\u00e3o reagente",
          "3" ~ "Inconclusivo",
          "4" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$TIT_IGG_S2
        )
      ) %>%
      dplyr::mutate(TIT_IGG_S2 = as.factor(.data$TIT_IGG_S2))
  }

  # RES_HIST
  if ("RES_HIST" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        RES_HIST = dplyr::case_match(
          .data$RES_HIST,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "3" ~ "N\u00e3o realizado",
          "9" ~ NA,
          .default = .data$RES_HIST
        )
      ) %>%
      dplyr::mutate(RES_HIST = as.factor(.data$RES_HIST))
  }

  # ESPECIFICO
  if ("ESPECIFICO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ESPECIFICO = dplyr::case_match(
          .data$ESPECIFICO,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$ESPECIFICO
        )
      ) %>%
      dplyr::mutate(ESPECIFICO = as.factor(.data$ESPECIFICO))
  }

  # SINTOMATIC
  if ("SINTOMATIC" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        SINTOMATIC = dplyr::case_match(
          .data$SINTOMATIC,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$SINTOMATIC
        )
      ) %>%
      dplyr::mutate(SINTOMATIC = as.factor(.data$SINTOMATIC))
  }

  # DROGA
  if ("DROGA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        DROGA = dplyr::case_match(
          .data$DROGA,
          "1" ~ "Benznidazol",
          "2" ~ "Outro",
          "9" ~ NA,
          .default = .data$DROGA
        )
      ) %>%
      dplyr::mutate(DROGA = as.factor(.data$DROGA))
  }

  # CON_TRIAT
  if ("CON_TRIAT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CON_TRIAT = dplyr::case_match(
          .data$CON_TRIAT,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "3" ~ "N\u00e3o se aplica",
          "9" ~ NA,
          .default = .data$CON_TRIAT
        )
      ) %>%
      dplyr::mutate(CON_TRIAT = as.factor(.data$CON_TRIAT))
  }

  # BIOSSEG
  if ("BIOSSEG" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        BIOSSEG = dplyr::case_match(
          .data$BIOSSEG,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "3" ~ "N\u00e3o se aplica",
          "9" ~ NA,
          .default = .data$BIOSSEG
        )
      ) %>%
      dplyr::mutate(BIOSSEG = as.factor(.data$BIOSSEG))
  }

  # FISCALIZA
  if ("FISCALIZA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        FISCALIZA = dplyr::case_match(
          .data$FISCALIZA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "3" ~ "N\u00e3o se aplica",
          "9" ~ NA,
          .default = .data$FISCALIZA
        )
      ) %>%
      dplyr::mutate(FISCALIZA = as.factor(.data$FISCALIZA))
  }

  # MED_OUTRO
  if ("MED_OUTRO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        MED_OUTRO = dplyr::case_match(
          .data$MED_OUTRO,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "3" ~ "N\u00e3o se aplica",
          "9" ~ NA,
          .default = .data$MED_OUTRO
        )
      ) %>%
      dplyr::mutate(MED_OUTRO = as.factor(.data$MED_OUTRO))
  }

  # CLASSI_FIN
  if ("CLASSI_FIN" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CLASSI_FIN = dplyr::case_match(
          .data$CLASSI_FIN,
          "9" ~ NA,
          "1" ~ "Confirmado",
          "2" ~ "Descartado",
          "8" ~ "Inconclusivo",
          .default = .data$CLASSI_FIN
        )
      ) %>%
      dplyr::mutate(CLASSI_FIN = as.factor(.data$CLASSI_FIN))
  }

  # CRITERIO
  if ("CRITERIO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CRITERIO = dplyr::case_match(
          .data$CRITERIO,
          "9" ~ NA,
          "1" ~ "Laboratorial",
          "2" ~ "Cl\\u00ednico-epidemiol\\u00f3gico",
          .default = .data$CRITERIO
        )
      ) %>%
      dplyr::mutate(CRITERIO = as.factor(.data$CRITERIO))
  }

  # CON_PROVAV
  if ("CON_PROVAV" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CON_PROVAV = dplyr::case_match(
          .data$CON_PROVAV,
          "9" ~ NA,
          "1" ~ "Transfusional",
          "2" ~ "Vetorial",
          "3" ~ "Vertical",
          "4" ~ "Acidental",
          "5" ~ "Oral",
          "6" ~ "Outro",
          .default = .data$CON_PROVAV
        )
      ) %>%
      dplyr::mutate(CON_PROVAV = as.factor(.data$CON_PROVAV))
  }

  # CON_LOCAL
  if ("CON_LOCAL" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CON_LOCAL = dplyr::case_match(
          .data$CON_LOCAL,
          "9" ~ NA,
          "1" ~ "Unidade de Hemoterapia",
          "2" ~ "Domic\\u00edlio",
          "3" ~ "Laborat\\u00f3rio",
          "4" ~ "Outro",
          .default = .data$CON_LOCAL
        )
      ) %>%
      dplyr::mutate(CON_LOCAL = as.factor(.data$CON_LOCAL))
  }

  # TPAUTOCTO
  if ("TPAUTOCTO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TPAUTOCTO = dplyr::case_match(
          .data$TPAUTOCTO,
          "9" ~ NA,
          "1" ~ "Sim",
          "2" ~ "N\\u00e3o",
          "3" ~ "Indeterminado",
          .default = .data$TPAUTOCTO
        )
      ) %>%
      dplyr::mutate(TPAUTOCTO = as.factor(.data$TPAUTOCTO))
  }

  # COUFINF
  if ("COUFINF" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        COUFINF = dplyr::case_match(
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
        )
      ) %>%
      dplyr::mutate(COUFINF = as.factor(.data$COUFINF))
  }

  # COPAISINF
  if ("COPAISINF" %in% variables_names) {
    data$COPAISINF <- dplyr::left_join(
      data,
      microdatasus::paisnet,
      by = c("COPAISINF" = "COPAISINF")
    )$NM_PAIS
  }

  # DOENCA_TRA
  if ("DOENCA_TRA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        DOENCA_TRA = dplyr::case_match(
          .data$DOENCA_TRA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$DOENCA_TRA
        )
      ) %>%
      dplyr::mutate(DOENCA_TRA = as.factor(.data$DOENCA_TRA))
  }

  # EVOLUCAO
  if ("EVOLUCAO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        EVOLUCAO = dplyr::case_match(
          .data$EVOLUCAO,
          "9" ~ NA,
          "1" ~ "Vivo",
          "2" ~ "\\u00d3bito pelo agravo notificado",
          "3" ~ "\\u00d3bito por outra causa",
          .default = .data$EVOLUCAO
        )
      ) %>%
      dplyr::mutate(EVOLUCAO = as.factor(.data$EVOLUCAO))
  }

  # From data.table to tibble
  data <- tibble::as_tibble(data)

  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  data <- suppressWarnings(tibble::as_tibble(lapply(
    X = data,
    FUN = stringi::stri_unescape_unicode
  )))

  # Return
  return(data)
}
