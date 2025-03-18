#' Process SINAN Leishmaniose Tegumentar variables from DataSUS
#'
#' \code{process_sinan_leishmaniose_tegumentar} processes SINAN Leishmaniose Tegumentar variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SINAN Leishmaniose Tegumentar variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples
#' process_sinan_leishmaniose_tegumentar(sinan_leishmaniose_tegumentar_sample)
#'
#' @return a \code{data.frame} with the processed data.
#'
#' @export

process_sinan_leishmaniose_tegumentar <- function(
  data,
  municipality_data = TRUE
) {
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

  # DT_INVEST
  if ("DT_INVEST" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_INVEST = as.Date(.data$DT_INVEST))
  }

  # DT_INIC_TR
  if ("DT_INIC_TR" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_INIC_TR = as.Date(.data$DT_INIC_TR))
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

  # DT_DESC1
  if ("DT_DESC1" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_DESC1 = as.Date(.data$DT_DESC1))
  }

  # DT_DESC2
  if ("DT_DESC2" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_DESC2 = as.Date(.data$DT_DESC2))
  }

  # DT_DESC3
  if ("DT_DESC3" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DT_DESC3 = as.Date(.data$DT_DESC3))
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
      dplyr::mutate(CS_ESCOL_N <- as.factor(.data$CS_ESCOL_N))
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

  # CLI_CUTANE
  if ("CLI_CUTANE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CLI_CUTANE = dplyr::case_match(
          .data$CLI_CUTANE,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$CLI_CUTANE
        )
      ) %>%
      dplyr::mutate(CLI_CUTANE = as.factor(.data$CLI_CUTANE))
  }

  # CLI_MUCOSA
  if ("CLI_MUCOSA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CLI_MUCOSA = dplyr::case_match(
          .data$CLI_MUCOSA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$CLI_MUCOSA
        )
      ) %>%
      dplyr::mutate(CLI_MUCOSA = as.factor(.data$CLI_MUCOSA))
  }

  # CLI_CICATR
  if ("CLI_CICATR" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CLI_CICATR = dplyr::case_match(
          .data$CLI_CICATR,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$CLI_CICATR
        )
      ) %>%
      dplyr::mutate(CLI_CICATR = as.factor(.data$CLI_CICATR))
  }

  # CLI_CO_HIV
  if ("CLI_CO_HIV" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CLI_CO_HIV = dplyr::case_match(
          .data$CLI_CO_HIV,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$CLI_CO_HIV
        )
      ) %>%
      dplyr::mutate(CLI_CO_HIV = as.factor(.data$CLI_CO_HIV))
  }

  # LAB_PARASI
  if ("LAB_PARASI" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        LAB_PARASI = dplyr::case_match(
          .data$LAB_PARASI,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "9" ~ NA,
          .default = .data$LAB_PARASI
        )
      ) %>%
      dplyr::mutate(LAB_PARASI = as.factor(.data$LAB_PARASI))
  }

  # LAB_IRM
  if ("LAB_IRM" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        LAB_IRM = dplyr::case_match(
          .data$LAB_IRM,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "9" ~ NA,
          .default = .data$LAB_IRM
        )
      ) %>%
      dplyr::mutate(LAB_IRM = as.factor(.data$LAB_IRM))
  }

  # LAB_HISTOP
  if ("LAB_HISTOP" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        LAB_HISTOP = dplyr::case_match(
          .data$LAB_HISTOP,
          "1" ~ "Encontro do parasita",
          "2" ~ "Compat\\u00edvel",
          "3" ~ "N\\u00e3o Compat\\u00edvel",
          "4" ~ "N\\u00e3o realizado",
          "9" ~ NA,
          .default = .data$LAB_HISTOP
        )
      ) %>%
      dplyr::mutate(LAB_HISTOP = as.factor(.data$LAB_HISTOP))
  }

  # CLA_TIPO_N
  if ("CLA_TIPO_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CLA_TIPO_N = dplyr::case_match(
          .data$CLA_TIPO_N,
          "1" ~ "Caso novo",
          "2" ~ "Recidiva",
          "3" ~ "Transfer\\u00eancia",
          "9" ~ NA,
          .default = .data$CLA_TIPO_N
        )
      ) %>%
      dplyr::mutate(CLA_TIPO_N = as.factor(.data$CLA_TIPO_N))
  }

  # CLAS_FORMA
  if ("CLAS_FORMA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CLAS_FORMA = dplyr::case_match(
          .data$CLAS_FORMA,
          "1" ~ "Cut\\u00e2nea",
          "2" ~ "Mucosa",
          "9" ~ NA,
          .default = .data$CLAS_FORMA
        )
      ) %>%
      dplyr::mutate(CLAS_FORMA = as.factor(.data$CLAS_FORMA))
  }

  # TRA_DROGA_
  if ("TRA_DROGA_" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TRA_DROGA_ = dplyr::case_match(
          .data$TRA_DROGA_,
          "1" ~ "Antimonial Pentavalente",
          "2" ~ "Anfotericina b",
          "3" ~ "Pentamidina",
          "4" ~ "Outras",
          "5" ~ "N\\u00e3o Utilizada",
          "9" ~ NA,
          .default = .data$TRA_DROGA_
        )
      ) %>%
      dplyr::mutate(TRA_DROGA_ = as.factor(.data$TRA_DROGA_))
  }

  # TRA_OUTR_N
  if ("TRA_OUTR_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TRA_OUTR_N = dplyr::case_match(
          .data$TRA_OUTR_N,
          "1" ~ "Antimonial Pentavalente",
          "2" ~ "Anfotericina b",
          "3" ~ "Pentamidina",
          "4" ~ "Outras",
          "5" ~ "N\\u00e3o se aplica",
          "9" ~ NA,
          .default = .data$TRA_OUTR_N
        )
      ) %>%
      dplyr::mutate(TRA_OUTR_N = as.factor(.data$TRA_OUTR_N))
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

  # CON_CLASS_
  if ("CON_CLASS_" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CON_CLASS_ = dplyr::case_match(
          .data$CON_CLASS_,
          "1" ~ "Aut\\u00f3ctone",
          "2" ~ "Importado",
          "3" ~ "Indeterminado",
          .default = .data$CON_CLASS_
        )
      ) %>%
      dplyr::mutate(CON_CLASS_ = as.factor(.data$CON_CLASS_))
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
          "1" ~ "Cura",
          "2" ~ "Abandono",
          "3" ~ "\\u00d3bito por LV",
          "4" ~ "\\u00d3bito por outra causa",
          "5" ~ "Transfer\\u00eancia",
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
