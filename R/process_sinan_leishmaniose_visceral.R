#' Process SINAN Leishmaniose Visceral variables from DataSUS
#'
#' \code{process_sinan_leishmaniose_visceral} processes SINAN Leishmaniose Visceral variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SINAN Leishmaniose Visceral variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples
#' process_sinan_leishmaniose_visceral(sinan_leishmaniose_visceral_sample)
#'
#' @return a \code{data.frame} with the processed data.
#'
#' @export

process_sinan_leishmaniose_visceral <- function(
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

  # FRAQUEZA
  if ("FRAQUEZA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        FRAQUEZA = dplyr::case_match(
          .data$FRAQUEZA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$FRAQUEZA
        )
      ) %>%
      dplyr::mutate(FRAQUEZA = as.factor(.data$FRAQUEZA))
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

  # EMAGRA
  if ("EMAGRA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        EMAGRA = dplyr::case_match(
          .data$EMAGRA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$EMAGRA
        )
      ) %>%
      dplyr::mutate(EMAGRA = as.factor(.data$EMAGRA))
  }

  # TOSSE
  if ("TOSSE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TOSSE = dplyr::case_match(
          .data$TOSSE,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$TOSSE
        )
      ) %>%
      dplyr::mutate(TOSSE = as.factor(.data$TOSSE))
  }

  # PALIDEZ
  if ("PALIDEZ" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        PALIDEZ = dplyr::case_match(
          .data$PALIDEZ,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$PALIDEZ
        )
      ) %>%
      dplyr::mutate(PALIDEZ = as.factor(.data$PALIDEZ))
  }

  # BACO
  if ("BACO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        BACO = dplyr::case_match(
          .data$BACO,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$BACO
        )
      ) %>%
      dplyr::mutate(BACO = as.factor(.data$BACO))
  }

  # INFECCIOSO
  if ("INFECCIOSO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        INFECCIOSO = dplyr::case_match(
          .data$INFECCIOSO,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$INFECCIOSO
        )
      ) %>%
      dplyr::mutate(INFECCIOSO = as.factor(.data$INFECCIOSO))
  }

  # FEN_HEMORR
  if ("FEN_HEMORR" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        FEN_HEMORR = dplyr::case_match(
          .data$FEN_HEMORR,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$FEN_HEMORR
        )
      ) %>%
      dplyr::mutate(FEN_HEMORR = as.factor(.data$FEN_HEMORR))
  }

  # FIGADO
  if ("FIGADO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        FIGADO = dplyr::case_match(
          .data$FIGADO,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$FIGADO
        )
      ) %>%
      dplyr::mutate(FIGADO = as.factor(.data$FIGADO))
  }

  # ICTERICIA
  if ("ICTERICIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ICTERICIA = dplyr::case_match(
          .data$ICTERICIA,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$ICTERICIA
        )
      ) %>%
      dplyr::mutate(ICTERICIA = as.factor(.data$ICTERICIA))
  }

  # OUTROS
  if ("OUTROS" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        OUTROS = dplyr::case_match(
          .data$OUTROS,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$OUTROS
        )
      ) %>%
      dplyr::mutate(OUTROS = as.factor(.data$OUTROS))
  }

  # HIV
  if ("HIV" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        HIV = dplyr::case_match(
          .data$HIV,
          "1" ~ "Sim",
          "2" ~ "N\u00e3o",
          "9" ~ NA,
          .default = .data$HIV
        )
      ) %>%
      dplyr::mutate(HIV = as.factor(.data$HIV))
  }

  # DIAG_PAR_N
  if ("DIAG_PAR_N" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        DIAG_PAR_N = dplyr::case_match(
          .data$DIAG_PAR_N,
          "9" ~ NA,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "3" ~ "N\\u00e3o realizado",
          .default = .data$DIAG_PAR_N
        )
      ) %>%
      dplyr::mutate(DIAG_PAR_N = as.factor(.data$DIAG_PAR_N))
  }

  # IFI
  if ("IFI" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        IFI = dplyr::case_match(
          .data$IFI,
          "9" ~ NA,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "3" ~ "N\\u00e3o realizado",
          .default = .data$IFI
        )
      ) %>%
      dplyr::mutate(IFI = as.factor(.data$IFI))
  }

  # OUTRO
  if ("OUTRO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        OUTRO = dplyr::case_match(
          .data$OUTRO,
          "9" ~ NA,
          "1" ~ "Positivo",
          "2" ~ "Negativo",
          "3" ~ "N\\u00e3o realizado",
          .default = .data$OUTRO
        )
      ) %>%
      dplyr::mutate(OUTRO = as.factor(.data$OUTRO))
  }

  # ENTRADA
  if ("ENTRADA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ENTRADA = dplyr::case_match(
          .data$ENTRADA,
          "9" ~ NA,
          "1" ~ "Caso novo",
          "2" ~ "Recidiva",
          "3" ~ "Transfer\\u00eancia",
          .default = .data$ENTRADA
        )
      ) %>%
      dplyr::mutate(ENTRADA = as.factor(.data$ENTRADA))
  }

  # DROGA
  if ("DROGA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        DROGA = dplyr::case_match(
          .data$DROGA,
          "9" ~ NA,
          "1" ~ "Antimonial Pentavalente",
          "2" ~ "Anfotericina b",
          "3" ~ "Pentamidina",
          "4" ~ "Anfotericina b lipossomal",
          "5" ~ "Outras drogas",
          .default = .data$DROGA
        )
      ) %>%
      dplyr::mutate(DROGA = as.factor(.data$DROGA))
  }

  # FALENCIA
  if ("FALENCIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        FALENCIA = dplyr::case_match(
          .data$FALENCIA,
          "9" ~ NA,
          "1" ~ "Anfotericina b",
          "2" ~ "Anfotericina b lipossomal",
          "3" ~ "Outras drogas",
          .default = .data$FALENCIA
        )
      ) %>%
      dplyr::mutate(FALENCIA = as.factor(.data$FALENCIA))
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
