#' Process SINAN Zika variables from DataSUS
#'
#' \code{process_sinan_zika} processes SINAN Zika variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SINAN Zika variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples
#' process_sinan_zika(sinan_zika_sample)
#'
#' @return a \code{data.frame} with the processed data.
#'
#' @export

process_sinan_zika <- function(data, municipality_data = TRUE){
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

  # CLASSI_FIN
  if ("CLASSI_FIN" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CLASSI_FIN = dplyr::case_match(
        .data$CLASSI_FIN,
        "1" ~ "Confirmado",
        "2" ~ "Descartado",
        "8" ~ "Inconclusivo",
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
