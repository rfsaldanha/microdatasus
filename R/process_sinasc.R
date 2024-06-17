#' Process SINASC variables from DataSUS
#'
#' \code{process_sinasc} processes SINASC variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SINASC variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples
#' process_sinasc(sinasc_sample)
#'
#' @return a \code{data.frame} with the processed data.
#'
#' @export

process_sinasc <- function(data, municipality_data = TRUE) {
  # Variables names
  variables_names <- names(data)

  # Use dtplyr
  data <- dtplyr::lazy_dt(data)

  # CODINST
  if("CODINST" %in% variables_names){
    data <- data %>%
      dplyr::mutate(CODINST = as.integer(.data$CODINST))
  }

  # ORIGEM
  if("ORIGEM" %in% variables_names){
    data <- data %>%
      dplyr::mutate(ORIGEM = as.integer(.data$ORIGEM))
  }

  # NUMERODV
  if("NUMERODV" %in% variables_names){
    data <- data %>%
      dplyr::mutate(NUMERODV = as.integer(.data$NUMERODV))
  }

  # PREFIXODN
  if("PREFIXODN" %in% variables_names){
    data <- data %>%
      dplyr::mutate(PREFIXODN = as.integer(.data$PREFIXODN))
  }

  # LOCNASC
  if("LOCNASC" %in% variables_names){
    data <- data %>%
      dplyr::mutate(LOCNASC = dplyr::case_match(
        .data$LOCNASC,
        "0" ~ NA,
        "1" ~ "Hospital",
        "2" ~ "Outro estabelecimento de sa\\u00fade",
        "3" ~ "Domic\\u00edlio",
        "4" ~ "Outros",
        "5" ~ NA,
        "6" ~ NA,
        "7" ~ NA,
        "8" ~ NA,
        "9" ~ NA,
        .default = .data$LOCNASC
      )) %>%
      dplyr::mutate(LOCNASC <- as.factor(.data$LOCNASC))
  }

  # IDADEMAE
  if("IDADEMAE" %in% variables_names){
    data <- data %>%
      dplyr::mutate(IDADEMAE = dplyr::case_match(
        .data$IDADEMAE,
        "0" ~ NA,
        "99" ~ NA,
        .default = IDADEMAE
      )) %>%
      dplyr::mutate(IDADEMAE <- as.factor(.data$IDADEMAE))
  }

  # ESTCIVMAE
  if("ESTCIVMAE" %in% variables_names){
    data <- data %>%
      dplyr::mutate(ESTCIVMAE = dplyr::case_match(
        .data$ESTCIVMAE,
        "0" ~ NA,
        "1" ~ "Solteira",
        "2" ~ "Casada",
        "3" ~ "Vi\\u00fava",
        "4" ~ "Separada judicialmente",
        "5" ~ "Uni\\u00e3o consensual",
        "6" ~ NA,
        "7" ~ NA,
        "8" ~ NA,
        "9" ~ NA,
        .default = .data$ESTCIVMAE
      )) %>%
      dplyr::mutate(ESTCIVMAE <- as.factor(.data$ESTCIVMAE))
  }

  # ESCMAE
  if("ESCMAE" %in% variables_names){
    data <- data %>%
      dplyr::mutate(ESCMAE = dplyr::case_match(
        .data$ESCMAE,
        "1" ~ "Nenhum",
        "2" ~ "1 a 3 anos",
        "3" ~ "4 a 7 anos",
        "4" ~ "8 a 11 anos",
        "5" ~ "12 anos ou mais",
        "6" ~ NA,
        "7" ~ NA,
        "8" ~ NA,
        "9" ~ NA,
        .default = .data$ESCMAE
      )) %>%
      dplyr::mutate(ESCMAE <- as.factor(.data$ESCMAE))
  }

  # DTNASC
  if("DTNASC" %in% variables_names){
    data <- data %>%
      dplyr::mutate(DTNASC = as.Date(.data$DTNASC, format = "%d%m%Y"))
  }

  # CODOCUPMAE
  if ("CODOCUPMAE" %in% variables_names) {
    if (!("DTNASC" %in% variables_names))
      cli::cli_abort("The variable DTNASC is needed to preprocess the variable CODOCUPMAE")

    colnames(tabCBO)[1] <- "CODOCUPMAE"
    tabCBO$CODOCUPMAE = as.character(tabCBO$CODOCUPMAE)

    data <- data %>%
      dplyr::left_join(tabCBO, by = "CODOCUPMAE") %>%
      dplyr::select(-"CODOCUPMAE") %>%
      dplyr::rename("CODOCUPMAE" = "nome")
  }


  # QTDFILVIVO
  if("QTDFILVIVO" %in% variables_names){
    data <- data %>%
      dplyr::mutate(QTDFILVIVO = dplyr::case_match(
        .data$QTDFILVIVO,
        "99" ~ NA,
        .default = .data$QTDFILVIVO
      )) %>%
      dplyr::mutate(QTDFILVIVO = as.numeric(.data$QTDFILVIVO))
  }

  # QTDFILMORT
  if("QTDFILMORT" %in% variables_names){
    data <- data %>%
      dplyr::mutate(QTDFILMORT = dplyr::case_match(
        .data$QTDFILMORT,
        "99" ~ NA,
        .default = .data$QTDFILMORT
      )) %>%
      dplyr::mutate(QTDFILMORT = as.numeric(.data$QTDFILMORT))
  }

  # CODMUNRES
  if("CODMUNRES" %in% variables_names & municipality_data == TRUE){
    colnames(tabMun)[1] <- "CODMUNRES"
    tabMun$CODMUNRES <- as.character(tabMun$CODMUNRES)

    data <- data %>%
      dplyr::left_join(tabMun, by = "CODMUNRES")
  }

  # GESTACAO
  if("GESTACAO" %in% variables_names){
    data <- data %>%
      dplyr::mutate(GESTACAO = dplyr::case_match(
        .data$GESTACAO,
        "0" ~ NA,
        "1" ~ "Menos de 22 semanas",
        "2" ~ "22 a 27 semanas",
        "3" ~ "28 a 31 semanas",
        "4" ~ "32 a 36 semanas",
        "5" ~ "37 a 41 semanas",
        "6" ~ "42 semanas ou mais",
        "7" ~ NA,
        "8" ~ NA,
        "9" ~ NA,
        .default = .data$GESTACAO
      )) %>%
      dplyr::mutate(GESTACAO <- as.factor(.data$GESTACAO))
  }

  # GRAVIDEZ
  if("GRAVIDEZ" %in% variables_names){
    data <- data %>%
      dplyr::mutate(GRAVIDEZ = dplyr::case_match(
        .data$GRAVIDEZ,
        "0" ~ NA,
        "1" ~ "\\u00fanica",
        "2" ~ "Dupla",
        "3" ~ "Tripla e mais",
        "4" ~ NA,
        "5" ~ NA,
        "6" ~ NA,
        "7" ~ NA,
        "8" ~ NA,
        "9" ~ NA,
        .default = .data$GRAVIDEZ
      )) %>%
      dplyr::mutate(GRAVIDEZ = as.factor(.data$GRAVIDEZ))
  }

  # PARTO
  if("PARTO" %in% variables_names){
    data <- data %>%
      dplyr::mutate(PARTO = dplyr::case_match(
        .data$PARTO,
        "0" ~ NA,
        "1" ~ "Vaginal",
        "2" ~ "Ces\\u00e1reo",
        "3" ~ NA,
        "4" ~ NA,
        "5" ~ NA,
        "6" ~ NA,
        "7" ~ NA,
        "8" ~ NA,
        "9" ~ NA,
        .default = .data$PARTO
      )) %>%
      dplyr::mutate(PARTO <- as.factor(.data$PARTO))
  }

  # CONSULTAS
  if("CONSULTAS" %in% variables_names){
    data <- data %>%
      dplyr::mutate(CONSULTAS = dplyr::case_match(
        .data$CONSULTAS,
        "0" ~ NA,
        "1" ~ "Nenhuma",
        "2" ~ "1 a 3 vezes",
        "3" ~ "4 a 6 vezes",
        "4" ~ "7 ou mais vezes",
        "5" ~ NA,
        "6" ~ NA,
        "7" ~ NA,
        "8" ~ NA,
        "9" ~ NA,
        .default = .data$CONSULTAS
      )) %>%
      dplyr::mutate(CONSULTAS = as.factor(.data$CONSULTAS))
  }

  # SEXO
  if("SEXO" %in% variables_names){
    data <- data %>%
      dplyr::mutate(SEXO = dplyr::case_match(
        .data$SEXO,
        "0" ~ NA,
        "1" ~ "Masculino",
        "M" ~ "Masculino",
        "2" ~ "Feminino",
        "F" ~ "Feminino",
        "9" ~ NA,
        "I" ~ NA,
        .default = .data$SEXO
      )) %>%
      dplyr::mutate(SEXO = as.factor(.data$SEXO))
  }

  # APGAR1
  if("APGAR1" %in% variables_names){
    data <- data %>%
      dplyr::mutate(APGAR1 = dplyr::case_match(
        .data$APGAR1,
        "99" ~ NA,
        .default = .data$APGAR1
      )) %>%
      dplyr::mutate(APGAR1 = as.numeric(.data$APGAR1))
  }

  # APGAR5
  if("APGAR5" %in% variables_names){
    data <- data %>%
      dplyr::mutate(APGAR5 = dplyr::case_match(
        .data$APGAR5,
        "99" ~ NA,
        .default = .data$APGAR5
      )) %>%
      dplyr::mutate(APGAR5 = as.numeric(.data$APGAR5))
  }

  # RACACOR
  if("RACACOR" %in% variables_names){
    data <- data %>%
      dplyr::mutate(RACACOR = dplyr::case_match(
        .data$RACACOR,
        "1" ~ "Branca",
        "2" ~ "Preta",
        "3" ~ "Amarela",
        "4" ~ "Parda",
        "5" ~ "Ind\\u00edgena",
        "9" ~ NA,
        .default = .data$RACACOR
      )) %>%
      dplyr::mutate(RACACOR = as.factor(.data$RACACOR))
  }

  # PESO
  if("PESO" %in% variables_names){
    data <- data %>%
      dplyr::mutate(RACACOR = dplyr::case_match(
        .data$PESO,
        "0" ~ NA,
        "9999" ~ NA
      )) %>%
      dplyr::mutate(PESO = as.numeric(.data$PESO))
  }

  # IDANOMAL
  if("IDANOMAL" %in% variables_names){
    data <- data %>%
      dplyr::mutate(IDANOMAL = dplyr::case_match(
        .data$IDANOMAL,
        "1" ~ "Sim",
        "2" ~ "N\\u00e3o",
        "3" ~ NA,
        "4" ~ NA,
        "5" ~ NA,
        "6" ~ NA,
        "7" ~ NA,
        "8" ~ NA,
        "9" ~ NA,
        .default = .data$IDANOMAL
      )) %>%
      dplyr::mutate(IDANOMAL = as.factor(.data$IDANOMAL))
  }

  # DTCADASTRO
  if("DTCADASTRO" %in% variables_names){
    data <- data %>%
      dplyr::mutate(DTCADASTRO = as.Date(.data$DTCADASTRO, format = "%d%m%Y"))
  }

  # DTRECEBIM
  if("DTRECEBIM" %in% variables_names){
    data <- data %>%
      dplyr::mutate(DTRECEBIM = as.Date(.data$DTRECEBIM, format = "%d%m%Y"))
  }

  # DIFDATA
  if("DIFDATA" %in% variables_names){
    data <- data %>%
      dplyr::mutate(DIFDATA = as.Date(.data$DIFDATA, format = "%d%m%Y"))
  }

  # DTRECORIG
  if("DTRECORIG" %in% variables_names){
    data <- data %>%
      dplyr::mutate(DTRECORIG = as.Date(.data$DTRECORIG, format = "%d%m%Y"))
  }

  # NATURALMAE
  if("NATURALMAE" %in% variables_names){
    data <- data %>%
      dplyr::mutate(NATURALMAE = as.numeric(.data$NATURALMAE))
  }

  # CODUFNATU
  if("CODUFNATU" %in% variables_names){
    data <- data %>%
      dplyr::mutate(CODUFNATU = as.numeric(.data$CODUFNATU))
  }

  # SERIESCMAE
  if("SERIESCMAE" %in% variables_names){
    data <- data %>%
      dplyr::mutate(SERIESCMAE = as.numeric(.data$SERIESCMAE))
  }

  # DTNASCMAE
  if("DTNASCMAE" %in% variables_names){
    data <- data %>%
      dplyr::mutate(DTNASCMAE = as.Date(.data$DTNASCMAE, format = "%d%m%Y"))
  }

  # RACACORMAE
  if("RACACORMAE" %in% variables_names){
    data <- data %>%
      dplyr::mutate(RACACORMAE = dplyr::case_match(
        .data$RACACORMAE,
        "1" ~ "Branca",
        "2" ~ "Preta",
        "3" ~ "Amarela",
        "4" ~ "Parda",
        "5" ~ "Ind\\u00edgena",
        .default = .data$RACACORMAE
      )) %>%
      dplyr::mutate(RACACORMAE = as.factor(.data$RACACORMAE))
  }

  # QTDGESTANT
  if("QTDGESTANT" %in% variables_names){
    data <- data %>%
      dplyr::mutate(QTDGESTANT = as.numeric(.data$QTDGESTANT))
  }

  # QTDPARTNOR
  if("QTDPARTNOR" %in% variables_names){
    data <- data %>%
      dplyr::mutate(QTDPARTNOR = as.numeric(.data$QTDPARTNOR))
  }

  # QTDPARTCES
  if("QTDPARTCES" %in% variables_names){
    data <- data %>%
      dplyr::mutate(QTDPARTCES = as.numeric(.data$QTDPARTCES))
  }

  # IDADEPAI
  if("IDADEPAI" %in% variables_names){
    data <- data %>%
      dplyr::mutate(IDADEPAI = dplyr::case_match(
        .data$IDADEPAI,
        "0" ~ NA,
        "99" ~ NA,
        .default = .data$IDADEPAI
      )) %>%
      dplyr::mutate(IDADEPAI = as.numeric(.data$IDADEPAI))
  }

  # DTULTMENST
  if("DTULTMENST" %in% variables_names){
    data <- data %>%
      dplyr::mutate(DTULTMENST = as.Date(.data$DTULTMENST, format = "%d%m%Y"))
  }

  # SEMAGESTAC
  if("SEMAGESTAC" %in% variables_names){
    data <- data %>%
      dplyr::mutate(SEMAGESTAC = as.numeric(.data$SEMAGESTAC))
  }

  # TPMETESTIM
  if("TPMETESTIM" %in% variables_names){
    data <- data %>%
      dplyr::mutate(TPMETESTIM = as.numeric(.data$TPMETESTIM))
  }

  # CONSPRENAT
  if("CONSPRENAT" %in% variables_names){
    data <- data %>%
      dplyr::mutate(CONSPRENAT = as.numeric(.data$CONSPRENAT))
  }

  # MESPRENAT
  if("MESPRENAT" %in% variables_names){
    data <- data %>%
      dplyr::mutate(MESPRENAT = as.numeric(.data$MESPRENAT))
  }

  # TPAPRESENT
  if("TPAPRESENT" %in% variables_names){
    data <- data %>%
      dplyr::mutate(TPAPRESENT = as.numeric(.data$TPAPRESENT))
  }

  # STTRABPART
  if("STTRABPART" %in% variables_names){
    data <- data %>%
      dplyr::mutate(STTRABPART = as.numeric(.data$STTRABPART))
  }

  # STCESPARTO
  if("STCESPARTO" %in% variables_names){
    data <- data %>%
      dplyr::mutate(STCESPARTO = as.numeric(.data$STCESPARTO))
  }

  # TPNASCASSI
  if("TPNASCASSI" %in% variables_names){
    data <- data %>%
      dplyr::mutate(TPNASCASSI = as.numeric(.data$TPNASCASSI))
  }

  # TPFUNCRESP
  if("TPFUNCRESP" %in% variables_names){
    data <- data %>%
      dplyr::mutate(TPFUNCRESP = as.numeric(.data$TPFUNCRESP))
  }

  # TPDOCRESP
  if("TPDOCRESP" %in% variables_names){
    data <- data %>%
      dplyr::mutate(TPDOCRESP = as.numeric(.data$TPDOCRESP))
  }

  # DTDECLARAC
  if("DTDECLARAC" %in% variables_names){
    data <- data %>%
      dplyr::mutate(DTDECLARAC = as.Date(.data$DTDECLARAC, format = "%d%m%Y"))
  }

  # ESCMAEAGR1
  if("ESCMAEAGR1" %in% variables_names){
    data <- data %>%
      dplyr::mutate(ESCMAEAGR1 = as.numeric(.data$ESCMAEAGR1))
  }

  # TPROBSON
  if("TPROBSON" %in% variables_names){
    data <- data %>%
      dplyr::mutate(TPROBSON = as.numeric(.data$TPROBSON))
  }

  # STDNEPIDEM
  if("STDNEPIDEM" %in% variables_names){
    data <- data %>%
      dplyr::mutate(STDNEPIDEM = as.numeric(.data$STDNEPIDEM))
  }

  # STDNNOVA
  if("STDNNOVA" %in% variables_names){
    data <- data %>%
      dplyr::mutate(STDNNOVA = as.numeric(.data$STDNNOVA))
  }

  # CODPAISRES
  if("CODPAISRES" %in% variables_names){
    data <- data %>%
      dplyr::mutate(CODPAISRES = as.numeric(.data$CODPAISRES))
  }

  # PARIDADE
  if("PARIDADE" %in% variables_names){
    data <- data %>%
      dplyr::mutate(PARIDADE = as.numeric(.data$PARIDADE))
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
