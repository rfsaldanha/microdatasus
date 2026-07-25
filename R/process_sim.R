#' Prepare SIM mortality microdata
#'
#' Recodes supported SIM mortality fields into descriptive values and normalizes
#' escaped Unicode text. Codes without a documented conversion are retained.
#'
#' Columns not explicitly recoded are retained, but Unicode normalization is
#' applied to every column and consequently the returned tibble contains
#' character columns.
#'
#' @param data A data frame returned by [fetch_datasus()] for a SIM mortality
#'   system, or another data frame with a compatible layout.
#' @param municipality_data Logical scalar. If `TRUE`, add municipality names
#'   and available territorial attributes for supported municipality-code
#'   columns.
#'
#' @examples
#' process_sim(sim_do_sample)
#'
#' @return A tibble with character columns. Supported codes are replaced with
#'   descriptions, and municipality fields are added when requested and
#'   available.
#'
#' @references
#' Saldanha, R. F. (2026). [SIM -- Sistema de Informação sobre
#' Mortalidade](https://rfsaldanha.github.io/sis/sim.html).
#'
#' @seealso [fetch_datasus()]
#'
#' @export

process_sim <- function(data, municipality_data = TRUE) {
  # Variables names
  variables_names <- names(data)

  # Use dtplyr
  # data <- dtplyr::lazy_dt(data)

  # CODINST
  if ("CODINST" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CODINST = dplyr::recode_values(
          .data$CODINST,
          "E" ~ "Estadual",
          "R" ~ "Regional",
          "M" ~ "Municipal",
          default = .data$CODINST
        )
      ) %>%
      dplyr::mutate(CODINST = as.factor(.data$CODINST))
  }

  # TIPOBITO
  if ("TIPOBITO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TIPOBITO = dplyr::recode_values(
          .data$TIPOBITO,
          "0" ~ NA,
          "9" ~ NA,
          "1" ~ "Fetal",
          "2" ~ "N\\u00e3o Fetal",
          default = .data$TIPOBITO
        )
      ) %>%
      dplyr::mutate(TIPOBITO = as.factor(.data$TIPOBITO))
  }

  # DTOBITO
  if ("DTOBITO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DTOBITO = as.Date(.data$DTOBITO, format = "%d%m%Y"))
  }

  # NATURAL
  if ("NATURAL" %in% variables_names) {
    colnames(tabNaturalidade)[1] <- "NATURAL"
    data <- data %>%
      dplyr::left_join(tabNaturalidade, by = "NATURAL") %>%
      dplyr::select(-"NATURAL") %>%
      dplyr::rename("NATURAL" = "nome") %>%
      dplyr::mutate(NATURAL = as.factor(.data$NATURAL))
  }

  # DTNASC
  if ("DTNASC" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DTNASC = as.Date(.data$DTNASC, format = "%d%m%Y"))
  }

  # IDADE
  if ("IDADE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        IDADE = dplyr::recode_values(
          .data$IDADE,
          "000" ~ NA,
          "999" ~ NA,
          default = .data$IDADE
        )
      ) %>%
      # Codigo e valor
      dplyr::mutate(
        idade_cod = substr(.data$IDADE, 1, 1),
        idade_value = as.numeric(substr(.data$IDADE, 2, 3)),
      ) %>%
      dplyr::mutate(
        IDADEminutos = dplyr::recode_values(
          .data$idade_cod,
          "0" ~ idade_value,
          default = NA
        )
      ) %>%
      dplyr::mutate(
        IDADEhoras = dplyr::recode_values(
          .data$idade_cod,
          "1" ~ idade_value,
          default = NA
        )
      ) %>%
      dplyr::mutate(
        IDADEdias = dplyr::recode_values(
          .data$idade_cod,
          "2" ~ idade_value,
          default = NA
        )
      ) %>%
      dplyr::mutate(
        IDADEmeses = dplyr::recode_values(
          .data$idade_cod,
          "3" ~ idade_value,
          default = NA
        )
      ) %>%
      dplyr::mutate(
        IDADEanos = dplyr::recode_values(
          .data$idade_cod,
          "4" ~ idade_value,
          "5" ~ idade_value + 100,
          default = NA
        )
      ) %>%
      dplyr::select(-"idade_cod", -"idade_value")
  }

  # SEXO
  if ("SEXO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        SEXO = dplyr::recode_values(
          .data$SEXO,
          "0" ~ NA,
          "9" ~ NA,
          "1" ~ "Masculino",
          "2" ~ "Feminino",
          default = .data$SEXO
        )
      ) %>%
      dplyr::mutate(SEXO = as.factor(.data$SEXO))
  }

  # RACACOR
  if ("RACACOR" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        RACACOR = dplyr::recode_values(
          .data$RACACOR,
          "0" ~ NA,
          "1" ~ "Branca",
          "2" ~ "Preta",
          "3" ~ "Amarela",
          "4" ~ "Parda",
          "5" ~ "Ind\\u00edgena",
          "6" ~ NA,
          "7" ~ NA,
          "8" ~ NA,
          "9" ~ NA,
          default = .data$RACACOR
        )
      ) %>%
      dplyr::mutate(RACACOR = as.factor(.data$RACACOR))
  }

  # ESTCIV
  if ("ESTCIV" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ESTCIV = dplyr::recode_values(
          .data$ESTCIV,
          "0" ~ NA,
          "1" ~ "Solteiro",
          "2" ~ "Casado",
          "3" ~ "Vi\\u00favo",
          "4" ~ "Separado judicialmente",
          "5" ~ "Uni\\u00e3o consensual",
          "6" ~ NA,
          "7" ~ NA,
          "8" ~ NA,
          "9" ~ NA,
          default = .data$ESTCIV
        )
      ) %>%
      dplyr::mutate(ESTCIV = as.factor(.data$ESTCIV))
  }

  # ESC
  if ("ESC" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ESC = dplyr::recode_values(
          .data$ESC,
          "0" ~ NA,
          "6" ~ NA,
          "7" ~ NA,
          "9" ~ NA,
          "A" ~ NA,
          "1" ~ "Nenhuma",
          "2" ~ "1 a 3 anos",
          "3" ~ "4 a 7 anos",
          "4" ~ "8 a 11 anos",
          "5" ~ "12 anos ou mais",
          "8" ~ "9 a 11 anos",
          "9" ~ NA,
          default = .data$ESC
        )
      ) %>%
      dplyr::mutate(ESC = as.factor(.data$ESC))
  }

  # ESC2010
  if ("ESC2010" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(ESC2010 = as.character(.data$ESC2010))
  }

  # SERIESCFAL
  if ("SERIESCFAL" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(SERIESCFAL = as.character(.data$SERIESCFAL))
  }

  # OCUP
  if ("OCUP" %in% variables_names) {
    if (!("DTOBITO" %in% variables_names)) {
      cli::cli_abort(
        "The variable DTOBITO is needed to preprocess the variable OCUP."
      )
    }

    colnames(tabCBO)[1] <- "OCUP"
    tabCBO$OCUP = as.character(tabCBO$OCUP)

    data <- data %>%
      dplyr::left_join(tabCBO, by = "OCUP") %>%
      dplyr::select(-"OCUP") %>%
      dplyr::rename("OCUP" = "nome")
  }

  # CODMUNRES
  if ("CODMUNRES" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CODMUNRES = substr(.data$CODMUNRES, 0, 6))

    if (municipality_data == TRUE) {
      colnames(tabMun)[1] <- "CODMUNRES"
      tabMun$CODMUNRES <- as.character(tabMun$CODMUNRES)

      data <- data %>%
        dplyr::left_join(tabMun, by = "CODMUNRES")
    }
  }

  # CODMUNOCOR
  if ("CODMUNOCOR" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(CODMUNOCOR = substr(.data$CODMUNOCOR, 0, 6))
  }

  # LOCOCOR
  if ("LOCOCOR" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        LOCOCOR = dplyr::recode_values(
          .data$LOCOCOR,
          "1" ~ "Hospital",
          "2" ~ "Outro estabelecimento de sa\\u00fade",
          "3" ~ "Domic\\u00edlio",
          "4" ~ "Via p\\u00fablica",
          "5" ~ "Outros",
          "9" ~ NA,
          default = .data$LOCOCOR
        )
      ) %>%
      dplyr::mutate(LOCOCOR = as.factor(.data$LOCOCOR))
  }

  # IDADEMAE
  if ("IDADEMAE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        IDADEMAE = dplyr::recode_values(
          .data$IDADEMAE,
          "0" ~ NA,
          default = .data$IDADEMAE
        )
      ) %>%
      dplyr::mutate(IDADEMAE = as.numeric(.data$IDADEMAE))
  }

  # ESCMAE
  if ("ESCMAE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ESCMAE = dplyr::recode_values(
          .data$ESCMAE,
          "0" ~ NA,
          "6" ~ NA,
          "7" ~ NA,
          "9" ~ NA,
          "A" ~ NA,
          "1" ~ "Nenhuma",
          "2" ~ "1 a 3 anos",
          "3" ~ "4 a 7 anos",
          "4" ~ "8 a 11 anos",
          "5" ~ "12 anos ou mais",
          "8" ~ "9 a 11 anos",
          "9" ~ NA,
          default = .data$ESCMAE
        )
      ) %>%
      dplyr::mutate(ESCMAE = as.factor(.data$ESCMAE))
  }

  # OCUPMAE
  if ("OCUPMAE" %in% variables_names) {
    if (!("DTOBITO" %in% variables_names)) {
      cli::cli_abort(
        "The variable DTOBITO is needed to preprocess the variable OCUPMAE."
      )
    }

    colnames(tabOcupacao)[1] <- "OCUPMAE"
    colnames(tabCBO)[1] <- "OCUPMAE"

    data <- data %>%
      dplyr::left_join(tabCBO, by = "OCUPMAE") %>%
      dplyr::select(-"OCUPMAE") %>%
      dplyr::rename("OCUPMAE" = "nome")
  }

  # QTDFILVIVO
  if ("QTDFILVIVO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(QTDFILVIVO = as.numeric(.data$QTDFILVIVO))
  }

  # QTDFILMORT
  if ("QTDFILMORT" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(QTDFILMORT = as.numeric(.data$QTDFILMORT))
  }

  # GRAVIDEZ
  if ("GRAVIDEZ" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        GRAVIDEZ = dplyr::recode_values(
          .data$GRAVIDEZ,
          "1" ~ "\\u00fanica",
          "2" ~ "Dupla",
          "3" ~ "Tr\\u00edplice e mais",
          "9" ~ NA,
          default = .data$GRAVIDEZ
        )
      ) %>%
      dplyr::mutate(GRAVIDEZ = as.factor(.data$GRAVIDEZ))
  }

  # GESTACAO
  if ("GESTACAO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        GESTACAO = dplyr::recode_values(
          .data$GESTACAO,
          "0" ~ NA,
          "A" ~ "21 a 27 semanas",
          "1" ~ "Menos de 22 semanas",
          "2" ~ "22 a 27 semanas",
          "3" ~ "28 a 31 semanas",
          "4" ~ "32 a 36 semanas",
          "5" ~ "37 a 41 semanas",
          "6" ~ "42 semanas e mais",
          "7" ~ "28 semanas e mais",
          "8" ~ "28 a 36 semanas",
          "9" ~ NA,
          default = .data$GESTACAO
        )
      ) %>%
      dplyr::mutate(GESTACAO = as.factor(.data$GESTACAO))
  }

  # PARTO
  if ("PARTO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        PARTO = dplyr::recode_values(
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
          default = .data$PARTO
        )
      ) %>%
      dplyr::mutate(PARTO = as.factor(.data$PARTO))
  }

  # OBITOPARTO
  if ("OBITOPARTO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        OBITOPARTO = dplyr::recode_values(
          .data$OBITOPARTO,
          "0" ~ NA,
          "1" ~ "Antes",
          "2" ~ "Durante",
          "3" ~ "Depois",
          "4" ~ NA,
          "5" ~ NA,
          "6" ~ NA,
          "7" ~ NA,
          "8" ~ NA,
          "9" ~ NA,
          default = .data$OBITOPARTO
        )
      ) %>%
      dplyr::mutate(OBITOPARTO = as.factor(.data$OBITOPARTO))
  }

  # PESO
  if ("PESO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        PESO = dplyr::recode_values(.data$PESO, "0" ~ NA, default = .data$PESO)
      ) %>%
      dplyr::mutate(PESO = as.numeric(.data$PESO))
  }

  # OBITOGRAV
  if ("OBITOGRAV" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        OBITOGRAV = dplyr::recode_values(
          .data$OBITOGRAV,
          "1" ~ "Sim",
          "2" ~ "N\\u00e3o",
          "3" ~ NA,
          "4" ~ NA,
          "5" ~ NA,
          "6" ~ NA,
          "7" ~ NA,
          "8" ~ NA,
          "9" ~ NA,
          default = .data$OBITOGRAV
        )
      ) %>%
      dplyr::mutate(OBITOGRAV = as.factor(.data$OBITOGRAV))
  }

  # OBITOPUERP
  if ("OBITOPUERP" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        OBITOPUERP = dplyr::recode_values(
          .data$OBITOPUERP,
          "1" ~ "De 0 a 42 dias",
          "2" ~ "De 43 dias a 1 ano",
          "3" ~ "N\\u00e3o",
          "4" ~ NA,
          "5" ~ NA,
          "6" ~ NA,
          "7" ~ NA,
          "8" ~ NA,
          "9" ~ NA,
          default = .data$OBITOPUERP
        )
      ) %>%
      dplyr::mutate(OBITOPUERP = as.factor(.data$OBITOPUERP))
  }

  # ASSISTMED
  if ("ASSISTMED" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ASSISTMED = dplyr::recode_values(
          .data$ASSISTMED,
          "1" ~ "Sim",
          "2" ~ "N\\u00e3o",
          "9" ~ NA,
          default = .data$ASSISTMED
        )
      ) %>%
      dplyr::mutate(ASSISTMED = as.factor(.data$ASSISTMED))
  }

  # EXAME
  if ("EXAME" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        EXAME = dplyr::recode_values(
          .data$EXAME,
          "1" ~ "Sim",
          "2" ~ "N\\u00e3o",
          "9" ~ NA,
          default = .data$EXAME
        )
      ) %>%
      dplyr::mutate(EXAME = as.factor(.data$EXAME))
  }

  # CIRURGIA
  if ("CIRURGIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CIRURGIA = dplyr::recode_values(
          .data$CIRURGIA,
          "1" ~ "Sim",
          "2" ~ "N\\u00e3o",
          "9" ~ NA,
          default = .data$CIRURGIA
        )
      ) %>%
      dplyr::mutate(CIRURGIA = as.factor(.data$CIRURGIA))
  }

  # NECROPSIA
  if ("NECROPSIA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        NECROPSIA = dplyr::recode_values(
          .data$NECROPSIA,
          "1" ~ "Sim",
          "2" ~ "N\\u00e3o",
          "9" ~ NA,
          default = .data$NECROPSIA
        )
      ) %>%
      dplyr::mutate(NECROPSIA = as.factor(.data$NECROPSIA))
  }

  # DTATESTADO
  if ("DTATESTADO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DTATESTADO = as.Date(.data$DTATESTADO, format = "%d%m%Y"))
  }

  # CIRCOBITO
  if ("CIRCOBITO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        CIRCOBITO = dplyr::recode_values(
          .data$CIRCOBITO,
          "0" ~ NA,
          "1" ~ "Acidente",
          "2" ~ "Suic\\u00eddio",
          "3" ~ "Homic\\u00eddio",
          "4" ~ "Outro",
          "5" ~ NA,
          "6" ~ NA,
          "7" ~ NA,
          "8" ~ NA,
          "9" ~ NA,
          default = .data$CIRCOBITO
        )
      ) %>%
      dplyr::mutate(CIRCOBITO = as.factor(.data$CIRCOBITO))
  }

  # ACIDTRAB
  if ("ACIDTRAB" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ACIDTRAB = dplyr::recode_values(
          .data$ACIDTRAB,
          "1" ~ "Sim",
          "2" ~ "N\\u00e3o",
          "9" ~ NA,
          default = .data$ACIDTRAB
        )
      ) %>%
      dplyr::mutate(ACIDTRAB = as.factor(.data$ACIDTRAB))
  }

  # FONTE
  if ("FONTE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        FONTE = dplyr::recode_values(
          .data$FONTE,
          "1" ~ "Boletim de Ocorr\\u00eancia",
          "2" ~ "Hospital",
          "3" ~ "Fam\\u00edlia",
          "4" ~ "Outro",
          "9" ~ NA,
          default = .data$FONTE
        )
      ) %>%
      dplyr::mutate(FONTE = as.factor(.data$FONTE))
  }

  # TPPOS
  if ("TPPOS" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        TPPOS = dplyr::recode_values(
          .data$TPPOS,
          "N" ~ "N\\u00e3o investigado",
          "S" ~ "Investigado",
          default = .data$TPPOS
        )
      ) %>%
      dplyr::mutate(TPPOS = as.factor(.data$TPPOS))
  }

  # DTINVESTIG
  if ("DTINVESTIG" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DTINVESTIG = as.Date(.data$DTINVESTIG, format = "%d%m%Y"))
  }

  # DTCADASTRO
  if ("DTCADASTRO" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DTCADASTRO = as.Date(.data$DTCADASTRO, format = "%d%m%Y"))
  }

  # ATESTANTE
  if ("ATESTANTE" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        ATESTANTE = dplyr::recode_values(
          .data$ATESTANTE,
          "0" ~ NA,
          "1" ~ "Sim",
          "2" ~ "Substituto",
          "3" ~ "IML",
          "4" ~ "SVO",
          "5" ~ "Outro",
          "6" ~ NA,
          "7" ~ NA,
          "8" ~ NA,
          "9" ~ NA,
          default = .data$ATESTANTE
        )
      ) %>%
      dplyr::mutate(ATESTANTE = as.factor(.data$ATESTANTE))
  }

  # FONTEINV
  if ("FONTEINV" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(
        FONTEINV = dplyr::recode_values(
          .data$FONTEINV,
          "1" ~ "Comit\\u00ea de Mortalidade Materna e/ou Infantil",
          "2" ~ "Visita familiar / Entrevista fam\\u00edlia",
          "3" ~ "Estabelecimento de sa\\u00fade / Prontu\\u00e1rio",
          "4" ~ "Relacionamento com outros bancos de dados",
          "5" ~ "SVO",
          "6" ~ "IML",
          "7" ~ "Outra fonte",
          "8" ~ "M\\u00faltiplas fontes",
          "9" ~ NA,
          default = .data$FONTEINV
        )
      ) %>%
      dplyr::mutate(FONTEINV = as.factor(.data$FONTEINV))
  }

  # DTRECEBIM
  if ("DTRECEBIM" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DTRECEBIM = as.Date(.data$DTRECEBIM, format = "%d%m%Y"))
  }

  # DTRECORIGA
  if ("DTRECORIGA" %in% variables_names) {
    data <- data %>%
      dplyr::mutate(DTRECORIGA = as.Date(.data$DTRECORIGA, format = "%d%m%Y"))
  }

  # From data.table to tibble
  data <- tibble::as_tibble(data)

  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  suppressWarnings(
    data <- tibble::as_tibble(lapply(
      X = data,
      FUN = stringi::stri_unescape_unicode
    ))
  )

  # Return
  return(data)
}
