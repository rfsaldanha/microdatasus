# SIH-RD and SIH-RJ share most fields, but each official DEF contains a few
# layout-specific declarations. Only direct labels and detailed DBF tables are
# applied; analytical groupings such as age bands remain numeric variables.
.sih_rd_rj_categorical_fields <- c(
  "NATUREZA", "NAT_JUR", "GESTAO", "SEXO", "RACA_COR", "NACIONAL",
  "ESPEC", "IDENT", "CAR_INT", "MARCA_UTI", "COBRANCA", "MORTE",
  "FINANC", "REGCT", "COMPLEX", "IND_VDRL", "INFEHOSP", "INSTRU",
  "CONTRACEP1", "CONTRACEP2", "GESTRISCO", "CNES", "PROC_REA",
  "PROC_SOLIC", "CBOR", "ETNIA", "CNAER", "VINCPREV", "MARCA_UCI",
  "FONTE_ORC", "INSC_PN", "ST_SITUAC", "ST_BLOQ", "ST_MOT_BLO",
  paste0("TPDISEC", seq_len(9L))
)

# The professional-services and rejected-record layouts have their own DEF
# files inside the current SIH TabWin archive.
.sih_sp_categorical_fields <- c(
  "IN_TP_VAL", "SP_PROCREA", "SP_ATOPROF", "SERV_CLA", "SP_PF_CBO",
  "SP_CNES"
)
.sih_er_categorical_fields <- c("CNES", "CO_ERRO")

# Quantities are deliberately kept separate from identifiers. In particular,
# COD_IDADE remains a code: the TabWin age conversion expects a composite
# value that is not stored in that raw field alone.
.sih_integer_fields <- c(
  "ANO_CMPT", "MES_CMPT",
  "UTI_MES_IN", "UTI_MES_AN", "UTI_MES_AL", "UTI_MES_TO",
  "UTI_INT_IN", "UTI_INT_AN", "UTI_INT_AL", "UTI_INT_TO",
  "DIAR_ACOM", "QT_DIARIAS", "RUBRICA", "IDADE", "DIAS_PERM",
  "NUM_PROC", "TOT_PT_SP", "NUM_FILHOS",
  "SP_AA", "SP_MM", "SP_QTD_ATO", "SP_PTSP", "SP_NF",
  "SP_DES_HOS", "SP_DES_PAC", "SP_QT_PROC", "ANO", "MES"
)

.sih_competence_year <- function(x) {
  year <- .process_as_integer(x)
  historical <- !is.na(year) & year >= 0L & year <= 99L
  year[historical] <- year[historical] + 1900L
  year
}

.sih_date_fields <- c(
  "NASC", "DT_INTER", "DT_SAIDA", "GESTOR_DT",
  "SP_DTINTER", "SP_DTSAIDA"
)

.sih_double_fields <- c(
  "US_TOT", "SP_VALATO", "SP_M_HOSP", "SP_M_PAC"
)

.sih_add_age_fields <- function(data) {
  unit_field <- .process_find_fields(data, "COD_IDADE")
  value_field <- .process_find_fields(data, "IDADE")
  if (!length(unit_field) || !length(value_field)) {
    return(data)
  }

  # The official IDADEDET.CNV combines COD_IDADE with IDADE: 2 means days,
  # 3 months, 4 years, and 5 means the stored value plus 100 years.
  .process_add_age_fields(
    data,
    data[[unit_field[[1L]]]],
    data[[value_field[[1L]]]],
    units = c(
      "2" = "IDADEdias",
      "3" = "IDADEmeses",
      "4" = "IDADEanos",
      "5" = "IDADEanos"
    ),
    century_units = "5"
  )
}

.sih_categorical_fields <- function(information_system) {
  switch(
    information_system,
    "SIH-RD" = .sih_rd_rj_categorical_fields,
    "SIH-RJ" = .sih_rd_rj_categorical_fields,
    "SIH-SP" = .sih_sp_categorical_fields,
    "SIH-ER" = .sih_er_categorical_fields
  )
}

.sih_dictionary_rows <- function(data, information_system) {
  rows <- seq_len(nrow(data))
  if (!information_system %in% c("SIH-RD", "SIH-RJ") || !length(rows)) {
    return(stats::setNames(list(rows), information_system))
  }

  year_field <- .process_find_fields(data, "ANO_CMPT")
  if (!length(year_field)) {
    return(stats::setNames(list(rows), information_system))
  }
  year <- .sih_competence_year(data[[year_field[[1L]]]])
  month_field <- .process_find_fields(data, "MES_CMPT")
  month <- if (length(month_field)) {
    .process_as_integer(data[[month_field[[1L]]]])
  } else {
    rep(NA_integer_, length(year))
  }

  # Unknown competencies use the current definition. Historical intervals are
  # selected only when the row contains enough information to do so safely.
  dictionary <- rep(information_system, length(rows))
  file_type <- substring(information_system, 5L)
  dictionary[!is.na(year) & year >= 1992L & year <= 1997L] <-
    paste0("SIH-", file_type, "-1992-1997")
  dictionary[!is.na(year) & year >= 1998L & year <= 2002L] <-
    paste0("SIH-", file_type, "-1998-2003-07")
  dictionary[
    !is.na(year) & year == 2003L & !is.na(month) & month <= 7L
  ] <- paste0("SIH-", file_type, "-1998-2003-07")
  dictionary[
    !is.na(year) & year == 2003L & !is.na(month) & month >= 8L
  ] <- paste0("SIH-", file_type, "-2003-08-2007")
  dictionary[!is.na(year) & year >= 2004L & year <= 2007L] <-
    paste0("SIH-", file_type, "-2003-08-2007")

  # Keep first-occurrence order so cache and progress messages are stable.
  keys <- unique(dictionary)
  stats::setNames(
    lapply(keys, function(key) which(dictionary == key)),
    keys
  )
}

#' Prepare SIH hospital-admission microdata
#'
#' Uses the official DataSUS TabWin definitions to label all four SIH file
#' families available from [fetch_datasus()]: reduced admissions (`"SIH-RD"`),
#' rejected admissions (`"SIH-RJ"`), professional services (`"SIH-SP"`), and
#' rejected/error records (`"SIH-ER"`). Dictionaries are downloaded on first
#' use and cached for the rest of the R session.
#'
#' For RD and RJ, the definition is selected from the official historical
#' archives according to each row's competence. This supports data sets
#' concatenated across the 1997, July 2003, and 2007 layout boundaries. Codes
#' absent from a conversion table remain visible as factor levels.
#'
#' @param data A data frame returned by [fetch_datasus()] for a supported SIH
#'   file family, or another data frame with a compatible layout.
#' @param information_system SIH file family represented by `data`. One of
#'   `"SIH-RD"`, `"SIH-RJ"`, `"SIH-SP"`, or `"SIH-ER"`. The default preserves
#'   previous calls to `process_sih()`.
#' @param municipality_data Logical scalar. If `TRUE`, add municipality names
#'   and available territorial attributes for the residence municipality in
#'   RD, RJ, and ER files.
#'
#' @param labels Output type for categorical labels: `"factor"` (the default),
#'   `"character"`, or `"none"` to retain the original codes.
#' @param diagnostics Logical scalar. If `TRUE`, attach a processing report,
#'   including codes absent from official conversion tables. Retrieve it with
#'   [processing_diagnostics()].
#' @examplesIf interactive() && curl::has_internet()
#' process_sih(sih_rd_sample)
#'
#' @return A tibble. Dates are returned as `Date`, counts, quantities, and
#'   derived `IDADEdias`, `IDADEmeses`, and `IDADEanos` fields as integer,
#'   monetary values as double, labelled categorical fields as factors, and
#'   identifiers and free text as character. Derived age fields are added when
#'   the source contains both `COD_IDADE` and `IDADE`.
#'
#' @references
#' Saldanha, R. F. (2026). [SIH -- Sistema de Informações Hospitalares do
#' SUS](https://rfsaldanha.github.io/sis/sih.html).
#'
#' @seealso [fetch_tabwin_dictionary()], [fetch_datasus()]
#'
#' @export
process_sih <- function(
  data,
  information_system = "SIH-RD",
  municipality_data = TRUE,
  labels = c("factor", "character", "none"),
  diagnostics = FALSE
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  .datasus_assert_flag(municipality_data, "municipality_data")
  options <- .process_validate_options(labels, diagnostics)
  labels <- options$labels
  sih_types <- c("SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER")
  if (
    !is.character(information_system) ||
      length(information_system) != 1L ||
      is.na(information_system) ||
      !information_system %in% sih_types
  ) {
    cli::cli_abort(
      "{.arg information_system} must be one of: {.val {sih_types}}."
    )
  }

  result <- tibble::as_tibble(data)
  collector <- .process_diagnostic_collector(
    diagnostics, information_system, result
  )
  # DBF readers can expose text columns as factors depending on their options
  # and R version. Reset source factors to character; only fields successfully
  # labelled by an official TabWin table become factors below.
  for (field in names(result)) {
    if (is.factor(result[[field]])) {
      result[[field]] <- as.character(result[[field]])
    }
  }
  result <- .process_normalize_text(result)
  categorical_fields <- .process_find_fields(
    result,
    .sih_categorical_fields(information_system)
  )
  dictionary_rows <- .sih_dictionary_rows(result, information_system)

  # Fetch all required dictionaries before the start message, preserving the
  # message order requested for the preprocessing lifecycle.
  dictionaries <- list()
  if (length(categorical_fields) &&
      (!identical(labels, "none") || diagnostics)) {
    for (key in names(dictionary_rows)) {
      if (length(dictionary_rows[[key]])) {
        dictionaries[[key]] <- fetch_tabwin_dictionary(key)
      }
    }
  }

  cli::cli_alert_info(
    "Starting {.strong {information_system}} data pre-processing..."
  )

  # RD/RJ files through 1997 use YYMMDD. Resolve their century from
  # ANO_CMPT instead of R percent-y pivot rules; later layouts use YYYYMMDD.
  year_field <- .process_find_fields(result, "ANO_CMPT")
  reference_year <- if (length(year_field)) {
    .sih_competence_year(result[[year_field[[1L]]]])
  } else {
    rep(NA_integer_, nrow(result))
  }
  for (field in .process_find_fields(result, .sih_date_fields)) {
    result[[field]] <- if (information_system %in% c("SIH-RD", "SIH-RJ")) {
      .process_as_sih_date(
        result[[field]], reference_year, collector, field
      )
    } else {
      .process_as_date(
        result[[field]], format = "%Y%m%d", collector = collector,
        field = field
      )
    }
  }
  for (field in .process_find_fields(result, .sih_integer_fields)) {
    result[[field]] <- .process_as_integer(result[[field]], collector = collector, field = field)
    if (toupper(field) == "ANO_CMPT") {
      result[[field]] <- .sih_competence_year(result[[field]])
    }
  }

  result <- .sih_add_age_fields(result)

  # Monetary columns share a VAL_ prefix in RD/RJ. Explicit SP and US fields
  # cover values whose names follow a different convention.
  value_fields <- c(
    names(result)[startsWith(toupper(names(result)), "VAL_")],
    .process_find_fields(result, .sih_double_fields)
  )
  for (field in unique(value_fields)) {
    result[[field]] <- .process_as_double(result[[field]], collector = collector, field = field)
  }

  # A concatenated RD/RJ data set can require more than one official DEF.
  # Applying a map to its row subset prevents historical labels from leaking
  # into records governed by a later layout.
  result <- .process_apply_dictionaries(
    result,
    dictionaries,
    categorical_fields,
    dictionary_rows,
    labels = labels,
    collector = collector
  )

  # Municipality codes are identifiers, not measurements or categories.
  result <- .process_normalize_code_fields(
    result,
    c("MUNIC_RES", "MUNIC_MOV", "MUN_RES", "MUN_MOV")
  )
  if (municipality_data) {
    residence_field <- switch(
      information_system,
      "SIH-RD" = "MUNIC_RES",
      "SIH-RJ" = "MUNIC_RES",
      "SIH-ER" = "MUN_RES",
      NULL
    )
    if (!is.null(residence_field)) {
      result <- .process_add_municipality_data(result, residence_field, collector)
    }
  }

  cli::cli_alert_success(
    "Finished {.strong {information_system}} data pre-processing."
  )
  .process_finalize(result, collector)
}
