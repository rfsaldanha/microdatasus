# SIA uses one current TabWin archive for twelve file families. PA also has
# three historical layouts, selected below from the record competence.
.sia_information_systems <- paste0(
  "SIA-",
  c("AB", "ABO", "ACF", "AD", "AN", "AM", "AQ", "AR", "ATD", "PA", "PS", "SAD")
)

.sia_dictionary_rows <- function(data, information_system) {
  rows <- seq_len(nrow(data))
  if (!identical(information_system, "SIA-PA") || !length(rows)) {
    return(stats::setNames(list(rows), information_system))
  }

  competence_field <- .process_find_fields(data, c("PA_MVM", "PA_DATREF"))
  if (!length(competence_field)) {
    return(stats::setNames(list(rows), information_system))
  }
  # Row-bound old/current files can expose both names. Fill each row from the
  # first non-empty competence rather than choosing one column globally.
  competence <- rep(NA_character_, length(rows))
  for (field in competence_field) {
    candidate <- trimws(as.character(data[[field]]))
    fill <- (is.na(competence) | !nzchar(competence)) &
      !is.na(candidate) & nzchar(candidate)
    competence[fill] <- candidate[fill]
  }
  # The oldest PA files store YYMM. Interpret the only published range
  # (1994-2007) explicitly instead of relying on the current century.
  four_digits <- grepl("^[0-9]{4}$", competence)
  year <- suppressWarnings(as.integer(substr(competence, 1L, 2L)))
  competence[four_digits] <- paste0(
    ifelse(year[four_digits] >= 94L, "19", "20"),
    competence[four_digits]
  )
  numeric_competence <- suppressWarnings(as.integer(competence))

  dictionary <- rep(information_system, length(rows))
  dictionary[
    !is.na(numeric_competence) &
      numeric_competence >= 199407L & numeric_competence <= 199910L
  ] <- "SIA-PA-1994-07-1999-10"
  dictionary[
    !is.na(numeric_competence) &
      numeric_competence >= 199911L & numeric_competence <= 200307L
  ] <- "SIA-PA-1999-11-2003-07"
  dictionary[
    !is.na(numeric_competence) &
      numeric_competence >= 200308L & numeric_competence <= 200712L
  ] <- "SIA-PA-2003-08-2007"

  keys <- unique(dictionary)
  stats::setNames(lapply(keys, function(key) which(dictionary == key)), keys)
}

.sia_type_fields <- function(data) {
  fields <- names(data)
  upper <- toupper(fields)
  source_numeric <- fields[vapply(data, is.numeric, logical(1))]
  # These fields are units for a composite age, not categorical values by
  # themselves. PA_FLIDADE is intentionally excluded because it is a status.
  age_unit_fields <- fields[grepl("COIDADE|TPIDADE", upper)]

  value_fields <- fields[
    grepl("(^|_)(VL|VAL)", upper) |
      grepl("DIF_VAL|VALPRO|VALAPR", upper)
  ]
  integer_fields <- fields[
    grepl(
      "QTD|QTDATE|PERMANEN|NUIDADE|IDADEPAC|(^|_)IDADE$|IDADEMIN|IDADEMAX|PESO|ALTURA|DIURES|GLICOS|LINFIN|ESTADI|NUMC[0-9]*$|TOTM|ANOACOM|MESACOM|PONTBAR|TABBARR",
      upper
    ) &
      !grepl("COIDADE|TPIDADE|FLIDADE", upper)
  ]

  # Only eight-digit fields are full dates. Six-digit processing/reference
  # months remain character identifiers of competence.
  date_fields <- fields[vapply(fields, function(field) {
    if (!grepl("DT|DATA", toupper(field))) {
      return(FALSE)
    }
    values <- trimws(as.character(data[[field]]))
    values <- values[!is.na(values) & nzchar(values)]
    length(values) > 0L && all(grepl("^[0-9]{8}$", values))
  }, logical(1))]

  list(
    date = unique(date_fields),
    integer = setdiff(unique(integer_fields), date_fields),
    double = setdiff(
      unique(c(value_fields, source_numeric)),
      unique(c(date_fields, integer_fields))
    ),
    protected = unique(c(
      date_fields, integer_fields, value_fields, source_numeric,
      age_unit_fields,
      # Codes below remain stable identifiers even if the DEF offers
      # analytical groupings for TabWin rows or columns.
      fields[grepl(
        "CID|CNS|CNES|CODUNI|CNPJ|CPF|CEP|AUTORIZ|APAC|NUMAPA|NUMAIH|UFMUN|MUN|DATREF|(^|_)MVM$|(^|_)CMP$|DT_PROCESS|DT_ATEND",
        upper
      )]
    ))
  )
}

.sia_dictionary_fields <- function(
  data,
  dictionaries,
  types,
  nome_proced,
  nome_ocupacao,
  nome_equipe
) {
  declared <- unique(unlist(lapply(
    dictionaries,
    function(dictionary) dictionary$definitions$field
  )))
  fields <- .process_find_fields(data, setdiff(declared, toupper(types$protected)))
  upper <- toupper(fields)
  if (!nome_proced) {
    fields <- fields[!grepl(
      "PROC|CODPRO|PRIPAL|PROCAIH|PRCAIH|ATOPROF",
      upper
    )]
    upper <- toupper(fields)
  }
  if (!nome_ocupacao) {
    fields <- fields[!grepl("CBO|CODOCO", upper)]
    upper <- toupper(fields)
  }
  if (!nome_equipe) {
    fields <- fields[!grepl("(^|_)INE$|EQUI", upper)]
  }
  fields
}

.sia_add_age_fields <- function(data, information_system) {
  if (identical(information_system, "SIA-PA")) {
    value_field <- .process_find_fields(data, "PA_IDADE")
    if (!length(value_field)) {
      return(data)
    }
    value <- .process_as_integer(data[[value_field[[1L]]]])
    # PA uses years directly; 998 and 999 are error/not-required sentinels.
    value[value %in% c(998L, 999L)] <- NA_integer_
    return(.process_add_age_fields(
      data,
      rep("4", length(value)),
      value,
      units = c("4" = "IDADEanos")
    ))
  }

  fields <- if (information_system %in% c("SIA-PS", "SIA-SAD")) {
    c("TPIDADEPAC", "IDADEPAC")
  } else {
    c("AP_COIDADE", "AP_NUIDADE")
  }
  unit_field <- .process_find_fields(data, fields[[1L]])
  value_field <- .process_find_fields(data, fields[[2L]])
  if (!length(unit_field) || !length(value_field)) {
    return(data)
  }

  # APAC and RAAS use the same composite convention as IDADEDET.CNV:
  # unit 2 is days, 3 months, 4 years, and 5 is years above 100.
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

#' Prepare SIA outpatient-production microdata
#'
#' Uses the official DataSUS TabWin definitions to label all twelve SIA file
#' families supported by [fetch_datasus()]. The required ZIP is downloaded on
#' first use and cached for the rest of the R session. For `"SIA-PA"`, the
#' function selects one of three historical definitions by record competence
#' when processing files from before 2008.
#'
#' @param data A data frame returned by [fetch_datasus()] for a supported SIA
#'   file family, or another data frame with a compatible layout.
#' @param information_system SIA file family represented by `data`. The
#'   default `"SIA-PA"` preserves previous calls.
#' @param nome_proced Logical scalar. If `TRUE`, use procedure-description
#'   tables declared by the official DEF. Kept in its original position for
#'   compatibility.
#' @param nome_ocupacao Logical scalar. If `TRUE`, use occupation-description
#'   tables declared by the official DEF.
#' @param nome_equipe Logical scalar. If `TRUE`, use team-description tables
#'   declared by the official DEF.
#' @param municipality_data Logical scalar. If `TRUE`, add municipality names
#'   and available territorial attributes for the patient/residence field
#'   supported by the selected layout.
#'
#' @param labels Output type for categorical labels: `"factor"` (the default),
#'   `"character"`, or `"none"` to retain the original codes.
#' @param diagnostics Logical scalar. If `TRUE`, attach a processing report,
#'   including codes absent from official conversion tables. Retrieve it with
#'   [processing_diagnostics()].
#' @examplesIf interactive() && curl::has_internet()
#' process_sia(sia_pa_sample, nome_proced = FALSE)
#'
#' @return A tibble. Full dates are returned as `Date`; quantities and derived
#'   `IDADEdias`, `IDADEmeses`, and `IDADEanos` fields as integer; values as
#'   double; labelled categorical fields as factors; and identifiers and free
#'   text as character. Derived age fields are added whenever the selected
#'   layout contains patient-age information.
#'
#' @references
#' Saldanha, R. F. (2026). [SIA -- Sistema de Informações Ambulatoriais do
#' SUS](https://rfsaldanha.github.io/sis/sia.html).
#'
#' @seealso [fetch_tabwin_dictionary()], [fetch_datasus()]
#'
#' @export
process_sia <- function(
  data,
  information_system = "SIA-PA",
  nome_proced = TRUE,
  nome_ocupacao = TRUE,
  nome_equipe = TRUE,
  municipality_data = TRUE,
  labels = c("factor", "character", "none"),
  diagnostics = FALSE
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  for (argument in c(
    "nome_proced", "nome_ocupacao", "nome_equipe", "municipality_data"
  )) {
    .datasus_assert_flag(get(argument), argument)
  }
  options <- .process_validate_options(labels, diagnostics)
  labels <- options$labels
  sia_types <- .sia_information_systems
  if (
    !is.character(information_system) ||
      length(information_system) != 1L ||
      is.na(information_system) ||
      !information_system %in% sia_types
  ) {
    cli::cli_abort(
      "{.arg information_system} must be one of: {.val {sia_types}}."
    )
  }

  result <- tibble::as_tibble(data)
  collector <- .process_diagnostic_collector(
    diagnostics, information_system, result
  )
  for (field in names(result)) {
    if (is.factor(result[[field]])) {
      result[[field]] <- as.character(result[[field]])
    }
  }
  result <- .process_normalize_text(result)
  types <- .sia_type_fields(result)
  dictionary_rows <- .sia_dictionary_rows(result, information_system)

  # Resolve every required dictionary before the preprocessing start message.
  dictionaries <- list()
  if (!identical(labels, "none") || diagnostics) {
    for (key in names(dictionary_rows)) {
      if (length(dictionary_rows[[key]])) {
        dictionaries[[key]] <- fetch_tabwin_dictionary(key)
      }
    }
  }
  categorical_fields <- .sia_dictionary_fields(
    result,
    dictionaries,
    types,
    nome_proced,
    nome_ocupacao,
    nome_equipe
  )

  cli::cli_alert_info(
    "Starting {.strong {information_system}} data pre-processing..."
  )

  for (field in types$date) {
    result[[field]] <- .process_as_date(result[[field]], "%Y%m%d", collector, field)
  }
  for (field in setdiff(types$integer, types$double)) {
    result[[field]] <- .process_as_integer(result[[field]], collector = collector, field = field)
  }
  for (field in types$double) {
    result[[field]] <- .process_as_double(result[[field]], collector = collector, field = field)
  }
  result <- .sia_add_age_fields(result, information_system)

  result <- .process_apply_dictionaries(
    result,
    dictionaries,
    categorical_fields,
    dictionary_rows,
    labels = labels,
    collector = collector
  )

  municipality_fields <- c(
    "PA_MUNPCN", "AP_MUNPCN", "MUNPAC", "PA_MUNAT"
  )
  result <- .process_normalize_code_fields(result, municipality_fields)
  if (municipality_data) {
    available <- .process_find_fields(result, municipality_fields)
    if (length(available)) {
      result <- .process_add_municipality_data(result, available[[1L]])
    }
  }

  cli::cli_alert_success(
    "Finished {.strong {information_system}} data pre-processing."
  )
  .process_finalize(result, collector)
}
