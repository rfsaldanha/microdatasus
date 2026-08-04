# All CNES products exposed by fetch_datasus() share one official TabWin ZIP.
.cnes_information_systems <- paste0(
  "CNES-",
  c("LT", "ST", "DC", "EQ", "SR", "HB", "PF", "EP", "RC", "IN", "EE", "EF", "GM")
)

.cnes_dictionary_rows <- function(data, information_system) {
  rows <- seq_len(nrow(data))
  if (!identical(information_system, "CNES-SR") || !length(rows)) {
    return(stats::setNames(list(rows), information_system))
  }

  competence_field <- .process_find_fields(data, "COMPETEN")
  if (!length(competence_field)) {
    return(stats::setNames(list(rows), information_system))
  }
  competence <- suppressWarnings(as.integer(
    trimws(as.character(data[[competence_field[[1L]]]]))
  ))
  dictionary <- rep(information_system, length(rows))
  # The portal documents February 2008 as the last competence of the original
  # service/classification table and March 2008 as the first current layout.
  dictionary[!is.na(competence) & competence <= 200802L] <-
    "CNES-SR-2005-08-2008-02"
  keys <- unique(dictionary)
  stats::setNames(lapply(keys, function(key) which(dictionary == key)), keys)
}

.cnes_as_date <- function(x, collector = NULL, field = NA_character_) {
  values <- trimws(as.character(x))
  values[!nzchar(values)] <- NA_character_
  result <- as.Date(rep(NA_character_, length(values)))
  ymd <- !is.na(values) & grepl("^[0-9]{8}$", values)
  dmy <- !is.na(values) & grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", values)
  result[ymd] <- as.Date(values[ymd], format = "%Y%m%d")
  result[dmy] <- as.Date(values[dmy], format = "%d/%m/%Y")
  .process_record_coercion(collector, field, "Date", x, result)
  result
}

.cnes_type_fields <- function(data, dictionaries) {
  fields <- names(data)
  upper <- toupper(fields)

  # Full dates begin with DT in CNES. DT_ATUAL is a six-digit reference month,
  # not a calendar date, and therefore stays character.
  date_fields <- fields[
    grepl("^DT", upper) & upper != "DT_ATUAL"
  ]

  declared_numeric <- unique(unlist(lapply(
    dictionaries,
    function(dictionary) {
      if (is.null(dictionary$numeric_fields)) {
        character()
      } else {
        dictionary$numeric_fields
      }
    }
  )))
  declared_categorical <- unique(unlist(lapply(
    dictionaries,
    function(dictionary) dictionary$definitions$field
  )))
  numeric_fallback <- fields[grepl(
    paste0(
      "^(QT|HORA)|^NULEITOS$|^CONTSRVU$|^S_[A-Z0-9]|",
      "^(MAQ_|SIMUL_RD$|PLANJ_RD$|ARMAZ_FT$|CONF_MAS$|SALA_MOL$|",
      "BLOCOPER$|ORTV[0-9]|ORV[0-9]|OV[0-9]|UN_COBAL$|",
      "EQBR|EQSIS|EQDOS|EQFON)"
    ),
    upper
  )]
  integer_fields <- .process_find_fields(
    data,
    setdiff(
      unique(c(declared_numeric, numeric_fallback)),
      declared_categorical
    )
  )

  identifiers <- fields[grepl(
    paste0(
      "CNES|CNPJ|CPF|CNS|CEP|CODUFMUN|UFMUN|COMPETEN|^CMPT_|",
      "^MAPORTAR$|^PORTARIA$|^REGISTRO$|^SOURCE$|^CO_BANCO$|",
      "^CO_AGENC$|^C_CORREN$|^CONTRAT|^ALVARA$|^CLASS_SR$"
    ),
    upper
  )]
  free_text <- fields[grepl(
    "^(NOME|DESC)|^REGSAUDE$|^MICR_REG$|^DISTRSAN$|^DISTRADM$",
    upper
  )]
  protected <- unique(c(
    date_fields,
    integer_fields,
    identifiers,
    free_text
  ))

  # Preserve numeric input supplied directly by users when the field is not a
  # DEF increment, identifier, date, or categorical variable.
  source_double <- fields[vapply(data, is.double, logical(1))]
  double_fields <- setdiff(
    source_double,
    unique(c(protected, .process_find_fields(data, declared_categorical)))
  )

  list(
    date = unique(date_fields),
    integer = setdiff(unique(integer_fields), date_fields),
    double = unique(double_fields),
    identifier = setdiff(unique(c(identifiers, free_text)), date_fields),
    protected = unique(c(protected, double_fields))
  )
}

.cnes_dictionary_fields <- function(data, dictionaries, types) {
  declared <- unique(unlist(lapply(
    dictionaries,
    function(dictionary) dictionary$definitions$field
  )))
  fields <- .process_find_fields(
    data,
    setdiff(declared, toupper(types$protected))
  )
  # Establishment names are added in FANTASIA when requested, preserving CNES.
  # Service/classification needs a six-digit key built from two source fields.
  fields[!toupper(fields) %in% c("CNES", "SERV_ESP")]
}

.cnes_apply_service_classification <- function(
  data,
  dictionaries,
  dictionary_rows,
  labels = "factor",
  collector = NULL
) {
  service <- .process_find_fields(data, "SERV_ESP")
  classification <- .process_find_fields(data, "CLASS_SR")
  if (!length(service) || !length(classification)) {
    return(data)
  }
  service <- service[[1L]]
  classification <- classification[[1L]]
  result <- as.character(data[[service]])

  for (key in names(dictionaries)) {
    rows <- dictionary_rows[[key]]
    combined <- paste0(
      trimws(as.character(data[[service]][rows])),
      trimws(as.character(data[[classification]][rows]))
    )
    selected <- .tabwin_select_conversion(
      dictionaries[[key]],
      "SERV_ESP",
      combined
    )
    if (!is.null(selected)) {
      converted <- .tabwin_apply_conversion_values(combined, selected)
      .process_record_dictionary_diagnostics(
        collector,
        service,
        key,
        combined,
        selected
      )
      if (!identical(labels, "none")) {
        result[rows] <- converted
      }
    }
  }
  if (identical(labels, "factor")) {
    data[[service]] <- factor(result)
  } else if (identical(labels, "character")) {
    data[[service]] <- result
  }
  data
}

.cnes_add_establishment_names <- function(data, dictionary) {
  field <- .process_find_fields(data, "CNES")
  if (!length(field)) {
    return(data)
  }
  field <- field[[1L]]
  selected <- .tabwin_select_conversion(
    dictionary,
    "CNES",
    data[[field]]
  )
  if (is.null(selected)) {
    return(data)
  }

  codes <- trimws(as.character(data[[field]]))
  # Unlike ordinary categorical conversion, a failed entity lookup is missing
  # rather than a factor containing the original identifier.
  data$FANTASIA <- unname(selected$conversion$map[codes])
  data
}

#' Prepare CNES microdata
#'
#' Uses the official DataSUS TabWin definitions to label all thirteen CNES file
#' families supported by [fetch_datasus()]. The shared ZIP is downloaded on
#' first use and cached for the rest of the R session. For `"CNES-SR"`, the
#' service/classification definition is selected by record competence because
#' DataSUS changed that table in March 2008.
#'
#' @param data A data frame returned by [fetch_datasus()] for a supported CNES
#'   file family, or another data frame with a compatible layout.
#' @param information_system CNES file family represented by `data`. If
#'   omitted, `"CNES-ST"` is used, preserving the first value of the historical
#'   default.
#' @param nomes Logical scalar. If `TRUE`, add `FANTASIA` using the
#'   establishment-name DBF declared by the official definition. The `CNES`
#'   identifier itself is preserved.
#' @param municipality_data Logical scalar. If `TRUE`, add municipality names
#'   and available territorial attributes. Professional files prefer
#'   `UFMUNRES`; other layouts use the establishment field `CODUFMUN`.
#'
#' @param labels Output type for categorical labels: `"factor"` (the default),
#'   `"character"`, or `"none"` to retain the original codes.
#' @param diagnostics Logical scalar. If `TRUE`, attach a processing report,
#'   including codes absent from official conversion tables. Retrieve it with
#'   [processing_diagnostics()].
#' @examplesIf interactive() && curl::has_internet()
#' process_cnes(cnes_st_sample, information_system = "CNES-ST")
#' process_cnes(cnes_pf_sample, information_system = "CNES-PF")
#'
#' @return A tibble. Full dates are returned as `Date`, quantities as integer,
#'   labelled categorical fields as factors, and identifiers, reference months,
#'   and free text as character.
#'
#' @references
#' Saldanha, R. F. (2026). [CNES -- Cadastro Nacional de Estabelecimentos de
#' Saúde](https://rfsaldanha.github.io/sis/cnes.html).
#'
#' @seealso [fetch_tabwin_dictionary()], [fetch_datasus()], [fetch_cadger()]
#'
#' @export
process_cnes <- function(
  data,
  information_system = c("CNES-ST", "CNES-PF"),
  nomes = FALSE,
  municipality_data = TRUE,
  labels = c("factor", "character", "none"),
  diagnostics = FALSE
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  .datasus_assert_flag(nomes, "nomes")
  .datasus_assert_flag(municipality_data, "municipality_data")
  options <- .process_validate_options(labels, diagnostics)
  labels <- options$labels

  # The original default was a two-value vector even though the implementation
  # required one value. Missing calls historically meant the first, CNES-ST.
  if (missing(information_system)) {
    information_system <- information_system[[1L]]
  }
  cnes_types <- .cnes_information_systems
  if (
    !is.character(information_system) ||
      length(information_system) != 1L ||
      is.na(information_system) ||
      !information_system %in% cnes_types
  ) {
    cli::cli_abort(
      "{.arg information_system} must be one of: {.val {cnes_types}}."
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
  dictionary_rows <- .cnes_dictionary_rows(result, information_system)

  # Resolve every required DEF before announcing that preprocessing has begun.
  # This keeps the cache/start/finish lifecycle visible in a stable order.
  dictionaries <- list()
  for (key in names(dictionary_rows)) {
    if (length(dictionary_rows[[key]])) {
      dictionaries[[key]] <- fetch_tabwin_dictionary(key)
    }
  }
  types <- .cnes_type_fields(result, dictionaries)
  categorical_fields <- .cnes_dictionary_fields(
    result,
    dictionaries,
    types
  )

  cli::cli_alert_info(
    "Starting {.strong {information_system}} data pre-processing..."
  )

  for (field in types$identifier) {
    result[[field]] <- as.character(result[[field]])
  }
  for (field in types$date) {
    result[[field]] <- .cnes_as_date(result[[field]], collector, field)
  }
  for (field in setdiff(types$integer, types$double)) {
    result[[field]] <- .process_as_integer(result[[field]], collector = collector, field = field)
  }
  for (field in types$double) {
    result[[field]] <- .process_as_double(result[[field]], collector = collector, field = field)
  }

  result <- .process_apply_dictionaries(
    result,
    dictionaries,
    categorical_fields,
    dictionary_rows,
    labels = labels,
    collector = collector
  )
  if (identical(information_system, "CNES-SR")) {
    result <- .cnes_apply_service_classification(
      result,
      dictionaries,
      dictionary_rows,
      labels = labels,
      collector = collector
    )
  }
  if (nomes && length(dictionaries)) {
    result <- .cnes_add_establishment_names(
      result,
      dictionaries[[1L]]
    )
  }

  result <- .process_normalize_code_fields(
    result,
    c("CODUFMUN", "UFMUNRES")
  )
  if (municipality_data) {
    municipality_fields <- if (identical(information_system, "CNES-PF")) {
      c("UFMUNRES", "CODUFMUN")
    } else {
      "CODUFMUN"
    }
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
