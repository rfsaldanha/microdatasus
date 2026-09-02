.sinan_as_date <- function(x, collector = NULL, field = NA_character_) {
  if (inherits(x, "Date")) {
    return(x)
  }
  values <- trimws(as.character(x))
  missing <- c("0", "000000", "00000000", "********")
  values[!nzchar(values) | values %in% missing] <- NA_character_
  result <- as.Date(rep(NA_character_, length(values)))
  iso <- !is.na(values) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", values)
  ymd <- !is.na(values) & grepl("^[0-9]{8}$", values)
  dmy <- !is.na(values) & grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", values)
  dmy_short <- !is.na(values) & grepl(
    "^[0-9]{2}/[0-9]{2}/[0-9]{2}$", values
  )
  result[iso] <- as.Date(values[iso])
  result[ymd] <- as.Date(values[ymd], format = "%Y%m%d")
  result[dmy] <- as.Date(values[dmy], format = "%d/%m/%Y")
  result[dmy_short] <- as.Date(values[dmy_short], format = "%d/%m/%y")
  .process_record_coercion(collector, field, "Date", x, result, missing)
  result
}

.sinan_municipality_fields <- function(data) {
  fields <- names(data)
  upper <- toupper(fields)
  fields[
    upper %in% c("ID_MUNICIP", "ID_MN_RESI", "MUNICIPIO") |
      grepl("^ID_MN_|^MUN(_|$)|^CO_MUN", upper)
  ]
}

.sinan_type_fields <- function(data, dictionary) {
  fields <- names(data)
  upper <- toupper(fields)
  # Most SINAN dates start with DT, but several official DBF layouts expose
  # historical date fields under semantic names. Existing Date columns are
  # authoritative and must never be passed to a categorical CNV conversion.
  date_aliases <- c("PARASITO", "RESUL_HIS", "TRATAMENTO", "DEXAME", "DTRATA")
  existing_dates <- vapply(data, inherits, logical(1), "Date")
  physical_types <- attr(data, "dbf_field_types", exact = TRUE)
  physical_dates <- if (is.null(physical_types)) {
    character()
  } else {
    physical_field_types <- toupper(unname(physical_types[fields]))
    # SEM_PRI is a WWYYYY epidemiological-week key. Two official SDTA
    # layouts declare it as DBF D, while the newer layout declares it as N.
    fields[
      !is.na(physical_field_types) & physical_field_types == "D" &
        upper != "SEM_PRI"
    ]
  }
  alias_dates <- vapply(seq_along(fields), function(index) {
    if (!upper[[index]] %in% date_aliases || existing_dates[[index]]) {
      return(FALSE)
    }
    values <- trimws(as.character(data[[index]]))
    values <- values[
      !is.na(values) & nzchar(values) &
        !values %in% c("0", "000000", "00000000", "********")
    ]
    any(grepl(
      paste0(
        "^[0-9]{8}$|^[0-9]{4}-[0-9]{2}-[0-9]{2}$|",
        "^[0-9]{2}/[0-9]{2}/[0-9]{2}([0-9]{2})?$"
      ),
      values
    ))
  }, logical(1))
  date_fields <- fields[
    grepl("DT|DATA", upper) |
      fields %in% physical_dates |
      alias_dates |
      existing_dates
  ]
  municipality_fields <- .sinan_municipality_fields(data)
  identifiers <- fields[grepl(
    paste0(
      "^ID_(UNIDADE|REGIONA|DISTRIT|BAIRRO|OCUPA)|^ID_AGRAVO$|",
      "^NU_NOTIFIC$|^NU_IDADE_N$|^NU_(CEP|CNS)|CPF|CNPJ|^SOURCE$"
    ),
    upper
  )]
  reference_fields <- fields[
    grepl("^SEM_|^NU_ANO$", upper)
  ]
  # The official meningitis data dictionary defines ANT_OU_DE as the
  # varchar2(30) description of an "other" vaccine. MeningeNET.def
  # incorrectly reuses it for year/month analytical relations, which would
  # turn free text such as "03 HEPATITE" into a month label.
  free_text <- fields[
    grepl("^(NM|DS|NO)_", upper) | upper == "ANT_OU_DE"
  ]

  # DEF I commands identify exact counts or measurements. They take priority
  # over optional analytical groupings of the same source field.
  declared_numeric <- if (is.null(dictionary[["numeric_fields"]])) {
    character()
  } else {
    dictionary[["numeric_fields"]]
  }
  integer_fields <- .process_find_fields(
    data,
    unique(c("NU_ANO", declared_numeric))
  )

  list(
    date = unique(date_fields),
    integer = setdiff(unique(integer_fields), date_fields),
    identifier = setdiff(
      unique(c(
        identifiers,
        reference_fields,
        free_text,
        municipality_fields
      )),
      unique(c(date_fields, integer_fields))
    ),
    protected = unique(c(
      date_fields,
      integer_fields,
      identifiers,
      reference_fields,
      free_text,
      municipality_fields
    ))
  )
}

.sinan_add_age_fields <- function(data) {
  field <- .process_find_fields(data, "NU_IDADE_N")
  if (!length(field)) {
    return(data)
  }
  field <- field[[1L]]
  code <- trimws(as.character(data[[field]]))
  code[code %in% c("", "999", "9999")] <- NA_character_
  unit <- substr(code, 1L, 1L)
  value <- suppressWarnings(as.integer(substr(code, 2L, 4L)))
  data[[field]] <- code
  .process_add_age_fields(
    data,
    unit,
    value,
    units = c(
      "0" = "IDADEminutos",
      "1" = "IDADEhoras",
      "2" = "IDADEdias",
      "3" = "IDADEmeses",
      "4" = "IDADEanos"
    )
  )
}

.sinan_dictionary_fields <- function(data, dictionary, types) {
  declared <- unique(dictionary$definitions$field)
  # NotIndiviNet.def is a catalogue containing disease-specific alternatives,
  # not a disease-neutral dictionary. Only fields whose definitions are shared
  # unchanged by the dedicated official DEFs are safe for generic families.
  definition <- dictionary$definition
  generic <- length(definition) == 1L &&
    identical(tolower(basename(definition)), "notindivinet.def")
  if (generic) {
    declared <- intersect(
      declared,
      c("CS_ESCOL_N", "CS_RACA", "CS_SEXO", "CS_GESTANT", "CS_ZONA")
    )
  }
  .process_find_fields(
    data,
    setdiff(declared, toupper(types$protected))
  )
}

#' Prepare SINAN notification microdata
#'
#' Uses the official DataSUS TabWin definitions to label all SINAN file
#' families supported by [fetch_datasus()]. The corresponding
#' `TAB_SINANNET.zip` or `TAB_SINANONLINE.zip` archive is downloaded on first
#' use and cached for the rest of the R session. When DataSUS publishes no
#' disease-specific DEF, the official `NotIndiviNet.def` supplies labels for
#' common notification fields; unmapped disease-specific codes remain visible.
#' Historical chikungunya records that use the former generic classification
#' domain additionally reuse its official relation from `TAB_SINANNET.zip`.
#'
#' @param data A data frame returned by [fetch_datasus()] for a supported SINAN
#'   file family, or another data frame with a compatible layout.
#' @param information_system SINAN file family represented by `data`. Preferred
#'   values use readable names such as `"SINAN-DENGUE"` and
#'   `"SINAN-TUBERCULOSE"`. All former acronym-based values remain accepted as
#'   aliases. Use [datasus_information_systems()] and filter `system == "SINAN"`
#'   to consult both forms.
#' @param municipality_data Logical scalar. If `TRUE`, add municipality names
#'   and available territorial attributes. The historical `MUNICIPIO` field is
#'   preferred when present, followed by residence and notification fields.
#'
#' @param labels Output type for categorical labels: `"factor"` (the default),
#'   `"character"`, or `"none"` to retain the original codes.
#' @param diagnostics Logical scalar. If `TRUE`, attach a processing report,
#'   including codes absent from official conversion tables. Retrieve it with
#'   [processing_diagnostics()].
#' @examplesIf interactive() && curl::has_internet()
#' process_sinan(sinan_dengue_sample, "SINAN-DENGUE")
#' process_sinan(sinan_chagas_sample, "SINAN-DOENCA-DE-CHAGAS-AGUDA")
#'
#' @return A tibble. Dates are returned as `Date`, DEF increment fields and
#'   derived age components as integer, labelled categorical fields as factors,
#'   and identifiers and free text as character.
#'
#' @references
#' Saldanha, R. F. (2026). [SINAN -- Sistema de Informação de Agravos de
#' Notificação](https://rfsaldanha.github.io/sis/sinan.html).
#'
#' @seealso [datasus_information_systems()], [fetch_tabwin_dictionary()],
#'   [fetch_datasus()]
#'
#' @export
process_sinan <- function(
  data,
  information_system = "SINAN-DENGUE",
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
  information_system <- .sinan_resolve_information_system(
    information_system
  )
  sinan_types <- .sinan_information_systems()
  if (
    !is.character(information_system) ||
      length(information_system) != 1L ||
      is.na(information_system) ||
      !information_system %in% sinan_types
  ) {
    cli::cli_abort(
      "{.arg information_system} must be one of: {.val {sinan_types}}."
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

  # Resolve the DEF before the preprocessing start message so users see the
  # same cache/start/finish lifecycle as the other system processors.
  dictionary <- fetch_tabwin_dictionary(information_system)
  types <- .sinan_type_fields(result, dictionary)
  categorical_fields <- .sinan_dictionary_fields(
    result,
    dictionary,
    types
  )

  # The 2014 chikungunya DBC mixes the current Classchik domain (5/13) with
  # the earlier generic SINAN classification domain (1/2/8). Classifi.cnv is
  # still published in the SINAN Net archive and defines those legacy codes.
  chik_classification <- character()
  legacy_chik_rows <- integer()
  current_chik_rows <- integer()
  if (identical(information_system, "SINAN-FEBRE-DE-CHIKUNGUNYA")) {
    chik_classification <- .process_find_fields(result, "CLASSI_FIN")
    if (length(chik_classification)) {
      values <- trimws(as.character(result[[chik_classification[[1L]]]]))
      legacy_chik_rows <- which(values %in% c("1", "2", "8"))
    }
  }
  if (length(legacy_chik_rows)) {
    current_chik_rows <- setdiff(seq_len(nrow(result)), legacy_chik_rows)
    categorical_fields <- categorical_fields[
      toupper(categorical_fields) != "CLASSI_FIN"
    ]
    legacy_chik_dictionary <- fetch_tabwin_dictionary(
      "SINAN-FEBRE-TIFOIDE"
    )
  }

  cli::cli_alert_info(
    "Starting {.strong {information_system}} data pre-processing..."
  )

  for (field in types$identifier) {
    result[[field]] <- as.character(result[[field]])
  }
  for (field in types$date) {
    result[[field]] <- .sinan_as_date(result[[field]], collector, field)
  }
  for (field in types$integer) {
    result[[field]] <- .process_as_integer(result[[field]], collector = collector, field = field)
  }
  result <- .sinan_add_age_fields(result)
  result <- .process_apply_dictionary(
    result,
    dictionary,
    categorical_fields,
    labels = labels,
    collector = collector
  )
  if (length(legacy_chik_rows)) {
    if (length(current_chik_rows)) {
      result <- .process_apply_dictionary(
        result,
        dictionary,
        chik_classification,
        rows = current_chik_rows,
        labels = labels,
        collector = collector
      )
    }
    result <- .process_apply_dictionary(
      result,
      legacy_chik_dictionary,
      chik_classification,
      rows = legacy_chik_rows,
      labels = labels,
      collector = collector
    )
  }

  municipality_fields <- .sinan_municipality_fields(result)
  result <- .process_normalize_code_fields(result, municipality_fields)
  if (municipality_data && length(municipality_fields)) {
    priority <- c("MUNICIPIO", "ID_MN_RESI", "ID_MUNICIP")
    selected <- .process_find_fields(result, priority)
    if (!length(selected)) {
      selected <- municipality_fields
    }
    result <- .process_add_municipality_data(result, selected[[1L]], collector)
  }

  cli::cli_alert_success(
    "Finished {.strong {information_system}} data pre-processing."
  )
  .process_finalize(result, collector)
}
