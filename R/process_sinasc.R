# Direct categorical fields declared by the current SINASC TabWin DEF.
.sinasc_categorical_fields <- c(
  "ORIGEM", "LOCNASC", "CODESTAB", "ESCMAE", "ESCMAE2010",
  "ESCMAEAGR1", "ESTCIVMAE", "RACACORMAE", "CODOCUPMAE",
  "GESTACAO", "GRAVIDEZ", "PARTO", "CONSULTAS", "SEXO", "RACACOR",
  "IDANOMAL", "TPMETESTIM", "TPAPRESENT", "STTRABPART", "STCESPARTO",
  "TPNASCASSI", "TPFUNCRESP", "TPROBSON", "STDNEPIDEM", "KOTELCHUCK"
)

# The original 1994-1995 files use a different naming convention and their
# own official NASC.DEF conversion tables.
.sinasc_legacy_categorical_fields <- c(
  "LOCAL_OCOR", "SEXO", "RACCOR", "GESTACAO", "TIPO_GRAV",
  "TIPO_PARTO", "PRE_NATAL", "INSTR_MAE"
)

.sinasc_modern_marker_fields <- c(
  "DTNASC", "LOCNASC", "CODMUNRES", "GRAVIDEZ", "ESCMAE"
)

.sinasc_legacy_marker_fields <- c(
  "DATA_NASC", "LOCAL_OCOR", "MUNI_MAE", "TIPO_GRAV", "INSTR_MAE"
)

# Counts and measurements are standardized as integer rather than categorical
# labels, even when the DEF also offers analytical groupings for TabWin.
.sinasc_integer_fields <- c(
  "CONTADOR", "NUMERODV", "IDADEMAE", "QTDFILVIVO", "QTDFILMORT",
  "APGAR1", "APGAR5", "PESO", "DIFDATA", "SERIESCMAE", "QTDGESTANT",
  "QTDPARTNOR", "QTDPARTCES", "IDADEPAI", "SEMAGESTAC", "CONSPRENAT",
  "MESPRENAT", "PARIDADE", "IDADE_MAE", "FIL_VIVOS", "FIL_MORTOS",
  "FIL_ABORT"
)

# Missing-value conventions retained from the published SINASC layouts.
.sinasc_integer_missing <- list(
  "IDADEMAE" = c("0", "00", "99"),
  "IDADEPAI" = c("0", "00"),
  "QTDFILVIVO" = "99",
  "QTDFILMORT" = "99",
  "APGAR1" = "99",
  "APGAR5" = "99",
  "CONSPRENAT" = "99",
  "MESPRENAT" = "99",
  "IDADE_MAE" = c("0", "00", "99"),
  "FIL_VIVOS" = "99",
  "FIL_MORTOS" = "99",
  "FIL_ABORT" = "99"
)

.sinasc_rows_with_values <- function(data, fields) {
  rows <- rep(FALSE, nrow(data))
  for (field in .process_find_fields(data, fields)) {
    value <- trimws(as.character(data[[field]]))
    rows <- rows | (!is.na(value) & nzchar(value))
  }
  which(rows)
}

.sinasc_as_birth_weight <- function(
  x,
  legacy_rows,
  collector = NULL,
  field = NA_character_
) {
  source <- x
  values <- trimws(as.character(x))
  result <- suppressWarnings(as.integer(values))
  # PESO.CNV explicitly accepts 1--7999 g in 1994--1995 and 1--8999 g
  # in the current layout; the remaining four-digit codes are ignored.
  maximum <- rep(8999L, length(result))
  maximum[legacy_rows] <- 7999L
  invalid <- !is.na(result) & (result < 1L | result > maximum)
  result[invalid] <- NA_integer_
  .process_record_coercion(
    collector,
    field,
    "integer",
    source,
    result,
    missing = c("0", "00", "000", "0000", "9999")
  )
  result
}

#' Prepare SINASC live-birth microdata
#'
#' Uses the official DataSUS TabWin definitions to label SINASC live-birth
#' fields. The processor supports both the original 1994-1995 layout and the
#' layout used from 1996 onward, including data sets that contain columns from
#' both periods. Required dictionaries are downloaded on first use and cached
#' for the rest of the R session.
#'
#' Codes absent from the official conversion table remain visible as factor
#' levels. Dates, integer quantities, categorical variables, and identifiers
#' retain distinct types.
#'
#' @param data A data frame returned by [fetch_datasus()] with
#'   `information_system = "SINASC"`, or a compatible layout.
#' @param municipality_data Logical scalar. If `TRUE`, add municipality names
#'   and available territorial attributes for the residence municipality.
#'
#' @param labels Output type for categorical labels: `"factor"` (the default),
#'   `"character"`, or `"none"` to retain the original codes.
#' @param diagnostics Logical scalar. If `TRUE`, attach a processing report,
#'   including codes absent from official conversion tables. Retrieve it with
#'   [processing_diagnostics()].
#' @examplesIf interactive() && curl::has_internet()
#' process_sinasc(sinasc_sample)
#'
#' @return A tibble. Dates are returned as `Date`, counts and measurements as
#'   integer, labelled categorical fields as factors, and identifiers and free
#'   text as character.
#'
#' @references
#' Saldanha, R. F. (2026). [SINASC -- Sistema de Informação sobre Nascidos
#' Vivos](https://rfsaldanha.github.io/sis/sinasc.html).
#'
#' @seealso [fetch_tabwin_dictionary()], [fetch_datasus()]
#'
#' @export
process_sinasc <- function(
  data,
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

  result <- tibble::as_tibble(data)
  collector <- .process_diagnostic_collector(diagnostics, "SINASC", result)
  result <- .process_normalize_text(result)

  # Shared names such as SEXO and GESTACAO have period-specific domains.
  # Layout-exclusive columns identify the applicable dictionary per row in
  # data sets assembled across periods.
  modern_markers <- .process_find_fields(
    result, .sinasc_modern_marker_fields
  )
  legacy_markers <- .process_find_fields(
    result, .sinasc_legacy_marker_fields
  )
  modern_layout <- length(modern_markers) > 0L
  legacy_layout <- length(legacy_markers) > 0L
  if (modern_layout && !legacy_layout) {
    modern_rows <- seq_len(nrow(result))
    legacy_rows <- integer()
  } else if (legacy_layout && !modern_layout) {
    modern_rows <- integer()
    legacy_rows <- seq_len(nrow(result))
  } else {
    legacy_rows <- .sinasc_rows_with_values(
      result, .sinasc_legacy_marker_fields
    )
    modern_rows <- setdiff(
      .sinasc_rows_with_values(result, .sinasc_modern_marker_fields),
      legacy_rows
    )
  }
  modern_fields <- if (length(modern_rows)) {
    .process_find_fields(result, .sinasc_categorical_fields)
  } else {
    character()
  }
  legacy_fields <- if (length(legacy_rows)) {
    .process_find_fields(result, .sinasc_legacy_categorical_fields)
  } else {
    character()
  }

  # Download dictionaries before announcing preprocessing so the lifecycle
  # messages follow any "Cached..." messages shown to the user.
  dictionaries <- list()
  dictionary_rows <- list()
  if (length(modern_fields) &&
      (!identical(labels, "none") || diagnostics)) {
    dictionaries[["SINASC"]] <- fetch_tabwin_dictionary("SINASC")
    dictionary_rows[["SINASC"]] <- modern_rows
  }
  if (length(legacy_fields) &&
      (!identical(labels, "none") || diagnostics)) {
    dictionaries[["SINASC-1994-1995"]] <- fetch_tabwin_dictionary(
      "SINASC-1994-1995"
    )
    dictionary_rows[["SINASC-1994-1995"]] <- legacy_rows
  }

  cli::cli_alert_info(
    "Starting {.strong SINASC} data pre-processing..."
  )

  # Current DT* fields use DDMMYYYY. The 1994-1995 DATA_* layout
  # instead publishes YYYYMMDD, as observed in its official DBC files.
  modern_date_fields <- names(result)[
    startsWith(toupper(names(result)), "DT")
  ]
  for (field in modern_date_fields) {
    result[[field]] <- .process_as_date(
      result[[field]], collector = collector, field = field
    )
  }
  for (field in .process_find_fields(result, c("DATA_NASC", "DATA_CART"))) {
    result[[field]] <- .process_as_date(
      result[[field]], format = "%Y%m%d", collector = collector,
      field = field
    )
  }

  for (field in .process_find_fields(result, .sinasc_integer_fields)) {
    if (identical(toupper(field), "PESO")) {
      result[[field]] <- .sinasc_as_birth_weight(
        result[[field]], legacy_rows, collector, field
      )
    } else {
      missing <- .sinasc_integer_missing[[toupper(field)]]
      if (is.null(missing)) {
        missing <- character()
      }
      result[[field]] <- .process_as_integer(
        result[[field]], missing, collector, field
      )
    }
  }

  if (length(dictionaries)) {
    result <- .process_apply_dictionaries(
      result,
      dictionaries,
      unique(c(modern_fields, legacy_fields)),
      dictionary_rows = dictionary_rows,
      labels = labels,
      collector = collector
    )
  }

  # Municipality identifiers keep six digits and are never converted to
  # factors by analytical municipality groupings in the DEF.
  result <- .process_normalize_code_fields(
    result,
    c(
      "CODMUNNASC", "CODMUNRES", "CODMUNNATU",
      "MUNI_OCOR", "MUNI_MAE"
    )
  )
  if (municipality_data) {
    residence_field <- if (
      length(.process_find_fields(result, "CODMUNRES"))
    ) {
      "CODMUNRES"
    } else {
      "MUNI_MAE"
    }
    result <- .process_add_municipality_data(result, residence_field, collector)
  }

  cli::cli_alert_success(
    "Finished {.strong SINASC} data pre-processing."
  )
  .process_finalize(result, collector)
}
