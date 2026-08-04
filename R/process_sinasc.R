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
  "IDADEMAE" = c("0", "99"),
  "IDADEPAI" = c("0", "99"),
  "QTDFILVIVO" = "99",
  "QTDFILMORT" = "99",
  "APGAR1" = "99",
  "APGAR5" = "99",
  "PESO" = c("0", "9999"),
  "IDADE_MAE" = c("0", "99"),
  "FIL_VIVOS" = "99",
  "FIL_MORTOS" = "99",
  "FIL_ABORT" = "99"
)

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

  # Shared names such as SEXO exist in both layouts, so use layout-exclusive
  # markers to decide which official dictionary is required.
  modern_layout <- length(.process_find_fields(
    result,
    c("DTNASC", "LOCNASC", "CODMUNRES", "GRAVIDEZ", "ESCMAE")
  )) > 0L
  legacy_layout <- length(.process_find_fields(
    result,
    c("DATA_NASC", "LOCAL_OCOR", "MUNI_MAE", "TIPO_GRAV", "INSTR_MAE")
  )) > 0L
  modern_fields <- if (modern_layout) {
    .process_find_fields(result, .sinasc_categorical_fields)
  } else {
    character()
  }
  legacy_fields <- if (legacy_layout) {
    .process_find_fields(result, .sinasc_legacy_categorical_fields)
  } else {
    character()
  }
  if (modern_layout && legacy_layout) {
    # In a row-bound cross-period data set, use the current dictionary for
    # shared columns and the historical dictionary for legacy-only columns.
    legacy_fields <- legacy_fields[
      !toupper(legacy_fields) %in% .sinasc_categorical_fields
    ]
  }

  # Download dictionaries before announcing preprocessing so the lifecycle
  # messages follow any "Cached..." messages shown to the user.
  if (length(modern_fields) &&
      (!identical(labels, "none") || diagnostics)) {
    modern_dictionary <- fetch_tabwin_dictionary("SINASC")
  }
  if (length(legacy_fields) &&
      (!identical(labels, "none") || diagnostics)) {
    legacy_dictionary <- fetch_tabwin_dictionary("SINASC-1994-1995")
  }

  cli::cli_alert_info(
    "Starting {.strong SINASC} data pre-processing..."
  )

  # Every DT* column is an eight-digit DataSUS date. The original files use
  # DATA_NASC and DATA_CART instead.
  date_fields <- c(
    names(result)[startsWith(toupper(names(result)), "DT")],
    .process_find_fields(result, c("DATA_NASC", "DATA_CART"))
  )
  for (field in unique(date_fields)) {
    result[[field]] <- .process_as_date(result[[field]], collector = collector, field = field)
  }

  for (field in .process_find_fields(result, .sinasc_integer_fields)) {
    missing <- .sinasc_integer_missing[[toupper(field)]]
    if (is.null(missing)) {
      missing <- character()
    }
    result[[field]] <- .process_as_integer(result[[field]], missing, collector, field)
  }

  if (length(modern_fields) &&
      (!identical(labels, "none") || diagnostics)) {
    result <- .process_apply_dictionary(
      result,
      modern_dictionary,
      modern_fields,
      labels = labels,
      collector = collector
    )
  }
  if (length(legacy_fields) &&
      (!identical(labels, "none") || diagnostics)) {
    result <- .process_apply_dictionary(
      result,
      legacy_dictionary,
      legacy_fields,
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
    result <- .process_add_municipality_data(result, residence_field)
  }

  cli::cli_alert_success(
    "Finished {.strong SINASC} data pre-processing."
  )
  .process_finalize(result, collector)
}
