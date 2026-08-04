# Fields whose raw codes are replaced by the most appropriate direct
# conversion declared in the current SIM CID-10 death-certificate DEF.
.sim_categorical_fields <- c(
  "ORIGEM", "CODINST", "TIPOBITO", "NATURAL", "SEXO", "RACACOR", "ESTCIV",
  "ESC", "ESC2010", "SERIESCFAL", "OCUP", "LOCOCOR", "CODESTAB", "ESCMAE",
  "ESCFALAGR1", "ESCMAE2010", "SERIESCMAE", "ESCMAEAGR1", "OCUPMAE",
  "GRAVIDEZ", "GESTACAO",
  "PARTO", "OBITOPARTO", "OBITOGRAV", "OBITOPUERP", "ASSISTMED",
  "EXAME", "CIRURGIA", "NECROPSIA", "CIRCOBITO", "ACIDTRAB",
  "FONTE", "TPPOS", "ATESTANTE", "FONTEINV", "TPMORTEOCO",
  "CAUSAMAT", "STDONOVA", "STDOEPIDEM", "TPOBITOCOR", "MORTEPARTO",
  "STCODIFICA", "CODIFICADO", "RETROALIM", "TPRESGINFO",
  "TPNIVELINV", "ALTCAUSA",
  # Names used by the oldest national fetal-death files.
  "OCUPACAO", "OCUPPAI", "INSTRUCAO", "INSTRPAI", "INSTRMAE",
  "TIPOGRAV", "TIPOPARTO", "TIPOVIOL", "FONTINFO"
)

# These fields contain counts or measurements rather than identifiers.
.sim_integer_fields <- c(
  "CONTADOR", "IDADEMAE", "QTDFILVIVO", "QTDFILMORT", "SEMAGESTAC",
  "PESO", "DIFDATA", "NUDIASOBCO", "NUDIASOBIN", "NUDIASINF",
  "FILHVIVOS", "FILHMORT", "SEMANGEST", "PESONASC", "QTDGRAVIDA",
  "QTDPARTNOR", "QTDPARTCES", "QTDABORTO", "QTDPRENAT", "NUIDADEGES",
  "IDADEGESPR", "IDADEGESOB"
)

# Legacy variable names are looked up in the modern DEF under the equivalent
# current SIM field. Output column names are deliberately left unchanged.
.sim_dictionary_aliases <- c(
  "OCUPACAO" = "OCUP",
  "OCUPPAI" = "OCUP",
  "INSTRUCAO" = "ESC",
  "INSTRPAI" = "ESC",
  "INSTRMAE" = "ESCMAE",
  "TIPOGRAV" = "GRAVIDEZ",
  "TIPOPARTO" = "PARTO",
  "TIPOVIOL" = "CIRCOBITO",
  "FONTINFO" = "FONTE"
)

.sim_add_age_fields <- function(data) {
  if (!"IDADE" %in% names(data)) {
    return(data)
  }
  # SIM stores age as a unit digit followed by a two-digit value:
  # 0 minutes, 1 hours, 2 days, 3 months, 4 years, and 5 years + 100.
  age <- as.character(data$IDADE)
  age[age %in% c("000", "999")] <- NA_character_
  unit <- substring(age, 1L, 1L)
  value <- suppressWarnings(as.integer(substring(age, 2L, 3L)))
  data$IDADE <- age
  .process_add_age_fields(
    data,
    unit,
    value,
    units = c(
      "0" = "IDADEminutos",
      "1" = "IDADEhoras",
      "2" = "IDADEdias",
      "3" = "IDADEmeses",
      "4" = "IDADEanos",
      "5" = "IDADEanos"
    ),
    century_units = "5"
  )
}

#' Prepare SIM mortality microdata
#'
#' Uses the current official DataSUS TabWin CID-10 dictionary to label
#' supported SIM mortality fields from 1996 onward. The dictionary is
#' downloaded on first use and cached for the rest of the R session. Dates,
#' integer quantities, categorical variables, and identifier fields retain
#' distinct and consistent types.
#'
#' Codes not covered by the current TabWin conversion are retained as factor
#' levels instead of being silently discarded. SIM files and definitions that
#' use CID-9 are outside the scope of this version.
#'
#' @param data A data frame returned by [fetch_datasus()] for a supported SIM
#'   mortality type, or another data frame with a compatible layout.
#' @param municipality_data Logical scalar. If `TRUE`, add municipality names
#'   and available territorial attributes for `CODMUNRES`.
#' @param information_system SIM data type represented by `data`. One of
#'   `"SIM-DO"`, `"SIM-DOFET"`, `"SIM-DOEXT"`, `"SIM-DOINF"`, or
#'   `"SIM-DOMAT"`. The default preserves the previous `process_sim()` call.
#' @param labels Output type for categorical labels: `"factor"` (the default),
#'   `"character"`, or `"none"` to retain the original codes.
#' @param diagnostics Logical scalar. If `TRUE`, attach a processing report,
#'   including codes absent from official conversion tables. Retrieve it with
#'   [processing_diagnostics()].
#'
#' @examplesIf interactive() && curl::has_internet()
#' process_sim(sim_do_sample)
#'
#' @return A tibble. Dates are returned as `Date`, quantities as integer,
#'   labelled categorical fields as factors, and identifiers and free text as
#'   character.
#'
#' @references
#' Saldanha, R. F. (2026). [SIM -- Sistema de Informação sobre
#' Mortalidade](https://rfsaldanha.github.io/sis/sim.html).
#'
#' @seealso [fetch_tabwin_dictionary()], [fetch_datasus()]
#'
#' @export
process_sim <- function(
  data,
  municipality_data = TRUE,
  information_system = "SIM-DO",
  labels = c("factor", "character", "none"),
  diagnostics = FALSE
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  .datasus_assert_flag(municipality_data, "municipality_data")
  options <- .process_validate_options(labels, diagnostics)
  labels <- options$labels
  sim_types <- grep("^SIM-", names(.tabwin_registry()), value = TRUE)
  if (
    !is.character(information_system) ||
      length(information_system) != 1L ||
      is.na(information_system) ||
      !information_system %in% sim_types
  ) {
    cli::cli_abort(
      "{.arg information_system} must be one of: {.val {sim_types}}."
    )
  }

  result <- tibble::as_tibble(data)
  collector <- .process_diagnostic_collector(
    diagnostics, information_system, result
  )
  result <- .process_normalize_text(result)

  categorical_fields <- .process_find_fields(result, .sim_categorical_fields)
  if (length(categorical_fields) &&
      (!identical(labels, "none") || diagnostics)) {
    # The first call downloads the archive; later calls retrieve this object
    # and all already-parsed maps from the session cache.
    dictionary <- fetch_tabwin_dictionary(information_system)
  }

  cli::cli_alert_info(
    "Starting {.strong {information_system}} data pre-processing..."
  )

  # Type conversion precedes labelling so measurements never become factors.
  date_fields <- names(result)[startsWith(toupper(names(result)), "DT")]
  for (field in date_fields) {
    result[[field]] <- .process_as_date(result[[field]], collector = collector, field = field)
  }
  integer_fields <- .process_find_fields(result, .sim_integer_fields)
  for (field in integer_fields) {
    result[[field]] <- .process_as_integer(result[[field]], collector = collector, field = field)
  }
  result <- .sim_add_age_fields(result)

  if (length(categorical_fields) &&
      (!identical(labels, "none") || diagnostics)) {
    result <- .process_apply_dictionary(
      result,
      dictionary,
      categorical_fields,
      aliases = .sim_dictionary_aliases,
      labels = labels,
      collector = collector
    )
  }

  # Municipality codes remain identifiers. Optional territorial attributes
  # are joined only after their width has been normalized.
  for (field in intersect(c("CODMUNRES", "CODMUNOCOR"), names(result))) {
    result[[field]] <- substring(as.character(result[[field]]), 1L, 6L)
  }
  if (municipality_data) {
    result <- .process_add_municipality_data(result, "CODMUNRES", collector)
  }

  cli::cli_alert_success(
    "Finished {.strong {information_system}} data pre-processing."
  )
  .process_finalize(result, collector)
}
