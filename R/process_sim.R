.sim_information_systems <- c(
  "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT"
)

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
  "SEMANGEST", "TIPOGRAV", "TIPOPARTO", "TIPOVIOL", "TIPOACID",
  "FONTINFO", "LOCACID"
)

# These fields contain counts or measurements rather than identifiers.
.sim_integer_fields <- c(
  "CONTADOR", "IDADEMAE", "QTDFILVIVO", "QTDFILMORT", "SEMAGESTAC",
  "PESO", "DIFDATA", "NUDIASOBCO", "NUDIASOBIN", "NUDIASINF",
  "FILHVIVOS", "FILHMORT", "PESONASC", "QTDGRAVIDA",
  "QTDPARTNOR", "QTDPARTCES", "QTDABORTO", "QTDPRENAT", "NUIDADEGES",
  "IDADEGESPR", "IDADEGESOB"
)

# The reduced fetal-death files for the first CID-10 years retain fields from
# the previous SIM layout. Their codes must use the official CID-9-era DEF;
# several domains are not aliases of their modern counterparts.
.sim_legacy_categorical_fields <- c(
  "TIPOBITO", "NATURAL", "SEXO", "ESTCIV", "LOCOCOR", "OCUPMAE",
  "GESTACAO", "ASSISTMED", "ATESTANTE", "EXAME", "CIRURGIA",
  "NECROPSIA", "ACIDTRAB", "OCUPACAO", "OCUPPAI", "INSTRUCAO",
  "INSTRPAI", "INSTRMAE", "SEMANGEST", "TIPOGRAV", "TIPOPARTO",
  "TIPOVIOL", "TIPOACID", "FONTINFO", "LOCACID"
)

.sim_legacy_dictionary_aliases <- c(
  "ESTCIV" = "ESTCIVIL",
  "GESTACAO" = "SEMANGEST"
)

.sim_legacy_marker_fields <- c(
  "UFINFORM", "OCUPACAO", "OCUPPAI", "INSTRUCAO", "INSTRPAI",
  "INSTRMAE", "FILHVIVOS", "FILHMORT", "SEMANGEST", "TIPOGRAV",
  "TIPOPARTO", "TIPOVIOL", "TIPOACID", "FONTINFO", "LOCACID"
)

.sim_legacy_rows <- function(data) {
  legacy <- rep(FALSE, nrow(data))
  for (field in .process_find_fields(data, .sim_legacy_marker_fields)) {
    value <- trimws(as.character(data[[field]]))
    legacy <- legacy | (!is.na(value) & nzchar(value))
  }
  # The old CBO field is three characters; current CBO 2002 codes have six.
  occupation <- .process_find_fields(data, "OCUPMAE")
  if (length(occupation)) {
    value <- trimws(as.character(data[[occupation[[1L]]]]))
    legacy <- legacy | (!is.na(value) & grepl("^[0-9]{1,3}$", value))
  }
  which(legacy)
}

.sim_legacy_count_fields <- c(
  "FILHVIVOS", "FILHMORT", "QTDFILVIVO", "QTDFILMORT"
)

.sim_as_legacy_count <- function(x, rows, collector = NULL,
                                 field = NA_character_) {
  source <- x
  values <- trimws(as.character(x))
  result <- suppressWarnings(as.integer(values))
  if (length(rows)) {
    # The CID-9 manual and NUMFILH.CNV define XX as none, 00 as
    # ignored, and only the explicit counts 01 through 15 as valid.
    legacy <- values[rows]
    valid <- legacy %in% c(as.character(1:15), sprintf("%02d", 1:15))
    result[rows] <- NA_integer_
    result[rows[valid]] <- suppressWarnings(as.integer(legacy[valid]))
    result[rows[which(legacy == "XX")]] <- 0L
  }
  .process_record_coercion(
    collector,
    field,
    "integer",
    source,
    result,
    missing = c("0", "00")
  )
  result
}

.sim_weight_fields <- c("PESO", "PESONASC")

.sim_as_weight <- function(x, collector = NULL, field = NA_character_) {
  source <- x
  values <- trimws(as.character(x))
  result <- suppressWarnings(as.integer(values))
  # PESO.CNV defines 1--8000 g; zero and all larger codes are ignored.
  invalid <- !is.na(result) & (result < 1L | result > 8000L)
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
#' Uses the official DataSUS TabWin CID-10 and historical CID-9 dictionaries
#' to label supported SIM mortality fields with period-correct domains. The dictionary is
#' downloaded on first use and cached for the rest of the R session. Dates,
#' integer quantities, categorical variables, and identifier fields retain
#' distinct and consistent types.
#'
#' Codes not covered by the applicable TabWin conversion are retained as
#' factor levels instead of being silently discarded. Historical fields kept
#' in early CID-10 fetal-death files use their original CID-9 definitions.
#' Official numeric missing sentinels and out-of-domain measurements are
#' returned as `NA`; malformed values are included in diagnostics.
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
  sim_types <- .sim_information_systems
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

  legacy_rows <- .sim_legacy_rows(result)
  current_rows <- setdiff(seq_len(nrow(result)), legacy_rows)
  categorical_fields <- .process_find_fields(result, .sim_categorical_fields)
  legacy_fields <- .process_find_fields(
    result, .sim_legacy_categorical_fields
  )
  legacy_overlap <- toupper(.sim_legacy_categorical_fields)
  current_stable_fields <- categorical_fields[
    !toupper(categorical_fields) %in% legacy_overlap
  ]
  current_period_fields <- categorical_fields[
    toupper(categorical_fields) %in% legacy_overlap
  ]
  need_current_dictionary <- length(current_stable_fields) ||
    (length(current_period_fields) && length(current_rows))
  need_legacy_dictionary <- length(legacy_fields) && length(legacy_rows)
  if (need_current_dictionary &&
      (!identical(labels, "none") || diagnostics)) {
    # The first call downloads the archive; later calls retrieve this object
    # and all already-parsed maps from the session cache.
    dictionary <- fetch_tabwin_dictionary(information_system)
  }
  if (need_legacy_dictionary &&
      (!identical(labels, "none") || diagnostics)) {
    legacy_dictionary <- fetch_tabwin_dictionary("SIM-DO-CID9")
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
    result[[field]] <- if (
      toupper(field) %in% .sim_legacy_count_fields && length(legacy_rows)
    ) {
      .sim_as_legacy_count(
        result[[field]], legacy_rows, collector = collector, field = field
      )
    } else if (toupper(field) %in% .sim_weight_fields) {
      .sim_as_weight(
        result[[field]], collector = collector, field = field
      )
    } else {
      .process_as_integer(
        result[[field]], collector = collector, field = field
      )
    }
  }
  result <- .sim_add_age_fields(result)

  if (length(current_stable_fields) &&
      (!identical(labels, "none") || diagnostics)) {
    result <- .process_apply_dictionary(
      result,
      dictionary,
      current_stable_fields,
      labels = labels,
      collector = collector
    )
  }
  if (length(current_period_fields) && length(current_rows) &&
      (!identical(labels, "none") || diagnostics)) {
    result <- .process_apply_dictionary(
      result,
      dictionary,
      current_period_fields,
      rows = current_rows,
      labels = labels,
      collector = collector
    )
  }
  if (need_legacy_dictionary &&
      (!identical(labels, "none") || diagnostics)) {
    result <- .process_apply_dictionary(
      result,
      legacy_dictionary,
      legacy_fields,
      aliases = .sim_legacy_dictionary_aliases,
      rows = legacy_rows,
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
