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

.sim_find_fields <- function(data, fields) {
  # DBC field names are normally uppercase, but early files contain lowercase
  # names such as "contador". Match without renaming user-visible columns.
  indexes <- match(unique(toupper(fields)), toupper(names(data)), nomatch = 0L)
  names(data)[indexes[indexes > 0L]]
}

.sim_as_integer <- function(x) {
  if (is.integer(x)) {
    return(x)
  }
  suppressWarnings(as.integer(as.character(x)))
}

.sim_as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  as.Date(as.character(x), format = "%d%m%Y")
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
  data$IDADEminutos <- ifelse(unit == "0", value, NA_integer_)
  data$IDADEhoras <- ifelse(unit == "1", value, NA_integer_)
  data$IDADEdias <- ifelse(unit == "2", value, NA_integer_)
  data$IDADEmeses <- ifelse(unit == "3", value, NA_integer_)
  data$IDADEanos <- ifelse(
    unit == "4",
    value,
    ifelse(unit == "5", value + 100L, NA_integer_)
  )
  data
}

.sim_add_municipality_data <- function(data) {
  if (!"CODMUNRES" %in% names(data)) {
    return(data)
  }
  # Work on a local copy to avoid mutating the package data object.
  municipality <- get("tabMun", envir = asNamespace("microdatasus"))
  names(municipality)[[1L]] <- "CODMUNRES"
  municipality$CODMUNRES <- as.character(municipality$CODMUNRES)
  dplyr::left_join(data, municipality, by = "CODMUNRES")
}

.sim_normalize_text <- function(data) {
  # Normalize only textual data. Dates and numeric columns must keep the types
  # assigned by the processing steps below.
  for (name in names(data)) {
    column <- data[[name]]
    if (is.character(column)) {
      data[[name]] <- stringi::stri_unescape_unicode(
        stringi::stri_enc_toutf8(column)
      )
    } else if (is.factor(column)) {
      levels(column) <- stringi::stri_unescape_unicode(
        stringi::stri_enc_toutf8(levels(column))
      )
      data[[name]] <- droplevels(column)
    }
  }
  data
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
  information_system = "SIM-DO"
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  .datasus_assert_flag(municipality_data, "municipality_data")
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
  result <- .sim_normalize_text(result)

  categorical_fields <- .sim_find_fields(result, .sim_categorical_fields)
  if (length(categorical_fields)) {
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
    result[[field]] <- .sim_as_date(result[[field]])
  }
  integer_fields <- .sim_find_fields(result, .sim_integer_fields)
  for (field in integer_fields) {
    result[[field]] <- .sim_as_integer(result[[field]])
  }
  result <- .sim_add_age_fields(result)

  if (length(categorical_fields)) {
    for (field in categorical_fields) {
      dictionary_field <- toupper(field)
      if (dictionary_field %in% names(.sim_dictionary_aliases)) {
        dictionary_field <- unname(
          .sim_dictionary_aliases[[dictionary_field]]
        )
      }
      selected <- .tabwin_select_conversion(
        dictionary,
        dictionary_field,
        result[[field]]
      )
      if (!is.null(selected)) {
        result[[field]] <- .tabwin_apply_conversion(
          result[[field]],
          selected
        )
      }
    }
  }

  # Municipality codes remain identifiers. Optional territorial attributes
  # are joined only after their width has been normalized.
  for (field in intersect(c("CODMUNRES", "CODMUNOCOR"), names(result))) {
    result[[field]] <- substring(as.character(result[[field]]), 1L, 6L)
  }
  if (municipality_data) {
    result <- .sim_add_municipality_data(result)
  }

  result <- .sim_normalize_text(result)
  cli::cli_alert_success(
    "Finished {.strong {information_system}} data pre-processing."
  )
  tibble::as_tibble(result)
}
