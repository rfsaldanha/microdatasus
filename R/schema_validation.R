# Schema contracts join raw DBC fields, official DEF declarations, and the
# types produced by the matching processor without maintaining another manual
# variable dictionary.
.datasus_contract_dictionary_keys <- function(data, information_system) {
  family <- sub("-.*$", "", information_system)
  keys <- switch(
    family,
    SIM = information_system,
    SINASC = {
      modern <- length(.process_find_fields(data, c(
        "DTNASC", "LOCNASC", "CODMUNRES", "GRAVIDEZ", "ESCMAE"
      ))) > 0L
      legacy <- length(.process_find_fields(data, c(
        "DATA_NASC", "LOCAL_OCOR", "MUNI_MAE", "TIPO_GRAV",
        "INSTR_MAE"
      ))) > 0L
      selected <- character()
      if (modern || !legacy) selected <- c(selected, "SINASC")
      if (legacy) selected <- c(selected, "SINASC-1994-1995")
      selected
    },
    SIH = names(.sih_dictionary_rows(data, information_system)),
    SIA = names(.sia_dictionary_rows(data, information_system)),
    CNES = names(.cnes_dictionary_rows(data, information_system)),
    SINAN = information_system
  )
  unique(keys)
}

.datasus_contract_type <- function(x) {
  if (inherits(x, "Date")) return("Date")
  if (is.factor(x)) return("factor")
  if (is.integer(x)) return("integer")
  if (is.double(x)) return("double")
  if (is.character(x)) return("character")
  if (is.logical(x)) return("logical")
  class(x)[[1L]]
}

.datasus_contract_process_args <- function(information_system) {
  family <- sub("-.*$", "", information_system)
  args <- list(
    municipality_data = FALSE, labels = "none", diagnostics = TRUE
  )
  if (identical(family, "SIA")) {
    args <- c(args, list(
      nome_proced = FALSE, nome_ocupacao = FALSE, nome_equipe = FALSE
    ))
  }
  if (identical(family, "CNES")) args$nomes <- FALSE
  args
}

# Select rows across every historical dictionary represented in one table.
.datasus_contract_sample_rows <- function(
  data, information_system, sample_n, by_dictionary = TRUE
) {
  rows <- seq_len(nrow(data))
  if (!length(rows)) return(rows)
  groups <- if (by_dictionary) {
    family <- sub("-.*$", "", information_system)
    switch(
      family,
      SIA = .sia_dictionary_rows(data, information_system),
      SIH = .sih_dictionary_rows(data, information_system),
      CNES = .cnes_dictionary_rows(data, information_system),
      stats::setNames(list(rows), information_system)
    )
  } else {
    list(all = rows)
  }
  sampled <- unlist(lapply(groups, function(group) {
    group <- group[!is.na(group)]
    if (length(group) <= sample_n) return(group)
    group[unique(round(seq(1, length(group), length.out = sample_n)))]
  }), use.names = FALSE)
  sort(unique(sampled))
}

.datasus_contract_diagnostic_counts <- function(report, component, fields) {
  counts <- stats::setNames(rep(0L, length(fields)), fields)
  if (is.null(report) || is.null(report[[component]]) ||
      !nrow(report[[component]])) return(counts)
  table <- report[[component]]
  totals <- tapply(table$n, table$field, sum)
  matched <- intersect(names(totals), fields)
  counts[matched] <- as.integer(totals[matched])
  counts
}

#' Validate a DBC table against its dictionary and processor
#'
#' Builds a field-level contract from a raw DataSUS table (or local DBC),
#' the official DEF selected for each represented period, and the column
#' classes produced by the corresponding `process_*()` function.
#'
#' @param data A data frame returned by [fetch_datasus()] or one local DBC
#'   path. Reading a path loads that DBC in memory.
#' @param information_system One public value listed by
#'   [datasus_information_systems()].
#' @param process Logical scalar. If `TRUE`, process one representative row
#'   to report the resulting column types without processing the full table.
#' @param period Optional scalar label stored in the returned contract, such
#'   as a year or competence. Historical DEF selection uses fields in `data`.
#' @param sample_n Positive integer giving the maximum deterministic sample
#'   size processed for each represented historical dictionary.
#' @param sample_by_dictionary Logical scalar. If `TRUE`, sample every
#'   represented current or historical definition instead of only the table
#'   as a whole.
#' @inheritParams fetch_tabwin_dictionary
#' @return A tibble with one row per field observed, declared by a selected
#'   DEF, or added by the processor. `status` distinguishes matched,
#'   observed-only, dictionary-only, and processor-added fields.
#' @export
validate_datasus_schema <- function(
  data,
  information_system,
  process = TRUE,
  period = NULL,
  timeout = 240,
  refresh = FALSE,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL),
  sample_n = 100L,
  sample_by_dictionary = TRUE
) {
  .datasus_assert_flag(process, "process")
  .datasus_assert_flag(refresh, "refresh")
  .datasus_assert_flag(sample_by_dictionary, "sample_by_dictionary")
  .datasus_assert_number(sample_n, "sample_n", integer = TRUE, lower = 1)
  if (is.character(data) && length(data) == 1L && !is.na(data)) {
    data <- read_dbc(data, as_character = FALSE)
  }
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame or one local DBC path.")
  }
  if (!is.character(information_system) || length(information_system) != 1L ||
      is.na(information_system) || !nzchar(information_system)) {
    cli::cli_abort("{.arg information_system} must be one supported data family.")
  }
  registry <- .datasus_registry()
  resolved <- .sinan_resolve_information_system(information_system)
  if (length(resolved) != 1L || !resolved %in% names(registry)) {
    cli::cli_abort("{.arg information_system} must identify one supported data family.")
  }
  information_system <- resolved
  if (!is.null(period) && (length(period) != 1L || is.na(period))) {
    cli::cli_abort("{.arg period} must be NULL or one non-missing value.")
  }
  period <- if (is.null(period)) NA_character_ else as.character(period)

  dictionary_keys <- .datasus_contract_dictionary_keys(
    data, information_system
  )
  definitions <- lapply(dictionary_keys, function(key) {
    datasus_variables(
      key, include_labels = FALSE, timeout = timeout, refresh = refresh,
      quiet = quiet, cache_dir = cache_dir
    )
  })
  declared <- unique(unlist(lapply(definitions, function(x) x$field)))
  observed <- names(data)

  processed <- NULL
  report <- NULL
  sampled_rows <- integer()
  if (process) {
    sampled_rows <- .datasus_contract_sample_rows(
      data, information_system, sample_n, sample_by_dictionary
    )
    sample <- data[sampled_rows, , drop = FALSE]
    processed <- .datasus_process_file(
      sample, information_system,
      .datasus_contract_process_args(information_system), cache_dir
    )
    report <- processing_diagnostics(processed)
  }
  processed_fields <- if (is.null(processed)) character() else names(processed)
  fields <- unique(c(observed, declared, processed_fields))

  raw_types <- stats::setNames(
    vapply(data, .datasus_contract_type, character(1)), observed
  )
  processed_types <- if (is.null(processed)) character() else stats::setNames(
    vapply(processed, .datasus_contract_type, character(1)), processed_fields
  )
  dictionary_type <- vapply(fields, function(field) {
    types <- unique(unlist(lapply(definitions, function(x) x$type[x$field == field])))
    if (!length(types)) NA_character_ else paste(sort(types), collapse = "+")
  }, character(1))
  field_keys <- lapply(fields, function(field) {
    dictionary_keys[vapply(definitions, function(x) field %in% x$field, logical(1))]
  })
  field_checksums <- lapply(seq_along(fields), function(index) {
    keys <- field_keys[[index]]
    unique(unlist(lapply(definitions[dictionary_keys %in% keys], function(x) {
      x$archive_checksum
    })))
  })
  is_observed <- fields %in% observed
  is_declared <- fields %in% declared
  is_processed <- fields %in% processed_fields
  status <- ifelse(
    is_observed & is_declared, "matched",
    ifelse(
      is_observed, "observed_only",
      ifelse(is_declared, "dictionary_only", "processor_added")
    )
  )
  coercion_counts <- .datasus_contract_diagnostic_counts(
    report, "coercion_failures", fields
  )
  unknown_counts <- .datasus_contract_diagnostic_counts(
    report, "unknown_codes", fields
  )
  tibble::tibble(
    information_system = information_system, period = period, field = fields,
    observed = is_observed, dictionary_declared = is_declared,
    processed = is_processed, raw_type = unname(raw_types[fields]),
    dictionary_type = dictionary_type,
    processed_type = unname(processed_types[fields]),
    type_changed = is_observed & is_processed &
      !is.na(raw_types[fields]) & !is.na(processed_types[fields]) &
      unname(raw_types[fields]) != unname(processed_types[fields]),
    sample_rows = length(sampled_rows),
    coercion_failures = unname(coercion_counts[fields]),
    unknown_codes = unname(unknown_counts[fields]),
    status = status, dictionary_keys = field_keys,
    archive_checksums = field_checksums
  )
}
