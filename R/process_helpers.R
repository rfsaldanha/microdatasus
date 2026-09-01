# Shared helpers keep the system processors focused on field semantics while
# preserving column names and unknown source codes.
.process_find_fields <- function(data, fields) {
  indexes <- match(unique(toupper(fields)), toupper(names(data)), nomatch = 0L)
  names(data)[indexes[indexes > 0L]]
}

.process_as_integer <- function(x, missing = character(), collector = NULL,
                                field = NA_character_) {
  # Keep the allocation-free path for ordinary processing; diagnostics pay
  # the extra character conversion only when the user explicitly requests it.
  if (is.null(collector)) {
    if (is.integer(x) && !length(missing)) return(x)
    direct <- is.character(x) || is.factor(x) ||
      ((is.integer(x) || is.double(x)) && !is.object(x))
    if (!length(missing) && direct) {
      if (is.factor(x)) x <- as.character(x)
      return(suppressWarnings(as.integer(x)))
    }
  }
  source <- x
  if (is.factor(x)) x <- as.character(x)
  values <- trimws(as.character(x))
  values[values %in% missing] <- NA_character_
  result <- suppressWarnings(as.integer(values))
  .process_record_coercion(collector, field, "integer", source, result, missing)
  result
}

.process_as_double <- function(x, missing = character(), collector = NULL,
                               field = NA_character_) {
  if (is.null(collector)) {
    if (is.double(x) && !length(missing)) return(x)
    direct <- is.character(x) || is.factor(x) ||
      ((is.integer(x) || is.double(x)) && !is.object(x))
    if (!length(missing) && direct) {
      if (is.factor(x)) x <- as.character(x)
      return(suppressWarnings(as.numeric(x)))
    }
  }
  source <- x
  if (is.factor(x)) x <- as.character(x)
  values <- trimws(as.character(x))
  values[values %in% missing] <- NA_character_
  result <- suppressWarnings(as.numeric(values))
  .process_record_coercion(collector, field, "double", source, result, missing)
  result
}

.process_as_date <- function(x, format = "%d%m%Y", collector = NULL,
                             field = NA_character_,
                             missing = c("00000000", "000000")) {
  if (inherits(x, "Date")) return(x)
  source <- x
  values <- trimws(as.character(x))
  values[values %in% missing] <- NA_character_
  pattern <- switch(
    format,
    "%d%m%Y" = "^[0-9]{8}$",
    "%Y%m%d" = "^[0-9]{8}$",
    NULL
  )
  valid <- !is.na(values)
  if (!is.null(pattern)) valid <- valid & grepl(pattern, values)
  result <- rep(as.Date(NA), length(values))
  result[valid] <- as.Date(values[valid], format = format)
  .process_record_coercion(
    collector, field, "Date", source, result, missing
  )
  result
}

.process_as_sih_date <- function(x, reference_year, collector = NULL,
                                 field = NA_character_) {
  if (inherits(x, "Date")) return(x)
  source <- x
  missing <- c("00000000", "000000")
  values <- trimws(as.character(x))
  values[values %in% missing] <- NA_character_
  result <- rep(as.Date(NA), length(values))

  modern <- !is.na(values) & grepl("^[0-9]{8}$", values)
  result[modern] <- as.Date(values[modern], format = "%Y%m%d")

  historical <- !is.na(values) & grepl("^[0-9]{6}$", values)
  reference_year <- suppressWarnings(as.integer(as.character(reference_year)))
  resolvable <- historical & !is.na(reference_year)
  if (any(resolvable)) {
    year <- suppressWarnings(as.integer(substr(values, 1L, 2L)))
    full_year <- (reference_year %/% 100L) * 100L + year
    after_reference <- !is.na(full_year) & !is.na(reference_year) &
      full_year > reference_year
    full_year[after_reference] <- full_year[after_reference] - 100L
    expanded <- paste0(
      sprintf("%04d", full_year[resolvable]),
      substring(values[resolvable], 3L)
    )
    result[resolvable] <- as.Date(expanded, format = "%Y%m%d")
  }
  .process_record_coercion(
    collector, field, "Date", source, result, missing
  )
  result
}

.process_add_age_fields <- function(
  data,
  unit,
  value,
  units,
  century_units = character()
) {
  unit <- trimws(as.character(unit))
  value <- suppressWarnings(as.integer(as.character(value)))
  outputs <- unique(unname(units))

  # Initialize every output explicitly as integer, including empty data sets.
  for (output in outputs) {
    data[[output]] <- rep(NA_integer_, length(value))
  }

  for (unit_code in names(units)) {
    output <- unname(units[[unit_code]])
    matches <- !is.na(unit) & unit == unit_code & !is.na(value)
    adjusted <- value[matches]
    if (unit_code %in% century_units) {
      adjusted <- adjusted + 100L
    }
    data[[output]][matches] <- adjusted
  }
  data
}

.process_normalize_character <- function(x) {
  # Obfuscated identifiers emitted by the DBC reader are intentionally tagged
  # as bytes. They are lossless binary identifiers, not text to be transcoded.
  bytes <- !is.na(x) & Encoding(x) == "bytes"
  result <- x
  text <- !bytes
  result[text] <- stringi::stri_unescape_unicode(
    stringi::stri_enc_toutf8(x[text])
  )
  result
}

.process_normalize_text <- function(data) {
  # Normalize only text and factor levels; numeric and Date columns retain the
  # types assigned by each processor.
  for (name in names(data)) {
    column <- data[[name]]
    if (is.character(column)) {
      data[[name]] <- .process_normalize_character(column)
    } else if (is.factor(column)) {
      levels(column) <- .process_normalize_character(levels(column))
      data[[name]] <- droplevels(column)
    }
  }
  data
}

# Apply all row-specific dictionaries while materializing and factorizing each
# source field only once. This avoids one full-column copy per historical
# period in concatenated SIH, SIA, and CNES data sets.
.process_apply_dictionaries <- function(
  data,
  dictionaries,
  fields,
  dictionary_rows = NULL,
  aliases = character(),
  labels = "factor",
  collector = NULL
) {
  if (!length(dictionaries)) {
    return(data)
  }
  keys <- names(dictionaries)
  if (is.null(keys) || any(!nzchar(keys))) {
    keys <- as.character(seq_along(dictionaries))
    names(dictionaries) <- keys
  }
  for (dictionary in dictionaries) {
    .process_record_dictionary(collector, dictionary)
  }
  if (is.null(dictionary_rows)) {
    dictionary_rows <- stats::setNames(
      rep(list(seq_len(nrow(data))), length(dictionaries)),
      keys
    )
  } else if (is.null(names(dictionary_rows))) {
    names(dictionary_rows) <- keys
  }

  for (field in .process_find_fields(data, fields)) {
    dictionary_field <- toupper(field)
    if (dictionary_field %in% names(aliases)) {
      dictionary_field <- unname(aliases[[dictionary_field]])
    }
    values <- as.character(data[[field]])
    converted <- FALSE
    conversion_levels <- character()

    for (key in keys) {
      rows <- dictionary_rows[[key]]
      if (is.null(rows) || !length(rows)) {
        next
      }
      selected <- .tabwin_select_conversion(
        dictionaries[[key]],
        dictionary_field,
        values[rows],
        data[rows, , drop = FALSE],
        field
      )
      if (!is.null(selected)) {
        source_values <- selected$source_values
        if (is.null(source_values)) source_values <- values[rows]
        converted_values <- .tabwin_apply_conversion_values(
          source_values,
          selected
        )
        .process_record_dictionary_diagnostics(
          collector,
          field,
          key,
          source_values,
          selected
        )
        if (!identical(labels, "none")) {
          values[rows] <- converted_values
          selected_levels <- selected$conversion$levels
          if (is.null(selected_levels)) {
            selected_levels <- unique(unname(selected$conversion$map))
          }
          conversion_levels <- unique(c(
            conversion_levels, selected_levels
          ))
        }
        converted <- TRUE
      }
    }
    if (!converted) {
      .process_record_unmapped_field(collector, field)
    }
    if (converted && !identical(labels, "none")) {
      data[[field]] <- if (identical(labels, "factor")) {
        .tabwin_factor(values, conversion_levels)
      } else {
        values
      }
    }
  }
  data
}

# Retain the single-dictionary helper for processors whose fields use only one
# layout and for internal callers outside the historical batching path.
.process_apply_dictionary <- function(
  data,
  dictionary,
  fields,
  aliases = character(),
  rows = NULL,
  labels = "factor",
  collector = NULL
) {
  dictionaries <- list(single = dictionary)
  dictionary_rows <- if (is.null(rows)) {
    NULL
  } else {
    list(single = rows)
  }
  .process_apply_dictionaries(
    data,
    dictionaries,
    fields,
    dictionary_rows,
    aliases,
    labels,
    collector
  )
}

.process_normalize_code_fields <- function(data, fields, width = 6L) {
  for (field in .process_find_fields(data, fields)) {
    data[[field]] <- substring(as.character(data[[field]]), 1L, width)
  }
  data
}

.process_add_municipality_data <- function(data, field, collector = NULL) {
  actual <- .process_find_fields(data, field)
  if (!length(actual)) {
    return(data)
  }
  actual <- actual[[1L]]
  municipality <- get("tabMun", envir = asNamespace("microdatasus"))
  # The packaged spatial snapshot is retained for backward compatibility.
  # Normalize it at use time and record it in diagnostics so it is never a
  # silent substitute for a current official dictionary.
  municipality <- .process_normalize_text(municipality)
  .process_record_reference(collector, "tabMun")
  names(municipality)[[1L]] <- actual
  municipality[[actual]] <- as.character(municipality[[actual]])
  dplyr::left_join(data, municipality, by = actual)
}
