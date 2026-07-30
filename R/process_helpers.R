# Shared helpers keep the system processors focused on field semantics while
# preserving column names and unknown source codes.
.process_find_fields <- function(data, fields) {
  indexes <- match(unique(toupper(fields)), toupper(names(data)), nomatch = 0L)
  names(data)[indexes[indexes > 0L]]
}

.process_as_integer <- function(x, missing = character()) {
  if (is.integer(x) && !length(missing)) {
    return(x)
  }
  # Raw DBF columns are character, factor, or plain numeric vectors. Coerce
  # those directly, but retain the legacy character path for other classes.
  direct <- is.character(x) || is.factor(x) ||
    ((is.integer(x) || is.double(x)) && !is.object(x))
  if (!length(missing) && direct) {
    if (is.factor(x)) {
      x <- as.character(x)
    }
    return(suppressWarnings(as.integer(x)))
  }
  values <- trimws(as.character(x))
  values[values %in% missing] <- NA_character_
  suppressWarnings(as.integer(values))
}

.process_as_double <- function(x, missing = character()) {
  if (is.double(x) && !length(missing)) {
    return(x)
  }
  # Raw DBF columns are character, factor, or plain numeric vectors. Coerce
  # those directly, but retain the legacy character path for other classes.
  direct <- is.character(x) || is.factor(x) ||
    ((is.integer(x) || is.double(x)) && !is.object(x))
  if (!length(missing) && direct) {
    if (is.factor(x)) {
      x <- as.character(x)
    }
    return(suppressWarnings(as.numeric(x)))
  }
  values <- trimws(as.character(x))
  values[values %in% missing] <- NA_character_
  suppressWarnings(as.numeric(values))
}

.process_as_date <- function(x, format = "%d%m%Y") {
  if (inherits(x, "Date")) {
    return(x)
  }
  as.Date(as.character(x), format = format)
}

# Add one integer column for every supported unit without discarding the
# original unit and value fields. More than one source code can target the same
# output; this is how code 5 stores ages of 100 years or more in SIM, SIH, and
# SIA while code 4 stores the ordinary year value.
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

.process_normalize_text <- function(data) {
  # Normalize only text and factor levels; numeric and Date columns retain the
  # types assigned by each processor.
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

    for (key in keys) {
      rows <- dictionary_rows[[key]]
      if (is.null(rows) || !length(rows)) {
        next
      }
      selected <- .tabwin_select_conversion(
        dictionaries[[key]],
        dictionary_field,
        values[rows]
      )
      if (!is.null(selected)) {
        converted_values <- .tabwin_apply_conversion_values(
          values[rows],
          selected
        )
        .process_record_dictionary_diagnostics(
          collector,
          field,
          key,
          values[rows],
          selected
        )
        if (!identical(labels, "none")) {
          values[rows] <- converted_values
        }
        converted <- TRUE
      }
    }
    if (converted && !identical(labels, "none")) {
      data[[field]] <- if (identical(labels, "factor")) {
        factor(values)
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

.process_add_municipality_data <- function(data, field) {
  actual <- .process_find_fields(data, field)
  if (!length(actual)) {
    return(data)
  }
  actual <- actual[[1L]]
  municipality <- get("tabMun", envir = asNamespace("microdatasus"))
  names(municipality)[[1L]] <- actual
  municipality[[actual]] <- as.character(municipality[[actual]])
  dplyr::left_join(data, municipality, by = actual)
}
