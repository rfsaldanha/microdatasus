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
  values <- trimws(as.character(x))
  values[values %in% missing] <- NA_character_
  suppressWarnings(as.integer(values))
}

.process_as_double <- function(x, missing = character()) {
  if (is.double(x) && !length(missing)) {
    return(x)
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
    adjusted <- value
    if (unit_code %in% century_units) {
      adjusted <- adjusted + 100L
    }
    data[[output]][matches] <- adjusted[matches]
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

.process_apply_dictionary <- function(
  data,
  dictionary,
  fields,
  aliases = character(),
  rows = NULL
) {
  if (is.null(rows)) {
    rows <- seq_len(nrow(data))
  }
  if (!length(rows)) {
    return(data)
  }
  for (field in .process_find_fields(data, fields)) {
    dictionary_field <- toupper(field)
    if (dictionary_field %in% names(aliases)) {
      dictionary_field <- unname(aliases[[dictionary_field]])
    }
    selected <- .tabwin_select_conversion(
      dictionary,
      dictionary_field,
      data[[field]][rows]
    )
    if (!is.null(selected)) {
      values <- as.character(data[[field]])
      values[rows] <- as.character(.tabwin_apply_conversion(
        data[[field]][rows],
        selected
      ))
      data[[field]] <- factor(values)
    }
  }
  data
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
