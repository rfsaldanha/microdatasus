
# Convert dummy columns c(0, 1) to factor c("Sim", "Nao")
#' @keywords internal
convert_dummy_to_sim_nao <- function(data, column_names) {

  for (col in column_names) {
    if (col %in% names(data)) {
      data[, (col) := as.numeric(as.character(get(col)))]

      data[, (col) := data.table::fcase(
        get(col) >= 1, "Sim",
        get(col) <= 0, "N\u00e3o",
        is.na(get(col)), as.character(NA)
        )]

    #  data[, (col) := factor(get(col))]
    }
  }
  return(data)
}


# Convert columns to integer
#' @keywords internal
convert_to_integer <- function(data, column_names) {
  for (col in column_names) {
    if (col %in% names(data)) {
      data[, (col) := as.integer(get(col))]
    }
  }
  return(data)
  # return(invisible(gtfs))

}


# Convert columns to numeric
#' @keywords internal
convert_to_numeric <- function(data, column_names) {
  for (col in column_names) {
    if (col %in% names(data)) {
      data[, (col) := as.numeric(as.character(get(col)))]
    }
  }
  # return(data)
  return(invisible(data))

}


# Convert columns to character
#' @keywords internal
convert_to_character <- function(data, column_names) {
  for (col in column_names) {
    if (col %in% names(data)) {
      data[, (col) := as.character(get(col))]
    }
  }
  return(data)
}

