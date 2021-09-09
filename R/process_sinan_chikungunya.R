#' Process SINAN Chikungunya variables from DataSUS
#'
#' \code{process_sinan_chikungunya} processes SINAN Dengue variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SINAN Chikungunya variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples \dontrun{
#' df <- fetch_datasus(year_start = 2016, year_end = 2016, uf = "RJ", information_system = "SINAN-CHIKUNGUNYA-FINAL")
#' df_a <- process_sinan_dengue(df)
#' df_b <- process_sinan_dengue(df, municipality_data = FALSE)
#' }
#' @export

process_sinan_chikungunya <- function(data, municipality_data = TRUE){
  # Variables names
  variables_names <- names(data)

  # Declara objetos
  ano <- NULL
  unidade <- NULL










  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  data <- as.data.frame(lapply(X = data, FUN = stringi::stri_unescape_unicode))

  # Return
  return(data)
}
