# dbc functions, copied from healthbR package (https://github.com/SidneyBissoli/healthbR/)

# ============================================================================
# .dbc2dbf - decompress .dbc to .dbf
# ============================================================================

#' Decompress a .dbc file to .dbf (internal)
#'
#' Calls the vendored C code (blast library) to decompress a DATASUS .dbc
#' file into a standard .dbf file.
#'
#' Original function from `healthbR` package.
#'
#' @param input_file Character. Path to the input .dbc file.
#' @param output_file Character. Path to the output .dbf file.
#'
#' @return Logical. TRUE if decompression succeeded, FALSE otherwise.
#'
#' @noRd
.dbc2dbf <- function(input_file, output_file) {
  if (!file.exists(input_file)) {
    cli::cli_abort("File not found: {.file {input_file}}")
  }

  result <- .C(
    "microdatasus_dbc2dbf",
    input = as.character(normalizePath(input_file, mustWork = TRUE)),
    output = as.character(path.expand(output_file)),
    ret_code = as.integer(0L),
    error_str = as.character("")
  )

  if (result$ret_code != 0L) {
    cli::cli_warn(c(
      "DBC decompression failed.",
      "x" = "Error: {result$error_str}",
      "i" = "File: {.file {input_file}}"
    ))
    return(FALSE)
  }

  file.exists(output_file) && file.size(output_file) > 0
}


# ============================================================================
# .read_dbc - read a .dbc file into a tibble
# ============================================================================

#' Read a .dbc file into a tibble (internal)
#'
#' Original function from `healthbR` package.
#'
#' Decompresses a DATASUS .dbc file to a temporary .dbf, reads it with
#' foreign::read.dbf(), and converts all columns to character for safety.
#'
#' @param file character. Path to the .dbc file.
#' @param as_character logical. If `TRUE`, convert all variables to character.
#'
#' @return A tibble with all columns as character.
#'
#' @export
read_dbc <- function(file, as_character = TRUE) {
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.file {file}}")
  }

  # create temporary .dbf file
  temp_dbf <- tempfile(fileext = ".dbf")
  on.exit(if (file.exists(temp_dbf)) file.remove(temp_dbf), add = TRUE)

  # decompress .dbc to .dbf
  success <- .dbc2dbf(file, temp_dbf)

  if (!success) {
    cli::cli_abort(c(
      "Failed to decompress .dbc file.",
      "x" = "File: {.file {file}}",
      "i" = "The file may be corrupted or in an unexpected format."
    ))
  }

  # read .dbf with foreign::read.dbf (returns data.frame)
  df <- foreign::read.dbf(temp_dbf, as.is = TRUE)

  # convert all columns to character (preserves leading zeros, etc.)
  df[] <- lapply(df, as.character)

  # return as tibble
  tibble::as_tibble(df)
}
