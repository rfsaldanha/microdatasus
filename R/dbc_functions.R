# DBC functions adapted from the healthbR package:
# https://github.com/SidneyBissoli/healthbR/

# ============================================================================
# .dbc2dbf - decompress .dbc to .dbf
# ============================================================================

.dbc_file_size <- function(file) {
  unname(file.info(file)$size)
}

.dbc_assert_regular_file <- function(file, argument) {
  if (!file.exists(file)) {
    cli::cli_abort(
      "File not found: {.file {file}}",
      class = c(
        "microdatasus_dbc_file_error",
        "microdatasus_dbc_error"
      )
    )
  }
  if (!isTRUE(utils::file_test("-f", file))) {
    cli::cli_abort(
      "{.arg {argument}} must refer to a regular file: {.file {file}}",
      class = c(
        "microdatasus_dbc_file_error",
        "microdatasus_dbc_error"
      )
    )
  }

  size <- .dbc_file_size(file)
  if (length(size) != 1L || is.na(size)) {
    cli::cli_abort(
      "Could not determine the size of DBC file {.file {file}}.",
      class = c(
        "microdatasus_dbc_file_error",
        "microdatasus_dbc_error"
      )
    )
  }
  if (size <= 0) {
    cli::cli_abort(
      "DBC file {.file {file}} is empty.",
      class = c(
        "microdatasus_dbc_file_error",
        "microdatasus_dbc_error"
      )
    )
  }
  if (file.access(file, mode = 4L) != 0L) {
    cli::cli_abort(
      "DBC file {.file {file}} is not readable.",
      class = c(
        "microdatasus_dbc_file_error",
        "microdatasus_dbc_error"
      )
    )
  }

  invisible(file)
}

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
  if (!is.character(input_file) ||
      length(input_file) != 1L ||
      is.na(input_file) ||
      !nzchar(input_file)) {
    cli::cli_abort("{.arg input_file} must be a single, non-empty file path.")
  }
  if (!is.character(output_file) ||
      length(output_file) != 1L ||
      is.na(output_file) ||
      !nzchar(output_file)) {
    cli::cli_abort("{.arg output_file} must be a single, non-empty file path.")
  }
  .dbc_assert_regular_file(input_file, "input_file")

  result <- .C(
    microdatasus_dbc2dbf,
    input = as.character(normalizePath(input_file, mustWork = TRUE)),
    output = as.character(path.expand(output_file)),
    ret_code = as.integer(0L),
    error_str = as.character("")
  )

  if (result$ret_code != 0L) {
    cli::cli_abort(
      c(
        "Failed to decompress the DBC file.",
        "x" = "Error: {result$error_str}",
        "i" = "File: {.file {input_file}}"
      ),
      class = c(
        "microdatasus_dbc_decompression_error",
        "microdatasus_dbc_error"
      )
    )
  }

  output_exists <- file.exists(output_file)
  output_size <- if (output_exists) .dbc_file_size(output_file) else NA_real_
  if (!output_exists || is.na(output_size) || output_size <= 0) {
    cli::cli_abort(
      "DBC decompression produced no valid output.",
      class = c(
        "microdatasus_dbc_decompression_error",
        "microdatasus_dbc_error"
      )
    )
  }

  invisible(TRUE)
}


# ============================================================================
# read_dbc - read a .dbc file into a tibble
# ============================================================================

#' Read a DBC file
#'
#' Decompresses a DataSUS DBC file to a temporary DBF file and reads it into
#' a tibble. Use this function for a DBC file already available locally; use
#' [fetch_datasus()] to discover and download files from DataSUS.
#'
#' @param file A single character string with the path to a readable, non-empty
#'   DBC file.
#' @param as_character If `TRUE` (the default), converts every column to
#'   character. If `FALSE`, preserves the types inferred from the DBF metadata.
#'
#' @return A tibble with one column per DBF field. By default, all columns are
#'   character vectors; with `as_character = FALSE`, DBF-inferred types are
#'   retained.
#'
#' @details
#' Decompression is performed through the package's bundled DBC implementation.
#' The intermediate DBF file is created in the R temporary directory and removed
#' before the function returns or aborts. The implementation was adapted from
#' the `healthbR` package.
#'
#' Invalid input files, decompression failures, and DBF reading failures abort
#' with errors in the `microdatasus_dbc_error` family.
#'
#' @references
#' Saldanha, R. F. (2026). [*Sistemas de Informação em Saúde no
#' Brasil*](https://rfsaldanha.github.io/sis/).
#'
#' @seealso [fetch_datasus()]
#'
#' @export
read_dbc <- function(file, as_character = TRUE) {
  if (!is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(file)) {
    cli::cli_abort("{.arg file} must be a single, non-empty file path.")
  }
  if (!is.logical(as_character) ||
      length(as_character) != 1L ||
      is.na(as_character)) {
    cli::cli_abort("{.arg as_character} must be `TRUE` or `FALSE`.")
  }
  .dbc_assert_regular_file(file, "file")

  # Create a temporary DBF file.
  temp_dbf <- tempfile(fileext = ".dbf")
  on.exit(unlink(temp_dbf), add = TRUE)

  # Decompress the DBC file and read the resulting DBF.
  .dbc2dbf(file, temp_dbf)
  df <- tryCatch(
    foreign::read.dbf(temp_dbf, as.is = TRUE),
    error = function(error) {
      cli::cli_abort(
        c(
          "Failed to read the decompressed DBF file.",
          "i" = "DBC file: {.file {file}}",
          "x" = "Reason: {conditionMessage(error)}"
        ),
        class = c(
          "microdatasus_dbc_read_error",
          "microdatasus_dbc_error"
        ),
        parent = error
      )
    }
  )

  if (as_character) {
    df[] <- lapply(df, as.character)
  }

  tibble::as_tibble(df)
}
