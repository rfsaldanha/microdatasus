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

.dbc_encoding_by_ldid <- c(
  "1" = "CP437",
  "2" = "CP850",
  "3" = "CP1252",
  "4" = "MACINTOSH",
  "87" = "CP1252",
  "100" = "CP852",
  "101" = "CP866",
  "102" = "CP865",
  "103" = "CP861",
  "106" = "CP737",
  "107" = "CP857",
  "120" = "BIG5",
  "121" = "CP949",
  "122" = "GBK",
  "123" = "CP932",
  "124" = "CP874",
  "125" = "CP1255",
  "126" = "CP1256",
  "200" = "CP1250",
  "201" = "CP1251",
  "202" = "CP1254",
  "203" = "CP1253"
)

.dbc_resolve_encoding <- function(encoding, language_driver) {
  if (tolower(encoding) == "auto") {
    resolved <- unname(.dbc_encoding_by_ldid[as.character(language_driver)])
    if (length(resolved) == 0L || is.na(resolved)) {
      resolved <- "CP1252"
      if (language_driver != 0L) {
        cli::cli_warn(c(
          "Unknown DBF language-driver byte {language_driver}; assuming Windows-1252.",
          "i" = "Set {.arg encoding} explicitly if the file uses another code page."
        ))
      }
    }
  } else {
    resolved <- encoding
  }

  supported <- tryCatch(
    {
      withCallingHandlers(
        iconv("", from = resolved, to = "UTF-8"),
        warning = function(warning) stop(warning)
      )
      TRUE
    },
    error = function(error) FALSE
  )
  if (!supported) {
    cli::cli_abort(
      "Encoding {.val {resolved}} is not supported by this R installation.",
      class = c(
        "microdatasus_dbc_encoding_error",
        "microdatasus_dbc_error"
      )
    )
  }

  resolved
}

.dbc_decode_text <- function(value, encoding, context, file) {
  converted <- tryCatch(
    suppressWarnings(iconv(value, from = encoding, to = "UTF-8", sub = NA)),
    error = function(error) rep(NA_character_, length(value))
  )
  invalid <- !is.na(value) & is.na(converted)
  if (any(invalid)) {
    cli::cli_abort(
      c(
        "Failed to decode {context} as {.val {encoding}}.",
        "i" = "DBC file: {.file {file}}",
        "i" = "Set {.arg encoding} to the file's actual code page."
      ),
      class = c(
        "microdatasus_dbc_encoding_error",
        "microdatasus_dbc_error"
      )
    )
  }
  converted
}

.dbc_abort_native_error <- function(error, file) {
  cli::cli_abort(
    c(
      "Failed to read the DBC file.",
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

#' Read a DBC file
#'
#' Reads a DataSUS DBC file directly into a tibble, without creating an
#' intermediate DBF file. Use this function for a DBC file already available
#' locally; use [fetch_datasus()] to discover and download files from DataSUS.
#'
#' @param file A single character string with the path to a readable, non-empty
#'   DBC file.
#' @param as_character If `TRUE` (the default), converts every column to
#'   character. If `FALSE`, preserves the types inferred from the DBF metadata.
#' @param vars `NULL` (the default), or a character vector containing the
#'   columns to read. Unselected columns are neither allocated nor parsed.
#' @param encoding Character scalar naming the source encoding, or `"auto"`
#'   (the default) to use the DBF language-driver byte. Unmarked DataSUS files
#'   default to Windows-1252.
#'
#' @return A tibble with one column per DBF field. By default, all columns are
#'   character vectors; with `as_character = FALSE`, DBF-inferred types are
#'   retained.
#'
#' @details
#' Decompression is performed through the package's bundled DBC implementation.
#' The DBF header and decompressed fixed-width records are parsed directly into
#' R columns without creating an intermediate DBF file. The decompressor was
#' adapted from the `healthbR` package.
#'
#' Character fields are converted explicitly to UTF-8. Invalid byte sequences
#' abort with a `microdatasus_dbc_encoding_error` instead of being silently
#' replaced. Supply `encoding` when a file has a missing or incorrect code-page
#' marker.
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
read_dbc <- function(
  file,
  as_character = TRUE,
  vars = NULL,
  encoding = "auto"
) {
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
  if (!is.null(vars) && (
    !is.character(vars) ||
      length(vars) < 1L ||
      anyNA(vars) ||
      any(!nzchar(vars))
  )) {
    cli::cli_abort(
      "{.arg vars} must be `NULL` or a vector of non-empty names."
    )
  }
  if (!is.null(vars) && anyDuplicated(vars)) {
    cli::cli_abort("{.arg vars} must not contain duplicated names.")
  }
  if (!is.character(encoding) ||
      length(encoding) != 1L ||
      is.na(encoding) ||
      !nzchar(encoding)) {
    cli::cli_abort(
      "{.arg encoding} must be {.val auto} or one non-empty encoding name."
    )
  }
  .dbc_assert_regular_file(file, "file")
  normalized <- as.character(normalizePath(file, mustWork = TRUE))

  info <- tryCatch(
    .Call(microdatasus_dbc_info, normalized),
    error = function(error) .dbc_abort_native_error(error, file)
  )
  source_encoding <- .dbc_resolve_encoding(
    encoding,
    info$language_driver
  )
  original_names <- .dbc_decode_text(
    info$names,
    source_encoding,
    "DBF field names",
    file
  )
  repaired_names <- make.names(original_names, unique = TRUE)

  if (!is.null(vars) && !all(vars %in% repaired_names)) {
    unknown <- setdiff(vars, repaired_names)
    cli::cli_abort(
      "Unknown variable name{?s}: {.field {unknown}}.",
      class = "microdatasus_unknown_vars"
    )
  }
  selection <- if (is.null(vars)) NULL else match(vars, repaired_names)
  selected_indices <- if (is.null(selection)) {
    seq_along(repaired_names)
  } else {
    selection
  }

  # Stream the compressed DBF record area directly into selected R columns.
  df <- tryCatch(
    .Call(
      microdatasus_read_dbc,
      normalized,
      selection
    ),
    error = function(error) .dbc_abort_native_error(error, file)
  )

  actual_names <- .dbc_decode_text(
    names(df),
    source_encoding,
    "DBF field names",
    file
  )
  expected_names <- original_names[selected_indices]
  expected_types <- info$data_types[selected_indices]
  actual_types <- attr(df, "data_types", exact = TRUE)
  if (!identical(actual_names, expected_names) ||
      !identical(actual_types, expected_types)) {
    .dbc_abort_native_error(
      simpleError("The DBC header changed while the file was being read."),
      file
    )
  }

  names(df) <- repaired_names[selected_indices]
  changed <- original_names[selected_indices] != repaired_names[selected_indices]
  if (any(changed)) {
    for (index in selected_indices[changed]) {
      message(
        sprintf(
          "Field name: %s changed to: %s",
          sQuote(original_names[[index]]),
          sQuote(repaired_names[[index]])
        )
      )
    }
  }

  character_columns <- which(vapply(df, is.character, logical(1)))
  for (index in character_columns) {
    df[[index]] <- .dbc_decode_text(
      df[[index]],
      source_encoding,
      sprintf("DBF field %s", sQuote(names(df)[[index]])),
      file
    )
  }

  data_types <- attr(df, "data_types", exact = TRUE)
  for (index in which(data_types == "D")) {
    df[[index]] <- as.Date(df[[index]], format = "%Y%m%d")
  }

  if (as_character) {
    df[] <- lapply(df, as.character)
  }

  result <- tibble::as_tibble(df)
  attr(result, "dbc_encoding") <- source_encoding
  attr(result, "dbf_language_driver") <- info$language_driver
  result
}
