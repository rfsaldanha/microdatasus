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

  normalized_input <- as.character(normalizePath(input_file, mustWork = TRUE))
  expanded_output <- path.expand(output_file)
  normalized_output <- if (file.exists(expanded_output)) {
    as.character(normalizePath(expanded_output, mustWork = TRUE))
  } else if (dir.exists(dirname(expanded_output))) {
    file.path(
      as.character(normalizePath(dirname(expanded_output), mustWork = TRUE)),
      basename(expanded_output)
    )
  } else {
    expanded_output
  }
  if (identical(normalized_input, normalized_output)) {
    cli::cli_abort(
      "{.arg input_file} and {.arg output_file} must refer to different files.",
      class = c(
        "microdatasus_dbc_decompression_error",
        "microdatasus_dbc_error"
      )
    )
  }

  result <- .C(
    microdatasus_dbc2dbf,
    input = normalized_input,
    output = as.character(expanded_output),
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

# General LDID mappings follow GDAL's DBF reader. LDID 53 is the Borland
# Latin-1 driver found in official DataSUS SIM files and uses Windows-1252.
.dbc_encoding_by_ldid <- c(
  "1" = "CP437",
  "2" = "CP850",
  "3" = "CP1252",
  "4" = "MACINTOSH",
  "8" = "CP865",
  "10" = "CP850",
  "11" = "CP437",
  "13" = "CP437",
  "14" = "CP850",
  "15" = "CP437",
  "16" = "CP850",
  "17" = "CP437",
  "18" = "CP850",
  "19" = "CP932",
  "20" = "CP850",
  "21" = "CP437",
  "22" = "CP850",
  "23" = "CP865",
  "24" = "CP437",
  "25" = "CP437",
  "26" = "CP850",
  "27" = "CP437",
  "28" = "CP863",
  "29" = "CP850",
  "31" = "CP852",
  "34" = "CP852",
  "35" = "CP852",
  "36" = "CP860",
  "37" = "CP850",
  "38" = "CP866",
  "53" = "CP1252",
  "55" = "CP850",
  "64" = "CP852",
  "77" = "CP936",
  "78" = "CP949",
  "79" = "CP950",
  "80" = "CP874",
  "87" = "ISO-8859-1",
  "88" = "CP1252",
  "89" = "CP1252",
  "100" = "CP852",
  "101" = "CP866",
  "102" = "CP865",
  "103" = "CP861",
  "104" = "CP895",
  "105" = "CP620",
  "106" = "CP737",
  "107" = "CP857",
  "108" = "CP863",
  "120" = "CP950",
  "121" = "CP949",
  "122" = "CP936",
  "123" = "CP932",
  "124" = "CP874",
  "125" = "CP1255",
  "126" = "CP1256",
  "134" = "CP737",
  "135" = "CP852",
  "136" = "CP857",
  "150" = "CP10007",
  "151" = "MAC-CENTRALEUROPE",
  "200" = "CP1250",
  "201" = "CP1251",
  "202" = "CP1254",
  "203" = "CP1253",
  "204" = "CP1257"
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

.dbc_text_penalty <- function(value) {
  value <- value[!is.na(value)]
  if (!length(value)) return(0)
  codepoints <- unlist(lapply(value, utf8ToInt), use.names = FALSE)
  if (!length(codepoints)) return(0)

  portuguese <- utf8ToInt(paste0(
    "\u00c0\u00c1\u00c2\u00c3\u00c9\u00ca\u00cd\u00d3\u00d4\u00d5",
    "\u00da\u00dc\u00c7\u00e0\u00e1\u00e2\u00e3\u00e9\u00ea\u00ed\u00f3\u00f4\u00f5\u00fa\u00fc\u00e7\u00d1\u00f1\u00aa\u00ba"
  ))
  control <- codepoints >= 128L & codepoints <= 159L
  non_latin_script <-
    (codepoints >= 880L & codepoints <= 1023L) |
    (codepoints >= 9472L & codepoints <= 9727L)
  latin_supplement <- codepoints >= 160L & codepoints <= 255L
  uncommon_latin <- latin_supplement & !codepoints %in% portuguese
  other_non_ascii <- codepoints > 255L & !non_latin_script

  sum(
    20 * control +
      10 * non_latin_script +
      uncommon_latin +
      3 * other_non_ascii
  ) / length(codepoints)
}

.dbc_candidate_score <- function(value, encoding) {
  converted <- suppressWarnings(iconv(
    value,
    from = encoding,
    to = "UTF-8",
    sub = NA
  ))
  invalid <- is.na(converted) & !is.na(value)
  valid <- converted[!invalid]
  if (!length(valid)) return(Inf)
  .dbc_text_penalty(valid) + 0.25 * mean(invalid)
}

.dbc_obfuscated_numeric_bytes <- function(value) {
  value <- value[!is.na(value) & nzchar(value)]
  if (!length(value)) return(FALSE)
  value <- utils::head(value, 5000L)
  bytes <- lapply(value, charToRaw)
  all_digits <- vapply(bytes, function(x) {
    length(x) > 0L && all(as.integer(x) >= 123L & as.integer(x) <= 132L)
  }, logical(1))
  any_high <- any(vapply(bytes, function(x) {
    any(as.integer(x) >= 128L)
  }, logical(1)))
  widths <- vapply(bytes, length, integer(1))
  stats::median(widths) >= 8 && mean(all_digits) >= 0.95 && any_high
}

.dbc_decode_text_auto <- function(
  value,
  encoding,
  language_driver,
  context,
  file
) {
  present <- !is.na(value)
  if (!any(present)) {
    attr(value, "dbc_encoding_used") <- encoding
    return(value)
  }

  non_ascii <- present & grepl("[^ -~]", value, useBytes = TRUE)
  result <- value
  used <- character()

  if (any(non_ascii) && .dbc_obfuscated_numeric_bytes(value[present])) {
    # DataSUS anonymizes some CPF/CNS fields with bytes 0x7b--0x84. They are
    # identifiers, not encoded prose, so keep the original bytes losslessly.
    attr(result, "dbc_encoding_used") <- "bytes"
    cli::cli_warn(
      c(
        "{context} contains DataSUS obfuscated identifier bytes; values were preserved losslessly.",
        "i" = "The affected strings have encoding {.val bytes}, not text encoding."
      ),
      class = "microdatasus_dbc_encoding_warning"
    )
    return(result)
  }

  utf8_valid <- rep(FALSE, length(value))
  utf8_valid[present] <- stringi::stri_enc_isutf8(value[present])
  utf8_non_ascii <- non_ascii & utf8_valid
  use_utf8 <- any(utf8_non_ascii) &&
    sum(utf8_non_ascii) / sum(non_ascii) >= 0.8
  if (use_utf8) {
    result[utf8_non_ascii] <- iconv(
      value[utf8_non_ascii],
      from = "UTF-8",
      to = "UTF-8"
    )
    used <- c(used, "UTF-8")
  }

  remaining <- present & !(use_utf8 & utf8_non_ascii)
  legacy_encoding <- encoding
  legacy_non_ascii <- remaining & non_ascii
  if (language_driver == 0L && any(legacy_non_ascii)) {
    sample <- utils::head(value[legacy_non_ascii], 5000L)
    candidates <- unique(c(encoding, "CP850"))
    scores <- vapply(
      candidates,
      function(candidate) .dbc_candidate_score(sample, candidate),
      numeric(1)
    )
    best <- which.min(scores)
    source_score <- scores[[match(encoding, candidates)]]
    # Change an unmarked file away from the conservative CP1252 default only
    # when the evidence is strong. Ambiguous/mixed fields retain source bytes
    # for values that CP1252 cannot decode instead of being mis-transcoded.
    if (scores[[best]] + 0.05 < source_score) {
      legacy_encoding <- candidates[[best]]
    }
  }

  if (any(remaining)) {
    converted <- suppressWarnings(iconv(
      value[remaining],
      from = legacy_encoding,
      to = "UTF-8",
      sub = NA
    ))
    valid <- !is.na(converted)
    remaining_indices <- which(remaining)
    result[remaining_indices[valid]] <- converted[valid]
    if (any(legacy_non_ascii)) {
      used <- c(used, legacy_encoding)
    } else if (!length(used)) {
      used <- encoding
    }

    invalid_indices <- remaining_indices[!valid]
    if (length(invalid_indices)) {
      # Keep the native CE_BYTES strings. This is the only lossless
      # representation when a field mixes text and non-text bytes.
      used <- c(used, "bytes")
      cli::cli_warn(
        c(
          "{length(invalid_indices)} value{?s} in {context} could not be decoded safely.",
          "i" = "Original byte content was preserved losslessly.",
          "i" = "Affected strings have encoding {.val bytes}; set {.arg encoding} explicitly to require strict decoding.",
          "i" = "DBC file: {.file {file}}"
        ),
        class = "microdatasus_dbc_encoding_warning"
      )
    }
  }

  used <- unique(used)
  attr(result, "dbc_encoding_used") <- if (length(used) == 1L) {
    used
  } else {
    paste0("mixed:", paste(used, collapse = "+"))
  }
  result
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
#'   (the default) to use the DBF language-driver byte together with byte-level
#'   evidence from each character field. Unmarked DataSUS files start with
#'   Windows-1252 and switch to UTF-8 or CP850 only when the data support it.
#'
#' @return A tibble with one column per DBF field. By default, all columns are
#'   character vectors; with `as_character = FALSE`, DBF-inferred types are
#'   retained. The `dbc_encoding` and `dbf_language_driver` attributes record
#'   the header-derived source encoding and the original header byte;
#'   `dbc_column_encodings` records the encoding used for each column. The
#'   `dbf_field_types`, `dbf_field_widths`, and `dbf_field_decimals`
#'   attributes retain the complete physical DBF layout so DEF conversions
#'   whose fixed-width code crosses adjacent fields can be reproduced.
#'
#' @details
#' Decompression is performed through the package's bundled DBC implementation.
#' The DBF header and decompressed fixed-width records are parsed directly into
#' R columns without creating an intermediate DBF file. The decompressor was
#' adapted from the `healthbR` package. The DBC CRC32 is verified against the
#' complete decompressed DBF contents before a result is returned.
#'
#' In automatic mode, character fields are converted to UTF-8 when their byte
#' encoding can be identified safely. Undecodable mixed data and the obfuscated
#' byte representation used by some CPF/CNS fields are preserved losslessly as
#' strings marked with encoding `"bytes"`, with a warning. An explicit
#' `encoding` requests strict decoding and invalid byte sequences then abort
#' with a `microdatasus_dbc_encoding_error`.
#'
#' Header layout, field widths, record markers, compressed-stream termination,
#' numeric syntax, finite numeric values, calendar dates, and the complete DBF
#' CRC32 are validated. Structural corruption and internal contradictions abort
#' instead of returning a partial table. Malformed numeric or date values found
#' in otherwise valid official files are converted to `NA` with a warning that
#' reports their count and first location.
#'
#' The native parser accepts the fixed-width DBF field types used by DataSUS:
#' character (`C`), date (`D`), floating point (`F`), logical (`L`), and
#' numeric (`N`). As in [foreign::read.dbf()], records carrying the DBF deleted
#' marker are retained; no DataSUS row is discarded implicitly.
#'
#' Invalid input files, decompression failures, and record parsing failures abort
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
  automatic_encoding <- identical(tolower(encoding), "auto")
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
      !identical(actual_types, expected_types) ||
      !identical(attr(df, "dbf_header", exact = TRUE), info$header)) {
    .dbc_abort_native_error(
      simpleError("The DBC header changed while the file was being read."),
      file
    )
  }
  attr(df, "dbf_header") <- NULL

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
  column_encodings <- rep(NA_character_, length(df))
  for (index in character_columns) {
    context <- sprintf("DBF field %s", sQuote(names(df)[[index]]))
    decoded <- if (automatic_encoding) {
      .dbc_decode_text_auto(
        df[[index]],
        source_encoding,
        info$language_driver,
        context,
        file
      )
    } else {
      .dbc_decode_text(df[[index]], source_encoding, context, file)
    }
    column_encodings[[index]] <- if (automatic_encoding) {
      attr(decoded, "dbc_encoding_used", exact = TRUE)
    } else {
      source_encoding
    }
    attr(decoded, "dbc_encoding_used") <- NULL
    df[[index]] <- decoded
  }

  data_types <- attr(df, "data_types", exact = TRUE)
  date_problems <- list()
  for (index in which(data_types == "D")) {
    raw_date <- df[[index]]
    parsed_date <- as.Date(raw_date, format = "%Y%m%d")
    valid_parse <- !is.na(parsed_date)
    roundtrip <- rep(NA_character_, length(parsed_date))
    roundtrip[valid_parse] <- format(parsed_date[valid_parse], "%Y%m%d")
    invalid <- which(
      !is.na(raw_date) & (!valid_parse | roundtrip != raw_date)
    )
    if (length(invalid) > 0L) {
      date_problems[[length(date_problems) + 1L]] <- data.frame(
        field = names(df)[[index]],
        count = length(invalid),
        first_record = invalid[[1L]],
        first_value = raw_date[[invalid[[1L]]]],
        stringsAsFactors = FALSE
      )
    }
    df[[index]] <- parsed_date
  }
  if (length(date_problems)) {
    problems <- data.table::rbindlist(date_problems)
    first_field <- problems$field[[1L]]
    first_record <- problems$first_record[[1L]]
    first_value <- problems$first_value[[1L]]
    cli::cli_warn(c(
      "{sum(problems$count)} invalid DBF date value{?s} converted to `NA`.",
      "i" = "First: field {.field {first_field}}, record {first_record}, value {.val {first_value}}."
    ))
  }

  if (as_character) {
    df[] <- lapply(df, as.character)
  }

  result <- tibble::as_tibble(df)
  attr(result, "dbc_encoding") <- source_encoding
  attr(result, "dbf_language_driver") <- info$language_driver
  attr(result, "dbc_column_encodings") <- stats::setNames(
    column_encodings,
    names(result)
  )
  descriptor_offsets <- 32L + (seq_along(repaired_names) - 1L) * 32L
  field_widths <- vapply(
    descriptor_offsets + 17L,
    function(index) as.integer(info$header[[index]]),
    integer(1)
  )
  field_decimals <- vapply(
    descriptor_offsets + 18L,
    function(index) as.integer(info$header[[index]]),
    integer(1)
  )
  attr(result, "dbf_field_types") <- stats::setNames(
    info$data_types,
    repaired_names
  )
  attr(result, "dbf_field_widths") <- stats::setNames(
    field_widths,
    repaired_names
  )
  attr(result, "dbf_field_decimals") <- stats::setNames(
    field_decimals,
    repaired_names
  )
  result
}
