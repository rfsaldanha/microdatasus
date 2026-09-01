dbc_fixture <- function() {
  hex <- paste0(
    "035f071a02000000810020000000000000000000000000000000000000000000",
    "434f444500000000000000430000000004000000000000000000000000000000",
    "56414c55450000000000004e00000000130f0000000000000000000000000000",
    "5748454e00000000000000440000000008000000000000000000000000000000",
    "0d6f8f7a5e000640c080110364470c17355bc40c90f9c186b659ff6d80508934",
    "903f03fe01"
  )
  starts <- seq.int(1L, nchar(hex), by = 2L)
  bytes <- substring(hex, starts, starts + 1L)
  path <- tempfile(fileext = ".dbc")
  writeBin(as.raw(strtoi(bytes, base = 16L)), path)
  path
}

write_dbc_bytes <- function(bytes) {
  path <- tempfile(fileext = ".dbc")
  writeBin(bytes, path)
  path
}

dbc_bytes_with_compressed_body <- function(bytes = raw()) {
  header <- raw(33L)
  header[9L] <- as.raw(33L)
  c(header, raw(4L), bytes)
}

little_endian_raw <- function(value, size) {
  as.raw((value %/% (256^(seq_len(size) - 1L))) %% 256)
}

blast_literal_stream <- function(value) {
  literal_bits <- unlist(lapply(as.integer(value), function(byte) {
    c(0L, as.integer(intToBits(byte))[seq_len(8L)])
  }), use.names = FALSE)
  # Length symbol 15 plus an eight-bit value of 255 is blast's end code 519.
  bits <- c(literal_bits, 1L, rep(0L, 7L), rep(1L, 8L))
  bits <- c(bits, rep(0L, (-length(bits)) %% 8L))
  starts <- seq.int(1L, length(bits), by = 8L)
  packed <- vapply(starts, function(start) {
    sum(bits[start + 0:7] * 2^(0:7))
  }, numeric(1))
  c(as.raw(c(0L, 6L)), as.raw(packed))
}

dbc_crc32 <- function(value) {
  hex <- digest::digest(value, algo = "crc32", serialize = FALSE)
  starts <- seq.int(1L, nchar(hex), by = 2L)
  rev(as.raw(strtoi(substring(hex, starts, starts + 1L), base = 16L)))
}

literal_dbc_fixture <- function(
  fields,
  rows,
  language_driver = 0L,
  statuses = NULL,
  trailing = raw(),
  record_padding = raw(),
  version = 3L,
  header_extension = raw(),
  terminator = 13L
) {
  field_count <- length(fields)
  base_header_size <- 33L + 32L * field_count
  header_size <- base_header_size + length(header_extension)
  record_size <- 1L + sum(vapply(fields, `[[`, integer(1), "width")) +
    length(record_padding)
  header <- raw(header_size)
  header[1L] <- as.raw(version)
  header[5:8] <- little_endian_raw(length(rows), 4L)
  header[9:10] <- little_endian_raw(header_size, 2L)
  header[11:12] <- little_endian_raw(record_size, 2L)
  header[30L] <- as.raw(language_driver)

  for (index in seq_along(fields)) {
    field <- fields[[index]]
    start <- 33L + (index - 1L) * 32L
    name <- field$name
    if (is.character(name)) name <- charToRaw(name)
    stopifnot(length(name) <= 11L, field$width <= 255L)
    header[start + seq_along(name) - 1L] <- name
    header[start + 11L] <- charToRaw(field$type)
    header[start + 16L] <- as.raw(field$width)
    decimals <- if (is.null(field$decimals)) 0L else field$decimals
    header[start + 17L] <- as.raw(decimals)
  }
  header[base_header_size] <- as.raw(terminator)
  if (length(header_extension)) {
    header[base_header_size + seq_along(header_extension)] <- header_extension
  }

  if (is.null(statuses)) statuses <- rep(32L, length(rows))
  stopifnot(length(statuses) == length(rows))
  records <- unlist(lapply(seq_along(rows), function(row_index) {
    row <- rows[[row_index]]
    record <- as.raw(statuses[[row_index]])
    for (index in seq_along(fields)) {
      value <- row[[index]]
      if (is.character(value)) value <- charToRaw(value)
      width <- fields[[index]]$width
      stopifnot(length(value) <= width)
      record <- c(record, value, as.raw(rep(32L, width - length(value))))
    }
    c(record, record_padding)
  }), use.names = FALSE)

  path <- tempfile(fileext = ".dbc")
  body <- c(records, trailing)
  writeBin(c(header, dbc_crc32(c(header, body)), blast_literal_stream(body)), path)
  path
}

test_that("read_dbc public signature remains stable", {
  expect_identical(
    formals(read_dbc),
    as.pairlist(alist(
      file = ,
      as_character = TRUE,
      vars = NULL,
      encoding = "auto"
    ))
  )
})

test_that("read_dbc reads DBC data and preserves leading zeros", {
  path <- dbc_fixture()
  on.exit(unlink(path), add = TRUE)

  result <- read_dbc(path)

  expect_s3_class(result, "tbl_df")
  expect_identical(result$CODE, c("001", "010"))
  expect_identical(result$VALUE, c("1.5", NA_character_))
  expect_identical(result$WHEN, c("2020-01-02", NA_character_))
  expect_true(all(vapply(result, is.character, logical(1))))
  expect_identical(names(result), c("CODE", "VALUE", "WHEN"))
  expect_equal(nrow(result), 2L)
})

test_that("read_dbc can preserve DBF column types", {
  path <- dbc_fixture()
  on.exit(unlink(path), add = TRUE)

  result <- read_dbc(path, as_character = FALSE)

  expect_identical(result$CODE, c("001", "010"))
  expect_equal(result$VALUE, c(1.5, NA_real_))
  expect_s3_class(result$WHEN, "Date")
  expect_equal(result$WHEN, as.Date(c("2020-01-02", NA)))
})

test_that("read_dbc diagnoses impossible non-missing DBF dates", {
  path <- literal_dbc_fixture(
    fields = list(
      list(name = "WHEN", type = "D", width = 8L, decimals = 0L)
    ),
    rows = list(list("20230231"))
  )
  on.exit(unlink(path), add = TRUE)

  expect_warning(
    result <- read_dbc(path, as_character = FALSE),
    "invalid DBF date value.*record 1"
  )
  expect_s3_class(result$WHEN, "Date")
  expect_true(is.na(result$WHEN))
})

test_that("read_dbc projects columns natively in requested order", {
  path <- dbc_fixture()
  on.exit(unlink(path), add = TRUE)

  result <- read_dbc(
    path,
    as_character = FALSE,
    vars = c("WHEN", "CODE")
  )

  expect_named(result, c("WHEN", "CODE"))
  expect_s3_class(result$WHEN, "Date")
  expect_identical(result$CODE, c("001", "010"))
  expect_false("VALUE" %in% names(result))
})

test_that("read_dbc decodes CP1252 and skips invalid unselected text", {
  cp1252_text <- as.raw(c(
    83L, 227L, 111L, 32L, 74L, 111L, 115L, 233L,
    32L, 151L, 32L, 99L, 97L, 102L, 233L
  ))
  path <- literal_dbc_fixture(
    fields = list(
      list(
        name = as.raw(c(78L, 79L, 77L, 201L)),
        type = "C", width = 20L, decimals = 0L
      ),
      list(name = "INVALID", type = "C", width = 1L, decimals = 0L)
    ),
    rows = list(list(cp1252_text, as.raw(129L))),
    language_driver = 3L
  )
  on.exit(unlink(path), add = TRUE)

  result <- read_dbc(path, vars = "NOMÉ")

  expect_named(result, "NOMÉ")
  expect_identical(result[[1L]], "São José — café")
  expect_identical(attr(result, "dbc_encoding"), "CP1252")
  expect_identical(attr(result, "dbf_language_driver"), 3L)
  expect_warning(
    full <- read_dbc(path),
    "preserved losslessly",
    class = "microdatasus_dbc_encoding_warning"
  )
  expect_identical(charToRaw(full$INVALID[[1L]]), as.raw(129L))
  expect_identical(Encoding(full$INVALID[[1L]]), "bytes")
  expect_error(
    read_dbc(path, encoding = "CP1252"),
    "Failed to decode",
    class = "microdatasus_dbc_encoding_error"
  )
})

test_that("read_dbc detects CP850 and accepts an encoding override", {
  cp850 <- literal_dbc_fixture(
    fields = list(
      list(name = "TEXT", type = "C", width = 4L, decimals = 0L)
    ),
    rows = list(list(as.raw(c(67L, 97L, 102L, 130L)))),
    language_driver = 2L
  )
  unmarked <- literal_dbc_fixture(
    fields = list(
      list(name = "TEXT", type = "C", width = 4L, decimals = 0L)
    ),
    rows = list(list(as.raw(c(67L, 97L, 102L, 233L)))),
    language_driver = 0L
  )
  on.exit(unlink(c(cp850, unmarked)), add = TRUE)

  expect_identical(read_dbc(cp850)$TEXT, "Café")
  expect_identical(read_dbc(unmarked, encoding = "latin1")$TEXT, "Café")
})

test_that("read_dbc auto-detects unmarked CP850, UTF-8, and binary IDs", {
  cp850 <- literal_dbc_fixture(
    fields = list(list(name = "TEXT", type = "C", width = 9L, decimals = 0L)),
    rows = list(list(c(charToRaw("DORM"), as.raw(210L), charToRaw("NCIA"))))
  )
  utf8 <- literal_dbc_fixture(
    fields = list(list(name = "TEXT", type = "C", width = 7L, decimals = 0L)),
    rows = list(list(charToRaw("MÁCULA")))
  )
  identifier_bytes <- as.raw(c(123:132, 123L))
  binary <- literal_dbc_fixture(
    fields = list(list(name = "CNS", type = "C", width = 11L, decimals = 0L)),
    rows = list(list(identifier_bytes)),
    language_driver = 88L
  )
  on.exit(unlink(c(cp850, utf8, binary)), add = TRUE)

  expect_no_warning(cp850_result <- read_dbc(cp850))
  expect_identical(cp850_result$TEXT, "DORMÊNCIA")
  expect_identical(
    unname(attr(cp850_result, "dbc_column_encodings")[["TEXT"]]),
    "CP850"
  )
  expect_no_warning(utf8_result <- read_dbc(utf8))
  expect_identical(utf8_result$TEXT, "MÁCULA")
  expect_identical(
    unname(attr(utf8_result, "dbc_column_encodings")[["TEXT"]]),
    "UTF-8"
  )
  expect_warning(
    binary_result <- read_dbc(binary),
    "obfuscated identifier bytes",
    class = "microdatasus_dbc_encoding_warning"
  )
  expect_identical(charToRaw(binary_result$CNS[[1L]]), identifier_bytes)
  expect_identical(Encoding(binary_result$CNS[[1L]]), "bytes")
})

test_that("read_dbc recognizes DataSUS and Portuguese LDIDs", {
  datasus <- literal_dbc_fixture(
    fields = list(
      list(name = "TEXT", type = "C", width = 3L, decimals = 0L)
    ),
    rows = list(list(as.raw(c(65L, 151L, 66L)))),
    language_driver = 53L
  )
  portuguese <- literal_dbc_fixture(
    fields = list(
      list(name = "TEXT", type = "C", width = 3L, decimals = 0L)
    ),
    rows = list(list(as.raw(c(83L, 132L, 111L)))),
    language_driver = 36L
  )
  latin1 <- literal_dbc_fixture(
    fields = list(
      list(name = "TEXT", type = "C", width = 1L, decimals = 0L)
    ),
    rows = list(list(as.raw(128L))),
    language_driver = 87L
  )
  on.exit(unlink(c(datasus, portuguese, latin1)), add = TRUE)

  expect_no_warning(datasus_result <- read_dbc(datasus))
  expect_identical(datasus_result$TEXT, "A—B")
  expect_identical(attr(datasus_result, "dbc_encoding"), "CP1252")
  expect_identical(read_dbc(portuguese)$TEXT, "São")
  expect_identical(attr(read_dbc(portuguese), "dbc_encoding"), "CP860")
  expect_identical(
    enc2utf8(read_dbc(latin1)$TEXT),
    intToUtf8(128L)
  )
  expect_identical(attr(read_dbc(latin1), "dbc_encoding"), "ISO-8859-1")
})

test_that("read_dbc accepts one DBF EOF marker and rejects extra output", {
  field <- list(name = "VALUE", type = "C", width = 1L, decimals = 0L)
  valid <- literal_dbc_fixture(
    fields = list(field),
    rows = list(list("x")),
    trailing = as.raw(26L)
  )
  extra <- literal_dbc_fixture(
    fields = list(field),
    rows = list(list("x")),
    trailing = as.raw(c(26L, 0L))
  )
  on.exit(unlink(c(valid, extra)), add = TRUE)

  expect_identical(read_dbc(valid)$VALUE, "x")
  expect_error(
    read_dbc(extra),
    "Unexpected data after",
    class = "microdatasus_dbc_read_error"
  )
})

test_that("read_dbc rejects bytes after the compressed stream", {
  path <- literal_dbc_fixture(
    fields = list(
      list(name = "VALUE", type = "C", width = 1L, decimals = 0L)
    ),
    rows = list(list("x"))
  )
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  writeBin(c(bytes, as.raw(0L)), path)
  on.exit(unlink(path), add = TRUE)

  expect_error(
    read_dbc(path),
    "Unexpected data after the compressed DBC stream",
    class = "microdatasus_dbc_read_error"
  )
})

test_that("DBC readers reject a CRC32 mismatch", {
  path <- literal_dbc_fixture(
    fields = list(
      list(name = "VALUE", type = "C", width = 1L, decimals = 0L)
    ),
    rows = list(list("x"))
  )
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  header_size <- sum(as.integer(bytes[9:10]) * c(1L, 256L))
  checksum_index <- header_size + 1L
  bytes[[checksum_index]] <- as.raw(
    bitwXor(as.integer(bytes[[checksum_index]]), 1L)
  )
  writeBin(bytes, path)
  output <- tempfile(fileext = ".dbf")
  on.exit(unlink(c(path, output)), add = TRUE)

  expect_error(
    read_dbc(path),
    "checksum mismatch",
    class = "microdatasus_dbc_read_error"
  )
  expect_error(
    microdatasus:::.dbc2dbf(path, output),
    "CRC32 checksum mismatch",
    class = "microdatasus_dbc_decompression_error"
  )
  expect_false(file.exists(output))
})

test_that("read_dbc validates field metadata and record status markers", {
  cases <- list(
    list(
      fields = list(
        list(name = raw(), type = "C", width = 1L, decimals = 0L)
      ),
      regexp = "empty name"
    ),
    list(
      fields = list(
        list(name = "VALUE", type = "X", width = 1L, decimals = 0L)
      ),
      regexp = "Unsupported DBF field type"
    ),
    list(
      fields = list(
        list(name = "DATE", type = "D", width = 7L, decimals = 0L)
      ),
      regexp = "date fields must have width 8"
    ),
    list(
      fields = list(
        list(name = "FLAG", type = "L", width = 2L, decimals = 0L)
      ),
      regexp = "logical fields must have width 1"
    ),
    list(
      fields = list(
        list(name = "VALUE", type = "N", width = 2L, decimals = 1L)
      ),
      regexp = "Invalid decimal count"
    )
  )
  paths <- vapply(cases, function(case) {
    width <- case$fields[[1L]]$width
    literal_dbc_fixture(case$fields, list(list(raw(width))))
  }, character(1))
  invalid_status <- literal_dbc_fixture(
    fields = list(
      list(name = "VALUE", type = "C", width = 1L, decimals = 0L)
    ),
    rows = list(list("x")),
    statuses = 0L
  )
  on.exit(unlink(c(paths, invalid_status)), add = TRUE)

  for (index in seq_along(cases)) {
    expect_error(
      read_dbc(paths[[index]]),
      cases[[index]]$regexp,
      class = "microdatasus_dbc_read_error"
    )
  }
  expect_error(
    read_dbc(invalid_status),
    "Invalid DBF record status marker",
    class = "microdatasus_dbc_read_error"
  )
})

test_that("read_dbc validates the terminator and exact record layout", {
  invalid_terminator <- literal_dbc_fixture(
    fields = list(
      list(name = "VALUE", type = "C", width = 1L, decimals = 0L)
    ),
    rows = list(list("x"))
  )
  bytes <- readBin(
    invalid_terminator,
    "raw",
    n = file.info(invalid_terminator)$size
  )
  header_size <- sum(as.integer(bytes[9:10]) * c(1L, 256L))
  bytes[[header_size]] <- as.raw(10L)
  writeBin(bytes, invalid_terminator)

  extra_record_byte <- literal_dbc_fixture(
    fields = list(
      list(name = "VALUE", type = "C", width = 1L, decimals = 0L)
    ),
    rows = list(list("x")),
    record_padding = as.raw(0L)
  )
  on.exit(unlink(c(invalid_terminator, extra_record_byte)), add = TRUE)

  expect_error(
    read_dbc(invalid_terminator),
    "field descriptor terminator",
    class = "microdatasus_dbc_read_error"
  )
  expect_error(
    read_dbc(extra_record_byte),
    "field widths do not match",
    class = "microdatasus_dbc_read_error"
  )
})

test_that("read_dbc supports validated DataSUS header variants", {
  field <- list(name = "VALUE", type = "C", width = 1L, decimals = 0L)
  padded <- literal_dbc_fixture(
    fields = list(field), rows = list(list("x")),
    header_extension = as.raw(0L)
  )
  foxpro <- literal_dbc_fixture(
    fields = list(field), rows = list(list("x")),
    version = 48L, header_extension = raw(263L)
  )
  nul_terminated <- literal_dbc_fixture(
    fields = list(field), rows = list(list("x")), terminator = 0L
  )
  unsupported <- literal_dbc_fixture(
    fields = list(field), rows = list(list("x")),
    header_extension = raw(2L)
  )
  on.exit(
    unlink(c(padded, foxpro, nul_terminated, unsupported)),
    add = TRUE
  )

  expect_identical(read_dbc(padded)$VALUE, "x")
  expect_identical(read_dbc(foxpro)$VALUE, "x")
  expect_identical(read_dbc(nul_terminated)$VALUE, "x")
  expect_error(
    read_dbc(unsupported),
    "Unsupported DBF header extension",
    class = "microdatasus_dbc_read_error"
  )
})

test_that("read_dbc rejects malformed and non-finite numerics", {
  invalid_values <- c("NaN", "Inf", "-Inf", "12x", "*12", "1e999")
  paths <- vapply(invalid_values, function(value) {
    literal_dbc_fixture(
      fields = list(
        list(
          name = "VALUE",
          type = "N",
          width = max(5L, nchar(value)),
          decimals = 0L
        )
      ),
      rows = list(list(value))
    )
  }, character(1))
  null_path <- literal_dbc_fixture(
    fields = list(
      list(name = "VALUE", type = "N", width = 4L, decimals = 0L)
    ),
    rows = list(list("****"))
  )
  on.exit(unlink(c(paths, null_path)), add = TRUE)

  for (path in paths) {
    expect_warning(
      result <- read_dbc(path, as_character = FALSE),
      "invalid numeric value"
    )
    expect_identical(result$VALUE, NA_integer_)
  }
  expect_identical(
    read_dbc(null_path, as_character = FALSE)$VALUE,
    NA_integer_
  )
})

test_that("read_dbc does not silently round plain DBF integers", {
  exact <- literal_dbc_fixture(
    fields = list(
      list(name = "VALUE", type = "N", width = 16L, decimals = 0L)
    ),
    rows = list(list("9007199254740992"))
  )
  inexact <- literal_dbc_fixture(
    fields = list(
      list(name = "VALUE", type = "N", width = 16L, decimals = 0L)
    ),
    rows = list(list("9007199254740993"))
  )
  on.exit(unlink(c(exact, inexact)), add = TRUE)

  expect_identical(
    read_dbc(exact, as_character = FALSE)$VALUE,
    9007199254740992
  )
  expect_error(
    read_dbc(inexact),
    "cannot be represented exactly",
    class = "microdatasus_dbc_read_error"
  )
})

test_that("read_dbc treats blank logicals as missing and warns on invalid ones", {
  blank <- literal_dbc_fixture(
    fields = list(
      list(name = "FLAG", type = "L", width = 1L, decimals = 0L)
    ),
    rows = list(list(" "))
  )
  invalid <- literal_dbc_fixture(
    fields = list(
      list(name = "FLAG", type = "L", width = 1L, decimals = 0L)
    ),
    rows = list(list("z"))
  )
  on.exit(unlink(c(blank, invalid)), add = TRUE)

  expect_no_warning(blank_result <- read_dbc(blank, as_character = FALSE))
  expect_identical(blank_result$FLAG, NA)
  expect_warning(
    invalid_result <- read_dbc(invalid, as_character = FALSE),
    "invalid value"
  )
  expect_identical(invalid_result$FLAG, NA)
})

test_that("read_dbc includes DBF records marked as deleted", {
  path <- literal_dbc_fixture(
    fields = list(
      list(name = "VALUE", type = "C", width = 1L, decimals = 0L)
    ),
    rows = list(list("x")),
    statuses = 42L
  )
  on.exit(unlink(path), add = TRUE)

  expect_identical(read_dbc(path)$VALUE, "x")
})

test_that("read_dbc handles a valid zero-row table", {
  path <- literal_dbc_fixture(
    fields = list(
      list(name = "VALUE", type = "C", width = 1L, decimals = 0L)
    ),
    rows = list()
  )
  on.exit(unlink(path), add = TRUE)

  result <- read_dbc(path)

  expect_named(result, "VALUE")
  expect_equal(nrow(result), 0L)
  expect_identical(result$VALUE, character())
})

test_that("read_dbc streams records across decompressor chunk boundaries", {
  fields <- lapply(seq_len(20L), function(index) {
    list(
      name = sprintf("F%02d", index),
      type = "C",
      width = 255L,
      decimals = 0L
    )
  })
  row <- lapply(seq_along(fields), function(index) as.character(index))
  path <- literal_dbc_fixture(fields, list(row, row))
  on.exit(unlink(path), add = TRUE)

  result <- read_dbc(path, vars = c("F01", "F20"))

  expect_equal(nrow(result), 2L)
  expect_identical(result$F01, c("1", "1"))
  expect_identical(result$F20, c("20", "20"))
})

test_that("DBC decompression supports spaced paths and overwrites output", {
  source <- dbc_fixture()
  input <- tempfile(pattern = "dbc fixture ", fileext = ".dbc")
  output <- tempfile(pattern = "dbf output ", fileext = ".dbf")
  on.exit(unlink(c(source, input, output)), add = TRUE)
  expect_true(file.copy(source, input))
  writeBin(charToRaw("stale output"), output)

  result <- microdatasus:::.dbc2dbf(input, output)
  dbf <- foreign::read.dbf(output, as.is = TRUE)

  expect_true(result)
  expect_true(file.exists(output))
  expect_gt(file.size(output), 0)
  expect_identical(dbf$CODE, c("001", "010"))
})

test_that("read_dbc validates its arguments", {
  path <- dbc_fixture()
  on.exit(unlink(path), add = TRUE)

  expect_error(read_dbc(character()), "single, non-empty file path")
  expect_error(read_dbc(NA_character_), "single, non-empty file path")
  expect_error(read_dbc(""), "single, non-empty file path")
  expect_error(read_dbc(c(path, path)), "single, non-empty file path")
  expect_error(read_dbc(1), "single, non-empty file path")
  expect_error(read_dbc(path, as_character = NA), "TRUE.*FALSE")
  expect_error(read_dbc(path, as_character = 1), "TRUE.*FALSE")
  expect_error(
    read_dbc(path, as_character = c(TRUE, FALSE)),
    "TRUE.*FALSE"
  )
  expect_error(read_dbc(path, as_character = NULL), "TRUE.*FALSE")
  expect_error(read_dbc(path, vars = character()), "vars.*NULL")
  expect_error(read_dbc(path, vars = c("CODE", NA)), "vars.*NULL")
  expect_error(read_dbc(path, vars = c("CODE", "CODE")), "duplicated")
  expect_error(read_dbc(path, vars = 1), "vars.*NULL")
  expect_error(read_dbc(path, encoding = character()), "encoding.*auto")
  expect_error(read_dbc(path, encoding = NA_character_), "encoding.*auto")
  expect_error(
    read_dbc(path, encoding = "not-a-real-code-page"),
    "not supported",
    class = "microdatasus_dbc_encoding_error"
  )
  expect_error(
    read_dbc(path, vars = "MISSING"),
    "Unknown variable",
    class = "microdatasus_unknown_vars"
  )
  expect_error(read_dbc(tempfile(fileext = ".dbc")), "File not found")
})

test_that("read_dbc rejects directories and empty files before decompression", {
  directory <- tempfile("dbc-directory-")
  empty <- tempfile(fileext = ".dbc")
  dir.create(directory)
  file.create(empty)
  on.exit(unlink(c(directory, empty), recursive = TRUE), add = TRUE)

  expect_error(
    read_dbc(directory),
    "must refer to a regular file",
    class = "microdatasus_dbc_file_error"
  )
  expect_error(
    microdatasus:::.dbc2dbf(directory, tempfile(fileext = ".dbf")),
    "must refer to a regular file",
    class = "microdatasus_dbc_file_error"
  )
  expect_error(
    read_dbc(empty),
    "is empty",
    class = "microdatasus_dbc_file_error"
  )
  expect_error(
    microdatasus:::.dbc2dbf(empty, tempfile(fileext = ".dbf")),
    "is empty",
    class = "microdatasus_dbc_file_error"
  )
})

test_that("read_dbc rejects unreadable files when permissions are enforced", {
  skip_on_os("windows")
  path <- dbc_fixture()
  on.exit({
    Sys.chmod(path, mode = "0600")
    unlink(path)
  }, add = TRUE)
  Sys.chmod(path, mode = "0000")
  skip_if(
    file.access(path, mode = 4L) == 0L,
    "Current user can read files regardless of permission bits."
  )

  expect_error(
    read_dbc(path),
    "not readable",
    class = "microdatasus_dbc_file_error"
  )
})

test_that("read_dbc rejects files whose size cannot be determined", {
  path <- dbc_fixture()
  on.exit(unlink(path), add = TRUE)
  local_mocked_bindings(
    .dbc_file_size = function(file) NA_real_,
    .package = "microdatasus"
  )

  expect_error(
    read_dbc(path),
    "Could not determine the size",
    class = "microdatasus_dbc_file_error"
  )
})

test_that("DBC decompressor validates input and output paths", {
  input <- dbc_fixture()
  output <- tempfile(fileext = ".dbf")
  on.exit(unlink(c(input, output)), add = TRUE)

  invalid_paths <- list(
    character(),
    NA_character_,
    "",
    c(input, input),
    1
  )
  for (value in invalid_paths) {
    expect_error(
      microdatasus:::.dbc2dbf(value, output),
      "input_file.*single, non-empty file path"
    )
    expect_error(
      microdatasus:::.dbc2dbf(input, value),
      "output_file.*single, non-empty file path"
    )
  }

  expect_error(
    microdatasus:::.dbc2dbf(tempfile(fileext = ".dbc"), output),
    "File not found"
  )
})

test_that("read_dbc rejects invalid DBC files", {
  path <- tempfile(fileext = ".dbc")
  output <- tempfile(fileext = ".dbf")
  writeBin(as.raw(1:10), path)
  on.exit(unlink(c(path, output)), add = TRUE)

  expect_error(
    microdatasus:::.dbc2dbf(path, output),
    "Failed to decompress the DBC file",
    class = "microdatasus_dbc_decompression_error"
  )
  expect_false(file.exists(output))
})

test_that("DBC decompressor survives a deterministic malformed corpus", {
  set.seed(20260804)
  for (index in seq_len(25L)) {
    size <- sample(c(1:64, 128, 256), 1L)
    body <- as.raw(sample.int(256L, size, replace = TRUE) - 1L)
    input <- write_dbc_bytes(dbc_bytes_with_compressed_body(body))
    output <- tempfile(fileext = ".dbf")
    result <- try(microdatasus:::.dbc2dbf(input, output), silent = TRUE)
    expect_true(inherits(result, "try-error") || isTRUE(result))
    unlink(c(input, output))
  }
})

test_that("DBC decompressor reports specific malformed-file errors", {
  invalid_header <- raw(10L)
  invalid_header[9L] <- as.raw(32L)
  truncated_header <- raw(10L)
  truncated_header[9L] <- as.raw(33L)

  cases <- list(
    list(
      bytes = raw(9L),
      regexp = "file too small"
    ),
    list(
      bytes = invalid_header,
      regexp = "invalid DBF header size"
    ),
    list(
      bytes = truncated_header,
      regexp = "failed to read DBF header"
    ),
    list(
      bytes = dbc_bytes_with_compressed_body(),
      regexp = "compressed data ended unexpectedly"
    ),
    list(
      bytes = dbc_bytes_with_compressed_body(as.raw(c(2L, 4L))),
      regexp = "invalid literal flag"
    ),
    list(
      bytes = dbc_bytes_with_compressed_body(as.raw(c(0L, 3L))),
      regexp = "invalid dictionary size"
    )
  )

  paths <- vapply(cases, function(case) {
    write_dbc_bytes(case$bytes)
  }, character(1))
  on.exit(unlink(paths), add = TRUE)

  for (index in seq_along(cases)) {
    output <- tempfile(fileext = ".dbf")
    expect_error(
      microdatasus:::.dbc2dbf(paths[[index]], output),
      cases[[index]]$regexp,
      class = "microdatasus_dbc_error",
      info = paste("Malformed DBC case", index)
    )
    expect_false(file.exists(output))
  }

  expect_error(
    read_dbc(paths[[1L]]),
    "Failed to read the DBC file",
    class = "microdatasus_dbc_read_error"
  )
})

test_that("DBC decompressor reports output paths that cannot be created", {
  input <- dbc_fixture()
  missing_parent <- tempfile("missing-output-parent-")
  output <- file.path(missing_parent, "output.dbf")
  on.exit(unlink(c(input, missing_parent), recursive = TRUE), add = TRUE)

  expect_error(
    microdatasus:::.dbc2dbf(input, output),
    "cannot open output"
  )
  expect_false(file.exists(output))
})

test_that("DBC decompressor never overwrites its input", {
  input <- dbc_fixture()
  before <- readBin(input, "raw", n = file.info(input)$size)
  on.exit(unlink(input), add = TRUE)

  expect_error(
    microdatasus:::.dbc2dbf(input, input),
    "must refer to different files",
    class = "microdatasus_dbc_decompression_error"
  )
  expect_identical(readBin(input, "raw", n = file.info(input)$size), before)
})

test_that("DBC decompressor rejects a zero-sized output target", {
  skip_on_os("windows")
  skip_if_not(file.exists("/dev/null"))
  input <- dbc_fixture()
  on.exit(unlink(input), add = TRUE)

  expect_error(
    microdatasus:::.dbc2dbf(input, "/dev/null"),
    "produced no valid output",
    class = "microdatasus_dbc_decompression_error"
  )
})

test_that("DBC decompressor rejects output whose size cannot be determined", {
  input <- dbc_fixture()
  output <- tempfile(fileext = ".dbf")
  on.exit(unlink(c(input, output)), add = TRUE)
  local_mocked_bindings(
    .dbc_file_size = function(file) {
      if (identical(file, output)) {
        return(NA_real_)
      }
      unname(file.info(file)$size)
    },
    .package = "microdatasus"
  )

  expect_error(
    microdatasus:::.dbc2dbf(input, output),
    "produced no valid output",
    class = "microdatasus_dbc_decompression_error"
  )
})

test_that("read_dbc uses the direct native reader", {
  input <- dbc_fixture()
  on.exit(unlink(input), add = TRUE)
  local_mocked_bindings(
    .dbc2dbf = function(...) {
      stop("the DBF conversion path must not be called")
    },
    .package = "microdatasus"
  )

  result <- read_dbc(input)
  repeated <- read_dbc(input)

  expect_identical(result$CODE, c("001", "010"))
  expect_identical(repeated, result)
})

test_that("read_dbc reports native reader failures with stable classes", {
  input <- write_dbc_bytes(as.raw(1:10))
  on.exit(unlink(input), add = TRUE)

  error <- tryCatch(read_dbc(input), error = identity)

  expect_s3_class(error, "microdatasus_dbc_read_error")
  expect_s3_class(error, "microdatasus_dbc_error")
  expect_match(
    conditionMessage(error),
    "Failed to read the DBC file"
  )
  expect_match(conditionMessage(error), basename(input), fixed = TRUE)
  expect_s3_class(error$parent, "error")
})
