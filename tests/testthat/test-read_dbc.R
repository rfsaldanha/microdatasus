dbc_fixture <- function() {
  hex <- paste0(
    "035f071a02000000810020000000000000000000000000000000000000000000",
    "434f444500000000000000430000000004000000000000000000000000000000",
    "56414c55450000000000004e00000000130f0000000000000000000000000000",
    "5748454e00000000000000440000000008000000000000000000000000000000",
    "0d00000000000640c080110364470c17355bc40c90f9c186b659ff6d80508934",
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

test_that("read_dbc public signature remains stable", {
  expect_identical(
    formals(read_dbc),
    as.pairlist(alist(
      file = ,
      as_character = TRUE
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
  expect_error(read_dbc(tempfile(fileext = ".dbc")), "File not found")
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
    "Failed to decompress the DBC file"
  )
  expect_false(file.exists(output))
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
      info = paste("Malformed DBC case", index)
    )
    expect_false(file.exists(output))
  }

  expect_error(
    read_dbc(paths[[1L]]),
    "Failed to decompress the DBC file"
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

test_that("read_dbc removes its temporary DBF after successful reading", {
  input <- dbc_fixture()
  dbf_fixture <- tempfile(fileext = ".dbf")
  on.exit(unlink(c(input, dbf_fixture)), add = TRUE)
  foreign::write.dbf(
    data.frame(
      CODE = c("01", "02"),
      FLAG = c(TRUE, NA),
      stringsAsFactors = FALSE
    ),
    dbf_fixture
  )
  temporary <- character()
  local_mocked_bindings(
    .dbc2dbf = function(input_file, output_file) {
      temporary <<- c(temporary, output_file)
      expect_true(file.copy(dbf_fixture, output_file))
      invisible(TRUE)
    },
    .package = "microdatasus"
  )

  result <- read_dbc(input)
  repeated <- read_dbc(input)

  expect_identical(result$CODE, c("01", "02"))
  expect_identical(result$FLAG, c("TRUE", NA_character_))
  expect_identical(repeated, result)
  expect_length(unique(temporary), 2L)
  expect_false(any(file.exists(temporary)))
})

test_that("read_dbc removes its temporary DBF after decompression failure", {
  input <- dbc_fixture()
  temporary <- NULL
  on.exit(unlink(input), add = TRUE)
  local_mocked_bindings(
    .dbc2dbf = function(input_file, output_file) {
      temporary <<- output_file
      writeBin(charToRaw("partial DBF"), output_file)
      stop("mock decompression failure")
    },
    .package = "microdatasus"
  )

  expect_error(read_dbc(input), "mock decompression failure")
  expect_false(file.exists(temporary))
})

test_that("read_dbc removes its temporary DBF after DBF reading failure", {
  input <- dbc_fixture()
  temporary <- NULL
  on.exit(unlink(input), add = TRUE)
  local_mocked_bindings(
    .dbc2dbf = function(input_file, output_file) {
      temporary <<- output_file
      writeBin(charToRaw("not a DBF"), output_file)
      invisible(TRUE)
    },
    .package = "microdatasus"
  )

  expect_error(read_dbc(input))
  expect_false(file.exists(temporary))
})
