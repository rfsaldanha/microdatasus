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

test_that("read_dbc reads DBC data and preserves leading zeros", {
  path <- dbc_fixture()
  on.exit(unlink(path), add = TRUE)

  result <- read_dbc(path)

  expect_s3_class(result, "tbl_df")
  expect_identical(result$CODE, c("001", "010"))
  expect_identical(result$VALUE, c("1.5", NA_character_))
  expect_identical(result$WHEN, c("2020-01-02", NA_character_))
  expect_true(all(vapply(result, is.character, logical(1))))
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

test_that("read_dbc validates its arguments", {
  path <- dbc_fixture()
  on.exit(unlink(path), add = TRUE)

  expect_error(read_dbc(character()), "single, non-empty file path")
  expect_error(read_dbc(NA_character_), "single, non-empty file path")
  expect_error(read_dbc(path, as_character = NA), "TRUE.*FALSE")
  expect_error(read_dbc(path, as_character = 1), "TRUE.*FALSE")
  expect_error(read_dbc(tempfile(fileext = ".dbc")), "File not found")
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
