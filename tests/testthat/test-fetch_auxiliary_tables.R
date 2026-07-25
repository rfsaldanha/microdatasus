test_that("auxiliary fetchers keep their public signatures", {
  expected <- as.pairlist(alist(timeout = 240))
  expect_identical(formals(fetch_cadger), expected)
  expect_identical(formals(fetch_sigtab), expected)
})

test_that("CADGER keeps only its public columns as character", {
  local_mocked_bindings(
    .datasus_fetch_zip_dbf = function(url, internal_file, timeout) {
      data.frame(
        CNES = 1234567,
        FANTASIA = "Unidade de Saude",
        EXTRA = "ignored"
      )
    },
    .package = "microdatasus"
  )
  original_timeout <- getOption("timeout")

  result <- fetch_cadger(timeout = 17)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("CNES", "FANTASIA"))
  expect_type(result$CNES, "character")
  expect_type(result$FANTASIA, "character")
  expect_identical(getOption("timeout"), original_timeout)
})

test_that("CADGER validates its schema", {
  local_mocked_bindings(
    .datasus_fetch_zip_dbf = function(url, internal_file, timeout) {
      data.frame(CNES = "123")
    },
    .package = "microdatasus"
  )

  expect_error(fetch_cadger(), "FANTASIA")
})

test_that("SIGTAB returns stable names and character columns", {
  local_mocked_bindings(
    .datasus_fetch_zip_dbf = function(url, internal_file, timeout) {
      data.frame(code = 101, label = "Procedimento")
    },
    .package = "microdatasus"
  )
  original_timeout <- getOption("timeout")

  result <- fetch_sigtab(timeout = 18)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("COD", "nome_proced"))
  expect_type(result$COD, "character")
  expect_type(result$nome_proced, "character")
  expect_identical(getOption("timeout"), original_timeout)
})

test_that("SIGTAB validates its schema", {
  local_mocked_bindings(
    .datasus_fetch_zip_dbf = function(url, internal_file, timeout) {
      data.frame(one = 1, two = 2, three = 3)
    },
    .package = "microdatasus"
  )

  expect_error(fetch_sigtab(), "exactly two columns")
})

create_auxiliary_zip <- function(internal_file, data) {
  root <- tempfile("microdatasus-zip-fixture-")
  dir.create(file.path(root, dirname(internal_file)), recursive = TRUE)
  foreign::write.dbf(data, file.path(root, internal_file))

  archive <- tempfile(fileext = ".zip")
  zip::zipr(archive, files = dirname(internal_file), root = root)
  unlink(root, recursive = TRUE)
  archive
}

test_that("ZIP helper extracts DBF data and cleans its private directory", {
  archive <- create_auxiliary_zip(
    file.path("DBF", "TABLE.dbf"),
    data.frame(code = c("01", "02"), label = c("One", "Two"))
  )
  on.exit(unlink(archive), add = TRUE)
  work_dir <- NULL
  seen_timeout <- NULL
  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout) {
      work_dir <<- dirname(destination)
      seen_timeout <<- timeout
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )

  result <- microdatasus:::.datasus_fetch_zip_dbf(
    "ftp://example/table.zip",
    file.path("DBF", "TABLE.dbf"),
    timeout = 19
  )

  expect_equal(result$code, c("01", "02"))
  expect_type(result$label, "character")
  expect_equal(seen_timeout, 19)
  expect_false(dir.exists(work_dir))
})

test_that("ZIP helper cleans temporary files after download failure", {
  work_dir <- NULL
  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout) {
      work_dir <<- dirname(destination)
      stop("download failed")
    },
    .package = "microdatasus"
  )

  expect_error(
    microdatasus:::.datasus_fetch_zip_dbf(
      "ftp://example/table.zip",
      file.path("DBF", "TABLE.dbf"),
      timeout = 20
    ),
    "download failed"
  )
  expect_false(dir.exists(work_dir))
})

test_that("ZIP helper rejects invalid timeout and archives", {
  expect_error(
    microdatasus:::.datasus_fetch_zip_dbf(
      "ftp://example/table.zip",
      file.path("DBF", "TABLE.dbf"),
      timeout = 0
    ),
    "single number"
  )

  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout) {
      writeBin(charToRaw("not a zip archive"), destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  expect_error(
    microdatasus:::.datasus_fetch_zip_dbf(
      "ftp://example/table.zip",
      file.path("DBF", "TABLE.dbf"),
      timeout = 20
    ),
    "Failed to extract"
  )
})
