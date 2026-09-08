.historical_root <- test_path("..", "fixtures", "historical")
.historical_manifest <- read.csv(file.path(.historical_root, "manifest.csv"),
                                  colClasses = "character", check.names = FALSE)
.historical_expected <- read.csv(file.path(.historical_root, "expected.csv"),
                                  colClasses = "character", fileEncoding = "UTF-8")

.local_historical_sources <- function(ids, env = parent.frame()) {
  cases <- .historical_manifest[.historical_manifest$case %in% ids, ]
  withr::local_options(microdatasus.cache_dir = NULL, .local_envir = env)
  microdatasus:::.tabwin_clear_cache()
  withr::defer(restore_empty_tabwin_cache(), envir = env)
  sources <- list()
  for (url in unique(cases$dictionary_url)) {
    rows <- cases[cases$dictionary_url == url, ]
    if (nrow(rows) == 1L) {
      sources[[url]] <- file.path(.historical_root, rows$case, "dictionary.zip")
    } else {
      root <- tempfile("historical-merged-")
      dir.create(root)
      withr::defer(unlink(root, recursive = TRUE), envir = env)
      for (id in rows$case) {
        utils::unzip(file.path(.historical_root, id, "dictionary.zip"), exdir = root)
      }
      archive <- tempfile(fileext = ".zip")
      withr::defer(unlink(archive), envir = env)
      zip::zipr(archive, list.files(root, recursive = TRUE), root = root,
                mode = "mirror", include_directories = FALSE)
      sources[[url]] <- archive
    }
  }
  testthat::local_mocked_bindings(
    .datasus_download_file = function(url, destination, ...) {
      if (!url %in% names(sources)) stop("Unexpected network request: ", url)
      stopifnot(file.copy(sources[[url]], destination, overwrite = TRUE))
      invisible(destination)
    },
    .package = "microdatasus", .env = env
  )
}

.expect_historical_values <- function(result, id, labels, raw, rows = 1:3) {
  expected <- .historical_expected[.historical_expected$case == id, ]
  expect_setequal(expected$field, names(raw))
  for (i in seq_len(nrow(expected))) {
    field <- expected$field[[i]]
    type <- expected$type[[i]]
    value <- unname(unlist(expected[i, c("value1", "value2", "value3")]))
    actual <- result[[field]][rows]
    if (type == "categorical") {
      if (labels == "factor") expect_s3_class(actual, "factor") else expect_type(actual, "character")
      if (labels == "none") value <- raw[[field]]
      expect_identical(as.character(actual), value, info = paste(id, field, labels))
      expect_equal(table(as.character(actual), dnn = field), table(value, dnn = field))
    } else {
      value <- switch(type, integer = as.integer(value), double = as.numeric(value),
                      Date = as.Date(value), character = value)
      expect_identical(class(actual), class(value), info = paste(id, field))
      expect_equal(actual, value, tolerance = 1e-12, info = paste(id, field))
    }
  }
}

test_that("frozen historical inputs and official dictionary members match their hashes", {
  checksum <- function(file) digest::digest(file, algo = "sha256", file = TRUE)
  members <- read.csv(file.path(.historical_root, "dictionary-members.csv"),
                       colClasses = "character")
  for (i in seq_len(nrow(.historical_manifest))) {
    case <- .historical_manifest[i, ]
    root <- file.path(.historical_root, case$case)
    expect_identical(checksum(file.path(root, "input.dbc")), case$input_sha256)
    expect_identical(checksum(file.path(root, "dictionary.zip")), case$dictionary_fixture_sha256)
    expect_identical(checksum(file.path(root, "raw.csv")), case$raw_sha256)
    extracted <- withr::local_tempdir()
    utils::unzip(file.path(root, "dictionary.zip"), exdir = extracted)
    expected <- members[members$case == case$case, ]
    for (j in seq_len(nrow(expected))) {
      expect_identical(checksum(file.path(extracted, expected$member[[j]])), expected$sha256[[j]])
    }
  }
})

for (.historical_id in .historical_manifest$case) {
  test_that(paste("frozen historical data are processed correctly:", .historical_id), {
    id <- .historical_id
    .local_historical_sources(id)
    case <- .historical_manifest[.historical_manifest$case == id, ]
    root <- file.path(.historical_root, id)
    raw <- read_dbc(file.path(root, "input.dbc"))
    independent <- read.csv(file.path(root, "raw.csv"), colClasses = "character",
                             check.names = FALSE)
    expect_identical(names(raw), names(independent))
    for (field in names(raw)) expect_identical(raw[[field]], independent[[field]])
    for (labels in c("factor", "character", "none")) {
      result <- microdatasus:::.datasus_process_file(raw, case$information_system,
        list(municipality_data = FALSE, labels = labels, diagnostics = TRUE))
      expect_identical(nrow(result), 3L)
      expect_identical(names(result), names(raw))
      .expect_historical_values(result, id, labels, raw)
      report <- processing_diagnostics(result)
      expect_identical(report$dictionaries$information_system, case$dictionary)
      expect_equal(nrow(report$unknown_codes), 0L)
      expect_equal(nrow(report$coercion_failures), 0L)
    }
  })
}

test_that("mixed historical files keep row-specific meanings, types and counts", {
  for (system in unique(.historical_manifest$information_system)) {
    cases <- .historical_manifest[.historical_manifest$information_system == system, ]
    .local_historical_sources(cases$case)
    raw <- lapply(cases$case, function(id) read_dbc(file.path(.historical_root, id, "input.dbc")))
    combined <- as.data.frame(data.table::rbindlist(raw, use.names = TRUE, fill = TRUE))
    for (labels in c("factor", "character", "none")) {
      result <- microdatasus:::.datasus_process_file(combined, system,
        list(municipality_data = FALSE, labels = labels, diagnostics = TRUE))
      expect_identical(nrow(result), 6L)
      for (i in seq_along(raw)) {
        .expect_historical_values(result, cases$case[[i]], labels, raw[[i]],
                                  rows = (i - 1L) * 3L + 1:3)
      }
      expect_setequal(processing_diagnostics(result)$dictionaries$information_system, cases$dictionary)
    }
  }
})
