# Multipart coverage is intentionally independent of the number of parts
# currently published by DataSUS. Counts below are test sizes, not API limits.
.multipart_test_names <- function(spec, suffixes, uf = "AC", year = "2022", month = "01") {
  national <- identical(spec$geography, "national")
  period <- if (identical(spec$granularity, "month")) {
    paste0(substr(year, 3L, 4L), month)
  } else if (identical(spec$year_digits, 2L)) {
    substr(year, 3L, 4L)
  } else {
    year
  }
  paste0(spec$repositories[[1L]]$prefix, if (national) "" else uf,
         period, suffixes, ".dbc")
}

test_that("every system discovers arbitrary multipart suffixes without a fixed limit", {
  registry <- microdatasus:::.datasus_registry()
  # More than one alphabet, multi-character suffixes, numeric suffixes, gaps,
  # and mixed case must all be discovered from the listing itself.
  suffixes <- c(letters, paste0("a", letters), paste0("b", letters),
                "aaa", "Zzz", "part0001", "part10000", "000001", "999999")
  listing <- character()
  local_mocked_bindings(
    .datasus_list_directory = function(url, timeout) {
      paste(c(rev(listing), listing[[1L]], "README.txt"), collapse = "\n")
    },
    .package = "microdatasus"
  )

  for (system in names(registry)) {
    spec <- registry[[system]]
    listing <- .multipart_test_names(spec, suffixes)
    period <- if (identical(spec$granularity, "month")) "2201" else "2022"
    result <- microdatasus:::.datasus_build_manifest(spec, period, "AC", 1)
    expected <- listing[order(toupper(suffixes), listing)]

    expect_equal(length(result$errors), 0L, info = system)
    expect_equal(nrow(result$manifest), length(suffixes), info = system)
    expect_identical(result$manifest$file, expected, info = system)
    expect_identical(result$manifest$url,
                     paste0(spec$repositories[[1L]]$url, expected), info = system)
    expect_identical(sort(result$manifest$fragment), sort(toupper(suffixes)), info = system)
  }
})

test_that("every system downloads and aggregates all listed parts beyond z", {
  registry <- microdatasus:::.datasus_registry()
  fixture <- test_path("..", "fixtures", "historical", "RDAC9801", "input.dbc")
  reference <- read_dbc(fixture)
  suffixes <- c(letters, "aa", "az", "ba", "aaa")
  listing <- downloaded <- character()
  local_mocked_bindings(
    .datasus_list_directory = function(url, timeout) paste(rev(listing), collapse = "\n"),
    .datasus_download_file = function(url, destination, timeout, quiet) {
      downloaded <<- c(downloaded, url)
      if (!file.copy(fixture, destination, overwrite = TRUE)) stop("Cannot copy test fixture")
      invisible(destination)
    },
    .package = "microdatasus"
  )

  for (system in names(registry)) {
    spec <- registry[[system]]
    listing <- .multipart_test_names(spec, suffixes)
    downloaded <- character()
    args <- list(year_start = 2022, year_end = 2022,
                 uf = if (identical(spec$geography, "national")) "all" else "AC",
                 information_system = system, quiet = TRUE, cache_dir = NULL,
                 track_source = TRUE, provenance = TRUE, stop_on_error = TRUE)
    if (identical(spec$granularity, "month")) {
      args$month_start <- args$month_end <- 1
    }
    result <- do.call(fetch_datasus, args)
    expected_files <- listing[order(toupper(suffixes), listing)]

    expect_identical(downloaded, paste0(spec$repositories[[1L]]$url, expected_files), info = system)
    expect_equal(nrow(result), length(suffixes) * nrow(reference), info = system)
    expect_identical(result$source, rep(expected_files, each = nrow(reference)), info = system)
    expect_identical(datasus_provenance(result)$file, expected_files, info = system)
    for (field in names(reference)) {
      expect_identical(result[[field]], rep(reference[[field]], length(suffixes)),
                       info = paste(system, field))
    }
  }
})

test_that("multipart counts remain specific to each state and period", {
  spec <- microdatasus:::.datasus_registry()[["SIA-PA"]]
  # MG and SP have different parts; January also differs from December.
  # In particular, there is no PAMG2512.dbc or PAMG2512c.dbc to invent.
  expected <- c(
    "PAMG2512a.dbc", "PAMG2512b.dbc",
    .multipart_test_names(spec, c(letters, "aa", "ab"), "SP", "2025", "12"),
    "PAMG2601a.dbc", "PAMG2601c.dbc", "PAMG2601aa.dbc",
    "PASP2601.dbc"
  )
  listing <- c(expected, "PARJ2512a.dbc", "PAMG2511a.dbc")
  fixture <- test_path("..", "fixtures", "historical", "RDAC9801", "input.dbc")
  downloaded <- character()
  local_mocked_bindings(
    .datasus_list_directory = function(url, timeout) paste(rev(listing), collapse = "\n"),
    .datasus_download_file = function(url, destination, timeout, quiet) {
      downloaded <<- c(downloaded, basename(url))
      if (!basename(url) %in% expected) stop("Requested a file not in the published state-period listing")
      if (!file.copy(fixture, destination, overwrite = TRUE)) stop("Cannot copy test fixture")
      invisible(destination)
    },
    .package = "microdatasus"
  )

  result <- fetch_datasus(
    year_start = 2025, month_start = 12, year_end = 2026, month_end = 1,
    uf = c("MG", "SP"), information_system = "SIA-PA",
    quiet = TRUE, cache_dir = NULL, track_source = TRUE,
    provenance = TRUE, stop_on_error = TRUE
  )
  expect_setequal(downloaded, expected)
  expect_length(downloaded, length(expected))
  expect_equal(nrow(result), nrow(read_dbc(fixture)) * length(expected))
  expect_setequal(unique(result$source), expected)
  expect_equal(anyDuplicated(datasus_provenance(result)$file), 0L)
})
