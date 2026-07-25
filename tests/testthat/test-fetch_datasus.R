test_that("fetch_datasus public signature remains compatible", {
  expect_identical(
    formals(fetch_datasus),
    as.pairlist(alist(
      year_start = ,
      month_start = NULL,
      year_end = ,
      month_end = NULL,
      uf = "all",
      information_system = ,
      vars = NULL,
      stop_on_error = FALSE,
      timeout = 240,
      track_source = FALSE
    ))
  )
})

test_that("registry contains every supported system", {
  registry <- microdatasus:::.datasus_registry()

  expect_length(registry, 43L)
  expect_setequal(
    names(registry),
    c(
      "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT",
      "SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER", "SINASC",
      paste0("CNES-", c(
        "LT", "ST", "DC", "EQ", "SR", "HB", "PF",
        "EP", "RC", "IN", "EE", "EF", "GM"
      )),
      paste0("SIA-", c(
        "AB", "ABO", "ACF", "AD", "AN", "AM",
        "AQ", "AR", "ATD", "PA", "PS", "SAD"
      )),
      paste0("SINAN-", c(
        "DENGUE", "CHIKUNGUNYA", "ZIKA", "MALARIA", "CHAGAS",
        "LEISHMANIOSE-VISCERAL", "LEISHMANIOSE-TEGUMENTAR",
        "LEPTOSPIROSE"
      ))
    )
  )
})

test_that("listings are parsed for all filename families", {
  registry <- microdatasus:::.datasus_registry()

  sim <- microdatasus:::.datasus_parse_listing(
    "DOAC2022.dbc\nDOSP2022.dbc\nREADME.txt\n",
    registry[["SIM-DO"]]$repositories[[1L]],
    registry[["SIM-DO"]]
  )
  expect_equal(sim$period, c("2022", "2022"))
  expect_equal(sim$uf, c("AC", "SP"))

  sih <- microdatasus:::.datasus_parse_listing(
    "RDAC2401.dbc\nRDAC2402.dbc\n",
    registry[["SIH-RD"]]$repositories[[1L]],
    registry[["SIH-RD"]]
  )
  expect_equal(sih$period, c("2401", "2402"))

  sinasc <- microdatasus:::.datasus_parse_listing(
    "DNRAC1994.dbc\n",
    registry[["SINASC"]]$repositories[[3L]],
    registry[["SINASC"]]
  )
  expect_equal(sinasc$period, "1994")
  expect_equal(sinasc$uf, "AC")

  cnes <- microdatasus:::.datasus_parse_listing(
    "STAC2401.dbc\n",
    registry[["CNES-ST"]]$repositories[[1L]],
    registry[["CNES-ST"]]
  )
  expect_equal(cnes$period, "2401")
  expect_equal(cnes$uf, "AC")

  sia <- microdatasus:::.datasus_parse_listing(
    "PAAC2401.dbc\nPAAC2401a.dbc\n",
    registry[["SIA-PA"]]$repositories[[1L]],
    registry[["SIA-PA"]]
  )
  expect_equal(sia$fragment, c("", "A"))

  sinan <- microdatasus:::.datasus_parse_listing(
    "DENGBR22.dbc\n",
    registry[["SINAN-DENGUE"]]$repositories[[1L]],
    registry[["SINAN-DENGUE"]]
  )
  expect_equal(sinan$period, "2022")
  expect_true(all(is.na(sinan$uf)))
})

test_that("exact prefixes prevent AB and ABO collisions", {
  registry <- microdatasus:::.datasus_registry()
  parsed <- microdatasus:::.datasus_parse_listing(
    "ABAC2401.dbc\nABOAC2401.dbc\n",
    registry[["SIA-AB"]]$repositories[[1L]],
    registry[["SIA-AB"]]
  )

  expect_equal(parsed$file, "ABAC2401.dbc")
})

test_that("manifest lists a directory once and applies release precedence", {
  spec <- list(
    granularity = "year",
    geography = "national",
    year_digits = 2L,
    repositories = list(
      list(
        url = "ftp://example/",
        release = "final",
        priority = 1L,
        prefix = "DENGBR"
      ),
      list(
        url = "ftp://example/",
        release = "preliminary",
        priority = 2L,
        prefix = "DENGBR"
      )
    )
  )
  calls <- 0L
  local_mocked_bindings(
    .datasus_list_directory = function(url, timeout) {
      calls <<- calls + 1L
      "DENGBR22.dbc\n"
    },
    .package = "microdatasus"
  )

  result <- microdatasus:::.datasus_build_manifest(
    spec,
    periods = "2022",
    ufs = "AC",
    timeout = 1
  )

  expect_equal(calls, 1L)
  expect_equal(nrow(result$manifest), 1L)
  expect_equal(result$manifest$release, "final")
})

test_that("manifest ordering follows requested periods, states and fragments", {
  registry <- microdatasus:::.datasus_registry()
  local_mocked_bindings(
    .datasus_list_directory = function(url, timeout) {
      paste(
        "PAAC2402b.dbc",
        "PASP2401.dbc",
        "PAAC2401a.dbc",
        "PAAC2401.dbc",
        sep = "\n"
      )
    },
    .package = "microdatasus"
  )

  result <- microdatasus:::.datasus_build_manifest(
    registry[["SIA-PA"]],
    periods = c("2401", "2402"),
    ufs = c("SP", "AC"),
    timeout = 1
  )

  expect_equal(
    result$manifest$file,
    c(
      "PASP2401.dbc",
      "PAAC2401.dbc",
      "PAAC2401a.dbc",
      "PAAC2402b.dbc"
    )
  )
})

test_that("retry performs at most three attempts", {
  attempts <- 0L
  waits <- numeric()
  local_mocked_bindings(
    .datasus_retry_wait = function(seconds) waits <<- c(waits, seconds),
    .package = "microdatasus"
  )

  result <- microdatasus:::.datasus_retry(function() {
    attempts <<- attempts + 1L
    if (attempts < 3L) {
      stop("temporary")
    }
    "ok"
  })

  expect_equal(result, "ok")
  expect_equal(attempts, 3L)
  expect_equal(waits, c(1, 2))
})

test_that("retry stops immediately for permanent errors", {
  attempts <- 0L
  permanent <- structure(
    list(message = "not found", call = NULL),
    class = c(
      "curl_error_ftp_couldnt_retr_file",
      "curl_error",
      "error",
      "condition"
    )
  )

  expect_error(
    microdatasus:::.datasus_retry(
      function() {
        attempts <<- attempts + 1L
        stop(permanent)
      },
      retry_if = microdatasus:::.datasus_is_transient_curl_error
    ),
    "not found"
  )
  expect_equal(attempts, 1L)
})

test_that("fractional timeouts are preserved in milliseconds", {
  expect_equal(microdatasus:::.datasus_timeout_ms(0.001), 1L)
  expect_equal(microdatasus:::.datasus_timeout_ms(0.0001), 1L)
  expect_equal(microdatasus:::.datasus_timeout_ms(1.5), 1500L)
  expect_equal(microdatasus:::.datasus_timeout_ms(240), 240000L)
  expect_equal(
    microdatasus:::.datasus_timeout_ms(.Machine$double.xmax),
    .Machine$integer.max
  )
})

test_that("download validates empty files without retrying them", {
  attempts <- 0L
  seen_timeout <- NULL
  destination <- tempfile()
  on.exit(unlink(destination), add = TRUE)
  local_mocked_bindings(
    .datasus_transfer_file = function(url, destination, timeout) {
      attempts <<- attempts + 1L
      seen_timeout <<- timeout
      file.create(destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )

  expect_error(
    microdatasus:::.datasus_download_file(
      "ftp://example/empty.dbc",
      destination,
      timeout = 17
    ),
    "empty"
  )
  expect_equal(attempts, 1L)
  expect_equal(seen_timeout, 17)
})

mock_manifest <- function(files, periods = rep("2022", length(files))) {
  data.frame(
    file = files,
    url = paste0("mock://", files),
    period = periods,
    uf = rep("AC", length(files)),
    fragment = rep("", length(files)),
    release = rep("final", length(files)),
    priority = rep(1L, length(files)),
    repository_order = rep(1L, length(files)),
    stringsAsFactors = FALSE
  )
}

mock_fetch_dependencies <- function(manifest, reader = NULL, downloader = NULL) {
  if (is.null(downloader)) {
    downloader <- function(url, destination, timeout) {
      writeBin(charToRaw(url), destination)
      invisible(destination)
    }
  }
  if (is.null(reader)) {
    reader <- function(file, as_character = TRUE) {
      marker <- rawToChar(readBin(file, "raw", n = file.info(file)$size))
      tibble::tibble(id = sub("mock://", "", marker), value = "x")
    }
  }
  local_mocked_bindings(
    .datasus_build_manifest = function(spec, periods, ufs, timeout) {
      list(manifest = manifest, errors = character())
    },
    .datasus_download_file = downloader,
    read_dbc = reader,
    .package = "microdatasus",
    .env = parent.frame()
  )
}

test_that("fetch selects variables once and always preserves source tracking", {
  mock_fetch_dependencies(mock_manifest(
    c("DOAC2022.dbc", "DOAC2023.dbc"),
    c("2022", "2023")
  ))

  result <- fetch_datasus(
    year_start = 2022,
    year_end = 2023,
    uf = "AC",
    information_system = "SIM-DO",
    vars = "value",
    track_source = TRUE
  )

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("value", "source"))
  expect_equal(result$source, c("DOAC2022.dbc", "DOAC2023.dbc"))
})

test_that("source conflicts and unknown variables always abort", {
  mock_fetch_dependencies(
    mock_manifest("DOAC2022.dbc"),
    reader = function(file, as_character = TRUE) {
      tibble::tibble(source = "existing", value = "x")
    }
  )
  expect_error(
    fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO",
      track_source = TRUE
    ),
    class = "microdatasus_source_conflict"
  )

  mock_fetch_dependencies(mock_manifest("DOAC2022.dbc"))
  expect_error(
    fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO",
      vars = "missing"
    ),
    class = "microdatasus_unknown_vars"
  )
})

test_that("partial failures return valid files and temporary files are removed", {
  temporary_paths <- character()
  mock_fetch_dependencies(
    mock_manifest(c("good.dbc", "bad.dbc")),
    downloader = function(url, destination, timeout) {
      temporary_paths <<- c(temporary_paths, destination)
      if (grepl("bad", url, fixed = TRUE)) {
        stop("network failure")
      }
      writeBin(charToRaw(url), destination)
      invisible(destination)
    }
  )

  expect_warning(
    result <- fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO"
    ),
    "could not be processed"
  )
  expect_equal(nrow(result), 1L)
  expect_false(any(file.exists(temporary_paths)))
})

test_that("invalid DBC files are reported without download retries", {
  reads <- 0L
  mock_fetch_dependencies(
    mock_manifest("invalid.dbc"),
    reader = function(file, as_character = TRUE) {
      reads <<- reads + 1L
      stop("invalid DBC")
    }
  )

  expect_warning(
    result <- fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO"
    ),
    "invalid DBC"
  )
  expect_equal(reads, 1L)
  expect_null(result)
})

test_that("stop_on_error controls file failures", {
  mock_fetch_dependencies(
    mock_manifest("bad.dbc"),
    downloader = function(url, destination, timeout) stop("network failure")
  )

  expect_error(
    fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO",
      stop_on_error = TRUE
    ),
    "Failed to process"
  )

  expect_warning(
    result <- fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO"
    ),
    "could not be processed"
  )
  expect_null(result)
})

test_that("directory failures respect stop_on_error and preserve NULL return", {
  local_mocked_bindings(
    .datasus_build_manifest = function(spec, periods, ufs, timeout) {
      list(
        manifest = data.frame(),
        errors = "ftp://example/: connection failed"
      )
    },
    .package = "microdatasus"
  )

  expect_warning(
    result <- fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO"
    ),
    "could not be listed"
  )
  expect_null(result)

  expect_error(
    fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO",
      stop_on_error = TRUE
    ),
    "could not be listed"
  )
})

test_that("argument validation covers scalar and system-specific rules", {
  expect_error(
    fetch_datasus(
      c(2020, 2021),
      year_end = 2022,
      information_system = "SIM-DO"
    ),
    "single whole number"
  )
  expect_error(
    fetch_datasus(
      2020,
      year_end = 2022,
      uf = c("all", "AC"),
      information_system = "SIM-DO"
    ),
    "cannot be combined"
  )
  expect_error(
    fetch_datasus(
      2020,
      year_end = 2022,
      information_system = "SIH-RD"
    ),
    "month_start"
  )
  expect_error(
    fetch_datasus(
      1994,
      month_start = 6,
      year_end = 1994,
      month_end = 7,
      information_system = "SIA-PA"
    ),
    "earliest supported"
  )
  expect_error(
    fetch_datasus(
      2022,
      year_end = 2021,
      information_system = "SIM-DO"
    ),
    "must not be later"
  )
})

test_that("live smoke tests are opt-in", {
  skip_if(
    Sys.getenv("MICRODATASUS_RUN_LIVE_TESTS") != "true",
    "Set MICRODATASUS_RUN_LIVE_TESTS=true to exercise the DataSUS FTP."
  )

  result <- fetch_datasus(
    year_start = 2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SIM-DO",
    vars = "DTOBITO"
  )
  expect_s3_class(result, "data.frame")
})
