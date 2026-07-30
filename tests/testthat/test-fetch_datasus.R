test_that("fetch_datasus public signature remains compatible", {
  expect_identical(
    as.pairlist(formals(fetch_datasus)[c("year_start", "month_start", "year_end", "month_end", "uf", "information_system", "vars", "stop_on_error", "timeout", "track_source", "quiet")]),
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
      track_source = FALSE,
      quiet = FALSE
    ))
  )
})

test_that("registry contains every supported system", {
  registry <- microdatasus:::.datasus_registry()

  expect_length(registry, 93L)
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
      microdatasus:::.sinan_information_systems()
    )
  )
})

test_that("information-system lookup covers the complete download registry", {
  registry <- microdatasus:::.datasus_registry()
  metadata <- microdatasus:::.datasus_information_system_metadata()
  lookup <- datasus_information_systems()

  expect_identical(formals(datasus_information_systems), pairlist())
  expect_s3_class(lookup, "tbl_df")
  expect_identical(
    names(lookup),
    c(
      "information_system", "system", "name", "file_acronym",
      "periodicity", "geography", "minimum_date", "aliases"
    )
  )
  expect_equal(nrow(lookup), 93L)
  expect_identical(lookup$information_system, names(registry))
  expect_setequal(metadata$information_system, names(registry))
  expect_identical(anyDuplicated(lookup$information_system), 0L)
  expect_true(all(nzchar(lookup$name)))
  expect_true(all(nzchar(lookup$file_acronym)))
  expect_s3_class(lookup$minimum_date, "Date")
  expect_identical(
    unname(lookup$periodicity),
    unname(vapply(registry, `[[`, character(1), "granularity"))
  )
  expect_identical(
    unname(lookup$geography),
    unname(vapply(registry, `[[`, character(1), "geography"))
  )
  expect_identical(
    unname(lookup$minimum_date),
    as.Date(unname(vapply(
      registry,
      function(spec) as.character(spec$minimum),
      character(1)
    )))
  )
  expect_identical(
    as.integer(table(lookup$system)),
    c(13L, 12L, 4L, 5L, 58L, 1L)
  )
  expect_true(all(lengths(lookup$aliases[lookup$system != "SINAN"]) == 0L))

  expect_identical(
    lookup$name[lookup$information_system == "SIM-DO"],
    "Declarações de óbito"
  )
  expect_identical(
    lookup$name[lookup$information_system == "SIH-RD"],
    "AIH reduzida"
  )
  expect_identical(
    lookup$name[lookup$information_system == "SIA-PA"],
    "Produção ambulatorial"
  )
  expect_identical(
    lookup$name[lookup$information_system == "CNES-ST"],
    "Estabelecimentos"
  )
})

test_that("every registry entry has a complete and valid specification", {
  registry <- microdatasus:::.datasus_registry()

  for (system in names(registry)) {
    spec <- registry[[system]]
    info <- paste("Invalid registry specification for", system)

    expect_true(
      spec$granularity %in% c("year", "month"),
      info = info
    )
    expect_true(
      spec$geography %in% c("state", "national"),
      info = info
    )
    expect_true(inherits(spec$minimum, "Date"), info = info)
    expect_false(is.na(spec$minimum), info = info)
    expect_true(length(spec$repositories) >= 1L, info = info)

    for (repository in spec$repositories) {
      expect_match(repository$url, "^ftp://.+/$", info = info)
      expect_true(
        repository$release %in% c(
          "final", "preliminary", "current", "old"
        ),
        info = info
      )
      expect_true(
        is.numeric(repository$priority) &&
          length(repository$priority) == 1L &&
          repository$priority > 0,
        info = info
      )
      expect_true(
        is.character(repository$prefix) &&
          length(repository$prefix) == 1L &&
          nzchar(repository$prefix),
        info = info
      )
    }
  }
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

  # National SIM subsets share one FTP directory but have distinct prefixes.
  for (prefix in c("DOFET", "DOEXT", "DOINF", "DOMAT")) {
    information_system <- paste0("SIM-", prefix)
    national_sim <- microdatasus:::.datasus_parse_listing(
      paste0(prefix, "24.dbc\n"),
      registry[[information_system]]$repositories[[1L]],
      registry[[information_system]]
    )
    expect_equal(national_sim$file, paste0(prefix, "24.dbc"))
    expect_equal(national_sim$period, "2024")
    expect_true(is.na(national_sim$uf))
  }

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

test_that("listing parser handles empty, unmatched and two-digit-year input", {
  registry <- microdatasus:::.datasus_registry()
  repository <- registry[["SINAN-DENGUE"]]$repositories[[1L]]
  spec <- registry[["SINAN-DENGUE"]]

  expect_equal(
    nrow(microdatasus:::.datasus_parse_listing("", repository, spec)),
    0L
  )
  expect_equal(
    nrow(microdatasus:::.datasus_parse_listing(
      "README.txt\nOTHER99.dbc\n",
      repository,
      spec
    )),
    0L
  )

  parsed <- microdatasus:::.datasus_parse_listing(
    "DENGBR99.dbc\nDENGBR01.dbc\n",
    repository,
    spec
  )
  expect_equal(parsed$period, c("1999", "2001"))
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

test_that("manifest precedence works across distinct current and old URLs", {
  spec <- list(
    granularity = "year",
    geography = "national",
    year_digits = 2L,
    repositories = list(
      list(
        url = "ftp://example/current/",
        release = "current",
        priority = 1L,
        prefix = "DENGBR"
      ),
      list(
        url = "ftp://example/old/",
        release = "old",
        priority = 3L,
        prefix = "DENGBR"
      )
    )
  )
  calls <- character()
  local_mocked_bindings(
    .datasus_list_directory = function(url, timeout) {
      calls <<- c(calls, url)
      "DENGBR22.dbc\nDENGBR22a.dbc\n"
    },
    .package = "microdatasus"
  )

  result <- microdatasus:::.datasus_build_manifest(
    spec,
    periods = "2022",
    ufs = "all",
    timeout = 2
  )

  expect_setequal(calls, c(
    "ftp://example/current/",
    "ftp://example/old/"
  ))
  expect_equal(result$manifest$file, c("DENGBR22.dbc", "DENGBR22a.dbc"))
  expect_equal(result$manifest$release, c("current", "current"))
  expect_equal(result$manifest$fragment, c("", "A"))
})

test_that("manifest preserves partial listing failures", {
  spec <- list(
    granularity = "year",
    geography = "national",
    year_digits = 2L,
    repositories = list(
      list(
        url = "ftp://example/good/",
        release = "final",
        priority = 1L,
        prefix = "DENGBR"
      ),
      list(
        url = "ftp://example/bad/",
        release = "preliminary",
        priority = 2L,
        prefix = "DENGBR"
      )
    )
  )
  local_mocked_bindings(
    .datasus_list_directory = function(url, timeout) {
      if (grepl("/bad/", url, fixed = TRUE)) {
        stop("listing failed")
      }
      "DENGBR22.dbc\n"
    },
    .package = "microdatasus"
  )

  partial <- microdatasus:::.datasus_build_manifest(
    spec,
    periods = "2022",
    ufs = "all",
    timeout = 2
  )
  empty <- microdatasus:::.datasus_build_manifest(
    spec,
    periods = "2021",
    ufs = "all",
    timeout = 2
  )

  expect_equal(nrow(partial$manifest), 1L)
  expect_match(partial$errors, "bad.+listing failed")
  expect_equal(nrow(empty$manifest), 0L)
  expect_match(empty$errors, "bad.+listing failed")
})

test_that("manifest returns cleanly when every listing fails", {
  registry <- microdatasus:::.datasus_registry()
  local_mocked_bindings(
    .datasus_list_directory = function(url, timeout) {
      stop("server unavailable")
    },
    .package = "microdatasus"
  )

  result <- microdatasus:::.datasus_build_manifest(
    registry[["SIM-DO"]],
    periods = "2022",
    ufs = "AC",
    timeout = 2
  )

  expect_equal(nrow(result$manifest), 0L)
  expect_length(result$errors, 2L)
  expect_true(all(grepl("server unavailable", result$errors)))
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

test_that("retry exhausts three attempts for transient curl errors", {
  attempts <- 0L
  waits <- numeric()
  transient <- structure(
    list(message = "temporary network failure", call = NULL),
    class = c(
      "curl_error_recv_error",
      "curl_error",
      "error",
      "condition"
    )
  )
  local_mocked_bindings(
    .datasus_retry_wait = function(seconds) waits <<- c(waits, seconds),
    .package = "microdatasus"
  )

  expect_error(
    microdatasus:::.datasus_retry(
      function() {
        attempts <<- attempts + 1L
        stop(transient)
      },
      retry_if = microdatasus:::.datasus_is_transient_curl_error
    ),
    "temporary network failure"
  )

  expect_equal(attempts, 3L)
  expect_equal(waits, c(1, 2))
  expect_false(
    microdatasus:::.datasus_is_transient_curl_error(
      simpleError("not curl")
    )
  )
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

test_that("curl helpers list and transfer local files", {
  source <- tempfile(fileext = ".txt")
  destination <- tempfile(fileext = ".dbc")
  on.exit(unlink(c(source, destination)), add = TRUE)
  payload <- "DOAC2022.dbc\nDOAC2023.dbc\n"
  writeBin(charToRaw(payload), source)
  normalized <- normalizePath(source, winslash = "/", mustWork = TRUE)
  url <- if (.Platform$OS.type == "windows") {
    paste0("file:///", normalized)
  } else {
    paste0("file://", normalized)
  }

  listing <- microdatasus:::.datasus_list_directory(url, timeout = 1.5)
  downloaded <- microdatasus:::.datasus_download_file(
    url,
    destination,
    timeout = 1.5,
    quiet = TRUE
  )

  expect_equal(listing, payload)
  expect_identical(downloaded, destination)
  expect_equal(
    readBin(destination, "raw", n = file.info(destination)$size),
    charToRaw(payload)
  )
})

test_that("download validates empty files without retrying them", {
  attempts <- 0L
  seen_timeout <- NULL
  destination <- tempfile()
  on.exit(unlink(destination), add = TRUE)
  local_mocked_bindings(
    .datasus_transfer_file = function(url, destination, timeout, quiet) {
      attempts <<- attempts + 1L
      seen_timeout <<- timeout
      expect_false(quiet)
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

test_that("transfers display curl download progress by default", {
  expect_identical(
    formals(microdatasus:::.datasus_transfer_file)$quiet,
    FALSE
  )
})

test_that("CLI diagnostic bullets preserve external braces", {
  bullets <- microdatasus:::.datasus_cli_bullets(
    c("one: timeout {code}", "two: empty")
  )

  expect_named(bullets, c("x", "x"))
  expect_equal(
    unname(bullets),
    c("one: timeout {{code}}", "two: empty")
  )
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
    downloader <- function(url, destination, timeout, quiet) {
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

test_that("fetch passes quiet to every download", {
  seen_quiet <- logical()
  mock_fetch_dependencies(
    mock_manifest(c("one.dbc", "two.dbc")),
    downloader = function(url, destination, timeout, quiet) {
      seen_quiet <<- c(seen_quiet, quiet)
      writeBin(charToRaw(url), destination)
      invisible(destination)
    }
  )

  fetch_datasus(
    2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SIM-DO",
    quiet = TRUE
  )

  expect_identical(seen_quiet, c(TRUE, TRUE))
})

test_that("quiet suppresses status messages and per-file announcements", {
  mock_fetch_dependencies(mock_manifest(c("one.dbc", "two.dbc")))

  visible <- testthat::capture_messages(fetch_datasus(
    2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SIM-DO",
    quiet = FALSE
  ))
  silent <- testthat::capture_messages(fetch_datasus(
    2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SIM-DO",
    quiet = TRUE
  ))

  visible <- paste(visible, collapse = "\n")
  expect_match(visible, "Downloading \\[1/2\\] 'one[.]dbc'")
  expect_match(visible, "Downloading \\[2/2\\] 'two[.]dbc'")
  expect_match(visible, "Reading \\[1/2\\] 'one[.]dbc'")
  expect_match(visible, "Reading \\[2/2\\] 'two[.]dbc'")
  expect_match(visible, "Downloaded and read 2 of 2 DataSUS files")
  expect_length(silent, 0L)
})

test_that("ignored-month alerts precede downloads and use CLI formatting", {
  manifest <- mock_manifest("DENGBR22.dbc")
  manifest$uf <- NA_character_
  manifest$release <- "preliminary"
  mock_fetch_dependencies(manifest)

  alerts <- testthat::capture_messages(fetch_datasus(
    2022,
    month_start = 1,
    year_end = 2022,
    month_end = 12,
    uf = "all",
    information_system = "SINAN-DENGUE",
    quiet = TRUE
  ))
  single_alert <- testthat::capture_messages(fetch_datasus(
    2022,
    month_start = 1,
    year_end = 2022,
    uf = "all",
    information_system = "SINAN-DENGUE",
    quiet = TRUE
  ))
  visible <- testthat::capture_messages(fetch_datasus(
    2022,
    month_start = 1,
    year_end = 2022,
    month_end = 12,
    uf = "all",
    information_system = "SINAN-DENGUE",
    quiet = FALSE
  ))

  expect_length(alerts, 1L)
  expect_match(
    alerts,
    "`month_start` and `month_end` are ignored because \"SINAN-DENGUE\" uses annual files"
  )
  expect_length(single_alert, 1L)
  expect_match(
    single_alert,
    "`month_start` is ignored because \"SINAN-DENGUE\" uses annual files"
  )
  expect_lt(
    grep("are ignored because", visible),
    grep("Downloading \\[1/1\\]", visible)
  )
})

test_that("national systems warn when a specific state is ignored", {
  manifest <- mock_manifest("DENGBR22.dbc")
  manifest$uf <- NA_character_
  mock_fetch_dependencies(manifest)

  alerts <- testthat::capture_messages(fetch_datasus(
    2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SINAN-DENGUE",
    quiet = TRUE
  ))

  expect_length(alerts, 1L)
  expect_match(
    alerts,
    "`uf` is ignored because \"SINAN-DENGUE\" publishes national files[.]"
  )
})

test_that("fetch_datasus accepts SINAN acronym aliases silently", {
  manifest <- mock_manifest("TUBEBR22.dbc")
  manifest$uf <- NA_character_
  mock_fetch_dependencies(manifest)

  expect_no_warning(result <- fetch_datasus(
    2022,
    year_end = 2022,
    uf = "all",
    information_system = "SINAN-TUBE",
    quiet = TRUE
  ))
  expect_s3_class(result, "tbl_df")
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
    downloader = function(url, destination, timeout, quiet) {
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
      information_system = "SIM-DO",
      quiet = TRUE
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
    downloader = function(url, destination, timeout, quiet) {
      stop("network failure")
    }
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
      2022,
      year_end = 2022,
      information_system = "SIM-DO",
      quiet = NA
    ),
    "quiet.*TRUE.*FALSE"
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

test_that("argument validation rejects malformed public values", {
  base <- list(
    year_start = 2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SIM-DO",
    quiet = TRUE
  )
  cases <- list(
    list(
      label = "unknown system",
      args = list(information_system = "UNKNOWN"),
      regexp = "supported systems"
    ),
    list(
      label = "multiple systems",
      args = list(information_system = c("SIM-DO", "SINASC")),
      regexp = "supported systems"
    ),
    list(
      label = "fractional year",
      args = list(year_end = 2022.5),
      regexp = "whole number"
    ),
    list(
      label = "zero timeout",
      args = list(timeout = 0),
      regexp = "single number"
    ),
    list(
      label = "infinite timeout",
      args = list(timeout = Inf),
      regexp = "single number"
    ),
    list(
      label = "non-scalar stop_on_error",
      args = list(stop_on_error = c(TRUE, FALSE)),
      regexp = "stop_on_error.*TRUE.*FALSE"
    ),
    list(
      label = "missing track_source",
      args = list(track_source = NA),
      regexp = "track_source.*TRUE.*FALSE"
    ),
    list(
      label = "non-logical quiet",
      args = list(quiet = "yes"),
      regexp = "quiet.*TRUE.*FALSE"
    ),
    list(
      label = "empty states",
      args = list(uf = character()),
      regexp = "valid two-letter state codes"
    ),
    list(
      label = "unknown state",
      args = list(uf = "XX"),
      regexp = "valid two-letter state codes"
    ),
    list(
      label = "missing state",
      args = list(uf = NA_character_),
      regexp = "valid two-letter state codes"
    ),
    list(
      label = "duplicated states",
      args = list(uf = c("AC", "AC")),
      regexp = "duplicated states"
    ),
    list(
      label = "empty vars",
      args = list(vars = character()),
      regexp = "non-empty names"
    ),
    list(
      label = "missing vars",
      args = list(vars = c("id", NA_character_)),
      regexp = "non-empty names"
    ),
    list(
      label = "duplicated vars",
      args = list(vars = c("id", "id")),
      regexp = "duplicated names"
    ),
    list(
      label = "non-character vars",
      args = list(vars = 1),
      regexp = "non-empty names"
    )
  )

  for (case in cases) {
    expect_error(
      do.call(
        fetch_datasus,
        utils::modifyList(base, case$args, keep.null = TRUE)
      ),
      case$regexp,
      info = case$label
    )
  }
})

test_that("monthly argument validation covers missing and invalid months", {
  base <- list(
    year_start = 2022,
    month_start = 1,
    year_end = 2022,
    month_end = 1,
    uf = "AC",
    information_system = "SIH-RD",
    quiet = TRUE
  )
  cases <- list(
    list(
      label = "missing end month",
      args = list(month_end = NULL),
      regexp = "month_end.*required"
    ),
    list(
      label = "zero start month",
      args = list(month_start = 0),
      regexp = "month_start.*between 1 and 12"
    ),
    list(
      label = "month above twelve",
      args = list(month_end = 13),
      regexp = "month_end.*between 1 and 12"
    ),
    list(
      label = "non-scalar month",
      args = list(month_start = c(1, 2)),
      regexp = "month_start.*whole number"
    )
  )

  for (case in cases) {
    expect_error(
      do.call(
        fetch_datasus,
        utils::modifyList(base, case$args, keep.null = TRUE)
      ),
      case$regexp,
      info = case$label
    )
  }
})

test_that("monthly requests preserve periods, multiple states and timeout", {
  manifest <- mock_manifest(
    c(
      "RDSP2401.dbc", "RDAC2401.dbc",
      "RDSP2402.dbc", "RDAC2402.dbc"
    ),
    c("2401", "2401", "2402", "2402")
  )
  manifest$uf <- c("SP", "AC", "SP", "AC")
  manifest$release <- "current"
  seen <- NULL
  local_mocked_bindings(
    .datasus_build_manifest = function(spec, periods, ufs, timeout) {
      seen <<- list(periods = periods, ufs = ufs, timeout = timeout)
      list(manifest = manifest, errors = character())
    },
    .datasus_download_file = function(url, destination, timeout, quiet) {
      writeBin(charToRaw(url), destination)
      invisible(destination)
    },
    read_dbc = function(file, as_character = TRUE) {
      marker <- rawToChar(readBin(file, "raw", n = file.info(file)$size))
      tibble::tibble(file = sub("mock://", "", marker))
    },
    .package = "microdatasus"
  )

  result <- fetch_datasus(
    2024,
    month_start = 1,
    year_end = 2024,
    month_end = 2,
    uf = c("SP", "AC"),
    information_system = "SIH-RD",
    timeout = 9.5,
    quiet = TRUE
  )

  expect_equal(seen$periods, c("2401", "2402"))
  expect_equal(seen$ufs, c("SP", "AC"))
  expect_equal(seen$timeout, 9.5)
  expect_equal(result$file, manifest$file)
})

test_that("all expands to every state and reports truncated missing keys", {
  mock_fetch_dependencies(mock_manifest("DOAC2022.dbc"))
  result <- NULL

  warnings <- testthat::capture_warnings(
    result <- fetch_datasus(
      2022,
      year_end = 2022,
      uf = "all",
      information_system = "SIM-DO",
      quiet = TRUE
    )
  )

  expect_s3_class(result, "data.frame")
  expect_true(any(grepl(
    "26 requested state-period combinations",
    warnings
  )))
  expect_true(any(grepl("and 16 more", warnings)))
})

test_that("missing periods and state-period combinations are consolidated", {
  mock_fetch_dependencies(mock_manifest("DOAC2022.dbc"))
  result <- NULL

  warnings <- testthat::capture_warnings(
    result <- fetch_datasus(
      2022,
      year_end = 2023,
      uf = c("AC", "SP"),
      information_system = "SIM-DO",
      quiet = TRUE
    )
  )

  expect_s3_class(result, "data.frame")
  expect_true(any(grepl("1 requested period unavailable", warnings)))
  expect_true(any(grepl(
    "3 requested state-period combinations",
    warnings
  )))
})

test_that("an empty manifest warns and returns NULL without downloading", {
  downloads <- 0L
  local_mocked_bindings(
    .datasus_build_manifest = function(spec, periods, ufs, timeout) {
      list(manifest = data.frame(), errors = character())
    },
    .datasus_download_file = function(url, destination, timeout, quiet) {
      downloads <<- downloads + 1L
    },
    .package = "microdatasus"
  )
  result <- NULL

  warnings <- testthat::capture_warnings(
    result <- fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO",
      quiet = TRUE
    )
  )

  expect_null(result)
  expect_equal(downloads, 0L)
  expect_true(any(grepl("requested period unavailable", warnings)))
})

test_that("historical releases and partial completion have clear status", {
  manifest <- mock_manifest(c("good.dbc", "bad.dbc"))
  manifest$release <- c("old", "old")
  mock_fetch_dependencies(
    manifest,
    downloader = function(url, destination, timeout, quiet) {
      if (grepl("bad", url, fixed = TRUE)) {
        stop("network failure")
      }
      writeBin(charToRaw(url), destination)
      invisible(destination)
    }
  )
  result <- NULL

  messages <- testthat::capture_messages(
    expect_warning(
      result <- fetch_datasus(
        2022,
        year_end = 2022,
        uf = "AC",
        information_system = "SIM-DO",
        quiet = FALSE
      ),
      "could not be processed"
    )
  )
  messages <- paste(messages, collapse = "\n")

  expect_equal(nrow(result), 1L)
  expect_match(messages, "Using historical data for 1 period")
  expect_match(messages, "Downloaded and read 1 of 2 DataSUS files")
})

test_that("zero-row files are processed but return NULL", {
  mock_fetch_dependencies(
    mock_manifest("empty-rows.dbc"),
    reader = function(file, as_character = TRUE) {
      tibble::tibble(id = character())
    }
  )
  result <- NULL

  messages <- testthat::capture_messages(
    result <- fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO",
      quiet = FALSE
    )
  )

  expect_null(result)
  expect_true(any(grepl(
    "Downloaded and read 1 of 1 DataSUS file",
    messages
  )))
})

test_that("files with different columns combine deterministically", {
  mock_fetch_dependencies(
    mock_manifest(c("first.dbc", "second.dbc")),
    reader = function(file, as_character = TRUE) {
      marker <- rawToChar(readBin(file, "raw", n = file.info(file)$size))
      if (grepl("first", marker, fixed = TRUE)) {
        tibble::tibble(id = "first", a = "A")
      } else {
        tibble::tibble(id = "second", b = "B")
      }
    }
  )

  result <- fetch_datasus(
    2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SIM-DO",
    quiet = TRUE
  )

  expect_named(result, c("id", "a", "b"))
  expect_equal(result$id, c("first", "second"))
  expect_equal(result$a, c("A", NA_character_))
  expect_equal(result$b, c(NA_character_, "B"))
})

test_that("live smoke tests are opt-in", {
  skip_on_cran()
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


test_that("persistent DBC cache reuses files and records provenance", {
  downloads <- 0L
  mock_fetch_dependencies(
    mock_manifest("DOAC2022.dbc"),
    downloader = function(url, destination, timeout, quiet) {
      downloads <<- downloads + 1L
      writeBin(charToRaw(url), destination)
      invisible(destination)
    }
  )
  cache <- tempfile("microdatasus-cache-")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  first <- fetch_datasus(
    2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SIM-DO",
    cache_dir = cache,
    provenance = TRUE,
    quiet = TRUE
  )
  second <- fetch_datasus(
    2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SIM-DO",
    cache_dir = cache,
    provenance = TRUE,
    quiet = TRUE
  )

  expect_equal(downloads, 1L)
  expect_identical(first, second, ignore_attr = TRUE)
  expect_false(datasus_provenance(first)$cached)
  expect_true(datasus_provenance(second)$cached)
  expect_equal(nrow(datasus_cache_info(cache)), 1L)
})

test_that("non-collecting fetch writes one RDS file per DBC", {
  mock_fetch_dependencies(mock_manifest(c("one.dbc", "two.dbc")))
  destination <- tempfile("microdatasus-output-")
  on.exit(unlink(destination, recursive = TRUE), add = TRUE)

  manifest <- fetch_datasus(
    2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SIM-DO",
    destination = destination,
    collect = FALSE,
    quiet = TRUE
  )

  expect_s3_class(manifest, "tbl_df")
  expect_equal(nrow(manifest), 2L)
  expect_true(all(file.exists(manifest$data_path)))
  expect_identical(readRDS(manifest$data_path[[1L]])$value, "x")
})

test_that("fetch can process each file before selecting variables", {
  mock_fetch_dependencies(
    mock_manifest("DOAC2022.dbc"),
    reader = function(file, as_character = TRUE) {
      tibble::tibble(IDADE = "402")
    }
  )

  result <- fetch_datasus(
    2022,
    year_end = 2022,
    uf = "AC",
    information_system = "SIM-DO",
    vars = "IDADEanos",
    process = TRUE,
    process_args = list(municipality_data = FALSE, diagnostics = TRUE),
    quiet = TRUE
  )

  expect_named(result, "IDADEanos")
  expect_identical(result$IDADEanos, 2L)
  expect_length(processing_diagnostics(result)$files, 1L)
})

test_that("scalable fetch arguments are validated", {
  mock_fetch_dependencies(mock_manifest("DOAC2022.dbc"))

  expect_error(
    fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO",
      collect = FALSE
    ),
    "destination"
  )
  expect_error(
    fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO",
      process_args = list(data = "replacement")
    ),
    "cannot replace"
  )
  expect_error(
    fetch_datasus(
      2022,
      year_end = 2022,
      uf = "AC",
      information_system = "SIM-DO",
      process_args = structure(list(FALSE), names = "")
    ),
    "unique, non-empty names"
  )
})
