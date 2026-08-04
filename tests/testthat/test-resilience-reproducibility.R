create_repro_auxiliary_zip <- function(internal_file, data) {
  root <- tempfile("microdatasus-zip-fixture-")
  dir.create(file.path(root, dirname(internal_file)), recursive = TRUE)
  foreign::write.dbf(data, file.path(root, internal_file))
  archive <- tempfile(fileext = ".zip")
  zip::zipr(archive, files = dirname(internal_file), root = root)
  unlink(root, recursive = TRUE)
  archive
}

test_that("new cache manifests use SHA-256 and legacy MD5 remains readable", {
  payload <- tempfile()
  manifest <- tempfile()
  writeBin(charToRaw("microdatasus"), payload)

  current <- microdatasus:::.datasus_file_provenance(
    payload, "fixture://payload"
  )
  expect_identical(current$checksum_algorithm, "sha256")
  expect_match(current$checksum, "^[0-9a-f]{64}$")

  legacy <- current
  legacy$checksum <- unname(tools::md5sum(payload))
  legacy$checksum_algorithm <- NULL
  saveRDS(legacy, manifest)
  expect_true(microdatasus:::.datasus_cache_valid(payload, manifest))
})

test_that("transport mirrors preserve the original URL and resume partial files", {
  withr::local_options(microdatasus.mirrors = c(
    "https://mirror-one.example", "https://mirror-two.example/"
  ))
  candidates <- microdatasus:::.datasus_url_candidates(
    "ftp://ftp.datasus.gov.br/path/file.dbc"
  )
  expect_identical(candidates, c(
    "ftp://ftp.datasus.gov.br/path/file.dbc",
    "https://mirror-one.example/path/file.dbc",
    "https://mirror-two.example/path/file.dbc"
  ))

  destination <- tempfile()
  attempts <- 0L
  local_mocked_bindings(
    .datasus_retry_wait = function(...) invisible(NULL),
    .datasus_transfer_file = function(url, destination, timeout, quiet) {
      attempts <<- attempts + 1L
      if (attempts == 1L) {
        writeBin(charToRaw("abc"), destination)
        error <- simpleError("temporary interruption")
        class(error) <- c("curl_error", class(error))
        stop(error)
      }
      expect_equal(file.size(destination), 3)
      connection <- file(destination, "ab")
      on.exit(close(connection))
      writeBin(charToRaw("def"), connection)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.datasus_download_file(
    "file://fixture", destination, timeout = 1, quiet = TRUE
  )
  expect_identical(readBin(destination, "raw", n = 6), charToRaw("abcdef"))
  expect_identical(attempts, 2L)
})

test_that("row filters validate results and have stable failures", {
  data <- data.frame(id = 1:4)
  expect_identical(
    microdatasus:::.datasus_apply_row_filter(data, function(x) x$id > 2)$id,
    c(3L, 4L)
  )
  expect_error(
    microdatasus:::.datasus_apply_row_filter(data, function(x) TRUE),
    class = "microdatasus_row_filter_error"
  )
  expect_error(
    microdatasus:::.datasus_apply_row_filter(
      data, function(x) stop("filter boom")
    ),
    "filter boom",
    class = "microdatasus_row_filter_error"
  )
})

test_that("lockfiles pin requests, dictionaries and retained source files", {
  payload <- tempfile(fileext = ".dbc")
  writeBin(charToRaw("source dbc"), payload)
  provenance <- tibble::tibble(
    file = "fixture.dbc", url = "fixture://fixture.dbc",
    period = "2022", uf = "AC", release = "final",
    source_rows = 10L, rows = 2L, size = file.size(payload),
    checksum = microdatasus:::.datasus_checksum(payload),
    checksum_algorithm = "sha256", downloaded_at = Sys.time(),
    cached = FALSE, dbc_path = payload, data_path = NA_character_
  )
  result <- tibble::tibble(id = 1:2)
  attr(result, "microdatasus_provenance") <- provenance
  attr(result, "microdatasus_request") <- list(
    information_system = "SIM-DO", year_start = 2022L,
    year_end = 2022L, row_filter = "{ x$id <= 2 }"
  )
  attr(result, "microdatasus_diagnostics") <- structure(
    list(
      dictionaries = tibble::tibble(
        information_system = "SIM-DO", definition = "OBITO.DEF",
        archive_checksum = "dictionary-sha", source = "fixture"
      ),
      reference_tables = tibble::tibble(
        table = "tabMun", checksum = "reference-sha",
        checksum_algorithm = "sha256"
      )
    ),
    class = "microdatasus_processing_diagnostics"
  )

  path <- tempfile(fileext = ".lock.rds")
  lock <- datasus_lockfile(result, path)
  restored <- read_datasus_lockfile(path)
  expect_s3_class(lock, "microdatasus_lockfile")
  expect_identical(restored$request$information_system, "SIM-DO")
  expect_identical(restored$files$checksum_algorithm, "sha256")
  expect_identical(restored$dictionaries$information_system, "SIM-DO")
  expect_identical(verify_datasus_lockfile(restored)$status, "ok")

  writeBin(charToRaw("changed"), payload)
  expect_identical(verify_datasus_lockfile(path)$status, "mismatch")
})

test_that("packaged reference metadata is explicit and normalized", {
  references <- datasus_reference_tables()
  expect_setequal(
    references$table,
    c(
      "tabMun", "tabCBO", "tabNaturalidade", "tabOcupacao",
      "sigtab", "equipe", "paisnet"
    )
  )
  expect_true(all(references$checksum_algorithm == "sha256"))
  expect_true(all(grepl("^[0-9a-f]{64}$", references$checksum)))
  expect_false(any(grepl("\\\\u[0-9A-Fa-f]{4}", tabMun$munResNome)))
  expect_true(any(tabMun$munResNome == "Alta Floresta D'Oeste"))
})

test_that("schema samples every represented dictionary deterministically", {
  data <- data.frame(group = rep(c("old", "current"), each = 10))
  local_mocked_bindings(
    .sia_dictionary_rows = function(data, information_system) {
      split(seq_len(nrow(data)), data$group)
    },
    .package = "microdatasus"
  )
  first <- microdatasus:::.datasus_contract_sample_rows(
    data, "SIA-PA", sample_n = 3L
  )
  second <- microdatasus:::.datasus_contract_sample_rows(
    data, "SIA-PA", sample_n = 3L
  )
  expect_identical(first, second)
  expect_length(first, 6L)
  expect_setequal(unique(data$group[first]), c("old", "current"))
})

test_that("dictionary issues distinguish upstream and internal origins", {
  classified <- function(class) {
    error <- simpleError("fixture")
    class(error) <- c(class, class(error))
    microdatasus:::.datasus_dictionary_issue_class(error)
  }
  expect_identical(
    classified("microdatasus_dictionary_missing_error"),
    "upstream_archive_missing"
  )
  expect_identical(
    classified("microdatasus_dictionary_invalid_error"),
    "upstream_content_invalid"
  )
  expect_identical(
    classified("microdatasus_dictionary_relation_error"),
    "relation_io_or_parser"
  )
})

test_that("auxiliary DBFs reuse a validated persistent archive", {
  archive <- create_repro_auxiliary_zip(
    file.path("DBF", "TABLE.dbf"),
    data.frame(code = "01", label = "One")
  )
  cache <- tempfile("aux-cache-")
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout, quiet) {
      downloads <<- downloads + 1L
      file.copy(archive, destination, overwrite = TRUE)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  first <- microdatasus:::.datasus_fetch_zip_dbf(
    "ftp://example/table.zip", file.path("DBF", "TABLE.dbf"), 10,
    cache_dir = cache, quiet = TRUE, information_system = "TEST"
  )
  second <- microdatasus:::.datasus_fetch_zip_dbf(
    "ftp://example/table.zip", file.path("DBF", "TABLE.dbf"), 10,
    cache_dir = cache, quiet = TRUE, information_system = "TEST"
  )
  expect_identical(downloads, 1L)
  expect_false(datasus_provenance(first)$cached)
  expect_true(datasus_provenance(second)$cached)
  expect_true(any(datasus_cache_info(cache)$type == "auxiliary"))
})

test_that("strict audits fail on known upstream missing relations", {
  local_mocked_bindings(
    datasus_variables = function(information_system, ...) {
      tibble::tibble(
        information_system = information_system,
        definition = "OBITO.DEF", archive_checksum = "fixture",
        field = "SEXO", file = "missing.cnv", range_rules = 0L,
        status = "missing", issue_class = "upstream_archive_missing",
        message = "official relation absent"
      )
    },
    .package = "microdatasus"
  )
  relaxed <- audit_datasus_dictionaries(
    "SIM-DO", quiet = TRUE, fail_on_error = TRUE
  )
  expect_identical(relaxed$status, "missing")
  expect_error(
    audit_datasus_dictionaries(
      "SIM-DO", quiet = TRUE, fail_on_issues = TRUE
    ),
    "missing or invalid"
  )
})

test_that("schema diagnostic counters aggregate sampled failures", {
  report <- list(
    coercion_failures = tibble::tibble(
      field = c("IDADE", "IDADE", "OTHER"), n = c(2L, 3L, 9L)
    ),
    unknown_codes = tibble::tibble(
      field = c("SEXO", "SEXO"), n = c(1L, 4L)
    )
  )
  expect_identical(
    microdatasus:::.datasus_contract_diagnostic_counts(
      report, "coercion_failures", c("IDADE", "SEXO")
    ),
    c(IDADE = 5L, SEXO = 0L)
  )
  expect_identical(
    microdatasus:::.datasus_contract_diagnostic_counts(
      report, "unknown_codes", c("IDADE", "SEXO")
    ),
    c(IDADE = 0L, SEXO = 5L)
  )
})

test_that("municipality joins expose normalized names and reference provenance", {
  source <- data.frame(CODMUNRES = "110001", stringsAsFactors = FALSE)
  collector <- microdatasus:::.process_diagnostic_collector(
    TRUE, "SIM-DO", source
  )
  joined <- microdatasus:::.process_add_municipality_data(
    source, "CODMUNRES", collector
  )
  result <- microdatasus:::.process_finalize(joined, collector)
  report <- processing_diagnostics(result)

  expect_identical(result$munResNome, "Alta Floresta D'Oeste")
  expect_identical(result$munResUf, "Rondônia")
  expect_identical(report$reference_tables$table, "tabMun")
  expect_identical(
    report$reference_tables$role, "municipality_fallback"
  )
})

test_that("lockfile APIs validate malformed, nested and unavailable inputs", {
  expect_error(datasus_lockfile(tibble::tibble()), "No provenance")
  expect_error(read_datasus_lockfile(tempfile()), "does not exist")

  malformed <- tempfile()
  writeBin(charToRaw("not an RDS"), malformed)
  expect_error(read_datasus_lockfile(malformed), "Could not read")
  plain <- tempfile()
  saveRDS(list(), plain)
  expect_error(read_datasus_lockfile(plain), "microdatasus lockfile")

  empty <- structure(
    list(files = data.frame()),
    class = "microdatasus_lockfile"
  )
  expect_equal(nrow(verify_datasus_lockfile(empty)), 0L)

  unavailable <- structure(
    list(files = data.frame(
      file = "old.dbc", checksum = "legacy-md5",
      stringsAsFactors = FALSE
    )),
    class = "microdatasus_lockfile"
  )
  checked <- verify_datasus_lockfile(unavailable)
  expect_identical(checked$checksum_algorithm, "md5")
  expect_identical(checked$status, "unavailable")

  report <- list(files = list(
    first = list(dictionaries = data.frame(
      information_system = "SIM-DO", stringsAsFactors = FALSE
    )),
    second = list(dictionaries = data.frame(
      information_system = "SINASC", stringsAsFactors = FALSE
    ))
  ))
  nested <- microdatasus:::.datasus_lockfile_diagnostics(
    report, "dictionaries"
  )
  expect_setequal(nested$information_system, c("SIM-DO", "SINASC"))
  expect_equal(
    nrow(microdatasus:::.datasus_lockfile_diagnostics(NULL, "dictionaries")),
    0L
  )

  with_provenance <- tibble::tibble(id = 1L)
  attr(with_provenance, "microdatasus_provenance") <- tibble::tibble()
  expect_error(datasus_lockfile(with_provenance, ""), "non-empty path")
})
