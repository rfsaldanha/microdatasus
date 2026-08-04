create_dictionary_api_fixture <- function(large_range = FALSE) {
  root <- tempfile("dictionary-api-")
  tabdo <- file.path(root, "OBITOS_CID10_TAB", "tabdo")
  dir.create(tabdo, recursive = TRUE)
  write_tabwin_text(
    file.path(tabdo, "Obito_1996_CID10.def"),
    c(
      "Ado*.db?",
      "XTipo obito, TIPOBITO, 1, TIPOBITO.CNV",
      if (large_range) "XNumero DO, NUMERODO, 1, LARGE.CNV",
      "IContador, CONTADOR"
    )
  )
  write_tabwin_text(
    file.path(tabdo, "TIPOBITO.CNV"),
    c(
      "2 1",
      tabwin_cnv_line(1, "Fetal", "1"),
      tabwin_cnv_line(2, "Nao fetal", "2")
    )
  )
  if (large_range) {
    write_tabwin_text(
      file.path(tabdo, "LARGE.CNV"),
      c(
        "1 8",
        tabwin_cnv_line(1, "Faixa analitica", "00000000-89999999")
      )
    )
  }
  archive <- tempfile(fileext = ".zip")
  zip::zipr(archive, files = "OBITOS_CID10_TAB", root = root)
  unlink(root, recursive = TRUE)
  archive
}

test_that("persistent TabWin cache survives the in-memory cache", {
  archive <- create_dictionary_api_fixture()
  cache <- tempfile("tabwin-persistent-")
  on.exit(unlink(c(archive, cache), recursive = TRUE), add = TRUE)
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      downloads <<- downloads + 1L
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(microdatasus:::.tabwin_clear_cache(), add = TRUE)

  first <- fetch_tabwin_dictionary(
    "SIM-DO",
    quiet = TRUE,
    cache_dir = cache
  )
  microdatasus:::.tabwin_clear_cache()
  second <- fetch_tabwin_dictionary(
    "SIM-DO",
    quiet = TRUE,
    cache_dir = cache
  )

  expect_equal(downloads, 1L)
  expect_false(first$cache_hit)
  expect_true(second$cache_hit)
  expect_identical(first$archive_checksum, second$archive_checksum)
  expect_equal(nrow(datasus_cache_info(cache)), 1L)
})

test_that("datasus_variables exposes types and code-label tables", {
  archive <- create_dictionary_api_fixture()
  on.exit(unlink(archive), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(microdatasus:::.tabwin_clear_cache(), add = TRUE)

  variables <- datasus_variables("SIM-DO", quiet = TRUE)

  expect_s3_class(variables, "tbl_df")
  expect_setequal(variables$field, c("TIPOBITO", "CONTADOR"))
  categorical <- variables[variables$field == "TIPOBITO", ]
  expect_identical(
    categorical$labels[[1L]],
    tibble::tibble(
      code = c("1", "2"),
      label = c("Fetal", "Nao fetal")
    )
  )
  expect_identical(variables$type[variables$field == "CONTADOR"], "numeric")
})

test_that("datasus_variables reuses assembled tables within the session", {
  archive <- create_dictionary_api_fixture()
  on.exit(unlink(archive), add = TRUE)
  prefetch <- microdatasus:::.datasus_prefetch_dictionary_relations
  calls <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout, quiet = FALSE) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .datasus_prefetch_dictionary_relations = function(dictionary, definitions) {
      calls <<- calls + 1L
      prefetch(dictionary, definitions)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(microdatasus:::.tabwin_clear_cache(), add = TRUE)

  first <- datasus_variables("SIM-DO", quiet = TRUE)
  second <- datasus_variables("SIM-DO", quiet = TRUE)

  expect_identical(second, first)
  expect_equal(calls, 1L)
})

test_that("datasus_variables exposes non-enumerable analytical ranges", {
  archive <- create_dictionary_api_fixture(large_range = TRUE)
  on.exit(unlink(archive), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(
      url, destination, timeout, quiet = FALSE
    ) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(microdatasus:::.tabwin_clear_cache(), add = TRUE)

  variables <- datasus_variables("SIM-DO", quiet = TRUE)
  analytical <- variables[variables$field == "NUMERODO", ]

  expect_equal(nrow(analytical), 1L)
  expect_equal(nrow(analytical$labels[[1L]]), 0L)
  expect_equal(analytical$categories[[1L]], 0L)
  expect_equal(analytical$range_rules[[1L]], 1L)
  expect_identical(analytical$status[[1L]], "non_enumerable")
  expect_false(analytical$labels_complete[[1L]])
})

test_that("dictionary comparison reports changed labels", {
  archive <- create_dictionary_api_fixture()
  on.exit(unlink(archive), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(microdatasus:::.tabwin_clear_cache(), add = TRUE)
  previous <- datasus_variables("SIM-DO", quiet = TRUE)
  row <- which(previous$field == "TIPOBITO")
  previous$labels[[row]]$label[previous$labels[[row]]$code == "1"] <- "Antigo"

  changes <- compare_datasus_dictionary(
    "SIM-DO",
    previous = previous,
    refresh = TRUE,
    quiet = TRUE
  )

  changed <- changes[changes$field == "TIPOBITO" & changes$code == "1", ]
  expect_equal(nrow(changed), 1L)
  expect_identical(changed$change, "changed")
  expect_identical(changed$before, "Antigo")
  expect_identical(changed$after, "Fetal")
})

test_that("dictionary queries filter fields and provide field contracts", {
  archive <- create_dictionary_api_fixture(large_range = TRUE)
  on.exit(unlink(archive), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout, quiet = FALSE) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(microdatasus:::.tabwin_clear_cache(), add = TRUE)

  variables <- datasus_variables("SIM-DO", fields = "numerodo", quiet = TRUE)
  fields <- datasus_variables("SIM-DO", view = "fields", quiet = TRUE)
  schema <- datasus_schema("SIM-DO", inspect = TRUE, quiet = TRUE)

  expect_error(
    datasus_variables("SIM-DO", fields = "NAO_EXISTE", quiet = TRUE),
    "not declared"
  )
  expect_identical(unique(variables$field), "NUMERODO")
  expect_identical(variables$status, "non_enumerable")
  expect_true(all(c("definitions_count", "definitions") %in% names(fields)))
  expect_setequal(schema$field, c("TIPOBITO", "NUMERODO", "CONTADOR"))
})

test_that("dictionary audit reuses one physical archive for multiple keys", {
  archive <- create_dictionary_api_fixture()
  cache <- tempfile("dictionary-audit-cache-")
  on.exit(unlink(c(archive, cache), recursive = TRUE), add = TRUE)
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout, quiet = FALSE) {
      downloads <<- downloads + 1L
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(microdatasus:::.tabwin_clear_cache(), add = TRUE)

  audit <- audit_datasus_dictionaries(
    c("SIM-DO", "SIM-DOFET"), cache_dir = cache, quiet = TRUE
  )

  expect_equal(nrow(audit), 2L)
  expect_equal(downloads, 1L)
  expect_identical(length(unique(audit$archive_key)), 1L)
  expect_true(all(audit$status == "ok"))
})

test_that("parsed conversions persist across R session-cache resets", {
  archive <- create_dictionary_api_fixture()
  cache <- tempfile("parsed-conversion-cache-")
  on.exit(unlink(c(archive, cache), recursive = TRUE), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout, quiet = FALSE) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(microdatasus:::.tabwin_clear_cache(), add = TRUE)
  first <- datasus_variables("SIM-DO", cache_dir = cache, quiet = TRUE)
  microdatasus:::.tabwin_clear_cache()
  local_mocked_bindings(
    .tabwin_parse_cnv = function(path) stop("parser should not run"),
    .package = "microdatasus"
  )

  second <- datasus_variables("SIM-DO", cache_dir = cache, quiet = TRUE)

  expect_identical(second$labels, first$labels)
  expect_true(any(grepl("[.]rds$", list.files(cache, recursive = TRUE))))
})

test_that("dictionary comparison includes symbolic range changes", {
  archive <- create_dictionary_api_fixture(large_range = TRUE)
  on.exit(unlink(archive), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout, quiet = FALSE) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(microdatasus:::.tabwin_clear_cache(), add = TRUE)
  previous <- datasus_variables("SIM-DO", quiet = TRUE)
  row <- which(previous$field == "NUMERODO")
  previous$ranges[[row]]$label <- "Rotulo anterior"

  changes <- compare_datasus_dictionary(
    "SIM-DO", previous = previous, refresh = TRUE, quiet = TRUE
  )
  changed <- changes[changes$kind == "range", ]

  expect_equal(nrow(changed), 1L)
  expect_identical(changed$before, "Rotulo anterior")
  expect_identical(changed$after, "Faixa analitica")
})

test_that("long parsed-cache keys retain collision-resistant suffixes", {
  dictionary <- list(
    persistent = TRUE, archive_checksum = "checksum", cache_dir = tempdir()
  )
  prefix <- paste(rep("a", 200L), collapse = "")
  first <- microdatasus:::.tabwin_conversion_cache_path(
    dictionary, paste0(prefix, "x")
  )
  second <- microdatasus:::.tabwin_conversion_cache_path(
    dictionary, paste0(prefix, "y")
  )

  expect_false(identical(first, second))
  expect_lte(nchar(basename(first)), 255L)
})

test_that("cache helpers use unique temporaries and clear stale locks", {
  destination <- tempfile("cache-target-")
  first <- microdatasus:::.datasus_temporary_path(destination)
  second <- microdatasus:::.datasus_temporary_path(destination)
  lock <- paste0(destination, ".lock")
  dir.create(lock)
  Sys.setFileTime(lock, Sys.time() - 700)

  value <- microdatasus:::.datasus_with_cache_lock(destination, 42L)

  expect_false(identical(first, second))
  expect_identical(value, 42L)
  expect_false(dir.exists(lock))
})

test_that("clear_datasus_cache preserves unrelated files", {
  cache <- tempfile("cache-root-")
  dir.create(file.path(cache, "dbc"), recursive = TRUE)
  dir.create(file.path(cache, "tabwin"), recursive = TRUE)
  marker <- file.path(cache, "keep.txt")
  file.create(marker)
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)

  clear_datasus_cache(cache)

  expect_true(file.exists(marker))
  expect_false(dir.exists(file.path(cache, "dbc")))
  expect_false(dir.exists(file.path(cache, "tabwin")))
})

test_that("persistent cache validates its checksum", {
  root <- tempfile("cache-integrity-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  payload <- file.path(root, "data.dbc")
  manifest_path <- file.path(root, "manifest.rds")
  writeLines("original", payload)
  manifest <- microdatasus:::.datasus_file_provenance(
    payload,
    "fixture://data"
  )
  saveRDS(manifest, manifest_path)

  expect_true(
    microdatasus:::.datasus_cache_valid(payload, manifest_path)
  )
  writeLines("changed", payload)
  expect_false(
    microdatasus:::.datasus_cache_valid(payload, manifest_path)
  )
})

test_that("dictionary schema and audit validate selections and expose failures", {
  expect_length(microdatasus:::.datasus_validate_system_selection(NULL), 104L)
  expect_error(
    microdatasus:::.datasus_validate_system_selection(NA_character_),
    "character vector"
  )
  expect_error(
    microdatasus:::.datasus_validate_system_selection("NOT-A-SYSTEM"),
    "Unsupported dictionary keys"
  )
  expect_error(
    datasus_schema(c("SIM-DO", "SINASC"), quiet = TRUE),
    "exactly one dictionary"
  )

  local_mocked_bindings(
    datasus_variables = function(...) stop("simulated dictionary failure"),
    .package = "microdatasus"
  )
  audit <- audit_datasus_dictionaries("SIM-DO", quiet = FALSE)

  expect_identical(audit$status, "dictionary_error")
  expect_match(audit$issues[[1L]]$message, "simulated dictionary failure")
  expect_error(
    audit_datasus_dictionaries("SIM-DO", quiet = TRUE, fail_on_error = TRUE),
    "could not be audited"
  )
})
