create_dictionary_api_fixture <- function() {
  root <- tempfile("dictionary-api-")
  tabdo <- file.path(root, "OBITOS_CID10_TAB", "tabdo")
  dir.create(tabdo, recursive = TRUE)
  write_tabwin_text(
    file.path(tabdo, "Obito_1996_CID10.def"),
    c(
      "Ado*.db?",
      "XTipo obito, TIPOBITO, 1, TIPOBITO.CNV",
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
