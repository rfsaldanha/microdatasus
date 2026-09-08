source(test_path("..", "support", "territory-reference.R"), local = TRUE)

test_that("pinned official TXT files exactly reconstruct the packaged municipality table", {
  archive <- system.file("extdata", "territory", "tabmun-source.zip", package = "microdatasus")
  expect_true(file.exists(archive))
  rebuilt <- .rebuild_tabmun(archive)
  expect_identical(rebuilt, tabMun)
  expect_identical(nrow(rebuilt), 5659L)
  expect_identical(anyDuplicated(rebuilt$munResCod), 0L)
  expect_identical(sum(is.na(rebuilt$munResLat)), 28L)
  expect_identical(rebuilt$munResUf[rebuilt$munResCod == 200010L], "Pernambuco")
  expect_identical(as.character(rebuilt$munResTipo[rebuilt$munResCod == 200010L]), "TERRIT")
  expect_identical(rebuilt$munResUf[rebuilt$munResCod == 0L], "")
})

test_that("changed territorial source members are rejected before reconstruction", {
  root <- withr::local_tempdir()
  writeBin(charToRaw("altered source"), file.path(root, "tb_municip.txt"))
  archive <- withr::local_tempfile(fileext = ".zip")
  zip::zipr(archive, "tb_municip.txt", root = root)
  expect_error(.territory_read_member(archive, "tb_municip.txt"),
               "Territorial source checksum differs")
})

test_that("municipal provenance distinguishes source timestamps from territorial validity", {
  refs <- datasus_reference_tables()
  municipal <- refs[refs$table == "tabMun", ]
  expect_identical(municipal$source_version, "datasus-territorio-2023-txt-20220516")
  expect_identical(municipal$source_date, as.Date("2022-05-16"))
  expect_match(municipal$source_date_basis, "not territorial validity", fixed = TRUE)
  expect_match(municipal$source_archive, "/2023/base_territorial_2023.zip", fixed = TRUE)
  expect_identical(municipal$source_archive_sha256,
    "798be2f62a53dd1af8e335a44a1916154f36ee3a3051a7375864a74ef47c3bc4")
  expect_true(all(is.na(refs$source_date[refs$table != "tabMun"])))
  expect_true(all(is.na(refs$source_version[refs$table != "tabMun"])))
  result <- process_sim(data.frame(CODMUNRES = c("110001", "110001", "999999")),
                         labels = "none", diagnostics = TRUE)
  expect_identical(nrow(result), 3L)
  expect_identical(processing_diagnostics(result)$reference_tables$source_version,
                     municipal$source_version)
  expect_identical(result$munResNome[1:2], rep("Alta Floresta D'Oeste", 2))
  expect_true(is.na(result$munResNome[[3L]]))
})
