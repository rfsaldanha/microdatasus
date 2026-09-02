create_sinasc_tabwin_fixture <- function(legacy = FALSE) {
  root <- tempfile("sinasc-tabwin-fixture-")
  tabwin <- file.path(root, "tabwin")
  dir.create(tabwin, recursive = TRUE)

  if (legacy) {
    definition <- c(
      "Anasc*.db?",
      "XLocal nascimento, LOCAL_OCOR, 1, LOCAL.CNV",
      "XSexo, SEXO, 1, SEXO.CNV",
      "XGestacao, GESTACAO, 1, GESTACAO.CNV",
      "XTipo gravidez, TIPO_GRAV, 1, GRAVIDEZ.CNV"
    )
    definition_name <- "NASC.DEF"
  } else {
    definition <- c(
      "Adn*.db?",
      "XOrigem, ORIGEM, 1, ORIGEM.CNV",
      "XLocal nascimento, LOCNASC, 1, LOCAL.CNV",
      "XSexo, SEXO, 1, SEXO.CNV",
      "XGestacao, GESTACAO, 1, GESTACAO.CNV"
    )
    definition_name <- "NASCIDO.def"
  }
  write_tabwin_text(file.path(tabwin, definition_name), definition)
  write_tabwin_text(
    file.path(tabwin, "ORIGEM.CNV"),
    c(
      "2 1",
      tabwin_cnv_line(1, "Cartorio", "1"),
      tabwin_cnv_line(2, "Estabelecimento", "2")
    )
  )
  write_tabwin_text(
    file.path(tabwin, "LOCAL.CNV"),
    c(
      "3 1",
      tabwin_cnv_line(1, "Hospital", "1"),
      tabwin_cnv_line(2, "Domicilio", "3"),
      tabwin_cnv_line(3, "Ignorado", "9")
    )
  )
  write_tabwin_text(
    file.path(tabwin, "SEXO.CNV"),
    c(
      "3 1",
      tabwin_cnv_line(3, "Ignorado", "0-9"),
      tabwin_cnv_line(1, "Masculino", "1"),
      tabwin_cnv_line(2, "Feminino", "2")
    )
  )
  gestation <- if (legacy) {
    c(
      "6 1",
      tabwin_cnv_line(6, "Ignorado", "0-9"),
      tabwin_cnv_line(4, "37-41 semanas", "4")
    )
  } else {
    c(
      "11 1 L",
      tabwin_cnv_line(4, "32-36 semanas", "4"),
      tabwin_cnv_line(9, "Ignorado", "0,9")
    )
  }
  write_tabwin_text(
    file.path(tabwin, "GESTACAO.CNV"),
    gestation
  )
  write_tabwin_text(
    file.path(tabwin, "GRAVIDEZ.CNV"),
    c(
      "2 1",
      tabwin_cnv_line(1, "Unica", "1"),
      tabwin_cnv_line(2, "Multipla", "2-3")
    )
  )

  archive <- tempfile(fileext = ".zip")
  zip::zipr(archive, files = "tabwin", root = root)
  unlink(root, recursive = TRUE)
  archive
}

test_that("process_sinasc keeps its established arguments", {
  expect_identical(
    as.pairlist(formals(process_sinasc)[c("data", "municipality_data")]),
    as.pairlist(alist(data = , municipality_data = TRUE))
  )
})

test_that("SINASC registry uses both archives returned by transfer portal", {
  registry <- microdatasus:::.tabwin_registry()

  expect_match(
    registry[["SINASC"]]$url,
    "Arq_Para_Tabulacao_A_Partir_1996.zip",
    fixed = TRUE
  )
  expect_match(
    registry[["SINASC-1994-1995"]]$url,
    "Arq_Para_Tabulacao_Ate_1995.zip",
    fixed = TRUE
  )
  expect_identical(registry[["SINASC"]]$definition, "/NASCIDO.def")
  expect_identical(
    registry[["SINASC-1994-1995"]]$definition,
    "/NASC.DEF"
  )
})

test_that("process_sinasc labels and types the layout from 1996 onward", {
  archive <- create_sinasc_tabwin_fixture()
  on.exit(unlink(archive), add = TRUE)
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
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  messages <- capture_messages({
    result <- process_sinasc(
      data.frame(
        contador = c("1", "2"),
        ORIGEM = c("1", "2"),
        LOCNASC = c("1", "3"),
        DTNASC = c("01012024", "02012024"),
        IDADEMAE = c("25", "99"),
        IDADEPAI = c("99", "00"),
        CONSPRENAT = c("99", "10"),
        MESPRENAT = c("99", "01"),
        SEXO = c("1", "2"),
        PESO = c("3200", "9999"),
        CODMUNRES = c("120020", "120030"),
        stringsAsFactors = FALSE
      ),
      municipality_data = FALSE
    )
  })

  expect_equal(downloads, 1L)
  expect_type(result$contador, "integer")
  expect_s3_class(result$DTNASC, "Date")
  expect_identical(
    as.character(result$DTNASC), c("2024-01-01", "2024-01-02")
  )
  expect_type(result$IDADEMAE, "integer")
  expect_identical(result$IDADEPAI, c(99L, NA_integer_))
  expect_identical(result$CONSPRENAT, c(NA_integer_, 10L))
  expect_identical(result$MESPRENAT, c(NA_integer_, 1L))
  expect_type(result$PESO, "integer")
  expect_true(is.na(result$IDADEMAE[[2L]]))
  expect_true(is.na(result$PESO[[2L]]))
  expect_identical(as.character(result$ORIGEM), c("Cartorio", "Estabelecimento"))
  expect_identical(as.character(result$LOCNASC), c("Hospital", "Domicilio"))
  expect_identical(as.character(result$SEXO), c("Masculino", "Feminino"))
  expect_match(messages, "Cached.+SINASC", all = FALSE)
  expect_match(messages, "Starting SINASC", all = FALSE)
  expect_match(messages, "Finished SINASC", all = FALSE)
})

test_that("process_sinasc labels and types the 1994-1995 layout", {
  archive <- create_sinasc_tabwin_fixture(legacy = TRUE)
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
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  result <- process_sinasc(
    data.frame(
      contador = c("1", "2"),
      DATA_NASC = c("19940101", "19940102"),
      LOCAL_OCOR = c("1", "3"),
      SEXO = c("1", "2"),
      TIPO_GRAV = c("1", "2"),
      IDADE_MAE = c("20", "99"),
      PESO = c("3000", "9999"),
      MUNI_MAE = c("120020", "120030"),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE
  )

  expect_s3_class(result$DATA_NASC, "Date")
  expect_identical(
    as.character(result$DATA_NASC), c("1994-01-01", "1994-01-02")
  )
  expect_type(result$contador, "integer")
  expect_type(result$IDADE_MAE, "integer")
  expect_type(result$PESO, "integer")
  expect_true(is.na(result$IDADE_MAE[[2L]]))
  expect_true(is.na(result$PESO[[2L]]))
  expect_identical(
    as.character(result$LOCAL_OCOR),
    c("Hospital", "Domicilio")
  )
  expect_identical(as.character(result$SEXO), c("Masculino", "Feminino"))
  expect_identical(as.character(result$TIPO_GRAV), c("Unica", "Multipla"))
})

test_that("process_sinasc applies period-specific dictionaries by row", {
  modern_archive <- create_sinasc_tabwin_fixture()
  legacy_archive <- create_sinasc_tabwin_fixture(legacy = TRUE)
  on.exit(unlink(c(modern_archive, legacy_archive)), add = TRUE)
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      downloads <<- downloads + 1L
      archive <- if (grepl("Ate_1995", url, fixed = TRUE)) {
        legacy_archive
      } else {
        modern_archive
      }
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  result <- process_sinasc(
    data.frame(
      DATA_NASC = c("19940101", NA),
      DTNASC = c(NA, "01012024"),
      LOCAL_OCOR = c("1", NA),
      LOCNASC = c(NA, "1"),
      SEXO = c("1", "1"),
      GESTACAO = c("4", "4"),
      PESO = c("8000", "8000"),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE
  )

  expect_equal(downloads, 2L)
  expect_true(is.factor(result$GESTACAO))
  expect_identical(
    as.character(result$GESTACAO),
    c("37-41 semanas", "32-36 semanas")
  )
  expect_identical(
    as.character(result$DATA_NASC),
    c("1994-01-01", NA)
  )
  expect_identical(
    as.character(result$DTNASC),
    c(NA, "2024-01-01")
  )
  expect_identical(result$PESO, c(NA_integer_, 8000L))
})

test_that("process_sinasc enforces the official birth-weight domain", {
  result <- process_sinasc(
    data.frame(
      DTNASC = rep("01012024", 6L),
      PESO = c("8999", "9000", "9998", "0", "9999", "bad"),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE,
    labels = "none",
    diagnostics = TRUE
  )

  expect_identical(
    result$PESO,
    c(8999L, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
  )
  report <- processing_diagnostics(result)
  failures <- report$coercion_failures
  failures <- failures[failures$field == "PESO", , drop = FALSE]
  expect_setequal(failures$value, c("9000", "9998", "bad"))
})

test_that("process_sinasc enforces current numeric CNV domains", {
  result <- process_sinasc(
    data.frame(
      DTNASC = rep("01012024", 2L),
      APGAR1 = c("10", "11"),
      IDADEMAE = c("70", "71"),
      IDADEPAI = c("99", "09"),
      QTDGESTANT = c("30", "31"),
      QTDPARTNOR = c("30", "31"),
      QTDPARTCES = c("30", "31"),
      SEMAGESTAC = c("50", "51"),
      MESPRENAT = c("09", "10"),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE,
    labels = "none",
    diagnostics = TRUE
  )

  fields <- setdiff(names(result), "DTNASC")
  first_valid <- vapply(
    result[fields], function(x) !is.na(x[[1L]]), logical(1)
  )
  second_missing <- vapply(
    result[fields], function(x) is.na(x[[2L]]), logical(1)
  )
  expect_true(all(first_valid))
  expect_true(all(second_missing))
  failures <- processing_diagnostics(result)$coercion_failures
  expect_setequal(failures$field, fields)
})

test_that("process_sinasc enforces legacy numeric CNV domains", {
  result <- process_sinasc(
    data.frame(
      DATA_NASC = c("19940101", "19940102"),
      APGAR5 = c("10", "11"),
      IDADE_MAE = c("59", "60"),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE,
    labels = "none",
    diagnostics = TRUE
  )

  expect_identical(result$APGAR5, c(10L, NA_integer_))
  expect_identical(result$IDADE_MAE, c(59L, NA_integer_))
  failures <- processing_diagnostics(result)$coercion_failures
  expect_setequal(failures$field, c("APGAR5", "IDADE_MAE"))
})
