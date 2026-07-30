create_sinasc_tabwin_fixture <- function(legacy = FALSE) {
  root <- tempfile("sinasc-tabwin-fixture-")
  tabwin <- file.path(root, "tabwin")
  dir.create(tabwin, recursive = TRUE)

  if (legacy) {
    definition <- c(
      "Anasc*.db?",
      "XLocal nascimento, LOCAL_OCOR, 1, LOCAL.CNV",
      "XSexo, SEXO, 1, SEXO.CNV",
      "XTipo gravidez, TIPO_GRAV, 1, GRAVIDEZ.CNV"
    )
    definition_name <- "NASC.DEF"
  } else {
    definition <- c(
      "Adn*.db?",
      "XOrigem, ORIGEM, 1, ORIGEM.CNV",
      "XLocal nascimento, LOCNASC, 1, LOCAL.CNV",
      "XSexo, SEXO, 1, SEXO.CNV"
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
  expect_type(result$IDADEMAE, "integer")
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
      DATA_NASC = c("01011994", "02011994"),
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
