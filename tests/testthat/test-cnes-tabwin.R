create_cnes_tabwin_fixture <- function() {
  root <- tempfile("cnes-tabwin-fixture-")
  dir.create(file.path(root, "CNV"), recursive = TRUE)
  dir.create(file.path(root, "DBF"), recursive = TRUE)

  definitions <- c(
    "CNES-LT" = "Leitos_Especialidade.def",
    "CNES-ST" = "Estabelecimento.def",
    "CNES-DC" = "DadosComplementares.def",
    "CNES-EQ" = "Equipamento.def",
    "CNES-SR" = "Servico_Especializado_200803_.def",
    "CNES-HB" = "Habilitacao.def",
    "CNES-PF" = "Profissional.def",
    "CNES-EP" = "Equipes.def",
    "CNES-RC" = "Regras_Contratuais.def",
    "CNES-IN" = "Incentivos.def",
    "CNES-EE" = "Estabel_Ensino.def",
    "CNES-EF" = "Estabel_Filantropico.def",
    "CNES-GM" = "Gestao_de_Metas.def"
  )
  for (information_system in names(definitions)) {
    lines <- c(
      "A*.dbc",
      "XFlag, FLAG, 1, CNV/FLAG.CNV",
      "IQuantidade de teste, QT_TEST",
      "DNome fantasia, CNES, FANTASIA, DBF/CADGERBR.DBF"
    )
    if (identical(information_system, "CNES-SR")) {
      lines <- c(
        lines,
        "XServico atual, SERV_ESP, LABEL, DBF/SERVICE_CURRENT.DBF"
      )
    }
    write_tabwin_text(
      file.path(root, unname(definitions[[information_system]])),
      lines
    )
  }
  write_tabwin_text(
    file.path(root, "Servico_Especializado_200508_200802.def"),
    c(
      "A*.dbc",
      "XFlag, FLAG, 1, CNV/FLAG.CNV",
      "IQuantidade de teste, QT_TEST",
      "XServico antigo, SERV_ESP, LABEL, DBF/SERVICE_OLD.DBF",
      "DNome fantasia, CNES, FANTASIA, DBF/CADGERBR.DBF"
    )
  )
  write_tabwin_text(
    file.path(root, "CNV", "FLAG.CNV"),
    c("1 1", tabwin_cnv_line(1, "Rotulo CNES", "1"))
  )
  foreign::write.dbf(
    data.frame(CHAVE = "024103", LABEL = "Servico antigo"),
    file.path(root, "DBF", "SERVICE_OLD.DBF")
  )
  foreign::write.dbf(
    data.frame(CHAVE = "111001", LABEL = "Servico atual"),
    file.path(root, "DBF", "SERVICE_CURRENT.DBF")
  )
  foreign::write.dbf(
    data.frame(CNES = "0000001", FANTASIA = "Unidade teste"),
    file.path(root, "DBF", "CADGERBR.DBF")
  )

  archive <- tempfile(fileext = ".zip")
  zip::zipr(
    archive,
    files = list.files(root),
    root = root
  )
  unlink(root, recursive = TRUE)
  archive
}

test_that("process_cnes preserves all established arguments", {
  expect_identical(
    as.pairlist(formals(process_cnes)[c("data", "information_system", "nomes", "municipality_data")]),
    as.pairlist(alist(
      data = ,
      information_system = c("CNES-ST", "CNES-PF"),
      nomes = FALSE,
      municipality_data = TRUE
    ))
  )
})

test_that("CNES registry covers every downloadable layout and one official ZIP", {
  registry <- microdatasus:::.tabwin_registry()
  expected <- paste0(
    "CNES-",
    c("LT", "ST", "DC", "EQ", "SR", "HB", "PF", "EP", "RC", "IN", "EE", "EF", "GM")
  )
  expect_setequal(microdatasus:::.cnes_information_systems, expected)
  expect_true(all(vapply(
    registry[expected],
    function(spec) identical(basename(spec$url), "TAB_CNES.zip"),
    logical(1)
  )))
  expect_true(all(vapply(
    registry[c(expected, "CNES-SR-2005-08-2008-02")],
    function(spec) identical(spec$archive_key, "CNES-200508"),
    logical(1)
  )))
  expect_identical(
    registry[["CNES-SR-2005-08-2008-02"]]$definition,
    "Servico_Especializado_200508_200802.def"
  )
})

test_that("CNES reference months are identifiers, not full dates", {
  data <- data.frame(
    DT_ATUAL = "202601", DT_ATIVA = "202501", DT_DESAT = "900001",
    DT_ABERTU = "20250131", stringsAsFactors = FALSE
  )
  dictionary <- list(list(
    numeric_fields = character(),
    definitions = data.frame(field = character())
  ))

  types <- microdatasus:::.cnes_type_fields(data, dictionary)

  expect_identical(types$date, "DT_ABERTU")
  expect_setequal(
    intersect(types$identifier, names(data)),
    c("DT_ATUAL", "DT_ATIVA", "DT_DESAT")
  )
})

test_that("all CNES layouts share one archive and label from their DEF", {
  archive <- create_cnes_tabwin_fixture()
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

  for (information_system in microdatasus:::.cnes_information_systems) {
    result <- process_cnes(
      data.frame(FLAG = "1"),
      information_system = information_system,
      municipality_data = FALSE
    )
    expect_identical(as.character(result$FLAG), "Rotulo CNES")
  }
  expect_equal(downloads, 1L)
})

test_that("process_cnes standardizes types, identifiers, names, and messages", {
  archive <- create_cnes_tabwin_fixture()
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

  source <- data.frame(
    CNES = factor("0000001"),
    CODUFMUN = "1200209",
    CPF_CNPJ = "00123456000100",
    COMPETEN = "202606",
    DTPORTAR = "31/01/2024",
    QT_TEST = "3",
    FLAG = "1",
    MEASURE = 1.5,
    stringsAsFactors = FALSE
  )
  messages <- capture_messages({
    result <- process_cnes(
      source,
      information_system = "CNES-ST",
      nomes = TRUE,
      municipality_data = FALSE
    )
  })

  expect_identical(result$CNES, "0000001")
  expect_identical(result$CODUFMUN, "120020")
  expect_identical(result$CPF_CNPJ, "00123456000100")
  expect_identical(result$COMPETEN, "202606")
  expect_s3_class(result$DTPORTAR, "Date")
  expect_type(result$QT_TEST, "integer")
  expect_type(result$MEASURE, "double")
  expect_s3_class(result$FLAG, "factor")
  expect_identical(as.character(result$FLAG), "Rotulo CNES")
  expect_identical(result$FANTASIA, "Unidade teste")
  expect_lt(
    which(grepl("Cached.+CNES-ST", messages)),
    which(grepl("Starting CNES-ST", messages))
  )
  expect_lt(
    which(grepl("Starting CNES-ST", messages)),
    which(grepl("Finished CNES-ST", messages))
  )
})

test_that("process_cnes selects both service definitions row by row", {
  archive <- create_cnes_tabwin_fixture()
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

  result <- process_cnes(
    data.frame(
      COMPETEN = c("200802", "200803"),
      SERV_ESP = c("024", "111"),
      CLASS_SR = c("103", "001"),
      FLAG = c("1", "1")
    ),
    information_system = "CNES-SR",
    municipality_data = FALSE
  )

  expect_identical(
    as.character(result$SERV_ESP),
    c("Servico antigo", "Servico atual")
  )
  expect_identical(result$CLASS_SR, c("103", "001"))
  expect_equal(downloads, 1L)
})

test_that("process_cnes keeps its missing default and validates arguments", {
  expect_message(
    result <- process_cnes(
      data.frame(CNES = "0000001"),
      municipality_data = FALSE
    ),
    "Starting CNES-ST"
  )
  expect_identical(result$CNES, "0000001")

  expect_error(
    process_cnes(
      data.frame(),
      information_system = c("CNES-ST", "CNES-PF")
    ),
    "must be one of"
  )
  expect_error(process_cnes(NULL), "must be a data frame")
  expect_error(process_cnes(data.frame(), nomes = NA), "TRUE.*FALSE")
  expect_error(
    process_cnes(data.frame(), municipality_data = 1),
    "TRUE.*FALSE"
  )
})
