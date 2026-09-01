create_sih_tabwin_fixture <- function(period = "current") {
  root <- tempfile("sih-tabwin-fixture-")
  cnv <- file.path(root, "CNV")
  dbf <- file.path(root, "DBF")
  dir.create(cnv, recursive = TRUE)
  dir.create(dbf, recursive = TRUE)

  if (identical(period, "current")) {
    definitions <- list(
      "RD2008.DEF" = c(
        "Ard*.db?",
        "XSexo, SEXO, 1, CNV/SEXO.CNV",
        "XVinculo, VINCPREV, 1, CNV/VINCULO.CNV"
      ),
      "RJ2008.DEF" = c(
        "Arj*.db?",
        "XSexo, SEXO, 1, CNV/SEXO.CNV",
        "XVinculo, VINCPREV, 1, CNV/VINCULO.CNV",
        "XSituacao, ST_SITUAC, 1, CNV/SITUAC.CNV"
      ),
      "SP2008.DEF" = c(
        "Asp*.db?",
        "XTipo valor, IN_TP_VAL, 1, CNV/TPVAL.CNV"
      ),
      "Motivo_de_Erro.DEF" = c(
        "Aer*.db?",
        "XErro, CO_ERRO, DESCR, DBF/MOTERRO.DBF"
      )
    )
    for (name in names(definitions)) {
      write_tabwin_text(file.path(root, name), definitions[[name]])
    }
    write_tabwin_text(
      file.path(cnv, "SEXO.CNV"),
      c(
        "2 1",
        tabwin_cnv_line(1, "Masculino", "1"),
        tabwin_cnv_line(2, "Feminino", "2")
      )
    )
    write_tabwin_text(
      file.path(cnv, "VINCULO.CNV"),
      c(
        "2 1",
        tabwin_cnv_line(1, "Autonomo", "1"),
        tabwin_cnv_line(2, "Empregado", "5")
      )
    )
    write_tabwin_text(
      file.path(cnv, "SITUAC.CNV"),
      c(
        "2 1",
        tabwin_cnv_line(1, "Rejeitada", "1"),
        tabwin_cnv_line(2, "Liberada", "2")
      )
    )
    write_tabwin_text(
      file.path(cnv, "TPVAL.CNV"),
      c(
        "2 1",
        tabwin_cnv_line(1, "Valor aprovado", "1"),
        tabwin_cnv_line(2, "Valor rateado", "2")
      )
    )
    foreign::write.dbf(
      data.frame(
        CO_ERRO = c("E01", "E02"),
        DESCR = c("Erro de teste", "Outro erro"),
        stringsAsFactors = FALSE
      ),
      file.path(dbf, "MOTERRO.DBF")
    )
  } else {
    label <- switch(
      period,
      "1992-1997" = "Historico 1992",
      "1998-2003-07" = "Historico 1998",
      "2003-08-2007" = "Historico 2003"
    )
    for (definition in c("RD.DEF", "RJ.DEF")) {
      write_tabwin_text(
        file.path(root, definition),
        c("A*.db?", "XSexo, SEXO, 1, CNV/HIST.CNV")
      )
    }
    write_tabwin_text(
      file.path(cnv, "HIST.CNV"),
      c("1 1", tabwin_cnv_line(1, label, "1"))
    )
  }

  archive <- tempfile(fileext = ".zip")
  zip::zipr(archive, files = list.files(root), root = root)
  unlink(root, recursive = TRUE)
  archive
}

test_that("process_sih keeps its established argument order", {
  expect_identical(
    as.pairlist(formals(process_sih)[c("data", "information_system", "municipality_data")]),
    as.pairlist(alist(
      data = ,
      information_system = "SIH-RD",
      municipality_data = TRUE
    ))
  )
})

test_that("SIH registry matches all archives published by transfer portal", {
  registry <- microdatasus:::.tabwin_registry()

  expect_identical(registry[["SIH-RD"]]$definition, "RD2008.DEF")
  expect_identical(registry[["SIH-RJ"]]$definition, "RJ2008.DEF")
  expect_identical(registry[["SIH-SP"]]$definition, "SP2008.DEF")
  expect_identical(
    registry[["SIH-ER"]]$definition,
    "Motivo_de_Erro.DEF"
  )
  expect_match(registry[["SIH-RD"]]$url, "TAB_SIH.zip", fixed = TRUE)
  expect_match(
    registry[["SIH-RD-1992-1997"]]$url,
    "TAB_SIH_199201-199712.zip",
    fixed = TRUE
  )
  expect_match(
    registry[["SIH-RD-1998-2003-07"]]$url,
    "TAB_SIH_199801-200307.zip",
    fixed = TRUE
  )
  expect_match(
    registry[["SIH-RD-2003-08-2007"]]$url,
    "TAB_SIH_200308-200712.zip",
    fixed = TRUE
  )
})

test_that("process_sih labels and types every current SIH file family", {
  archive <- create_sih_tabwin_fixture()
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
    rd <- process_sih(
      data.frame(
        ANO_CMPT = "2024",
        MES_CMPT = "1",
        NASC = "20000102",
        DT_INTER = "20240101",
        SEXO = "1",
        VINCPREV = "5",
        DIAG_PRINC = "001",
        IDADE = "24",
        COD_IDADE = "4",
        VAL_TOT = "10.25",
        MUNIC_RES = "1200209",
        AUD_JUST = "S\\u00e3o",
        stringsAsFactors = FALSE
      ),
      municipality_data = FALSE
    )
  })
  rj <- process_sih(
    data.frame(
      ANO_CMPT = "2024",
      MES_CMPT = "1",
      SEXO = "2",
      ST_SITUAC = "1",
      stringsAsFactors = FALSE
    ),
    information_system = "SIH-RJ",
    municipality_data = FALSE
  )
  sp <- process_sih(
    data.frame(
      SP_AA = "24",
      SP_MM = "1",
      SP_DTINTER = "20240101",
      SP_QTD_ATO = "3",
      SP_VALATO = "12.50",
      IN_TP_VAL = "1",
      stringsAsFactors = FALSE
    ),
    information_system = "SIH-SP",
    municipality_data = FALSE
  )
  er <- process_sih(
    data.frame(
      ANO = "2024",
      MES = "1",
      DT_INTER = "20240101",
      DT_SAIDA = "20240103",
      MUN_RES = "1200209",
      CO_ERRO = "E01",
      stringsAsFactors = FALSE
    ),
    information_system = "SIH-ER",
    municipality_data = FALSE
  )

  expect_equal(downloads, 1L)
  expect_type(rd$ANO_CMPT, "integer")
  expect_type(rd$IDADE, "integer")
  expect_type(rd$COD_IDADE, "character")
  expect_identical(rd$DIAG_PRINC, "001")
  expect_type(rd$VAL_TOT, "double")
  expect_s3_class(rd$NASC, "Date")
  expect_s3_class(rd$DT_INTER, "Date")
  expect_identical(as.character(rd$SEXO), "Masculino")
  expect_identical(as.character(rd$VINCPREV), "Empregado")
  expect_identical(rd$MUNIC_RES, "120020")
  expect_identical(rd$AUD_JUST, "São")
  expect_identical(as.character(rj$SEXO), "Feminino")
  expect_identical(as.character(rj$ST_SITUAC), "Rejeitada")
  expect_type(sp$SP_QTD_ATO, "integer")
  expect_type(sp$SP_VALATO, "double")
  expect_s3_class(sp$SP_DTINTER, "Date")
  expect_identical(as.character(sp$IN_TP_VAL), "Valor aprovado")
  expect_type(er$ANO, "integer")
  expect_s3_class(er$DT_SAIDA, "Date")
  expect_identical(as.character(er$CO_ERRO), "Erro de teste")
  expect_identical(er$MUN_RES, "120020")
  expect_match(messages, "Cached.+SIH-RD", all = FALSE)
  expect_match(messages, "Starting SIH-RD", all = FALSE)
  expect_match(messages, "Finished SIH-RD", all = FALSE)
  expect_lt(
    which(grepl("Cached.+SIH-RD", messages)),
    which(grepl("Starting SIH-RD", messages))
  )
  expect_lt(
    which(grepl("Starting SIH-RD", messages)),
    which(grepl("Finished SIH-RD", messages))
  )
})

test_that("process_sih parses six-digit historical dates without a pivot", {
  result <- suppressMessages(process_sih(
    data.frame(
      ANO_CMPT = c("1992", "1992", "1992"),
      MES_CMPT = c("1", "1", "1"),
      NASC = c("231102", "920101", "000000"),
      DT_SAIDA = c("910123", "920102", "invalid"),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE,
    labels = "none",
    diagnostics = TRUE
  ))

  expect_identical(
    as.character(result$NASC),
    c("1923-11-02", "1992-01-01", NA)
  )
  expect_identical(
    as.character(result$DT_SAIDA),
    c("1991-01-23", "1992-01-02", NA)
  )
  failures <- processing_diagnostics(result)$coercion_failures
  expect_identical(failures$field, "DT_SAIDA")
  expect_identical(failures$value, "invalid")
})

test_that("process_sih chooses historical dictionaries row by row", {
  periods <- c(
    "current", "1992-1997", "1998-2003-07", "2003-08-2007"
  )
  archives <- stats::setNames(
    lapply(periods, create_sih_tabwin_fixture),
    periods
  )
  on.exit(unlink(unlist(archives)), add = TRUE)
  downloads <- character()
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      period <- if (grepl("199201-199712", url, fixed = TRUE)) {
        "1992-1997"
      } else if (grepl("199801-200307", url, fixed = TRUE)) {
        "1998-2003-07"
      } else if (grepl("200308-200712", url, fixed = TRUE)) {
        "2003-08-2007"
      } else {
        "current"
      }
      downloads <<- c(downloads, period)
      file.copy(archives[[period]], destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  result <- process_sih(
    data.frame(
      ANO_CMPT = c("1996", "2000", "2003", "2003", "2007", "2008"),
      MES_CMPT = c("1", "1", "7", "8", "12", "1"),
      SEXO = rep("1", 6L),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE
  )

  expect_setequal(downloads, periods)
  expect_identical(
    as.character(result$SEXO),
    c(
      "Historico 1992", "Historico 1998", "Historico 1998",
      "Historico 2003", "Historico 2003", "Masculino"
    )
  )
})

test_that("process_sih retains unknown codes and normalizes source factors", {
  archive <- create_sih_tabwin_fixture()
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

  result <- process_sih(
    data.frame(
      ANO_CMPT = c("2024", "2024"),
      MES_CMPT = c("1", "1"),
      SEXO = factor(c("1", "9")),
      N_AIH = factor(c("120000000001", "120000000002")),
      stringsAsFactors = TRUE
    ),
    municipality_data = FALSE
  )

  expect_s3_class(result$SEXO, "factor")
  expect_identical(as.character(result$SEXO), c("Masculino", "9"))
  expect_type(result$N_AIH, "character")
  expect_identical(
    result$N_AIH,
    c("120000000001", "120000000002")
  )
})

test_that("process_sih decodes the patient age unit in RD and RJ files", {
  source <- data.frame(
    COD_IDADE = c("2", "3", "4", "5", "0", "9"),
    IDADE = c("10", "5", "24", "1", "0", "99"),
    stringsAsFactors = FALSE
  )

  for (information_system in c("SIH-RD", "SIH-RJ")) {
    result <- process_sih(
      source,
      information_system = information_system,
      municipality_data = FALSE
    )

    expect_identical(result$COD_IDADE, source$COD_IDADE)
    expect_identical(result$IDADE, c(10L, 5L, 24L, 1L, 0L, 99L))
    expect_identical(
      result$IDADEdias,
      c(10L, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
    )
    expect_identical(
      result$IDADEmeses,
      c(NA_integer_, 5L, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
    )
    expect_identical(
      result$IDADEanos,
      c(NA_integer_, NA_integer_, 24L, 101L, NA_integer_, NA_integer_)
    )
  }
})

test_that("process_sih validates its file family and flags", {
  expect_error(process_sih(data.frame(), "SIH-XX"), "must be one of")
  expect_error(
    process_sih(data.frame(), municipality_data = NA),
    "municipality_data"
  )
})
