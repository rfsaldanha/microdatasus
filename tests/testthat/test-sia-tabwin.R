create_sia_tabwin_fixture <- function(period = "current") {
  root <- tempfile("sia-tabwin-fixture-")
  dir.create(file.path(root, "CNV"), recursive = TRUE)
  dir.create(file.path(root, "DBF"), recursive = TRUE)

  current_definitions <- c(
    "SIA-AB" = "APAC_Cirurgia_Bariatica.DEF",
    "SIA-ABO" = "APAC_Pos_Cirurgia_Bariatica.def",
    "SIA-ACF" = "APAC_Confeccao_de_Fistula.DEF",
    "SIA-AD" = "APAC_Laudos_Diversos.DEF",
    "SIA-AN" = "APAC_Nefrologia.DEF",
    "SIA-AM" = "APAC_Medicamentos.DEF",
    "SIA-AQ" = "APAC_Quimioterapia.DEF",
    "SIA-AR" = "APAC_Radioterapia.DEF",
    "SIA-ATD" = "APAC_Tratamento_Dialitico.DEF",
    "SIA-PA" = "Producao_Ambulatorial.DEF",
    "SIA-PS" = "RAAS_Psicossocial.def",
    "SIA-SAD" = "Atencao_Domiciliar.def"
  )

  if (identical(period, "current")) {
    for (information_system in names(current_definitions)) {
      lines <- c("A*.dbc", "XFlag, FLAG, 1, CNV/FLAG.CNV")
      if (information_system %in% c("SIA-AQ", "SIA-AR")) {
        field <- if (identical(information_system, "SIA-AQ")) {
          "AQ_GRAHIS"
        } else {
          "AR_GRAHIS"
        }
        lines <- c(
          lines,
          paste0("XGrau histologico, ", field, ", 1, CNV/GRAU_HIS.CNV")
        )
      }
      if (identical(information_system, "SIA-ACF")) {
        lines <- c(
          lines,
          "XDuplex previo, ACF_DUPLEX, 1, CNV/SIMNAO2.CNV"
        )
      }
      if (identical(information_system, "SIA-AB")) {
        lines <- c(
          lines,
          "IAltas, AP_ALTA",
          "XAltas, AP_ALTA, 1, CNV/FLAG.CNV",
          "IValor aprovado, AP_VL_AP",
          "XComorbidade, AB_PONTBARR, 1, CNV/P_BAROS.CNV"
        )
      }
      if (identical(information_system, "SIA-PA")) {
        lines <- c(
          lines,
          "DProcedimento, PA_PROC_ID, LABEL, DBF/PROC.DBF",
          "DOcupacao, PA_CBOCOD, LABEL, DBF/CBO.DBF",
          "DEquipe, PA_INE, LABEL, DBF/EQUIPE.DBF"
        )
      }
      write_tabwin_text(
        file.path(root, unname(current_definitions[[information_system]])),
        lines
      )
    }
    label <- "Atual"
    foreign::write.dbf(
      data.frame(PA_PROC_ID = "010101", LABEL = "Procedimento teste"),
      file.path(root, "DBF", "PROC.DBF")
    )
    foreign::write.dbf(
      data.frame(PA_CBOCOD = "123456", LABEL = "Ocupacao teste"),
      file.path(root, "DBF", "CBO.DBF")
    )
    foreign::write.dbf(
      data.frame(PA_INE = "0000000001", LABEL = "Equipe teste"),
      file.path(root, "DBF", "EQUIPE.DBF")
    )
  } else {
    definition <- switch(
      period,
      "1994-07-1999-10" = "PRODUCAO.DEF",
      "1999-11-2003-07" = "PROD_SIA.DEF",
      "2003-08-2007" = "PRODCNES.DEF"
    )
    label <- switch(
      period,
      "1994-07-1999-10" = "Historico 1994",
      "1999-11-2003-07" = "Historico 1999",
      "2003-08-2007" = "Historico 2003"
    )
    write_tabwin_text(
      file.path(root, definition),
      c("A*.dbc", "XFlag, FLAG, 1, CNV/FLAG.CNV")
    )
  }

  write_tabwin_text(
    file.path(root, "CNV", "FLAG.CNV"),
    c("1 1", tabwin_cnv_line(1, label, "1"))
  )
  if (identical(period, "current")) {
    write_tabwin_text(
      file.path(root, "CNV", "GRAU_HIS.CNV"),
      c(
        "5 2 L",
        tabwin_cnv_line(1, "Grau nao avaliavel", "GX"),
        tabwin_cnv_line(2, "Bem diferenciado", "G1"),
        tabwin_cnv_line(3, "Moderadamente diferenciado", "G2"),
        tabwin_cnv_line(4, "Pouco diferenciado", "G3"),
        tabwin_cnv_line(5, "Indiferenciado", "G4")
      )
    )
    write_tabwin_text(
      file.path(root, "CNV", "SIMNAO2.CNV"),
      c(
        "2 1",
        tabwin_cnv_line(1, "Sim", "1"),
        tabwin_cnv_line(2, "Nao", "0")
      )
    )
    write_tabwin_text(
      file.path(root, "CNV", "P_BAROS.CNV"),
      c(
        "2 1 L",
        tabwin_cnv_line(1, "Com comorbidade", "0"),
        tabwin_cnv_line(2, "Sem comorbidade", "1")
      )
    )
  }
  archive <- tempfile(fileext = ".zip")
  zip::zipr(
    archive,
    # Add the top-level directories so CNV/ and DBF/ paths are preserved.
    files = list.files(root),
    root = root
  )
  unlink(root, recursive = TRUE)
  archive
}

test_that("process_sia preserves all established arguments", {
  expect_identical(
    as.pairlist(formals(process_sia)[c("data", "information_system", "nome_proced", "nome_ocupacao", "nome_equipe", "municipality_data")]),
    as.pairlist(alist(
      data = ,
      information_system = "SIA-PA",
      nome_proced = TRUE,
      nome_ocupacao = TRUE,
      nome_equipe = TRUE,
      municipality_data = TRUE
    ))
  )
})

test_that("SIA registry covers every downloadable layout and official ZIP", {
  registry <- microdatasus:::.tabwin_registry()
  expect_setequal(
    microdatasus:::.sia_information_systems,
    paste0(
      "SIA-",
      c(
        "AB", "ABO", "ACF", "AD", "AN", "AM",
        "AQ", "AR", "ATD", "PA", "PS", "SAD"
      )
    )
  )
  expect_true(all(vapply(
    registry[microdatasus:::.sia_information_systems],
    function(spec) identical(basename(spec$url), "TAB_SIA.zip"),
    logical(1)
  )))
  expect_match(
    registry[["SIA-PA-1994-07-1999-10"]]$url,
    "TAB_SIA_199407-199910.zip",
    fixed = TRUE
  )
  expect_match(
    registry[["SIA-PA-1999-11-2003-07"]]$url,
    "TAB_SIA_199911-200307.zip",
    fixed = TRUE
  )
  expect_match(
    registry[["SIA-PA-2003-08-2007"]]$url,
    "TAB_SIA_200308-200712.zip",
    fixed = TRUE
  )
})

test_that("SIA categorical flags are not coerced to integers", {
  data <- data.frame(
    AB_PONTBAR = "E", AQ_LINFIN = "S", AR_LINFIN = "N",
    PESO = "70", TABBARR = "1", stringsAsFactors = FALSE
  )

  types <- microdatasus:::.sia_type_fields(data)

  expect_false(any(c("AB_PONTBAR", "AQ_LINFIN", "AR_LINFIN") %in%
    types$integer))
  expect_true(all(c("PESO", "TABBARR") %in% types$integer))
})

test_that("SIA dates keep their type when blank or partially malformed", {
  data <- data.frame(
    AP_DTOCOR = c("", NA, "20260131", "invalid"),
    DT_PROCESS = rep("202601", 4L),
    AM_QTDTRAN = rep("12", 4L),
    AR_CIDTR1 = rep("C500", 4L)
  )

  types <- microdatasus:::.sia_type_fields(data)

  expect_identical(types$date, "AP_DTOCOR")
  converted <- microdatasus:::.process_as_date(data$AP_DTOCOR, "%Y%m%d")
  expect_s3_class(converted, "Date")
  expect_identical(
    converted,
    as.Date(c(NA, NA, "2026-01-31", NA))
  )
})

test_that("SIA DEF increments take precedence over categorical views", {
  archive <- create_sia_tabwin_fixture()
  on.exit(unlink(archive), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout, quiet = FALSE) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  result <- process_sia(
    data.frame(
      AP_ALTA = c("0", "1"),
      AP_VL_AP = c("12.50", "0.75")
    ),
    information_system = "SIA-AB",
    municipality_data = FALSE,
    labels = "none"
  )

  expect_identical(result$AP_ALTA, c(0L, 1L))
  expect_identical(result$AP_VL_AP, c(12.5, 0.75))
})

test_that("SIA resolves documented physical DEF field names", {
  dictionaries <- list(list(definitions = data.frame(
    field = c(
      "AB_PONTBARR", "AP_TPATEND", "AP_NAT_JUR",
      "AP_TIPPRE", "ATD_SEPERIA"
    ),
    stringsAsFactors = FALSE
  )))
  data <- data.frame(
    AB_PONTBAR = "0", AP_TPATEN = "1", AP_NATJUR = "1000",
    AP_TPPRE = "40", ATD_SEPERI = "1", AP_DTOCOR = "20260101"
  )
  types <- microdatasus:::.sia_type_fields(data)

  aliases <- microdatasus:::.sia_dictionary_aliases(
    data, dictionaries, types
  )

  expect_identical(
    aliases,
    c(
      "AB_PONTBAR" = "AB_PONTBARR",
      "AP_TPATEN" = "AP_TPATEND",
      "AP_NATJUR" = "AP_NAT_JUR",
      "AP_TPPRE" = "AP_TIPPRE",
      "ATD_SEPERI" = "ATD_SEPERIA"
    )
  )
  expect_false("AP_DTOCOR" %in% names(aliases))
})

test_that("process_sia applies labels through physical DBF names", {
  archive <- create_sia_tabwin_fixture()
  on.exit(unlink(archive), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(url, destination, timeout, quiet = FALSE) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  result <- process_sia(
    data.frame(AB_PONTBAR = c("0", "1", "E")),
    information_system = "SIA-AB",
    municipality_data = FALSE,
    diagnostics = TRUE
  )

  expect_s3_class(result$AB_PONTBAR, "factor")
  expect_identical(
    as.character(result$AB_PONTBAR),
    c("Com comorbidade", "Sem comorbidade", "E")
  )
  expect_identical(
    processing_diagnostics(result)$unknown_codes$code,
    "E"
  )
})

test_that("SIA nephrology measurements remain quantitative", {
  data <- data.frame(
    AN_TRU = c("67", "65.5", "bad"),
    AN_ALBUMI = c("4", "3.5", "bad"),
    AN_HB = c("11", "10.5", "bad"),
    AN_INTFIS = c("2", "3", "bad"),
    stringsAsFactors = FALSE
  )
  types <- microdatasus:::.sia_type_fields(data)

  expect_setequal(types$double, c("AN_TRU", "AN_ALBUMI", "AN_HB"))
  expect_identical(types$integer, "AN_INTFIS")
  expect_true(all(names(data) %in% types$protected))

  archive <- create_sia_tabwin_fixture()
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

  result <- process_sia(
    data,
    information_system = "SIA-AN",
    nome_proced = FALSE,
    nome_ocupacao = FALSE,
    nome_equipe = FALSE,
    municipality_data = FALSE,
    diagnostics = TRUE
  )

  expect_identical(result$AN_TRU, c(67, 65.5, NA_real_))
  expect_identical(result$AN_ALBUMI, c(4, 3.5, NA_real_))
  expect_identical(result$AN_HB, c(11, 10.5, NA_real_))
  expect_identical(result$AN_INTFIS, c(2L, 3L, NA_integer_))
  report <- processing_diagnostics(result)
  expect_equal(nrow(report$unknown_codes), 0L)
  failures <- report$coercion_failures
  expect_setequal(failures$field, names(data))
  expect_setequal(failures$target, c("double", "integer"))
})

test_that("all current SIA layouts share one archive and label from their DEF", {
  archive <- create_sia_tabwin_fixture()
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

  for (information_system in microdatasus:::.sia_information_systems) {
    result <- process_sia(
      data.frame(FLAG = "1"),
      information_system = information_system,
      nome_proced = FALSE,
      nome_ocupacao = FALSE,
      nome_equipe = FALSE,
      municipality_data = FALSE
    )
    expect_identical(as.character(result$FLAG), "Atual")
  }
  expect_equal(downloads, 1L)
})

test_that("process_sia standardizes types and honors optional label flags", {
  archive <- create_sia_tabwin_fixture()
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
    PA_DATREF = "202401",
    PA_DTTEST = "20240131",
    PA_QTDPRO = "3",
    PA_VALAPR = "12.50",
    PA_CODUNI = factor("0000001"),
    PA_MUNPCN = "1200209",
    PA_PROC_ID = "010101",
    PA_CBOCOD = "123456",
    PA_INE = "0000000001",
    FLAG = c("1"),
    stringsAsFactors = FALSE
  )
  messages <- capture_messages({
    labelled <- process_sia(source, municipality_data = FALSE)
  })
  unlabelled <- process_sia(
    source,
    nome_proced = FALSE,
    nome_ocupacao = FALSE,
    nome_equipe = FALSE,
    municipality_data = FALSE
  )

  expect_s3_class(labelled$PA_DTTEST, "Date")
  expect_type(labelled$PA_QTDPRO, "integer")
  expect_type(labelled$PA_VALAPR, "double")
  expect_type(labelled$PA_CODUNI, "character")
  expect_identical(labelled$PA_MUNPCN, "120020")
  expect_identical(as.character(labelled$PA_PROC_ID), "Procedimento teste")
  expect_identical(as.character(labelled$PA_CBOCOD), "Ocupacao teste")
  expect_identical(as.character(labelled$PA_INE), "Equipe teste")
  expect_identical(unlabelled$PA_PROC_ID, "010101")
  expect_identical(unlabelled$PA_CBOCOD, "123456")
  expect_identical(unlabelled$PA_INE, "0000000001")
  expect_lt(
    which(grepl("Cached.+SIA-PA", messages)),
    which(grepl("Starting SIA-PA", messages))
  )
  expect_lt(
    which(grepl("Starting SIA-PA", messages)),
    which(grepl("Finished SIA-PA", messages))
  )
})

test_that("process_sia recovers only audited histological-grade aliases", {
  archive <- create_sia_tabwin_fixture()
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

  source <- c("1", "01", "2", "02", "3", "03", "4", "04", "0", "00", "99")
  expected <- c(
    "Bem diferenciado", "Bem diferenciado",
    "Moderadamente diferenciado", "Moderadamente diferenciado",
    "Pouco diferenciado", "Pouco diferenciado",
    "Indiferenciado", "Indiferenciado",
    "0", "00", "99"
  )
  for (information_system in c("SIA-AQ", "SIA-AR")) {
    field <- if (identical(information_system, "SIA-AQ")) {
      "AQ_GRAHIS"
    } else {
      "AR_GRAHIS"
    }
    data <- stats::setNames(data.frame(source), field)
    result <- process_sia(
      data,
      information_system = information_system,
      nome_proced = FALSE,
      nome_ocupacao = FALSE,
      nome_equipe = FALSE,
      municipality_data = FALSE,
      labels = "character",
      diagnostics = TRUE
    )

    expect_identical(result[[field]], expected)
    unknown <- processing_diagnostics(result)$unknown_codes
    unknown <- unknown[unknown$field == field, , drop = FALSE]
    expect_setequal(unknown$code, c("0", "00", "99"))
    expect_true(all(unknown$n == 1L))
  }
})

test_that("process_sia accepts both official yes-no source dialects", {
  archive <- create_sia_tabwin_fixture()
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

  result <- process_sia(
    data.frame(ACF_DUPLEX = c("S", "N", "1", "0", "X")),
    information_system = "SIA-ACF",
    nome_proced = FALSE,
    nome_ocupacao = FALSE,
    nome_equipe = FALSE,
    municipality_data = FALSE,
    labels = "character",
    diagnostics = TRUE
  )

  expect_identical(result$ACF_DUPLEX, c("Sim", "Nao", "Sim", "Nao", "X"))
  unknown <- processing_diagnostics(result)$unknown_codes
  expect_identical(unknown$field, "ACF_DUPLEX")
  expect_identical(unknown$code, "X")
  expect_identical(unknown$n, 1L)
})

test_that("process_sia selects historical PA definitions row by row", {
  periods <- c(
    "current", "1994-07-1999-10",
    "1999-11-2003-07", "2003-08-2007"
  )
  archives <- stats::setNames(
    lapply(periods, create_sia_tabwin_fixture),
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
      period <- if (grepl("199407-199910", url, fixed = TRUE)) {
        "1994-07-1999-10"
      } else if (grepl("199911-200307", url, fixed = TRUE)) {
        "1999-11-2003-07"
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

  result <- process_sia(
    data.frame(
      PA_MVM = c("", "", "", "202401"),
      PA_DATREF = c("9701", "200001", "200401", ""),
      FLAG = c("1", "1", "1", "9"),
      stringsAsFactors = FALSE
    ),
    nome_proced = FALSE,
    nome_ocupacao = FALSE,
    nome_equipe = FALSE,
    municipality_data = FALSE
  )

  expect_setequal(downloads, periods)
  expect_identical(
    as.character(result$FLAG),
    c("Historico 1994", "Historico 1999", "Historico 2003", "9")
  )
})

test_that("process_sia standardizes patient age in every applicable layout", {
  archive <- create_sia_tabwin_fixture()
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

  units <- c("2", "3", "4", "5", "0", "9")
  values <- c("10", "5", "24", "1", "0", "99")
  expected_days <- c(
    10L, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_
  )
  expected_months <- c(
    NA_integer_, 5L, NA_integer_, NA_integer_, NA_integer_, NA_integer_
  )
  expected_years <- c(
    NA_integer_, NA_integer_, 24L, 101L, NA_integer_, NA_integer_
  )

  apac_systems <- setdiff(
    microdatasus:::.sia_information_systems,
    c("SIA-PA", "SIA-PS", "SIA-SAD")
  )
  for (information_system in apac_systems) {
    result <- process_sia(
      data.frame(AP_COIDADE = units, AP_NUIDADE = values),
      information_system = information_system,
      nome_proced = FALSE,
      nome_ocupacao = FALSE,
      nome_equipe = FALSE,
      municipality_data = FALSE
    )
    expect_identical(result$AP_COIDADE, units)
    expect_identical(result$AP_NUIDADE, as.integer(values))
    expect_identical(result$IDADEdias, expected_days)
    expect_identical(result$IDADEmeses, expected_months)
    expect_identical(result$IDADEanos, expected_years)
  }

  for (information_system in c("SIA-PS", "SIA-SAD")) {
    result <- process_sia(
      data.frame(TPIDADEPAC = units, IDADEPAC = values),
      information_system = information_system,
      nome_proced = FALSE,
      nome_ocupacao = FALSE,
      nome_equipe = FALSE,
      municipality_data = FALSE
    )
    expect_identical(result$TPIDADEPAC, units)
    expect_identical(result$IDADEPAC, as.integer(values))
    expect_identical(result$IDADEdias, expected_days)
    expect_identical(result$IDADEmeses, expected_months)
    expect_identical(result$IDADEanos, expected_years)
  }

  pa <- process_sia(
    data.frame(PA_IDADE = c("0", "24", "130", "998", "999")),
    nome_proced = FALSE,
    nome_ocupacao = FALSE,
    nome_equipe = FALSE,
    municipality_data = FALSE
  )
  expect_identical(pa$PA_IDADE, c(0L, 24L, 130L, 998L, 999L))
  expect_identical(pa$IDADEanos, c(0L, 24L, 130L, NA_integer_, NA_integer_))
})

test_that("process_sia validates layout and every compatibility flag", {
  expect_error(process_sia(data.frame(), "SIA-XX"), "must be one of")
  for (argument in c(
    "nome_proced", "nome_ocupacao", "nome_equipe", "municipality_data"
  )) {
    call <- list(data = data.frame())
    call[[argument]] <- NA
    expect_error(do.call(process_sia, call), argument)
  }
})
