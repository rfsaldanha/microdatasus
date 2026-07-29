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
    formals(process_sia),
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
