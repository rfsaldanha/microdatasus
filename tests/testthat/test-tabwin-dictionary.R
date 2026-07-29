tabwin_cnv_line <- function(number, label, codes) {
  paste0(
    "   ",
    sprintf("%4d", number),
    "  ",
    label,
    strrep(" ", 50L - nchar(label, type = "chars")),
    " ",
    codes
  )
}

write_tabwin_text <- function(path, lines) {
  writeLines(
    iconv(lines, from = "UTF-8", to = "windows-1252"),
    path,
    useBytes = TRUE
  )
}

create_tabwin_fixture <- function() {
  root <- tempfile("tabwin-fixture-")
  tabdo <- file.path(root, "OBITOS_CID10_TAB", "tabdo")
  dir.create(tabdo, recursive = TRUE)
  definition <- c(
    "; SIM-DO test fixture based on the official TabWin format",
    "Ado*.db?",
    "XTipo Obito, TIPOBITO, 1, TIPOBITO.CNV",
    "XSexo, SEXO, 1, SEXO.CNV",
    ";XRaca antiga, RACACOR, 1, OLD.CNV",
    "XRaca Cor, RACACOR, 1, RACACOR.CNV",
    "XEstabelecimento, CODESTAB, DESCESTAB, CNES26.DBF"
  )
  write_tabwin_text(
    file.path(tabdo, "Obito_1996_CID10.def"),
    definition
  )
  write_tabwin_text(
    file.path(tabdo, "TIPOBITO.CNV"),
    c(
      "003 1",
      tabwin_cnv_line(3, "Ignorado", "9"),
      tabwin_cnv_line(1, "Fetal", "1"),
      tabwin_cnv_line(2, "Não Fetal", "2")
    )
  )
  write_tabwin_text(
    file.path(tabdo, "SEXO.CNV"),
    c(
      "3 1",
      tabwin_cnv_line(3, "I", "I,0,9"),
      tabwin_cnv_line(1, "M", "M,1"),
      tabwin_cnv_line(2, "F", "F,2")
    )
  )
  write_tabwin_text(
    file.path(tabdo, "RACACOR.CNV"),
    c(
      "6 1 L",
      tabwin_cnv_line(1, "N Inf", ""),
      tabwin_cnv_line(2, "Bra", "1"),
      tabwin_cnv_line(3, "Preta", "2"),
      tabwin_cnv_line(4, "Amar", "3"),
      tabwin_cnv_line(5, "Parda", "4"),
      tabwin_cnv_line(6, "Indig", "5")
    )
  )
  foreign::write.dbf(
    data.frame(
      CD_CNES = "0000001",
      DESCESTAB = "0000001 POSTO DE SAUDE PARQUE AGUA LIMPA",
      stringsAsFactors = FALSE
    ),
    file.path(tabdo, "CNES26.DBF")
  )
  archive <- tempfile(fileext = ".zip")
  zip::zipr(
    archive,
    files = "OBITOS_CID10_TAB",
    root = root
  )
  unlink(root, recursive = TRUE)
  archive
}

restore_empty_tabwin_cache <- function() {
  microdatasus:::.tabwin_clear_cache()
  for (information_system in c(
    "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT"
  )) {
    assign(
      information_system,
      .empty_tabwin_dictionary(information_system),
      envir = microdatasus:::.tabwin_cache
    )
  }
}

test_that("process_sim appends its data type argument compatibly", {
  expect_identical(
    formals(process_sim),
    as.pairlist(alist(
      data = ,
      municipality_data = TRUE,
      information_system = "SIM-DO"
    ))
  )
})

test_that("TabWin registry covers every SIM type supported by fetch_datasus", {
  expect_setequal(
    names(microdatasus:::.tabwin_registry()),
    c("SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT")
  )
})

test_that("DEF parser reads active CNV metadata without commented entries", {
  archive <- create_tabwin_fixture()
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

  dictionary <- fetch_tabwin_dictionary(quiet = TRUE)

  expect_s3_class(dictionary, "microdatasus_tabwin_dictionary")
  expect_setequal(
    dictionary$definitions$field,
    c("TIPOBITO", "SEXO", "RACACOR", "CODESTAB")
  )
  expect_false("OLD.CNV" %in% dictionary$definitions$file)
  expect_equal(downloads, 1L)

  cached <- fetch_tabwin_dictionary(quiet = TRUE)
  expect_identical(cached, dictionary)
  expect_equal(downloads, 1L)

  refreshed <- fetch_tabwin_dictionary(refresh = TRUE, quiet = TRUE)
  expect_s3_class(refreshed, "microdatasus_tabwin_dictionary")
  expect_equal(downloads, 2L)
})

test_that("CNV parser reads labels, aliases, and numeric ranges", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(
    path,
    c(
      "2 2",
      tabwin_cnv_line(1, "Primeiro", "01-03"),
      tabwin_cnv_line(2, "Segundo", "09")
    )
  )

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(
    unname(conversion$map[c("01", "02", "03", "09")]),
    c("Primeiro", "Primeiro", "Primeiro", "Segundo")
  )
})

test_that("DBF relationships use the description field declared by DEF", {
  archive <- create_tabwin_fixture()
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
  dictionary <- fetch_tabwin_dictionary(quiet = TRUE)

  selected <- microdatasus:::.tabwin_select_conversion(
    dictionary,
    "CODESTAB",
    "0000001"
  )
  result <- microdatasus:::.tabwin_apply_conversion("0000001", selected)

  expect_identical(
    as.character(result),
    "0000001 POSTO DE SAUDE PARQUE AGUA LIMPA"
  )
})

test_that("all SIM types share one TabWin archive download", {
  archive <- create_tabwin_fixture()
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

  sim_types <- names(microdatasus:::.tabwin_registry())
  dictionaries <- lapply(
    sim_types,
    fetch_tabwin_dictionary,
    quiet = TRUE
  )

  expect_equal(downloads, 1L)
  expect_identical(
    vapply(dictionaries, `[[`, character(1), "information_system"),
    sim_types
  )
  expect_length(unique(vapply(
    dictionaries,
    `[[`,
    character(1),
    "archive"
  )), 1L)
})

test_that("process_sim applies official-style labels and stable types", {
  archive <- create_tabwin_fixture()
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
  fetch_tabwin_dictionary(quiet = TRUE)

  data <- data.frame(
    CONTADOR = c("1", "2"),
    TIPOBITO = c("1", "2"),
    DTOBITO = c("01012024", "02012024"),
    IDADE = c("405", "501"),
    SEXO = c("1", "2"),
    RACACOR = c("4", "8"),
    CODESTAB = c("0000001", "9999999"),
    CODMUNRES = c("120020", "120030"),
    stringsAsFactors = FALSE
  )
  result <- process_sim(data, municipality_data = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_s3_class(result$DTOBITO, "Date")
  expect_type(result$CONTADOR, "integer")
  expect_type(result$IDADEanos, "integer")
  expect_s3_class(result$TIPOBITO, "factor")
  expect_s3_class(result$SEXO, "factor")
  expect_s3_class(result$RACACOR, "factor")
  expect_s3_class(result$CODESTAB, "factor")
  expect_identical(as.character(result$TIPOBITO), c("Fetal", "Não Fetal"))
  expect_identical(as.character(result$SEXO), c("M", "F"))
  expect_identical(as.character(result$RACACOR), c("Parda", "8"))
  expect_identical(
    as.character(result$CODESTAB),
    c("0000001 POSTO DE SAUDE PARQUE AGUA LIMPA", "9999999")
  )
  expect_identical(result$IDADEanos, c(5L, 101L))
  expect_identical(result$CODMUNRES, c("120020", "120030"))
})

test_that("process_sim reports caching and pre-processing in order", {
  archive <- create_tabwin_fixture()
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

  messages <- capture_messages(
    process_sim(
      data.frame(TIPOBITO = "2"),
      municipality_data = FALSE
    )
  )
  patterns <- c(
    "Cached the DataSUS TabWin dictionary",
    "Starting SIM-DO data pre-processing",
    "Finished SIM-DO data pre-processing"
  )
  positions <- vapply(patterns, function(pattern) {
    which(grepl(pattern, messages, fixed = TRUE))[[1L]]
  }, integer(1))

  expect_true(all(diff(positions) > 0L))
})

test_that("process_sim handles every SIM type and legacy numeric names", {
  archive <- create_tabwin_fixture()
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

  for (information_system in names(microdatasus:::.tabwin_registry())) {
    messages <- capture_messages({
      result <- process_sim(
        data.frame(
          contador = "1",
          TIPOBITO = "2",
          DTOBITO = "01012024",
          SEMANGEST = "38",
          stringsAsFactors = FALSE
        ),
        municipality_data = FALSE,
        information_system = information_system
      )
    })
    expect_type(result$contador, "integer")
    expect_type(result$SEMANGEST, "integer")
    expect_s3_class(result$DTOBITO, "Date")
    expect_identical(as.character(result$TIPOBITO), "Não Fetal")
    expect_true(any(grepl(
      paste("Starting", information_system, "data pre-processing"),
      messages,
      fixed = TRUE
    )))
    expect_true(any(grepl(
      paste("Finished", information_system, "data pre-processing"),
      messages,
      fixed = TRUE
    )))
  }
  expect_equal(downloads, 1L)
})

test_that("process_sim rejects unsupported SIM data types", {
  expect_error(
    process_sim(
      data.frame(x = "1"),
      municipality_data = FALSE,
      information_system = "SIM-UNKNOWN"
    ),
    "information_system"
  )
})
