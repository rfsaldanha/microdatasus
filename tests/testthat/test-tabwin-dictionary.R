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

test_that("process_sim appends its data type argument compatibly", {
  expect_identical(
    as.pairlist(formals(process_sim)[c("data", "municipality_data", "information_system")]),
    as.pairlist(alist(
      data = ,
      municipality_data = TRUE,
      information_system = "SIM-DO"
    ))
  )
})

test_that("TabWin registry covers every SIM type supported by fetch_datasus", {
  expect_setequal(
    grep(
      "^SIM-",
      names(microdatasus:::.tabwin_registry()),
      value = TRUE
    ),
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

test_that("specific CNV categories override earlier catch-all ranges", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(
    path,
    c(
      "3 1",
      tabwin_cnv_line(3, "Ignorado", "0-9"),
      tabwin_cnv_line(1, "Masculino", "1"),
      tabwin_cnv_line(2, "Feminino", "2")
    )
  )

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(
    unname(conversion$map[c("1", "2", "9")]),
    c("Masculino", "Feminino", "Ignorado")
  )
})

test_that("CNV parser preserves analytical ranges symbolically", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(
    path,
    c(
      "1 8",
      tabwin_cnv_line(1, "Faixa analitica", "00000000-89999999")
    )
  )

  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L), conversion = conversion
  )

  expect_length(conversion$map, 0L)
  expect_identical(conversion$ranges$token, "00000000-89999999")
  expect_identical(
    as.character(microdatasus:::.tabwin_apply_conversion(
      c("00000001", "90000000"), selected
    )),
    c("Faixa analitica", "90000000")
  )
})

test_that("symbolic ranges preserve later-category priority", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "2 8",
    tabwin_cnv_line(1, "Faixa ampla", "00000000-89999999"),
    tabwin_cnv_line(2, "Codigo especifico", "00000001")
  ))
  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(definition = data.frame(position = 1L), conversion = conversion)

  result <- microdatasus:::.tabwin_apply_conversion_values(
    c("00000001", "00000002", "90000000"), selected
  )

  expect_identical(result, c("Codigo especifico", "Faixa ampla", "90000000"))
})

test_that("symbolic alphanumeric CNV ranges retain their prefix", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "1 7",
    tabwin_cnv_line(1, "Faixa alfa", "A000000-A999999")
  ))
  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(definition = data.frame(position = 1L), conversion = conversion)

  result <- microdatasus:::.tabwin_apply_conversion_values(
    c("A123456", "B123456"), selected
  )

  expect_identical(conversion$ranges$kind, "alphanumeric")
  expect_identical(result, c("Faixa alfa", "B123456"))
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

test_that("DBF selection prefers national and detailed official relations", {
  definitions <- data.frame(
    order = 1:4,
    command = c("L", "L", "D", "L"),
    description = c("CNES BR", "CNES AC", "Detalhado", "Grupo"),
    field = c("CNES", "CNES", "PROCED", "PROCED"),
    argument = "LABEL",
    position = NA_integer_,
    file = c(
      "DBF/TCNESBR.DBF", "DBF/TCNESAC.DBF",
      "DBF/TB_SIGTAW.DBF", "DBF/TB_GRUPO.DBF"
    ),
    extension = "DBF",
    stringsAsFactors = FALSE
  )
  dictionary <- list(
    definitions = definitions,
    conversions = new.env(parent = emptyenv())
  )
  for (i in seq_len(nrow(definitions))) {
    conversion <- structure(
      list(
        type = "dbf",
        code_width = 1L,
        category_count = 1L,
        map = stats::setNames(definitions$description[[i]], "1")
      ),
      class = "microdatasus_tabwin_conversion"
    )
    assign(
      microdatasus:::.tabwin_conversion_key(
        definitions[i, , drop = FALSE]
      ),
      conversion,
      envir = dictionary$conversions
    )
  }

  cnes <- microdatasus:::.tabwin_select_conversion(dictionary, "CNES", "1")
  procedure <- microdatasus:::.tabwin_select_conversion(
    dictionary,
    "PROCED",
    "1"
  )

  expect_identical(cnes$definition$file, "DBF/TCNESBR.DBF")
  expect_identical(procedure$definition$file, "DBF/TB_SIGTAW.DBF")
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

  sim_types <- grep(
    "^SIM-",
    names(microdatasus:::.tabwin_registry()),
    value = TRUE
  )
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

  sim_types <- grep(
    "^SIM-",
    names(microdatasus:::.tabwin_registry()),
    value = TRUE
  )
  for (information_system in sim_types) {
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
