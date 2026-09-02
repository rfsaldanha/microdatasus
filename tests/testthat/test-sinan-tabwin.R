create_sinan_tabwin_fixtures <- function() {
  specs <- microdatasus:::.sinan_system_specs()

  net_parent <- tempfile("sinan-net-fixture-")
  net_root <- file.path(net_parent, "TAB_SINANNET")
  dir.create(net_root, recursive = TRUE)
  online_root <- tempfile("sinan-online-fixture-")
  dir.create(online_root)

  write_definitions <- function(root, definitions, archive) {
    for (definition in unique(definitions)) {
      classification <- if (
        identical(archive, "online") &&
          identical(tolower(definition), "chiknon.def")
      ) {
        "XClassification, CLASSI_FIN, 1, CLASSCHIK.CNV"
      } else if (
        identical(archive, "net") &&
          identical(tolower(definition), "ftifoidenet.def")
      ) {
        "XClassification, CLASSI_FIN, 1, CLASSIFI.CNV"
      } else {
        character()
      }
      write_tabwin_text(
        file.path(root, definition),
        c(
          "A*.dbc",
          "IContagem, CONTEXAM",
          "XFlag, FLAG, 1, FLAG.CNV",
          "XSexo, CS_SEXO, 1, SEX.CNV",
          classification
        )
      )
    }
    write_tabwin_text(
      file.path(root, "FLAG.CNV"),
      c("1 1", tabwin_cnv_line(1, "Rotulo SINAN", "1"))
    )
    write_tabwin_text(
      file.path(root, "SEX.CNV"),
      c("2 1", tabwin_cnv_line(1, "Masculino", "M"))
    )
    if (identical(archive, "online")) {
      write_tabwin_text(
        file.path(root, "CLASSCHIK.CNV"),
        c(
          "2 2",
          tabwin_cnv_line(1, "Descartado atual", "5"),
          tabwin_cnv_line(2, "Chikungunya", "13")
        )
      )
    } else {
      write_tabwin_text(
        file.path(root, "CLASSIFI.CNV"),
        c(
          "3 1",
          tabwin_cnv_line(1, "Confirmado historico", "1"),
          tabwin_cnv_line(2, "Descartado historico", "2"),
          tabwin_cnv_line(3, "Inconclusivo historico", "8")
        )
      )
    }
  }
  write_definitions(
    net_root,
    specs$definition[specs$archive == "SINAN-NET"],
    "net"
  )
  write_definitions(
    online_root,
    specs$definition[specs$archive == "SINAN-ONLINE"],
    "online"
  )

  net_archive <- tempfile(fileext = ".zip")
  zip::zipr(
    net_archive,
    files = list.files(net_parent),
    root = net_parent
  )
  online_archive <- tempfile(fileext = ".zip")
  zip::zipr(
    online_archive,
    files = list.files(online_root),
    root = online_root
  )
  unlink(net_parent, recursive = TRUE)
  unlink(online_root, recursive = TRUE)
  list(net = net_archive, online = online_archive)
}

test_that("process_sinan has a stable unified signature", {
  expect_identical(
    as.pairlist(formals(process_sinan)[c("data", "information_system", "municipality_data")]),
    as.pairlist(alist(
      data = ,
      information_system = "SINAN-DENGUE",
      municipality_data = TRUE
    ))
  )
})

test_that("SINAN lookup exposes readable names and every accepted alias", {
  all_systems <- datasus_information_systems()
  lookup <- all_systems[
    all_systems$system == "SINAN",
    c("information_system", "name", "file_acronym", "aliases")
  ]
  specs <- microdatasus:::.sinan_system_specs()
  aliases <- microdatasus:::.sinan_alias_table()

  expect_s3_class(lookup, "tbl_df")
  expect_identical(
    names(lookup),
    c("information_system", "name", "file_acronym", "aliases")
  )
  expect_equal(nrow(lookup), 58L)
  expect_identical(anyDuplicated(lookup$information_system), 0L)
  expect_identical(anyDuplicated(lookup$file_acronym), 0L)
  expect_true(all(grepl(
    "^SINAN-[A-Z0-9]+(?:-[A-Z0-9]+)*$",
    lookup$information_system
  )))
  expect_identical(lookup$information_system, specs$information_system)
  expect_identical(lookup$name, specs$name)
  expect_identical(lookup$file_acronym, specs$acronym)
  expect_identical(anyDuplicated(aliases$alias), 0L)
  expect_identical(
    unname(vapply(
      specs$legacy_information_system,
      microdatasus:::.sinan_resolve_information_system,
      character(1)
    )),
    specs$information_system
  )

  for (index in seq_len(nrow(lookup))) {
    expected <- aliases$alias[
      aliases$information_system == lookup$information_system[[index]]
    ]
    expect_identical(lookup$aliases[[index]], expected)
    expect_true(all(vapply(
      expected,
      microdatasus:::.sinan_resolve_information_system,
      character(1)
    ) == lookup$information_system[[index]]))
  }

  expect_identical(
    lookup$information_system[lookup$file_acronym == "TUBE"],
    "SINAN-TUBERCULOSE"
  )
  expect_identical(
    lookup$name[lookup$file_acronym == "ANIM"],
    "Acidente por animais peçonhentos"
  )
  expect_true("SINAN-TUBE" %in%
    lookup$aliases[[match("TUBE", lookup$file_acronym)]])
})

test_that("SINAN registry covers all transfer-page families and definitions", {
  specs <- microdatasus:::.sinan_system_specs()
  downloads <- microdatasus:::.datasus_registry()
  dictionaries <- microdatasus:::.tabwin_registry()
  expected_acronyms <- c(
    "ANIM", "ANTR", "AIDA", "AIDC", "BOTU", "COLE", "COQU", "DENG",
    "DIFT", "DCRJ", "CHAG", "EXAN", "ESQU", "ESPO", "CHIK", "FMAC",
    "FTIF", "HANS", "HANT", "HEPA", "HIVA", "HIVC", "HIVE", "HIVG",
    "INFL", "IEXO", "LEIV", "LTAN", "LEPT", "MALA", "MENI", "PFAN",
    "PEST", "RAIV", "ROTA", "SIFA", "SIFC", "SIFG", "SRC", "SDTA",
    "TETA", "TETN", "TOXC", "TOXG", "NTRA", "TRAC", "TUBE", "VARC",
    "VIOL", "ZIKA", "ACBI", "ACGR", "CANC", "DERM", "LERD", "PAIR",
    "PNEU", "MENT"
  )

  expect_equal(nrow(specs), 58L)
  expect_setequal(specs$acronym, expected_acronyms)
  expect_true(all(specs$information_system %in% names(downloads)))
  expect_true(all(specs$information_system %in% names(dictionaries)))
  expect_identical(
    downloads[["SINAN-DENGUE"]]$repositories[[1L]]$prefix,
    "DENGBR"
  )
  expect_identical(
    vapply(
      downloads[["SINAN-LER-DORT"]]$repositories,
      `[[`,
      character(1),
      "prefix"
    ),
    c("LERDBR", "LERBR", "LERDBR")
  )
  expect_equal(sum(specs$archive == "SINAN-ONLINE"), 2L)
  expect_true(all(vapply(
    dictionaries[specs$information_system[specs$archive == "SINAN-ONLINE"]],
    function(spec) identical(basename(spec$url), "TAB_SINANONLINE.zip"),
    logical(1)
  )))
  expect_true(all(vapply(
    dictionaries[specs$information_system[specs$archive == "SINAN-NET"]],
    function(spec) identical(basename(spec$url), "TAB_SINANNET.zip"),
    logical(1)
  )))
  for (index in seq_len(nrow(specs))) {
    system <- specs$information_system[[index]]
    listing <- paste0(specs$prefix[[index]], "24.dbc\n")
    parsed <- microdatasus:::.datasus_parse_listing(
      listing,
      downloads[[system]]$repositories[[1L]],
      downloads[[system]]
    )
    expect_identical(parsed$period, "2024")
    expect_true(is.na(parsed$uf))
  }
})

test_that("old SINAN identifiers resolve silently and reuse canonical cache", {
  archives <- create_sinan_tabwin_fixtures()
  on.exit(unlink(unlist(archives)), add = TRUE)
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      downloads <<- downloads + 1L
      file.copy(archives$net, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  canonical <- fetch_tabwin_dictionary(
    "SINAN-TUBERCULOSE",
    quiet = TRUE
  )
  expect_no_warning(
    legacy <- fetch_tabwin_dictionary("SINAN-TUBE", quiet = TRUE)
  )
  expect_identical(legacy, canonical)
  expect_equal(downloads, 1L)

  messages <- capture_messages({
    expect_no_warning(result <- process_sinan(
      data.frame(FLAG = "1"),
      information_system = "SINAN-TUBE",
      municipality_data = FALSE
    ))
  })
  expect_identical(as.character(result$FLAG), "Rotulo SINAN")
  expect_true(any(grepl("Starting SINAN-TUBERCULOSE", messages)))
  expect_true(any(grepl("Finished SINAN-TUBERCULOSE", messages)))
})

test_that("all SINAN families use one of two shared archives", {
  archives <- create_sinan_tabwin_fixtures()
  on.exit(unlink(unlist(archives)), add = TRUE)
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      downloads <<- downloads + 1L
      source <- if (grepl("ONLINE", url, fixed = TRUE)) {
        archives$online
      } else {
        archives$net
      }
      file.copy(source, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  specs <- microdatasus:::.sinan_system_specs()
  for (information_system in microdatasus:::.sinan_information_systems()) {
    result <- process_sinan(
      data.frame(FLAG = "1"),
      information_system = information_system,
      municipality_data = FALSE
    )
    definition <- specs$definition[
      match(information_system, specs$information_system)
    ]
    expected <- if (tolower(definition) == "notindivinet.def") {
      "1"
    } else {
      "Rotulo SINAN"
    }
    expect_identical(as.character(result$FLAG), expected)
  }
  expect_equal(downloads, 2L)
})

test_that("generic SINAN definitions label only verified common fields", {
  definitions <- data.frame(
    field = c("CS_SEXO", "CLASSI_FIN", "CRITERIO", "EVOLUCAO"),
    stringsAsFactors = FALSE
  )
  dictionary <- list(
    definition = "TAB_SINANNET/NotIndiviNet.def",
    definitions = definitions
  )
  data <- data.frame(
    CS_SEXO = "M", CLASSI_FIN = "1", CRITERIO = "1", EVOLUCAO = "1"
  )
  types <- list(protected = character())

  fields <- microdatasus:::.sinan_dictionary_fields(data, dictionary, types)

  expect_identical(fields, "CS_SEXO")
})

test_that("process_sinan resolves both chikungunya classification domains", {
  archives <- create_sinan_tabwin_fixtures()
  on.exit(unlink(unlist(archives)), add = TRUE)
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      downloads <<- downloads + 1L
      source <- if (grepl("ONLINE", url, fixed = TRUE)) {
        archives[["online"]]
      } else {
        archives[["net"]]
      }
      file.copy(source, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  result <- process_sinan(
    data.frame(CLASSI_FIN = c("1", "2", "8", "5", "13")),
    information_system = "SINAN-FEBRE-DE-CHIKUNGUNYA",
    municipality_data = FALSE,
    diagnostics = TRUE
  )

  expect_s3_class(result[["CLASSI_FIN"]], "factor")
  expect_identical(
    as.character(result[["CLASSI_FIN"]]),
    c(
      "Confirmado historico", "Descartado historico",
      "Inconclusivo historico", "Descartado atual", "Chikungunya"
    )
  )
  report <- processing_diagnostics(result)
  expect_false("CLASSI_FIN" %in% report[["unmapped_fields"]])
  expect_false("CLASSI_FIN" %in% report[["unknown_codes"]][["field"]])
  expect_setequal(
    report[["dictionaries"]][["information_system"]],
    c("SINAN-FEBRE-DE-CHIKUNGUNYA", "SINAN-FEBRE-TIFOIDE")
  )
  expect_equal(downloads, 2L)
})

test_that("SINAN date roles use content and accept legacy date syntax", {
  existing_date <- as.Date(c("2024-01-01", "2024-01-02"))
  data <- data.frame(
    TRATAMENTO = c("1", "4"),
    DTRATA = c("20240101", "20240102"),
    ANT_DT_ACI = c("2024-01-03", "2024-01-04"),
    COLETAMARC = c("2024-01-05", "2024-01-06"),
    SEM_PRI = c("012024", "022024"),
    EXISTING = existing_date,
    stringsAsFactors = FALSE
  )
  attr(data, "dbf_field_types") <- c(
    TRATAMENTO = "C", DTRATA = "C", ANT_DT_ACI = "D",
    COLETAMARC = "D", SEM_PRI = "D", EXISTING = "C"
  )

  types <- microdatasus:::.sinan_type_fields(data, list())

  expect_false(any(c("TRATAMENTO", "SEM_PRI") %in% types$date))
  expect_true(all(c(
    "DTRATA", "ANT_DT_ACI", "COLETAMARC", "EXISTING"
  ) %in% types$date))

  collector <- microdatasus:::.process_diagnostic_collector(
    TRUE, "SINAN-TEST", data.frame(DATE = character())
  )
  parsed <- microdatasus:::.sinan_as_date(
    c("29/10/15", "02/03/10", "0", "********", "26/12"),
    collector, "DATE"
  )
  finalized <- microdatasus:::.process_finalize(
    data.frame(DATE = parsed), collector
  )
  failures <- processing_diagnostics(finalized)$coercion_failures

  expect_identical(
    as.character(parsed),
    c("2015-10-29", "2010-03-02", NA, NA, NA)
  )
  expect_identical(failures$value, "26/12")
  expect_identical(failures$n, 1L)
})

test_that("SINAN DEF increments take precedence over analytical groupings", {
  data <- data.frame(
    NU_LESOES = c("01", "12"),
    NERVOSAFET = c("0", "4"),
    FLAG = c("1", "2"),
    stringsAsFactors = FALSE
  )
  dictionary <- list(
    numeric_fields = c("NU_LESOES", "NERVOSAFET"),
    definitions = data.frame(
      field = c("NU_LESOES", "NERVOSAFET", "FLAG"),
      stringsAsFactors = FALSE
    ),
    definition = "HansNET.def"
  )

  types <- microdatasus:::.sinan_type_fields(data, dictionary)
  categorical <- microdatasus:::.sinan_dictionary_fields(
    data, dictionary, types
  )

  expect_setequal(types[["integer"]], c("NU_LESOES", "NERVOSAFET"))
  expect_true(all(types[["integer"]] %in% types[["protected"]]))
  expect_identical(categorical, "FLAG")
})

test_that("SINAN meningitis preserves other-vaccine descriptions", {
  data <- data.frame(
    ANT_OU_DE = c("01", "03 HEPATITE", "HEPATITE B", NA_character_),
    stringsAsFactors = FALSE
  )
  dictionary <- list(
    numeric_fields = character(),
    definitions = data.frame(
      field = "ANT_OU_DE",
      stringsAsFactors = FALSE
    ),
    definition = "MeningeNET.def"
  )

  types <- microdatasus:::.sinan_type_fields(data, dictionary)
  categorical <- microdatasus:::.sinan_dictionary_fields(
    data,
    dictionary,
    types
  )

  expect_identical(types$identifier, "ANT_OU_DE")
  expect_identical(types$protected, "ANT_OU_DE")
  expect_length(categorical, 0L)
})

test_that("SINAN recovered official increment names remain numeric", {
  tuberculosis <- tibble::tibble(NU_CONTATO = c("0", "14", "99"))
  diphtheria <- tibble::tibble(MED_QUAN_P = c("0", "2", NA_character_))

  tuberculosis_dictionary <- list(
    numeric_fields = c("NU_CONTATO", "NU_COMU_EX")
  )
  diphtheria_dictionary <- list(numeric_fields = "MED_QUAN_P")

  expect_identical(
    microdatasus:::.sinan_type_fields(
      tuberculosis, tuberculosis_dictionary
    )$integer,
    "NU_CONTATO"
  )
  expect_identical(
    microdatasus:::.sinan_type_fields(
      diphtheria, diphtheria_dictionary
    )$integer,
    "MED_QUAN_P"
  )
})

test_that("process_sinan standardizes common types and message order", {
  archives <- create_sinan_tabwin_fixtures()
  on.exit(unlink(unlist(archives)), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      file.copy(archives$online, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  treatment <- as.Date(c("2024-02-02", "2024-02-03"))
  source <- data.frame(
    DT_NOTIFIC = c("2024-01-31", "20240201"),
    DEXAME = c("20240202", "2024-02-03"),
    TRATAMENTO = treatment,
    NU_ANO = c("2024", "2024"),
    CONTEXAM = c("02", "3"),
    NU_IDADE_N = c("4116", "3023"),
    NU_NOTIFIC = c("0000001", "0000002"),
    ID_MN_RESI = c("1200209", "1200308"),
    CS_SEXO = c("M", "X"),
    stringsAsFactors = FALSE
  )
  messages <- capture_messages({
    result <- process_sinan(
      source,
      information_system = "SINAN-DENGUE",
      municipality_data = FALSE
    )
  })

  expect_s3_class(result$DT_NOTIFIC, "Date")
  expect_s3_class(result$DEXAME, "Date")
  expect_identical(result$TRATAMENTO, treatment)
  expect_type(result[["NU_ANO"]], "integer")
  expect_identical(result[["CONTEXAM"]], c(2L, 3L))
  expect_identical(result$NU_IDADE_N, c("4116", "3023"))
  expect_identical(result$IDADEanos, c(116L, NA_integer_))
  expect_identical(result$IDADEmeses, c(NA_integer_, 23L))
  expect_identical(result$NU_NOTIFIC, c("0000001", "0000002"))
  expect_identical(result$ID_MN_RESI, c("120020", "120030"))
  expect_s3_class(result$CS_SEXO, "factor")
  expect_identical(as.character(result$CS_SEXO), c("Masculino", "X"))
  expect_lt(
    which(grepl("Cached.+SINAN-DENGUE", messages)),
    which(grepl("Starting SINAN-DENGUE", messages))
  )
  expect_lt(
    which(grepl("Starting SINAN-DENGUE", messages)),
    which(grepl("Finished SINAN-DENGUE", messages))
  )
})

test_that("legacy SINAN wrappers preserve signatures and emit deprecation", {
  wrappers <- c(
    process_sinan_chagas = "SINAN-DOENCA-DE-CHAGAS-AGUDA",
    process_sinan_chikungunya = "SINAN-FEBRE-DE-CHIKUNGUNYA",
    process_sinan_dengue = "SINAN-DENGUE",
    process_sinan_leishmaniose_tegumentar =
      "SINAN-LEISHMANIOSE-TEGUMENTAR",
    process_sinan_leishmaniose_visceral = "SINAN-LEISHMANIOSE-VISCERAL",
    process_sinan_malaria = "SINAN-MALARIA",
    process_sinan_zika = "SINAN-ZIKA-VIRUS"
  )
  expected_formals <- as.pairlist(alist(
    data = ,
    municipality_data = TRUE
  ))
  for (wrapper in names(wrappers)) {
    fun <- get(wrapper, envir = asNamespace("microdatasus"))
    expect_identical(formals(fun), expected_formals)
    expect_warning(
      result <- fun(
        data.frame(NU_IDADE_N = "4029"),
        municipality_data = FALSE
      ),
      class = "microdatasus_sinan_deprecated"
    )
    expect_s3_class(result, "tbl_df")
    expect_identical(result$IDADEanos, 29L)
  }
})

test_that("process_sinan validates all public arguments", {
  expect_error(process_sinan(NULL), "must be a data frame")
  expect_error(
    process_sinan(data.frame(), information_system = "SINAN-UNKNOWN"),
    "must be one of"
  )
  expect_error(
    process_sinan(data.frame(), municipality_data = NA),
    "TRUE.*FALSE"
  )
})
