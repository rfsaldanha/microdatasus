test_that("schema validation joins raw, dictionary, and processed fields", {
  local_mocked_bindings(
    datasus_variables = function(information_system, ...) {
      tibble::tibble(
        information_system = information_system,
        archive_checksum = paste0("checksum-", information_system),
        field = c("DTOBITO", "SEXO", "DECLARED_ONLY"),
        type = c("categorical", "categorical", "numeric")
      )
    },
    .datasus_process_file = function(data, ...) {
      data$DTOBITO <- as.Date(data$DTOBITO, format = "%d%m%Y")
      data$IDADEanos <- 1L
      tibble::as_tibble(data)
    },
    .package = "microdatasus"
  )
  raw <- data.frame(
    DTOBITO = "01012020", SEXO = "1", RAW_ONLY = "x",
    stringsAsFactors = FALSE
  )

  contract <- validate_datasus_schema(
    raw, "SIM-DO", period = "2020", quiet = TRUE
  )

  expect_setequal(
    contract$status,
    c("matched", "observed_only", "dictionary_only", "processor_added")
  )
  expect_identical(contract$processed_type[contract$field == "DTOBITO"], "Date")
  expect_true(contract$type_changed[contract$field == "DTOBITO"])
  expect_identical(contract$period[[1L]], "2020")
  expect_true(all(lengths(contract$dictionary_keys[contract$dictionary_declared]) > 0L))
})

test_that("schema contracts select every represented historical definition", {
  sia <- data.frame(PA_MVM = c("9407", "199911", "200308", "200801"))
  sih <- data.frame(
    ANO_CMPT = c(1997, 2003, 2003, 2008), MES_CMPT = c(12, 7, 8, 1)
  )
  cnes <- data.frame(COMPETEN = c(200802, 200803))
  sinasc <- data.frame(DTNASC = "01012020", DATA_NASC = "01011995")

  expect_setequal(
    microdatasus:::.datasus_contract_dictionary_keys(sia, "SIA-PA"),
    c(
      "SIA-PA-1994-07-1999-10", "SIA-PA-1999-11-2003-07",
      "SIA-PA-2003-08-2007", "SIA-PA"
    )
  )
  expect_setequal(
    microdatasus:::.datasus_contract_dictionary_keys(sih, "SIH-RD"),
    c("SIH-RD-1992-1997", "SIH-RD-1998-2003-07",
      "SIH-RD-2003-08-2007", "SIH-RD")
  )
  expect_setequal(
    microdatasus:::.datasus_contract_dictionary_keys(cnes, "CNES-SR"),
    c("CNES-SR-2005-08-2008-02", "CNES-SR")
  )
  expect_setequal(
    microdatasus:::.datasus_contract_dictionary_keys(sinasc, "SINASC"),
    c("SINASC", "SINASC-1994-1995")
  )
})

test_that("every public family resolves to registered contract dictionaries", {
  public <- names(microdatasus:::.datasus_registry())
  tabwin <- names(microdatasus:::.tabwin_registry())
  for (information_system in public) {
    keys <- microdatasus:::.datasus_contract_dictionary_keys(
      data.frame(), information_system
    )
    expect_true(all(keys %in% tabwin), info = information_system)
  }
})

test_that("schema validation checks public arguments", {
  expect_error(validate_datasus_schema(1, "SIM-DO"), "data frame")
  expect_error(
    validate_datasus_schema(data.frame(), "NOT-A-SYSTEM"),
    "supported data family"
  )
  expect_error(
    validate_datasus_schema(data.frame(), "SIM-DO", period = c(1, 2)),
    "period"
  )
})


test_that("schema contract helpers cover supported classes and processor options", {
  values <- list(
    as.Date("2020-01-01"), factor("a"), 1L, 1, "a", TRUE,
    structure(list(1), class = "custom_class")
  )
  expect_identical(
    vapply(values, microdatasus:::.datasus_contract_type, character(1)),
    c("Date", "factor", "integer", "double", "character", "logical",
      "custom_class")
  )
  sia <- microdatasus:::.datasus_contract_process_args("SIA-PA")
  expect_false(sia$nome_proced)
  expect_false(sia$nome_ocupacao)
  expect_false(sia$nome_equipe)
  expect_false(microdatasus:::.datasus_contract_process_args("CNES-ST")$nomes)
  expect_null(microdatasus:::.datasus_contract_process_args("SIM-DO")$nomes)
})

test_that("schema validation can inspect without running the processor", {
  local_mocked_bindings(
    datasus_variables = function(information_system, ...) {
      tibble::tibble(
        archive_checksum = "checksum", field = c("SEXO", "ONLY_DEF"),
        type = c("categorical", "numeric")
      )
    },
    .datasus_process_file = function(...) stop("processor should not run"),
    .package = "microdatasus"
  )
  contract <- validate_datasus_schema(
    data.frame(SEXO = "1"), "SIM-DO", process = FALSE, quiet = TRUE
  )
  expect_false(any(contract$processed))
  expect_true(all(is.na(contract$processed_type)))
  expect_identical(contract$status, c("matched", "dictionary_only"))
})

test_that("schema validation reads a single local DBC path", {
  local_mocked_bindings(
    read_dbc = function(file, as_character) {
      expect_identical(file, "sample.dbc")
      expect_false(as_character)
      data.frame(SEXO = "1")
    },
    datasus_variables = function(...) {
      tibble::tibble(
        archive_checksum = "checksum", field = "SEXO", type = "categorical"
      )
    },
    .package = "microdatasus"
  )
  contract <- validate_datasus_schema(
    "sample.dbc", "SIM-DO", process = FALSE, quiet = TRUE
  )
  expect_identical(contract$field, "SEXO")
  expect_identical(contract$status, "matched")
})

test_that("schema validation rejects malformed scalar arguments", {
  for (value in list(NULL, NA_character_, "", c("SIM-DO", "SIM-DO"), 1)) {
    expect_error(
      validate_datasus_schema(data.frame(), value),
      "one supported data family"
    )
  }
  expect_error(
    validate_datasus_schema(data.frame(), "SIM-DO", process = NA),
    "process"
  )
  expect_error(
    validate_datasus_schema(data.frame(), "SIM-DO", period = NA),
    "period"
  )
})
