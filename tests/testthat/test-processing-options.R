test_that("label policy preserves factor default and supports alternatives", {
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = list(
      type = "cnv",
      code_width = 1L,
      map = c("1" = "Masculino")
    )
  )
  local_mocked_bindings(
    fetch_tabwin_dictionary = function(...) list(),
    .tabwin_select_conversion = function(dictionary, field, values, ...) selected,
    .package = "microdatasus"
  )
  source <- data.frame(SEXO = c("1", "9"), stringsAsFactors = FALSE)

  factor_result <- process_sim(source, municipality_data = FALSE)
  character_result <- process_sim(
    source,
    municipality_data = FALSE,
    labels = "character"
  )
  code_result <- process_sim(
    source,
    municipality_data = FALSE,
    labels = "none"
  )

  expect_s3_class(factor_result$SEXO, "factor")
  expect_identical(as.character(factor_result$SEXO), c("Masculino", "9"))
  expect_identical(character_result$SEXO, c("Masculino", "9"))
  expect_identical(code_result$SEXO, c("1", "9"))
})

test_that("processing diagnostics report unknown dictionary codes", {
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = list(
      type = "cnv",
      code_width = 1L,
      map = c("1" = "Masculino")
    )
  )
  local_mocked_bindings(
    fetch_tabwin_dictionary = function(...) list(),
    .tabwin_select_conversion = function(dictionary, field, values, ...) selected,
    .package = "microdatasus"
  )

  result <- process_sim(
    data.frame(SEXO = c("1", "9", "9")),
    municipality_data = FALSE,
    labels = "none",
    diagnostics = TRUE
  )
  report <- processing_diagnostics(result)

  expect_s3_class(report, "microdatasus_processing_diagnostics")
  expect_identical(report$information_system, "SIM-DO")
  expect_identical(report$mapped_fields, "SEXO")
  expect_identical(report$unknown_codes$field, "SEXO")
  expect_identical(report$unknown_codes$code, "9")
  expect_identical(report$unknown_codes$n, 2L)
})

test_that("processing diagnostics record coercion failures and provenance", {
  result <- process_sim(
    data.frame(DTOBITO = c("01012024", "not-a-date"),
               CONTADOR = c("1", "not-a-number")),
    municipality_data = FALSE, labels = "none", diagnostics = TRUE
  )
  report <- processing_diagnostics(result)

  expect_setequal(report$coercion_failures$field, c("DTOBITO", "CONTADOR"))
  expect_setequal(report$coercion_failures$target, c("Date", "integer"))
  expect_true(all(c("dictionaries", "missing_expected_fields",
                    "package_version") %in% names(report)))
})

test_that("all processors append the common options compatibly", {
  processors <- list(
    process_sim,
    process_sinasc,
    process_sih,
    process_sia,
    process_cnes,
    process_sinan
  )
  for (processor in processors) {
    arguments <- formals(processor)
    expect_identical(
      tail(names(arguments), 2L),
      c("labels", "diagnostics")
    )
    expect_identical(arguments$diagnostics, FALSE)
  }
})

test_that("processing options reject invalid values", {
  expect_error(
    process_sim(data.frame(), labels = "labelled"),
    "arg"
  )
  expect_error(
    process_sim(data.frame(), diagnostics = NA),
    "diagnostics"
  )
})
