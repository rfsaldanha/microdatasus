test_that("processed binding preserves the independent column and type contract", {
  first <- data.frame(f = factor(c("b", "a"), levels = c("a", "b")),
                      date = as.Date(c("2020-01-01", NA)))
  second <- data.frame(extra = 1L, f = "c", date = as.Date("2021-01-01"))
  result <- microdatasus:::.datasus_bind_processed(list(first, second))
  expect_identical(names(result), c("f", "date", "extra"))
  expect_identical(result$f, c("b", "a", "c"))
  expect_identical(result$extra, c(NA_integer_, NA_integer_, 1L))
  expect_s3_class(result$date, "Date")
  second$f <- factor("c", levels = c("c", "a"))
  result <- microdatasus:::.datasus_bind_processed(list(first, second))
  expect_identical(levels(result$f), c("a", "b", "c"))
  expect_identical(as.character(result$f), c("b", "a", "c"))
})

test_that("source context restores physical date types and rejects stale rows", {
  first <- data.frame(ATE_INTERN = "2018-01-03", PRE_MUNIRE = "010001",
                      stringsAsFactors = FALSE)
  second <- data.frame(PRE_MUNIRE = "354260", ATE_INTERN = "2019-01-01",
                       EXTRA = "x", stringsAsFactors = FALSE)
  for (part in c("first", "second")) {
    data <- get(part)
    attr(data, "dbf_field_types") <- setNames(
      ifelse(names(data) == "ATE_INTERN", "D", "C"), names(data))
    attr(data, "dbf_field_widths") <- setNames(rep(8L, ncol(data)), names(data))
    assign(part, data)
  }
  raw <- microdatasus:::.datasus_bind_raw(list(first, second), "SINAN-MENINGITE")
  for (labels in c("factor", "character", "none")) {
    result <- process_sinan(raw, "SINAN-MENINGITE", FALSE, labels, TRUE)
    expect_identical(result$ATE_INTERN, as.Date(c("2018-01-03", "2019-01-01")))
    expect_identical(result$PRE_MUNIRE, c("010001", "354260"))
    expect_identical(processing_diagnostics(result)$input_rows, 2L)
    expect_null(attr(result, "microdatasus_sources"))
  }
  for (bad in list(raw[2:1, ], raw[1, ], raw[, 1:2])) {
    expect_error(process_sinan(bad, "SINAN-MENINGITE", FALSE),
                 class = "microdatasus_source_context_error")
  }
  expect_error(process_sinan(raw, "SINAN-DENGUE", FALSE),
               class = "microdatasus_source_context_error")
})

test_that("every processor accepts source context and merges diagnostics once", {
  systems <- c("SIM-DO", "SINASC", "SIH-RD", "SIA-PA", "CNES-ST", "SINAN-DENGUE")
  for (system in systems) {
    parts <- list(data.frame(SEXO = "1", source = "first"),
                  data.frame(SEXO = "2", EXTRA = "x", source = "second"))
    raw <- microdatasus:::.datasus_bind_raw(parts, system)
    attr(raw, "microdatasus_provenance") <- data.frame(file = c("first", "second"))
    args <- list(municipality_data = FALSE, labels = "none", diagnostics = TRUE)
    if (system == "SIA-PA") args <- c(args, list(nome_proced = FALSE, nome_ocupacao = FALSE, nome_equipe = FALSE))
    expected <- microdatasus:::.datasus_bind_processed(lapply(parts, function(x) {
      microdatasus:::.datasus_process_file(x, system, args)
    }))
    actual <- microdatasus:::.datasus_process_file(raw, system, args)
    expect_identical(names(actual), names(expected), info = system)
    for (field in names(expected)) expect_identical(actual[[field]], expected[[field]], info = system)
    expect_identical(processing_diagnostics(actual)$input_rows, 2L)
    expect_identical(processing_diagnostics(actual)$output_rows, 2L)
    expect_identical(datasus_provenance(actual), datasus_provenance(raw))
  }
})

test_that("municipal PRE_MUNIRE codes cannot compete with UF conversions", {
  local_mocked_bindings(
    .tabwin_select_conversion = function(dictionary, field, ...) {
      if (field == "PRE_MUNIRE") stop("Municipal identifier reached categorical conversion")
      NULL
    }, .package = "microdatasus"
  )
  # Literal municipal identifiers from the stress files; preserve six digits,
  # including leading zeros and unknown codes, rather than applying UF labels.
  raw <- data.frame(PRE_MUNIRE = c("354260", "410830", "010001", NA, "999999"))
  for (labels in c("factor", "character", "none")) {
    result <- process_sinan(raw, "SINAN-SIFILIS-EM-GESTANTE", FALSE, labels)
    expect_identical(result$PRE_MUNIRE, raw$PRE_MUNIRE)
  }
})

test_that("AMB_NSUS retains its four physical flag fields after aggregation", {
  path <- test_path("..", "fixtures", "stress", "AMB_HOSP.CNV")
  expect_identical(digest::digest(path, algo = "sha256", file = TRUE),
                   "d69f9f2383b0234d981b1385c47acd68a1047e0301223c891c080795acce89c3")
  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  definition <- data.frame(field = "AMB_NSUS", position = 1L, extension = "CNV")
  local_mocked_bindings(.tabwin_select_conversion = function(dictionary, field, values, data, source_field, rows) {
    if (field != "AMB_NSUS") return(NULL)
    list(definition = definition, conversion = conversion,
         source_values = microdatasus:::.tabwin_definition_values(
           data, field, definition, conversion, values, rows))
  }, .package = "microdatasus")
  # First records of SRAC0802/SRAC0803: literal flags and physical widths.
  parts <- list(
    data.frame(AMB_NSUS = "0", AMB_SUS = "1", HOSP_NSUS = "0", HOSP_SUS = "0", COMPETEN = "200802"),
    data.frame(AMB_NSUS = "1", AMB_SUS = "0", HOSP_NSUS = "0", HOSP_SUS = "0", COMPETEN = "200803")
  )
  for (i in seq_along(parts)) {
    attr(parts[[i]], "dbf_field_types") <- setNames(rep("C", 5), names(parts[[i]]))
    attr(parts[[i]], "dbf_field_widths") <- setNames(c(1L, 1L, 1L, 1L, 6L), names(parts[[i]]))
  }
  # Supply only the relevant declaration; conversion contents remain official.
  for (key in c("CNES-SR", "CNES-SR-2005-08-2008-02")) {
    dictionary <- .empty_tabwin_dictionary(key)
    dictionary$definitions <- definition
    assign(paste(key, "session", sep = "::"), dictionary, microdatasus:::.tabwin_cache)
  }
  withr::defer(restore_empty_tabwin_cache())
  raw <- microdatasus:::.datasus_bind_raw(parts, "CNES-SR")
  # Under the documented last-physical-rule policy, official category 17
  # overrides categories 1/3 with its 0000-9999 interval. These expectations
  # are transcribed from that rule, not produced by either processing path.
  expected <- rep("ESPAÇOS, Zeros ou Outros Códigos Inválidos", 2L)
  for (labels in c("factor", "character", "none")) {
    result <- process_cnes(raw, "CNES-SR", municipality_data = FALSE, labels = labels)
    expect_identical(as.character(result$AMB_NSUS), if (labels == "none") c("0", "1") else expected)
    if (labels == "factor") expect_identical(levels(result$AMB_NSUS), unique(expected))
  }
})
