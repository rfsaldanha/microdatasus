make_selected_conversion <- function(map) {
  list(
    definition = data.frame(position = 1L),
    conversion = list(
      type = "cnv",
      code_width = 1L,
      map = map
    )
  )
}

test_that("numeric helpers preserve values while using their direct path", {
  expect_identical(
    microdatasus:::.process_as_integer(c(" 12", "001", NA_character_)),
    c(12L, 1L, NA_integer_)
  )
  expect_identical(
    microdatasus:::.process_as_integer(factor(c("001", " 12"))),
    c(1L, 12L)
  )
  expect_identical(
    microdatasus:::.process_as_integer(c(" 99", "12"), missing = "99"),
    c(NA_integer_, 12L)
  )
  expect_identical(
    microdatasus:::.process_as_double(c(" 10.25", "001", NA_character_)),
    c(10.25, 1, NA_real_)
  )
  expect_identical(
    microdatasus:::.process_as_double(factor(c("1.5", " 12"))),
    c(1.5, 12)
  )
  expect_identical(
    microdatasus:::.process_as_integer(c(TRUE, FALSE)),
    c(NA_integer_, NA_integer_)
  )
  expect_identical(
    microdatasus:::.process_as_double(c(TRUE, FALSE)),
    c(NA_real_, NA_real_)
  )
})

test_that("text normalization preserves byte identifiers losslessly", {
  identifier <- rawToChar(as.raw(c(0x81, 0x90, 0xff)))
  Encoding(identifier) <- "bytes"
  source <- data.frame(
    VALUE = c(identifier, "S\u00e3o", NA_character_),
    stringsAsFactors = FALSE
  )

  result <- microdatasus:::.process_normalize_text(source)

  expect_identical(charToRaw(result$VALUE[[1L]]), charToRaw(identifier))
  expect_identical(Encoding(result$VALUE[[1L]]), "bytes")
  expect_identical(result$VALUE[[2L]], "São")
  expect_true(is.na(result$VALUE[[3L]]))
})

test_that("historical SIH dates resolve centuries from competence", {
  result <- microdatasus:::.process_as_sih_date(
    c("231102", "920101", "000000", "invalid", "20000102"),
    c("1992", "1992", "1992", "1992", NA_character_)
  )

  expect_identical(
    as.character(result),
    c("1923-11-02", "1992-01-01", NA, NA, "2000-01-02")
  )
})

test_that("batched dictionaries preserve row-specific conversion results", {
  old <- list(
    selected = make_selected_conversion(
      c("1" = "Old one", "2" = "Old two")
    )
  )
  current <- list(
    selected = make_selected_conversion(
      c("1" = "Current one", "2" = "Current two")
    )
  )
  dictionaries <- list(old = old, current = current)
  dictionary_rows <- list(old = 1:2, current = 3:4)
  source <- data.frame(
    FIRST = c("1", "2", "9", "1"),
    SECOND = c("2", "1", "9", "2"),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    .tabwin_select_conversion = function(dictionary, field, values, ...) {
      dictionary$selected
    },
    .package = "microdatasus"
  )

  batched <- microdatasus:::.process_apply_dictionaries(
    source,
    dictionaries,
    c("FIRST", "SECOND"),
    dictionary_rows
  )

  expect_identical(
    as.character(batched$FIRST),
    c("Old one", "Old two", "9", "Current one")
  )
  expect_identical(
    as.character(batched$SECOND),
    c("Old two", "Old one", "9", "Current two")
  )
  expect_identical(
    levels(batched$FIRST),
    c("Old one", "Old two", "Current one", "9")
  )
  expect_identical(
    levels(batched$SECOND),
    c("Old one", "Old two", "Current two", "9")
  )
})

test_that("dictionary substrings never truncate unmatched source values", {
  selected <- make_selected_conversion(c("1" = "Known"))
  selected$source_values <- c("1", "9")
  dictionary <- list(selected = selected)
  source <- data.frame(
    CODE = c("known composite value", "complete unknown value"),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    .tabwin_select_conversion = function(dictionary, field, values, ...) {
      dictionary$selected
    },
    .package = "microdatasus"
  )

  result <- microdatasus:::.process_apply_dictionaries(
    source,
    list(test = dictionary),
    "CODE",
    labels = "character"
  )

  expect_identical(result$CODE, c("Known", "complete unknown value"))
})

test_that("dictionary value aliases preserve the source code policy", {
  dictionary <- list(selected = make_selected_conversion(c("1" = "Known")))
  source <- data.frame(
    CODE = c("Already labelled", "9"),
    stringsAsFactors = FALSE
  )
  selected_values <- character()
  local_mocked_bindings(
    .tabwin_select_conversion = function(dictionary, field, values, ...) {
      selected_values <<- values
      dictionary$selected
    },
    .package = "microdatasus"
  )
  aliases <- list(CODE = c("Already labelled" = "1"))

  labelled <- microdatasus:::.process_apply_dictionaries(
    source,
    list(test = dictionary),
    "CODE",
    labels = "character",
    value_aliases = aliases
  )
  codes <- microdatasus:::.process_apply_dictionaries(
    source,
    list(test = dictionary),
    "CODE",
    labels = "none",
    value_aliases = aliases
  )

  expect_identical(selected_values, c("1", "9"))
  expect_identical(labelled$CODE, c("Known", "9"))
  expect_identical(codes$CODE, source$CODE)
})

test_that("batched dictionaries leave data unchanged when none are supplied", {
  source <- data.frame(CODE = c("1", "2"), stringsAsFactors = FALSE)

  expect_identical(
    microdatasus:::.process_apply_dictionaries(
      source,
      list(),
      "CODE"
    ),
    source
  )
})

test_that("empty tables do not report unevaluated fields as unmapped", {
  source <- data.frame(CODE = character(), stringsAsFactors = FALSE)
  dictionary <- list(
    information_system = "TEST",
    definitions = data.frame(field = "CODE"),
    selected = make_selected_conversion(c("1" = "One"))
  )
  collector <- microdatasus:::.process_diagnostic_collector(
    TRUE, "TEST", source
  )
  local_mocked_bindings(
    .tabwin_select_conversion = function(...) {
      stop("an empty field must not be evaluated")
    },
    .package = "microdatasus"
  )

  result <- microdatasus:::.process_apply_dictionaries(
    source,
    list(test = dictionary),
    "CODE",
    collector = collector
  )
  result <- microdatasus:::.process_finalize(result, collector)
  report <- processing_diagnostics(result)

  expect_identical(nrow(result), 0L)
  expect_length(report$mapped_fields, 0L)
  expect_length(report$unmapped_fields, 0L)
})
