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
    .tabwin_select_conversion = function(dictionary, field, values) {
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
