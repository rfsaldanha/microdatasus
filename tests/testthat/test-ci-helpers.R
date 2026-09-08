source(test_path("..", "support", "ci-helpers.R"), local = TRUE)

test_that("coverage gates accept complete reports at the exact limits", {
  expect_true(.ci_check_coverage(92.25, c("R/a.R" = 80, "R/b.R" = 100),
                                 c("R/a.R", "R/b.R")))
})

test_that("coverage gates fail closed for missing or invalid critical entries", {
  expect_error(.ci_check_coverage(95, c("R/a.R" = 95), c("R/a.R", "R/b.R")),
               "missing.*R/b.R")
  for (value in c(NA_real_, NaN, Inf, -Inf, -1, 101)) {
    expect_error(.ci_check_coverage(95, c("R/a.R" = value), "R/a.R"),
                 "Invalid.*R/a.R")
  }
  expect_error(.ci_check_coverage(95, c("R/a.R" = 79.99), "R/a.R"),
               "below 80%.*R/a.R")
  expect_error(.ci_check_coverage(92.24, c("R/a.R" = 100), "R/a.R"), "Total")
  for (value in c(NA_real_, NaN, Inf, -Inf, -1, 101)) {
    expect_error(.ci_check_coverage(value, c("R/a.R" = 95), "R/a.R"),
                 "finite percentages")
  }
})

test_that("coverage gates reject malformed reports and critical-file lists", {
  for (report in list(numeric(), 95, c("R/a.R" = 95, "R/a.R" = 95),
                      setNames(95, ""), setNames(95, NA_character_), c("R/a.R" = "95"))) {
    expect_error(.ci_check_coverage(95, report, "R/a.R"), "Per-file coverage")
  }
  for (critical in list(character(), NA_character_, "", c("R/a.R", "R/a.R"))) {
    expect_error(.ci_check_coverage(95, c("R/a.R" = 95), critical), "Critical coverage")
  }
  expect_error(.ci_check_coverage(95, c("R/a.R" = 95), "R/a.R", min_total = NA),
               "finite percentages")
})

test_that("weekly smoke rotation reaches every registered system across years", {
  metadata <- datasus_information_systems()
  families <- split(metadata$information_system, metadata$system)
  for (systems in families) {
    dates <- as.Date("2025-12-29") + 7L * seq.int(0L, length(systems) - 1L)
    selected <- vapply(seq_along(dates), function(index) {
      .ci_rotating_system(systems, dates[index])
    }, character(1))
    expect_setequal(selected, systems)
    expect_false(anyDuplicated(selected) > 0L)
    expect_identical(
      .ci_rotating_system(systems, dates[1L] + 7L * length(systems)),
      selected[[1L]]
    )
  }
})

test_that("weekly rotation stays stable within a week and spans ISO leap weeks", {
  systems <- sprintf("fixture-%02d", 1:58)
  monday <- as.Date("2020-12-28")
  selected <- vapply(0:6, function(day) {
    .ci_rotating_system(systems, monday + day)
  }, character(1))
  expect_length(unique(selected), 1L)
  expect_false(identical(selected[[1L]], .ci_rotating_system(systems, monday + 7)))
  expect_identical(.ci_rotating_system("single", monday), "single")
})

test_that("weekly rotation rejects invalid candidates and dates", {
  for (systems in list(character(), NA_character_, "", c("a", "a"), 1:2)) {
    expect_error(.ci_rotating_system(systems), "identifiers")
  }
  for (date in list(Sys.Date()[FALSE], as.Date(NA), as.Date(Inf, origin = "1970-01-01"),
                    c(Sys.Date(), Sys.Date()), "2026-01-01")) {
    expect_error(.ci_rotating_system("a", date), "Date")
  }
})
