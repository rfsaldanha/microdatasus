test_that("sih rd works at 2016, 6-7", {
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIH-RD")
  sih <- process_sih(data = sih, information_system = "SIH-RD", municipality_data = TRUE)
  expect_true(class(sih) == "data.frame")
})

test_that("sih rj works at 2016, 6-7", {
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIH-RJ")
  expect_true(class(sih) == "data.frame")
})

test_that("sih sp works at 2016, 6-7", {
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIH-SP")
  expect_true(class(sih) == "data.frame")
})

test_that("sih er works at 2016, 6-7", {
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIH-ER")
  expect_true(class(sih) == "data.frame")
})

test_that("sih rd works with two UFs", {
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = c("AC", "AP"), information_system = "SIH-RD")
  sih <- process_sih(data = sih, information_system = "SIH-RD", municipality_data = TRUE)
  expect_true(class(sih) == "data.frame")
})


