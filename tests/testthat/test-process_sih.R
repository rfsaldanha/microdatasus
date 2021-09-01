test_that("process_sih works", {
  sih <- fetch_datasus(year_start = 2016, month_start = 1, year_end = 2016, month_end = 1, uf = "AC", information_system = "SIH-RD")
  sih <- process_sih(data = sih, information_system = "SIH-RD", municipality_data = TRUE)
  expect_true(class(sih) == "data.frame")
})
