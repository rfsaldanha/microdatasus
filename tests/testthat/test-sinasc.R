test_that("sinasc works", {
  sinasc_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SINASC")
  sinasc <- process_sinasc(data = sinasc_raw, municipality_data = TRUE)
  expect_true(class(sinasc)[1] == "tbl_df")
})

