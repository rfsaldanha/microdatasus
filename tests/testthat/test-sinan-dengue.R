test_that("sinan-dengue at 2016", {
  sinan_dengue <- fetch_datasus(year_start = 2010, year_end = 2010, information_system = "SINAN-DENGUE")
  sinan_dengue <- process_sinan_dengue(sinan_dengue, municipality_data = TRUE)
})
