test_that("sinan-dengue-final at 2016", {
  sinan_dengue_final <- fetch_datasus(year_start = 2016, year_end = 2016, information_system = "SINAN-DENGUE-FINAL")
  sinan_dengue_final <- process_sinan_dengue(sinan_dengue_final, municipality_data = TRUE)
})

test_that("sinan-dengue-preliminar at 2020", {
  sinan_dengue_preliminar <- fetch_datasus(year_start = 2021, year_end = 2021, information_system = "SINAN-DENGUE-PRELIMINAR")
  sinan_dengue_preliminar <- process_sinan_dengue(sinan_dengue_preliminar, municipality_data = TRUE)
})
