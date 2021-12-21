test_that("sinan-chikungunya-final at 2016", {
  sinan_dengue_final <- fetch_datasus(year_start = 2016, year_end = 2016, information_system = "SINAN-CHIKUNGUNYA-FINAL")
  sinan_dengue_final <- process_sinan_chikungunya(sinan_dengue_final, municipality_data = TRUE)
})

test_that("sinan-chikungunya-preliminar at 2020", {
  sinan_dengue_preliminar <- fetch_datasus(year_start = 2020, year_end = 2020, information_system = "SINAN-CHIKUNGUNYA-PRELIMINAR")
  sinan_dengue_preliminar <- process_sinan_chikungunya(sinan_dengue_preliminar, municipality_data = TRUE)
})
