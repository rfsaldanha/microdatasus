test_that("sinan-zika-final at 2016", {
  sinan_zika_final <- fetch_datasus(year_start = 2016, year_end = 2016, information_system = "SINAN-ZIKA-FINAL")
  sinan_zika_final <- process_sinan_zika(sinan_zika_final, municipality_data = TRUE)
})

test_that("sinan-dengue-preliminar at 2020", {
  sinan_zika_preliminar <- fetch_datasus(year_start = 2020, year_end = 2020, information_system = "SINAN-ZIKA-PRELIMINAR")
  sinan_zika_preliminar <- process_sinan_zika(sinan_zika_preliminar, municipality_data = TRUE)
})
