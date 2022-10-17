test_that("sinan-zika at 2016", {
  sinan_zika <- fetch_datasus(year_start = 2016, year_end = 2016, information_system = "SINAN-ZIKA")
  sinan_zika <- process_sinan_zika(sinan_zika, municipality_data = TRUE)
})
