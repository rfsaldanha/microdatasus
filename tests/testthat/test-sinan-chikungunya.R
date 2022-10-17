test_that("sinan-chikungunya at 2016", {
  sinan_chikungunya <- fetch_datasus(year_start = 2016, year_end = 2016, information_system = "SINAN-CHIKUNGUNYA")
  sinan_chikungunya <- process_sinan_chikungunya(sinan_chikungunya, municipality_data = TRUE)
})

