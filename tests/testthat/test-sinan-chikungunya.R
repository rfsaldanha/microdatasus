test_that("sinan-chikungunya at 2016", {
  sinan_chikungunya_raw <- fetch_datasus(year_start = 2021, year_end = 2021, information_system = "SINAN-CHIKUNGUNYA")
  sinan_chikungunya <- process_sinan_chikungunya(sinan_chikungunya_raw, municipality_data = TRUE)
})

