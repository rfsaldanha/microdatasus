test_that("sinan-chikungunya at 2022", {
  sinan_chikungunya_raw <- fetch_datasus(year_start = 2022, year_end = 2022, information_system = "SINAN-CHIKUNGUNYA")
  sinan_chikungunya <- process_sinan_chikungunya(sinan_chikungunya_raw, municipality_data = TRUE)
})

