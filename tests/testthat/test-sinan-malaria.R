test_that("sinan-malaria at 2016", {
  sinan_malaria_raw <- fetch_datasus(year_start = 2016, year_end = 2016, information_system = "SINAN-MALARIA")
  sinan_malaria <- process_sinan_malaria(sinan_malaria_raw, municipality_data = TRUE)
})

