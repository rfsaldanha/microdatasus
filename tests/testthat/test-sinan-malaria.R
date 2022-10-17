test_that("sinan-malaria at 2016", {
  sinan_malaria <- fetch_datasus(year_start = 2016, year_end = 2016, information_system = "SINAN-MALARIA")
  sinan_malaria <- process_sinan_malaria(sinan_malaria, municipality_data = TRUE)
})

