test_that("sinan-malaria-final at 2016", {
  sinan_malaria_final <- fetch_datasus(year_start = 2016, year_end = 2016, uf = "AM", information_system = "SINAN-MALARIA-FINAL")
  sinan_malaria_final <- process_sinan_malaria(sinan_malaria_final, municipality_data = TRUE)
})

test_that("sinan-malaria-preliminar at 2020", {
  sinan_malaria_preliminar <- fetch_datasus(year_start = 2020, year_end = 2020, uf = "AM", information_system = "SINAN-MALARIA-PRELIMINAR")
  sinan_malaria_preliminar <- process_sinan_malaria(sinan_malaria_preliminar, municipality_data = TRUE)
})
