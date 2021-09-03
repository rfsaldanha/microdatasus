test_that("sinan-dengue-final at 2016", {
  sinan_dengue_final <- fetch_datasus(year_start = 2016, year_end = 2016, uf = "AC", information_system = "SINAN-DENGUE-FINAL")
})

test_that("sinan-dengue-preliminar at 2020", {
  sinan_dengue_preliminar <- fetch_datasus(year_start = 2020, year_end = 2020, uf = "AC", information_system = "SINAN-DENGUE-PRELIMINAR")
})
