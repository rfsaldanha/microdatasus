test_that("cnes-st works at 2016, 6-7", {
  cnes_st <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 6, uf = "AC", information_system = "CNES-ST")
  cnes_st <- process_cnes(cnes_st, information_system = "CNES-ST", nomes = TRUE, municipality_data = TRUE)
  expect_true(class(cnes_st) == "data.frame")
})

test_that("cnes-sf works at 2016, 6-7", {
  cnes_pf <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 6, uf = "AC", information_system = "CNES-PF")
  cnes_pf <- process_cnes(cnes_pf, information_system = "CNES-PF", nomes = TRUE, municipality_data = TRUE)
  expect_true(class(cnes_pf) == "data.frame")
})
