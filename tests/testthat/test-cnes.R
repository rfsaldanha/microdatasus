test_that("cnes-st works at 2016, 6-7", {
  cnes_st_raw <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 6, uf = "AC", information_system = "CNES-ST")
  cnes_st <- process_cnes(data = cnes_st_raw, information_system = "CNES-ST", nomes = TRUE, municipality_data = TRUE)
  expect_true(class(cnes_st)[1] == "tbl_df")
})

test_that("cnes-sf works at 2016, 6-7", {
  cnes_pf_raw <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 6, uf = "AC", information_system = "CNES-PF")
  cnes_pf <- process_cnes(data = cnes_pf_raw, information_system = "CNES-PF", nomes = TRUE, municipality_data = TRUE)
  expect_true(class(cnes_pf)[1] == "tbl_df")
})
