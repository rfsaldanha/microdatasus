test_that("cnes-pf works at 2016, 6-7", {
  cnes_pf <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 6, uf = "AC", information_system = "CNES-PF")
  cnes_pf <- process_cnes_pf(cnes_pf, municipality_data = TRUE)
  expect_true(class(cnes_pf) == "data.frame")
})

test_that("cnes-pf works at random", {
  uf <- sample(x = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SE","TO"), size = 1)
  year <- round(runif(n = 1, min = 2010, max = lubridate::year(Sys.Date())-1))
  month <- round(runif(n = 1, min = 1, max = 12))

  cnes_pf <- fetch_datasus(year_start = year, month_start = month, year_end = year, month_end = month, uf = uf, information_system = "CNES-PF")
  cnes_pf <- process_cnes_pf(cnes_pf, municipality_data = TRUE)
  expect_true(class(cnes_pf) == "data.frame")
})
