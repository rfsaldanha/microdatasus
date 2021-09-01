test_that("sih works at 2016, 6-7", {
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIH-RD")
  sih <- process_sih(data = sih, information_system = "SIH-RD", municipality_data = TRUE)
  expect_true(class(sih) == "data.frame")
})

test_that("sih works at random", {
  uf <- sample(x = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SE","TO"), size = 1)
  year <- round(runif(n = 1, min = 2010, max = lubridate::year(Sys.Date())-1))
  month <- round(runif(n = 1, min = 1, max = 12))

  sih <- fetch_datasus(year_start = year, month_start = month, year_end = year, month_end = month, uf = uf, information_system = "SIH-RD")
  sih <- process_sih(data = sih, information_system = "SIH-RD", municipality_data = TRUE)
  expect_true(class(sih) == "data.frame")
})
