test_that("sinasc works at 2016, 6-7", {
  sinasc <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SINASC")
  sinasc <- process_sinasc(data = sinasc, municipality_data = TRUE)
  expect_true(class(sinasc) == "data.frame")
})

test_that("sinasc works at random", {
  uf <- sample(x = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SE","TO"), size = 1)
  year <- round(runif(n = 1, min = 2010, max = lubridate::year(Sys.Date())-3))

  sinasc <- fetch_datasus(year_start = year, year_end = year, uf = uf, information_system = "SINASC")
  sinasc <- process_sinasc(data = sinasc, municipality_data = TRUE)
  expect_true(class(sinasc) == "data.frame")
})
