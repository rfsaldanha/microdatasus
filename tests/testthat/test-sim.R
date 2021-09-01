test_that("sim works at 2016, 6-7", {
  sim <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DO")
  sim <- process_sim(data = sim, municipality_data = TRUE)
  expect_true(class(sim) == "data.frame")
})

test_that("sim works at random", {
  uf <- sample(x = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SE","TO"), size = 1)
  year <- round(runif(n = 1, min = 2010, max = lubridate::year(Sys.Date())-3))

  sim <- fetch_datasus(year_start = year, year_end = year, uf = uf, information_system = "SIM-DO")
  sim <- process_sim(data = sim, municipality_data = TRUE)
  expect_true(class(sim) == "data.frame")
})
