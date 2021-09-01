test_that("fetch_datasus works", {
  # SIM
  sim <- fetch_datasus(year_start = 2016, year_end = 2016, uf = "AC", information_system = "SIM-DO")
  expect_true(class(sim) == "data.frame")

  # SIM, two years
  sim <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DO")
  expect_true(class(sim) == "data.frame")

  # SINASC
  sinasc <- fetch_datasus(year_start = 2016, year_end = 2016, uf = "AC", information_system = "SINASC")
  expect_true(class(sinasc) == "data.frame")

  # SIH
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 6, uf = "AC", information_system = "SIH-RD")
  expect_true(class(sih) == "data.frame")

  # CNES-ST
  cnes <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 6, uf = "AC", information_system = "CNES-ST")
  expect_true(class(sih) == "data.frame")
})
