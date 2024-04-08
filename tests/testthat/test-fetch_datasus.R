test_that("fetch cnes st", {
  skip()
  cnes_st_raw <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 6, uf = "AC", information_system = "CNES-ST")
  expect_true("data.frame" %in% class(cnes_st_raw))
})

test_that("fetch cnes pf", {
  skip()
  cnes_pf_raw <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 6, uf = "AC", information_system = "CNES-PF")
  expect_true("data.frame" %in% class(cnes_pf_raw))
})

test_that("fetch sia", {
  skip()
  sia_raw <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIA-PA")
  expect_true("data.frame" %in% class(sia_raw))
})

test_that("fetch sih rd", {
  skip()
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIH-RD")
  expect_true("data.frame" %in% class(sih))
})

test_that("fetch sih rj", {
  skip()
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIH-RJ")
  expect_true("data.frame" %in% class(sih))
})

test_that("fetch sih sp", {
  skip()
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIH-SP")
  expect_true("data.frame" %in% class(sih))
})

test_that("fetch sih er", {
  skip()
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIH-ER")
  expect_true("data.frame" %in% class(sih))
})

test_that("fetch sih er", {
  skip()
  sih <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIH-ER")
  expect_true("data.frame" %in% class(sih))
})

test_that("fetch sim do", {
  skip()
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DO")
  expect_true("data.frame" %in% class(sim_raw))
})

test_that("fetch sim dofet", {
  skip()
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DOFET")
  expect_true("data.frame" %in% class(sim_raw))
})

test_that("fetch sim doext", {
  skip()
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DOEXT")
  expect_true("data.frame" %in% class(sim_raw))
})

test_that("fetch sim doinf", {
  skip()
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DOINF")
  expect_true("data.frame" %in% class(sim_raw))
})

test_that("fetch sim domat", {
  skip()
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DOMAT")
  expect_true("data.frame" %in% class(sim_raw))
})

test_that("fetch sim domat", {
  skip()
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DOMAT")
  expect_true("data.frame" %in% class(sim_raw))
})

test_that("fetch sinasc", {
  skip()
  sinasc_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SINASC")
})

test_that("fetch sinan chikungunya", {
  skip()
  sinan_chikungunya_raw <- fetch_datasus(year_start = 2022, year_end = 2022, information_system = "SINAN-CHIKUNGUNYA")
  expect_true("data.frame" %in% class(sinan_chikungunya_raw))
})

test_that("fetch sinan dengue", {
  skip()
  sinan_dengue_raw <- fetch_datasus(year_start = 2010, year_end = 2010, information_system = "SINAN-DENGUE")
  expect_true("data.frame" %in% class(sinan_dengue_raw))
})

test_that("fetch sinan malaria", {
  skip()
  sinan_malaria_raw <- fetch_datasus(year_start = 2016, year_end = 2016, information_system = "SINAN-MALARIA")
  expect_true("data.frame" %in% class(sinan_malaria_raw))
})

test_that("fetch sinan zika", {
  skip()
  sinan_zika_raw <- fetch_datasus(year_start = 2016, year_end = 2016, information_system = "SINAN-ZIKA")
  expect_true("data.frame" %in% class(sinan_zika_raw))
})

