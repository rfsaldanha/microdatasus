test_that("sim do works", {
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DO")
  sim <- process_sim(data = sim_raw, municipality_data = TRUE)
  expect_true(class(sim)[1] == "tbl_df")
})

test_that("sim dofet works", {
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DOFET")
  sim <- process_sim(data = sim_raw, municipality_data = TRUE)
  expect_true(class(sim)[1] == "tbl_df")
})

test_that("sim doext works", {
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DOEXT")
  sim <- process_sim(data = sim_raw, municipality_data = TRUE)
  expect_true(class(sim)[1] == "tbl_df")
})

test_that("sim doinf works", {
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DOINF")
  sim <- process_sim(data = sim_raw, municipality_data = TRUE)
  expect_true(class(sim)[1] == "tbl_df")
})

test_that("sim domat works", {
  sim_raw <- fetch_datasus(year_start = 2016, year_end = 2017, uf = "AC", information_system = "SIM-DOMAT")
  sim <- process_sim(data = sim_raw, municipality_data = TRUE)
  expect_true(class(sim)[1] == "tbl_df")
})
