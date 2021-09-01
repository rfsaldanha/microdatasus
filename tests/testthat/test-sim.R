test_that("sim works", {
  sim <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 6, uf = "AC", information_system = "SIM-DO")
  sim <- process_sim(data = sim, municipality_data = TRUE)
  expect_true(class(sim) == "data.frame")
})
