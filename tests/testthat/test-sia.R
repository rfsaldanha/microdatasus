test_that("sia works", {
  sia <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIA-PA")
  sia <- process_sia(data = sia)
  expect_true(class(sia) == "data.frame")
})
