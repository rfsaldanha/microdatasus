test_that("sia works at 2016, 6-7", {
  sia <- fetch_datasus(year_start = 2016, month_start = 6, year_end = 2016, month_end = 7, uf = "AC", information_system = "SIA-PA")
  sia <- process_sia(data = sia, nome_proced = TRUE, nome_ocupacao = TRUE, nome_equipe = TRUE, information_system = "SIA-PA", municipality_data = TRUE)
  expect_true(class(sia) == "data.frame")
})
