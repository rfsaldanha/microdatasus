## code to prepare `sia_sample` dataset goes here

sia_pa_sample <- fetch_datasus(
  year_start = 2016, month_start = 6,
  year_end = 2016, month_end = 6,
  uf = "AC", information_system = "SIA-PA"
) %>%
  head(100)

usethis::use_data(sia_pa_sample, overwrite = TRUE)
