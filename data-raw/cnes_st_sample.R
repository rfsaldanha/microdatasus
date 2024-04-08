## code to prepare `cnes_st_sample` dataset goes here

cnes_st_sample <- fetch_datasus(
  year_start = 2016, month_start = 6,
  year_end = 2016, month_end = 6,
  uf = "AC", information_system = "CNES-ST"
) %>%
  head(100)

usethis::use_data(cnes_st_sample, overwrite = TRUE)
