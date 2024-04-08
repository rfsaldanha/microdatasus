## code to prepare `sinasc_sample` dataset goes here

sinasc_sample <- fetch_datasus(
  year_start = 2016, year_end = 2016,
  uf = "AC", information_system = "SINASC"
) %>%
  head(100)

usethis::use_data(sinasc_sample, overwrite = TRUE)
