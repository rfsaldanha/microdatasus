## code to prepare `sinan_zika_sample` dataset goes here

sinan_zika_sample <- fetch_datasus(
  year_start = 2016, year_end = 2016,
  information_system = "SINAN-ZIKA"
) %>%
  head(100)

usethis::use_data(sinan_zika_sample, overwrite = TRUE)
