## code to prepare `sinan_chagas_sample` dataset goes here

sinan_chagas_sample <- fetch_datasus(
  year_start = 2023,
  year_end = 2023,
  information_system = "SINAN-CHAGAS"
) %>%
  head(100)

usethis::use_data(sinan_chagas_sample, overwrite = TRUE)
