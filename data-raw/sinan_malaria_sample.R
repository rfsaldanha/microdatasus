## code to prepare `sinan_malaria_sample` dataset goes here

sinan_malaria_sample <- fetch_datasus(
  year_start = 2016, year_end = 2016,
  information_system = "SINAN-MALARIA"
) %>%
  head(100)

usethis::use_data(sinan_malaria_sample, overwrite = TRUE)
