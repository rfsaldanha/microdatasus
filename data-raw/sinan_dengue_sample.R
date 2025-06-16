## code to prepare `sinan_dengue_sample` dataset goes here

sinan_dengue_sample <- fetch_datasus(
  year_start = 2010, year_end = 2010,
  information_system = "SINAN-DENGUE"
) %>%
  head(100)

usethis::use_data(sinan_dengue_sample, overwrite = TRUE)
