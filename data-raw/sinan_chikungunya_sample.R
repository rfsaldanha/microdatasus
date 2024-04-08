## code to prepare `sinan_chikungunya_sample` dataset goes here

sinan_chikungunya_sample <- fetch_datasus(
  year_start = 2022, year_end = 2022,
  information_system = "SINAN-CHIKUNGUNYA"
) %>%
  head(100)

usethis::use_data(sinan_chikungunya_sample, overwrite = TRUE)
