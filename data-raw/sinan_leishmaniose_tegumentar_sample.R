## code to prepare `sinan_leishmaniose_tegumentar_sample` dataset goes here

sinan_leishmaniose_tegumentar_sample <- fetch_datasus(
  year_start = 2023,
  year_end = 2023,
  information_system = "SINAN-LEISHMANIOSE-TEGUMENTAR"
) %>%
  head(100)

usethis::use_data(sinan_leishmaniose_tegumentar_sample, overwrite = TRUE)
