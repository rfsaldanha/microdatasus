## code to prepare `sih_rd_sample` dataset goes here

sih_rd_sample <- fetch_datasus(
  year_start = 2016, month_start = 6,
  year_end = 2016, month_end = 6,
  uf = "AC", information_system = "SIH-RD"
) %>%
  head(100)

usethis::use_data(sih_rd_sample, overwrite = TRUE)
