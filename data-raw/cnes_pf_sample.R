## code to prepare `cnes_pf_sample` dataset goes here

cnes_pf_sample <- fetch_datasus(
  year_start = 2016, month_start = 6,
  year_end = 2016, month_end = 6,
  uf = "AC", information_system = "CNES-PF"
) %>%
  head(100)

cnes_pf_sample$CPF_PROF <- NULL

usethis::use_data(cnes_pf_sample, overwrite = TRUE)
