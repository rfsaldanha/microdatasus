## code to prepare `sim_do_sample` dataset goes here

sim_do_sample <- fetch_datasus(
  year_start = 2016, year_end = 2016,
  uf = "AC", information_system = "SIM-DO"
) %>%
  head(100)

usethis::use_data(sim_do_sample, overwrite = TRUE)
