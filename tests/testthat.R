library(testthat)
library(microdatasus)

# df <- fetch_datasus(
#   year_start = 2018, year_end = 2018,
#   uf = "RJ", information_system = "SINASC"
# )
# 
# df <- dplyr::ungroup(dplyr::slice_sample(dplyr::group_by(df, IDADEPAI), prop = 0.05))
# 
# saveRDS(df, "tests/testthat/data/sinasc.rds")

test_check("microdatasus")
