test_that("cnes-st works at 2016, 6-7", {
  cnes_st <- process_cnes(
    data = cnes_st_sample,
    information_system = "CNES-ST",
    nomes = FALSE,
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(cnes_st))
})

test_that("cnes-sf works at 2016, 6-7", {
  cnes_pf <- process_cnes(
    data = cnes_pf_sample,
    information_system = "CNES-PF",
    nomes = FALSE,
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(cnes_pf))
})

test_that("sia works at 2016, 6-7", {
  sia <- process_sia(
    data = sia_pa_sample,
    nome_proced = FALSE,
    nome_ocupacao = TRUE,
    nome_equipe = TRUE,
    information_system = "SIA-PA",
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(sia))
})

test_that("sih rd works at 2016, 6-7", {
  sih <- process_sih(
    data = sih_rd_sample,
    information_system = "SIH-RD",
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(sih))
})

test_that("sim do works", {
  sim <- process_sim(data = sim_do_sample, municipality_data = TRUE)
  expect_true("tbl_df" %in% class(sim))
})

test_that("sinasc works", {
  sinasc <- process_sinasc(data = sinasc_sample, municipality_data = TRUE)
  expect_true("tbl_df" %in% class(sinasc))
})

test_that("sinan-chikungunya at 2022", {
  sinan_chikungunya <- process_sinan_chikungunya(
    data = sinan_chikungunya_sample,
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(sinan_chikungunya))
})

test_that("sinan-dengue at 2016", {
  sinan_dengue <- process_sinan_dengue(
    data = sinan_dengue_sample,
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(sinan_dengue))
})

test_that("sinan-malaria at 2016", {
  sinan_malaria <- process_sinan_malaria(
    data = sinan_malaria_sample,
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(sinan_malaria))
})

test_that("sinan-zika at 2016", {
  sinan_zika <- process_sinan_zika(
    data = sinan_zika_sample,
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(sinan_zika))
})

test_that("sinan-leishmaniose-visceral at 2023", {
  sinan_leishmaniose_visceral <- process_sinan_leishmaniose_visceral(
    data = sinan_leishmaniose_visceral_sample,
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(sinan_leishmaniose_visceral))
})

test_that("sinan-leishmaniose-tegumentar at 2023", {
  sinan_leishmaniose_tegumentar <- process_sinan_leishmaniose_tegumentar(
    data = sinan_leishmaniose_tegumentar_sample,
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(sinan_leishmaniose_tegumentar))
})

test_that("sinan-chagas at 2023", {
  sinan_chagas <- process_sinan_chagas(
    data = sinan_chagas_sample,
    municipality_data = TRUE
  )
  expect_true("tbl_df" %in% class(sinan_chagas))
})
