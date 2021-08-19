test_that("sinasc processing works", {
  
  df <- readRDS("data/sinasc.rds")
  
  expect_equal(class(df$IDADEPAI), "factor")
  
  res <- process_sinasc(df)
  
  idx <- c(6435L, 6196L, 10739L, 939L, 9086L, 6194L, 666L, 3050L, 1767L, 1289L)
  ref <- as.numeric(levels(df$IDADEPAI[idx]))[df$IDADEPAI[idx]]
  ref[ref==0] <- NA
  ref[ref==99] <- NA
  
  expect_equal(class(res$IDADEPAI), "numeric")
  expect_identical(res$IDADEPAI[idx], ref)
  
  ref <- as.numeric(levels(df$IDADEMAE[idx]))[df$IDADEMAE[idx]]
  ref[ref==0] <- NA
  ref[ref==99] <- NA
  
  expect_equal(class(res$IDADEMAE), "numeric")
  expect_identical(res$IDADEMAE[idx], ref)
})
