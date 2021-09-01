
load(file = "data-raw/tabMun.rda")
tabMun$munResNome <- stringi::stri_escape_unicode(str = tabMun$munResNome)
tabMun$munResUf <- stringi::stri_escape_unicode(str = tabMun$munResUf)
usethis::use_data(tabMun, overwrite = TRUE)
