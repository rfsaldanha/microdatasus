## tabNaturalidade was created at LibreOffice, based on NATUR.CNV, from ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/TAB/OBITOS_CID10_TAB.ZIP

tabNaturalidade <- read.csv2(file = "data-raw/tabNaturalidade.csv")
tabNaturalidade$nome <- stringi::stri_escape_unicode(str = tabNaturalidade$nome)
tabNaturalidade$cod <- as.character(tabNaturalidade$cod)

usethis::use_data(tabNaturalidade, overwrite = TRUE)
