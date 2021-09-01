## tabCBO was created at LibreOffice, based on CBO2002.CNV, from ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/TAB/OBITOS_CID10_TAB.ZIP

tabCBO <- read.csv2(file = "data-raw/tabCBO.csv")
tabCBO$nome <- stringi::stri_escape_unicode(str = tabCBO$nome)
tabCBO$cod <- as.character(tabCBO$cod)

usethis::use_data(tabCBO, overwrite = TRUE)
