# tabOcupacao.csv was created at LibreOffice, based on OCUPACAO.CNV, from ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/TAB/OBITOS_CID10_TAB.ZIP

tabOcupacao <- read.csv(file = "data-raw/tabOcupacao.csv")
tabOcupacao$nome <- stringi::stri_escape_unicode(str = tabOcupacao$nome)

usethis::use_data(tabOcupacao, overwrite = TRUE)
