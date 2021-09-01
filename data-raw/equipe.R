## equipe was created based on INE_EQUIPE_BR.dbf, from ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Auxiliar/TAB_SIA.zip

equipe <- foreign::read.dbf(file = "data-raw/INE_EQUIPE_BR.dbf", as.is = TRUE)
Encoding(equipe$NO_REF) <- "latin1"
equipe$NO_REF <- stringi::stri_escape_unicode(str = equipe$NO_REF)
colnames(equipe) <- c("COD", "equipe_ref")

usethis::use_data(equipe, overwrite = TRUE)
