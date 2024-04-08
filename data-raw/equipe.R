## equipe was created based on INE_EQUIPE_BR.dbf, from ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Auxiliar/TAB_SIA.zip

equipe <- foreign::read.dbf(file = "data-raw/INE_EQUIPE_BR.dbf", as.is = TRUE)
Encoding(equipe$DS_REGRA) <- "latin1"
equipe$DS_REGRA <- stringi::stri_escape_unicode(str = equipe$DS_REGRA)
colnames(equipe) <- c("COD", "equipe_ref")

usethis::use_data(equipe, overwrite = TRUE, compress = "xz")
