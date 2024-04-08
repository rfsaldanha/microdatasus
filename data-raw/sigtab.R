## sigtab was created based on TB_SIGTAP.dbf, from ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Auxiliar/TAB_SIA.zip

sigtab <- foreign::read.dbf(file = "data-raw/TB_SIGTAW.dbf", as.is = TRUE)
#Encoding(sigtab$DS_REGRA) <- "latin1"
#sigtab$DS_REGRA <- stringi::stri_escape_unicode(str = sigtab$DS_REGRA)
colnames(sigtab) <- c("COD", "nome_proced")

usethis::use_data(sigtab, overwrite = TRUE, compress = "xz")
