## sigtab was created based on TB_SIGTAP.dbf, from ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Auxiliar/TAB_SIA.zip

sigtab <- foreign::read.dbf(file = "data-raw/TB_SIGTAW.dbf", as.is = TRUE)
Encoding(sigtab$IP_DSCR) <- "latin1"
sigtab$IP_DSCR <- stringi::stri_escape_unicode(str = sigtab$IP_DSCR)
colnames(sigtab) <- c("COD", "nome_proced")

usethis::use_data(sigtab, overwrite = TRUE, compress = "xz")
