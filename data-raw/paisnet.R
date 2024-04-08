## paisnet was created based on PAISNET.DBF, from ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/AUXILIAR/TAB_SINANNET.zip

paisnet <- foreign::read.dbf(file = "data-raw/PAISNET.DBF", as.is = TRUE)
Encoding(paisnet$NM_PAIS) <- "latin1"
paisnet$NM_PAIS <- stringi::stri_escape_unicode(str = paisnet$NM_PAIS)

usethis::use_data(paisnet, overwrite = TRUE, compress = "xz")
