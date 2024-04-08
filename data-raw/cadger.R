## cadger was created based on CADGERBR.dbf, from ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Auxiliar/TAB_CNES.zip

cadger <- foreign::read.dbf(file = "data-raw/CADGERBR.dbf", as.is = TRUE)
cadger <- dplyr::select(cadger, CNES, FANTASIA)
cadger$CNES <- as.integer(cadger$CNES)
Encoding(cadger$FANTASIA) <- "latin1"
cadger$FANTASIA <- stringi::stri_escape_unicode(str = cadger$FANTASIA)

usethis::use_data(cadger, overwrite = TRUE, compress = "xz")
