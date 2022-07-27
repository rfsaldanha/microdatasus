#' Process SIH variables from DataSUS
#'
#' \code{process_SIH} processes SIH variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SIH variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' Currently, only "SIH-RD" is supported.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param information_system string. The abbreviation of the health information system. See \emph{Details}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples \dontrun{
#' df <- fetch_datasus(year_start = 2010, month_start = 1,
#'                     year_end = 2010, month_end = 1,
#'                     uf = "RJ",
#'                     information_system = "SIH-RD")
#' df_a <- process_sih(df)
#' df_b <- process_sih(df, municipality_data = FALSE)
#' }
#' @export

process_sih <- function(data, information_system = "SIH-RD", municipality_data = TRUE) {
  # Check information system
  available_information_system <- "SIH-RD"
  if(!(information_system %in% available_information_system)) stop("Health informaton system unknown.")

  # Variables names
  variables_names <- names(data)

  if(information_system == "SIH-RD"){

    # UF_ZI
    if("UF_ZI" %in% variables_names){
      data$UF_ZI <- as.character(data$UF_ZI)
    }

    # ANO_CMPT
    if("ANO_CMPT" %in% variables_names){
      data$ANO_CMPT <- as.integer(as.character(data$ANO_CMPT))
    }

    # MES_CMPT
    if("MES_CMPT" %in% variables_names){
      data$MES_CMPT <- as.integer(as.character(data$MES_CMPT))
    }

    # ESPEC
    if("ESPEC" %in% variables_names){
      data$ESPEC <- as.numeric(levels(data$ESPEC))[data$ESPEC]
      data$ESPEC[data$ESPEC==1] <- "Cir\u00fargico"
      data$ESPEC[data$ESPEC==2] <- "Obst\u00e9tricos"
      data$ESPEC[data$ESPEC==3] <- "Cl\u00ednicos"
      data$ESPEC[data$ESPEC==4] <- "Cr\u00f4nicos"
      data$ESPEC[data$ESPEC==5] <- "Psiquiatria"
      data$ESPEC[data$ESPEC==6] <- "Pneumologia sanit\u00e1ria (tsiologia)"
      data$ESPEC[data$ESPEC==7] <- "Pedi\u00e1tricos"
      data$ESPEC[data$ESPEC==8] <- "Reabilita\u00e7\u00e3o"
      data$ESPEC[data$ESPEC==9] <- "Leito Dia / Cir\u00fargicos"
      data$ESPEC[data$ESPEC==10] <- "Leito Dia / Aids"
      data$ESPEC[data$ESPEC==11] <- "Leito Dia / Fibrose C\u00edstica"
      data$ESPEC[data$ESPEC==12] <- "Leito Dia / Intercorr\u00eancia P\u00f3s-Transplante"
      data$ESPEC[data$ESPEC==13] <- "Leito Dia / Geriatria"
      data$ESPEC[data$ESPEC==14] <- "Leito Dia / Sa\u00fade Mental"
      data$ESPEC[data$ESPEC==51] <- "UTI II Adulto COVID 19"
      data$ESPEC[data$ESPEC==52] <- "UTI II Pedi\u00e1trica COVID 19"
      data$ESPEC[data$ESPEC==64] <- "Unidade Intermedi\u00e1ria"
      data$ESPEC[data$ESPEC==65] <- "Unidade Intermedi\u00e1ria Neonatal"
      data$ESPEC[data$ESPEC==74] <- "UTI I"
      data$ESPEC[data$ESPEC==75] <- "UTI Adulto II"
      data$ESPEC[data$ESPEC==76] <- "UTI Adulto III"
      data$ESPEC[data$ESPEC==77] <- "UTI Infantil I"
      data$ESPEC[data$ESPEC==78] <- "UTI Infantil II"
      data$ESPEC[data$ESPEC==79] <- "UTI Infantil III"
      data$ESPEC[data$ESPEC==80] <- "UTI Neonatal I"
      data$ESPEC[data$ESPEC==81] <- "UTI Neonatal II"
      data$ESPEC[data$ESPEC==82] <- "UTI Neonatal III"
      data$ESPEC[data$ESPEC==83] <- "UTI Queimados"
      data$ESPEC[data$ESPEC==84] <- "Acolhimento Noturno"
      data$ESPEC[data$ESPEC==85] <- "UTI Coronariana-UCO tipo II"
      data$ESPEC[data$ESPEC==86] <- "UTI Coronariana-UCO tipo III"
      data$ESPEC[data$ESPEC==87] <- "Sa\u00fade Mental (Cl\u00ednico)"
      data$ESPEC[data$ESPEC==88] <- "Queimado Adulto (Cl\u00ednico)"
      data$ESPEC[data$ESPEC==89] <- "Queimado Pedi\u00e1trico (Cl\u00ednico)"
      data$ESPEC[data$ESPEC==90] <- "Queimado Adulto (Cir\u00fargico)"
      data$ESPEC[data$ESPEC==91] <- "Queimado Pedi\u00e1trico (Cir\u00fargico)"
      data$ESPEC[data$ESPEC==92] <- "UCI Unidade de Cuidados Intermediarios Neonatal Convencional"
      data$ESPEC[data$ESPEC==93] <- "UCI Unidade de Cuidados Intermediarios Neonatal Canguru"
      data$ESPEC[data$ESPEC==94] <- "UCI Unidade de Cuidados Intermediarios Pediatrico"
      data$ESPEC[data$ESPEC==95] <- "UCI Unidade de Cuidados Intermediarios Adulto"
      data$ESPEC[data$ESPEC==96] <- "Suporte Ventilat\u00f3rio Pulmonar COVID-19"
      data$ESPEC <- factor(data$ESPEC)
    }

    # CGC_HOSP
    if("CGC_HOSP" %in% variables_names){
      data$CGC_HOSP <- as.character(data$CGC_HOSP)
    }

    # N_AIH
    if("N_AIH" %in% variables_names){
      data$N_AIH <- as.character(data$N_AIH)
    }

    # IDENT
    if("IDENT" %in% variables_names){
      data$IDENT <- as.numeric(levels(data$IDENT))[data$IDENT]
      data$IDENT[data$IDENT==1] <- "Principal"
      data$IDENT[data$IDENT==3] <- "Continua\u00e7\u00e3o"
      data$IDENT[data$IDENT==5] <- "Longa perman\u00eancia"
      data$IDENT <- factor(data$IDENT)
    }

    # CEP
    if("CEP" %in% variables_names){
      data$CEP <- as.character(data$CEP)
    }

    # MUNIC_RES
    if("MUNIC_RES" %in% variables_names & municipality_data == TRUE){
      data$MUNIC_RES <- as.integer(as.character(data$MUNIC_RES))
      colnames(tabMun)[1] <- "MUNIC_RES"
      data <- dplyr::left_join(data, tabMun, by = "MUNIC_RES")
    } else {
      data$MUNIC_RES <- as.integer(as.character(data$MUNIC_RES))
    }

    # NASC
    if("NASC" %in% variables_names){
      data$NASC <- as.character(data$NASC)
      data$NASC <- as.Date(data$NASC, format = "%Y%m%d")
    }

    # SEXO
    if("SEXO" %in% variables_names){
      data$SEXO <- as.numeric(levels(data$SEXO))[data$SEXO]
      data$SEXO[data$SEXO==1] <- "Masculino"
      data$SEXO[data$SEXO==2] <- "Feminino"
      data$SEXO[data$SEXO==3] <- "Feminino"
      data$SEXO[data$SEXO==0] <- NA
      data$SEXO[data$SEXO==9] <- NA
      data$SEXO <- factor(data$SEXO)
    }

    # UTI_MES_IN
    if("UTI_MES_IN" %in% variables_names){
      data$UTI_MES_IN <- as.integer(data$UTI_MES_IN)
    }

    # UTI_MES_AN
    if("UTI_MES_AN" %in% variables_names){
      data$UTI_MES_AN <- as.integer(data$UTI_MES_AN)
    }

    # UTI_MES_AL
    if("UTI_MES_AL" %in% variables_names){
      data$UTI_MES_AL <- as.integer(data$UTI_MES_AL)
    }

    # UTI_MES_TO
    if("UTI_MES_TO" %in% variables_names){
      data$UTI_MES_TO <- as.integer(data$UTI_MES_TO)
    }

    # MARCA_UTI
    if("MARCA_UTI" %in% variables_names){
      data$MARCA_UTI <- as.numeric(levels(data$MARCA_UTI))[data$MARCA_UTI]
      data$MARCA_UTI[data$MARCA_UTI==0] <- "N\u00e3o utilizou UTI"
      data$MARCA_UTI[data$MARCA_UTI==51] <- "UTI adulto - tipo II COVID 19"
      data$MARCA_UTI[data$MARCA_UTI==52] <- "UTI pedi\u00e1trica - tipo II COVID 19"
      data$MARCA_UTI[data$MARCA_UTI==74] <- "UTI adulto - tipo I"
      data$MARCA_UTI[data$MARCA_UTI==75] <- "UTI adulto - tipo II"
      data$MARCA_UTI[data$MARCA_UTI==76] <- "UTI adulto - tipo III"
      data$MARCA_UTI[data$MARCA_UTI==77] <- "UTI infantil - tipo I"
      data$MARCA_UTI[data$MARCA_UTI==78] <- "UTI infantil - tipo II"
      data$MARCA_UTI[data$MARCA_UTI==79] <- "UTI infantil - tipo III"
      data$MARCA_UTI[data$MARCA_UTI==80] <- "UTI neonatal - tipo I"
      data$MARCA_UTI[data$MARCA_UTI==81] <- "UTI neonatal - tipo II"
      data$MARCA_UTI[data$MARCA_UTI==82] <- "UTI neonatal - tipo III"
      data$MARCA_UTI[data$MARCA_UTI==83] <- "UTI de queimados"
      data$MARCA_UTI[data$MARCA_UTI==85] <- "UTI coronariana tipo II - UCO tipo II"
      data$MARCA_UTI[data$MARCA_UTI==86] <- "UTI coronariana tipo III - UCO tipo III"
      data$MARCA_UTI[data$MARCA_UTI==99] <- "UTI Doador"
      data$MARCA_UTI[data$MARCA_UTI==1] <- "Utilizou mais de um tipo de UTI"
      data$MARCA_UTI <- factor(data$MARCA_UTI)
    }

    # UTI_INT_IN
    if("UTI_INT_IN" %in% variables_names){
      data$UTI_INT_IN <- as.integer(data$UTI_INT_IN)
    }

    # UTI_INT_AN
    if("UTI_INT_AN" %in% variables_names){
      data$UTI_INT_AN <- as.integer(data$UTI_INT_AN)
    }

    # UTI_INT_AL
    if("UTI_INT_AL" %in% variables_names){
      data$UTI_INT_AL <- as.integer(data$UTI_INT_AL)
    }

    # UTI_INT_TO
    if("UTI_INT_TO" %in% variables_names){
      data$UTI_INT_TO <- as.integer(data$UTI_INT_TO)
    }

    # DIAR_ACOM
    if("DIAR_ACOM" %in% variables_names){
      data$DIAR_ACOM <- as.integer(data$DIAR_ACOM)
    }

    # QT_DIARIAS
    if("QT_DIARIAS" %in% variables_names){
      data$QT_DIARIAS <- as.integer(data$QT_DIARIAS)
    }

    # PROC_SOLIC
    if("PROC_SOLIC" %in% variables_names){
      data$PROC_SOLIC <- as.character(data$PROC_SOLIC)
    }

    # PROC_REA
    if("PROC_REA" %in% variables_names){
      data$PROC_REA <- as.character(data$PROC_REA)
    }

    # VAL_SH
    if("VAL_SH" %in% variables_names){
      data$VAL_SH <- as.numeric(data$VAL_SH)
    }

    # VAL_SP
    if("VAL_SP" %in% variables_names){
      data$VAL_SP <- as.numeric(data$VAL_SP)
    }

    # VAL_SADT
    if("VAL_SADT" %in% variables_names){
      data$VAL_SADT <- as.numeric(data$VAL_SADT)
    }

    # VAL_RN
    if("VAL_RN" %in% variables_names){
      data$VAL_RN <- as.numeric(data$VAL_RN)
    }

    # VAL_ACOMP
    if("VAL_ACOMP" %in% variables_names){
      data$VAL_ACOMP <- as.numeric(data$VAL_ACOMP)
    }

    # VAL_ORTP
    if("VAL_ORTP" %in% variables_names){
      data$VAL_ORTP <- as.numeric(data$VAL_ORTP)
    }

    # VAL_SANGUE
    if("VAL_SANGUE" %in% variables_names){
      data$VAL_SANGUE <- as.numeric(data$VAL_SANGUE)
    }

    # VAL_SADTSR
    if("VAL_SADTSR" %in% variables_names){
      data$VAL_SADTSR <- as.numeric(data$VAL_SADTSR)
    }

    # VAL_TRANSP
    if("VAL_TRANSP" %in% variables_names){
      data$VAL_TRANSP <- as.numeric(data$VAL_TRANSP)
    }

    # VAL_OBSANG
    if("VAL_OBSANG" %in% variables_names){
      data$VAL_OBSANG <- as.numeric(data$VAL_OBSANG)
    }

    # VAL_PED1AC
    if("VAL_PED1AC" %in% variables_names){
      data$VAL_PED1AC <- as.numeric(data$VAL_PED1AC)
    }

    # VAL_TOT
    if("VAL_TOT" %in% variables_names){
      data$VAL_TOT <- as.numeric(data$VAL_TOT)
    }

    # VAL_UTI
    if("VAL_UTI" %in% variables_names){
      data$VAL_UTI <- as.numeric(data$VAL_UTI)
    }

    # US_TOT
    if("US_TOT" %in% variables_names){
      data$US_TOT <- as.numeric(data$US_TOT)
    }

    # DT_INTER
    if("DT_INTER" %in% variables_names){
      data$DT_INTER <- as.character(data$DT_INTER)
      data$DT_INTER <- as.Date(data$DT_INTER, format = "%Y%m%d")
    }

    # DT_SAIDA
    if("DT_SAIDA" %in% variables_names){
      data$DT_SAIDA <- as.character(data$DT_SAIDA)
      data$DT_SAIDA <- as.Date(data$DT_SAIDA, format = "%Y%m%d")
    }

    # DIAG_PRINC
    if("DIAG_PRINC" %in% variables_names){
      data$DIAG_PRINC <- as.character(data$DIAG_PRINC)
    }

    # DIAG_SECUN
    if("DIAG_SECUN" %in% variables_names){
      data$DIAG_SECUN <- as.character(data$DIAG_SECUN)
    }

    # COBRANCA (motivo de saída/permanência, portaria SAS 719)
    if("COBRANCA" %in% variables_names){
      data$COBRANCA <- as.numeric(levels(data$COBRANCA))[data$COBRANCA]
      data$COBRANCA[data$COBRANCA==11] <- "Alta curado"
      data$COBRANCA[data$COBRANCA==12] <- "Alta melhorado"
      data$COBRANCA[data$COBRANCA==14] <- "Alta a pedido"
      data$COBRANCA[data$COBRANCA==15] <- "Alta com previs\u00e3o de retorno p/acomp do paciente"
      data$COBRANCA[data$COBRANCA==16] <- "Alta por evas\u00e3o"
      data$COBRANCA[data$COBRANCA==18] <- "Alta por outros motivos"
      data$COBRANCA[data$COBRANCA==19] <- "Alta de paciente agudo em psiquiatria"
      data$COBRANCA[data$COBRANCA==21] <- "Perman\u00eancia por caracter\u00edsticas pr\u00f3prias da doen\u00e7a"
      data$COBRANCA[data$COBRANCA==22] <- "Perman\u00eancia por intercorr\u00eancia"
      data$COBRANCA[data$COBRANCA==23] <- "Perman\u00eancia por impossibilidade s\u00f3cio-familiar"
      data$COBRANCA[data$COBRANCA==24] <- "Perman\u00eancia proc doa\u00e7\u00e3o \u00f3rg, tec, c\u00e9l-doador vivo"
      data$COBRANCA[data$COBRANCA==25] <- "Perman\u00eancia proc doa\u00e7\u00e3o \u00f3rg, tec, c\u00e9l-doador morto"
      data$COBRANCA[data$COBRANCA==26] <- "Perman\u00eancia por mudan\u00e7a de procedimento"
      data$COBRANCA[data$COBRANCA==27] <- "Perman\u00eancia por reopera\u00e7\u00e3o"
      data$COBRANCA[data$COBRANCA==28] <- "Perman\u00eancia por outros motivos"
      data$COBRANCA[data$COBRANCA==29] <- "Transfer\u00eancia para interna\u00e7\u00e3o domiciliar"
      data$COBRANCA[data$COBRANCA==32] <- "Transfer\u00eancia para interna\u00e7\u00e3o domiciliar"
      data$COBRANCA[data$COBRANCA==31] <- "Transfer\u00eancia para outro estabelecimento"
      data$COBRANCA[data$COBRANCA==41] <- "\u00d3bito com DO fornecida pelo m\u00e9dico assistente"
      data$COBRANCA[data$COBRANCA==42] <- "\u00d3bito com DO fornecida pelo IML"
      data$COBRANCA[data$COBRANCA==43] <- "\u00d3bito com DO fornecida pelo SVO"
      data$COBRANCA[data$COBRANCA==51] <- "Encerramento administrativo"
      data$COBRANCA[data$COBRANCA==61] <- "Alta da m\u00e3e/pu\u00e9rpera e do rec\u00e9m-nascido"
      data$COBRANCA[data$COBRANCA==17] <- "Alta da m\u00e3e/pu\u00e9rpera e do rec\u00e9m-nascido"
      data$COBRANCA[data$COBRANCA==62] <- "Alta da m\u00e3e/pu\u00e9rpera e perman\u00eancia rec\u00e9m-nascido"
      data$COBRANCA[data$COBRANCA==13] <- "Alta da m\u00e3e/pu\u00e9rpera e perman\u00eancia rec\u00e9m-nascido"
      data$COBRANCA[data$COBRANCA==63] <- "Alta da m\u00e3e/pu\u00e9rpera e \u00f3bito do rec\u00e9m-nascido"
      data$COBRANCA[data$COBRANCA==64] <- "Alta da m\u00e3e/pu\u00e9rpera com \u00f3bito fetal"
      data$COBRANCA[data$COBRANCA==65] <- "\u00d3bito da gestante e do concepto"
      data$COBRANCA[data$COBRANCA==66] <- "\u00d3bito da m\u00e3e/pu\u00e9rpera e alta do rec\u00e9m-nascido"
      data$COBRANCA[data$COBRANCA==67] <- "\u00d3bito da m\u00e3e/pu\u00e9rpera e perman\u00eancia rec\u00e9m-nascido"
      data$COBRANCA <- factor(data$COBRANCA)
    }

    # NATUREZA
    if("NATUREZA" %in% variables_names){
      data$NATUREZA <- as.numeric(levels(data$NATUREZA))[data$NATUREZA]
      data$NATUREZA[data$NATUREZA==0] <- NA
      data$NATUREZA[data$NATUREZA==99] <- NA
      data$NATUREZA[data$NATUREZA==10] <- "Pr\u00f3prio"
      data$NATUREZA[data$NATUREZA==20] <- "Contratado"
      data$NATUREZA[data$NATUREZA==22] <- "Contratado optante SIMPLES"
      data$NATUREZA[data$NATUREZA==30] <- "Federal"
      data$NATUREZA[data$NATUREZA==31] <- "Federal Verba Pr\u00f3pria"
      data$NATUREZA[data$NATUREZA==40] <- "Estadual"
      data$NATUREZA[data$NATUREZA==41] <- "Estadual Verba Pr\u00f3pria"
      data$NATUREZA[data$NATUREZA==50] <- "Municipal"
      data$NATUREZA[data$NATUREZA==60] <- "Filantr\u00f3pico"
      data$NATUREZA[data$NATUREZA==61] <- "Filantr\u00f3pico isento tributos e contr.sociais"
      data$NATUREZA[data$NATUREZA==63] <- "Filantr\u00f3pico isento IR e contr.s/lucro l\u00edquido"
      data$NATUREZA[data$NATUREZA==70] <- "Universit\u00e1rio Ensino"
      data$NATUREZA[data$NATUREZA==80] <- "Sindicato"
      data$NATUREZA[data$NATUREZA==90] <- "Universit\u00e1rio Pesquisas"
      data$NATUREZA[data$NATUREZA==91] <- "Univ. Pesquisas isento tributos e contr.sociais"
      data$NATUREZA[data$NATUREZA==93] <- "Univ. Pesquisas isento IR e contr.s/lucro l\u00edquido"
      data$NATUREZA[data$NATUREZA==94] <- "Universit\u00e1rio de ensino e pesquisa privado"
      data$NATUREZA[data$NATUREZA==92] <- "Universit\u00e1rio de ensino e pesquisa privado"
      data$NATUREZA <- factor(data$NATUREZA)
    }

    # NAT_JUR
    if("NAT_JUR" %in% variables_names){
      data$NAT_JUR <- as.numeric(levels(data$NAT_JUR))[data$NAT_JUR]
      data$NAT_JUR[data$NAT_JUR==1015] <- "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Federal"
      data$NAT_JUR[data$NAT_JUR==1023] <- "\u00d3rg\u00e3o P\u00fablico do Poder Exec Estadual ou Distr Fed"
      data$NAT_JUR[data$NAT_JUR==1031] <- "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Municipal"
      data$NAT_JUR[data$NAT_JUR==1040] <- "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Federal"
      data$NAT_JUR[data$NAT_JUR==1058] <- "\u00d3rg\u00e3o P\u00fablico do Poder Legisl Estadual ou Dist Fed"
      data$NAT_JUR[data$NAT_JUR==1066] <- "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Municipal"
      data$NAT_JUR[data$NAT_JUR==1074] <- "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Federal"
      data$NAT_JUR[data$NAT_JUR==1082] <- "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Estadual"
      data$NAT_JUR[data$NAT_JUR==1104] <- "Autarquia Federal"
      data$NAT_JUR[data$NAT_JUR==1112] <- "Autarquia Estadual ou do Distrito Federal"
      data$NAT_JUR[data$NAT_JUR==1120] <- "Autarquia Municipal"
      data$NAT_JUR[data$NAT_JUR==1139] <- "Funda\u00e7\u00e3o Federal"
      data$NAT_JUR[data$NAT_JUR==1147] <- "Funda\u00e7\u00e3o Estadual ou do Distrito Federal"
      data$NAT_JUR[data$NAT_JUR==1155] <- "Funda\u00e7\u00e3o Municipal"
      data$NAT_JUR[data$NAT_JUR==1163] <- "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Federal"
      data$NAT_JUR[data$NAT_JUR==1171] <- "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Estadual ou Distr Federal"
      data$NAT_JUR[data$NAT_JUR==1180] <- "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Estadual ou Distr Federal"
      data$NAT_JUR[data$NAT_JUR==1198] <- "Comiss\u00e3o Polinacional"
      data$NAT_JUR[data$NAT_JUR==1201] <- "Fundo P\u00fablico"
      data$NAT_JUR[data$NAT_JUR==1210] <- "Associa\u00e7\u00e3o P\u00fablica"
      data$NAT_JUR[data$NAT_JUR==2011] <- "Empresa P\u00fablica"
      data$NAT_JUR[data$NAT_JUR==2038] <- "Sociedade de Economia Mista"
      data$NAT_JUR[data$NAT_JUR==2046] <- "Sociedade An\u00f4nima Aberta"
      data$NAT_JUR[data$NAT_JUR==2054] <- "Sociedade An\u00f4nima Fechada"
      data$NAT_JUR[data$NAT_JUR==2062] <- "Sociedade Empres\u00e1ria Limitada"
      data$NAT_JUR[data$NAT_JUR==2070] <- "Sociedade Empres\u00e1ria em Nome Coletivo"
      data$NAT_JUR[data$NAT_JUR==2089] <- "Sociedade Empres\u00e1ria em Comandita Simples"
      data$NAT_JUR[data$NAT_JUR==2097] <- "Sociedade Empres\u00e1ria em Comandita por A\u00e7\u00f5es"
      data$NAT_JUR[data$NAT_JUR==2127] <- "Sociedade em Conta de Participa\u00e7\u00e3o"
      data$NAT_JUR[data$NAT_JUR==2135] <- "Empres\u00e1rio (Individual)"
      data$NAT_JUR[data$NAT_JUR==2143] <- "Cooperativa"
      data$NAT_JUR[data$NAT_JUR==2151] <- "Cons\u00f3rcio de Sociedades"
      data$NAT_JUR[data$NAT_JUR==2160] <- "Grupo de Sociedades"
      data$NAT_JUR[data$NAT_JUR==2178] <- "Estabelecimento no Brasil de Sociedade Estrangeira"
      data$NAT_JUR[data$NAT_JUR==2194] <- "Estab no Brasil Empr Binacional Argentina-Brasil"
      data$NAT_JUR[data$NAT_JUR==2216] <- "Empresa Domiciliada no Exterior"
      data$NAT_JUR[data$NAT_JUR==2224] <- "Clube/Fundo de Investimento"
      data$NAT_JUR[data$NAT_JUR==2232] <- "Sociedade Simples Pura"
      data$NAT_JUR[data$NAT_JUR==2240] <- "Sociedade Simples Limitada"
      data$NAT_JUR[data$NAT_JUR==2259] <- "Sociedade Simples em Nome Coletivo"
      data$NAT_JUR[data$NAT_JUR==2267] <- "Sociedade Simples em Comandita Simples"
      data$NAT_JUR[data$NAT_JUR==2275] <- "Empresa Binacional"
      data$NAT_JUR[data$NAT_JUR==2283] <- "Cons\u00f3rcio de Empregadores"
      data$NAT_JUR[data$NAT_JUR==2291] <- "Cons\u00f3rcio Simples"
      data$NAT_JUR[data$NAT_JUR==2305] <- "Empr Individ Responsab Limitada (Natur Empres\u00e1ria)"
      data$NAT_JUR[data$NAT_JUR==2313] <- "Empr Individ Responsab Limitada (Natureza Simples)"
      data$NAT_JUR[data$NAT_JUR==3034] <- "Servi\u00e7o Notarial e Registral (Cart\u00f3rio)"
      data$NAT_JUR[data$NAT_JUR==3069] <- "Funda\u00e7\u00e3o Privada"
      data$NAT_JUR[data$NAT_JUR==3077] <- "Servi\u00e7o Social Aut\u00f4nomo"
      data$NAT_JUR[data$NAT_JUR==3085] <- "Condom\u00ednio Edil\u00edcio"
      data$NAT_JUR[data$NAT_JUR==3107] <- "Comiss\u00e3o de Concilia\u00e7\u00e3o Pr\u00e9via"
      data$NAT_JUR[data$NAT_JUR==3115] <- "Entidade de Media\u00e7\u00e3o e Arbitragem"
      data$NAT_JUR[data$NAT_JUR==3123] <- "Partido Pol\u00edtico"
      data$NAT_JUR[data$NAT_JUR==3131] <- "Entidade Sindical"
      data$NAT_JUR[data$NAT_JUR==3204] <- "Estab no Brasil de Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Estrang"
      data$NAT_JUR[data$NAT_JUR==3212] <- "Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Domiciliada no Exterior"
      data$NAT_JUR[data$NAT_JUR==3220] <- "Organiza\u00e7\u00e3o Religiosa"
      data$NAT_JUR[data$NAT_JUR==3239] <- "Comunidade Ind\u00edgena"
      data$NAT_JUR[data$NAT_JUR==3247] <- "Fundo Privado"
      data$NAT_JUR[data$NAT_JUR==3999] <- "Associa\u00e7\u00e3o Privada"
      data$NAT_JUR[data$NAT_JUR==4014] <- "Empresa Individual Imobili\u00e1ria"
      data$NAT_JUR[data$NAT_JUR==4022] <- "Segurado Especial"
      data$NAT_JUR[data$NAT_JUR==4081] <- "Contribuinte Individual"
      data$NAT_JUR[data$NAT_JUR==4090] <- "Candidato a Cargo Pol\u00edtico Eletivo"
      data$NAT_JUR[data$NAT_JUR==4111] <- "Leiloeiro"
      data$NAT_JUR[data$NAT_JUR==5010] <- "Organiza\u00e7\u00e3o Internacional"
      data$NAT_JUR[data$NAT_JUR==5029] <- "Representa\u00e7\u00e3o Diplom\u00e1tica Estrangeira"
      data$NAT_JUR[data$NAT_JUR==5037] <- "Outras Institui\u00e7\u00f5es Extraterritoriais"
      data$NAT_JUR[data$NAT_JUR==0] <- NA
      data$NAT_JUR <- factor(data$NAT_JUR)
    }

    # GESTAO
    if("GESTAO" %in% variables_names){
      data$GESTAO <- as.numeric(levels(data$GESTAO))[data$GESTAO]
      data$GESTAO[data$GESTAO==0] <- "Estadual"
      data$GESTAO[data$GESTAO==2] <- "Estadual plena"
      data$GESTAO[data$GESTAO==1] <- "Municipal plena assist"
      data$GESTAO[data$GESTAO==3] <- NA
      data$GESTAO[data$GESTAO==9] <- NA
      data$GESTAO <- factor(data$GESTAO)
    }

    # RUBRICA
    if("RUBRICA" %in% variables_names){
      data$RUBRICA <- as.numeric(data$RUBRICA)
    }

    # IND_VDRL
    if("IND_VDRL" %in% variables_names){
      data$IND_VDRL <- as.numeric(levels(data$IND_VDRL))[data$IND_VDRL]
      data$IND_VDRL[data$IND_VDRL==0] <- "N\u00e3o"
      data$IND_VDRL[data$IND_VDRL==1] <- "Sim"
      data$IND_VDRL <- factor(data$IND_VDRL)
    }

    # MUNIC_MOV
    if("MUNIC_MOV" %in% variables_names){
      data$MUNIC_MOV <- as.integer(data$MUNIC_MOV)
    }

    # COD_IDADE
    if("COD_IDADE" %in% variables_names){
      data$COD_IDADE <- as.numeric(levels(data$COD_IDADE))[data$COD_IDADE]
      data$COD_IDADE[data$COD_IDADE==0] <- NA
      data$COD_IDADE[data$COD_IDADE==2] <- "Dias"
      data$COD_IDADE[data$COD_IDADE==3] <- "Meses"
      data$COD_IDADE[data$COD_IDADE==4] <- "Anos"
      data$COD_IDADE[data$COD_IDADE==5] <- "Centena de anos (100 + idade)"
      data$COD_IDADE <- factor(data$COD_IDADE)
    }

    # IDADE
    if("IDADE" %in% variables_names){
      data$IDADE <- as.integer(data$IDADE)
    }

    # DIAS_PERM
    if("DIAS_PERM" %in% variables_names){
      data$DIAS_PERM <- as.integer(data$DIAS_PERM)
    }

    # MORTE
    if("MORTE" %in% variables_names){
      data$MORTE[data$MORTE==0] <- "N\u00e3o"
      data$MORTE[data$MORTE==1] <- "Sim"
      data$MORTE <- factor(data$MORTE)
    }

    # NACIONAL
    if("NACIONAL" %in% variables_names){
      data$NACIONAL <- as.numeric(levels(data$NACIONAL))[data$NACIONAL]
      data$NACIONAL[data$NACIONAL==170] <- "Abissinia"
      data$NACIONAL[data$NACIONAL==171] <- "Acores"
      data$NACIONAL[data$NACIONAL==172] <- "Afar frances"
      data$NACIONAL[data$NACIONAL==241] <- "Afeganistao"
      data$NACIONAL[data$NACIONAL==93] <- "Albania"
      data$NACIONAL[data$NACIONAL==30] <- "Alemanha"
      data$NACIONAL[data$NACIONAL==174] <- "Alto volta"
      data$NACIONAL[data$NACIONAL==94] <- "Andorra"
      data$NACIONAL[data$NACIONAL==175] <- "Angola"
      data$NACIONAL[data$NACIONAL==334] <- "Antartica francesa"
      data$NACIONAL[data$NACIONAL==337] <- "Antartico argentino"
      data$NACIONAL[data$NACIONAL==333] <- "Antartico britanico, territorio"
      data$NACIONAL[data$NACIONAL==336] <- "Antartico chileno"
      data$NACIONAL[data$NACIONAL==338] <- "Antartico noruegues"
      data$NACIONAL[data$NACIONAL==28] <- "Antigua e. dep. barbuda"
      data$NACIONAL[data$NACIONAL==29] <- "Antilhas holandesas"
      data$NACIONAL[data$NACIONAL==339] <- "Apatrida"
      data$NACIONAL[data$NACIONAL==242] <- "Arabia saudita"
      data$NACIONAL[data$NACIONAL==176] <- "Argelia"
      data$NACIONAL[data$NACIONAL==21] <- "Argentina"
      data$NACIONAL[data$NACIONAL==347] <- "Armenia"
      data$NACIONAL[data$NACIONAL==289] <- "Arquipelago de bismark"
      data$NACIONAL[data$NACIONAL==175] <- "Angola"
      data$NACIONAL[data$NACIONAL==285] <- "Arquipelago manahiki"
      data$NACIONAL[data$NACIONAL==286] <- "Arquipelago midway"
      data$NACIONAL[data$NACIONAL==33] <- "Aruba"
      data$NACIONAL[data$NACIONAL==175] <- "Angola"
      data$NACIONAL[data$NACIONAL==198] <- "Ascensao e tristao da cunha,is"
      data$NACIONAL[data$NACIONAL==287] <- "Ashmore e cartier"
      data$NACIONAL[data$NACIONAL==288] <- "Australia"
      data$NACIONAL[data$NACIONAL==95] <- "Austria"
      data$NACIONAL[data$NACIONAL==138] <- "Azerbaijao"
      data$NACIONAL[data$NACIONAL==243] <- "Bahrein"
      data$NACIONAL[data$NACIONAL==342] <- "Bangladesh"
      data$NACIONAL[data$NACIONAL==44] <- "Barbados"
      data$NACIONAL[data$NACIONAL==139] <- "Bashkista"
      data$NACIONAL[data$NACIONAL==177] <- "Bechuanalandia"
      data$NACIONAL[data$NACIONAL==31] <- "Belgica"
      data$NACIONAL[data$NACIONAL==46] <- "Belize"
      data$NACIONAL[data$NACIONAL==178] <- "Benin"
      data$NACIONAL[data$NACIONAL==83] <- "Bermudas"
      data$NACIONAL[data$NACIONAL==246] <- "Bhutan"
      data$NACIONAL[data$NACIONAL==244] <- "Birmania"
      data$NACIONAL[data$NACIONAL==22] <- "Bolivia"
      data$NACIONAL[data$NACIONAL==134] <- "Bosnia herzegovina"
      data$NACIONAL[data$NACIONAL==179] <- "Botsuana"
      data$NACIONAL[data$NACIONAL==10] <- "Brasil"
      data$NACIONAL[data$NACIONAL==245] <- "Brunei"
      data$NACIONAL[data$NACIONAL==96] <- "Bulgaria"
      data$NACIONAL[data$NACIONAL==238] <- "Burkina fasso"
      data$NACIONAL[data$NACIONAL==180] <- "Burundi"
      data$NACIONAL[data$NACIONAL==141] <- "Buryat"
      data$NACIONAL[data$NACIONAL==343] <- "Cabo verde"
      data$NACIONAL[data$NACIONAL==181] <- "Camaroes"
      data$NACIONAL[data$NACIONAL==34] <- "Canada"
      data$NACIONAL[data$NACIONAL==142] <- "Carelia"
      data$NACIONAL[data$NACIONAL==247] <- "Catar"
      data$NACIONAL[data$NACIONAL==143] <- "Cazaquistao"
      data$NACIONAL[data$NACIONAL==248] <- "Ceilao"
      data$NACIONAL[data$NACIONAL==182] <- "Ceuta e melilla"
      data$NACIONAL[data$NACIONAL==183] <- "Chade"
      data$NACIONAL[data$NACIONAL==144] <- "Chechen ingusth"
      data$NACIONAL[data$NACIONAL==23] <- "Chile"
      data$NACIONAL[data$NACIONAL==42] <- "China"
      data$NACIONAL[data$NACIONAL==249] <- "China (taiwan)"
      data$NACIONAL[data$NACIONAL==97] <- "Chipre"
      data$NACIONAL[data$NACIONAL==145] <- "Chuvash"
      data$NACIONAL[data$NACIONAL==275] <- "Cingapura"
      data$NACIONAL[data$NACIONAL==26] <- "Colombia"
      data$NACIONAL[data$NACIONAL==40] <- "Comunidade das bahamas"
      data$NACIONAL[data$NACIONAL==54] <- "Comunidade dominicana"
      data$NACIONAL[data$NACIONAL==185] <- "Congo"
      data$NACIONAL[data$NACIONAL==43] <- "Coreia"
      data$NACIONAL[data$NACIONAL==186] <- "Costa do marfim"
      data$NACIONAL[data$NACIONAL==51] <- "Costa rica"
      data$NACIONAL[data$NACIONAL==250] <- "Coveite"
      data$NACIONAL[data$NACIONAL==130] <- "Croacia"
      data$NACIONAL[data$NACIONAL==52] <- "Cuba"
      data$NACIONAL[data$NACIONAL==53] <- "Curacao"
      data$NACIONAL[data$NACIONAL==146] <- "Dagesta"
      data$NACIONAL[data$NACIONAL==187] <- "Daome"
      data$NACIONAL[data$NACIONAL==340] <- "Dependencia de ross"
      data$NACIONAL[data$NACIONAL==98] <- "Dinamarca"
      data$NACIONAL[data$NACIONAL==188] <- "Djibuti"
      data$NACIONAL[data$NACIONAL==99] <- "Eire"
      data$NACIONAL[data$NACIONAL==251] <- "Emirados arabes unidos"
      data$NACIONAL[data$NACIONAL==27] <- "Equador"
      data$NACIONAL[data$NACIONAL==100] <- "Escocia"
      data$NACIONAL[data$NACIONAL==136] <- "Eslovaquia"
      data$NACIONAL[data$NACIONAL==132] <- "Eslovenia"
      data$NACIONAL[data$NACIONAL==35] <- "Espanha"
      data$NACIONAL[data$NACIONAL==129] <- "Estado da cidade do vaticano"
      data$NACIONAL[data$NACIONAL==57] <- "Estados assoc. das antilhas"
      data$NACIONAL[data$NACIONAL==36] <- "Estados unidos da america (eua)"
      data$NACIONAL[data$NACIONAL==147] <- "Estonia"
      data$NACIONAL[data$NACIONAL==190] <- "Etiopia"
      data$NACIONAL[data$NACIONAL==252] <- "Filipinas"
      data$NACIONAL[data$NACIONAL==102] <- "Finlandia"
      data$NACIONAL[data$NACIONAL==37] <- "Franca"
      data$NACIONAL[data$NACIONAL==192] <- "Gambia"
      data$NACIONAL[data$NACIONAL==193] <- "Gana"
      data$NACIONAL[data$NACIONAL==194] <- "Gaza"
      data$NACIONAL[data$NACIONAL==148] <- "Georgia"
      data$NACIONAL[data$NACIONAL==103] <- "Gibraltar"
      data$NACIONAL[data$NACIONAL==149] <- "Gorno altai"
      data$NACIONAL[data$NACIONAL==32] <- "Gra-bretanha"
      data$NACIONAL[data$NACIONAL==59] <- "Granada"
      data$NACIONAL[data$NACIONAL==104] <- "Grecia"
      data$NACIONAL[data$NACIONAL==84] <- "Groenlandia"
      data$NACIONAL[data$NACIONAL==292] <- "Guam"
      data$NACIONAL[data$NACIONAL==61] <- "Guatemala"
      data$NACIONAL[data$NACIONAL==87] <- "Guiana francesa"
      data$NACIONAL[data$NACIONAL==195] <- "Guine"
      data$NACIONAL[data$NACIONAL==344] <- "Guine bissau"
      data$NACIONAL[data$NACIONAL==196] <- "Guine equatorial"
      data$NACIONAL[data$NACIONAL==105] <- "Holanda"
      data$NACIONAL[data$NACIONAL==64] <- "Honduras"
      data$NACIONAL[data$NACIONAL==63] <- "Honduras britanicas"
      data$NACIONAL[data$NACIONAL==253] <- "Hong-kong"
      data$NACIONAL[data$NACIONAL==106] <- "Hungria"
      data$NACIONAL[data$NACIONAL==254] <- "Iemen"
      data$NACIONAL[data$NACIONAL==345] <- "Iemen do sul"
      data$NACIONAL[data$NACIONAL==197] <- "Ifni"
      data$NACIONAL[data$NACIONAL==300] <- "Ilha johnston e sand"
      data$NACIONAL[data$NACIONAL==69] <- "Ilha milhos"
      data$NACIONAL[data$NACIONAL==293] <- "Ilhas baker"
      data$NACIONAL[data$NACIONAL==107] <- "Ilhas baleares"
      data$NACIONAL[data$NACIONAL==199] <- "Ilhas canarias"
      data$NACIONAL[data$NACIONAL==294] <- "Ilhas cantao e enderburg"
      data$NACIONAL[data$NACIONAL==295] <- "Ilhas carolinas"
      data$NACIONAL[data$NACIONAL==297] <- "Ilhas christmas"
      data$NACIONAL[data$NACIONAL==184] <- "Ilhas comores"
      data$NACIONAL[data$NACIONAL==290] <- "Ilhas cook"
      data$NACIONAL[data$NACIONAL==108] <- "Ilhas cosmoledo (lomores)"
      data$NACIONAL[data$NACIONAL==117] <- "Ilhas de man"
      data$NACIONAL[data$NACIONAL==109] <- "Ilhas do canal"
      data$NACIONAL[data$NACIONAL==296] <- "Ilhas do pacifico"
      data$NACIONAL[data$NACIONAL==58] <- "Ilhas falklands"
      data$NACIONAL[data$NACIONAL==101] <- "Ilhas faroes"
      data$NACIONAL[data$NACIONAL==298] <- "Ilhas gilbert"
      data$NACIONAL[data$NACIONAL==60] <- "Ilhas guadalupe"
      data$NACIONAL[data$NACIONAL==299] <- "Ilhas howland e jarvis"
      data$NACIONAL[data$NACIONAL==301] <- "Ilhas kingman reef"
      data$NACIONAL[data$NACIONAL==305] <- "Ilhas macdonal e heard"
      data$NACIONAL[data$NACIONAL==302] <- "Ilhas macquaire"
      data$NACIONAL[data$NACIONAL==67] <- "Ilhas malvinas"
      data$NACIONAL[data$NACIONAL==303] <- "Ilhas marianas"
      data$NACIONAL[data$NACIONAL==304] <- "Ilhas marshall"
      data$NACIONAL[data$NACIONAL==306] <- "Ilhas niue"
      data$NACIONAL[data$NACIONAL==307] <- "Ilhas norfolk"
      data$NACIONAL[data$NACIONAL==315] <- "Ilhas nova caledonia"
      data$NACIONAL[data$NACIONAL==318] <- "Ilhas novas hebridas"
      data$NACIONAL[data$NACIONAL==308] <- "Ilhas palau"
      data$NACIONAL[data$NACIONAL==320] <- "Ilhas pascoa"
      data$NACIONAL[data$NACIONAL==321] <- "Ilhas pitcairin"
      data$NACIONAL[data$NACIONAL==309] <- "Ilhas salomao"
      data$NACIONAL[data$NACIONAL==326] <- "Ilhas santa cruz"
      data$NACIONAL[data$NACIONAL==65] <- "Ilhas serranas"
      data$NACIONAL[data$NACIONAL==310] <- "Ilhas tokelau"
      data$NACIONAL[data$NACIONAL==80] <- "Ilhas turca"
      data$NACIONAL[data$NACIONAL==47] <- "Ilhas turks e caicos"
      data$NACIONAL[data$NACIONAL==82] <- "Ilhas virgens americanas"
      data$NACIONAL[data$NACIONAL==81] <- "Ilhas virgens britanicas"
      data$NACIONAL[data$NACIONAL==311] <- "Ilhas wake"
      data$NACIONAL[data$NACIONAL==332] <- "Ilhas wallis e futuna"
      data$NACIONAL[data$NACIONAL==255] <- "India"
      data$NACIONAL[data$NACIONAL==256] <- "Indonesia"
      data$NACIONAL[data$NACIONAL==110] <- "Inglaterra"
      data$NACIONAL[data$NACIONAL==257] <- "Ira"
      data$NACIONAL[data$NACIONAL==258] <- "Iraque"
      data$NACIONAL[data$NACIONAL==112] <- "Irlanda"
      data$NACIONAL[data$NACIONAL==111] <- "Irlanda do norte"
      data$NACIONAL[data$NACIONAL==113] <- "Islandia"
      data$NACIONAL[data$NACIONAL==259] <- "Israel"
      data$NACIONAL[data$NACIONAL==39] <- "Italia"
      data$NACIONAL[data$NACIONAL==114] <- "Iugoslavia"
      data$NACIONAL[data$NACIONAL==66] <- "Jamaica"
      data$NACIONAL[data$NACIONAL==41] <- "Japao"
      data$NACIONAL[data$NACIONAL==260] <- "Jordania"
      data$NACIONAL[data$NACIONAL==150] <- "Kabardino balkar"
      data$NACIONAL[data$NACIONAL==312] <- "Kalimatan"
      data$NACIONAL[data$NACIONAL==151] <- "Kalmir"
      data$NACIONAL[data$NACIONAL==346] <- "Kara kalpak"
      data$NACIONAL[data$NACIONAL==152] <- "Karachaevocherkess"
      data$NACIONAL[data$NACIONAL==153] <- "Khakass"
      data$NACIONAL[data$NACIONAL==261] <- "Kmer/camboja"
      data$NACIONAL[data$NACIONAL==154] <- "Komi"
      data$NACIONAL[data$NACIONAL==262] <- "Kuwait"
      data$NACIONAL[data$NACIONAL==263] <- "Laos"
      data$NACIONAL[data$NACIONAL==200] <- "Lesoto"
      data$NACIONAL[data$NACIONAL==155] <- "Letonia"
      data$NACIONAL[data$NACIONAL==264] <- "Libano"
      data$NACIONAL[data$NACIONAL==201] <- "Liberia"
      data$NACIONAL[data$NACIONAL==202] <- "Libia"
      data$NACIONAL[data$NACIONAL==115] <- "Liechtenstein"
      data$NACIONAL[data$NACIONAL==156] <- "Lituania"
      data$NACIONAL[data$NACIONAL==116] <- "Luxemburgo"
      data$NACIONAL[data$NACIONAL==265] <- "Macau"
      data$NACIONAL[data$NACIONAL==205] <- "Madagascar"
      data$NACIONAL[data$NACIONAL==203] <- "Madeira"
      data$NACIONAL[data$NACIONAL==266] <- "Malasia"
      data$NACIONAL[data$NACIONAL==204] <- "Malawi"
      data$NACIONAL[data$NACIONAL==267] <- "Maldivas,is"
      data$NACIONAL[data$NACIONAL==206] <- "Mali"
      data$NACIONAL[data$NACIONAL==157] <- "Mari"
      data$NACIONAL[data$NACIONAL==207] <- "Marrocos"
      data$NACIONAL[data$NACIONAL==68] <- "Martinica"
      data$NACIONAL[data$NACIONAL==268] <- "Mascate"
      data$NACIONAL[data$NACIONAL==208] <- "Mauricio"
      data$NACIONAL[data$NACIONAL==209] <- "Mauritania"
      data$NACIONAL[data$NACIONAL==85] <- "Mexico"
      data$NACIONAL[data$NACIONAL==284] <- "Mianma"
      data$NACIONAL[data$NACIONAL==210] <- "Mocambique"
      data$NACIONAL[data$NACIONAL==158] <- "Moldavia"
      data$NACIONAL[data$NACIONAL==118] <- "Monaco"
      data$NACIONAL[data$NACIONAL==269] <- "Mongolia"
      data$NACIONAL[data$NACIONAL==70] <- "Monte serrat"
      data$NACIONAL[data$NACIONAL==137] <- "Montenegro"
      data$NACIONAL[data$NACIONAL==240] <- "Namibia"
      data$NACIONAL[data$NACIONAL==314] <- "Nauru"
      data$NACIONAL[data$NACIONAL==270] <- "Nepal"
      data$NACIONAL[data$NACIONAL==211] <- "Nguane"
      data$NACIONAL[data$NACIONAL==71] <- "Nicaragua"
      data$NACIONAL[data$NACIONAL==213] <- "Nigeria"
      data$NACIONAL[data$NACIONAL==119] <- "Noruega"
      data$NACIONAL[data$NACIONAL==316] <- "Nova guine"
      data$NACIONAL[data$NACIONAL==317] <- "Nova zelandia"
      data$NACIONAL[data$NACIONAL==271] <- "Oman"
      data$NACIONAL[data$NACIONAL==159] <- "Ossetia setentrional"
      data$NACIONAL[data$NACIONAL==121] <- "Pais de gales"
      data$NACIONAL[data$NACIONAL==122] <- "Paises baixos"
      data$NACIONAL[data$NACIONAL==272] <- "Palestina"
      data$NACIONAL[data$NACIONAL==72] <- "Panama"
      data$NACIONAL[data$NACIONAL==73] <- "Panama(zona do canal)"
      data$NACIONAL[data$NACIONAL==214] <- "Papua nova guine"
      data$NACIONAL[data$NACIONAL==273] <- "Paquistao"
      data$NACIONAL[data$NACIONAL==24] <- "Paraguai"
      data$NACIONAL[data$NACIONAL==89] <- "Peru"
      data$NACIONAL[data$NACIONAL==322] <- "Polinesia francesa"
      data$NACIONAL[data$NACIONAL==123] <- "Polonia"
      data$NACIONAL[data$NACIONAL==74] <- "Porto rico"
      data$NACIONAL[data$NACIONAL==45] <- "Portugal"
      data$NACIONAL[data$NACIONAL==215] <- "Pracas norte africanas"
      data$NACIONAL[data$NACIONAL==216] <- "Protetor do sudoeste africano"
      data$NACIONAL[data$NACIONAL==217] <- "Quenia"
      data$NACIONAL[data$NACIONAL==160] <- "Quirguistao"
      data$NACIONAL[data$NACIONAL==75] <- "Quitasueno"
      data$NACIONAL[data$NACIONAL==189] <- "Republica arabe do egito"
      data$NACIONAL[data$NACIONAL==218] <- "Republica centro africana"
      data$NACIONAL[data$NACIONAL==173] <- "Republica da africa do sul"
      data$NACIONAL[data$NACIONAL==140] <- "Republica da bielorrussia"
      data$NACIONAL[data$NACIONAL==133] <- "Republica da macedonia"
      data$NACIONAL[data$NACIONAL==56] <- "Republica de el salvador"
      data$NACIONAL[data$NACIONAL==291] <- "Republica de fiji"
      data$NACIONAL[data$NACIONAL==120] <- "Republica de malta"
      data$NACIONAL[data$NACIONAL==191] <- "Republica do gabao"
      data$NACIONAL[data$NACIONAL==62] <- "Republica do haiti"
      data$NACIONAL[data$NACIONAL==212] <- "Republica do niger"
      data$NACIONAL[data$NACIONAL==55] <- "Republica dominicana"
      data$NACIONAL[data$NACIONAL==88] <- "Republica guiana"
      data$NACIONAL[data$NACIONAL==135] <- "Republica tcheca"
      data$NACIONAL[data$NACIONAL==20] <- "Reservado"
      data$NACIONAL[data$NACIONAL==48] <- "Reservado"
      data$NACIONAL[data$NACIONAL==49] <- "Reservado"
      data$NACIONAL[data$NACIONAL==50] <- "Reservado"
      data$NACIONAL[data$NACIONAL==219] <- "Reuniao"
      data$NACIONAL[data$NACIONAL==220] <- "Rodesia (zimbabwe)"
      data$NACIONAL[data$NACIONAL==124] <- "Romenia"
      data$NACIONAL[data$NACIONAL==76] <- "Roncador"
      data$NACIONAL[data$NACIONAL==221] <- "Ruanda"
      data$NACIONAL[data$NACIONAL==274] <- "Ruiquiu,is"
      data$NACIONAL[data$NACIONAL==348] <- "Russia"
      data$NACIONAL[data$NACIONAL==222] <- "Saara espanhol"
      data$NACIONAL[data$NACIONAL==323] <- "Sabah"
      data$NACIONAL[data$NACIONAL==324] <- "Samoa americana"
      data$NACIONAL[data$NACIONAL==325] <- "Samoa ocidental"
      data$NACIONAL[data$NACIONAL==125] <- "San marino"
      data$NACIONAL[data$NACIONAL==223] <- "Santa helena"
      data$NACIONAL[data$NACIONAL==77] <- "Santa lucia"
      data$NACIONAL[data$NACIONAL==78] <- "Sao cristovao"
      data$NACIONAL[data$NACIONAL==224] <- "Sao tome e principe"
      data$NACIONAL[data$NACIONAL==79] <- "Sao vicente"
      data$NACIONAL[data$NACIONAL==327] <- "Sarawak"
      data$NACIONAL[data$NACIONAL==349] <- "Senegal"
      data$NACIONAL[data$NACIONAL==276] <- "Sequin"
      data$NACIONAL[data$NACIONAL==226] <- "Serra leoa"
      data$NACIONAL[data$NACIONAL==131] <- "Servia"
      data$NACIONAL[data$NACIONAL==225] <- "Seychelles"
      data$NACIONAL[data$NACIONAL==277] <- "Siria"
      data$NACIONAL[data$NACIONAL==227] <- "Somalia, republica"
      data$NACIONAL[data$NACIONAL==278] <- "Sri-lanka"
      data$NACIONAL[data$NACIONAL==86] <- "St. pierre et miquelon"
      data$NACIONAL[data$NACIONAL==228] <- "Suazilandia"
      data$NACIONAL[data$NACIONAL==229] <- "Sudao"
      data$NACIONAL[data$NACIONAL==126] <- "Suecia"
      data$NACIONAL[data$NACIONAL==38] <- "Suica"
      data$NACIONAL[data$NACIONAL==90] <- "Suriname"
      data$NACIONAL[data$NACIONAL==127] <- "Svalbard e jan mayer,is"
      data$NACIONAL[data$NACIONAL==161] <- "Tadjiquistao"
      data$NACIONAL[data$NACIONAL==279] <- "Tailandia"
      data$NACIONAL[data$NACIONAL==230] <- "Tanganica"
      data$NACIONAL[data$NACIONAL==350] <- "Tanzania"
      data$NACIONAL[data$NACIONAL==162] <- "Tartaria"
      data$NACIONAL[data$NACIONAL==128] <- "Tchecoslovaquia"
      data$NACIONAL[data$NACIONAL==335] <- "Terr. antartico da australia"
      data$NACIONAL[data$NACIONAL==341] <- "Terras austrais"
      data$NACIONAL[data$NACIONAL==231] <- "Territ. britanico do oceano indico"
      data$NACIONAL[data$NACIONAL==328] <- "Territorio de cocos"
      data$NACIONAL[data$NACIONAL==319] <- "Territorio de papua"
      data$NACIONAL[data$NACIONAL==329] <- "Timor"
      data$NACIONAL[data$NACIONAL==233] <- "Togo"
      data$NACIONAL[data$NACIONAL==330] <- "Tonga"
      data$NACIONAL[data$NACIONAL==232] <- "Transkei"
      data$NACIONAL[data$NACIONAL==280] <- "Tregua, estado"
      data$NACIONAL[data$NACIONAL==91] <- "Trinidad e tobago"
      data$NACIONAL[data$NACIONAL==234] <- "Tunisia"
      data$NACIONAL[data$NACIONAL==163] <- "Turcomenistao"
      data$NACIONAL[data$NACIONAL==281] <- "Turquia"
      data$NACIONAL[data$NACIONAL==331] <- "Tuvalu"
      data$NACIONAL[data$NACIONAL==164] <- "Tuvin"
      data$NACIONAL[data$NACIONAL==165] <- "Ucrania"
      data$NACIONAL[data$NACIONAL==166] <- "Udmurt"
      data$NACIONAL[data$NACIONAL==235] <- "Uganda"
      data$NACIONAL[data$NACIONAL==167] <- "Uniao sovietica"
      data$NACIONAL[data$NACIONAL==25] <- "Uruguai"
      data$NACIONAL[data$NACIONAL==168] <- "Uzbequistao"
      data$NACIONAL[data$NACIONAL==92] <- "Venezuela"
      data$NACIONAL[data$NACIONAL==282] <- "Vietna do norte"
      data$NACIONAL[data$NACIONAL==283] <- "Vietna do sul"
      data$NACIONAL[data$NACIONAL==169] <- "Yakut"
      data$NACIONAL[data$NACIONAL==236] <- "Zaire"
      data$NACIONAL[data$NACIONAL==237] <- "Zambia"
      data$NACIONAL[data$NACIONAL==239] <- "Zimbabwe"
      data$NACIONAL <- factor(data$NACIONAL)
    }

    # NUM_PROC
    if("NUM_PROC" %in% variables_names){
      data$NUM_PROC <- as.integer(data$NUM_PROC)
    }

    # CAR_INT
    if("CAR_INT" %in% variables_names){
      data$CAR_INT <- as.numeric(levels(data$CAR_INT))[data$CAR_INT]
      data$CAR_INT[data$CAR_INT==1] <- "Eletivo"
      data$CAR_INT[data$CAR_INT==2] <- "Urg\u00eancia"
      data$CAR_INT[data$CAR_INT==3] <- "Acidente no local trabalho ou a serv da empresa"
      data$CAR_INT[data$CAR_INT==4] <- "Acidente no trajeto para o trabalho"
      data$CAR_INT[data$CAR_INT==5] <- "Outros tipo de acidente de tr\u00e2nsito"
      data$CAR_INT[data$CAR_INT==6] <- "Out tp les\u00f5es e envenen por agent qu\u00edm f\u00edsicos"
      data$CAR_INT <- factor(data$CAR_INT)
    }

    # TOT_PT_SP
    if("TOT_PT_SP" %in% variables_names){
      data$TOT_PT_SP <- as.integer(data$TOT_PT_SP)
    }

    # CPF_AUT
    if("CPF_AUT" %in% variables_names){
      data$CPF_AUT <- as.integer(data$CPF_AUT)
    }

    # HOMONIMO
    if("HOMONIMO" %in% variables_names){
      data$HOMONIMO <- as.numeric(levels(data$HOMONIMO))[data$HOMONIMO]
      data$HOMONIMO[data$HOMONIMO==0] <- "N\u00e3o"
      data$HOMONIMO[data$HOMONIMO==1] <- "Sim"
      data$HOMONIMO <- factor(data$HOMONIMO)
    }

    # NUM_FILHOS
    if("NUM_FILHOS" %in% variables_names){
      data$NUM_FILHOS <- as.integer(data$NUM_FILHOS)
    }

    # INSTRU
    if("INSTRU" %in% variables_names){
      data$INSTRU <- as.numeric(levels(data$INSTRU))[data$INSTRU]
      data$INSTRU[data$INSTRU==1] <- "Analfabeto"
      data$INSTRU[data$INSTRU==2] <- "1\u00ba grau"
      data$INSTRU[data$INSTRU==3] <- "2\u00ba grau"
      data$INSTRU[data$INSTRU==4] <- "3\u00ba grau"
      data$INSTRU[data$INSTRU==0] <- NA
      data$INSTRU[data$INSTRU==9] <- NA
      data$INSTRU <- factor(data$INSTRU)
    }

    # CID_NOTIF
    if("CID_NOTIF" %in% variables_names){
      data$CID_NOTIF <- as.character(data$CID_NOTIF)
    }

    # CONTRACEP1
    if("CONTRACEP1" %in% variables_names){
      data$CONTRACEP1 <- as.numeric(levels(data$CONTRACEP1))[data$CONTRACEP1]
      data$CONTRACEP1[data$CONTRACEP1==1] <- "LAM"
      data$CONTRACEP1[data$CONTRACEP1==2] <- "Ogino Kaus"
      data$CONTRACEP1[data$CONTRACEP1==3] <- "Temperatura basal"
      data$CONTRACEP1[data$CONTRACEP1==4] <- "Billings"
      data$CONTRACEP1[data$CONTRACEP1==5] <- "Cinto t\u00e9rmico"
      data$CONTRACEP1[data$CONTRACEP1==6] <- "DIU"
      data$CONTRACEP1[data$CONTRACEP1==7] <- "Diafragma"
      data$CONTRACEP1[data$CONTRACEP1==8] <- "Preservativo"
      data$CONTRACEP1[data$CONTRACEP1==9] <- "Espermicida"
      data$CONTRACEP1[data$CONTRACEP1==10] <- "Horm\u00f4nio oral"
      data$CONTRACEP1[data$CONTRACEP1==11] <- "Horm\u00f4nio injet\u00e1vel"
      data$CONTRACEP1[data$CONTRACEP1==12] <- "Coito interrompido"
      data$CONTRACEP1[data$CONTRACEP1==0] <- NA
      data$CONTRACEP1[data$CONTRACEP1==99] <- NA
      data$CONTRACEP1 <- factor(data$CONTRACEP1)
    }

    # CONTRACEP2
    if("CONTRACEP2" %in% variables_names){
      data$CONTRACEP2 <- as.numeric(levels(data$CONTRACEP2))[data$CONTRACEP2]
      data$CONTRACEP2[data$CONTRACEP2==1] <- "LAM"
      data$CONTRACEP2[data$CONTRACEP2==2] <- "Ogino Kaus"
      data$CONTRACEP2[data$CONTRACEP2==3] <- "Temperatura basal"
      data$CONTRACEP2[data$CONTRACEP2==4] <- "Billings"
      data$CONTRACEP2[data$CONTRACEP2==5] <- "Cinto t\u00e9rmico"
      data$CONTRACEP2[data$CONTRACEP2==6] <- "DIU"
      data$CONTRACEP2[data$CONTRACEP2==7] <- "Diafragma"
      data$CONTRACEP2[data$CONTRACEP2==8] <- "Preservativo"
      data$CONTRACEP2[data$CONTRACEP2==9] <- "Espermicida"
      data$CONTRACEP2[data$CONTRACEP2==10] <- "Horm\u00f4nio oral"
      data$CONTRACEP2[data$CONTRACEP2==11] <- "Horm\u00f4nio injet\u00e1vel"
      data$CONTRACEP2[data$CONTRACEP2==12] <- "Coito interrompido"
      data$CONTRACEP2[data$CONTRACEP2==0] <- NA
      data$CONTRACEP2[data$CONTRACEP2==99] <- NA
      data$CONTRACEP2 <- factor(data$CONTRACEP2)
    }

    # GESTRISCO
    if("GESTRISCO" %in% variables_names){
      data$GESTRISCO <- as.numeric(levels(data$GESTRISCO))[data$GESTRISCO]
      data$GESTRISCO[data$GESTRISCO==0] <- "N\u00e3o"
      data$GESTRISCO[data$GESTRISCO==1] <- "Sim"
      data$GESTRISCO <- factor(data$GESTRISCO)
    }

    # INSC_PN
    if("INSC_PN" %in% variables_names){
      data$INSC_PN <- as.character(data$INSC_PN)
    }

    # SEQ_AIH5
    if("SEQ_AIH5" %in% variables_names){
      data$SEQ_AIH5 <- as.numeric(levels(data$SEQ_AIH5))[data$SEQ_AIH5]
      data$SEQ_AIH5[data$SEQ_AIH5==0] <- "Sequencial zerado"
      data$SEQ_AIH5[data$SEQ_AIH5==1] <- "Seq 1"
      data$SEQ_AIH5[data$SEQ_AIH5==2] <- "Seq 2"
      data$SEQ_AIH5[data$SEQ_AIH5==3] <- "Seq 3"
      data$SEQ_AIH5[data$SEQ_AIH5==4] <- NA
      data$SEQ_AIH5[data$SEQ_AIH5==999] <- NA
      data$SEQ_AIH5 <- factor(data$SEQ_AIH5)
    }

    # CBOR
    if ("CBOR" %in% variables_names) {
      data$CBOR <- as.character(data$CBOR)
      colnames(tabCBO)[1] <- "CBOR"
      data$CBOR <- factor(dplyr::left_join(data, tabCBO, by = "CBOR")$nome)
    }

    # CNAER
    if("CNAER" %in% variables_names){
      data$CNAER <- as.character(data$CNAER)
    }

    # VINCPREV
    if("VINCPREV" %in% variables_names){
      data$VINCPREV <- as.numeric(levels(data$VINCPREV))[data$VINCPREV]
      data$VINCPREV[data$VINCPREV==1] <- "Aut\u00f4nomo"
      data$VINCPREV[data$VINCPREV==2] <- "Desempregado"
      data$VINCPREV[data$VINCPREV==3] <- "Aposentado"
      data$VINCPREV[data$VINCPREV==4] <- "N\u00e3o segurado"
      data$VINCPREV[data$VINCPREV==5] <- "Empregado"
      data$VINCPREV[data$VINCPREV==6] <- "Empregador"
      data$VINCPREV[data$VINCPREV==0] <- NA
      data$VINCPREV[data$VINCPREV==9] <- NA
      data$VINCPREV <- factor(data$VINCPREV)
    }

    # GESTOR_COD
    if("GESTOR_COD" %in% variables_names){
      data$GESTOR_COD <- as.numeric(levels(data$GESTOR_COD))[data$GESTOR_COD]
      data$GESTOR_COD[data$GESTOR_COD==1] <- "TEMPO DE PERMANENCIA"
      data$GESTOR_COD[data$GESTOR_COD==2] <- "IDADE MENOR"
      data$GESTOR_COD[data$GESTOR_COD==3] <- "IDADE MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==4] <- "TEMPO DE PERMANENCIA E IDADE"
      data$GESTOR_COD[data$GESTOR_COD==5] <- "QUANTIDADE MAXIMA"
      data$GESTOR_COD[data$GESTOR_COD==7] <- "PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==8] <- "ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==9] <- "ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==10] <- "ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==11] <- "ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==14] <- "QTD"
      data$GESTOR_COD[data$GESTOR_COD==15] <- "QTD E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==16] <- "QTD E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==17] <- "QTD E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==18] <- "QTD E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==19] <- "QTD E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==38] <- "CBO"
      data$GESTOR_COD[data$GESTOR_COD==39] <- "CBO E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==40] <- "CBO E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==41] <- "CBO E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==42] <- "CBO E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==43] <- "CBO E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==46] <- "CBO E QTD"
      data$GESTOR_COD[data$GESTOR_COD==47] <- "CBO E QTD E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==48] <- "CBO E QTD E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==49] <- "CBO E QTD E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==50] <- "CBO E QTD E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==51] <- "CBO E QTD E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==70] <- "TELEFONE"
      data$GESTOR_COD[data$GESTOR_COD==71] <- "TELEFONE E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==72] <- "TELEFONE E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==73] <- "TELEFONE E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==74] <- "TELEFONE E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==75] <- "TELEFONE E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==78] <- "TELEFONE E QTD"
      data$GESTOR_COD[data$GESTOR_COD==79] <- "TELEFONE E QTD E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==80] <- "TELEFONE E QTD E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==81] <- "TELEFONE E QTD E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==82] <- "TELEFONE E QTD E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==83] <- "TELEFONE E QTD E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==1] <- "TEMPO DE PERMANENCIA"
      data$GESTOR_COD[data$GESTOR_COD==102] <- "TELEFONE E CBO"
      data$GESTOR_COD[data$GESTOR_COD==103] <- "TELEFONE E CBO E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==104] <- "TELEFONE E CBO E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==105] <- "TELEFONE E CBO E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==106] <- "TELEFONE E CBO E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==107] <- "TELEFONE E CBO E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==110] <- "TELEFONE E CBO E QTD"
      data$GESTOR_COD[data$GESTOR_COD==111] <- "TELEFONE E CBO E QTD E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==112] <- "TELEFONE E CBO E QTD E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==113] <- "TELEFONE E CBO E QTD E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==114] <- "TELEFONE E CBO E QTD E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==115] <- "TELEFONE E CBO E QTD E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==134] <- "CNS"
      data$GESTOR_COD[data$GESTOR_COD==136] <- "CNS E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==137] <- "CNS E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==138] <- "CNS E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==139] <- "CNS E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==142] <- "CNS E QTD"
      data$GESTOR_COD[data$GESTOR_COD==143] <- "CNS E QTD E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==144] <- "CNS E QTD E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==145] <- "CNS E QTD E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==146] <- "CNS E QTD E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==147] <- "CNS E QTD E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==166] <- "CNS E CBO"
      data$GESTOR_COD[data$GESTOR_COD==167] <- "CNS E CBO E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==168] <- "CNS E CBO E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==169] <- "CNS E CBO E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==170] <- "CNS E CBO E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==171] <- "CNS E CBO E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==174] <- "CNS E CBO E QTD"
      data$GESTOR_COD[data$GESTOR_COD==175] <- "CNS E CBO E QTD E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==176] <- "CNS E CBO E QTD E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==177] <- "CNS E CBO E QTD E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==178] <- "CNS E CBO E QTD E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==179] <- "CNS E CBO E QTD E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==198] <- "CNS E TELEFONE"
      data$GESTOR_COD[data$GESTOR_COD==199] <- "CNS E TELEFONE E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==2] <- "IDADE MENOR"
      data$GESTOR_COD[data$GESTOR_COD==200] <- "CNS E TELEFONE E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==201] <- "CNS E TELEFONE E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==202] <- "CNS E TELEFONE E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==203] <- "CNS E TELEFONE E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==206] <- "CNS E TELEFONE E QTD"
      data$GESTOR_COD[data$GESTOR_COD==207] <- "CNS E TELEFONE E QTD E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==208] <- "CNS E TELEFONE E QTD E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==209] <- "CNS E TELEFONE E QTD E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==210] <- "CNS E TELEFONE E QTD E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==211] <- "CNS E TELEFONE E QTD E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==230] <- "CNS E TELEFONE E CBO"
      data$GESTOR_COD[data$GESTOR_COD==231] <- "CNS E TELEFONE E CBO E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==232] <- "CNS E TELEFONE E CBO E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==233] <- "CNS E TELEFONE E CBO E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==234] <- "CNS E TELEFONE E CBO E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==235] <- "CNS E TELEFONE E CBO E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==238] <- "CNS E TELEFONE E CBO E QTD"
      data$GESTOR_COD[data$GESTOR_COD==239] <- "CNS E TELEFONE E CBO E QTD E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==240] <- "CNS E TELEFONE E CBO E QTD E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==241] <- "CNS E TELEFONE E CBO E QTD E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==242] <- "CNS E TELEFONE E CBO E QTD E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==243] <- "CNS E TELEFONE E CBO E QTD E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==3] <- "IDADE MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==4] <- "TEMPO DE PERMANENCIA E IDADE"
      data$GESTOR_COD[data$GESTOR_COD==5] <- "QUANTIDADE MAXIMA"
      data$GESTOR_COD[data$GESTOR_COD==6] <- "CID"
      data$GESTOR_COD[data$GESTOR_COD==7] <- "PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==8] <- "ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==9] <- "ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==10] <- "ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==11] <- "ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==14] <- "QTD"
      data$GESTOR_COD[data$GESTOR_COD==15] <- "QTD E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==16] <- "QTD E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==17] <- "QTD E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==18] <- "QTD E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==19] <- "QTD E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==22] <- "CID"
      data$GESTOR_COD[data$GESTOR_COD==23] <- "CID E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==24] <- "CID E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==25] <- "CID E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==26] <- "CID E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==27] <- "CID E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==30] <- "CID E QTD"
      data$GESTOR_COD[data$GESTOR_COD==31] <- "CID E QTD E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==32] <- "CID E QTD E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==33] <- "CID E QTD E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==34] <- "CID E QTD E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==35] <- "CID E QTD E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==38] <- "CBO"
      data$GESTOR_COD[data$GESTOR_COD==39] <- "CBO E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==40] <- "CBO E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==41] <- "CBO E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==42] <- "CBO E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==43] <- "CBO E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==46] <- "CBO E QTD"
      data$GESTOR_COD[data$GESTOR_COD==47] <- "CBO E QTD E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==48] <- "CBO E QTD E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==49] <- "CBO E QTD E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==50] <- "CBO E QTD E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==51] <- "CBO E QTD E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==70] <- "TELEFONE"
      data$GESTOR_COD[data$GESTOR_COD==71] <- "TELEFONE E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==72] <- "TELEFONE E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==73] <- "TELEFONE E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==74] <- "TELEFONE E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==75] <- "TELEFONE E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==78] <- "TELEFONE E QTD"
      data$GESTOR_COD[data$GESTOR_COD==79] <- "TELEFONE E QTD E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==80] <- "TELEFONE E QTD E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==81] <- "TELEFONE E QTD E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==82] <- "TELEFONE E QTD E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==83] <- "TELEFONE E QTD E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==102] <- "TELEFONE E CBO"
      data$GESTOR_COD[data$GESTOR_COD==103] <- "TELEFONE E CBO E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==104] <- "TELEFONE E CBO E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==105] <- "TELEFONE E CBO E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==106] <- "TELEFONE E CBO E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==107] <- "TELEFONE E CBO E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==110] <- "TELEFONE E CBO E QTD"
      data$GESTOR_COD[data$GESTOR_COD==111] <- "TELEFONE E CBO E QTD E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==112] <- "TELEFONE E CBO E QTD E ID.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==113] <- "TELEFONE E CBO E QTD E ID.MENOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==114] <- "TELEFONE E CBO E QTD E ID.MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==115] <- "TELEFONE E CBO E QTD E ID.MAIOR E PERM.MENOR"
      data$GESTOR_COD[data$GESTOR_COD==134] <- "CNS"
      data$GESTOR_COD[data$GESTOR_COD==135] <- "CNS E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==136] <- "CNS E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==137] <- "CNS E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==138] <- "CNS E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==139] <- "CNS E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==142] <- "CNS E QTD"
      data$GESTOR_COD[data$GESTOR_COD==143] <- "CNS E QTD E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==144] <- "CNS E QTD E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==145] <- "CNS E QTD E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==146] <- "CNS E QTD E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==147] <- "CNS E QTD E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==166] <- "CNS E CBO"
      data$GESTOR_COD[data$GESTOR_COD==167] <- "CNS E CBO E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==168] <- "CNS E CBO E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==169] <- "CNS E CBO E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==170] <- "CNS E CBO E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==171] <- "CNS E CBO E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==174] <- "CNS E CBO E QTD"
      data$GESTOR_COD[data$GESTOR_COD==175] <- "CNS E CBO E QTD E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==176] <- "CNS E CBO E QTD E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==177] <- "CNS E CBO E QTD E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==178] <- "CNS E CBO E QTD E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==179] <- "CNS E CBO E QTD E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==198] <- "CNS E TELEFONE"
      data$GESTOR_COD[data$GESTOR_COD==199] <- "CNS E TELEFONE E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==200] <- "CNS E TELEFONE E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==201] <- "CNS E TELEFONE E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==202] <- "CNS E TELEFONE E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==203] <- "CNS E TELEFONE E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==206] <- "CNS E TELEFONE E QTD"
      data$GESTOR_COD[data$GESTOR_COD==207] <- "CNS E TELEFONE E QTD E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==208] <- "CNS E TELEFONE E QTD E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==209] <- "CNS E TELEFONE E QTD E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==211] <- "CNS E TELEFONE E QTD E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==230] <- "CNS E TELEFONE E CBO"
      data$GESTOR_COD[data$GESTOR_COD==231] <- "CNS E TELEFONE E CBO E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==232] <- "CNS E TELEFONE E CBO E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==233] <- "CNS E TELEFONE E CBO E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==234] <- "CNS E TELEFONE E CBO E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==235] <- "CNS E TELEFONE E CBO E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==238] <- "CNS E TELEFONE E CBO E QTD"
      data$GESTOR_COD[data$GESTOR_COD==239] <- "CNS E TELEFONE E CBO E QTD E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==240] <- "CNS E TELEFONE E CBO E QTD E ID. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==241] <- "CNS E TELEFONE E CBO E QTD E ID. MENOR E PERM. MENOR"
      data$GESTOR_COD[data$GESTOR_COD==242] <- "CNS E TELEFONE E CBO E QTD E ID. MAIOR"
      data$GESTOR_COD[data$GESTOR_COD==243] <- "CNS E TELEFONE E CBO E QTD E ID. MAIOR E PERM. MENOR"
      data$GESTOR_COD <- factor(data$GESTOR_COD)
    }

    # GESTOR_TP
    if("GESTOR_TP" %in% variables_names){
      data$GESTOR_TP <- as.integer(data$GESTOR_TP)
    }

    # GESTOR_CPF
    if("GESTOR_CPF" %in% variables_names){
      data$GESTOR_CPF <- as.integer(data$GESTOR_CPF)
    }

    # GESTOR_DT
    if("GESTOR_DT" %in% variables_names){
      data$GESTOR_DT <- as.character(data$GESTOR_DT)
      data$GESTOR_DT <- as.Date(data$GESTOR_DT, format = "%Y%m%d")
    }

    # CNES
    if("CNES" %in% variables_names){
      data$CNES <- as.character(data$CNES)
    }

    # CNPJ_MANT
    if("CNPJ_MANT" %in% variables_names){
      data$CNPJ_MANT <- as.character(data$CNPJ_MANT)
    }

    # INFEHOSP
    if("INFEHOSP" %in% variables_names){
      data$INFEHOSP <- as.numeric(levels(data$INFEHOSP))[data$INFEHOSP]
      data$INFEHOSP[data$INFEHOSP==0] <- "N\u00e3o"
      data$INFEHOSP[data$INFEHOSP==1] <- "Sim"
      data$INFEHOSP <- factor(data$INFEHOSP)
    }

    # CID_ASSO
    if("CID_ASSO" %in% variables_names){
      data$CID_ASSO <- as.character(data$CID_ASSO)
    }

    # CID_MORTE
    if("CID_MORTE" %in% variables_names){
      data$CID_MORTE <- as.character(data$CID_MORTE)
    }

    # COMPLEX
    if("COMPLEX" %in% variables_names){
      data$COMPLEX <- as.numeric(levels(data$COMPLEX))[data$COMPLEX]
      data$COMPLEX[data$COMPLEX==1] <- "Aten\u00e7\u00e3o B\u00e1sica"
      data$COMPLEX[data$COMPLEX==2] <- "M\u00e9dia complexidade"
      data$COMPLEX[data$COMPLEX==3] <- "Alta complexidade"
      data$COMPLEX[data$COMPLEX==0] <- NA
      data$COMPLEX[data$COMPLEX==99] <- NA
      data$COMPLEX <- factor(data$COMPLEX)
    }

    # FINANC
    if("FINANC" %in% variables_names){
      data$FINANC <- as.numeric(levels(data$FINANC))[data$FINANC]
      data$FINANC[data$FINANC==1] <- "Aten\u00e7\u00e3o B\u00e1sica (PAB)"
      data$FINANC[data$FINANC==2] <- "Assist\u00eancia Farmac\u00eautica"
      data$FINANC[data$FINANC==4] <- "Fundo de A\u00e7\u00f5es Estrat\u00e9gicas e Compensa\u00e7\u00f5es FAEC"
      data$FINANC[data$FINANC==5] <- "Incentivo - MAC"
      data$FINANC[data$FINANC==6] <- "M\u00e9dia e Alta Complexidade (MAC)"
      data$FINANC[data$FINANC==7] <- "Vigil\u00e2ncia em Sa\u00fade"
      data$FINANC[data$FINANC==0] <- NA
      data$FINANC[data$FINANC==99] <- NA
      data$FINANC <- factor(data$FINANC)
    }

    # FAEC_TP
    if("FAEC_TP" %in% variables_names){
      data$FAEC_TP <- as.numeric(levels(data$FAEC_TP))[data$FAEC_TP]
      data$FAEC_TP[data$FAEC_TP==10000] <- "Aten\u00e7\u00e3o B\u00e1sica (PAB)"
      data$FAEC_TP[data$FAEC_TP==20000] <- "Assist\u00eancia Farmac\u00eautica"
      data$FAEC_TP[data$FAEC_TP==40001] <- "Coleta de material"
      data$FAEC_TP[data$FAEC_TP==40002] <- "Diagn\u00f3stico em laborat\u00f3rio cl\u00ednico"
      data$FAEC_TP[data$FAEC_TP==40003] <- "Coleta/exame an\u00e1tomo-patol\u00f3gico colo uterino"
      data$FAEC_TP[data$FAEC_TP==40004] <- "Diagn\u00f3stico em neurologia"
      data$FAEC_TP[data$FAEC_TP==40005] <- "Diagn\u00f3stico em otorrinolaringologia/fonoaudiologia"
      data$FAEC_TP[data$FAEC_TP==40006] <- "Diagn\u00f3stico em psicologia/psiquiatria"
      data$FAEC_TP[data$FAEC_TP==40007] <- "Consultas m\u00e9dicas/outros profissionais de n\u00edvel superior"
      data$FAEC_TP[data$FAEC_TP==40008] <- "Aten\u00e7\u00e3o domiciliar"
      data$FAEC_TP[data$FAEC_TP==40009] <- "Atendimento/acompanhamento em reabilita\u00e7\u00e3o f\u00edsica, mental, visual, auditiva e m\u00faltiplas defic"
      data$FAEC_TP[data$FAEC_TP==40010] <- "Atendimento/acompanhamento psicossocial"
      data$FAEC_TP[data$FAEC_TP==40011] <- "Atendimento/acompanhamento em sa\u00fade do idoso"
      data$FAEC_TP[data$FAEC_TP==40012] <- "Atendimento/acompanhamento de queimados"
      data$FAEC_TP[data$FAEC_TP==40013] <- "Atendimento/acompanhamento de diagn\u00f3stico de doen\u00e7as endocrinas/metab\u00f3licas e nutricionais"
      data$FAEC_TP[data$FAEC_TP==40014] <- "Tratamento de doen\u00e7as do sistema nervoso central e perif\u00e9rico"
      data$FAEC_TP[data$FAEC_TP==40015] <- "Tratamento de doen\u00e7as do aparelho da vis\u00e3o"
      data$FAEC_TP[data$FAEC_TP==40016] <- "Tratamento em oncologia"
      data$FAEC_TP[data$FAEC_TP==40017] <- "Nefrologia"
      data$FAEC_TP[data$FAEC_TP==40018] <- "Tratamentos odontol\u00f3gicos"
      data$FAEC_TP[data$FAEC_TP==40019] <- "Cirurgia do sistema nervoso central e perif\u00e9rico"
      data$FAEC_TP[data$FAEC_TP==40020] <- "Cirurgias de ouvido, nariz e garganta"
      data$FAEC_TP[data$FAEC_TP==40021] <- "Deformidade labio-palatal e cr\u00e2nio-facial"
      data$FAEC_TP[data$FAEC_TP==40022] <- "Cirurgia do aparelho da vis\u00e3o"
      data$FAEC_TP[data$FAEC_TP==40023] <- "Cirurgia do aparelho circulat\u00f3rio"
      data$FAEC_TP[data$FAEC_TP==40024] <- "Cirurgia do aparelho digestivo, org\u00e3os anexos e parede abdominal(inclui pr\u00e9 e p\u00f3s operat\u00f3rio)"
      data$FAEC_TP[data$FAEC_TP==40025] <- "Cirurgia do aparelho geniturin\u00e1rio"
      data$FAEC_TP[data$FAEC_TP==40026] <- "Tratamento de queimados"
      data$FAEC_TP[data$FAEC_TP==40027] <- "Cirurgia reparadora para lipodistrofia"
      data$FAEC_TP[data$FAEC_TP==40028] <- "Outras cirurgias pl\u00e1sticas/reparadoras"
      data$FAEC_TP[data$FAEC_TP==40029] <- "Cirurgia orofacial"
      data$FAEC_TP[data$FAEC_TP==40030] <- "Sequenciais"
      data$FAEC_TP[data$FAEC_TP==40031] <- "Cirurgias em nefrologia"
      data$FAEC_TP[data$FAEC_TP==40032] <- "Transplantes de org\u00e3os, tecidos e c\u00e9lulas"
      data$FAEC_TP[data$FAEC_TP==40033] <- "Medicamentos para transplante"
      data$FAEC_TP[data$FAEC_TP==40034] <- "OPM auditivas"
      data$FAEC_TP[data$FAEC_TP==40035] <- "OPM em odontologia"
      data$FAEC_TP[data$FAEC_TP==40036] <- "OPM em queimados"
      data$FAEC_TP[data$FAEC_TP==40037] <- "OPM em nefrologia"
      data$FAEC_TP[data$FAEC_TP==40038] <- "OPM para transplantes"
      data$FAEC_TP[data$FAEC_TP==40039] <- "Incentivos ao pr\u00e9-natal e nascimento"
      data$FAEC_TP[data$FAEC_TP==40040] <- "Incentivo ao registro c\u00edvil de nascimento"
      data$FAEC_TP[data$FAEC_TP==40041] <- "Central Nacional de Regula\u00e7\u00e3o de Alta Complexidade (CNRAC)"
      data$FAEC_TP[data$FAEC_TP==40042] <- "Reguladores de Atividade hormonal - Inibidores de prolactina"
      data$FAEC_TP[data$FAEC_TP==40043] <- "Pol\u00edtica Nacional de Cirurgias Eletivas"
      data$FAEC_TP[data$FAEC_TP==40044] <- "Redesigna\u00e7\u00e3o e Acompanhamento"
      data$FAEC_TP[data$FAEC_TP==40045] <- "Projeto Olhar Brasil"
      data$FAEC_TP[data$FAEC_TP==40046] <- "Mamografia para Rastreamento"
      data$FAEC_TP[data$FAEC_TP==40047] <- "Projeto Olhar Brasil - Consulta"
      data$FAEC_TP[data$FAEC_TP==40048] <- "Projeto Olhar Brasil - \u00d3culos"
      data$FAEC_TP[data$FAEC_TP==40049] <- "Implementar Cirg. CV Pedi\u00e1trica"
      data$FAEC_TP[data$FAEC_TP==40050] <- "Cirurgias Eletivas - Componente I"
      data$FAEC_TP[data$FAEC_TP==40051] <- "Cirurgias Eletivas - Componente II"
      data$FAEC_TP[data$FAEC_TP==40052] <- "Cirurgias Eletivas - Componente III"
      data$FAEC_TP[data$FAEC_TP==40053] <- "Pr\u00f3tese Mam\u00e1ria - Exames"
      data$FAEC_TP[data$FAEC_TP==40054] <- "Pr\u00f3tese Mam\u00e1ria - Cirurgia"
      data$FAEC_TP[data$FAEC_TP==40055] <- "Transplante - Histocompatibilidade"
      data$FAEC_TP[data$FAEC_TP==40056] <- "Triagem Neonatal"
      data$FAEC_TP[data$FAEC_TP==40057] <- "Controle de qualidade do exame citopatol\u00f3gico do colo de \u00fatero"
      data$FAEC_TP[data$FAEC_TP==40058] <- "Exames do Leite Materno"
      data$FAEC_TP[data$FAEC_TP==40059] <- "Aten\u00e7\u00e3o as Pessoas em Situa\u00e7\u00e3o de Viol\u00eancia Sexual"
      data$FAEC_TP[data$FAEC_TP==40060] <- "Sangue e Hemoderivados"
      data$FAEC_TP[data$FAEC_TP==40061] <- "Mamografia para rastreamento em faixa et\u00e1ria recomendada"
      data$FAEC_TP[data$FAEC_TP==40062] <- "Doen\u00e7as Raras"
      data$FAEC_TP[data$FAEC_TP==40063] <- "Cadeiras de Rodas"
      data$FAEC_TP[data$FAEC_TP==40064] <- "Sistema de Frequencia Modulada Pessoal-FM"
      data$FAEC_TP[data$FAEC_TP==40065] <- "Medicamentos em Urg\u00eancia"
      data$FAEC_TP[data$FAEC_TP==40066] <- "Cirurgias Eletivas - Componente \u00danico"
      data$FAEC_TP[data$FAEC_TP==40067] <- "Aten\u00e7\u00e3o Especializada em Sa\u00fade Auditiva"
      data$FAEC_TP[data$FAEC_TP==40068] <- "Terapias Especializadas em Angiologia"
      data$FAEC_TP[data$FAEC_TP==21012] <- "FAEC CNRAC (21012-c\u00f3d ant \u00e0 tab unif-v\u00e1l p/2008-01)"
      data$FAEC_TP[data$FAEC_TP==21014] <- "FAEC Eletiv(21014-c\u00f3d ant \u00e0 tab unif-v\u00e1l p/2008-01)"
      data$FAEC_TP[data$FAEC_TP==50000] <- "Incentivo - MAC"
      data$FAEC_TP[data$FAEC_TP==60000] <- "M\u00e9dia e Alta Complexidade (MAC)"
      data$FAEC_TP[data$FAEC_TP==70000] <- "Vigil\u00e2ncia em Sa\u00fade"
      data$FAEC_TP[data$FAEC_TP==80000] <- "Gest\u00e3o do SUS"
      data$FAEC_TP <- factor(data$FAEC_TP)
    }

    # REGCT
    if("REGCT" %in% variables_names){
      data$REGCT <- as.numeric(levels(data$REGCT))[data$REGCT]
      data$REGCT[data$REGCT==7100] <- "TABELA DE NAO GERACAO DE CREDITO POR PRODUCAO NA INTERNACAO E/OU AMBULATORIO"
      data$REGCT[data$REGCT==7101] <- "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO NA MEDIA COMPLEXIDADE AMBULATORIAL"
      data$REGCT[data$REGCT==7102] <- "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO NA MEDIA COMPLEXIDADE HOSPITALAR"
      data$REGCT[data$REGCT==7103] <- "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO NA ALTA COMPLEXIDADE AMBULATORIAL"
      data$REGCT[data$REGCT==7104] <- "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO NA ALTA COMPLEXIDADE HOSPITALAR"
      data$REGCT[data$REGCT==7105] <- "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO PARA OS PROCEDIMENTOS FINANCIADOS COM O FAEC"
      data$REGCT[data$REGCT==7106] <- "ESTABELECIMENTO SEM GERA\u00c7\u00c3O DE CREDITO TOTAL - EXCLUINDO FAEC"
      data$REGCT[data$REGCT==7107] <- "ESTABELECIMENTO SEM GERACAO DE CREDITO NAS ACOES ESPEC. DE ODONTOLOGIA(INCENTIVO CEO I,II E III)"
      data$REGCT[data$REGCT==7108] <- "ESTABELECIMENTO SEM GERACAO DE CREDITO(INCENTIVO A SAUDE DO TRABALHADOR)"
      data$REGCT[data$REGCT==7109] <- "ESTABELECIMENTO SEM GERACAO DE CREDITO TOTAL-MEC"
      data$REGCT[data$REGCT==7110] <- "ESTABELECIMENTO DE SAUDE DA ESTRUTURA DO MINISTERIO DA SAUDE - SEM GERA\u00c7AO DE CREDITO TOTAL"
      data$REGCT[data$REGCT==7111] <- "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO - NASF, EXCETO FAEC"
      data$REGCT[data$REGCT==7112] <- "ESTABELECIMENTO SEM GERA\u00c7\u00c3O DE CREDITO TOTAL - INCLUINDO FAEC  - EXCLUSIVO PARA REDE SARAH"
      data$REGCT[data$REGCT==7113] <- "ESTABELECIMENTO SEM GERA\u00c7\u00c3O DE CREDITO TOTAL - INCLUINDO FAEC - OUTROS ESTABELECIMENTOS FEDERAIS"
      data$REGCT[data$REGCT==7114] <- "ESTABELECIMENTO DE SA\u00daDE SEM GERA\u00c7\u00c3O DE CR\u00c9DITO TOTAL, INCLUSIVE FAEC - PRONTO ATENDIMENTO"
      data$REGCT[data$REGCT==7115] <- "ESTABELECIMENTO DE SA\u00daDE SEM GERA\u00c7\u00c3O DE CR\u00c9DITO NA M\u00c9DIA COMPLEXIDADE - HU/MEC"
      data$REGCT[data$REGCT==7116] <- "ESTABELECIMENTO DE SA\u00daDE SEM GERA\u00c7\u00c3O DE CR\u00c9DITO NA M\u00c9DIA COMPLEXIDADE - LRPD"
      data$REGCT[data$REGCT==7117] <- "Estabelecimento de Sa\u00fade sem gera\u00e7\u00e3o de cr\u00e9dito na m\u00e9dia complexidade (exceto OPM) - CER"
      data$REGCT[data$REGCT==0] <- "Sem regra contratual"
      data$REGCT <- factor(data$REGCT)
    }

    # RACA_COR
    if("RACA_COR" %in% variables_names){
      data$RACA_COR <- as.numeric(levels(data$RACA_COR))[data$RACA_COR]
      data$RACA_COR[data$RACA_COR==1] <- "Branca"
      data$RACA_COR[data$RACA_COR==2] <- "Preta"
      data$RACA_COR[data$RACA_COR==3] <- "Parda"
      data$RACA_COR[data$RACA_COR==4] <- "Amarela"
      data$RACA_COR[data$RACA_COR==5] <- "Ind\u00edgena"
      data$RACA_COR[data$RACA_COR==0] <- NA
      data$RACA_COR[data$RACA_COR==99] <- NA
      data$RACA_COR <- factor(data$RACA_COR)
    }

    # ETNIA
    if("ETNIA" %in% variables_names){
      data$ETNIA <- as.character(data$ETNIA)
      data$ETNIA[data$ETNIA == "0001"] <- "ACONA (WAKONAS, NACONAS, JAKONA, ACORANES)"
      data$ETNIA[data$ETNIA == "0002"] <-	"AIKANA (AIKANA, MAS SAKA,TUBARAO)"
      data$ETNIA[data$ETNIA == "0003"] <-	"AJURU"
      data$ETNIA[data$ETNIA == "0004"] <-	"AKUNSU (AKUNT'SU)"
      data$ETNIA[data$ETNIA == "0005"] <-	"AMANAYE"
      data$ETNIA[data$ETNIA == "0006"] <-	"AMONDAWA"
      data$ETNIA[data$ETNIA == "0007"] <-	"ANAMBE"
      data$ETNIA[data$ETNIA == "0008"] <-	"APARAI (APALAI)"
      data$ETNIA[data$ETNIA == "0009"] <-	"APIAKA (APIACA)"
      data$ETNIA[data$ETNIA == "0010"] <-	"APINAYE (APINAJE/APINAIE/APINAGE)"
      data$ETNIA[data$ETNIA == "0011"] <-	"APURINA (APORINA, IPURINA, IPURINA, IPURINAN)"
      data$ETNIA[data$ETNIA == "0012"] <-	"ARANA (ARACUAI DO VALE DO JEQUITINHONHA)"
      data$ETNIA[data$ETNIA == "0013"] <-	"ARAPASO (ARAPACO)"
      data$ETNIA[data$ETNIA == "0014"] <-	"ARARA DE RONDONIA (KARO, URUCU, URUKU)"
      data$ETNIA[data$ETNIA == "0015"] <-	"ARARA DO ACRE (SHAWANAUA, AMAWAKA)"
      data$ETNIA[data$ETNIA == "0016"] <-	"ARARA DO ARIPUANA (ARARA DO BEIRADAO/ARI-PUANA)"
      data$ETNIA[data$ETNIA == "0017"] <-	"ARARA DO PARA (UKARAGMA, UKARAMMA)"
      data$ETNIA[data$ETNIA == "0018"] <-	"ARAWETE (ARAUETE)"
      data$ETNIA[data$ETNIA == "0019"] <-	"ARIKAPU (ARICAPU, ARIKAPO, MASUBI, MAXUBI)"
      data$ETNIA[data$ETNIA == "0020"] <-	"ARIKEM (ARIQUEN, ARIQUEME, ARIKEME)"
      data$ETNIA[data$ETNIA == "0021"] <-	"ARIKOSE (ARICOBE)"
      data$ETNIA[data$ETNIA == "0022"] <-	"ARUA"
      data$ETNIA[data$ETNIA == "0023"] <-	"ARUAK (ARAWAK)"
      data$ETNIA[data$ETNIA == "0024"] <-	"ASHANINKA (KAMPA)"
      data$ETNIA[data$ETNIA == "0025"] <-	"ASURINI DO TOCANTINS (AKUAWA/AKWAWA)"
      data$ETNIA[data$ETNIA == "0026"] <-	"ASURINI DO XINGU (AWAETE)"
      data$ETNIA[data$ETNIA == "0027"] <-	"ATIKUM (ATICUM)"
      data$ETNIA[data$ETNIA == "0028"] <-	"AVA - CANOEIRO"
      data$ETNIA[data$ETNIA == "0029"] <-	"AWETI (AUETI/AUETO)"
      data$ETNIA[data$ETNIA == "0030"] <-	"BAKAIRI (KURA, BACAIRI)"
      data$ETNIA[data$ETNIA == "0031"] <-	"BANAWA YAFI (BANAWA, BANAWA-JAFI)"
      data$ETNIA[data$ETNIA == "0032"] <-	"BANIWA (BANIUA, BANIVA, WALIMANAI, WAKUENAI)"
      data$ETNIA[data$ETNIA == "0033"] <-	"BARA (WAIPINOMAKA)"
      data$ETNIA[data$ETNIA == "0034"] <-	"BARASANA (HANERA)"
      data$ETNIA[data$ETNIA == "0035"] <-	"BARE"
      data$ETNIA[data$ETNIA == "0036"] <-	"BORORO (BOE)"
      data$ETNIA[data$ETNIA == "0037"] <-	"BOTOCUDO (GEREN)"
      data$ETNIA[data$ETNIA == "0038"] <-	"CANOE"
      data$ETNIA[data$ETNIA == "0039"] <-	"CASSUPA"
      data$ETNIA[data$ETNIA == "0040"] <-	"CHAMACOCO"
      data$ETNIA[data$ETNIA == "0041"] <-	"CHIQUITANO (XIQUITANO)"
      data$ETNIA[data$ETNIA == "0042"] <-	"CIKIYANA (SIKIANA)"
      data$ETNIA[data$ETNIA == "0043"] <-	"CINTA LARGA (MATETAMAE)"
      data$ETNIA[data$ETNIA == "0044"] <-	"COLUMBIARA (CORUMBIARA)"
      data$ETNIA[data$ETNIA == "0045"] <-	"DENI"
      data$ETNIA[data$ETNIA == "0046"] <-	"DESANA (DESANA, DESANO, DESSANO, WIRA, UMUKOMASA)"
      data$ETNIA[data$ETNIA == "0047"] <-	"DIAHUI (JAHOI, JAHUI, DIARROI)"
      data$ETNIA[data$ETNIA == "0048"] <-	"ENAWENE-NAWE (SALUMA)"
      data$ETNIA[data$ETNIA == "0049"] <-	"FULNI-O"
      data$ETNIA[data$ETNIA == "0050"] <-	"GALIBI (GALIBI DO OIAPOQUE, KARINHA)"
      data$ETNIA[data$ETNIA == "0051"] <-	"GALIBI MARWORNO (GALIBI DO UACA, ARUA)"
      data$ETNIA[data$ETNIA == "0052"] <-	"GAVIAO DE RONDONIA (DIGUT)"
      data$ETNIA[data$ETNIA == "0053"] <-	"GAVIAO KRIKATEJE"
      data$ETNIA[data$ETNIA == "0054"] <-	"GAVIAO PARKATEJE (PARKATEJE)"
      data$ETNIA[data$ETNIA == "0055"] <-	"GAVIAO PUKOBIE (PUKOBIE, PYKOPJE, GAVIAO DO MARANHAO)"
      data$ETNIA[data$ETNIA == "0056"] <-	"GUAJA (AWA, AVA)"
      data$ETNIA[data$ETNIA == "0057"] <-	"GUAJAJARA (TENETEHARA)"
      data$ETNIA[data$ETNIA == "0058"] <-	"GUARANI KAIOWA (PAI TAVYTERA)"
      data$ETNIA[data$ETNIA == "0059"] <-	"GUARANI M'BYA"
      data$ETNIA[data$ETNIA == "0060"] <-	"GUARANI NANDEVA (AVAKATUETE, CHIRIPA,NHANDEWA, AVA GUARANI)"
      data$ETNIA[data$ETNIA == "0061"] <-	"GUATO"
      data$ETNIA[data$ETNIA == "0062"] <-	"HIMARIMA (HIMERIMA)"
      data$ETNIA[data$ETNIA == "0063"] <-	"INGARIKO (INGARICO, AKAWAIO, KAPON)"
      data$ETNIA[data$ETNIA == "0064"] <-	"IRANXE (IRANTXE)"
      data$ETNIA[data$ETNIA == "0065"] <-	"ISSE"
      data$ETNIA[data$ETNIA == "0066"] <-	"JABOTI (JABUTI, KIPIU, YABYTI)"
      data$ETNIA[data$ETNIA == "0067"] <-	"JAMAMADI (YAMAMADI, DJEOROMITXI)"
      data$ETNIA[data$ETNIA == "0068"] <-	"JARAWARA"
      data$ETNIA[data$ETNIA == "0069"] <-	"JIRIPANCO (JERIPANCO, GERIPANCO)"
      data$ETNIA[data$ETNIA == "0070"] <-	"JUMA (YUMA)"
      data$ETNIA[data$ETNIA == "0071"] <-	"JURUNA"
      data$ETNIA[data$ETNIA == "0072"] <-	"JURUTI (YURITI)"
      data$ETNIA[data$ETNIA == "0073"] <-	"KAAPOR (URUBU-KAAPOR, KA'APOR, KAAPORTE)"
      data$ETNIA[data$ETNIA == "0074"] <-	"KADIWEU (CADUVEO, CADIUEU)"
      data$ETNIA[data$ETNIA == "0075"] <-	"KAIABI (CAIABI, KAYABI)"
      data$ETNIA[data$ETNIA == "0076"] <-	"KAIMBE (CAIMBE)"
      data$ETNIA[data$ETNIA == "0077"] <-	"KAINGANG (CAINGANGUE)"
      data$ETNIA[data$ETNIA == "0078"] <-	"KAIXANA (CAIXANA)"
      data$ETNIA[data$ETNIA == "0079"] <-	"KALABASSA (CALABASSA, CALABACAS)"
      data$ETNIA[data$ETNIA == "0080"] <-	"KALANCO"
      data$ETNIA[data$ETNIA == "0081"] <-	"KALAPALO (CALAPALO)"
      data$ETNIA[data$ETNIA == "0082"] <-	"KAMAYURA (CAMAIURA, KAMAIURA)"
      data$ETNIA[data$ETNIA == "0083"] <-	"KAMBA (CAMBA)"
      data$ETNIA[data$ETNIA == "0084"] <-	"KAMBEBA (CAMBEBA, OMAGUA)"
      data$ETNIA[data$ETNIA == "0085"] <-	"KAMBIWA (CAMBIUA)"
      data$ETNIA[data$ETNIA == "0086"] <-	"KAMBIWA PIPIPA (PIPIPA)"
      data$ETNIA[data$ETNIA == "0087"] <-	"KAMPE"
      data$ETNIA[data$ETNIA == "0088"] <-	"KANAMANTI (KANAMATI, CANAMANTI)"
      data$ETNIA[data$ETNIA == "0089"] <-	"KANAMARI (CANAMARI, KANAMARY, TUKUNA)"
      data$ETNIA[data$ETNIA == "0090"] <-	"KANELA APANIEKRA (CANELA)"
      data$ETNIA[data$ETNIA == "0091"] <-	"KANELA RANKOKAMEKRA (CANELA)"
      data$ETNIA[data$ETNIA == "0092"] <-	"KANINDE"
      data$ETNIA[data$ETNIA == "0093"] <-	"KANOE (CANOE)"
      data$ETNIA[data$ETNIA == "0094"] <-	"KANTARURE (CANTARURE)"
      data$ETNIA[data$ETNIA == "0095"] <-	"KAPINAWA (CAPINAUA)"
      data$ETNIA[data$ETNIA == "0096"] <-	"KARAJA (CARAJA)"
      data$ETNIA[data$ETNIA == "0097"] <-	"KARAJA/JAVAE (JAVAE)"
      data$ETNIA[data$ETNIA == "0098"] <-	"KARAJA/XAMBIOA (KARAJA DO NORTE)"
      data$ETNIA[data$ETNIA == "0099"] <-	"KARAPANA (CARAPANA, MUTEAMASA, UKOPINOPONA)"
      data$ETNIA[data$ETNIA == "0100"] <-	"KARAPOTO (CARAPOTO)"
      data$ETNIA[data$ETNIA == "0101"] <-	"KARIPUNA (CARIPUNA)"
      data$ETNIA[data$ETNIA == "0102"] <-	"KARIPUNA DO AMAPA (CARIPUNA)"
      data$ETNIA[data$ETNIA == "0103"] <-	"KARIRI (CARIRI)"
      data$ETNIA[data$ETNIA == "0104"] <-	"KARIRI-XOCO (CARIRI-CHOCO)"
      data$ETNIA[data$ETNIA == "0105"] <-	"KARITIANA (CARITIANA)"
      data$ETNIA[data$ETNIA == "0106"] <-	"KATAWIXI (KATAUIXI,KATAWIN, KATAWISI, CATAUICHI)"
      data$ETNIA[data$ETNIA == "0107"] <-	"KATUENA (CATUENA, KATWENA)"
      data$ETNIA[data$ETNIA == "0108"] <-	"KATUKINA (PEDA DJAPA)"
      data$ETNIA[data$ETNIA == "0109"] <-	"KATUKINA DO ACRE"
      data$ETNIA[data$ETNIA == "0110"] <-	"KAXARARI (CAXARARI)"
      data$ETNIA[data$ETNIA == "0111"] <-	"KAXINAWA (HUNI-KUIN, CASHINAUA, CAXINAUA)"
      data$ETNIA[data$ETNIA == "0112"] <-	"KAXIXO"
      data$ETNIA[data$ETNIA == "0113"] <-	"KAXUYANA (CAXUIANA)"
      data$ETNIA[data$ETNIA == "0114"] <-	"KAYAPO (CAIAPO)"
      data$ETNIA[data$ETNIA == "0115"] <-	"KAYAPO KARARAO (KARARAO)"
      data$ETNIA[data$ETNIA == "0116"] <-	"KAYAPO TXUKAHAMAE (TXUKAHAMAE)"
      data$ETNIA[data$ETNIA == "0117"] <-	"KAYAPO XICRIM (XIKRIN)"
      data$ETNIA[data$ETNIA == "0118"] <-	"KAYUISANA (CAIXANA, CAUIXANA, KAIXANA)"
      data$ETNIA[data$ETNIA == "0119"] <-	"KINIKINAWA (GUAN, KOINUKOEN, KINIKINAO)"
      data$ETNIA[data$ETNIA == "0120"] <-	"KIRIRI"
      data$ETNIA[data$ETNIA == "0121"] <-	"KOCAMA (COCAMA, KOKAMA)"
      data$ETNIA[data$ETNIA == "0122"] <-	"KOKUIREGATEJE"
      data$ETNIA[data$ETNIA == "0123"] <-	"KORUBO"
      data$ETNIA[data$ETNIA == "0124"] <-	"KRAHO (CRAO, KRAO)"
      data$ETNIA[data$ETNIA == "0125"] <-	"KREJE (KRENYE)"
      data$ETNIA[data$ETNIA == "0126"] <-	"KRENAK (BORUN, CRENAQUE)"
      data$ETNIA[data$ETNIA == "0127"] <-	"KRIKATI (KRINKATI)"
      data$ETNIA[data$ETNIA == "0128"] <-	"KUBEO (CUBEO, COBEWA, KUBEWA, PAMIWA, CUBEU)"
      data$ETNIA[data$ETNIA == "0129"] <-	"KUIKURO (KUIKURU, CUICURO)"
      data$ETNIA[data$ETNIA == "0130"] <-	"KUJUBIM (KUYUBI, CUJUBIM)"
      data$ETNIA[data$ETNIA == "0131"] <-	"KULINA PANO (CULINA)"
      data$ETNIA[data$ETNIA == "0132"] <-	"KULINA/MADIHA (CULINA, MADIJA, MADIHA)"
      data$ETNIA[data$ETNIA == "0133"] <-	"KURIPAKO (CURIPACO, CURRIPACO, CORIPACO, WAKUENAI)"
      data$ETNIA[data$ETNIA == "0134"] <-	"KURUAIA (CURUAIA)"
      data$ETNIA[data$ETNIA == "0135"] <-	"KWAZA (COAIA, KOAIA)"
      data$ETNIA[data$ETNIA == "0136"] <-	"MACHINERI (MANCHINERI, MANXINERI)"
      data$ETNIA[data$ETNIA == "0137"] <-	"MACURAP (MAKURAP)"
      data$ETNIA[data$ETNIA == "0138"] <-	"MAKU DOW (DOW)"
      data$ETNIA[data$ETNIA == "0139"] <-	"MAKU HUPDA (HUPDA)"
      data$ETNIA[data$ETNIA == "0140"] <-	"MAKU NADEB (NADEB)"
      data$ETNIA[data$ETNIA == "0141"] <-	"MAKU YUHUPDE (YUHUPDE)"
      data$ETNIA[data$ETNIA == "0142"] <-	"MAKUNA (MACUNA, YEBA-MASA)"
      data$ETNIA[data$ETNIA == "0143"] <-	"MAKUXI (MACUXI, MACHUSI, PEMON)"
      data$ETNIA[data$ETNIA == "0144"] <-	"MARIMAM (MARIMA)"
      data$ETNIA[data$ETNIA == "0145"] <-	"MARUBO"
      data$ETNIA[data$ETNIA == "0146"] <-	"MATIPU"
      data$ETNIA[data$ETNIA == "0147"] <-	"MATIS"
      data$ETNIA[data$ETNIA == "0148"] <-	"MATSE (MAYORUNA)"
      data$ETNIA[data$ETNIA == "0149"] <-	"MAXAKALI (MAXACALI)"
      data$ETNIA[data$ETNIA == "0150"] <-	"MAYA (MAYA)"
      data$ETNIA[data$ETNIA == "0151"] <-	"MAYTAPU"
      data$ETNIA[data$ETNIA == "0152"] <-	"MEHINAKO (MEINAKU, MEINACU)"
      data$ETNIA[data$ETNIA == "0153"] <-	"MEKEN (MEQUEM, MEKHEM, MICHENS)"
      data$ETNIA[data$ETNIA == "0154"] <-	"MENKY (MYKY, MUNKU, MENKI, MYNKY)"
      data$ETNIA[data$ETNIA == "0155"] <-	"MIRANHA (MIRANHA, MIRANA)"
      data$ETNIA[data$ETNIA == "0156"] <-	"MIRITI TAPUIA (MIRITI-TAPUYA, BUIA-TAPUYA)"
      data$ETNIA[data$ETNIA == "0157"] <-	"MUNDURUKU (MUNDURUCU)"
      data$ETNIA[data$ETNIA == "0158"] <-	"MURA"
      data$ETNIA[data$ETNIA == "0159"] <-	"NAHUKWA (NAFUQUA)"
      data$ETNIA[data$ETNIA == "0160"] <-	"NAMBIKWARA DO CAMPO (HALOTESU, KITHAULU, WAKALITESU, SAWENTES, MANDUKA)"
      data$ETNIA[data$ETNIA == "0161"] <-	"NAMBIKWARA DO NORTE (NEGAROTE ,MAMAINDE, LATUNDE, SABANE E MANDUKA, TAWANDE)"
      data$ETNIA[data$ETNIA == "0162"] <-	"NAMBIKWARA DO SUL (WASUSU ,HAHAINTESU, ALANTESU, WAIKISU, ALAKETESU, WASUSU, SARARE)"
      data$ETNIA[data$ETNIA == "0163"] <-	"NARAVUTE (NARUVOTO)"
      data$ETNIA[data$ETNIA == "0164"] <-	"NAWA (NAUA)"
      data$ETNIA[data$ETNIA == "0165"] <-	"NUKINI (NUQUINI, NUKUINI)"
      data$ETNIA[data$ETNIA == "0166"] <-	"OFAIE (OFAYE-XAVANTE)"
      data$ETNIA[data$ETNIA == "0167"] <-	"ORO WIN"
      data$ETNIA[data$ETNIA == "0168"] <-	"PAIAKU (JENIPAPO-KANINDE)"
      data$ETNIA[data$ETNIA == "0169"] <-	"PAKAA NOVA (WARI, PACAAS NOVOS)"
      data$ETNIA[data$ETNIA == "0170"] <-	"PALIKUR (AUKWAYENE, AUKUYENE, PALIKU'ENE)"
      data$ETNIA[data$ETNIA == "0171"] <-	"PANARA (KRENHAKARORE , KRENAKORE, KRENA-KARORE)"
      data$ETNIA[data$ETNIA == "0172"] <-	"PANKARARE (PANCARARE)"
      data$ETNIA[data$ETNIA == "0173"] <-	"PANKARARU (PANCARARU)"
      data$ETNIA[data$ETNIA == "0174"] <-	"PANKARARU KALANKO (KALANKO)"
      data$ETNIA[data$ETNIA == "0175"] <-	"PANKARARU KARUAZU (KARUAZU)"
      data$ETNIA[data$ETNIA == "0176"] <-	"PANKARU (PANCARU)"
      data$ETNIA[data$ETNIA == "0177"] <-	"PARAKANA (PARACANA, APITEREWA, AWAETE)"
      data$ETNIA[data$ETNIA == "0178"] <-	"PARECI (PARESI, HALITI)"
      data$ETNIA[data$ETNIA == "0179"] <-	"PARINTINTIN"
      data$ETNIA[data$ETNIA == "0180"] <-	"PATAMONA (KAPON)"
      data$ETNIA[data$ETNIA == "0181"] <-	"PATAXO"
      data$ETNIA[data$ETNIA == "0182"] <-	"PATAXO HA-HA-HAE"
      data$ETNIA[data$ETNIA == "0183"] <-	"PAUMARI (PALMARI)"
      data$ETNIA[data$ETNIA == "0184"] <-	"PAUMELENHO"
      data$ETNIA[data$ETNIA == "0185"] <-	"PIRAHA (MURA PIRAHA)"
      data$ETNIA[data$ETNIA == "0186"] <-	"PIRATUAPUIA (PIRATAPUYA, PIRATAPUYO, PIRA-TAPUYA, WAIKANA)"
      data$ETNIA[data$ETNIA == "0187"] <-	"PITAGUARI"
      data$ETNIA[data$ETNIA == "0188"] <-	"POTIGUARA"
      data$ETNIA[data$ETNIA == "0189"] <-	"POYANAWA (POIANAUA)"
      data$ETNIA[data$ETNIA == "0190"] <-	"RIKBAKTSA (CANOEIROS, ERIGPAKTSA)"
      data$ETNIA[data$ETNIA == "0191"] <-	"SAKURABIAT(MEKENS, SAKIRABIAP, SAKIRABIAR)"
      data$ETNIA[data$ETNIA == "0192"] <-	"SATERE-MAWE (SATERE-MAUE)"
      data$ETNIA[data$ETNIA == "0193"] <-	"SHANENAWA (KATUKINA)"
      data$ETNIA[data$ETNIA == "0194"] <-	"SIRIANO (SIRIA-MASA)"
      data$ETNIA[data$ETNIA == "0195"] <-	"SURIANA"
      data$ETNIA[data$ETNIA == "0196"] <-	"SURUI DE RONDONIA (PAITER)"
      data$ETNIA[data$ETNIA == "0197"] <-	"SURUI DO PARA (AIKEWARA)"
      data$ETNIA[data$ETNIA == "0198"] <-	"SUYA (SUIA/KISEDJE)"
      data$ETNIA[data$ETNIA == "0199"] <-	"TAPAYUNA (BEICO-DE-PAU)"
      data$ETNIA[data$ETNIA == "0200"] <-	"TAPEBA"
      data$ETNIA[data$ETNIA == "0201"] <-	"TAPIRAPE (TAPI'IRAPE)"
      data$ETNIA[data$ETNIA == "0202"] <-	"TAPUIA (TAPUIA-XAVANTE, TAPUIO)"
      data$ETNIA[data$ETNIA == "0203"] <-	"TARIANO (TARIANA, TALIASERI)"
      data$ETNIA[data$ETNIA == "0204"] <-	"TAUREPANG (TAULIPANG, PEMON, AREKUNA, PAGEYN)"
      data$ETNIA[data$ETNIA == "0205"] <-	"TEMBE"
      data$ETNIA[data$ETNIA == "0206"] <-	"TENHARIM"
      data$ETNIA[data$ETNIA == "0207"] <-	"TERENA"
      data$ETNIA[data$ETNIA == "0208"] <-	"TICUNA (TIKUNA, TUKUNA, MAGUTA)"
      data$ETNIA[data$ETNIA == "0209"] <-	"TINGUI BOTO"
      data$ETNIA[data$ETNIA == "0210"] <-	"TIRIYO EWARHUYANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)"
      data$ETNIA[data$ETNIA == "0211"] <-	"TIRIYO KAH'YANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)"
      data$ETNIA[data$ETNIA == "0212"] <-	"TIRIYO TSIKUYANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)"
      data$ETNIA[data$ETNIA == "0213"] <-	"TORA"
      data$ETNIA[data$ETNIA == "0214"] <-	"TREMEMBE"
      data$ETNIA[data$ETNIA == "0215"] <-	"TRUKA"
      data$ETNIA[data$ETNIA == "0216"] <-	"TRUMAI"
      data$ETNIA[data$ETNIA == "0217"] <-	"TSOHOM DJAPA (TSUNHUM-DJAPA)"
      data$ETNIA[data$ETNIA == "0218"] <-	"TUKANO (TUCANO, YE'PA-MASA, DASEA)"
      data$ETNIA[data$ETNIA == "0219"] <-	"TUMBALALA"
      data$ETNIA[data$ETNIA == "0220"] <-	"TUNAYANA"
      data$ETNIA[data$ETNIA == "0221"] <-	"TUPARI"
      data$ETNIA[data$ETNIA == "0222"] <-	"TUPINAMBA"
      data$ETNIA[data$ETNIA == "0223"] <-	"TUPINIQUIM"
      data$ETNIA[data$ETNIA == "0224"] <-	"TURIWARA"
      data$ETNIA[data$ETNIA == "0225"] <-	"TUXA"
      data$ETNIA[data$ETNIA == "0226"] <-	"TUYUKA (TUIUCA, DOKAPUARA, UTAPINOMAKAPHONA)"
      data$ETNIA[data$ETNIA == "0227"] <-	"TXIKAO (TXICAO, IKPENG)"
      data$ETNIA[data$ETNIA == "0228"] <-	"UMUTINA (OMOTINA, BARBADOS)"
      data$ETNIA[data$ETNIA == "0229"] <-	"URU-EU-WAU-WAU (URUEU-UAU-UAU, URUPAIN, URUPA)"
      data$ETNIA[data$ETNIA == "0230"] <-	"WAI WAI HIXKARYANA (HIXKARYANA)"
      data$ETNIA[data$ETNIA == "0231"] <-	"WAI WAI KARAFAWYANA (KARAFAWYANA, KARA-PAWYANA)"
      data$ETNIA[data$ETNIA == "0232"] <-	"WAI WAI XEREU (XEREU)"
      data$ETNIA[data$ETNIA == "0233"] <-	"WAI WAI KATUENA (KATUENA)"
      data$ETNIA[data$ETNIA == "0234"] <-	"WAI WAI MAWAYANA (MAWAYANA)"
      data$ETNIA[data$ETNIA == "0235"] <-	"WAIAPI (WAYAMPI, OYAMPI, WAYAPY, )"
      data$ETNIA[data$ETNIA == "0236"] <-	"WAIMIRI ATROARI (KINA)"
      data$ETNIA[data$ETNIA == "0237"] <-	"WANANO (UANANO, WANANA)"
      data$ETNIA[data$ETNIA == "0238"] <-	"WAPIXANA (UAPIXANA, VAPIDIANA, WAPISIANA, WAPISHANA)"
      data$ETNIA[data$ETNIA == "0239"] <-	"WAREKENA (UAREQUENA, WEREKENA)"
      data$ETNIA[data$ETNIA == "0240"] <-	"WASSU"
      data$ETNIA[data$ETNIA == "0241"] <-	"WAURA (UAURA, WAUJA)"
      data$ETNIA[data$ETNIA == "0242"] <-	"WAYANA (WAIANA, UAIANA)"
      data$ETNIA[data$ETNIA == "0243"] <-	"WITOTO (UITOTO, HUITOTO)"
      data$ETNIA[data$ETNIA == "0244"] <-	"XAKRIABA (XACRIABA)"
      data$ETNIA[data$ETNIA == "0245"] <-	"XAVANTE (A'UWE, AKWE, AWEN, AKWEN)"
      data$ETNIA[data$ETNIA == "0246"] <-	"XERENTE (AKWE, AWEN, AKWEN)"
      data$ETNIA[data$ETNIA == "0247"] <-	"XETA"
      data$ETNIA[data$ETNIA == "0248"] <-	"XIPAIA (SHIPAYA, XIPAYA)"
      data$ETNIA[data$ETNIA == "0249"] <-	"XOKLENG (SHOKLENG, XOCLENG)"
      data$ETNIA[data$ETNIA == "0250"] <-	"XOKO (XOCO, CHOCO)"
      data$ETNIA[data$ETNIA == "0251"] <-	"XUKURU (XUCURU)"
      data$ETNIA[data$ETNIA == "0252"] <-	"XUKURU KARIRI (XUCURU-KARIRI)"
      data$ETNIA[data$ETNIA == "0253"] <-	"YAIPIYANA"
      data$ETNIA[data$ETNIA == "0254"] <-	"YAMINAWA (JAMINAWA, IAMINAWA)"
      data$ETNIA[data$ETNIA == "0255"] <-	"YANOMAMI NINAM (IANOMAMI, IANOAMA, XIRIANA)"
      data$ETNIA[data$ETNIA == "0256"] <-	"YANOMAMI SANUMA (IANOMAMI, IANOAMA, XIRIANA)"
      data$ETNIA[data$ETNIA == "0257"] <-	"YANOMAMI YANOMAM (IANOMAMI, IANOAMA, XIRIANA)"
      data$ETNIA[data$ETNIA == "0258"] <-	"YAWALAPITI (IAUALAPITI)"
      data$ETNIA[data$ETNIA == "0259"] <-	"YAWANAWA (IAUANAUA)"
      data$ETNIA[data$ETNIA == "0260"] <-	"YEKUANA (MAIONGON, YE'KUANA, YEKWANA, MAYONGONG)"
      data$ETNIA[data$ETNIA == "0261"] <-	"YUDJA (JURUNA, YURUNA)"
      data$ETNIA[data$ETNIA == "0262"] <-	"ZO'E (POTURU)"
      data$ETNIA[data$ETNIA == "0263"] <-	"ZORO (PAGEYN)"
      data$ETNIA[data$ETNIA == "0264"] <-	"ZURUAHA (SOROWAHA, SURUWAHA)"
      data$ETNIA[data$ETNIA == "X265"] <-	"AHANENAWA"
      data$ETNIA[data$ETNIA == "X266"] <-	"AICABA"
      data$ETNIA[data$ETNIA == "X267"] <-	"AIKAN\\u00c3-KWAS\\u00c1"
      data$ETNIA[data$ETNIA == "X268"] <-	"AKUNTSU"
      data$ETNIA[data$ETNIA == "X269"] <-	"ALANTESU"
      data$ETNIA[data$ETNIA == "X271"] <-	"AMAW\\u00c1KA"
      data$ETNIA[data$ETNIA == "X272"] <-	"ANAC\\u00c9"
      data$ETNIA[data$ETNIA == "X273"] <-	"APURIN\\u00c3"
      data$ETNIA[data$ETNIA == "X274"] <-	"ARAN\\u00c3"
      data$ETNIA[data$ETNIA == "X275"] <-	"ARAPA\\u00c7O"
      data$ETNIA[data$ETNIA == "X276"] <-	"ARARA APOLIMA"
      data$ETNIA[data$ETNIA == "X277"] <-	"ARARA DO ARIPUANA"
      data$ETNIA[data$ETNIA == "X278"] <-	"ARIPUAN\\u00c1"
      data$ETNIA[data$ETNIA == "X279"] <-	"ASSURINI"
      data$ETNIA[data$ETNIA == "X280"] <-	"AWUAR\\u00c1"
      data$ETNIA[data$ETNIA == "X281"] <-	"BORBA"
      data$ETNIA[data$ETNIA == "X282"] <-	"CABIXI"
      data$ETNIA[data$ETNIA == "X283"] <-	"CAMARAR\\u00c9"
      data$ETNIA[data$ETNIA == "X284"] <-	"CAMASURI"
      data$ETNIA[data$ETNIA == "X285"] <-	"CARA PRETA"
      data$ETNIA[data$ETNIA == "X286"] <-	"CHARRUA"
      data$ETNIA[data$ETNIA == "X287"] <-	"CUJUBIM"
      data$ETNIA[data$ETNIA == "X288"] <-	"DAW"
      data$ETNIA[data$ETNIA == "X289"] <-	"GAVI\\u00c3O"
      data$ETNIA[data$ETNIA == "X290"] <-	"GUARANI"
      data$ETNIA[data$ETNIA == "X291"] <-	"HALANTESU"
      data$ETNIA[data$ETNIA == "X292"] <-	"HALOTESU"
      data$ETNIA[data$ETNIA == "X293"] <-	"HENGAT\\u00da"
      data$ETNIA[data$ETNIA == "X294"] <-	"HIXKARYANA"
      data$ETNIA[data$ETNIA == "X295"] <-	"HUPDE"
      data$ETNIA[data$ETNIA == "X296"] <-	"HUPDES"
      data$ETNIA[data$ETNIA == "X297"] <-	"IAUANAUA"
      data$ETNIA[data$ETNIA == "X298"] <-	"IAUARETE A\\u00c7U"
      data$ETNIA[data$ETNIA == "X299"] <-	"IKPENG"
      data$ETNIA[data$ETNIA == "X300"] <-	"INAMBU"
      data$ETNIA[data$ETNIA == "X301"] <-	"INHABARANA"
      data$ETNIA[data$ETNIA == "X302"] <-	"JAVAE"
      data$ETNIA[data$ETNIA == "X303"] <-	"JENIPAPO"
      data$ETNIA[data$ETNIA == "X304"] <-	"JENIPAPO-KANINDE"
      data$ETNIA[data$ETNIA == "X305"] <-	"JIAHOI"
      data$ETNIA[data$ETNIA == "X306"] <-	"KAIOWA"
      data$ETNIA[data$ETNIA == "X307"] <-	"KAMPA"
      data$ETNIA[data$ETNIA == "X308"] <-	"KANELA"
      data$ETNIA[data$ETNIA == "X309"] <-	"KARAFAWYANA"
      data$ETNIA[data$ETNIA == "X310"] <-	"KARARAO"
      data$ETNIA[data$ETNIA == "X311"] <-	"KARUBO"
      data$ETNIA[data$ETNIA == "X312"] <-	"KASSUP\\u00c1"
      data$ETNIA[data$ETNIA == "X313"] <-	"KATITH\\u00c3ULU"
      data$ETNIA[data$ETNIA == "X314"] <-	"KATOKIN"
      data$ETNIA[data$ETNIA == "X315"] <-	"KATUKINA PANO"
      data$ETNIA[data$ETNIA == "X316"] <-	"KATUKINA PEDA DJAPA"
      data$ETNIA[data$ETNIA == "X317"] <-	"KATUKINA SHANENAUWA"
      data$ETNIA[data$ETNIA == "X318"] <-	"KAXAGO"
      data$ETNIA[data$ETNIA == "X319"] <-	"KAYABI"
      data$ETNIA[data$ETNIA == "X320"] <-	"KIN\\u00c3 (WAIMIRI-ATROARI)"
      data$ETNIA[data$ETNIA == "X321"] <-	"KIRIRI-BARRA"
      data$ETNIA[data$ETNIA == "X322"] <-	"KITH\\u00c3ULU"
      data$ETNIA[data$ETNIA == "X323"] <-	"KOIAI\\u00c1"
      data$ETNIA[data$ETNIA == "X324"] <-	"KOIUPANK\\u00c1"
      data$ETNIA[data$ETNIA == "X325"] <-	"KONTANAWA"
      data$ETNIA[data$ETNIA == "X326"] <-	"KRAH\\u00d4 KANELA"
      data$ETNIA[data$ETNIA == "X327"] <-	"KULINA"
      data$ETNIA[data$ETNIA == "X328"] <-	"LATUND\\u00ca"
      data$ETNIA[data$ETNIA == "X329"] <-	"MAKU"
      data$ETNIA[data$ETNIA == "X330"] <-	"MAKUNAMB\\u00c9"
      data$ETNIA[data$ETNIA == "X331"] <-	"MAMAIND\\u00ca"
      data$ETNIA[data$ETNIA == "X332"] <-	"MAMURI"
      data$ETNIA[data$ETNIA == "X333"] <-	"MANACAPURU"
      data$ETNIA[data$ETNIA == "X334"] <-	"MANAIRISSU"
      data$ETNIA[data$ETNIA == "X335"] <-	"MANCHINERI"
      data$ETNIA[data$ETNIA == "X336"] <-	"MANDUCA"
      data$ETNIA[data$ETNIA == "X337"] <-	"MARIBONDO"
      data$ETNIA[data$ETNIA == "X338"] <-	"MASSAKA"
      data$ETNIA[data$ETNIA == "X339"] <-	"MAWAYANA"
      data$ETNIA[data$ETNIA == "X340"] <-	"MAW\\u00c9"
      data$ETNIA[data$ETNIA == "X341"] <-	"MAYORUNA"
      data$ETNIA[data$ETNIA == "X342"] <-	"MIQUELENO"
      data$ETNIA[data$ETNIA == "X343"] <-	"MOKURI\\u00d1"
      data$ETNIA[data$ETNIA == "X344"] <-	"MON ORO WARAM"
      data$ETNIA[data$ETNIA == "X345"] <-	"MUTUM"
      data$ETNIA[data$ETNIA == "X346"] <-	"MYKY"
      data$ETNIA[data$ETNIA == "X347"] <-	"NADEB"
      data$ETNIA[data$ETNIA == "X348"] <-	"NAMBIKWARA"
      data$ETNIA[data$ETNIA == "X349"] <-	"NEGAROT\\u00ca"
      data$ETNIA[data$ETNIA == "X350"] <-	"NHENGATU"
      data$ETNIA[data$ETNIA == "X351"] <-	"OFAIE XAVANTE"
      data$ETNIA[data$ETNIA == "X352"] <-	"ON\\u00c7A"
      data$ETNIA[data$ETNIA == "X353"] <-	"ORO AT"
      data$ETNIA[data$ETNIA == "X354"] <-	"ORO EO"
      data$ETNIA[data$ETNIA == "X355"] <-	"ORO JOWIN"
      data$ETNIA[data$ETNIA == "X356"] <-	"ORO MIYLIN"
      data$ETNIA[data$ETNIA == "X357"] <-	"ORO MON"
      data$ETNIA[data$ETNIA == "X358"] <-	"ORO N\\u00c1O"
      data$ETNIA[data$ETNIA == "X359"] <-	"ORO WAM"
      data$ETNIA[data$ETNIA == "X360"] <-	"ORO WARAM"
      data$ETNIA[data$ETNIA == "X361"] <-	"ORO WARAM XIJEIN"
      data$ETNIA[data$ETNIA == "X362"] <-	"PACA"
      data$ETNIA[data$ETNIA == "X363"] <-	"PANKAR\\u00c1"
      data$ETNIA[data$ETNIA == "X364"] <-	"PAPAGAIO"
      data$ETNIA[data$ETNIA == "X365"] <-	"PAYAY\\u00c1"
      data$ETNIA[data$ETNIA == "X366"] <-	"PIPIPAN"
      data$ETNIA[data$ETNIA == "X367"] <-	"PIRATA"
      data$ETNIA[data$ETNIA == "X368"] <-	"PUROBOR\\u00c1"
      data$ETNIA[data$ETNIA == "X369"] <-	"SABAN\\u00ca"
      data$ETNIA[data$ETNIA == "X370"] <-	"SANUMA"
      data$ETNIA[data$ETNIA == "X371"] <-	"SAWENTES\\u00da"
      data$ETNIA[data$ETNIA == "X372"] <-	"SILCY-TAPUYA"
      data$ETNIA[data$ETNIA == "X373"] <-	"SIUCI"
      data$ETNIA[data$ETNIA == "X374"] <-	"TABAJARA"
      data$ETNIA[data$ETNIA == "X375"] <-	"TAKUARA"
      data$ETNIA[data$ETNIA == "X376"] <-	"TATU"
      data$ETNIA[data$ETNIA == "X377"] <-	"TAWAND\\u00ca"
      data$ETNIA[data$ETNIA == "X378"] <-	"TEF\\u00c9"
      data$ETNIA[data$ETNIA == "X379"] <-	"TIMBIRA"
      data$ETNIA[data$ETNIA == "X380"] <-	"TOR\\u00c1 DO BAIXO GRANDE"
      data$ETNIA[data$ETNIA == "X381"] <-	"TSUNHUM-DJAP\\u00c1"
      data$ETNIA[data$ETNIA == "X382"] <-	"TUBAR\\u00c3O"
      data$ETNIA[data$ETNIA == "X383"] <-	"TUPAIU"
      data$ETNIA[data$ETNIA == "X384"] <-	"TUPI"
      data$ETNIA[data$ETNIA == "X385"] <-	"TUPINAMB\\u00c1 DE BELMONTE"
      data$ETNIA[data$ETNIA == "X386"] <-	"URUBU"
      data$ETNIA[data$ETNIA == "X387"] <-	"URUBU KAAPOR"
      data$ETNIA[data$ETNIA == "X388"] <-	"URUP\\u00c1"
      data$ETNIA[data$ETNIA == "X389"] <-	"WAI WAI"
      data$ETNIA[data$ETNIA == "X390"] <-	"WAIKISU"
      data$ETNIA[data$ETNIA == "X391"] <-	"WAKALITES\\u00da"
      data$ETNIA[data$ETNIA == "X392"] <-	"WASSUSU"
      data$ETNIA[data$ETNIA == "X393"] <-	"XEREU"
      data$ETNIA[data$ETNIA == "X394"] <-	"XI EIN"
      data$ETNIA[data$ETNIA == "X395"] <-	"XICRIN"
      data$ETNIA[data$ETNIA == "X396"] <-	"XIPAYA"
      data$ETNIA[data$ETNIA == "X397"] <-	"XIRIANA"
      data$ETNIA[data$ETNIA == "X398"] <-	"XIRUAI"
      data$ETNIA[data$ETNIA == "X399"] <-	"YEPAMASS\\u00c3"
      data$ETNIA[data$ETNIA == "X400"] <-	"TIRIY\\u00d3"
      data$ETNIA[data$ETNIA == "X401"] <-	"YANOMAMI"
      data$ETNIA[data$ETNIA == "X402"] <-	"ARARA"
      data$ETNIA[data$ETNIA == "X403"] <-	"SAKIRIABAR"
      data$ETNIA[data$ETNIA == "X404"] <-	"TATZ"
      data$ETNIA[data$ETNIA == "X405"] <-	"SEM INFORMACAO"
      data$ETNIA <- factor(data$ETNIA)
    }

    # SEQUENCIA
    if("SEQUENCIA" %in% variables_names){
      data$SEQUENCIA <- as.character(data$SEQUENCIA)
    }

    # REMESSA
    if("REMESSA" %in% variables_names){
      data$REMESSA <- as.character(data$REMESSA)
    }

    # AUD_JUST
    if("AUD_JUST" %in% variables_names){
      data$AUD_JUST <- as.character(data$AUD_JUST)
    }

    # SIS_JUST
    if("SIS_JUST" %in% variables_names){
      data$SIS_JUST <- as.character(data$SIS_JUST)
    }

    # VAL_SH_FED
    if("VAL_SH_FED" %in% variables_names){
      data$VAL_SH_FED <- as.numeric(data$VAL_SH_FED)
    }

    # VAL_SP_FED
    if("VAL_SP_FED" %in% variables_names){
      data$VAL_SP_FED <- as.numeric(data$VAL_SP_FED)
    }

    # VAL_SH_GES
    if("VAL_SH_GES" %in% variables_names){
      data$VAL_SH_GES <- as.numeric(data$VAL_SH_GES)
    }

    # VAL_SP_GES
    if("VAL_SP_GES" %in% variables_names){
      data$VAL_SP_GES <- as.numeric(data$VAL_SP_GES)
    }

    # VAL_UCI
    if("VAL_UCI" %in% variables_names){
      data$VAL_UCI <- as.numeric(data$VAL_UCI)
    }

    # MARCA_UCI
    if("MARCA_UCI" %in% variables_names){
      data$MARCA_UCI <- as.numeric(levels(data$MARCA_UCI))[data$MARCA_UCI]
      data$MARCA_UCI[data$MARCA_UCI==0] <- "N\u00e3o utilizou UCI"
      data$MARCA_UCI[data$MARCA_UCI==1] <- "Unidade de cuidados intermed neonatal convencional"
      data$MARCA_UCI[data$MARCA_UCI==2] <- "Unidade de cuidados intermed neonatal canguru"
      data$MARCA_UCI[data$MARCA_UCI==3] <- "Unidade intermedi\u00e1ria neonatal"
      data$MARCA_UCI[data$MARCA_UCI==88] <- "Utilizou dois tipos de leitos UCI"
      data$MARCA_UCI <- factor(data$MARCA_UCI)
    }

    # DIAGSEC1
    if("DIAGSEC1" %in% variables_names){
      data$DIAGSEC1 <- as.character(data$DIAGSEC1)
    }

    # DIAGSEC2
    if("DIAGSEC2" %in% variables_names){
      data$DIAGSEC2 <- as.character(data$DIAGSEC2)
    }

    # DIAGSEC3
    if("DIAGSEC3" %in% variables_names){
      data$DIAGSEC3 <- as.character(data$DIAGSEC3)
    }

    # DIAGSEC4
    if("DIAGSEC4" %in% variables_names){
      data$DIAGSEC4 <- as.character(data$DIAGSEC4)
    }

    # DIAGSEC5
    if("DIAGSEC5" %in% variables_names){
      data$DIAGSEC5 <- as.character(data$DIAGSEC5)
    }

    # DIAGSEC6
    if("DIAGSEC6" %in% variables_names){
      data$DIAGSEC6 <- as.character(data$DIAGSEC6)
    }

    # DIAGSEC7
    if("DIAGSEC7" %in% variables_names){
      data$DIAGSEC7 <- as.character(data$DIAGSEC7)
    }

    # DIAGSEC8
    if("DIAGSEC8" %in% variables_names){
      data$DIAGSEC8 <- as.character(data$DIAGSEC8)
    }

    # DIAGSEC9
    if("DIAGSEC9" %in% variables_names){
      data$DIAGSEC9 <- as.character(data$DIAGSEC9)
    }

    # TPDISEC1
    if("TPDISEC1" %in% variables_names){
      data$TPDISEC1 <- as.numeric(levels(data$TPDISEC1))[data$TPDISEC1]
      data$TPDISEC1[data$TPDISEC1==0] <- NA
      data$TPDISEC1[data$TPDISEC1==1] <- "Pr\u00e9-existente"
      data$TPDISEC1[data$TPDISEC1==2] <- "Adquirido"
      data$TPDISEC1 <- factor(data$TPDISEC1)
    }

    # TPDISEC2
    if("TPDISEC2" %in% variables_names){
      data$TPDISEC2 <- as.numeric(levels(data$TPDISEC2))[data$TPDISEC2]
      data$TPDISEC2[data$TPDISEC2==0] <- NA
      data$TPDISEC2[data$TPDISEC2==1] <- "Pr\u00e9-existente"
      data$TPDISEC2[data$TPDISEC2==2] <- "Adquirido"
      data$TPDISEC2 <- factor(data$TPDISEC2)
    }

    # TPDISEC3
    if("TPDISEC3" %in% variables_names){
      data$TPDISEC3 <- as.numeric(levels(data$TPDISEC3))[data$TPDISEC3]
      data$TPDISEC3[data$TPDISEC3==0] <- NA
      data$TPDISEC3[data$TPDISEC3==1] <- "Pr\u00e9-existente"
      data$TPDISEC3[data$TPDISEC3==2] <- "Adquirido"
      data$TPDISEC3 <- factor(data$TPDISEC3)
    }

    # TPDISEC4
    if("TPDISEC4" %in% variables_names){
      data$TPDISEC4 <- as.numeric(levels(data$TPDISEC4))[data$TPDISEC4]
      data$TPDISEC4[data$TPDISEC4==0] <- NA
      data$TPDISEC4[data$TPDISEC4==1] <- "Pr\u00e9-existente"
      data$TPDISEC4[data$TPDISEC4==2] <- "Adquirido"
      data$TPDISEC4 <- factor(data$TPDISEC4)
    }

    # TPDISEC5
    if("TPDISEC5" %in% variables_names){
      data$TPDISEC5 <- as.numeric(levels(data$TPDISEC5))[data$TPDISEC5]
      data$TPDISEC5[data$TPDISEC5==0] <- NA
      data$TPDISEC5[data$TPDISEC5==1] <- "Pr\u00e9-existente"
      data$TPDISEC5[data$TPDISEC5==2] <- "Adquirido"
      data$TPDISEC5 <- factor(data$TPDISEC5)
    }

    # TPDISEC6
    if("TPDISEC6" %in% variables_names){
      data$TPDISEC6 <- as.numeric(levels(data$TPDISEC6))[data$TPDISEC6]
      data$TPDISEC6[data$TPDISEC6==0] <- NA
      data$TPDISEC6[data$TPDISEC6==1] <- "Pr\u00e9-existente"
      data$TPDISEC6[data$TPDISEC6==2] <- "Adquirido"
      data$TPDISEC6 <- factor(data$TPDISEC6)
    }

    # TPDISEC7
    if("TPDISEC7" %in% variables_names){
      data$TPDISEC7 <- as.numeric(levels(data$TPDISEC7))[data$TPDISEC7]
      data$TPDISEC7[data$TPDISEC7==0] <- NA
      data$TPDISEC7[data$TPDISEC7==1] <- "Pr\u00e9-existente"
      data$TPDISEC7[data$TPDISEC7==2] <- "Adquirido"
      data$TPDISEC7 <- factor(data$TPDISEC7)
    }

    # TPDISEC8
    if("TPDISEC8" %in% variables_names){
      data$TPDISEC8 <- as.numeric(levels(data$TPDISEC8))[data$TPDISEC8]
      data$TPDISEC8[data$TPDISEC8==0] <- NA
      data$TPDISEC8[data$TPDISEC8==1] <- "Pr\u00e9-existente"
      data$TPDISEC8[data$TPDISEC8==2] <- "Adquirido"
      data$TPDISEC8 <- factor(data$TPDISEC8)
    }

    # TPDISEC9
    if("TPDISEC9" %in% variables_names){
      data$TPDISEC9 <- as.numeric(levels(data$TPDISEC9))[data$TPDISEC9]
      data$TPDISEC9[data$TPDISEC9==0] <- NA
      data$TPDISEC9[data$TPDISEC9==1] <- "Pr\u00e9-existente"
      data$TPDISEC9[data$TPDISEC9==2] <- "Adquirido"
      data$TPDISEC9 <- factor(data$TPDISEC9)
    }
  }

  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  data <- as.data.frame(lapply(X = data, FUN = stringi::stri_unescape_unicode))

  # Return
  return(data)

}
