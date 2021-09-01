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
      data$ANO_CMPT <- as.integer(data$ANO_CMPT)
    }

    # MES_CMPT
    if("MES_CMPT" %in% variables_names){
      data$MES_CMPT <- as.integer(data$MES_CMPT)
    }

    # ESPEC
    if("ESPEC" %in% variables_names){
      data$ESPEC <- as.numeric(levels(data$ESPEC))[data$ESPEC]
      data$ESPEC[data$ESPEC==1] <- "Cirurgia"
      data$ESPEC[data$ESPEC==2] <- "Obstetrícia"
      data$ESPEC[data$ESPEC==3] <- "Clínica médica"
      data$ESPEC[data$ESPEC==4] <- "Crônicos"
      data$ESPEC[data$ESPEC==5] <- "Psiquiatria"
      data$ESPEC[data$ESPEC==6] <- "Pneumologia sanitária"
      data$ESPEC[data$ESPEC==7] <- "Pediatria"
      data$ESPEC[data$ESPEC==8] <- "Reabilitação"
      data$ESPEC[data$ESPEC==9] <- "Hospital dia (cirúrgicos)"
      data$ESPEC[data$ESPEC==10] <- "Hospital dia (AIDS)"
      data$ESPEC[data$ESPEC==11] <- "Hospital dia (fibrose cística)"
      data$ESPEC[data$ESPEC==12] <- "Hospital dia (intercorrência pós transplantes)"
      data$ESPEC[data$ESPEC==13] <- "Hospital dia (geriatria)"
      data$ESPEC[data$ESPEC==14] <- "Hospital dia (saúde mental)"
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
      data$IDENT[data$IDENT==3] <- "Continuação"
      data$IDENT[data$IDENT==5] <- "Longa permanência"
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
      data$MARCA_UTI[data$MARCA_UTI==0] <- "Não utilizou UTI"
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
      data$COBRANCA[data$COBRANCA==15] <- "Alta com previsão de retorno p/acomp do paciente"
      data$COBRANCA[data$COBRANCA==16] <- "Alta por evasão"
      data$COBRANCA[data$COBRANCA==18] <- "Alta por outros motivos"
      data$COBRANCA[data$COBRANCA==19] <- "Alta de paciente agudo em psiquiatria"
      data$COBRANCA[data$COBRANCA==21] <- "Permanência por características próprias da doença"
      data$COBRANCA[data$COBRANCA==22] <- "Permanência por intercorrência"
      data$COBRANCA[data$COBRANCA==23] <- "Permanência por impossibilidade sócio-familiar"
      data$COBRANCA[data$COBRANCA==24] <- "Permanência proc doação órg, tec, cél-doador vivo"
      data$COBRANCA[data$COBRANCA==25] <- "Permanência proc doação órg, tec, cél-doador morto"
      data$COBRANCA[data$COBRANCA==26] <- "Permanência por mudança de procedimento"
      data$COBRANCA[data$COBRANCA==27] <- "Permanência por reoperação"
      data$COBRANCA[data$COBRANCA==28] <- "Permanência por outros motivos"
      data$COBRANCA[data$COBRANCA==29] <- "Transferência para internação domiciliar"
      data$COBRANCA[data$COBRANCA==32] <- "Transferência para internação domiciliar"
      data$COBRANCA[data$COBRANCA==31] <- "Transferência para outro estabelecimento"
      data$COBRANCA[data$COBRANCA==41] <- "Óbito com DO fornecida pelo médico assistente"
      data$COBRANCA[data$COBRANCA==42] <- "Óbito com DO fornecida pelo IML"
      data$COBRANCA[data$COBRANCA==43] <- "Óbito com DO fornecida pelo SVO"
      data$COBRANCA[data$COBRANCA==51] <- "Encerramento administrativo"
      data$COBRANCA[data$COBRANCA==61] <- "Alta da mãe/puérpera e do recém-nascido"
      data$COBRANCA[data$COBRANCA==17] <- "Alta da mãe/puérpera e do recém-nascido"
      data$COBRANCA[data$COBRANCA==62] <- "Alta da mãe/puérpera e permanência recém-nascido"
      data$COBRANCA[data$COBRANCA==13] <- "Alta da mãe/puérpera e permanência recém-nascido"
      data$COBRANCA[data$COBRANCA==63] <- "Alta da mãe/puérpera e óbito do recém-nascido"
      data$COBRANCA[data$COBRANCA==64] <- "Alta da mãe/puérpera com óbito fetal"
      data$COBRANCA[data$COBRANCA==65] <- "Óbito da gestante e do concepto"
      data$COBRANCA[data$COBRANCA==66] <- "Óbito da mãe/puérpera e alta do recém-nascido"
      data$COBRANCA[data$COBRANCA==67] <- "Óbito da mãe/puérpera e permanência recém-nascido"
      data$COBRANCA <- factor(data$COBRANCA)
    }

    # NATUREZA
    if("NATUREZA" %in% variables_names){
      data$NATUREZA <- as.numeric(levels(data$NATUREZA))[data$NATUREZA]
      data$NATUREZA[data$NATUREZA==0] <- NA
      data$NATUREZA[data$NATUREZA==99] <- NA
      data$NATUREZA[data$NATUREZA==10] <- "Próprio"
      data$NATUREZA[data$NATUREZA==20] <- "Contratado"
      data$NATUREZA[data$NATUREZA==22] <- "Contratado optante SIMPLES"
      data$NATUREZA[data$NATUREZA==30] <- "Federal"
      data$NATUREZA[data$NATUREZA==31] <- "Federal Verba Própria"
      data$NATUREZA[data$NATUREZA==40] <- "Estadual"
      data$NATUREZA[data$NATUREZA==41] <- "Estadual Verba Própria"
      data$NATUREZA[data$NATUREZA==50] <- "Municipal"
      data$NATUREZA[data$NATUREZA==60] <- "Filantrópico"
      data$NATUREZA[data$NATUREZA==61] <- "Filantrópico isento tributos e contr.sociais"
      data$NATUREZA[data$NATUREZA==63] <- "Filantrópico isento IR e contr.s/lucro líquido"
      data$NATUREZA[data$NATUREZA==70] <- "Universitário Ensino"
      data$NATUREZA[data$NATUREZA==80] <- "Sindicato"
      data$NATUREZA[data$NATUREZA==90] <- "Universitário Pesquisas"
      data$NATUREZA[data$NATUREZA==91] <- "Univ. Pesquisas isento tributos e contr.sociais"
      data$NATUREZA[data$NATUREZA==93] <- "Univ. Pesquisas isento IR e contr.s/lucro líquido"
      data$NATUREZA[data$NATUREZA==94] <- "Universitário de ensino e pesquisa privado"
      data$NATUREZA[data$NATUREZA==92] <- "Universitário de ensino e pesquisa privado"
      data$NATUREZA <- factor(data$NATUREZA)
    }

    # NAT_JUR
    if("NAT_JUR" %in% variables_names){
      data$NAT_JUR <- as.numeric(levels(data$NAT_JUR))[data$NAT_JUR]
      data$NAT_JUR[data$NAT_JUR==1015] <- "Órgão Público do Poder Executivo Federal"
      data$NAT_JUR[data$NAT_JUR==1023] <- "Órgão Público do Poder Exec Estadual ou Distr Fed"
      data$NAT_JUR[data$NAT_JUR==1031] <- "Órgão Público do Poder Executivo Municipal"
      data$NAT_JUR[data$NAT_JUR==1040] <- "Órgão Público do Poder Legislativo Federal"
      data$NAT_JUR[data$NAT_JUR==1058] <- "Órgão Público do Poder Legisl Estadual ou Dist Fed"
      data$NAT_JUR[data$NAT_JUR==1066] <- "Órgão Público do Poder Legislativo Municipal"
      data$NAT_JUR[data$NAT_JUR==1074] <- "Órgão Público do Poder Judiciário Federal"
      data$NAT_JUR[data$NAT_JUR==1082] <- "Órgão Público do Poder Judiciário Estadual"
      data$NAT_JUR[data$NAT_JUR==1104] <- "Autarquia Federal"
      data$NAT_JUR[data$NAT_JUR==1112] <- "Autarquia Estadual ou do Distrito Federal"
      data$NAT_JUR[data$NAT_JUR==1120] <- "Autarquia Municipal"
      data$NAT_JUR[data$NAT_JUR==1139] <- "Fundação Federal"
      data$NAT_JUR[data$NAT_JUR==1147] <- "Fundação Estadual ou do Distrito Federal"
      data$NAT_JUR[data$NAT_JUR==1155] <- "Fundação Municipal"
      data$NAT_JUR[data$NAT_JUR==1163] <- "Órgão Público Autônomo Federal"
      data$NAT_JUR[data$NAT_JUR==1171] <- "Órgão Público Autônomo Estadual ou Distr Federal"
      data$NAT_JUR[data$NAT_JUR==1180] <- "Órgão Público Autônomo Estadual ou Distr Federal"
      data$NAT_JUR[data$NAT_JUR==1198] <- "Comissão Polinacional"
      data$NAT_JUR[data$NAT_JUR==1201] <- "Fundo Público"
      data$NAT_JUR[data$NAT_JUR==1210] <- "Associação Pública"
      data$NAT_JUR[data$NAT_JUR==2011] <- "Empresa Pública"
      data$NAT_JUR[data$NAT_JUR==2038] <- "Sociedade de Economia Mista"
      data$NAT_JUR[data$NAT_JUR==2046] <- "Sociedade Anônima Aberta"
      data$NAT_JUR[data$NAT_JUR==2054] <- "Sociedade Anônima Fechada"
      data$NAT_JUR[data$NAT_JUR==2062] <- "Sociedade Empresária Limitada"
      data$NAT_JUR[data$NAT_JUR==2070] <- "Sociedade Empresária em Nome Coletivo"
      data$NAT_JUR[data$NAT_JUR==2089] <- "Sociedade Empresária em Comandita Simples"
      data$NAT_JUR[data$NAT_JUR==2097] <- "Sociedade Empresária em Comandita por Ações"
      data$NAT_JUR[data$NAT_JUR==2127] <- "Sociedade em Conta de Participação"
      data$NAT_JUR[data$NAT_JUR==2135] <- "Empresário (Individual)"
      data$NAT_JUR[data$NAT_JUR==2143] <- "Cooperativa"
      data$NAT_JUR[data$NAT_JUR==2151] <- "Consórcio de Sociedades"
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
      data$NAT_JUR[data$NAT_JUR==2283] <- "Consórcio de Empregadores"
      data$NAT_JUR[data$NAT_JUR==2291] <- "Consórcio Simples"
      data$NAT_JUR[data$NAT_JUR==2305] <- "Empr Individ Responsab Limitada (Natur Empresária)"
      data$NAT_JUR[data$NAT_JUR==2313] <- "Empr Individ Responsab Limitada (Natureza Simples)"
      data$NAT_JUR[data$NAT_JUR==3034] <- "Serviço Notarial e Registral (Cartório)"
      data$NAT_JUR[data$NAT_JUR==3069] <- "Fundação Privada"
      data$NAT_JUR[data$NAT_JUR==3077] <- "Serviço Social Autônomo"
      data$NAT_JUR[data$NAT_JUR==3085] <- "Condomínio Edilício"
      data$NAT_JUR[data$NAT_JUR==3107] <- "Comissão de Conciliação Prévia"
      data$NAT_JUR[data$NAT_JUR==3115] <- "Entidade de Mediação e Arbitragem"
      data$NAT_JUR[data$NAT_JUR==3123] <- "Partido Político"
      data$NAT_JUR[data$NAT_JUR==3131] <- "Entidade Sindical"
      data$NAT_JUR[data$NAT_JUR==3204] <- "Estab no Brasil de Fundação ou Associação Estrang"
      data$NAT_JUR[data$NAT_JUR==3212] <- "Fundação ou Associação Domiciliada no Exterior"
      data$NAT_JUR[data$NAT_JUR==3220] <- "Organização Religiosa"
      data$NAT_JUR[data$NAT_JUR==3239] <- "Comunidade Indígena"
      data$NAT_JUR[data$NAT_JUR==3247] <- "Fundo Privado"
      data$NAT_JUR[data$NAT_JUR==3999] <- "Associação Privada"
      data$NAT_JUR[data$NAT_JUR==4014] <- "Empresa Individual Imobiliária"
      data$NAT_JUR[data$NAT_JUR==4022] <- "Segurado Especial"
      data$NAT_JUR[data$NAT_JUR==4081] <- "Contribuinte Individual"
      data$NAT_JUR[data$NAT_JUR==4090] <- "Candidato a Cargo Político Eletivo"
      data$NAT_JUR[data$NAT_JUR==4111] <- "Leiloeiro"
      data$NAT_JUR[data$NAT_JUR==5010] <- "Organização Internacional"
      data$NAT_JUR[data$NAT_JUR==5029] <- "Representação Diplomática Estrangeira"
      data$NAT_JUR[data$NAT_JUR==5037] <- "Outras Instituições Extraterritoriais"
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
      data$IND_VDRL[data$IND_VDRL==0] <- "Não"
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
      data$MORTE[data$MORTE==0] <- "Não"
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
      data$CAR_INT[data$CAR_INT==2] <- "Urgência"
      data$CAR_INT[data$CAR_INT==3] <- "Acidente no local trabalho ou a serv da empresa"
      data$CAR_INT[data$CAR_INT==4] <- "Acidente no trajeto para o trabalho"
      data$CAR_INT[data$CAR_INT==5] <- "Outros tipo de acidente de trânsito"
      data$CAR_INT[data$CAR_INT==6] <- "Out tp lesões e envenen por agent quím físicos"
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
      data$HOMONIMO[data$HOMONIMO==0] <- "Não"
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
      data$INSTRU[data$INSTRU==2] <- "1º grau"
      data$INSTRU[data$INSTRU==3] <- "2º grau"
      data$INSTRU[data$INSTRU==4] <- "3º grau"
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
      data$CONTRACEP1[data$CONTRACEP1==5] <- "Cinto térmico"
      data$CONTRACEP1[data$CONTRACEP1==6] <- "DIU"
      data$CONTRACEP1[data$CONTRACEP1==7] <- "Diafragma"
      data$CONTRACEP1[data$CONTRACEP1==8] <- "Preservativo"
      data$CONTRACEP1[data$CONTRACEP1==9] <- "Espermicida"
      data$CONTRACEP1[data$CONTRACEP1==10] <- "Hormônio oral"
      data$CONTRACEP1[data$CONTRACEP1==11] <- "Hormônio injetável"
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
      data$CONTRACEP2[data$CONTRACEP2==5] <- "Cinto térmico"
      data$CONTRACEP2[data$CONTRACEP2==6] <- "DIU"
      data$CONTRACEP2[data$CONTRACEP2==7] <- "Diafragma"
      data$CONTRACEP2[data$CONTRACEP2==8] <- "Preservativo"
      data$CONTRACEP2[data$CONTRACEP2==9] <- "Espermicida"
      data$CONTRACEP2[data$CONTRACEP2==10] <- "Hormônio oral"
      data$CONTRACEP2[data$CONTRACEP2==11] <- "Hormônio injetável"
      data$CONTRACEP2[data$CONTRACEP2==12] <- "Coito interrompido"
      data$CONTRACEP2[data$CONTRACEP2==0] <- NA
      data$CONTRACEP2[data$CONTRACEP2==99] <- NA
      data$CONTRACEP2 <- factor(data$CONTRACEP2)
    }

    # GESTRISCO
    if("GESTRISCO" %in% variables_names){
      data$GESTRISCO <- as.numeric(levels(data$GESTRISCO))[data$GESTRISCO]
      data$GESTRISCO[data$GESTRISCO==0] <- "Não"
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
    if("CBOR" %in% variables_names){
      data$CBOR <- as.character(data$CBOR)
    }

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
      data$VINCPREV[data$VINCPREV==1] <- "Autônomo"
      data$VINCPREV[data$VINCPREV==2] <- "Desempregado"
      data$VINCPREV[data$VINCPREV==3] <- "Aposentado"
      data$VINCPREV[data$VINCPREV==4] <- "Não segurado"
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
      data$INFEHOSP[data$INFEHOSP==0] <- "Não"
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
      data$COMPLEX[data$COMPLEX==1] <- "Atenção Básica"
      data$COMPLEX[data$COMPLEX==2] <- "Média complexidade"
      data$COMPLEX[data$COMPLEX==3] <- "Alta complexidade"
      data$COMPLEX[data$COMPLEX==0] <- NA
      data$COMPLEX[data$COMPLEX==99] <- NA
      data$COMPLEX <- factor(data$COMPLEX)
    }

    # FINANC
    if("FINANC" %in% variables_names){
      data$FINANC <- as.numeric(levels(data$FINANC))[data$FINANC]
      data$FINANC[data$FINANC==1] <- "Atenção Básica (PAB)"
      data$FINANC[data$FINANC==2] <- "Assistência Farmacêutica"
      data$FINANC[data$FINANC==4] <- "Fundo de Ações Estratégicas e Compensações FAEC"
      data$FINANC[data$FINANC==5] <- "Incentivo - MAC"
      data$FINANC[data$FINANC==6] <- "Média e Alta Complexidade (MAC)"
      data$FINANC[data$FINANC==7] <- "Vigilância em Saúde"
      data$FINANC[data$FINANC==0] <- NA
      data$FINANC[data$FINANC==99] <- NA
      data$FINANC <- factor(data$FINANC)
    }

    # FAEC_TP
    if("FAEC_TP" %in% variables_names){
      data$FAEC_TP <- as.numeric(levels(data$FAEC_TP))[data$FAEC_TP]
      data$FAEC_TP[data$FAEC_TP==10000] <- "Atenção Básica (PAB)"
      data$FAEC_TP[data$FAEC_TP==20000] <- "Assistência Farmacêutica"
      data$FAEC_TP[data$FAEC_TP==40001] <- "Coleta de material"
      data$FAEC_TP[data$FAEC_TP==40002] <- "Diagnóstico em laboratório clínico"
      data$FAEC_TP[data$FAEC_TP==40003] <- "Coleta/exame anátomo-patológico colo uterino"
      data$FAEC_TP[data$FAEC_TP==40004] <- "Diagnóstico em neurologia"
      data$FAEC_TP[data$FAEC_TP==40005] <- "Diagnóstico em otorrinolaringologia/fonoaudiologia"
      data$FAEC_TP[data$FAEC_TP==40006] <- "Diagnóstico em psicologia/psiquiatria"
      data$FAEC_TP[data$FAEC_TP==40007] <- "Consultas médicas/outros profissionais de nível superior"
      data$FAEC_TP[data$FAEC_TP==40008] <- "Atenção domiciliar"
      data$FAEC_TP[data$FAEC_TP==40009] <- "Atendimento/acompanhamento em reabilitação física, mental, visual, auditiva e múltiplas defic"
      data$FAEC_TP[data$FAEC_TP==40010] <- "Atendimento/acompanhamento psicossocial"
      data$FAEC_TP[data$FAEC_TP==40011] <- "Atendimento/acompanhamento em saúde do idoso"
      data$FAEC_TP[data$FAEC_TP==40012] <- "Atendimento/acompanhamento de queimados"
      data$FAEC_TP[data$FAEC_TP==40013] <- "Atendimento/acompanhamento de diagnóstico de doenças endocrinas/metabólicas e nutricionais"
      data$FAEC_TP[data$FAEC_TP==40014] <- "Tratamento de doenças do sistema nervoso central e periférico"
      data$FAEC_TP[data$FAEC_TP==40015] <- "Tratamento de doenças do aparelho da visão"
      data$FAEC_TP[data$FAEC_TP==40016] <- "Tratamento em oncologia"
      data$FAEC_TP[data$FAEC_TP==40017] <- "Nefrologia"
      data$FAEC_TP[data$FAEC_TP==40018] <- "Tratamentos odontológicos"
      data$FAEC_TP[data$FAEC_TP==40019] <- "Cirurgia do sistema nervoso central e periférico"
      data$FAEC_TP[data$FAEC_TP==40020] <- "Cirurgias de ouvido, nariz e garganta"
      data$FAEC_TP[data$FAEC_TP==40021] <- "Deformidade labio-palatal e crânio-facial"
      data$FAEC_TP[data$FAEC_TP==40022] <- "Cirurgia do aparelho da visão"
      data$FAEC_TP[data$FAEC_TP==40023] <- "Cirurgia do aparelho circulatório"
      data$FAEC_TP[data$FAEC_TP==40024] <- "Cirurgia do aparelho digestivo, orgãos anexos e parede abdominal(inclui pré e pós operatório)"
      data$FAEC_TP[data$FAEC_TP==40025] <- "Cirurgia do aparelho geniturinário"
      data$FAEC_TP[data$FAEC_TP==40026] <- "Tratamento de queimados"
      data$FAEC_TP[data$FAEC_TP==40027] <- "Cirurgia reparadora para lipodistrofia"
      data$FAEC_TP[data$FAEC_TP==40028] <- "Outras cirurgias plásticas/reparadoras"
      data$FAEC_TP[data$FAEC_TP==40029] <- "Cirurgia orofacial"
      data$FAEC_TP[data$FAEC_TP==40030] <- "Sequenciais"
      data$FAEC_TP[data$FAEC_TP==40031] <- "Cirurgias em nefrologia"
      data$FAEC_TP[data$FAEC_TP==40032] <- "Transplantes de orgãos, tecidos e células"
      data$FAEC_TP[data$FAEC_TP==40033] <- "Medicamentos para transplante"
      data$FAEC_TP[data$FAEC_TP==40034] <- "OPM auditivas"
      data$FAEC_TP[data$FAEC_TP==40035] <- "OPM em odontologia"
      data$FAEC_TP[data$FAEC_TP==40036] <- "OPM em queimados"
      data$FAEC_TP[data$FAEC_TP==40037] <- "OPM em nefrologia"
      data$FAEC_TP[data$FAEC_TP==40038] <- "OPM para transplantes"
      data$FAEC_TP[data$FAEC_TP==40039] <- "Incentivos ao pré-natal e nascimento"
      data$FAEC_TP[data$FAEC_TP==40040] <- "Incentivo ao registro cívil de nascimento"
      data$FAEC_TP[data$FAEC_TP==40041] <- "Central Nacional de Regulação de Alta Complexidade (CNRAC)"
      data$FAEC_TP[data$FAEC_TP==40042] <- "Reguladores de Atividade hormonal - Inibidores de prolactina"
      data$FAEC_TP[data$FAEC_TP==40043] <- "Política Nacional de Cirurgias Eletivas"
      data$FAEC_TP[data$FAEC_TP==40044] <- "Redesignação e Acompanhamento"
      data$FAEC_TP[data$FAEC_TP==40045] <- "Projeto Olhar Brasil"
      data$FAEC_TP[data$FAEC_TP==40046] <- "Mamografia para Rastreamento"
      data$FAEC_TP[data$FAEC_TP==40047] <- "Projeto Olhar Brasil - Consulta"
      data$FAEC_TP[data$FAEC_TP==40048] <- "Projeto Olhar Brasil - Óculos"
      data$FAEC_TP[data$FAEC_TP==40049] <- "Implementar Cirg. CV Pediátrica"
      data$FAEC_TP[data$FAEC_TP==40050] <- "Cirurgias Eletivas - Componente I"
      data$FAEC_TP[data$FAEC_TP==40051] <- "Cirurgias Eletivas - Componente II"
      data$FAEC_TP[data$FAEC_TP==40052] <- "Cirurgias Eletivas - Componente III"
      data$FAEC_TP[data$FAEC_TP==40053] <- "Prótese Mamária - Exames"
      data$FAEC_TP[data$FAEC_TP==40054] <- "Prótese Mamária - Cirurgia"
      data$FAEC_TP[data$FAEC_TP==40055] <- "Transplante - Histocompatibilidade"
      data$FAEC_TP[data$FAEC_TP==40056] <- "Triagem Neonatal"
      data$FAEC_TP[data$FAEC_TP==40057] <- "Controle de qualidade do exame citopatológico do colo de útero"
      data$FAEC_TP[data$FAEC_TP==40058] <- "Exames do Leite Materno"
      data$FAEC_TP[data$FAEC_TP==40059] <- "Atenção as Pessoas em Situação de Violência Sexual"
      data$FAEC_TP[data$FAEC_TP==40060] <- "Sangue e Hemoderivados"
      data$FAEC_TP[data$FAEC_TP==40061] <- "Mamografia para rastreamento em faixa etária recomendada"
      data$FAEC_TP[data$FAEC_TP==40062] <- "Doenças Raras"
      data$FAEC_TP[data$FAEC_TP==40063] <- "Cadeiras de Rodas"
      data$FAEC_TP[data$FAEC_TP==40064] <- "Sistema de Frequencia Modulada Pessoal-FM"
      data$FAEC_TP[data$FAEC_TP==40065] <- "Medicamentos em Urgência"
      data$FAEC_TP[data$FAEC_TP==40066] <- "Cirurgias Eletivas - Componente Único"
      data$FAEC_TP[data$FAEC_TP==40067] <- "Atenção Especializada em Saúde Auditiva"
      data$FAEC_TP[data$FAEC_TP==40068] <- "Terapias Especializadas em Angiologia"
      data$FAEC_TP[data$FAEC_TP==21012] <- "FAEC CNRAC (21012-cód ant à tab unif-vál p/2008-01)"
      data$FAEC_TP[data$FAEC_TP==21014] <- "FAEC Eletiv(21014-cód ant à tab unif-vál p/2008-01)"
      data$FAEC_TP[data$FAEC_TP==50000] <- "Incentivo - MAC"
      data$FAEC_TP[data$FAEC_TP==60000] <- "Média e Alta Complexidade (MAC)"
      data$FAEC_TP[data$FAEC_TP==70000] <- "Vigilância em Saúde"
      data$FAEC_TP[data$FAEC_TP==80000] <- "Gestão do SUS"
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
      data$REGCT[data$REGCT==7106] <- "ESTABELECIMENTO SEM GERAÇÃO DE CREDITO TOTAL - EXCLUINDO FAEC"
      data$REGCT[data$REGCT==7107] <- "ESTABELECIMENTO SEM GERACAO DE CREDITO NAS ACOES ESPEC. DE ODONTOLOGIA(INCENTIVO CEO I,II E III)"
      data$REGCT[data$REGCT==7108] <- "ESTABELECIMENTO SEM GERACAO DE CREDITO(INCENTIVO A SAUDE DO TRABALHADOR)"
      data$REGCT[data$REGCT==7109] <- "ESTABELECIMENTO SEM GERACAO DE CREDITO TOTAL-MEC"
      data$REGCT[data$REGCT==7110] <- "ESTABELECIMENTO DE SAUDE DA ESTRUTURA DO MINISTERIO DA SAUDE - SEM GERAÇAO DE CREDITO TOTAL"
      data$REGCT[data$REGCT==7111] <- "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO - NASF, EXCETO FAEC"
      data$REGCT[data$REGCT==7112] <- "ESTABELECIMENTO SEM GERAÇÃO DE CREDITO TOTAL - INCLUINDO FAEC  - EXCLUSIVO PARA REDE SARAH"
      data$REGCT[data$REGCT==7113] <- "ESTABELECIMENTO SEM GERAÇÃO DE CREDITO TOTAL - INCLUINDO FAEC - OUTROS ESTABELECIMENTOS FEDERAIS"
      data$REGCT[data$REGCT==7114] <- "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CRÉDITO TOTAL, INCLUSIVE FAEC - PRONTO ATENDIMENTO"
      data$REGCT[data$REGCT==7115] <- "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CRÉDITO NA MÉDIA COMPLEXIDADE - HU/MEC"
      data$REGCT[data$REGCT==7116] <- "ESTABELECIMENTO DE SAÚDE SEM GERAÇÃO DE CRÉDITO NA MÉDIA COMPLEXIDADE - LRPD"
      data$REGCT[data$REGCT==7117] <- "Estabelecimento de Saúde sem geração de crédito na média complexidade (exceto OPM) - CER"
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
      data$RACA_COR[data$RACA_COR==5] <- "Indígena"
      data$RACA_COR[data$RACA_COR==0] <- NA
      data$RACA_COR[data$RACA_COR==99] <- NA
      data$RACA_COR <- factor(data$RACA_COR)
    }

    # ETNIA
    if("ETNIA" %in% variables_names){
      data$ETNIA <- as.numeric(levels(data$ETNIA))[data$ETNIA]
      data$ETNIA[data$ETNIA==1] <- "ACONA (WAKONAS, NACONAS, JAKONA, ACORANES)"
      data$ETNIA[data$ETNIA==2] <- "AIKANA (AIKANA, MAS SAKA,TUBARAO)"
      data$ETNIA[data$ETNIA==3] <- "AJURU"
      data$ETNIA[data$ETNIA==4] <- "AKUNSU (AKUNT'SU)"
      data$ETNIA[data$ETNIA==5] <- "AMANAYE"
      data$ETNIA[data$ETNIA==6] <- "AMONDAWA"
      data$ETNIA[data$ETNIA==7] <- "ANAMBE"
      data$ETNIA[data$ETNIA==8] <- "APARAI (APALAI)"
      data$ETNIA[data$ETNIA==9] <- "APIAKA (APIACA)"
      data$ETNIA[data$ETNIA==10] <- "APINAYE (APINAJE/APINAIE/APINAGE)"
      data$ETNIA[data$ETNIA==11] <- "APURINA (APORINA, IPURINA, IPURINA, IPURINAN)"
      data$ETNIA[data$ETNIA==12] <- "ARANA (ARACUAI DO VALE DO JEQUITINHONHA)"
      data$ETNIA[data$ETNIA==13] <- "ARAPASO (ARAPACO)"
      data$ETNIA[data$ETNIA==14] <- "ARARA DE RONDONIA (KARO, URUCU, URUKU)"
      data$ETNIA[data$ETNIA==15] <- "ARARA DO ACRE (SHAWANAUA, AMAWAKA)"
      data$ETNIA[data$ETNIA==16] <- "ARARA DO ARIPUANA (ARARA DO BEIRADAO/ARIPUANA)"
      data$ETNIA[data$ETNIA==17] <- "ARARA DO PARA (UKARAGMA, UKARAMMA)"
      data$ETNIA[data$ETNIA==18] <- "ARAWETE (ARAUETE)"
      data$ETNIA[data$ETNIA==19] <- "ARIKAPU (ARICAPU, ARIKAPO, MASUBI, MAXUBI)"
      data$ETNIA[data$ETNIA==20] <- "ARIKEM (ARIQUEN, ARIQUEME, ARIKEME)"
      data$ETNIA[data$ETNIA==21] <- "ARIKOSE (ARICOBE)"
      data$ETNIA[data$ETNIA==22] <- "ARUA"
      data$ETNIA[data$ETNIA==23] <- "ARUAK (ARAWAK)"
      data$ETNIA[data$ETNIA==24] <- "ASHANINKA (KAMPA)"
      data$ETNIA[data$ETNIA==25] <- "ASURINI DO TOCANTINS (AKUAWA/AKWAWA)"
      data$ETNIA[data$ETNIA==26] <- "ASURINI DO XINGU (AWAETE)"
      data$ETNIA[data$ETNIA==27] <- "ATIKUM (ATICUM)"
      data$ETNIA[data$ETNIA==28] <- "AVA - CANOEIRO"
      data$ETNIA[data$ETNIA==29] <- "AWETI (AUETI/AUETO)"
      data$ETNIA[data$ETNIA==30] <- "BAKAIRI (KURA, BACAIRI)"
      data$ETNIA[data$ETNIA==31] <- "BANAWA YAFI (BANAWA, BANAWA-JAFI)"
      data$ETNIA[data$ETNIA==32] <- "BANIWA (BANIUA, BANIVA, WALIMANAI, WAKUENAI)"
      data$ETNIA[data$ETNIA==33] <- "BARA (WAIPINOMAKA)"
      data$ETNIA[data$ETNIA==34] <- "BARASANA (HANERA)"
      data$ETNIA[data$ETNIA==35] <- "BARE"
      data$ETNIA[data$ETNIA==36] <- "BORORO (BOE)"
      data$ETNIA[data$ETNIA==37] <- "BOTOCUDO (GEREN)"
      data$ETNIA[data$ETNIA==38] <- "CANOE"
      data$ETNIA[data$ETNIA==39] <- "CASSUPA"
      data$ETNIA[data$ETNIA==40] <- "CHAMACOCO"
      data$ETNIA[data$ETNIA==41] <- "CHIQUITANO (XIQUITANO)"
      data$ETNIA[data$ETNIA==42] <- "CIKIYANA (SIKIANA)"
      data$ETNIA[data$ETNIA==43] <- "CINTA LARGA (MATETAMAE)"
      data$ETNIA[data$ETNIA==44] <- "COLUMBIARA (CORUMBIARA)"
      data$ETNIA[data$ETNIA==45] <- "DENI"
      data$ETNIA[data$ETNIA==46] <- "DESANA (DESANA, DESANO, DESSANO, WIRA, UMUKOMASA)"
      data$ETNIA[data$ETNIA==47] <- "DIAHUI (JAHOI, JAHUI, DIARROI)"
      data$ETNIA[data$ETNIA==48] <- "ENAWENE-NAWE (SALUMA)"
      data$ETNIA[data$ETNIA==49] <- "FULNI-O"
      data$ETNIA[data$ETNIA==50] <- "GALIBI (GALIBI DO OIAPOQUE, KARINHA)"
      data$ETNIA[data$ETNIA==51] <- "GALIBI MARWORNO (GALIBI DO UACA, ARUA)"
      data$ETNIA[data$ETNIA==52] <- "GAVIAO DE RONDONIA (DIGUT)"
      data$ETNIA[data$ETNIA==53] <- "GAVIAO KRIKATEJE"
      data$ETNIA[data$ETNIA==54] <- "GAVIAO PARKATEJE (PARKATEJE)"
      data$ETNIA[data$ETNIA==55] <- "GAVIAO PUKOBIE (PUKOBIE, PYKOPJE, GAVIAO DO MARANH"
      data$ETNIA[data$ETNIA==56] <- "GUAJA (AWA, AVA)"
      data$ETNIA[data$ETNIA==57] <- "GUAJAJARA (TENETEHARA)"
      data$ETNIA[data$ETNIA==58] <- "GUARANI KAIOWA (PAI TAVYTERA)"
      data$ETNIA[data$ETNIA==59] <- "GUARANI M'BYA"
      data$ETNIA[data$ETNIA==60] <- "GUARANI NANDEVA (AVAKATUETE, CHIRIPA, NHANDEWA, AV"
      data$ETNIA[data$ETNIA==61] <- "GUATO"
      data$ETNIA[data$ETNIA==62] <- "HIMARIMA (HIMERIMA)"
      data$ETNIA[data$ETNIA==63] <- "INGARIKO (INGARICO, AKAWAIO, KAPON)"
      data$ETNIA[data$ETNIA==64] <- "IRANXE (IRANTXE)"
      data$ETNIA[data$ETNIA==65] <- "ISSE"
      data$ETNIA[data$ETNIA==66] <- "JABOTI (JABUTI, KIPIU, YABYTI)"
      data$ETNIA[data$ETNIA==67] <- "JAMAMADI (YAMAMADI, DJEOROMITXI)"
      data$ETNIA[data$ETNIA==68] <- "JARAWARA"
      data$ETNIA[data$ETNIA==69] <- "JIRIPANCO (JERIPANCO, GERIPANCO)"
      data$ETNIA[data$ETNIA==70] <- "JUMA (YUMA)"
      data$ETNIA[data$ETNIA==71] <- "JURUNA"
      data$ETNIA[data$ETNIA==72] <- "JURUTI (YURITI)"
      data$ETNIA[data$ETNIA==73] <- "KAAPOR (URUBU-KAAPOR, KA'APOR, KAAPORTE)"
      data$ETNIA[data$ETNIA==74] <- "KADIWEU (CADUVEO, CADIUEU)"
      data$ETNIA[data$ETNIA==75] <- "KAIABI (CAIABI, KAYABI)"
      data$ETNIA[data$ETNIA==76] <- "KAIMBE (CAIMBE)"
      data$ETNIA[data$ETNIA==77] <- "KAINGANG (CAINGANGUE)"
      data$ETNIA[data$ETNIA==78] <- "KAIXANA (CAIXANA)"
      data$ETNIA[data$ETNIA==79] <- "KALABASSA (CALABASSA, CALABACAS)"
      data$ETNIA[data$ETNIA==80] <- "KALANCO"
      data$ETNIA[data$ETNIA==81] <- "KALAPALO (CALAPALO)"
      data$ETNIA[data$ETNIA==82] <- "KAMAYURA (CAMAIURA, KAMAIURA)"
      data$ETNIA[data$ETNIA==83] <- "KAMBA (CAMBA)"
      data$ETNIA[data$ETNIA==84] <- "KAMBEBA (CAMBEBA, OMAGUA)"
      data$ETNIA[data$ETNIA==85] <- "KAMBIWA (CAMBIUA)"
      data$ETNIA[data$ETNIA==86] <- "KAMBIWA PIPIPA (PIPIPA)"
      data$ETNIA[data$ETNIA==87] <- "KAMPE"
      data$ETNIA[data$ETNIA==88] <- "KANAMANTI (KANAMATI, CANAMANTI)"
      data$ETNIA[data$ETNIA==89] <- "KANAMARI (CANAMARI, KANAMARY, TUKUNA)"
      data$ETNIA[data$ETNIA==90] <- "KANELA APANIEKRA (CANELA)"
      data$ETNIA[data$ETNIA==91] <- "KANELA RANKOKAMEKRA (CANELA)"
      data$ETNIA[data$ETNIA==92] <- "KANINDE"
      data$ETNIA[data$ETNIA==93] <- "KANOE (CANOE)"
      data$ETNIA[data$ETNIA==94] <- "KANTARURE (CANTARURE)"
      data$ETNIA[data$ETNIA==95] <- "KAPINAWA (CAPINAUA)"
      data$ETNIA[data$ETNIA==96] <- "KARAJA (CARAJA)"
      data$ETNIA[data$ETNIA==97] <- "KARAJA/JAVAE (JAVAE)"
      data$ETNIA[data$ETNIA==98] <- "KARAJA/XAMBIOA (KARAJA DO NORTE)"
      data$ETNIA[data$ETNIA==99] <- "KARAPANA (CARAPANA, MUTEAMASA, UKOPINOPONA)"
      data$ETNIA[data$ETNIA==100] <- "KARAPOTO (CARAPOTO)"
      data$ETNIA[data$ETNIA==101] <- "KARIPUNA (CARIPUNA)"
      data$ETNIA[data$ETNIA==102] <- "KARIPUNA DO AMAPA (CARIPUNA)"
      data$ETNIA[data$ETNIA==103] <- "KARIRI (CARIRI)"
      data$ETNIA[data$ETNIA==104] <- "KARIRI-XOCO (CARIRI-CHOCO)"
      data$ETNIA[data$ETNIA==105] <- "KARITIANA (CARITIANA)"
      data$ETNIA[data$ETNIA==106] <- "KATAWIXI (KATAUIXI,KATAWIN, KATAWISI, CATAUICHI)"
      data$ETNIA[data$ETNIA==107] <- "KATUENA (CATUENA, KATWENA)"
      data$ETNIA[data$ETNIA==108] <- "KATUKINA (PEDA DJAPA)"
      data$ETNIA[data$ETNIA==109] <- "KATUKINA DO ACRE"
      data$ETNIA[data$ETNIA==110] <- "KAXARARI (CAXARARI)"
      data$ETNIA[data$ETNIA==111] <- "KAXINAWA (HUNI-KUIN, CASHINAUA, CAXINAUA)"
      data$ETNIA[data$ETNIA==112] <- "KAXIXO"
      data$ETNIA[data$ETNIA==113] <- "KAXUYANA (CAXUIANA)"
      data$ETNIA[data$ETNIA==114] <- "KAYAPO (CAIAPO)"
      data$ETNIA[data$ETNIA==115] <- "KAYAPO KARARAO (KARARAO)"
      data$ETNIA[data$ETNIA==116] <- "KAYAPO TXUKAHAMAE (TXUKAHAMAE)"
      data$ETNIA[data$ETNIA==117] <- "KAYAPO XICRIM (XIKRIN)"
      data$ETNIA[data$ETNIA==118] <- "KAYUISANA (CAIXANA, CAUIXANA, KAIXANA)"
      data$ETNIA[data$ETNIA==119] <- "KINIKINAWA (GUAN, KOINUKOEN, KINIKINAO)"
      data$ETNIA[data$ETNIA==120] <- "KIRIRI"
      data$ETNIA[data$ETNIA==121] <- "KOCAMA (COCAMA, KOKAMA)"
      data$ETNIA[data$ETNIA==122] <- "KOKUIREGATEJE"
      data$ETNIA[data$ETNIA==123] <- "KORUBO"
      data$ETNIA[data$ETNIA==124] <- "KRAHO (CRAO, KRAO)"
      data$ETNIA[data$ETNIA==125] <- "KREJE (KRENYE)"
      data$ETNIA[data$ETNIA==126] <- "KRENAK (BORUN, CRENAQUE)"
      data$ETNIA[data$ETNIA==127] <- "KRIKATI (KRINKATI)"
      data$ETNIA[data$ETNIA==128] <- "KUBEO (CUBEO, COBEWA, KUBEWA, PAMIWA, CUBEU)"
      data$ETNIA[data$ETNIA==129] <- "KUIKURO (KUIKURU, CUICURO)"
      data$ETNIA[data$ETNIA==130] <- "KUJUBIM (KUYUBI, CUJUBIM)"
      data$ETNIA[data$ETNIA==131] <- "KULINA PANO (CULINA)"
      data$ETNIA[data$ETNIA==132] <- "KULINA/MADIHA (CULINA, MADIJA, MADIHA)"
      data$ETNIA[data$ETNIA==133] <- "KURIPAKO (CURIPACO, CURRIPACO, CORIPACO, WAKUENAI)"
      data$ETNIA[data$ETNIA==134] <- "KURUAIA (CURUAIA)"
      data$ETNIA[data$ETNIA==135] <- "KWAZA (COAIA, KOAIA)"
      data$ETNIA[data$ETNIA==136] <- "MACHINERI (MANCHINERI, MANXINERI)"
      data$ETNIA[data$ETNIA==137] <- "MACURAP (MAKURAP)"
      data$ETNIA[data$ETNIA==138] <- "MAKU DOW (DOW)"
      data$ETNIA[data$ETNIA==139] <- "MAKU HUPDA (HUPDA)"
      data$ETNIA[data$ETNIA==140] <- "MAKU NADEB (NADEB)"
      data$ETNIA[data$ETNIA==141] <- "MAKU YUHUPDE (YUHUPDE)"
      data$ETNIA[data$ETNIA==142] <- "MAKUNA (MACUNA, YEBA-MASA)"
      data$ETNIA[data$ETNIA==143] <- "MAKUXI (MACUXI, MACHUSI, PEMON)"
      data$ETNIA[data$ETNIA==144] <- "MARIMAM (MARIMA)"
      data$ETNIA[data$ETNIA==145] <- "MARUBO"
      data$ETNIA[data$ETNIA==146] <- "MATIPU"
      data$ETNIA[data$ETNIA==147] <- "MATIS"
      data$ETNIA[data$ETNIA==148] <- "MATSE (MAYORUNA)"
      data$ETNIA[data$ETNIA==149] <- "MAXAKALI (MAXACALI)"
      data$ETNIA[data$ETNIA==150] <- "MAYA (MAYA)"
      data$ETNIA[data$ETNIA==151] <- "MAYTAPU"
      data$ETNIA[data$ETNIA==152] <- "MEHINAKO (MEINAKU, MEINACU)"
      data$ETNIA[data$ETNIA==153] <- "MEKEN (MEQUEM, MEKHEM, MICHENS)"
      data$ETNIA[data$ETNIA==154] <- "MENKY (MYKY, MUNKU, MENKI, MYNKY)"
      data$ETNIA[data$ETNIA==155] <- "MIRANHA (MIRANHA, MIRANA)"
      data$ETNIA[data$ETNIA==156] <- "MIRITI TAPUIA (MIRITI-TAPUYA, BUIA-TAPUYA)"
      data$ETNIA[data$ETNIA==157] <- "MUNDURUKU (MUNDURUCU)"
      data$ETNIA[data$ETNIA==158] <- "MURA"
      data$ETNIA[data$ETNIA==159] <- "NAHUKWA (NAFUQUA)"
      data$ETNIA[data$ETNIA==160] <- "NAMBIKWARA DO CAMPO (HALOTESU, KITHAULU, WAKALITES"
      data$ETNIA[data$ETNIA==161] <- "NAMBIKWARA DO NORTE (NEGAROTE ,MAMAINDE, LATUNDE,"
      data$ETNIA[data$ETNIA==162] <- "NAMBIKWARA DO SUL (WASUSU ,HAHAINTESU, ALANTESU, W"
      data$ETNIA[data$ETNIA==163] <- "NARAVUTE (NARUVOTO)"
      data$ETNIA[data$ETNIA==164] <- "NAWA (NAUA)"
      data$ETNIA[data$ETNIA==165] <- "NUKINI (NUQUINI, NUKUINI)"
      data$ETNIA[data$ETNIA==166] <- "OFAIE (OFAYE-XAVANTE)"
      data$ETNIA[data$ETNIA==167] <- "ORO WIN"
      data$ETNIA[data$ETNIA==168] <- "PAIAKU (JENIPAPO-KANINDE)"
      data$ETNIA[data$ETNIA==169] <- "PAKAA NOVA (WARI, PACAAS NOVOS)"
      data$ETNIA[data$ETNIA==170] <- "PALIKUR (AUKWAYENE, AUKUYENE, PALIKU'ENE)"
      data$ETNIA[data$ETNIA==171] <- "PANARA (KRENHAKARORE , KRENAKORE, KRENAKARORE)"
      data$ETNIA[data$ETNIA==172] <- "PANKARARE (PANCARARE)"
      data$ETNIA[data$ETNIA==173] <- "PANKARARU (PANCARARU)"
      data$ETNIA[data$ETNIA==174] <- "PANKARARU KALANKO (KALANKO)"
      data$ETNIA[data$ETNIA==175] <- "PANKARARU KARUAZU (KARUAZU)"
      data$ETNIA[data$ETNIA==176] <- "PANKARU (PANCARU)"
      data$ETNIA[data$ETNIA==177] <- "PARAKANA (PARACANA, APITEREWA, AWAETE)"
      data$ETNIA[data$ETNIA==178] <- "PARECI (PARESI, HALITI)"
      data$ETNIA[data$ETNIA==179] <- "PARINTINTIN"
      data$ETNIA[data$ETNIA==180] <- "PATAMONA (KAPON)"
      data$ETNIA[data$ETNIA==181] <- "PATAXO"
      data$ETNIA[data$ETNIA==182] <- "PATAXO HA-HA-HAE"
      data$ETNIA[data$ETNIA==183] <- "PAUMARI (PALMARI)"
      data$ETNIA[data$ETNIA==184] <- "PAUMELENHO"
      data$ETNIA[data$ETNIA==185] <- "PIRAHA (MURA PIRAHA)"
      data$ETNIA[data$ETNIA==186] <- "PIRATUAPUIA (PIRATAPUYA, PIRATAPUYO, PIRATAPUYA,"
      data$ETNIA[data$ETNIA==187] <- "PITAGUARI"
      data$ETNIA[data$ETNIA==188] <- "POTIGUARA"
      data$ETNIA[data$ETNIA==189] <- "POYANAWA (POIANAUA)"
      data$ETNIA[data$ETNIA==190] <- "RIKBAKTSA (CANOEIROS, ERIGPAKTSA)"
      data$ETNIA[data$ETNIA==191] <- "SAKURABIAT (MEKENS, SAKIRABIAP, SAKIRABIAR)"
      data$ETNIA[data$ETNIA==192] <- "SATERE-MAWE (SATERE-MAUE)"
      data$ETNIA[data$ETNIA==193] <- "SHANENAWA (KATUKINA)"
      data$ETNIA[data$ETNIA==194] <- "SIRIANO (SIRIA-MASA)"
      data$ETNIA[data$ETNIA==195] <- "SURIANA"
      data$ETNIA[data$ETNIA==196] <- "SURUI DE RONDONIA (PAITER)"
      data$ETNIA[data$ETNIA==197] <- "SURUI DO PARA (AIKEWARA)"
      data$ETNIA[data$ETNIA==198] <- "SUYA (SUIA/KISEDJE)"
      data$ETNIA[data$ETNIA==199] <- "TAPAYUNA (BEICO-DE-PAU)"
      data$ETNIA[data$ETNIA==200] <- "TAPEBA"
      data$ETNIA[data$ETNIA==201] <- "TAPIRAPE (TAPI'IRAPE)"
      data$ETNIA[data$ETNIA==202] <- "TAPUIA (TAPUIA-XAVANTE, TAPUIO)"
      data$ETNIA[data$ETNIA==203] <- "TARIANO (TARIANA, TALIASERI)"
      data$ETNIA[data$ETNIA==204] <- "TAUREPANG (TAULIPANG, PEMON, AREKUNA, PAGEYN)"
      data$ETNIA[data$ETNIA==205] <- "TEMBE"
      data$ETNIA[data$ETNIA==206] <- "TENHARIM"
      data$ETNIA[data$ETNIA==207] <- "TERENA"
      data$ETNIA[data$ETNIA==208] <- "TICUNA (TIKUNA, TUKUNA, MAGUTA)"
      data$ETNIA[data$ETNIA==209] <- "TINGUI BOTO"
      data$ETNIA[data$ETNIA==210] <- "TIRIYO EWARHUYANA (TIRIYO, TRIO, TARONA, YAWI, PIA"
      data$ETNIA[data$ETNIA==211] <- "TIRIYO KAH'YANA (TIRIYO, TRIO, TARONA, YAWI, PIANO"
      data$ETNIA[data$ETNIA==212] <- "TIRIYO TSIKUYANA (TIRIYO, TRIO, TARONA, YAWI, PIAN"
      data$ETNIA[data$ETNIA==213] <- "TORA"
      data$ETNIA[data$ETNIA==214] <- "TREMEMBE"
      data$ETNIA[data$ETNIA==215] <- "TRUKA"
      data$ETNIA[data$ETNIA==216] <- "TRUMAI"
      data$ETNIA[data$ETNIA==217] <- "TSOHOM DJAPA (TSUNHUM-DJAPA)"
      data$ETNIA[data$ETNIA==218] <- "TUKANO (TUCANO, YE'PA-MASA, DASEA)"
      data$ETNIA[data$ETNIA==219] <- "TUMBALALA"
      data$ETNIA[data$ETNIA==220] <- "TUNAYANA"
      data$ETNIA[data$ETNIA==221] <- "TUPARI"
      data$ETNIA[data$ETNIA==222] <- "TUPINAMBA"
      data$ETNIA[data$ETNIA==223] <- "TUPINIQUIM"
      data$ETNIA[data$ETNIA==224] <- "TURIWARA"
      data$ETNIA[data$ETNIA==225] <- "TUXA"
      data$ETNIA[data$ETNIA==226] <- "TUYUKA (TUIUCA, DOKAPUARA, UTAPINOMAKAPHONA)"
      data$ETNIA[data$ETNIA==227] <- "TXIKAO (TXICAO, IKPENG)"
      data$ETNIA[data$ETNIA==228] <- "UMUTINA (OMOTINA, BARBADOS)"
      data$ETNIA[data$ETNIA==229] <- "URU-EU-WAU-WAU (URUEU-UAU-UAU, URUPAIN, URUPA)"
      data$ETNIA[data$ETNIA==230] <- "WAI WAI HIXKARYANA (HIXKARYANA)"
      data$ETNIA[data$ETNIA==231] <- "WAI WAI KARAFAWYANA (KARAFAWYANA, KARAPAWYANA)"
      data$ETNIA[data$ETNIA==232] <- "WAI WAI XEREU (XEREU)"
      data$ETNIA[data$ETNIA==233] <- "WAI WAI KATUENA (KATUENA)"
      data$ETNIA[data$ETNIA==234] <- "WAI WAI MAWAYANA (MAWAYANA)"
      data$ETNIA[data$ETNIA==235] <- "WAIAPI (WAYAMPI, OYAMPI, WAYAPY, )"
      data$ETNIA[data$ETNIA==236] <- "WAIMIRI ATROARI (KINA)"
      data$ETNIA[data$ETNIA==237] <- "WANANO (UANANO, WANANA)"
      data$ETNIA[data$ETNIA==238] <- "WAPIXANA (UAPIXANA, VAPIDIANA, WAPISIANA, WAPISHAN"
      data$ETNIA[data$ETNIA==239] <- "WAREKENA (UAREQUENA, WEREKENA)"
      data$ETNIA[data$ETNIA==240] <- "WASSU"
      data$ETNIA[data$ETNIA==241] <- "WAURA (UAURA, WAUJA)"
      data$ETNIA[data$ETNIA==242] <- "WAYANA (WAIANA, UAIANA)"
      data$ETNIA[data$ETNIA==243] <- "WITOTO (UITOTO, HUITOTO)"
      data$ETNIA[data$ETNIA==244] <- "XAKRIABA (XACRIABA)"
      data$ETNIA[data$ETNIA==245] <- "XAVANTE (A'UWE, AKWE, AWEN, AKWEN)"
      data$ETNIA[data$ETNIA==246] <- "XERENTE (AKWE, AWEN, AKWEN)"
      data$ETNIA[data$ETNIA==247] <- "XETA"
      data$ETNIA[data$ETNIA==248] <- "XIPAIA (SHIPAYA, XIPAYA)"
      data$ETNIA[data$ETNIA==249] <- "XOKLENG (SHOKLENG, XOCLENG)"
      data$ETNIA[data$ETNIA==250] <- "XOKO (XOCO, CHOCO)"
      data$ETNIA[data$ETNIA==251] <- "XUKURU (XUCURU)"
      data$ETNIA[data$ETNIA==252] <- "XUKURU KARIRI (XUCURU-KARIRI)"
      data$ETNIA[data$ETNIA==253] <- "YAIPIYANA"
      data$ETNIA[data$ETNIA==254] <- "YAMINAWA (JAMINAWA, IAMINAWA)"
      data$ETNIA[data$ETNIA==255] <- "YANOMAMI NINAM (IANOMAMI, IANOAMA, XIRIANA)"
      data$ETNIA[data$ETNIA==256] <- "YANOMAMI SANUMA (IANOMAMI, IANOAMA, XIRIANA)"
      data$ETNIA[data$ETNIA==257] <- "YANOMAMI YANOMAM (IANOMAMI, IANOAMA, XIRIANA)"
      data$ETNIA[data$ETNIA==258] <- "YAWALAPITI (IAUALAPITI)"
      data$ETNIA[data$ETNIA==259] <- "YAWANAWA (IAUANAUA)"
      data$ETNIA[data$ETNIA==260] <- "YEKUANA (MAIONGON, YE'KUANA, YEKWANA, MAYONGONG)"
      data$ETNIA[data$ETNIA==261] <- "YUDJA (JURUNA, YURUNA)"
      data$ETNIA[data$ETNIA==262] <- "ZO'E (POTURU)"
      data$ETNIA[data$ETNIA==263] <- "ZORO (PAGEYN)"
      data$ETNIA[data$ETNIA==264] <- "ZURUAHA (SOROWAHA, SURUWAHA)"
      data$ETNIA[data$ETNIA==0] <- NA
      data$ETNIA[data$ETNIA==9999] <- NA
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
      data$MARCA_UCI[data$MARCA_UCI==0] <- "Não utilizou UCI"
      data$MARCA_UCI[data$MARCA_UCI==1] <- "Unidade de cuidados intermed neonatal convencional"
      data$MARCA_UCI[data$MARCA_UCI==2] <- "Unidade de cuidados intermed neonatal canguru"
      data$MARCA_UCI[data$MARCA_UCI==3] <- "Unidade intermediária neonatal"
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
      data$TPDISEC1[data$TPDISEC1==1] <- "Pré-existente"
      data$TPDISEC1[data$TPDISEC1==2] <- "Adquirido"
      data$TPDISEC1 <- factor(data$TPDISEC1)
    }

    # TPDISEC2
    if("TPDISEC2" %in% variables_names){
      data$TPDISEC2 <- as.numeric(levels(data$TPDISEC2))[data$TPDISEC2]
      data$TPDISEC2[data$TPDISEC2==0] <- NA
      data$TPDISEC2[data$TPDISEC2==1] <- "Pré-existente"
      data$TPDISEC2[data$TPDISEC2==2] <- "Adquirido"
      data$TPDISEC2 <- factor(data$TPDISEC2)
    }

    # TPDISEC3
    if("TPDISEC3" %in% variables_names){
      data$TPDISEC3 <- as.numeric(levels(data$TPDISEC3))[data$TPDISEC3]
      data$TPDISEC3[data$TPDISEC3==0] <- NA
      data$TPDISEC3[data$TPDISEC3==1] <- "Pré-existente"
      data$TPDISEC3[data$TPDISEC3==2] <- "Adquirido"
      data$TPDISEC3 <- factor(data$TPDISEC3)
    }

    # TPDISEC4
    if("TPDISEC4" %in% variables_names){
      data$TPDISEC4 <- as.numeric(levels(data$TPDISEC4))[data$TPDISEC4]
      data$TPDISEC4[data$TPDISEC4==0] <- NA
      data$TPDISEC4[data$TPDISEC4==1] <- "Pré-existente"
      data$TPDISEC4[data$TPDISEC4==2] <- "Adquirido"
      data$TPDISEC4 <- factor(data$TPDISEC4)
    }

    # TPDISEC5
    if("TPDISEC5" %in% variables_names){
      data$TPDISEC5 <- as.numeric(levels(data$TPDISEC5))[data$TPDISEC5]
      data$TPDISEC5[data$TPDISEC5==0] <- NA
      data$TPDISEC5[data$TPDISEC5==1] <- "Pré-existente"
      data$TPDISEC5[data$TPDISEC5==2] <- "Adquirido"
      data$TPDISEC5 <- factor(data$TPDISEC5)
    }

    # TPDISEC6
    if("TPDISEC6" %in% variables_names){
      data$TPDISEC6 <- as.numeric(levels(data$TPDISEC6))[data$TPDISEC6]
      data$TPDISEC6[data$TPDISEC6==0] <- NA
      data$TPDISEC6[data$TPDISEC6==1] <- "Pré-existente"
      data$TPDISEC6[data$TPDISEC6==2] <- "Adquirido"
      data$TPDISEC6 <- factor(data$TPDISEC6)
    }

    # TPDISEC7
    if("TPDISEC7" %in% variables_names){
      data$TPDISEC7 <- as.numeric(levels(data$TPDISEC7))[data$TPDISEC7]
      data$TPDISEC7[data$TPDISEC7==0] <- NA
      data$TPDISEC7[data$TPDISEC7==1] <- "Pré-existente"
      data$TPDISEC7[data$TPDISEC7==2] <- "Adquirido"
      data$TPDISEC7 <- factor(data$TPDISEC7)
    }

    # TPDISEC8
    if("TPDISEC8" %in% variables_names){
      data$TPDISEC8 <- as.numeric(levels(data$TPDISEC8))[data$TPDISEC8]
      data$TPDISEC8[data$TPDISEC8==0] <- NA
      data$TPDISEC8[data$TPDISEC8==1] <- "Pré-existente"
      data$TPDISEC8[data$TPDISEC8==2] <- "Adquirido"
      data$TPDISEC8 <- factor(data$TPDISEC8)
    }

    # TPDISEC9
    if("TPDISEC9" %in% variables_names){
      data$TPDISEC9 <- as.numeric(levels(data$TPDISEC9))[data$TPDISEC9]
      data$TPDISEC9[data$TPDISEC9==0] <- NA
      data$TPDISEC9[data$TPDISEC9==1] <- "Pré-existente"
      data$TPDISEC9[data$TPDISEC9==2] <- "Adquirido"
      data$TPDISEC9 <- factor(data$TPDISEC9)
    }
  }

  # Purge levels
  data <- droplevels(data)

  # Return
  return(data)

}
