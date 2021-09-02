#' Process CNES-PF variables from DataSUS
#'
#' \code{process_cnes_pf} processes CNES-PF variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes CNES-PF (Pessoa Física) variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples \dontrun{
#' df <- fetch_datasus(year_start = 2015, month_start = 1,
#'                     year_end = 2015, month_end = 1,
#'                     uf = "RJ",
#'                     information_system = "CNES-PF")
#' df_a <- process_cnes_pf(df)
#' df_b <- process_cnes_pf(df, municipality_data = FALSE)
#' }

process_cnes_pf <- function(data, municipality_data = TRUE) {
  # Variables names
  variables_names <- names(data)

  # CNES
  if("CNES" %in% variables_names){
    #data$CNES <- as.integer(as.character(levels(data$CNES))[data$CNES])
    data$CNES <- as.character(data$CNES)
  }

  # UFMUNRES
  if ("UFMUNRES" %in% variables_names & municipality_data == TRUE) {
    data$UFMUNRES <- as.character(data$UFMUNRES)
    colnames(tabMun)[1] <- "UFMUNRES"
    data <- dplyr::left_join(data, microdatasus::tabMun, by = c("UFMUNRES" = "munResUf"))
  } else {
    data$UFMUNRES <- as.character(data$UFMUNRES)
  }

  # REGSAUDE
  if("REGSAUDE" %in% variables_names){
    data$REGSAUDE <- as.character(data$REGSAUDE)
  }

  # MICR_REG
  if("MICR_REG" %in% variables_names){
    data$MICR_REG <- as.integer(data$MICR_REG)
  }

  # DISTRSAN
  if("DISTRSAN" %in% variables_names){
    data$DISTRSAN <- as.integer(data$DISTRSAN)
  }

  # DISTRADM
  if("DISTRADM" %in% variables_names){
    data$DISTRADM <- as.integer(data$DISTRADM)
  }

  # TPGESTAO
  if("TPGESTAO" %in% variables_names){
    data$TPGESTAO <- as.character(levels(data$TPGESTAO))[data$TPGESTAO]
    data$TPGESTAO[data$TPGESTAO=="D"] <- "Dupla"
    data$TPGESTAO[data$TPGESTAO=="E"] <- "Estadual"
    data$TPGESTAO[data$TPGESTAO=="M"] <- "Municipal"
    data$TPGESTAO[data$TPGESTAO=="Z"] <- "Sem gest\u00e3o"
    data$TPGESTAO[data$TPGESTAO=="S"] <- "Sem gest\u00e3o"
    data$TPGESTAO <- factor(data$TPGESTAO)
  }

  # PF_PJ
  if("PF_PJ" %in% variables_names){
    data$PF_PJ <- as.numeric(levels(data$PF_PJ))[data$PF_PJ]
    data$PF_PJ[data$PF_PJ==1] <- "Pessoa f\u00edsica"
    data$PF_PJ[data$PF_PJ==3] <- "Pessoa jur\u00eddica"
    data$PF_PJ <- factor(data$PF_PJ)
  }

  # CPF_CNPJ
  if("CPF_CNPJ" %in% variables_names){
    data$CPF_CNPJ <- as.integer(data$CPF_CNPJ)
  }

  # NIV_DEP
  if("NIV_DEP" %in% variables_names){
    data$NIV_DEP <- as.numeric(levels(data$NIV_DEP))[data$NIV_DEP]
    data$NIV_DEP[data$NIV_DEP==1] <- "Individual"
    data$NIV_DEP[data$NIV_DEP==3] <- "Mantida"
    data$NIV_DEP <- factor(data$NIV_DEP)
  }

  # CNPJ_MAN
  if("CNPJ_MAN" %in% variables_names){
    data$CNPJ_MAN <- as.integer(data$CNPJ_MAN)
  }

  # ESFERA_A
  if("ESFERA_A" %in% variables_names){
    data$ESFERA_A <- as.numeric(levels(data$ESFERA_A))[data$ESFERA_A]
    data$ESFERA_A[data$ESFERA_A==1] <- "Federal"
    data$ESFERA_A[data$ESFERA_A==2] <- "Estadual"
    data$ESFERA_A[data$ESFERA_A==3] <- "Municipal"
    data$ESFERA_A[data$ESFERA_A==4] <- "Privada"
    data$ESFERA_A[data$ESFERA_A==-99] <- NA
    data$ESFERA_A <- factor(data$ESFERA_A)
  }

  # ATIVIDAD
  if("ATIVIDAD" %in% variables_names){
    data$ATIVIDAD <- as.numeric(levels(data$ATIVIDAD))[data$ATIVIDAD]
    data$ATIVIDAD[data$ATIVIDAD==-99] <- NA
    data$ATIVIDAD[data$ATIVIDAD==1] <- "Unidade Universit\u00e1ria"
    data$ATIVIDAD[data$ATIVIDAD==2] <- "Unidade Escola Superior Isolada"
    data$ATIVIDAD[data$ATIVIDAD==3] <- "Unidade Auxiliar de Ensino"
    data$ATIVIDAD[data$ATIVIDAD==4] <- "Unidade sem atividade de Ensino"
    data$ATIVIDAD[data$ATIVIDAD==5] <- "Hospital de ensino"
    data$ATIVIDAD <- factor(data$ATIVIDAD)
  }

  # RETENCAO
  if("RETENCAO" %in% variables_names){
    data$RETENCAO <- as.numeric(levels(data$RETENCAO))[data$RETENCAO]
    data$RETENCAO[data$RETENCAO==0] <- NA
    data$RETENCAO[data$RETENCAO==10] <- "Estabelecimento p\u00fablico"
    data$RETENCAO[data$RETENCAO==11] <- "Estabelecimento filantr\u00f3pico"
    data$RETENCAO[data$RETENCAO==12] <- "Estabelecimento sem fins lucrativos"
    data$RETENCAO[data$RETENCAO==13] <- "Estabelecimento privado luvrativa simples"
    data$RETENCAO[data$RETENCAO==14] <- "Estabelecimento privado luvrativa"
    data$RETENCAO[data$RETENCAO==15] <- "Estabelecimento sindical"
    data$RETENCAO[data$RETENCAO==16] <- "Estabelecimento pessoa f\u00edsica"
    data$RETENCAO <- factor(data$RETENCAO)
  }

  # NATUREZA
  if("NATUREZA" %in% variables_names){
    data$NATUREZA <- as.numeric(levels(data$NATUREZA))[data$NATUREZA]
    data$NATUREZA[data$NATUREZA==-99] <- NA
    data$NATUREZA[data$NATUREZA==1] <- "Administra\u00e7\u00e3o Direta da Sa\u00fade (MS, SES, e SMS)"
    data$NATUREZA[data$NATUREZA==2] <- "Adm Direta outros org\u00e3os (MEX, MEx, Marinha,...)"
    data$NATUREZA[data$NATUREZA==3] <- "Adm Indireta - Autarquias"
    data$NATUREZA[data$NATUREZA==4] <- "Adm Indireta - Funda\u00e7\u00e3o P\u00fablica"
    data$NATUREZA[data$NATUREZA==5] <- "Adm Indireta - Empresa P\u00fablica"
    data$NATUREZA[data$NATUREZA==6] <- "Adm Indireta - Organiza\u00e7\u00e3o Social P\u00fablica"
    data$NATUREZA[data$NATUREZA==7] <- "Empresa Privada"
    data$NATUREZA[data$NATUREZA==8] <- "Funda\u00e7\u00e3o Privada"
    data$NATUREZA[data$NATUREZA==9] <- "Cooperativa"
    data$NATUREZA[data$NATUREZA==10] <- "Servi\u00e7o Social Aut\u00f4nomo"
    data$NATUREZA[data$NATUREZA==11] <- "Entidade Beneficente sem fins lucrativos"
    data$NATUREZA[data$NATUREZA==12] <- "Economia Mista"
    data$NATUREZA[data$NATUREZA==13] <- "Sindicato"
    data$NATUREZA[data$NATUREZA==0] <- "Natureza inexistente"
    data$NATUREZA <- factor(data$NATUREZA)
  }

  # CLIENTEL
  if("CLIENTEL" %in% variables_names){
    data$CLIENTEL <- as.numeric(levels(data$CLIENTEL))[data$CLIENTEL]
    data$CLIENTEL[data$CLIENTEL==-99] <- NA
    data$CLIENTEL[data$CLIENTEL==1] <- "Atendimento de demanda espont\u00e2nea"
    data$CLIENTEL[data$CLIENTEL==2] <- "Atendimento de demanda referenciada"
    data$CLIENTEL[data$CLIENTEL==3] <- "Atendimento de demanda espont\u00e2nea e referenciada"
    data$CLIENTEL[data$CLIENTEL==0] <- "Fluxo de Clientela n\u00e3o exigido"
    data$CLIENTEL <- factor(data$CLIENTEL)
  }

  # TP_UNID
  if("TP_UNID" %in% variables_names){
    data$TP_UNID <- as.numeric(levels(data$TP_UNID))[data$TP_UNID]
    data$TP_UNID[data$TP_UNID==1] <- "Posto de sa\u00fade"
    data$TP_UNID[data$TP_UNID==2] <- "Centro de sa\u00fade / Unidade b\u00e1sica"
    data$TP_UNID[data$TP_UNID==4] <- "Policl\u00ednica"
    data$TP_UNID[data$TP_UNID==5] <- "Hospital geral"
    data$TP_UNID[data$TP_UNID==7] <- "Hospital Especializado"
    data$TP_UNID[data$TP_UNID==9] <- "Pronto socorro de hospital geral (antigo)"
    data$TP_UNID[data$TP_UNID==12] <- "Pronto socorro traumato-ortop\u00e9dico (antigo)"
    data$TP_UNID[data$TP_UNID==15] <- "Unidade mista"
    data$TP_UNID[data$TP_UNID==20] <- "Pronto socorro geral"
    data$TP_UNID[data$TP_UNID==21] <- "Pronto socorro especializado"
    data$TP_UNID[data$TP_UNID==22] <- "Consult\u00f3rio isolado"
    data$TP_UNID[data$TP_UNID==32] <- "Unidade m\u00f3vel fluvial"
    data$TP_UNID[data$TP_UNID==36] <- "Cl\u00ednica / Centro de sa\u00fade de especialidade"
    data$TP_UNID[data$TP_UNID==39] <- "Unidade de apoio diagnose e terapia (SADT isolado)"
    data$TP_UNID[data$TP_UNID==40] <- "Unidade m\u00f3vel terrestre"
    data$TP_UNID[data$TP_UNID==42] <- "Unidade m\u00f3vel de n\u00edvel pr\u00e9-hospitalar na \u00e1rea de urg\u00eancia"
    data$TP_UNID[data$TP_UNID==43] <- "Farm\u00e1cia"
    data$TP_UNID[data$TP_UNID==45] <- "Unidade de sa\u00fade da fam\u00edlia"
    data$TP_UNID[data$TP_UNID==50] <- "Unidade de vigil\u00e2ncia em sa\u00fade"
    data$TP_UNID[data$TP_UNID==60] <- "Cooperativa ou empresa de cess\u00e3o de trabalhadores na sa\u00fade"
    data$TP_UNID[data$TP_UNID==61] <- "Centro de parto normal - isolado"
    data$TP_UNID[data$TP_UNID==62] <- "Hospital / Dia - Isolado"
    data$TP_UNID[data$TP_UNID==63] <- "Unidade autorizadora"
    data$TP_UNID[data$TP_UNID==64] <- "Central de regula\u00e7\u00e3o de servi\u00e7os de sa\u00fade"
    data$TP_UNID[data$TP_UNID==65] <- "Unidade de vigil\u00e2ncia epidemiol\u00f3gica (antigo)"
    data$TP_UNID[data$TP_UNID==66] <- "Unidade de vigil\u00e2ncia sanit\u00e1ria (antigo)"
    data$TP_UNID[data$TP_UNID==67] <- "Laborat\u00f3rio central de sa\u00fade p\u00fablica LACEN"
    data$TP_UNID[data$TP_UNID==68] <- "Central de gest\u00e3o em sa\u00fade"
    data$TP_UNID[data$TP_UNID==69] <- "Centro de aten\u00e7\u00e3o hemoterapia e/ou hematologica"
    data$TP_UNID[data$TP_UNID==70] <- "Centro de aten\u00e7\u00e3o psicosocial"
    data$TP_UNID[data$TP_UNID==71] <- "Centro de apoio a sa\u00fade da fam\u00edlia"
    data$TP_UNID[data$TP_UNID==72] <- "Unidade de aten\u00e7\u00e3o a sa\u00fade ind\u00edgena"
    data$TP_UNID[data$TP_UNID==73] <- "Pronto atendimento"
    data$TP_UNID[data$TP_UNID==74] <- "P\u00f3lo academia da sa\u00fade"
    data$TP_UNID[data$TP_UNID==75] <- "Telessa\u00fade"
    data$TP_UNID[data$TP_UNID==76] <- "Central de regula\u00e7\u00e3o m\u00e9dica das urg\u00eancias"
    data$TP_UNID[data$TP_UNID==77] <- "Servi\u00e7o de aten\u00e7\u00e3o domiciliar isolado (Home care)"
    data$TP_UNID[data$TP_UNID==78] <- "Unidade de aten\u00e7\u00e3o em regime residencial"
    data$TP_UNID[data$TP_UNID==79] <- "Oficina ortop\u00e9dica"
    data$TP_UNID[data$TP_UNID==80] <- "Laborat\u00f3rio de sa\u00fade p\u00fablica"
    data$TP_UNID[data$TP_UNID==81] <- "Central de regula\u00e7\u00e3o do acesso"
    data$TP_UNID[data$TP_UNID==82] <- "Central de notifica\u00e7\u00e3o, capta\u00e7\u00e3o e distribui\u00e7\u00e3o de \u00f3rg\u00e3os estadual"
    data$TP_UNID[data$TP_UNID==83] <- "P\u00f3lo de preven\u00e7\u00e3o de doen\u00e7as e agravos e promo\u00e7\u00e3o da sa\u00fade"
    data$TP_UNID <- factor(data$TP_UNID)
  }

  # TURNO_AT
  if("TURNO_AT" %in% variables_names){
    data$TURNO_AT <- as.numeric(levels(data$TURNO_AT))[data$TURNO_AT]
    data$TURNO_AT[data$TURNO_AT==-99] <- NA
    data$TURNO_AT[data$TURNO_AT==1] <- "Turnos intermitentes"
    data$TURNO_AT[data$TURNO_AT==2] <- "Cont\u00ednuo 24h/dia (Pl Sab Dom Fer)"
    data$TURNO_AT[data$TURNO_AT==3] <- "Manh\u00e3 / Tarde / Noite"
    data$TURNO_AT[data$TURNO_AT==4] <- "Manh\u00e3"
    data$TURNO_AT[data$TURNO_AT==5] <- "Tarde"
    data$TURNO_AT[data$TURNO_AT==6] <- "Manh\u00e3 / Tarde"
    data$TURNO_AT[data$TURNO_AT==7] <- "Noite"
    data$TURNO_AT <- factor(data$TURNO_AT)
  }

  # NIV_HIER
  if("NIV_HIER" %in% variables_names){
    data$NIV_HIER <- as.numeric(levels(data$NIV_HIER))[data$NIV_HIER]
    data$NIV_HIER[data$NIV_HIER==0] <- NA
    data$NIV_HIER[data$NIV_HIER==99] <- NA
    data$NIV_HIER[data$NIV_HIER==1] <- "PAB-PABA"
    data$NIV_HIER[data$NIV_HIER==2] <- "M\u00e9dia M1"
    data$NIV_HIER[data$NIV_HIER==3] <- "M\u00e9dia M2 e M3"
    data$NIV_HIER[data$NIV_HIER==4] <- "Alta complexidade ambulatorial"
    data$NIV_HIER[data$NIV_HIER==5] <- "Baixa M1 e M2"
    data$NIV_HIER[data$NIV_HIER==6] <- "M\u00e9dia M2 e M3"
    data$NIV_HIER[data$NIV_HIER==7] <- "M\u00e9dia M3"
    data$NIV_HIER[data$NIV_HIER==8] <- "Alta complexidade hospitalar / ambulatorial"
    data$NIV_HIER <- factor(data$NIV_HIER)
  }

  # TERCEIRO
  if("TERCEIRO" %in% variables_names){
    data$TERCEIRO <- as.numeric(levels(data$TERCEIRO))[data$TERCEIRO]
    data$TERCEIRO[data$TERCEIRO==1] <- "Sim"
    data$TERCEIRO[data$TERCEIRO==0] <- "N\u00e3o"
    data$TERCEIRO[data$TERCEIRO==2] <- "N\u00e3o"
    data$TERCEIRO <- factor(data$TERCEIRO)
  }

  # CPF_PROF
  if("CPF_PROF" %in% variables_names){
    data$CPF_PROF <- as.character(levels(data$CPF_PROF))[data$CPF_PROF]
    data$CPF_PROF[data$CPF_PROF=="99999999999"] <- NA
    data$CPF_PROF[data$CPF_PROF=="00000000000000"] <- NA
    data$CPF_PROF <- factor(data$CPF_PROF)
  }

  # CBO
  if ("CBO" %in% variables_names) {
    data$CBO <- as.character(levels(data$CBO))[data$CBO]
    data <- dplyr::left_join(data, microdatasus::tabCBO, by = c("CBO" = "cod"))
  }

  # NOMEPROF
  if("NOMEPROF" %in% variables_names){
    data$NOMEPROF <- as.character(levels(data$NOMEPROF))[data$NOMEPROF]
  }

  # VINCULAC
  if("VINCULAC" %in% variables_names){
    data$VINCULAC <- as.numeric(levels(data$VINCULAC))[data$VINCULAC]
    data$VINCULAC[data$VINCULAC==1] <- "Profissional CONTRATADO"
    data$VINCULAC[data$VINCULAC==2] <- "Profissional AUT\u00d4NOMO"
    data$VINCULAC[data$VINCULAC==3] <- "Profissional V\u00cdNCULO N\u00c3O IDENTIFICADO"
    data$VINCULAC <- factor(data$VINCULAC)
  }

  # NAT_JUR
  if("NAT_JUR" %in% variables_names){
    data$NAT_JUR <- as.numeric(levels(data$NAT_JUR))[data$NAT_JUR]
    data$NAT_JUR[data$NAT_JUR==0] <- NA
    data$NAT_JUR[data$NAT_JUR==1000] <- "Administra\u00e7\u00e3o P\u00fablica"
    data$NAT_JUR[data$NAT_JUR==1015] <- "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Federal"
    data$NAT_JUR[data$NAT_JUR==1023] <- "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1031] <- "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Municipal"
    data$NAT_JUR[data$NAT_JUR==1040] <- "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Federal"
    data$NAT_JUR[data$NAT_JUR==1058] <- "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1066] <- "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Municipal"
    data$NAT_JUR[data$NAT_JUR==1074] <- "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Federal"
    data$NAT_JUR[data$NAT_JUR==1082] <- "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Estadual"
    data$NAT_JUR[data$NAT_JUR==1104] <- "Autarquia Federal"
    data$NAT_JUR[data$NAT_JUR==1112] <- "Autarquia Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1120] <- "Autarquia Municipal"
    data$NAT_JUR[data$NAT_JUR==1139] <- "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Federal"
    data$NAT_JUR[data$NAT_JUR==1147] <- "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1155] <- "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Municipal"
    data$NAT_JUR[data$NAT_JUR==1163] <- "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Federal"
    data$NAT_JUR[data$NAT_JUR==1171] <- "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1180] <- "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Municipal"
    data$NAT_JUR[data$NAT_JUR==1198] <- "Comiss\u00e3o Polinacional"
    data$NAT_JUR[data$NAT_JUR==1201] <- "Fundo P\u00fablico"
    data$NAT_JUR[data$NAT_JUR==1210] <- "Cons\u00f3rcio P\u00fablico de Direito P\u00fablico (Associa\u00e7\u00e3o P\u00fablica)"
    data$NAT_JUR[data$NAT_JUR==1228] <- "Cons\u00f3rcio P\u00fablico de Direito Privado"
    data$NAT_JUR[data$NAT_JUR==1236] <- "Estado ou Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1244] <- "Munic\u00edpio"
    data$NAT_JUR[data$NAT_JUR==1252] <- "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Federal"
    data$NAT_JUR[data$NAT_JUR==1260] <- "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1279] <- "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Municipal"
    data$NAT_JUR[data$NAT_JUR==2000] <- "Entidades Empresariais"
    data$NAT_JUR[data$NAT_JUR==2001] <- "Empresa P\u00fablica"
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
    data$NAT_JUR[data$NAT_JUR==2178] <- "Estabelecimento, no Brasil, de Sociedade Estrangeira"
    data$NAT_JUR[data$NAT_JUR==2194] <- "Estabelecimento, no Brasil, de Empresa Binacional Argentino-Brasileira"
    data$NAT_JUR[data$NAT_JUR==2216] <- "Empresa Domiciliada no Exterior"
    data$NAT_JUR[data$NAT_JUR==2224] <- "Clube/Fundo de Investimento"
    data$NAT_JUR[data$NAT_JUR==2232] <- "Sociedade Simples Pura"
    data$NAT_JUR[data$NAT_JUR==2240] <- "Sociedade Simples Limitada"
    data$NAT_JUR[data$NAT_JUR==2259] <- "Sociedade Simples em Nome Coletivo"
    data$NAT_JUR[data$NAT_JUR==2267] <- "Sociedade Simples em Comandita Simples"
    data$NAT_JUR[data$NAT_JUR==2275] <- "Empresa Binacional"
    data$NAT_JUR[data$NAT_JUR==2283] <- "Cons\u00f3rcio de Empregadores"
    data$NAT_JUR[data$NAT_JUR==2291] <- "Cons\u00f3rcio Simples"
    data$NAT_JUR[data$NAT_JUR==2305] <- "Empresa Individual de Responsabilidade Limitada (de Natureza Empres\u00e1ria)"
    data$NAT_JUR[data$NAT_JUR==2313] <- "Empresa Individual de Responsabilidade Limitada (de Natureza Simples)"
    data$NAT_JUR[data$NAT_JUR==2321] <- "Sociedade Unipessoal de Advogados"
    data$NAT_JUR[data$NAT_JUR==2330] <- "Cooperativas de Consumo"
    data$NAT_JUR[data$NAT_JUR==3000] <- "Entidades sem Fins Lucrativos"
    data$NAT_JUR[data$NAT_JUR==3034] <- "Servi\u00e7o Notarial e Registral (Cart\u00f3rio)"
    data$NAT_JUR[data$NAT_JUR==3069] <- "Funda\u00e7\u00e3o Privada"
    data$NAT_JUR[data$NAT_JUR==3077] <- "Servi\u00e7o Social Aut\u00f4nomo"
    data$NAT_JUR[data$NAT_JUR==3085] <- "Condom\u00ednio Edil\u00edcio"
    data$NAT_JUR[data$NAT_JUR==3107] <- "Comiss\u00e3o de Concilia\u00e7\u00e3o Pr\u00e9via"
    data$NAT_JUR[data$NAT_JUR==3115] <- "Entidade de Media\u00e7\u00e3o e Arbitragem"
    data$NAT_JUR[data$NAT_JUR==3131] <- "Entidade Sindical"
    data$NAT_JUR[data$NAT_JUR==3204] <- "Estabelecimento, no Brasil, de Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Estrangeiras"
    data$NAT_JUR[data$NAT_JUR==3212] <- "Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Domiciliada no Exterior"
    data$NAT_JUR[data$NAT_JUR==3220] <- "Organiza\u00e7\u00e3o Religiosa"
    data$NAT_JUR[data$NAT_JUR==3239] <- "Comunidade Ind\u00edgena"
    data$NAT_JUR[data$NAT_JUR==3247] <- "Fundo Privado"
    data$NAT_JUR[data$NAT_JUR==3255] <- "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Nacional de Partido Pol\u00edtico"
    data$NAT_JUR[data$NAT_JUR==3263] <- "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Regional de Partido Pol\u00edtico"
    data$NAT_JUR[data$NAT_JUR==3271] <- "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Local de Partido Pol\u00edtico"
    data$NAT_JUR[data$NAT_JUR==3280] <- "Comit\u00ea Financeiro de Partido Pol\u00edtico"
    data$NAT_JUR[data$NAT_JUR==3298] <- "Frente Plebiscit\u00e1ria ou Referend\u00e1ria"
    data$NAT_JUR[data$NAT_JUR==3306] <- "Organiza\u00e7\u00e3o Social (OS)"
    data$NAT_JUR[data$NAT_JUR==3310] <- "Demais Condom\u00ednios"
    data$NAT_JUR[data$NAT_JUR==3999] <- "Associa\u00e7\u00e3o Privada"
    data$NAT_JUR[data$NAT_JUR==4000] <- "Pessoas F\u00edsicas"
    data$NAT_JUR[data$NAT_JUR==4014] <- "Empresa Individual Imobili\u00e1ria"
    data$NAT_JUR[data$NAT_JUR==4022] <- "Segurado Especial"
    data$NAT_JUR[data$NAT_JUR==4081] <- "Contribuinte individual"
    data$NAT_JUR[data$NAT_JUR==4090] <- "Candidato a Cargo Pol\u00edtico Eletivo"
    data$NAT_JUR[data$NAT_JUR==4111] <- "Leiloeiro"
    data$NAT_JUR[data$NAT_JUR==4124] <- "Produtor Rural (Pessoa F\u00edsica)"
    data$NAT_JUR[data$NAT_JUR==5000] <- "Organiza\u00e7\u00f5es Internacionais e Outras Institui\u00e7\u00f5es Extraterritoriais"
    data$NAT_JUR[data$NAT_JUR==5010] <- "Organiza\u00e7\u00e3o Internacional"
    data$NAT_JUR[data$NAT_JUR==5029] <- "Representa\u00e7\u00e3o Diplom\u00e1tica Estrangeira"
    data$NAT_JUR[data$NAT_JUR==5037] <- "Outras Institui\u00e7\u00f5es Extraterritoriais"
    data$NAT_JUR <- factor(data$NAT_JUR)
  }


  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  data <- as.data.frame(lapply(X = data, FUN = stringi::stri_unescape_unicode))

  # Return
  return(data)
}
