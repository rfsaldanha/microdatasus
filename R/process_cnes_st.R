#' Process CNES-ST variables from DataSUS
#' 
#' \code{process_cnes_st} processes CNES-ST variables retrieved by \code{fetch_datasus()}.
#' 
#' This function processes CNES-ST (Estabelecimentos) variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#' 
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param nomes optional logical. \code{TRUE} by default, add  \code{FANTASIA} and \code{RAZÃO SOCIAL} names to the dataset.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#' 
#' @examples 
#' df <- fetch_datasus(year_start = 2015, month_start = 1,
#'                     year_end = 2015, month_end = 1,
#'                     uf = "RJ",
#'                     information_system = "CNES-ST")
#' df_a <- process_cnes_ST(df)
#' df_b <- process_cnes_pf(df, nomes = FALSE, municipality_data = FALSE)

process_cnes_st <- function(data, nomes = TRUE, municipality_data = TRUE) {
  # Variables names
  variables_names <- names(data)
  
  # CNES
  if("CNES" %in% variables_names){
    data$CNES <- as.character(levels(data$CNES))[data$CNES]
  }
  
  # Nome fantasia e razão social
  if(nomes == TRUE){
    data <- dplyr::left_join(data, cadger, by = c("CNES" = "CNES"))
  }
  
  # CODUFMUN
  if ("CODUFMUN" %in% variables_names & municipality_data == TRUE) {
    data$CODUFMUN <- as.integer(as.character(data$CODUFMUN))
    colnames(tabMun)[1] <- "CODUFMUN"
    data <- dplyr::left_join(data, tabMun, by = "CODUFMUN")
  } else {
    data$CODUFMUN <- as.integer(as.character(data$CODUFMUN))
  }
  
  # COD_CEP
  if("COD_CEP" %in% variables_names){
    data$COD_CEP <- as.integer(data$COD_CEP)
  }
  
  # CPF_CNPJ
  if("CPF_CNPJ" %in% variables_names){
    data$CPF_CNPJ <- as.integer(data$CPF_CNPJ)
  }

  # PF_PJ
  if("PF_PJ" %in% variables_names){
    data$PF_PJ <- as.numeric(levels(data$PF_PJ))[data$PF_PJ]
    data$PF_PJ[data$PF_PJ==1] <- "Pessoa física"
    data$PF_PJ[data$PF_PJ==3] <- "Pessoa jurídica"
    data$PF_PJ <- factor(data$PF_PJ)
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
  
  # COD_IR
  if("COD_IR" %in% variables_names){
    data$COD_IR <- as.numeric(levels(data$COD_IR))[data$COD_IR]
    data$COD_IR[data$COD_IR==0] <- NA
    data$COD_IR[data$COD_IR==10] <- "Estabelecimento público"
    data$COD_IR[data$COD_IR==11] <- "Estabelecimento filantrópico"
    data$COD_IR[data$COD_IR==12] <- "Estabelecimento sem fins lucrativos"
    data$COD_IR[data$COD_IR==13] <- "Estabelecimento privado luvrativa simples"
    data$COD_IR[data$COD_IR==14] <- "Estabelecimento privado luvrativa"
    data$COD_IR[data$COD_IR==15] <- "Estabelecimento sindical"
    data$COD_IR[data$COD_IR==16] <- "Estabelecimento pessoa física"
    data$COD_IR[data$COD_IR==19] <- "Estabelecimento Ret.Manten.código 19"
    data$COD_IR <- factor(data$COD_IR)
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
  
  # VINC_SUS
  if("VINC_SUS" %in% variables_names){
    data$VINC_SUS <- as.numeric(levels(data$VINC_SUS))[data$VINC_SUS]
    data$VINC_SUS[data$VINC_SUS==0] <- "Não"
    data$VINC_SUS[data$VINC_SUS==1] <- "Sim"
    data$VINC_SUS <- factor(data$VINC_SUS)
  }
  
  # TPGESTAO
  if("TPGESTAO" %in% variables_names){
    data$TPGESTAO <- as.character(levels(data$TPGESTAO))[data$TPGESTAO]
    data$TPGESTAO[data$TPGESTAO=="D"] <- "Dupla"
    data$TPGESTAO[data$TPGESTAO=="E"] <- "Estadual"
    data$TPGESTAO[data$TPGESTAO=="M"] <- "Municipal"
    data$TPGESTAO[data$TPGESTAO=="Z"] <- "Sem gestão"
    data$TPGESTAO[data$TPGESTAO=="S"] <- "Sem gestão"
    data$TPGESTAO <- factor(data$TPGESTAO)
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
  
  # RETENCAO
  if("RETENCAO" %in% variables_names){
    data$RETENCAO <- as.numeric(levels(data$RETENCAO))[data$RETENCAO]
    data$RETENCAO[data$RETENCAO==0] <- NA
    data$RETENCAO[data$RETENCAO==10] <- "Estabelecimento público"
    data$RETENCAO[data$RETENCAO==11] <- "Estabelecimento filantrópico"
    data$RETENCAO[data$RETENCAO==12] <- "Estabelecimento sem fins lucrativos"
    data$RETENCAO[data$RETENCAO==13] <- "Estabelecimento privado luvrativa simples"
    data$RETENCAO[data$RETENCAO==14] <- "Estabelecimento privado luvrativa"
    data$RETENCAO[data$RETENCAO==15] <- "Estabelecimento sindical"
    data$RETENCAO[data$RETENCAO==16] <- "Estabelecimento pessoa física"
    data$RETENCAO <- factor(data$RETENCAO)
  }
  
  # ATIVIDAD
  if("ATIVIDAD" %in% variables_names){
    data$ATIVIDAD <- as.numeric(levels(data$ATIVIDAD))[data$ATIVIDAD]
    data$ATIVIDAD[data$ATIVIDAD==-99] <- NA
    data$ATIVIDAD[data$ATIVIDAD==1] <- "Unidade Universitária"
    data$ATIVIDAD[data$ATIVIDAD==2] <- "Unidade Escola Superior Isolada"
    data$ATIVIDAD[data$ATIVIDAD==3] <- "Unidade Auxiliar de Ensino"
    data$ATIVIDAD[data$ATIVIDAD==4] <- "Unidade sem atividade de Ensino"
    data$ATIVIDAD[data$ATIVIDAD==5] <- "Hospital de ensino"
    data$ATIVIDAD <- factor(data$ATIVIDAD)
  }
  
  # NATUREZA
  if("NATUREZA" %in% variables_names){
    data$NATUREZA <- as.numeric(levels(data$NATUREZA))[data$NATUREZA]
    data$NATUREZA[data$NATUREZA==-99] <- NA
    data$NATUREZA[data$NATUREZA==1] <- "Administração Direta da Saúde (MS, SES, e SMS)"
    data$NATUREZA[data$NATUREZA==2] <- "Adm Direta outros orgãos (MEX, MEx, Marinha,...)"
    data$NATUREZA[data$NATUREZA==3] <- "Adm Indireta - Autarquias"
    data$NATUREZA[data$NATUREZA==4] <- "Adm Indireta - Fundação Pública"
    data$NATUREZA[data$NATUREZA==5] <- "Adm Indireta - Empresa Pública"
    data$NATUREZA[data$NATUREZA==6] <- "Adm Indireta - Organização Social Pública"
    data$NATUREZA[data$NATUREZA==7] <- "Empresa Privada"
    data$NATUREZA[data$NATUREZA==8] <- "Fundação Privada"
    data$NATUREZA[data$NATUREZA==9] <- "Cooperativa"
    data$NATUREZA[data$NATUREZA==10] <- "Serviço Social Autônomo"
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
    data$CLIENTEL[data$CLIENTEL==1] <- "Atendimento de demanda espontânea"
    data$CLIENTEL[data$CLIENTEL==2] <- "Atendimento de demanda referenciada"
    data$CLIENTEL[data$CLIENTEL==3] <- "Atendimento de demanda espontânea e referenciada"
    data$CLIENTEL[data$CLIENTEL==0] <- "Fluxo de Clientela não exigido"
    data$CLIENTEL <- factor(data$CLIENTEL)
  }
  
  # TP_UNID
  if("TP_UNID" %in% variables_names){
    data$TP_UNID <- as.numeric(levels(data$TP_UNID))[data$TP_UNID]
    data$TP_UNID[data$TP_UNID==1] <- "Posto de saúde"
    data$TP_UNID[data$TP_UNID==2] <- "Centro de saúde / Unidade básica"
    data$TP_UNID[data$TP_UNID==4] <- "Policlínica"
    data$TP_UNID[data$TP_UNID==5] <- "Hospital geral"
    data$TP_UNID[data$TP_UNID==7] <- "Hospital Especializado"
    data$TP_UNID[data$TP_UNID==9] <- "Pronto socorro de hospital geral (antigo)"
    data$TP_UNID[data$TP_UNID==12] <- "Pronto socorro traumato-ortopédico (antigo)"
    data$TP_UNID[data$TP_UNID==15] <- "Unidade mista"
    data$TP_UNID[data$TP_UNID==20] <- "Pronto socorro geral"
    data$TP_UNID[data$TP_UNID==21] <- "Pronto socorro especializado"
    data$TP_UNID[data$TP_UNID==22] <- "Consultório isolado"
    data$TP_UNID[data$TP_UNID==32] <- "Unidade móvel fluvial"
    data$TP_UNID[data$TP_UNID==36] <- "Clínica / Centro de saúde de especialidade"
    data$TP_UNID[data$TP_UNID==39] <- "Unidade de apoio diagnose e terapia (SADT isolado)"
    data$TP_UNID[data$TP_UNID==40] <- "Unidade móvel terrestre"
    data$TP_UNID[data$TP_UNID==42] <- "Unidade móvel de nível pré-hospitalar na área de urgência"
    data$TP_UNID[data$TP_UNID==43] <- "Farmácia"
    data$TP_UNID[data$TP_UNID==45] <- "Unidade de saúde da família"
    data$TP_UNID[data$TP_UNID==50] <- "Unidade de vigilância em saúde"
    data$TP_UNID[data$TP_UNID==60] <- "Cooperativa ou empresa de cessão de trabalhadores na saúde"
    data$TP_UNID[data$TP_UNID==61] <- "Centro de parto normal - isolado"
    data$TP_UNID[data$TP_UNID==62] <- "Hospital / Dia - Isolado"
    data$TP_UNID[data$TP_UNID==63] <- "Unidade autorizadora"
    data$TP_UNID[data$TP_UNID==64] <- "Central de regulação de serviços de saúde"
    data$TP_UNID[data$TP_UNID==65] <- "Unidade de vigilância epidemiológica (antigo)"
    data$TP_UNID[data$TP_UNID==66] <- "Unidade de vigilância sanitária (antigo)"
    data$TP_UNID[data$TP_UNID==67] <- "Laboratório central de saúde pública LACEN"
    data$TP_UNID[data$TP_UNID==68] <- "Central de gestão em saúde"
    data$TP_UNID[data$TP_UNID==69] <- "Centro de atenção hemoterapia e/ou hematologica"
    data$TP_UNID[data$TP_UNID==70] <- "Centro de atenção psicosocial"
    data$TP_UNID[data$TP_UNID==71] <- "Centro de apoio a saúde da família"
    data$TP_UNID[data$TP_UNID==72] <- "Unidade de atenção a saúde indígena"
    data$TP_UNID[data$TP_UNID==73] <- "Pronto atendimento"
    data$TP_UNID[data$TP_UNID==74] <- "Pólo academia da saúde"
    data$TP_UNID[data$TP_UNID==75] <- "Telessaúde"
    data$TP_UNID[data$TP_UNID==76] <- "Central de regulação médica das urgências"
    data$TP_UNID[data$TP_UNID==77] <- "Serviço de atenção domiciliar isolado (Home care)"
    data$TP_UNID[data$TP_UNID==78] <- "Unidade de atenção em regime residencial"
    data$TP_UNID[data$TP_UNID==79] <- "Oficina ortopédica"
    data$TP_UNID[data$TP_UNID==80] <- "Laboratório de saúde pública"
    data$TP_UNID[data$TP_UNID==81] <- "Central de regulação do acesso"
    data$TP_UNID[data$TP_UNID==82] <- "Central de notificação, captação e distribuição de órgãos estadual"
    data$TP_UNID[data$TP_UNID==83] <- "Pólo de prevenção de doenças e agravos e promoção da saúde"
    data$TP_UNID <- factor(data$TP_UNID)
  }
  
  # TURNO_AT
  if("TURNO_AT" %in% variables_names){
    data$TURNO_AT <- as.numeric(levels(data$TURNO_AT))[data$TURNO_AT]
    data$TURNO_AT[data$TURNO_AT==-99] <- NA
    data$TURNO_AT[data$TURNO_AT==1] <- "Turnos intermitentes"
    data$TURNO_AT[data$TURNO_AT==2] <- "Contínuo 24h/dia (Pl Sab Dom Fer)"
    data$TURNO_AT[data$TURNO_AT==3] <- "Manhã / Tarde / Noite"
    data$TURNO_AT[data$TURNO_AT==4] <- "Manhã"
    data$TURNO_AT[data$TURNO_AT==5] <- "Tarde"
    data$TURNO_AT[data$TURNO_AT==6] <- "Manhã / Tarde"
    data$TURNO_AT[data$TURNO_AT==7] <- "Noite"
    data$TURNO_AT <- factor(data$TURNO_AT)
  }
  
  # NIV_HIER
  if("NIV_HIER" %in% variables_names){
    data$NIV_HIER <- as.numeric(levels(data$NIV_HIER))[data$NIV_HIER]
    data$NIV_HIER[data$NIV_HIER==0] <- NA
    data$NIV_HIER[data$NIV_HIER==99] <- NA
    data$NIV_HIER[data$NIV_HIER==1] <- "PAB-PABA"
    data$NIV_HIER[data$NIV_HIER==2] <- "Média M1"
    data$NIV_HIER[data$NIV_HIER==3] <- "Média M2 e M3"
    data$NIV_HIER[data$NIV_HIER==4] <- "Alta complexidade ambulatorial"
    data$NIV_HIER[data$NIV_HIER==5] <- "Baixa M1 e M2"
    data$NIV_HIER[data$NIV_HIER==6] <- "Média M2 e M3"
    data$NIV_HIER[data$NIV_HIER==7] <- "Média M3"
    data$NIV_HIER[data$NIV_HIER==8] <- "Alta complexidade hospitalar / ambulatorial"
    data$NIV_HIER <- factor(data$NIV_HIER)
  }
  
  # TP_PREST
  if("TP_PREST" %in% variables_names){
    data$TP_PREST <- as.numeric(levels(data$TP_PREST))[data$TP_PREST]
    data$TP_PREST[data$TP_PREST==-99] <- NA
    data$TP_PREST[data$TP_PREST==30] <- "Público federal"
    data$TP_PREST[data$TP_PREST==40] <- "Público estadual"
    data$TP_PREST[data$TP_PREST==50] <- "Público municipal"
    data$TP_PREST[data$TP_PREST==61] <- "Filantrópico com CNAS válido"
    data$TP_PREST[data$TP_PREST==80] <- "Sindicato"
    data$TP_PREST[data$TP_PREST==20] <- "Privado com fins lucrativos"
    data$TP_PREST[data$TP_PREST==22] <- "Privado optantes pelo simples"
    data$TP_PREST[data$TP_PREST==60] <- "Privado sem fins lucrativos"
    data$TP_PREST <- factor(data$TP_PREST)
  }
  
  # CO_BANCO
  if("CO_BANCO" %in% variables_names){
    data$CO_BANCO <- as.integer(data$CO_BANCO)
  }
  
  # CO_AGENC
  if("CO_AGENC" %in% variables_names){
    data$CO_AGENC <- as.integer(data$CO_AGENC)
  }
  
  # C_CORREN
  if("C_CORREN" %in% variables_names){
    data$C_CORREN <- as.integer(data$C_CORREN)
  }
  
  # CONTRATM
  if("CONTRATM" %in% variables_names){
    data$CONTRATM <- as.integer(data$CONTRATM)
  }
  
  # DT_PUBLM
  if("DT_PUBLM" %in% variables_names){
    data$DT_PUBLM <- as.integer(data$DT_PUBLM)
  }
  
  # CONTRATE
  if("CONTRATE" %in% variables_names){
    data$CONTRATE <- as.integer(data$CONTRATE)
  }
  
  # DT_PUBLE
  if("DT_PUBLE" %in% variables_names){
    data$DT_PUBLE <- as.integer(data$DT_PUBLE)
  }
  
  # ALVARA
  if("ALVARA" %in% variables_names){
    data$ALVARA <- as.integer(data$ALVARA)
  }
  
  # DT_EXPED
  if("DT_EXPED" %in% variables_names){
    data$DT_EXPED <- as.integer(data$DT_EXPED)
  }
  
  # ORGEXPED
  if("ORGEXPED" %in% variables_names){
    data$ORGEXPED <- as.numeric(levels(data$ORGEXPED))[data$ORGEXPED]
    data$ORGEXPED[data$ORGEXPED==1] <- "SES"
    data$ORGEXPED[data$ORGEXPED==2] <- "SMS"
    data$ORGEXPED <- factor(data$ORGEXPED)
  }
  
  # AV_ACRED
  if("AV_ACRED" %in% variables_names){
    data$AV_ACRED <- as.numeric(levels(data$AV_ACRED))[data$AV_ACRED]
    data$AV_ACRED[data$AV_ACRED==1] <- "Sim"
    data$AV_ACRED[data$AV_ACRED==2] <- "Não"
    data$AV_ACRED <- factor(data$AV_ACRED)
  }
  
  # CLASAVAL
  if("CLASAVAL" %in% variables_names){
    data$CLASAVAL <- as.numeric(levels(data$CLASAVAL))[data$CLASAVAL]
    data$CLASAVAL[data$CLASAVAL==1] <- "Acreditado no nível 1"
    data$CLASAVAL[data$CLASAVAL==2] <- "Acreditado no nível 2"
    data$CLASAVAL[data$CLASAVAL==3] <- "Acreditado no nível 3"
    data$CLASAVAL[data$CLASAVAL==0] <- "Não atendeu aos padrões mínimos"
    data$CLASAVAL[data$CLASAVAL==-9] <- NA
    data$CLASAVAL <- factor(data$CLASAVAL)
  }
  
  # DT_ACRED
  if("DT_ACRED" %in% variables_names){
    data$DT_ACRED <- as.integer(data$DT_ACRED)
  }
  
  # AV_PNASS
  if("AV_PNASS" %in% variables_names){
    data$AV_PNASS <- as.numeric(levels(data$AV_PNASS))[data$AV_PNASS]
    data$AV_PNASS[data$AV_PNASS==1] <- "Sim"
    data$AV_PNASS[data$AV_PNASS==2] <- "Não"
    data$AV_PNASS <- factor(data$AV_PNASS)
  }
  
  # DT_PNASS
  if("DT_PNASS" %in% variables_names){
    data$DT_PNASS <- as.integer(data$DT_PNASS)
  }
  
  # GESPRG1E
  if("GESPRG1E" %in% variables_names){
    data$GESPRG1E <- as.numeric(levels(data$GESPRG1E))[data$GESPRG1E]
    data$GESPRG1E[data$GESPRG1E==1] <- "Sim"
    data$GESPRG1E[data$GESPRG1E==0] <- "Não"
    data$GESPRG1E <- factor(data$GESPRG1E)
  }
  
  # GESPRG1M
  if("GESPRG1M" %in% variables_names){
    data$GESPRG1M <- as.numeric(levels(data$GESPRG1M))[data$GESPRG1M]
    data$GESPRG1M[data$GESPRG1M==1] <- "Sim"
    data$GESPRG1M[data$GESPRG1M==0] <- "Não"
    data$GESPRG1M <- factor(data$GESPRG1M)
  }
  
  # GESPRG2E
  if("GESPRG2E" %in% variables_names){
    data$GESPRG2E <- as.numeric(levels(data$GESPRG2E))[data$GESPRG2E]
    data$GESPRG2E[data$GESPRG2E==1] <- "Sim"
    data$GESPRG2E[data$GESPRG2E==0] <- "Não"
    data$GESPRG2E <- factor(data$GESPRG2E)
  }
  
  # GESPRG2M
  if("GESPRG2M" %in% variables_names){
    data$GESPRG2M <- as.numeric(levels(data$GESPRG2M))[data$GESPRG2M]
    data$GESPRG2M[data$GESPRG2M==1] <- "Sim"
    data$GESPRG2M[data$GESPRG2M==0] <- "Não"
    data$GESPRG2M <- factor(data$GESPRG2M)
  }
  
  # GESPRG4E
  if("GESPRG4E" %in% variables_names){
    data$GESPRG4E <- as.numeric(levels(data$GESPRG4E))[data$GESPRG4E]
    data$GESPRG4E[data$GESPRG4E==1] <- "Sim"
    data$GESPRG4E[data$GESPRG4E==0] <- "Não"
    data$GESPRG4E <- factor(data$GESPRG4E)
  }
  
  # GESPRG4M
  if("GESPRG4M" %in% variables_names){
    data$GESPRG4M <- as.numeric(levels(data$GESPRG4M))[data$GESPRG4M]
    data$GESPRG4M[data$GESPRG4M==1] <- "Sim"
    data$GESPRG4M[data$GESPRG4M==0] <- "Não"
    data$GESPRG4M <- factor(data$GESPRG4M)
  }
  
  # NIVATE_A
  if("NIVATE_A" %in% variables_names){
    data$NIVATE_A <- as.numeric(levels(data$NIVATE_A))[data$NIVATE_A]
    data$NIVATE_A[data$NIVATE_A==1] <- "Sim"
    data$NIVATE_A[data$NIVATE_A==0] <- "Não"
    data$NIVATE_A <- factor(data$NIVATE_A)
  }
  
  # GESPRG3E
  if("GESPRG3E" %in% variables_names){
    data$GESPRG3E <- as.numeric(levels(data$GESPRG3E))[data$GESPRG3E]
    data$GESPRG3E[data$GESPRG3E==1] <- "Sim"
    data$GESPRG3E[data$GESPRG3E==0] <- "Não"
    data$GESPRG3E <- factor(data$GESPRG3E)
  }
  
  # GESPRG3M
  if("GESPRG3M" %in% variables_names){
    data$GESPRG3M <- as.numeric(levels(data$GESPRG3M))[data$GESPRG3M]
    data$GESPRG3M[data$GESPRG3M==1] <- "Sim"
    data$GESPRG3M[data$GESPRG3M==0] <- "Não"
    data$GESPRG3M <- factor(data$GESPRG3M)
  }
  
  # GESPRG5E
  if("GESPRG5E" %in% variables_names){
    data$GESPRG5E <- as.numeric(levels(data$GESPRG5E))[data$GESPRG5E]
    data$GESPRG5E[data$GESPRG5E==1] <- "Sim"
    data$GESPRG5E[data$GESPRG5E==0] <- "Não"
    data$GESPRG5E <- factor(data$GESPRG5E)
  }
  
  # GESPRG5M
  if("GESPRG5M" %in% variables_names){
    data$GESPRG5M <- as.numeric(levels(data$GESPRG5M))[data$GESPRG5M]
    data$GESPRG5M[data$GESPRG5M==1] <- "Sim"
    data$GESPRG5M[data$GESPRG5M==0] <- "Não"
    data$GESPRG5M <- factor(data$GESPRG5M)
  }
  
  # GESPRG6E
  if("GESPRG6E" %in% variables_names){
    data$GESPRG6E <- as.numeric(levels(data$GESPRG6E))[data$GESPRG6E]
    data$GESPRG6E[data$GESPRG6E==1] <- "Sim"
    data$GESPRG6E[data$GESPRG6E==0] <- "Não"
    data$GESPRG6E <- factor(data$GESPRG6E)
  }
  
  # GESPRG6M
  if("GESPRG6M" %in% variables_names){
    data$GESPRG6M <- as.numeric(levels(data$GESPRG6M))[data$GESPRG6M]
    data$GESPRG6M[data$GESPRG6M==1] <- "Sim"
    data$GESPRG6M[data$GESPRG6M==0] <- "Não"
    data$GESPRG6M <- factor(data$GESPRG6M)
  }
  
  # NIVATE_H
  if("NIVATE_H" %in% variables_names){
    data$NIVATE_H <- as.numeric(levels(data$NIVATE_H))[data$NIVATE_H]
    data$NIVATE_H[data$NIVATE_H==1] <- "Sim"
    data$NIVATE_H[data$NIVATE_H==0] <- "Não"
    data$NIVATE_H <- factor(data$NIVATE_H)
  }
  
  # GESPRG3E
  if("GESPRG3E" %in% variables_names){
    data$GESPRG3E <- as.numeric(levels(data$GESPRG3E))[data$GESPRG3E]
    data$GESPRG3E[data$GESPRG3E==1] <- "Sim"
    data$GESPRG3E[data$GESPRG3E==0] <- "Não"
    data$GESPRG3E <- factor(data$GESPRG3E)
  }
  
  # URGEMERG
  if("URGEMERG" %in% variables_names){
    data$URGEMERG <- as.numeric(levels(data$URGEMERG))[data$URGEMERG]
    data$URGEMERG[data$URGEMERG==1] <- "Sim"
    data$URGEMERG[data$URGEMERG==0] <- "Não"
    data$URGEMERG <- factor(data$URGEMERG)
  }
  
  # ATENDAMB
  if("ATENDAMB" %in% variables_names){
    data$ATENDAMB <- as.numeric(levels(data$ATENDAMB))[data$ATENDAMB]
    data$ATENDAMB[data$ATENDAMB==1] <- "Sim"
    data$ATENDAMB[data$ATENDAMB==0] <- "Não"
    data$ATENDAMB <- factor(data$ATENDAMB)
  }
  
  # CENTROBS
  if("CENTROBS" %in% variables_names){
    data$CENTROBS <- as.numeric(levels(data$CENTROBS))[data$CENTROBS]
    data$CENTROBS[data$CENTROBS==1] <- "Sim"
    data$CENTROBS[data$CENTROBS==0] <- "Não"
    data$CENTROBS <- factor(data$CENTROBS)
  }
  
  # CENTRNEO
  if("CENTRNEO" %in% variables_names){
    data$CENTRNEO <- as.numeric(levels(data$CENTRNEO))[data$CENTRNEO]
    data$CENTRNEO[data$CENTRNEO==1] <- "Sim"
    data$CENTRNEO[data$CENTRNEO==0] <- "Não"
    data$CENTRNEO <- factor(data$CENTRNEO)
  }
  
  # ATENDHOS
  if("ATENDHOS" %in% variables_names){
    data$ATENDHOS <- as.numeric(levels(data$ATENDHOS))[data$ATENDHOS]
    data$ATENDHOS[data$ATENDHOS==1] <- "Sim"
    data$ATENDHOS[data$ATENDHOS==0] <- "Não"
    data$ATENDHOS <- factor(data$ATENDHOS)
  }
  
  # SERAP01P
  if("SERAP01P" %in% variables_names){
    data$SERAP01P <- as.numeric(levels(data$SERAP01P))[data$SERAP01P]
    data$SERAP01P[data$SERAP01P==1] <- "Sim"
    data$SERAP01P[data$SERAP01P==0] <- "Não"
    data$SERAP01P <- factor(data$SERAP01P)
  }
  
  # SERAP01T
  if("SERAP01T" %in% variables_names){
    data$SERAP01T <- as.numeric(levels(data$SERAP01T))[data$SERAP01T]
    data$SERAP01T[data$SERAP01T==1] <- "Sim"
    data$SERAP01T[data$SERAP01T==0] <- "Não"
    data$SERAP01T <- factor(data$SERAP01T)
  }
  
  # SERAP02P
  if("SERAP02P" %in% variables_names){
    data$SERAP02P <- as.numeric(levels(data$SERAP02P))[data$SERAP02P]
    data$SERAP02P[data$SERAP02P==1] <- "Sim"
    data$SERAP02P[data$SERAP02P==0] <- "Não"
    data$SERAP02P <- factor(data$SERAP02P)
  }
  
  # SERAP02T
  if("SERAP02T" %in% variables_names){
    data$SERAP02T <- as.numeric(levels(data$SERAP02T))[data$SERAP02T]
    data$SERAP02T[data$SERAP02T==1] <- "Sim"
    data$SERAP02T[data$SERAP02T==0] <- "Não"
    data$SERAP02T <- factor(data$SERAP02T)
  }
  
  # SERAP03P
  if("SERAP03P" %in% variables_names){
    data$SERAP03P <- as.numeric(levels(data$SERAP03P))[data$SERAP03P]
    data$SERAP03P[data$SERAP03P==1] <- "Sim"
    data$SERAP03P[data$SERAP03P==0] <- "Não"
    data$SERAP03P <- factor(data$SERAP03P)
  }
  
  # SERAP03T
  if("SERAP03T" %in% variables_names){
    data$SERAP03T <- as.numeric(levels(data$SERAP03T))[data$SERAP03T]
    data$SERAP03T[data$SERAP03T==1] <- "Sim"
    data$SERAP03T[data$SERAP03T==0] <- "Não"
    data$SERAP03T <- factor(data$SERAP03T)
  }
  
  # SERAP04P
  if("SERAP04P" %in% variables_names){
    data$SERAP04P <- as.numeric(levels(data$SERAP04P))[data$SERAP04P]
    data$SERAP04P[data$SERAP04P==1] <- "Sim"
    data$SERAP04P[data$SERAP04P==0] <- "Não"
    data$SERAP04P <- factor(data$SERAP04P)
  }
  
  # SERAP04T
  if("SERAP04T" %in% variables_names){
    data$SERAP04T <- as.numeric(levels(data$SERAP04T))[data$SERAP04T]
    data$SERAP04T[data$SERAP04T==1] <- "Sim"
    data$SERAP04T[data$SERAP04T==0] <- "Não"
    data$SERAP04T <- factor(data$SERAP04T)
  }
  
  # SERAP05P
  if("SERAP05P" %in% variables_names){
    data$SERAP05P <- as.numeric(levels(data$SERAP05P))[data$SERAP05P]
    data$SERAP05P[data$SERAP05P==1] <- "Sim"
    data$SERAP05P[data$SERAP05P==0] <- "Não"
    data$SERAP05P <- factor(data$SERAP05P)
  }
  
  # SERAP05T
  if("SERAP05T" %in% variables_names){
    data$SERAP05T <- as.numeric(levels(data$SERAP05T))[data$SERAP05T]
    data$SERAP05T[data$SERAP05T==1] <- "Sim"
    data$SERAP05T[data$SERAP05T==0] <- "Não"
    data$SERAP05T <- factor(data$SERAP05T)
  }
  
  # SERAP06P
  if("SERAP06P" %in% variables_names){
    data$SERAP06P <- as.numeric(levels(data$SERAP06P))[data$SERAP06P]
    data$SERAP06P[data$SERAP06P==1] <- "Sim"
    data$SERAP06P[data$SERAP06P==0] <- "Não"
    data$SERAP06P <- factor(data$SERAP06P)
  }
  
  # SERAP06T
  if("SERAP06T" %in% variables_names){
    data$SERAP06T <- as.numeric(levels(data$SERAP06T))[data$SERAP06T]
    data$SERAP06T[data$SERAP06T==1] <- "Sim"
    data$SERAP06T[data$SERAP06T==0] <- "Não"
    data$SERAP06T <- factor(data$SERAP06T)
  }
  
  # SERAP07P
  if("SERAP07P" %in% variables_names){
    data$SERAP07P <- as.numeric(levels(data$SERAP07P))[data$SERAP07P]
    data$SERAP07P[data$SERAP07P==1] <- "Sim"
    data$SERAP07P[data$SERAP07P==0] <- "Não"
    data$SERAP07P <- factor(data$SERAP07P)
  }
  
  # SERAP07T
  if("SERAP07T" %in% variables_names){
    data$SERAP07T <- as.numeric(levels(data$SERAP07T))[data$SERAP07T]
    data$SERAP07T[data$SERAP07T==1] <- "Sim"
    data$SERAP07T[data$SERAP07T==0] <- "Não"
    data$SERAP07T <- factor(data$SERAP07T)
  }
  
  # SERAP08P
  if("SERAP08P" %in% variables_names){
    data$SERAP08P <- as.numeric(levels(data$SERAP08P))[data$SERAP08P]
    data$SERAP08P[data$SERAP08P==1] <- "Sim"
    data$SERAP08P[data$SERAP08P==0] <- "Não"
    data$SERAP08P <- factor(data$SERAP08P)
  }
  
  # SERAP08T
  if("SERAP08T" %in% variables_names){
    data$SERAP08T <- as.numeric(levels(data$SERAP08T))[data$SERAP08T]
    data$SERAP08T[data$SERAP08T==1] <- "Sim"
    data$SERAP08T[data$SERAP08T==0] <- "Não"
    data$SERAP08T <- factor(data$SERAP08T)
  }
  
  # SERAP09P
  if("SERAP09P" %in% variables_names){
    data$SERAP09P <- as.numeric(levels(data$SERAP09P))[data$SERAP09P]
    data$SERAP09P[data$SERAP09P==1] <- "Sim"
    data$SERAP09P[data$SERAP09P==0] <- "Não"
    data$SERAP09P <- factor(data$SERAP09P)
  }
  
  # SERAP09T
  if("SERAP09T" %in% variables_names){
    data$SERAP09T <- as.numeric(levels(data$SERAP09T))[data$SERAP09T]
    data$SERAP09T[data$SERAP09T==1] <- "Sim"
    data$SERAP09T[data$SERAP09T==0] <- "Não"
    data$SERAP09T <- factor(data$SERAP09T)
  }
  
  # SERAP10P
  if("SERAP10P" %in% variables_names){
    data$SERAP10P <- as.numeric(levels(data$SERAP10P))[data$SERAP10P]
    data$SERAP10P[data$SERAP10P==1] <- "Sim"
    data$SERAP10P[data$SERAP10P==0] <- "Não"
    data$SERAP10P <- factor(data$SERAP10P)
  }
  
  # SERAP10T
  if("SERAP10T" %in% variables_names){
    data$SERAP10T <- as.numeric(levels(data$SERAP10T))[data$SERAP10T]
    data$SERAP10T[data$SERAP10T==1] <- "Sim"
    data$SERAP10T[data$SERAP10T==0] <- "Não"
    data$SERAP10T <- factor(data$SERAP10T)
  }
  
  # SERAP11P
  if("SERAP11P" %in% variables_names){
    data$SERAP11P <- as.numeric(levels(data$SERAP11P))[data$SERAP11P]
    data$SERAP11P[data$SERAP11P==1] <- "Sim"
    data$SERAP11P[data$SERAP11P==0] <- "Não"
    data$SERAP11P <- factor(data$SERAP11P)
  }
  
  # SERAP11T
  if("SERAP11T" %in% variables_names){
    data$SERAP11T <- as.numeric(levels(data$SERAP11T))[data$SERAP11T]
    data$SERAP11T[data$SERAP11T==1] <- "Sim"
    data$SERAP11T[data$SERAP11T==0] <- "Não"
    data$SERAP11T <- factor(data$SERAP11T)
  }
  
  # SERAPOIO
  if("SERAPOIO" %in% variables_names){
    data$SERAPOIO <- as.numeric(levels(data$SERAPOIO))[data$SERAPOIO]
    data$SERAPOIO[data$SERAPOIO==1] <- "Sim"
    data$SERAPOIO[data$SERAPOIO==0] <- "Não"
    data$SERAPOIO <- factor(data$SERAPOIO)
  }
  
  # RES_BIOL
  if("RES_BIOL" %in% variables_names){
    data$RES_BIOL <- as.numeric(levels(data$RES_BIOL))[data$RES_BIOL]
    data$RES_BIOL[data$RES_BIOL==1] <- "Sim"
    data$RES_BIOL[data$RES_BIOL==0] <- "Não"
    data$RES_BIOL <- factor(data$RES_BIOL)
  }
  
  # RES_QUIM
  if("RES_QUIM" %in% variables_names){
    data$RES_QUIM <- as.numeric(levels(data$RES_QUIM))[data$RES_QUIM]
    data$RES_QUIM[data$RES_QUIM==1] <- "Sim"
    data$RES_QUIM[data$RES_QUIM==0] <- "Não"
    data$RES_QUIM <- factor(data$RES_QUIM)
  }
  
  # RES_RADI
  if("RES_RADI" %in% variables_names){
    data$RES_RADI <- as.numeric(levels(data$RES_RADI))[data$RES_RADI]
    data$RES_RADI[data$RES_RADI==1] <- "Sim"
    data$RES_RADI[data$RES_RADI==0] <- "Não"
    data$RES_RADI <- factor(data$RES_RADI)
  }
  
  # RES_COMU
  if("RES_COMU" %in% variables_names){
    data$RES_COMU <- as.numeric(levels(data$RES_COMU))[data$RES_COMU]
    data$RES_COMU[data$RES_COMU==1] <- "Sim"
    data$RES_COMU[data$RES_COMU==0] <- "Não"
    data$RES_COMU <- factor(data$RES_COMU)
  }
  
  # COLETRES
  if("COLETRES" %in% variables_names){
    data$COLETRES <- as.numeric(levels(data$COLETRES))[data$COLETRES]
    data$COLETRES[data$COLETRES==1] <- "Sim"
    data$COLETRES[data$COLETRES==0] <- "Não"
    data$COLETRES <- factor(data$COLETRES)
  }
  
  # COMISS01
  if("COMISS01" %in% variables_names){
    data$COMISS01 <- as.numeric(levels(data$COMISS01))[data$COMISS01]
    data$COMISS01[data$COMISS01==1] <- "Sim"
    data$COMISS01[data$COMISS01==0] <- "Não"
    data$COMISS01 <- factor(data$COMISS01)
  }
  
  # COMISS02
  if("COMISS02" %in% variables_names){
    data$COMISS02 <- as.numeric(levels(data$COMISS02))[data$COMISS02]
    data$COMISS02[data$COMISS02==1] <- "Sim"
    data$COMISS02[data$COMISS02==0] <- "Não"
    data$COMISS02 <- factor(data$COMISS02)
  }
  
  # COMISS03
  if("COMISS03" %in% variables_names){
    data$COMISS03 <- as.numeric(levels(data$COMISS03))[data$COMISS03]
    data$COMISS03[data$COMISS03==1] <- "Sim"
    data$COMISS03[data$COMISS03==0] <- "Não"
    data$COMISS03 <- factor(data$COMISS03)
  }
  
  # COMISS04
  if("COMISS04" %in% variables_names){
    data$COMISS04 <- as.numeric(levels(data$COMISS04))[data$COMISS04]
    data$COMISS04[data$COMISS04==1] <- "Sim"
    data$COMISS04[data$COMISS04==0] <- "Não"
    data$COMISS04 <- factor(data$COMISS04)
  }
  
  # COMISS05
  if("COMISS05" %in% variables_names){
    data$COMISS05 <- as.numeric(levels(data$COMISS05))[data$COMISS05]
    data$COMISS05[data$COMISS05==1] <- "Sim"
    data$COMISS05[data$COMISS05==0] <- "Não"
    data$COMISS05 <- factor(data$COMISS05)
  }
  
  # COMISS06
  if("COMISS06" %in% variables_names){
    data$COMISS06 <- as.numeric(levels(data$COMISS06))[data$COMISS06]
    data$COMISS06[data$COMISS06==1] <- "Sim"
    data$COMISS06[data$COMISS06==0] <- "Não"
    data$COMISS06 <- factor(data$COMISS06)
  }
  
  # COMISS07
  if("COMISS07" %in% variables_names){
    data$COMISS07 <- as.numeric(levels(data$COMISS07))[data$COMISS07]
    data$COMISS07[data$COMISS07==1] <- "Sim"
    data$COMISS07[data$COMISS07==0] <- "Não"
    data$COMISS07 <- factor(data$COMISS07)
  }
  
  # COMISS08
  if("COMISS08" %in% variables_names){
    data$COMISS08 <- as.numeric(levels(data$COMISS08))[data$COMISS08]
    data$COMISS08[data$COMISS08==1] <- "Sim"
    data$COMISS08[data$COMISS08==0] <- "Não"
    data$COMISS08 <- factor(data$COMISS08)
  }
  
  # COMISS09
  if("COMISS09" %in% variables_names){
    data$COMISS09 <- as.numeric(levels(data$COMISS09))[data$COMISS09]
    data$COMISS09[data$COMISS09==1] <- "Sim"
    data$COMISS09[data$COMISS09==0] <- "Não"
    data$COMISS09 <- factor(data$COMISS09)
  }
  
  # COMISS10
  if("COMISS10" %in% variables_names){
    data$COMISS10 <- as.numeric(levels(data$COMISS10))[data$COMISS10]
    data$COMISS10[data$COMISS10==1] <- "Sim"
    data$COMISS10[data$COMISS10==0] <- "Não"
    data$COMISS10 <- factor(data$COMISS10)
  }
  
  # COMISS11
  if("COMISS11" %in% variables_names){
    data$COMISS11 <- as.numeric(levels(data$COMISS11))[data$COMISS11]
    data$COMISS11[data$COMISS11==1] <- "Sim"
    data$COMISS11[data$COMISS11==0] <- "Não"
    data$COMISS11 <- factor(data$COMISS11)
  }
  
  # COMISS12
  if("COMISS12" %in% variables_names){
    data$COMISS12 <- as.numeric(levels(data$COMISS12))[data$COMISS12]
    data$COMISS12[data$COMISS12==1] <- "Sim"
    data$COMISS12[data$COMISS12==0] <- "Não"
    data$COMISS12 <- factor(data$COMISS12)
  }
  
  # COMISSAO
  if("COMISSAO" %in% variables_names){
    data$COMISSAO <- as.numeric(levels(data$COMISSAO))[data$COMISSAO]
    data$COMISSAO[data$COMISSAO==1] <- "Sim"
    data$COMISSAO[data$COMISSAO==0] <- "Não"
    data$COMISSAO <- factor(data$COMISSAO)
  }
  
  # AP01CV01
  if("AP01CV01" %in% variables_names){
    data$AP01CV01 <- as.numeric(levels(data$AP01CV01))[data$AP01CV01]
    data$AP01CV01[data$AP01CV01==1] <- "Sim"
    data$AP01CV01[data$AP01CV01==0] <- "Não"
    data$AP01CV01 <- factor(data$AP01CV01)
  }
  
  # AP01CV02
  if("AP01CV02" %in% variables_names){
    data$AP01CV02 <- as.numeric(levels(data$AP01CV02))[data$AP01CV02]
    data$AP01CV02[data$AP01CV02==1] <- "Sim"
    data$AP01CV02[data$AP01CV02==0] <- "Não"
    data$AP01CV02 <- factor(data$AP01CV02)
  }
  
  # AP01CV05
  if("AP01CV05" %in% variables_names){
    data$AP01CV05 <- as.numeric(levels(data$AP01CV05))[data$AP01CV05]
    data$AP01CV05[data$AP01CV05==1] <- "Sim"
    data$AP01CV05[data$AP01CV05==0] <- "Não"
    data$AP01CV05 <- factor(data$AP01CV05)
  }
  
  # AP01CV06
  if("AP01CV06" %in% variables_names){
    data$AP01CV06 <- as.numeric(levels(data$AP01CV06))[data$AP01CV06]
    data$AP01CV06[data$AP01CV06==1] <- "Sim"
    data$AP01CV06[data$AP01CV06==0] <- "Não"
    data$AP01CV06 <- factor(data$AP01CV06)
  }
  
  # AP01CV03
  if("AP01CV03" %in% variables_names){
    data$AP01CV03 <- as.numeric(levels(data$AP01CV03))[data$AP01CV03]
    data$AP01CV03[data$AP01CV03==1] <- "Sim"
    data$AP01CV03[data$AP01CV03==0] <- "Não"
    data$AP01CV03 <- factor(data$AP01CV03)
  }
  
  # AP01CV04
  if("AP01CV04" %in% variables_names){
    data$AP01CV04 <- as.numeric(levels(data$AP01CV04))[data$AP01CV04]
    data$AP01CV04[data$AP01CV04==1] <- "Sim"
    data$AP01CV04[data$AP01CV04==0] <- "Não"
    data$AP01CV04 <- factor(data$AP01CV04)
  }
  
  # AP02CV01
  if("AP02CV01" %in% variables_names){
    data$AP02CV01 <- as.numeric(levels(data$AP02CV01))[data$AP02CV01]
    data$AP02CV01[data$AP02CV01==1] <- "Sim"
    data$AP02CV01[data$AP02CV01==0] <- "Não"
    data$AP02CV01 <- factor(data$AP02CV01)
  }
  
  # AP02CV02
  if("AP02CV02" %in% variables_names){
    data$AP02CV02 <- as.numeric(levels(data$AP02CV02))[data$AP02CV02]
    data$AP02CV02[data$AP02CV02==1] <- "Sim"
    data$AP02CV02[data$AP02CV02==0] <- "Não"
    data$AP02CV02 <- factor(data$AP02CV02)
  }
  
  # AP02CV05
  if("AP02CV05" %in% variables_names){
    data$AP02CV05 <- as.numeric(levels(data$AP02CV05))[data$AP02CV05]
    data$AP02CV05[data$AP02CV05==1] <- "Sim"
    data$AP02CV05[data$AP02CV05==0] <- "Não"
    data$AP02CV05 <- factor(data$AP02CV05)
  }
  
  # AP02CV06
  if("AP02CV06" %in% variables_names){
    data$AP02CV06 <- as.numeric(levels(data$AP02CV06))[data$AP02CV06]
    data$AP02CV06[data$AP02CV06==1] <- "Sim"
    data$AP02CV06[data$AP02CV06==0] <- "Não"
    data$AP02CV06 <- factor(data$AP02CV06)
  }
  
  # AP02CV03
  if("AP02CV03" %in% variables_names){
    data$AP02CV03 <- as.numeric(levels(data$AP02CV03))[data$AP02CV03]
    data$AP02CV03[data$AP02CV03==1] <- "Sim"
    data$AP02CV03[data$AP02CV03==0] <- "Não"
    data$AP02CV03 <- factor(data$AP02CV03)
  }
  
  # AP02CV04
  if("AP02CV04" %in% variables_names){
    data$AP02CV04 <- as.numeric(levels(data$AP02CV04))[data$AP02CV04]
    data$AP02CV04[data$AP02CV04==1] <- "Sim"
    data$AP02CV04[data$AP02CV04==0] <- "Não"
    data$AP02CV04 <- factor(data$AP02CV04)
  }
  
  # AP03CV01
  if("AP03CV01" %in% variables_names){
    data$AP03CV01 <- as.numeric(levels(data$AP03CV01))[data$AP03CV01]
    data$AP03CV01[data$AP03CV01==1] <- "Sim"
    data$AP03CV01[data$AP03CV01==0] <- "Não"
    data$AP03CV01 <- factor(data$AP03CV01)
  }
  
  # AP03CV02
  if("AP03CV02" %in% variables_names){
    data$AP03CV02 <- as.numeric(levels(data$AP03CV02))[data$AP03CV02]
    data$AP03CV02[data$AP03CV02==1] <- "Sim"
    data$AP03CV02[data$AP03CV02==0] <- "Não"
    data$AP03CV02 <- factor(data$AP03CV02)
  }
  
  # AP03CV05
  if("AP03CV05" %in% variables_names){
    data$AP03CV05 <- as.numeric(levels(data$AP03CV05))[data$AP03CV05]
    data$AP03CV05[data$AP03CV05==1] <- "Sim"
    data$AP03CV05[data$AP03CV05==0] <- "Não"
    data$AP03CV05 <- factor(data$AP03CV05)
  }
  
  # AP03CV06
  if("AP03CV06" %in% variables_names){
    data$AP03CV06 <- as.numeric(levels(data$AP03CV06))[data$AP03CV06]
    data$AP03CV06[data$AP03CV06==1] <- "Sim"
    data$AP03CV06[data$AP03CV06==0] <- "Não"
    data$AP03CV06 <- factor(data$AP03CV06)
  }
  
  # AP03CV03
  if("AP03CV03" %in% variables_names){
    data$AP03CV03 <- as.numeric(levels(data$AP03CV03))[data$AP03CV03]
    data$AP03CV03[data$AP03CV03==1] <- "Sim"
    data$AP03CV03[data$AP03CV03==0] <- "Não"
    data$AP03CV03 <- factor(data$AP03CV03)
  }
  
  # AP03CV04
  if("AP03CV04" %in% variables_names){
    data$AP03CV04 <- as.numeric(levels(data$AP03CV04))[data$AP03CV04]
    data$AP03CV04[data$AP03CV04==1] <- "Sim"
    data$AP03CV04[data$AP03CV04==0] <- "Não"
    data$AP03CV04 <- factor(data$AP03CV04)
  }
  
  # AP04CV01
  if("AP04CV01" %in% variables_names){
    data$AP04CV01 <- as.numeric(levels(data$AP04CV01))[data$AP04CV01]
    data$AP04CV01[data$AP04CV01==1] <- "Sim"
    data$AP04CV01[data$AP04CV01==0] <- "Não"
    data$AP04CV01 <- factor(data$AP04CV01)
  }
  
  # AP04CV02
  if("AP04CV02" %in% variables_names){
    data$AP04CV02 <- as.numeric(levels(data$AP04CV02))[data$AP04CV02]
    data$AP04CV02[data$AP04CV02==1] <- "Sim"
    data$AP04CV02[data$AP04CV02==0] <- "Não"
    data$AP04CV02 <- factor(data$AP04CV02)
  }
  
  
  
  
  
  
  
  # Purge levels
  data <- droplevels(data)
  
  # Return
  return(data)
}