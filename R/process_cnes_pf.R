#' Process CNES-PF variables from DataSUS
#' 
#' \code{process_cnes_pf} processes CNES-PF variables retrieved by \code{fetch_datasus()}.
#' 
#' This function processes CNES-PF (Pessoa Física) variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#' 
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#' 
#' @examples 
#' df <- fetch_datasus(year_start = 2015, month_start = 1,
#'                     year_end = 2015, month_end = 1,
#'                     uf = "RJ",
#'                     information_system = "CNES-PF")
#' df_a <- process_cnes_pf(df)
#' df_b <- process_cnes_pf(df, municipality_data = FALSE)

process_cnes_pf <- function(data, municipality_data = TRUE) {
  # Variables names
  variables_names <- names(data)
  
  # CNES
  if("CNES" %in% variables_names){
    data$CNES <- as.integer(as.character(levels(data$CNES))[data$CNES])
  }
  
  # UFMUNRES
  if ("UFMUNRES" %in% variables_names & municipality_data == TRUE) {
    data$UFMUNRES <- as.integer(as.character(data$UFMUNRES))
    colnames(tabMun)[1] <- "UFMUNRES"
    data <- dplyr::left_join(data, tabMun, by = "UFMUNRES")
  } else {
    data$UFMUNRES <- as.integer(as.character(data$UFMUNRES))
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
    data$TPGESTAO[data$TPGESTAO=="Z"] <- "Sem gestão"
    data$TPGESTAO[data$TPGESTAO=="S"] <- "Sem gestão"
    data$TPGESTAO <- factor(data$TPGESTAO)
  }
  
  # PF_PJ
  if("PF_PJ" %in% variables_names){
    data$PF_PJ <- as.numeric(levels(data$PF_PJ))[data$PF_PJ]
    data$PF_PJ[data$PF_PJ==1] <- "Pessoa física"
    data$PF_PJ[data$PF_PJ==3] <- "Pessoa jurídica"
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
    data$ATIVIDAD[data$ATIVIDAD==1] <- "Unidade Universitária"
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
    data$RETENCAO[data$RETENCAO==10] <- "Estabelecimento público"
    data$RETENCAO[data$RETENCAO==11] <- "Estabelecimento filantrópico"
    data$RETENCAO[data$RETENCAO==12] <- "Estabelecimento sem fins lucrativos"
    data$RETENCAO[data$RETENCAO==13] <- "Estabelecimento privado luvrativa simples"
    data$RETENCAO[data$RETENCAO==14] <- "Estabelecimento privado luvrativa"
    data$RETENCAO[data$RETENCAO==15] <- "Estabelecimento sindical"
    data$RETENCAO[data$RETENCAO==16] <- "Estabelecimento pessoa física"
    data$RETENCAO <- factor(data$RETENCAO)
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
  
  # TERCEIRO
  if("TERCEIRO" %in% variables_names){
    data$TERCEIRO <- as.numeric(levels(data$TERCEIRO))[data$TERCEIRO]
    data$TERCEIRO[data$TERCEIRO==1] <- "Sim"
    data$TERCEIRO[data$TERCEIRO==0] <- "Não"
    data$TERCEIRO[data$TERCEIRO==2] <- "Não"
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
    data <- dplyr::left_join(data, cbo02, by = c("CBO" = "COD"))
  }
  
  # NOMEPROF
  if("NOMEPROF" %in% variables_names){
    data$NOMEPROF <- as.character(levels(data$NOMEPROF))[data$NOMEPROF]
  }
  
  # VINCULAC
  if("VINCULAC" %in% variables_names){
    data$VINCULAC <- as.character(levels(data$VINCULAC))[data$VINCULAC]
    data <- dplyr::left_join(data, vinculo, by = c("VINCULAC" = "CO_VINC"))
  }
  
  # NAT_JUR
  if("NAT_JUR" %in% variables_names){
    data$NAT_JUR <- as.numeric(levels(data$NAT_JUR))[data$NAT_JUR]
    data$NAT_JUR[data$NAT_JUR==0] <- NA
    data$NAT_JUR[data$NAT_JUR==1000] <- "Administração Pública"
    data$NAT_JUR[data$NAT_JUR==1015] <- "Órgão Público do Poder Executivo Federal"
    data$NAT_JUR[data$NAT_JUR==1023] <- "Órgão Público do Poder Executivo Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1031] <- "Órgão Público do Poder Executivo Municipal"
    data$NAT_JUR[data$NAT_JUR==1040] <- "Órgão Público do Poder Legislativo Federal"
    data$NAT_JUR[data$NAT_JUR==1058] <- "Órgão Público do Poder Legislativo Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1066] <- "Órgão Público do Poder Legislativo Municipal"
    data$NAT_JUR[data$NAT_JUR==1074] <- "Órgão Público do Poder Judiciário Federal"
    data$NAT_JUR[data$NAT_JUR==1082] <- "Órgão Público do Poder Judiciário Estadual"
    data$NAT_JUR[data$NAT_JUR==1104] <- "Autarquia Federal"
    data$NAT_JUR[data$NAT_JUR==1112] <- "Autarquia Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1120] <- "Autarquia Municipal"
    data$NAT_JUR[data$NAT_JUR==1139] <- "Fundação Pública de Direito Público Federal"
    data$NAT_JUR[data$NAT_JUR==1147] <- "Fundação Pública de Direito Público Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1155] <- "Fundação Pública de Direito Público Municipal"
    data$NAT_JUR[data$NAT_JUR==1163] <- "Órgão Público Autônomo Federal"
    data$NAT_JUR[data$NAT_JUR==1171] <- "Órgão Público Autônomo Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1180] <- "Órgão Público Autônomo Municipal"
    data$NAT_JUR[data$NAT_JUR==1198] <- "Comissão Polinacional"
    data$NAT_JUR[data$NAT_JUR==1201] <- "Fundo Público"
    data$NAT_JUR[data$NAT_JUR==1210] <- "Consórcio Público de Direito Público (Associação Pública)"
    data$NAT_JUR[data$NAT_JUR==1228] <- "Consórcio Público de Direito Privado"
    data$NAT_JUR[data$NAT_JUR==1236] <- "Estado ou Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1244] <- "Município"
    data$NAT_JUR[data$NAT_JUR==1252] <- "Fundação Pública de Direito Privado Federal"
    data$NAT_JUR[data$NAT_JUR==1260] <- "Fundação Pública de Direito Privado Estadual ou do Distrito Federal"
    data$NAT_JUR[data$NAT_JUR==1279] <- "Fundação Pública de Direito Privado Municipal"
    data$NAT_JUR[data$NAT_JUR==2000] <- "Entidades Empresariais"
    data$NAT_JUR[data$NAT_JUR==2001] <- "Empresa Pública"
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
    data$NAT_JUR[data$NAT_JUR==2178] <- "Estabelecimento, no Brasil, de Sociedade Estrangeira"
    data$NAT_JUR[data$NAT_JUR==2194] <- "Estabelecimento, no Brasil, de Empresa Binacional Argentino-Brasileira"
    data$NAT_JUR[data$NAT_JUR==2216] <- "Empresa Domiciliada no Exterior"
    data$NAT_JUR[data$NAT_JUR==2224] <- "Clube/Fundo de Investimento"
    data$NAT_JUR[data$NAT_JUR==2232] <- "Sociedade Simples Pura"
    data$NAT_JUR[data$NAT_JUR==2240] <- "Sociedade Simples Limitada"
    data$NAT_JUR[data$NAT_JUR==2259] <- "Sociedade Simples em Nome Coletivo"
    data$NAT_JUR[data$NAT_JUR==2267] <- "Sociedade Simples em Comandita Simples"
    data$NAT_JUR[data$NAT_JUR==2275] <- "Empresa Binacional"
    data$NAT_JUR[data$NAT_JUR==2283] <- "Consórcio de Empregadores"
    data$NAT_JUR[data$NAT_JUR==2291] <- "Consórcio Simples"
    data$NAT_JUR[data$NAT_JUR==2305] <- "Empresa Individual de Responsabilidade Limitada (de Natureza Empresária)"
    data$NAT_JUR[data$NAT_JUR==2313] <- "Empresa Individual de Responsabilidade Limitada (de Natureza Simples)"
    data$NAT_JUR[data$NAT_JUR==2321] <- "Sociedade Unipessoal de Advogados"
    data$NAT_JUR[data$NAT_JUR==2330] <- "Cooperativas de Consumo"
    data$NAT_JUR[data$NAT_JUR==3000] <- "Entidades sem Fins Lucrativos"
    data$NAT_JUR[data$NAT_JUR==3034] <- "Serviço Notarial e Registral (Cartório)"
    data$NAT_JUR[data$NAT_JUR==3069] <- "Fundação Privada"
    data$NAT_JUR[data$NAT_JUR==3077] <- "Serviço Social Autônomo"
    data$NAT_JUR[data$NAT_JUR==3085] <- "Condomínio Edilício"
    data$NAT_JUR[data$NAT_JUR==3107] <- "Comissão de Conciliação Prévia"
    data$NAT_JUR[data$NAT_JUR==3115] <- "Entidade de Mediação e Arbitragem"
    data$NAT_JUR[data$NAT_JUR==3131] <- "Entidade Sindical"
    data$NAT_JUR[data$NAT_JUR==3204] <- "Estabelecimento, no Brasil, de Fundação ou Associação Estrangeiras"
    data$NAT_JUR[data$NAT_JUR==3212] <- "Fundação ou Associação Domiciliada no Exterior"
    data$NAT_JUR[data$NAT_JUR==3220] <- "Organização Religiosa"
    data$NAT_JUR[data$NAT_JUR==3239] <- "Comunidade Indígena"
    data$NAT_JUR[data$NAT_JUR==3247] <- "Fundo Privado"
    data$NAT_JUR[data$NAT_JUR==3255] <- "Órgão de Direção Nacional de Partido Político"
    data$NAT_JUR[data$NAT_JUR==3263] <- "Órgão de Direção Regional de Partido Político"
    data$NAT_JUR[data$NAT_JUR==3271] <- "Órgão de Direção Local de Partido Político"
    data$NAT_JUR[data$NAT_JUR==3280] <- "Comitê Financeiro de Partido Político"
    data$NAT_JUR[data$NAT_JUR==3298] <- "Frente Plebiscitária ou Referendária"
    data$NAT_JUR[data$NAT_JUR==3306] <- "Organização Social (OS)"
    data$NAT_JUR[data$NAT_JUR==3310] <- "Demais Condomínios"
    data$NAT_JUR[data$NAT_JUR==3999] <- "Associação Privada"
    data$NAT_JUR[data$NAT_JUR==4000] <- "Pessoas Físicas"
    data$NAT_JUR[data$NAT_JUR==4014] <- "Empresa Individual Imobiliária"
    data$NAT_JUR[data$NAT_JUR==4022] <- "Segurado Especial"
    data$NAT_JUR[data$NAT_JUR==4081] <- "Contribuinte individual"
    data$NAT_JUR[data$NAT_JUR==4090] <- "Candidato a Cargo Político Eletivo"
    data$NAT_JUR[data$NAT_JUR==4111] <- "Leiloeiro"
    data$NAT_JUR[data$NAT_JUR==4124] <- "Produtor Rural (Pessoa Física)"
    data$NAT_JUR[data$NAT_JUR==5000] <- "Organizações Internacionais e Outras Instituições Extraterritoriais"
    data$NAT_JUR[data$NAT_JUR==5010] <- "Organização Internacional"
    data$NAT_JUR[data$NAT_JUR==5029] <- "Representação Diplomática Estrangeira"
    data$NAT_JUR[data$NAT_JUR==5037] <- "Outras Instituições Extraterritoriais"
    data$NAT_JUR <- factor(data$NAT_JUR)
  }
  
  
  # Purge levels
  data <- droplevels(data)
  
  # Return
  return(data)
}