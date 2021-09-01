#' Process SIA variables from DataSUS
#'
#' \code{process_sia} processes SIA variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes SIA variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' Currently, only "SIA-PA" is supported.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param information_system string. The abbreviation of the health information system. See \emph{Details}.
#' @param nome_proced optional logical. \code{TRUE} by default, add  \code{PA_PROCED_NOME} to the dataset.
#' @param nome_ocupacao optional logical. \code{TRUE} by default, add  \code{OCUPACAO} name to the dataset.
#' @param nome_equipe optional logical. \code{TRUE} by default, add  \code{EQUIPE} name to the dataset.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples \dontrun{
#' df <- fetch_datasus(year_start = 2010, month_start = 1,
#'                     year_end = 2010, month_end = 1,
#'                     uf = "RJ",
#'                     information_system = "SIA-PA")
#' df_a <- process_sia(df)
#' df_b <- process_sia(df, municipality_data = FALSE)
#' }

process_sia <- function(data, information_system = "SIA-PA", nome_proced = TRUE, nome_ocupacao = TRUE, nome_equipe = TRUE, municipality_data = TRUE) {
  # Check information system
  available_information_system <- "SIA-PA"
  if(!(information_system %in% available_information_system)) stop("Health informaton system unknown.")

  # Variables names
  variables_names <- names(data)

  if(information_system == "SIA-PA"){

    # PA_CODUNI
    if("PA_CODUNI" %in% variables_names){
      data$PA_CODUNI <- as.character(data$PA_CODUNI)
    }

    # PA_GESTAO
    if("PA_GESTAO" %in% variables_names){
      data$PA_GESTAO <- as.character(data$PA_GESTAO)
    }

    # PA_CONDIC
    if("PA_CONDIC" %in% variables_names){
      data$PA_CONDIC <- as.character(data$PA_CONDIC)
    }

    # PA_UFMUN
    if("PA_UFMUN" %in% variables_names){
      data$PA_UFMUN <- as.character(data$PA_UFMUN)
    }

    # PA_REGCT
    if("PA_REGCT" %in% variables_names){
      data$PA_REGCT <- as.character(data$PA_REGCT)
      data$PA_REGCT[data$PA_REGCT=="7100"] <- "TAB.DE NÃO GERAÇÃO CRÉDITO P/PROD.INTERN./AMBULAT."
      data$PA_REGCT[data$PA_REGCT=="7101"] <- "ESTAB.S/CRÉDITO NA MEDIA COMPLEXIDADE AMBULATORIAL"
      data$PA_REGCT[data$PA_REGCT=="7102"] <- "ESTAB.S/CRÉDITO NA MEDIA COMPLEXIDADE HOSPITALAR"
      data$PA_REGCT[data$PA_REGCT=="7103"] <- "ESTAB.S/CRÉDITO NA ALTA COMPLEXIDADE AMBULATORIAL"
      data$PA_REGCT[data$PA_REGCT=="7104"] <- "ESTAB.S/CRÉDITO NA ALTA COMPLEXIDADE HOSPITALAR"
      data$PA_REGCT[data$PA_REGCT=="7105"] <- "ESTAB.S/CRED.PROCED.FINANC.FD.AÇÕES ESTRAT/COMPENS."
      data$PA_REGCT[data$PA_REGCT=="7106"] <- "ESTABELECIM. DE SAÚDE SEM GERAÇÃO DE CRÉDITO TOTAL"
      data$PA_REGCT[data$PA_REGCT=="7107"] <- "ESTAB.S/CRÉDITO ACOES ESPEC.ODONT(INC.CEO I,II,III)"
      data$PA_REGCT[data$PA_REGCT=="7108"] <- "ESTAB.S/GER.CRÉDITO(INCENTIVO SAUDE DO TRABALHADOR)"
      data$PA_REGCT[data$PA_REGCT=="7109"] <- "ESTAB.SAÚDE (HU/MEC) SEM GERAÇÃO DE CRÉDITO TOTAL"
      data$PA_REGCT[data$PA_REGCT=="7110"] <- "ESTAB.SAUDE (MIN.SAUDE)SEM GERAÇÃO DE CRÉDITO TOTAL"
      data$PA_REGCT[data$PA_REGCT=="7111"] <- "ESTAB.SAUDE SEM GERAÇÃO DE CRÉDITO-NASF,exceto FAEC"
      data$PA_REGCT[data$PA_REGCT=="7112"] <- "ESTAB.S/CRED.TOTAL-INCL.FAEC-EXCLUSIVO P/REDE SARAH"
      data$PA_REGCT[data$PA_REGCT=="7113"] <- "ESTAB.S/CRED.TOTAL,INCL.FAEC-OUTROS ESTAB. FEDERAIS"
      data$PA_REGCT[data$PA_REGCT=="7116"] <- "ESTAB.SAÚDE S/GER DE CRÉDITO NA MÉDIA COMPLEX-LRPD"
      data$PA_REGCT[data$PA_REGCT=="7117"] <- "ESTAB.SAÚDE S/GER DE CRÉD. MÉD COMP(EXCETO OPM)-CER"
      data$PA_REGCT[data$PA_REGCT=="0000"] <- "SEM REGRA CONTRATUAL"
    }

    # PA_INCOUT
    if("PA_INCOUT" %in% variables_names){
      data$PA_INCOUT <- as.character(data$PA_INCOUT)
      data$PA_INCOUT[data$PA_INCOUT!="0000"] <- "Com incremento"
      data$PA_INCOUT[data$PA_INCOUT=="0000"] <- "Sem incremento"
    }

    # PA_INCURG
    if("PA_INCURG" %in% variables_names){
      data$PA_INCURG <- as.character(data$PA_INCURG)
      data$PA_INCURG[data$PA_INCURG!="0000"] <- "Com incremento"
      data$PA_INCURG[data$PA_INCURG=="0000"] <- "Sem incremento"
    }

    # PA_TPUPS
    if("PA_TPUPS" %in% variables_names){
      data$PA_TPUPS <- as.character(data$PA_TPUPS)
      data$PA_TPUPS[data$PA_TPUPS=="74"] <- "ACADEMIA DA SAÚDE"
      data$PA_TPUPS[data$PA_TPUPS=="81"] <- "CENTRAL DE REGULAÇÃO"
      data$PA_TPUPS[data$PA_TPUPS=="76"] <- "CENTRAL DE REGULAÇÃO MÉDICA DAS URGÊNCIAS"
      data$PA_TPUPS[data$PA_TPUPS=="71"] <- "CENTRO DE APOIO A SAÚDE DA FAMÍLIA-CASF"
      data$PA_TPUPS[data$PA_TPUPS=="69"] <- "CENTRO DE ATENÇÃO HEMOTERÁPICA E/OU HEMATOLÓGICA"
      data$PA_TPUPS[data$PA_TPUPS=="70"] <- "CENTRO DE ATENÇÃO PSICOSSOCIAL-CAPS"
      data$PA_TPUPS[data$PA_TPUPS=="61"] <- "CENTRO DE PARTO NORMAL"
      data$PA_TPUPS[data$PA_TPUPS=="02"] <- "CENTRO DE SAUDE/UNIDADE BASICA DE SAUDE"
      data$PA_TPUPS[data$PA_TPUPS=="64"] <- "CENTRAL DE REGULACAO DE SERVICOS DE SAUDE"
      data$PA_TPUPS[data$PA_TPUPS=="36"] <- "CLINICA ESPECIALIZADA/AMBULATORIO ESPECIALIZADO"
      data$PA_TPUPS[data$PA_TPUPS=="22"] <- "CONSULTORIO"
      data$PA_TPUPS[data$PA_TPUPS=="60"] <- "COOPERATIVA"
      data$PA_TPUPS[data$PA_TPUPS=="43"] <- "FARMACIA"
      data$PA_TPUPS[data$PA_TPUPS=="07"] <- "HOSPITAL ESPECIALIZADO"
      data$PA_TPUPS[data$PA_TPUPS=="05"] <- "HOSPITAL GERAL"
      data$PA_TPUPS[data$PA_TPUPS=="62"] <- "HOSPITAL DIA"
      data$PA_TPUPS[data$PA_TPUPS=="67"] <- "LABORATORIO CENTRAL DE SAUDE PUBLICA - LACEN"
      data$PA_TPUPS[data$PA_TPUPS=="80"] <- "ORIO DE SAUDE PUBLICA"
      data$PA_TPUPS[data$PA_TPUPS=="04"] <- "POLICLINICA"
      data$PA_TPUPS[data$PA_TPUPS=="79"] <- "OFICINA ORTOPEDICA"
      data$PA_TPUPS[data$PA_TPUPS=="01"] <- "POSTO DE SAUDE"
      data$PA_TPUPS[data$PA_TPUPS=="73"] <- "PRONTO ANTEDIMENTO"
      data$PA_TPUPS[data$PA_TPUPS=="21"] <- "PRONTO SOCORRO ESPECIALIZADO"
      data$PA_TPUPS[data$PA_TPUPS=="20"] <- "PRONTO SOCORRO GERAL"
      data$PA_TPUPS[data$PA_TPUPS=="68"] <- "SECRETARIA DE SAUDE"
      data$PA_TPUPS[data$PA_TPUPS=="77"] <- "SERVICO DE ATENCAO DOMICILIAR ISOLADO(HOME CARE)"
      data$PA_TPUPS[data$PA_TPUPS=="63"] <- "UNIDADE AUTORIZADORA"
      data$PA_TPUPS[data$PA_TPUPS=="72"] <- "UNIDADE DE ATENÇÃO À SAÚDE INDÍGENA"
      data$PA_TPUPS[data$PA_TPUPS=="78"] <- "UNIDADE DE ATENCAO EM REGIME RESIDENCIAL"
      data$PA_TPUPS[data$PA_TPUPS=="39"] <- "UNIDADE DE SERVICO DE APOIO DE DIAGNOSE E TERAPIA"
      data$PA_TPUPS[data$PA_TPUPS=="45"] <- "UNIDADE DE SAUDE DA FAMILIA"
      data$PA_TPUPS[data$PA_TPUPS=="50"] <- "UNIDADE DE VIGILANCIA EM SAUDE"
      data$PA_TPUPS[data$PA_TPUPS=="65"] <- "UNIDADE DE VIGILANCIA EPIDEMIOLOGIA (ANTIGO)"
      data$PA_TPUPS[data$PA_TPUPS=="66"] <- "UNIDADE DE VIGILANCIA SANITARIA (ANTIGO)"
      data$PA_TPUPS[data$PA_TPUPS=="15"] <- "UNIDADE MISTA"
      data$PA_TPUPS[data$PA_TPUPS=="42"] <- "UNIDADE MOVEL DE NIVEL PRE-HOSP-URGENCIA/EMERGENCIA"
      data$PA_TPUPS[data$PA_TPUPS=="32"] <- "UNIDADE MOVEL FLUVIAL"
      data$PA_TPUPS[data$PA_TPUPS=="40"] <- "UNIDADE MOVEL TERRESTRE"
      data$PA_TPUPS[data$PA_TPUPS=="75"] <- "TELESAÚDE"
      data$PA_TPUPS[data$PA_TPUPS=="09"] <- "PRONTO SOCORRO DE HOSPITAL GERAL (ANTIGO)"
      data$PA_TPUPS[data$PA_TPUPS=="12"] <- "PRONTO SOCORRO TRAUMATO-ORTOPEDICO (ANTIGO)"
      data$PA_TPUPS <- factor(data$PA_TPUPS)
    }

    # PA_TIPPRE
    if("PA_TIPPRE" %in% variables_names){
      data$PA_TIPPRE <- as.character(data$PA_TIPPRE)
      data$PA_TIPPRE[data$PA_TIPPRE=="20"] <- "PRIVADO COM FINS LUCRATIVOS"
      data$PA_TIPPRE[data$PA_TIPPRE=="22"] <- "PRIVADO OPTANTE PELO SIMPLES"
      data$PA_TIPPRE[data$PA_TIPPRE=="30"] <- "PUBLICO FEDERAL"
      data$PA_TIPPRE[data$PA_TIPPRE=="40"] <- "PUBLICO ESTADUAL"
      data$PA_TIPPRE[data$PA_TIPPRE=="50"] <- "PUBLICO MUNICIPAL"
      data$PA_TIPPRE[data$PA_TIPPRE=="60"] <- "PRIVADO SEM FINS LUCRATIVOS"
      data$PA_TIPPRE[data$PA_TIPPRE=="61"] <- "FILANTROPICO COM CNAS VALIDO"
      data$PA_TIPPRE[data$PA_TIPPRE=="80"] <- "SINDICATO"
      data$PA_TIPPRE <- factor(data$PA_TIPPRE)
    }

    # PA_MN_IND
    if("PA_MN_IND" %in% variables_names){
      data$PA_MN_IND <- as.character(data$PA_MN_IND)
      data$PA_MN_IND[data$PA_MN_IND=="M"] <- "Mantida"
      data$PA_MN_IND[data$PA_MN_IND=="I"] <- "Individual"
      data$PA_MN_IND <- factor(data$PA_MN_IND)
    }

    # PA_CNPJCPF
    if("PA_CNPJCPF" %in% variables_names){
      data$PA_CNPJCPF <- as.character(data$PA_CNPJCPF)
    }

    # PA_CNPJMNT
    if("PA_CNPJMNT" %in% variables_names){
      data$PA_CNPJMNT <- as.character(data$PA_CNPJMNT)
    }

    # PA_CNPJ_CC
    if("PA_CNPJ_CC" %in% variables_names){
      data$PA_CNPJ_CC <- as.character(data$PA_CNPJ_CC)
    }

    # PA_MVM
    if("PA_MVM" %in% variables_names){
      data$PA_MVM <- as.character(data$PA_MVM)
    }

    # PA_CMP
    if("PA_CMP" %in% variables_names){
      data$PA_CMP <- as.character(data$PA_CMP)
    }

    # PA_PROC_ID
    if("PA_PROC_ID" %in% variables_names){
      data$PA_PROC_ID <- as.character(data$PA_PROC_ID)
    }

    # PA_PROC_NOME
    if(nome_proced == TRUE){
      data <- dplyr::left_join(data, sigtab, by = c("PA_PROC_ID" = "COD"))
    }

    # PA_TPFIN
    if("PA_TPFIN" %in% variables_names){
      data$PA_TPFIN <- as.character(data$PA_TPFIN)
      data$PA_TPFIN[data$PA_TPFIN=="01"] <- "Atenção Básica (PAB)"
      data$PA_TPFIN[data$PA_TPFIN=="02"] <- "Assistência Farmacêutica"
      data$PA_TPFIN[data$PA_TPFIN=="04"] <- "Fundo de Ações Estratégicas e Compensações FAEC"
      data$PA_TPFIN[data$PA_TPFIN=="05"] <- "Incentivo - MAC"
      data$PA_TPFIN[data$PA_TPFIN=="06"] <- "Média e Alta Complexidade (MAC)"
      data$PA_TPFIN[data$PA_TPFIN=="07"] <- "Vigilância em Saúde"
      data$PA_TPFIN <- factor(data$PA_TPFIN)
    }

    # PA_SUBFIN
    if("PA_SUBFIN" %in% variables_names){
      data$PA_SUBFIN <- as.character(data$PA_SUBFIN)
    }

    # PA_NIVCPL
    if("PA_NIVCPL" %in% variables_names){
      data$PA_NIVCPL <- as.character(data$PA_NIVCPL)
      data$PA_NIVCPL[data$PA_NIVCPL=="0"] <- "Não se Aplica"
      data$PA_NIVCPL[data$PA_NIVCPL=="1"] <- "Atenção Básica"
      data$PA_NIVCPL[data$PA_NIVCPL=="2"] <- "Média Complexidade"
      data$PA_NIVCPL[data$PA_NIVCPL=="2"] <- "Alta Complexidade"
      data$PA_NIVCPL <- factor(data$PA_NIVCPL)
    }

    # PA_DOCORIG
    if("PA_DOCORIG" %in% variables_names){
      data$PA_DOCORIG <- as.character(data$PA_DOCORIG)
      data$PA_DOCORIG[data$PA_DOCORIG=="C"] <- "BPA-C"
      data$PA_DOCORIG[data$PA_DOCORIG=="I"] <- "BPA-I"
      data$PA_DOCORIG[data$PA_DOCORIG=="P"] <- "APAC - Procedimento Principal"
      data$PA_DOCORIG[data$PA_DOCORIG=="S"] <- "APAC - Procedimento Secundário"
      data$PA_DOCORIG[data$PA_DOCORIG=="A"] <- "RAAS - Atenção Domiciliar"
      data$PA_DOCORIG[data$PA_DOCORIG=="B"] <- "RAAS - Psicossocial"
      data$PA_DOCORIG <- factor(data$PA_DOCORIG)
    }

    # PA_AUTORIZ
    if("PA_AUTORIZ" %in% variables_names){
      data$PA_AUTORIZ <- as.character(data$PA_AUTORIZ)
    }

    # PA_CNSMED
    if("PA_CNSMED" %in% variables_names){
      data$PA_CNSMED <- as.character(data$PA_CNSMED)
    }

    # PA_CBOCOD
    if("PA_CBOCOD" %in% variables_names){
      data$PA_CBOCOD <- as.character(data$PA_CBOCOD)
    }

    # Nome OCUPACAO
    if(nome_ocupacao == TRUE){
      data <- dplyr::left_join(data, cbo02, by = c("PA_CBOCOD" = "COD"))
    }

    # PA_MOTSAI
    if("PA_MOTSAI" %in% variables_names){
      data$PA_MOTSAI <- as.character(data$PA_MOTSAI)
      data$PA_MOTSAI[data$PA_MOTSAI=="11"] <- "ALTA CURADO"
      data$PA_MOTSAI[data$PA_MOTSAI=="12"] <- "ALTA MELHORADO"
      data$PA_MOTSAI[data$PA_MOTSAI=="13"] <- "ALTA DA PUÉRPERA E PERMANÊNCIA DO RECÉM NASCIDO"
      data$PA_MOTSAI[data$PA_MOTSAI=="14"] <- "ALTA A PEDIDO"
      data$PA_MOTSAI[data$PA_MOTSAI=="15"] <- "ALTA COM PREVISÃO DE RETORNO P/ ACOMPAN. DO PACIENT"
      data$PA_MOTSAI[data$PA_MOTSAI=="16"] <- "ALTA POR EVASÃO"
      data$PA_MOTSAI[data$PA_MOTSAI=="17"] <- "ALTA DA PUÉRPERA E RECÉM NASCIDO"
      data$PA_MOTSAI[data$PA_MOTSAI=="18"] <- "ALTA POR OUTROS MOTIVOS"
      data$PA_MOTSAI[data$PA_MOTSAI=="21"] <- "PERMANÊNCIA POR CARACTERÍSTICAS PRÓPRIAS DA DOENÇA"
      data$PA_MOTSAI[data$PA_MOTSAI=="22"] <- "PERMANÊNCIA POR INTERCORRÊNCIA"
      data$PA_MOTSAI[data$PA_MOTSAI=="23"] <- "PERMANÊNCIA POR IMPOSSIBILIDADE SÓCIO-FAMILIAR"
      data$PA_MOTSAI[data$PA_MOTSAI=="24"] <- "PERMAN. POR PROCESSO-DOAÇÃO DE ÓRGÃOS-DOADOR VIVO"
      data$PA_MOTSAI[data$PA_MOTSAI=="25"] <- "PERMAN. POR PROCESSO-DOAÇÃO DE ÓRGÃOS-DOADOR MORTO"
      data$PA_MOTSAI[data$PA_MOTSAI=="26"] <- "PERMANÊNCIA POR MUDANÇA DE PROCEDIMENTO"
      data$PA_MOTSAI[data$PA_MOTSAI=="27"] <- "PERMANÊNCIA POR REOPERAÇÃO"
      data$PA_MOTSAI[data$PA_MOTSAI=="28"] <- "PERMANÊNCIA POR OUTROS MOTIVOS"
      data$PA_MOTSAI[data$PA_MOTSAI=="31"] <- "TRANSFERIDO PARA OUTRO ESTABELECIMENTO"
      data$PA_MOTSAI[data$PA_MOTSAI=="41"] <- "ÓBITO COM DECLARAÇÃO DE ÓBITO FORNEC. MÉDICO ASSIST"
      data$PA_MOTSAI[data$PA_MOTSAI=="42"] <- "ÓBITO COM DECLARAÇÃO DE ÓBITO FORNECIDA PELO I.M.L"
      data$PA_MOTSAI[data$PA_MOTSAI=="43"] <- "ÓBITO COM DECLARAÇÃO DE ÓBITO FORNECIDA PELO I.M.L"
      data$PA_MOTSAI[data$PA_MOTSAI=="51"] <- "ENCERRAMENTO ADMINSTRATIVO"
      data$PA_MOTSAI[data$PA_MOTSAI=="00"] <- "PRODUÇÃO SEM MOTIVO DE SAÍDA (BPA-C / BPA-I)"
      data$PA_MOTSAI <- factor(data$PA_MOTSAI)
    }

    # PA_OBITO
    if("PA_OBITO" %in% variables_names){
      data$PA_OBITO <- as.character(data$PA_OBITO)
      data$PA_OBITO[data$PA_OBITO=="1"] <- "Sim (motivo de saída-ÓBITO)"
      data$PA_OBITO[data$PA_OBITO=="0"] <- "Nao houve ÓBITO"
      data$PA_OBITO <- factor(data$PA_OBITO)
    }

    # PA_ENCERR
    if("PA_ENCERR" %in% variables_names){
      data$PA_ENCERR <- as.character(data$PA_ENCERR)
      data$PA_ENCERR[data$PA_ENCERR=="1"] <- "Sim (motivo de saída-ENCERRAMENTO)"
      data$PA_ENCERR[data$PA_ENCERR=="0"] <- "Nao houve ENCERRAMENTO"
      data$PA_ENCERR <- factor(data$PA_ENCERR)
    }

    # PA_PERMAN
    if("PA_PERMAN" %in% variables_names){
      data$PA_PERMAN <- as.character(data$PA_PERMAN)
      data$PA_PERMAN[data$PA_PERMAN=="1"] <- "Sim (motivo de saída-PERMANÊNCIA)"
      data$PA_PERMAN[data$PA_PERMAN=="0"] <- "Nao houve a PERMANÊNCIA do paciente na unidade"
      data$PA_PERMAN <- factor(data$PA_PERMAN)
    }

    # PA_ALTA
    if("PA_ALTA" %in% variables_names){
      data$PA_ALTA <- as.character(data$PA_ALTA)
      data$PA_ALTA[data$PA_ALTA=="1"] <- "Sim (motivo de saída-PERMANÊNCIA)"
      data$PA_ALTA[data$PA_ALTA=="0"] <- "Nao houve a PERMANÊNCIA do paciente na unidade"
      data$PA_ALTA <- factor(data$PA_ALTA)
    }

    # PA_TRANSF
    if("PA_TRANSF" %in% variables_names){
      data$PA_TRANSF <- as.character(data$PA_TRANSF)
      data$PA_TRANSF[data$PA_TRANSF=="1"] <- "Sim (motivo de saída-TRANSFERÊNCIA)"
      data$PA_TRANSF[data$PA_TRANSF=="0"] <- "Nao houve TRANSFERÊNCIA do paciente"
      data$PA_TRANSF <- factor(data$PA_TRANSF)
    }

    # PA_CIDPRI
    if("PA_CIDPRI" %in% variables_names){
      data$PA_CIDPRI <- as.character(data$PA_CIDPRI)
    }

    # PA_CIDSEC
    if("PA_CIDSEC" %in% variables_names){
      data$PA_CIDSEC <- as.character(data$PA_CIDSEC)
    }

    # PA_CIDCAS
    if("PA_CIDCAS" %in% variables_names){
      data$PA_CIDCAS <- as.character(data$PA_CIDCAS)
    }

    # PA_CATEND
    if("PA_CATEND" %in% variables_names){
      data$PA_CATEND <- as.character(data$PA_CATEND)
      data$PA_CATEND[data$PA_CATEND=="01"] <- "ELETIVO"
      data$PA_CATEND[data$PA_CATEND=="02"] <- "URGÊNCIA"
      data$PA_CATEND[data$PA_CATEND=="03"] <- "ACIDENTE NO LOCAL TRABALHO OU A SERViÇO DA EMPRESA"
      data$PA_CATEND[data$PA_CATEND=="04"] <- "ACIDENTE NO TRAJETO PARA O TRABALHO"
      data$PA_CATEND[data$PA_CATEND=="05"] <- "OUTROS TIPOS DE ACIDENTE DE TRÂNSITO"
      data$PA_CATEND[data$PA_CATEND=="06"] <- "OUTROS TIPOS LESÕES/ENVENENAMENTOS(AGENT.FIS./QUIM."
      data$PA_CATEND[data$PA_CATEND=="99"] <- "INFORMAÇÃO INEXISTENTE  (BPA-C)"
      data$PA_CATEND[data$PA_CATEND=="00"] <- "CARATER DE ATENDIMENTO NÃO INFORMADO"
      data$PA_CATEND[data$PA_CATEND=="07"] <- "CARATER DE ATENDIMENTO INVALIDO"
      data$PA_CATEND[data$PA_CATEND=="10"] <- "CARATER DE ATENDIMENTO INVALIDO"
      data$PA_CATEND[data$PA_CATEND=="12"] <- "CARATER DE ATENDIMENTO INVALIDO"
      data$PA_CATEND[data$PA_CATEND=="20"] <- "CARATER DE ATENDIMENTO INVALIDO"
      data$PA_CATEND[data$PA_CATEND=="53"] <- "CARATER DE ATENDIMENTO INVALIDO"
      data$PA_CATEND[data$PA_CATEND=="54"] <- "CARATER DE ATENDIMENTO INVALIDO"
      data$PA_CATEND[data$PA_CATEND=="57"] <- "CARATER DE ATENDIMENTO INVALIDO"
      data$PA_CATEND <- factor(data$PA_CATEND)
    }

    # PA_IDADE
    if("PA_IDADE" %in% variables_names){
      data$PA_IDADE <- as.numeric(levels(data$PA_IDADE))[data$PA_IDADE]
      data$PA_IDADE[data$PA_IDADE==999] <- NA
    }

    # IDADEMIN
    if("IDADEMIN" %in% variables_names){
      data$IDADEMIN <- as.numeric(levels(data$IDADEMIN))[data$IDADEMIN]
    }

    # IDADEMAX
    if("IDADEMAX" %in% variables_names){
      data$IDADEMAX <- as.numeric(levels(data$IDADEMAX))[data$IDADEMAX]
    }

    # PA_FLIDADE
    if("PA_FLIDADE" %in% variables_names){
      data$PA_FLIDADE <- as.character(data$PA_FLIDADE)
      data$PA_FLIDADE[data$PA_FLIDADE=="0"] <- "IDADE NÃO EXIGIDA"
      data$PA_FLIDADE[data$PA_FLIDADE=="1"] <- "IDADE COMPATIVEL COM O SIGTAP"
      data$PA_FLIDADE[data$PA_FLIDADE=="2"] <- "IDADE FORA DA FAIXA DO SIGTAP"
      data$PA_FLIDADE[data$PA_FLIDADE=="3"] <- "IDADE INEXISTENTE"
      data$PA_FLIDADE[data$PA_FLIDADE=="4"] <- "IDADE EM BRANCO"
      data$PA_FLIDADE <- factor(data$PA_FLIDADE)
    }

    # PA_SEXO
    if("PA_SEXO" %in% variables_names){
      data$PA_SEXO <- as.character(data$PA_SEXO)
      data$PA_SEXO[data$PA_SEXO=="0"] <- "Não exigido"
      data$PA_SEXO[data$PA_SEXO=="M"] <- "Masculino"
      data$PA_SEXO[data$PA_SEXO=="F"] <- "Feminino"
      data$PA_SEXO <- factor(data$PA_SEXO)
    }

    # PA_RACACOR
    if("PA_RACACOR" %in% variables_names){
      data$PA_RACACOR <- as.character(data$PA_RACACOR)
      data$PA_RACACOR[data$PA_RACACOR=="00"] <- "RAÇA/COR NÃO EXIGIDO"
      data$PA_RACACOR[data$PA_RACACOR=="01"] <- "BRANCA"
      data$PA_RACACOR[data$PA_RACACOR=="02"] <- "PRETA"
      data$PA_RACACOR[data$PA_RACACOR=="03"] <- "PARDA"
      data$PA_RACACOR[data$PA_RACACOR=="04"] <- "AMARELA"
      data$PA_RACACOR[data$PA_RACACOR=="05"] <- "INDIGENA"
      data$PA_RACACOR[data$PA_RACACOR=="99"] <- "SEM INFORMAÇÃO"
      data$PA_RACACOR[data$PA_RACACOR=="06"] <- "RAÇA/COR=06 (INDEVIDO)"
      data$PA_RACACOR[data$PA_RACACOR=="09"] <- "RAÇA/COR=09 (INDEVIDO)"
      data$PA_RACACOR[data$PA_RACACOR=="1M"] <- "RAÇA/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR[data$PA_RACACOR=="1G"] <- "RAÇA/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR[data$PA_RACACOR=="1C"] <- "RAÇA/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR[data$PA_RACACOR=="DE"] <- "RAÇA/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR[data$PA_RACACOR=="D"] <- "RAÇA/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR[data$PA_RACACOR=="87"] <- "RAÇA/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR <- factor(data$PA_RACACOR)
    }

    # PA_MUNPCN
    if("PA_MUNPCN" %in% variables_names & municipality_data == TRUE){
      data$PA_MUNPCN <- as.integer(as.character(data$PA_MUNPCN))
      colnames(tabMun)[1] <- "PA_MUNPCN"
      data <- dplyr::left_join(data, tabMun, by = "PA_MUNPCN")
    } else {
      data$PA_MUNPCN <- as.integer(as.character(data$PA_MUNPCN))
    }

    # PA_QTDPRO
    if("PA_QTDPRO" %in% variables_names){
      data$PA_QTDPRO <- as.integer(data$PA_QTDPRO)
    }

    # PA_QTDAPR
    if("PA_QTDAPR" %in% variables_names){
      data$PA_QTDAPR <- as.integer(data$PA_QTDAPR)
    }

    # PA_VALPRO
    if("PA_VALPRO" %in% variables_names){
      data$PA_VALPRO <- as.integer(data$PA_VALPRO)
    }

    # PA_VALPRO
    if("PA_VALPRO" %in% variables_names){
      data$PA_VALPRO <- as.numeric(data$PA_VALPRO)
    }

    # PA_VALAPR
    if("PA_VALAPR" %in% variables_names){
      data$PA_VALAPR <- as.numeric(data$PA_VALAPR)
    }

    # PA_UFDIF
    if("PA_UFDIF" %in% variables_names){
      data$PA_UFDIF <- as.character(data$PA_UFDIF)
      data$PA_UFDIF[data$PA_UFDIF=="1"] <- "Sim (houve invasão)"
      data$PA_UFDIF[data$PA_UFDIF=="0"] <- "Não houve invasão"
      data$PA_UFDIF <- factor(data$PA_UFDIF)
    }

    # PA_MNDIF
    if("PA_MNDIF" %in% variables_names){
      data$PA_MNDIF <- as.character(data$PA_MNDIF)
      data$PA_MNDIF[data$PA_MNDIF=="1"] <- "Sim (houve invasão)"
      data$PA_MNDIF[data$PA_MNDIF=="0"] <- "Não houve invasão"
      data$PA_MNDIF <- factor(data$PA_MNDIF)
    }

    # PA_DIF_VAL
    if("PA_DIF_VAL" %in% variables_names){
      data$PA_DIF_VAL <- as.numeric(data$PA_DIF_VAL)
    }

    # NU_VPA_TOT
    if("NU_VPA_TOT" %in% variables_names){
      data$NU_VPA_TOT <- as.numeric(data$NU_VPA_TOT)
    }

    # NU_PA_TOT
    if("NU_PA_TOT" %in% variables_names){
      data$NU_PA_TOT <- as.numeric(data$NU_PA_TOT)
    }

    # PA_INDICA
    if("PA_INDICA" %in% variables_names){
      data$PA_INDICA <- as.character(data$PA_INDICA)
      data$PA_INDICA[data$PA_INDICA=="5"] <- "Aprovado totalmente"
      data$PA_INDICA[data$PA_INDICA=="6"] <- "Aprovado parcialmente"
      data$PA_INDICA[data$PA_INDICA=="0"] <- "Não aprovado"
      data$PA_INDICA <- factor(data$PA_INDICA)
    }

    # PA_CODOCO
    if("PA_CODOCO" %in% variables_names){
      data$PA_CODOCO <- as.character(data$PA_CODOCO)
    }

    # PA_FLQT
    if("PA_FLQT" %in% variables_names){
      data$PA_FLQT <- as.character(data$PA_FLQT)
    }

    # PA_FLER
    if("PA_FLER" %in% variables_names){
      data$PA_FLER <- as.character(data$PA_FLER)
    }

    # PA_ETNIA
    if("PA_ETNIA" %in% variables_names){
      data$PA_ETNIA <- as.numeric(levels(data$PA_ETNIA))[data$PA_ETNIA]
      data$PA_ETNIA[data$PA_ETNIA==1] <- "ACONA (WAKONAS, NACONAS, JAKONA, ACORANES)"
      data$PA_ETNIA[data$PA_ETNIA==2] <- "AIKANA (AIKANA, MAS SAKA,TUBARAO)"
      data$PA_ETNIA[data$PA_ETNIA==3] <- "AJURU"
      data$PA_ETNIA[data$PA_ETNIA==4] <- "AKUNSU (AKUNT'SU)"
      data$PA_ETNIA[data$PA_ETNIA==5] <- "AMANAYE"
      data$PA_ETNIA[data$PA_ETNIA==6] <- "AMONDAWA"
      data$PA_ETNIA[data$PA_ETNIA==7] <- "ANAMBE"
      data$PA_ETNIA[data$PA_ETNIA==8] <- "APARAI (APALAI)"
      data$PA_ETNIA[data$PA_ETNIA==9] <- "APIAKA (APIACA)"
      data$PA_ETNIA[data$PA_ETNIA==10] <- "APINAYE (APINAJE/APINAIE/APINAGE)"
      data$PA_ETNIA[data$PA_ETNIA==11] <- "APURINA (APORINA, IPURINA, IPURINA, IPURINAN)"
      data$PA_ETNIA[data$PA_ETNIA==12] <- "ARANA (ARACUAI DO VALE DO JEQUITINHONHA)"
      data$PA_ETNIA[data$PA_ETNIA==13] <- "ARAPASO (ARAPACO)"
      data$PA_ETNIA[data$PA_ETNIA==14] <- "ARARA DE RONDONIA (KARO, URUCU, URUKU)"
      data$PA_ETNIA[data$PA_ETNIA==15] <- "ARARA DO ACRE (SHAWANAUA, AMAWAKA)"
      data$PA_ETNIA[data$PA_ETNIA==16] <- "ARARA DO ARIPUANA (ARARA DO BEIRADAO/ARIPUANA)"
      data$PA_ETNIA[data$PA_ETNIA==17] <- "ARARA DO PARA (UKARAGMA, UKARAMMA)"
      data$PA_ETNIA[data$PA_ETNIA==18] <- "ARAWETE (ARAUETE)"
      data$PA_ETNIA[data$PA_ETNIA==19] <- "ARIKAPU (ARICAPU, ARIKAPO, MASUBI, MAXUBI)"
      data$PA_ETNIA[data$PA_ETNIA==20] <- "ARIKEM (ARIQUEN, ARIQUEME, ARIKEME)"
      data$PA_ETNIA[data$PA_ETNIA==21] <- "ARIKOSE (ARICOBE)"
      data$PA_ETNIA[data$PA_ETNIA==22] <- "ARUA"
      data$PA_ETNIA[data$PA_ETNIA==23] <- "ARUAK (ARAWAK)"
      data$PA_ETNIA[data$PA_ETNIA==24] <- "ASHANINKA (KAMPA)"
      data$PA_ETNIA[data$PA_ETNIA==25] <- "ASURINI DO TOCANTINS (AKUAWA/AKWAWA)"
      data$PA_ETNIA[data$PA_ETNIA==26] <- "ASURINI DO XINGU (AWAETE)"
      data$PA_ETNIA[data$PA_ETNIA==27] <- "ATIKUM (ATICUM)"
      data$PA_ETNIA[data$PA_ETNIA==28] <- "AVA - CANOEIRO"
      data$PA_ETNIA[data$PA_ETNIA==29] <- "AWETI (AUETI/AUETO)"
      data$PA_ETNIA[data$PA_ETNIA==30] <- "BAKAIRI (KURA, BACAIRI)"
      data$PA_ETNIA[data$PA_ETNIA==31] <- "BANAWA YAFI (BANAWA, BANAWA-JAFI)"
      data$PA_ETNIA[data$PA_ETNIA==32] <- "BANIWA (BANIUA, BANIVA, WALIMANAI, WAKUENAI)"
      data$PA_ETNIA[data$PA_ETNIA==33] <- "BARA (WAIPINOMAKA)"
      data$PA_ETNIA[data$PA_ETNIA==34] <- "BARASANA (HANERA)"
      data$PA_ETNIA[data$PA_ETNIA==35] <- "BARE"
      data$PA_ETNIA[data$PA_ETNIA==36] <- "BORORO (BOE)"
      data$PA_ETNIA[data$PA_ETNIA==37] <- "BOTOCUDO (GEREN)"
      data$PA_ETNIA[data$PA_ETNIA==38] <- "CANOE"
      data$PA_ETNIA[data$PA_ETNIA==39] <- "CASSUPA"
      data$PA_ETNIA[data$PA_ETNIA==40] <- "CHAMACOCO"
      data$PA_ETNIA[data$PA_ETNIA==41] <- "CHIQUITANO (XIQUITANO)"
      data$PA_ETNIA[data$PA_ETNIA==42] <- "CIKIYANA (SIKIANA)"
      data$PA_ETNIA[data$PA_ETNIA==43] <- "CINTA LARGA (MATETAMAE)"
      data$PA_ETNIA[data$PA_ETNIA==44] <- "COLUMBIARA (CORUMBIARA)"
      data$PA_ETNIA[data$PA_ETNIA==45] <- "DENI"
      data$PA_ETNIA[data$PA_ETNIA==46] <- "DESANA (DESANA, DESANO, DESSANO, WIRA, UMUKOMASA)"
      data$PA_ETNIA[data$PA_ETNIA==47] <- "DIAHUI (JAHOI, JAHUI, DIARROI)"
      data$PA_ETNIA[data$PA_ETNIA==48] <- "ENAWENE-NAWE (SALUMA)"
      data$PA_ETNIA[data$PA_ETNIA==49] <- "FULNI-O"
      data$PA_ETNIA[data$PA_ETNIA==50] <- "GALIBI (GALIBI DO OIAPOQUE, KARINHA)"
      data$PA_ETNIA[data$PA_ETNIA==51] <- "GALIBI MARWORNO (GALIBI DO UACA, ARUA)"
      data$PA_ETNIA[data$PA_ETNIA==52] <- "GAVIAO DE RONDONIA (DIGUT)"
      data$PA_ETNIA[data$PA_ETNIA==53] <- "GAVIAO KRIKATEJE"
      data$PA_ETNIA[data$PA_ETNIA==54] <- "GAVIAO PARKATEJE (PARKATEJE)"
      data$PA_ETNIA[data$PA_ETNIA==55] <- "GAVIAO PUKOBIE (PUKOBIE, PYKOPJE, GAVIAO DO MARANH"
      data$PA_ETNIA[data$PA_ETNIA==56] <- "GUAJA (AWA, AVA)"
      data$PA_ETNIA[data$PA_ETNIA==57] <- "GUAJAJARA (TENETEHARA)"
      data$PA_ETNIA[data$PA_ETNIA==58] <- "GUARANI KAIOWA (PAI TAVYTERA)"
      data$PA_ETNIA[data$PA_ETNIA==59] <- "GUARANI M'BYA"
      data$PA_ETNIA[data$PA_ETNIA==60] <- "GUARANI NANDEVA (AVAKATUETE, CHIRIPA, NHANDEWA, AV"
      data$PA_ETNIA[data$PA_ETNIA==61] <- "GUATO"
      data$PA_ETNIA[data$PA_ETNIA==62] <- "HIMARIMA (HIMERIMA)"
      data$PA_ETNIA[data$PA_ETNIA==63] <- "INGARIKO (INGARICO, AKAWAIO, KAPON)"
      data$PA_ETNIA[data$PA_ETNIA==64] <- "IRANXE (IRANTXE)"
      data$PA_ETNIA[data$PA_ETNIA==65] <- "ISSE"
      data$PA_ETNIA[data$PA_ETNIA==66] <- "JABOTI (JABUTI, KIPIU, YABYTI)"
      data$PA_ETNIA[data$PA_ETNIA==67] <- "JAMAMADI (YAMAMADI, DJEOROMITXI)"
      data$PA_ETNIA[data$PA_ETNIA==68] <- "JARAWARA"
      data$PA_ETNIA[data$PA_ETNIA==69] <- "JIRIPANCO (JERIPANCO, GERIPANCO)"
      data$PA_ETNIA[data$PA_ETNIA==70] <- "JUMA (YUMA)"
      data$PA_ETNIA[data$PA_ETNIA==71] <- "JURUNA"
      data$PA_ETNIA[data$PA_ETNIA==72] <- "JURUTI (YURITI)"
      data$PA_ETNIA[data$PA_ETNIA==73] <- "KAAPOR (URUBU-KAAPOR, KA'APOR, KAAPORTE)"
      data$PA_ETNIA[data$PA_ETNIA==74] <- "KADIWEU (CADUVEO, CADIUEU)"
      data$PA_ETNIA[data$PA_ETNIA==75] <- "KAIABI (CAIABI, KAYABI)"
      data$PA_ETNIA[data$PA_ETNIA==76] <- "KAIMBE (CAIMBE)"
      data$PA_ETNIA[data$PA_ETNIA==77] <- "KAINGANG (CAINGANGUE)"
      data$PA_ETNIA[data$PA_ETNIA==78] <- "KAIXANA (CAIXANA)"
      data$PA_ETNIA[data$PA_ETNIA==79] <- "KALABASSA (CALABASSA, CALABACAS)"
      data$PA_ETNIA[data$PA_ETNIA==80] <- "KALANCO"
      data$PA_ETNIA[data$PA_ETNIA==81] <- "KALAPALO (CALAPALO)"
      data$PA_ETNIA[data$PA_ETNIA==82] <- "KAMAYURA (CAMAIURA, KAMAIURA)"
      data$PA_ETNIA[data$PA_ETNIA==83] <- "KAMBA (CAMBA)"
      data$PA_ETNIA[data$PA_ETNIA==84] <- "KAMBEBA (CAMBEBA, OMAGUA)"
      data$PA_ETNIA[data$PA_ETNIA==85] <- "KAMBIWA (CAMBIUA)"
      data$PA_ETNIA[data$PA_ETNIA==86] <- "KAMBIWA PIPIPA (PIPIPA)"
      data$PA_ETNIA[data$PA_ETNIA==87] <- "KAMPE"
      data$PA_ETNIA[data$PA_ETNIA==88] <- "KANAMANTI (KANAMATI, CANAMANTI)"
      data$PA_ETNIA[data$PA_ETNIA==89] <- "KANAMARI (CANAMARI, KANAMARY, TUKUNA)"
      data$PA_ETNIA[data$PA_ETNIA==90] <- "KANELA APANIEKRA (CANELA)"
      data$PA_ETNIA[data$PA_ETNIA==91] <- "KANELA RANKOKAMEKRA (CANELA)"
      data$PA_ETNIA[data$PA_ETNIA==92] <- "KANINDE"
      data$PA_ETNIA[data$PA_ETNIA==93] <- "KANOE (CANOE)"
      data$PA_ETNIA[data$PA_ETNIA==94] <- "KANTARURE (CANTARURE)"
      data$PA_ETNIA[data$PA_ETNIA==95] <- "KAPINAWA (CAPINAUA)"
      data$PA_ETNIA[data$PA_ETNIA==96] <- "KARAJA (CARAJA)"
      data$PA_ETNIA[data$PA_ETNIA==97] <- "KARAJA/JAVAE (JAVAE)"
      data$PA_ETNIA[data$PA_ETNIA==98] <- "KARAJA/XAMBIOA (KARAJA DO NORTE)"
      data$PA_ETNIA[data$PA_ETNIA==99] <- "KARAPANA (CARAPANA, MUTEAMASA, UKOPINOPONA)"
      data$PA_ETNIA[data$PA_ETNIA==100] <- "KARAPOTO (CARAPOTO)"
      data$PA_ETNIA[data$PA_ETNIA==101] <- "KARIPUNA (CARIPUNA)"
      data$PA_ETNIA[data$PA_ETNIA==102] <- "KARIPUNA DO AMAPA (CARIPUNA)"
      data$PA_ETNIA[data$PA_ETNIA==103] <- "KARIRI (CARIRI)"
      data$PA_ETNIA[data$PA_ETNIA==104] <- "KARIRI-XOCO (CARIRI-CHOCO)"
      data$PA_ETNIA[data$PA_ETNIA==105] <- "KARITIANA (CARITIANA)"
      data$PA_ETNIA[data$PA_ETNIA==106] <- "KATAWIXI (KATAUIXI,KATAWIN, KATAWISI, CATAUICHI)"
      data$PA_ETNIA[data$PA_ETNIA==107] <- "KATUENA (CATUENA, KATWENA)"
      data$PA_ETNIA[data$PA_ETNIA==108] <- "KATUKINA (PEDA DJAPA)"
      data$PA_ETNIA[data$PA_ETNIA==109] <- "KATUKINA DO ACRE"
      data$PA_ETNIA[data$PA_ETNIA==110] <- "KAXARARI (CAXARARI)"
      data$PA_ETNIA[data$PA_ETNIA==111] <- "KAXINAWA (HUNI-KUIN, CASHINAUA, CAXINAUA)"
      data$PA_ETNIA[data$PA_ETNIA==112] <- "KAXIXO"
      data$PA_ETNIA[data$PA_ETNIA==113] <- "KAXUYANA (CAXUIANA)"
      data$PA_ETNIA[data$PA_ETNIA==114] <- "KAYAPO (CAIAPO)"
      data$PA_ETNIA[data$PA_ETNIA==115] <- "KAYAPO KARARAO (KARARAO)"
      data$PA_ETNIA[data$PA_ETNIA==116] <- "KAYAPO TXUKAHAMAE (TXUKAHAMAE)"
      data$PA_ETNIA[data$PA_ETNIA==117] <- "KAYAPO XICRIM (XIKRIN)"
      data$PA_ETNIA[data$PA_ETNIA==118] <- "KAYUISANA (CAIXANA, CAUIXANA, KAIXANA)"
      data$PA_ETNIA[data$PA_ETNIA==119] <- "KINIKINAWA (GUAN, KOINUKOEN, KINIKINAO)"
      data$PA_ETNIA[data$PA_ETNIA==120] <- "KIRIRI"
      data$PA_ETNIA[data$PA_ETNIA==121] <- "KOCAMA (COCAMA, KOKAMA)"
      data$PA_ETNIA[data$PA_ETNIA==122] <- "KOKUIREGATEJE"
      data$PA_ETNIA[data$PA_ETNIA==123] <- "KORUBO"
      data$PA_ETNIA[data$PA_ETNIA==124] <- "KRAHO (CRAO, KRAO)"
      data$PA_ETNIA[data$PA_ETNIA==125] <- "KREJE (KRENYE)"
      data$PA_ETNIA[data$PA_ETNIA==126] <- "KRENAK (BORUN, CRENAQUE)"
      data$PA_ETNIA[data$PA_ETNIA==127] <- "KRIKATI (KRINKATI)"
      data$PA_ETNIA[data$PA_ETNIA==128] <- "KUBEO (CUBEO, COBEWA, KUBEWA, PAMIWA, CUBEU)"
      data$PA_ETNIA[data$PA_ETNIA==129] <- "KUIKURO (KUIKURU, CUICURO)"
      data$PA_ETNIA[data$PA_ETNIA==130] <- "KUJUBIM (KUYUBI, CUJUBIM)"
      data$PA_ETNIA[data$PA_ETNIA==131] <- "KULINA PANO (CULINA)"
      data$PA_ETNIA[data$PA_ETNIA==132] <- "KULINA/MADIHA (CULINA, MADIJA, MADIHA)"
      data$PA_ETNIA[data$PA_ETNIA==133] <- "KURIPAKO (CURIPACO, CURRIPACO, CORIPACO, WAKUENAI)"
      data$PA_ETNIA[data$PA_ETNIA==134] <- "KURUAIA (CURUAIA)"
      data$PA_ETNIA[data$PA_ETNIA==135] <- "KWAZA (COAIA, KOAIA)"
      data$PA_ETNIA[data$PA_ETNIA==136] <- "MACHINERI (MANCHINERI, MANXINERI)"
      data$PA_ETNIA[data$PA_ETNIA==137] <- "MACURAP (MAKURAP)"
      data$PA_ETNIA[data$PA_ETNIA==138] <- "MAKU DOW (DOW)"
      data$PA_ETNIA[data$PA_ETNIA==139] <- "MAKU HUPDA (HUPDA)"
      data$PA_ETNIA[data$PA_ETNIA==140] <- "MAKU NADEB (NADEB)"
      data$PA_ETNIA[data$PA_ETNIA==141] <- "MAKU YUHUPDE (YUHUPDE)"
      data$PA_ETNIA[data$PA_ETNIA==142] <- "MAKUNA (MACUNA, YEBA-MASA)"
      data$PA_ETNIA[data$PA_ETNIA==143] <- "MAKUXI (MACUXI, MACHUSI, PEMON)"
      data$PA_ETNIA[data$PA_ETNIA==144] <- "MARIMAM (MARIMA)"
      data$PA_ETNIA[data$PA_ETNIA==145] <- "MARUBO"
      data$PA_ETNIA[data$PA_ETNIA==146] <- "MATIPU"
      data$PA_ETNIA[data$PA_ETNIA==147] <- "MATIS"
      data$PA_ETNIA[data$PA_ETNIA==148] <- "MATSE (MAYORUNA)"
      data$PA_ETNIA[data$PA_ETNIA==149] <- "MAXAKALI (MAXACALI)"
      data$PA_ETNIA[data$PA_ETNIA==150] <- "MAYA (MAYA)"
      data$PA_ETNIA[data$PA_ETNIA==151] <- "MAYTAPU"
      data$PA_ETNIA[data$PA_ETNIA==152] <- "MEHINAKO (MEINAKU, MEINACU)"
      data$PA_ETNIA[data$PA_ETNIA==153] <- "MEKEN (MEQUEM, MEKHEM, MICHENS)"
      data$PA_ETNIA[data$PA_ETNIA==154] <- "MENKY (MYKY, MUNKU, MENKI, MYNKY)"
      data$PA_ETNIA[data$PA_ETNIA==155] <- "MIRANHA (MIRANHA, MIRANA)"
      data$PA_ETNIA[data$PA_ETNIA==156] <- "MIRITI TAPUIA (MIRITI-TAPUYA, BUIA-TAPUYA)"
      data$PA_ETNIA[data$PA_ETNIA==157] <- "MUNDURUKU (MUNDURUCU)"
      data$PA_ETNIA[data$PA_ETNIA==158] <- "MURA"
      data$PA_ETNIA[data$PA_ETNIA==159] <- "NAHUKWA (NAFUQUA)"
      data$PA_ETNIA[data$PA_ETNIA==160] <- "NAMBIKWARA DO CAMPO (HALOTESU, KITHAULU, WAKALITES"
      data$PA_ETNIA[data$PA_ETNIA==161] <- "NAMBIKWARA DO NORTE (NEGAROTE ,MAMAINDE, LATUNDE,"
      data$PA_ETNIA[data$PA_ETNIA==162] <- "NAMBIKWARA DO SUL (WASUSU ,HAHAINTESU, ALANTESU, W"
      data$PA_ETNIA[data$PA_ETNIA==163] <- "NARAVUTE (NARUVOTO)"
      data$PA_ETNIA[data$PA_ETNIA==164] <- "NAWA (NAUA)"
      data$PA_ETNIA[data$PA_ETNIA==165] <- "NUKINI (NUQUINI, NUKUINI)"
      data$PA_ETNIA[data$PA_ETNIA==166] <- "OFAIE (OFAYE-XAVANTE)"
      data$PA_ETNIA[data$PA_ETNIA==167] <- "ORO WIN"
      data$PA_ETNIA[data$PA_ETNIA==168] <- "PAIAKU (JENIPAPO-KANINDE)"
      data$PA_ETNIA[data$PA_ETNIA==169] <- "PAKAA NOVA (WARI, PACAAS NOVOS)"
      data$PA_ETNIA[data$PA_ETNIA==170] <- "PALIKUR (AUKWAYENE, AUKUYENE, PALIKU'ENE)"
      data$PA_ETNIA[data$PA_ETNIA==171] <- "PANARA (KRENHAKARORE , KRENAKORE, KRENAKARORE)"
      data$PA_ETNIA[data$PA_ETNIA==172] <- "PANKARARE (PANCARARE)"
      data$PA_ETNIA[data$PA_ETNIA==173] <- "PANKARARU (PANCARARU)"
      data$PA_ETNIA[data$PA_ETNIA==174] <- "PANKARARU KALANKO (KALANKO)"
      data$PA_ETNIA[data$PA_ETNIA==175] <- "PANKARARU KARUAZU (KARUAZU)"
      data$PA_ETNIA[data$PA_ETNIA==176] <- "PANKARU (PANCARU)"
      data$PA_ETNIA[data$PA_ETNIA==177] <- "PARAKANA (PARACANA, APITEREWA, AWAETE)"
      data$PA_ETNIA[data$PA_ETNIA==178] <- "PARECI (PARESI, HALITI)"
      data$PA_ETNIA[data$PA_ETNIA==179] <- "PARINTINTIN"
      data$PA_ETNIA[data$PA_ETNIA==180] <- "PATAMONA (KAPON)"
      data$PA_ETNIA[data$PA_ETNIA==181] <- "PATAXO"
      data$PA_ETNIA[data$PA_ETNIA==182] <- "PATAXO HA-HA-HAE"
      data$PA_ETNIA[data$PA_ETNIA==183] <- "PAUMARI (PALMARI)"
      data$PA_ETNIA[data$PA_ETNIA==184] <- "PAUMELENHO"
      data$PA_ETNIA[data$PA_ETNIA==185] <- "PIRAHA (MURA PIRAHA)"
      data$PA_ETNIA[data$PA_ETNIA==186] <- "PIRATUAPUIA (PIRATAPUYA, PIRATAPUYO, PIRATAPUYA,"
      data$PA_ETNIA[data$PA_ETNIA==187] <- "PITAGUARI"
      data$PA_ETNIA[data$PA_ETNIA==188] <- "POTIGUARA"
      data$PA_ETNIA[data$PA_ETNIA==189] <- "POYANAWA (POIANAUA)"
      data$PA_ETNIA[data$PA_ETNIA==190] <- "RIKBAKTSA (CANOEIROS, ERIGPAKTSA)"
      data$PA_ETNIA[data$PA_ETNIA==191] <- "SAKURABIAT (MEKENS, SAKIRABIAP, SAKIRABIAR)"
      data$PA_ETNIA[data$PA_ETNIA==192] <- "SATERE-MAWE (SATERE-MAUE)"
      data$PA_ETNIA[data$PA_ETNIA==193] <- "SHANENAWA (KATUKINA)"
      data$PA_ETNIA[data$PA_ETNIA==194] <- "SIRIANO (SIRIA-MASA)"
      data$PA_ETNIA[data$PA_ETNIA==195] <- "SURIANA"
      data$PA_ETNIA[data$PA_ETNIA==196] <- "SURUI DE RONDONIA (PAITER)"
      data$PA_ETNIA[data$PA_ETNIA==197] <- "SURUI DO PARA (AIKEWARA)"
      data$PA_ETNIA[data$PA_ETNIA==198] <- "SUYA (SUIA/KISEDJE)"
      data$PA_ETNIA[data$PA_ETNIA==199] <- "TAPAYUNA (BEICO-DE-PAU)"
      data$PA_ETNIA[data$PA_ETNIA==200] <- "TAPEBA"
      data$PA_ETNIA[data$PA_ETNIA==201] <- "TAPIRAPE (TAPI'IRAPE)"
      data$PA_ETNIA[data$PA_ETNIA==202] <- "TAPUIA (TAPUIA-XAVANTE, TAPUIO)"
      data$PA_ETNIA[data$PA_ETNIA==203] <- "TARIANO (TARIANA, TALIASERI)"
      data$PA_ETNIA[data$PA_ETNIA==204] <- "TAUREPANG (TAULIPANG, PEMON, AREKUNA, PAGEYN)"
      data$PA_ETNIA[data$PA_ETNIA==205] <- "TEMBE"
      data$PA_ETNIA[data$PA_ETNIA==206] <- "TENHARIM"
      data$PA_ETNIA[data$PA_ETNIA==207] <- "TERENA"
      data$PA_ETNIA[data$PA_ETNIA==208] <- "TICUNA (TIKUNA, TUKUNA, MAGUTA)"
      data$PA_ETNIA[data$PA_ETNIA==209] <- "TINGUI BOTO"
      data$PA_ETNIA[data$PA_ETNIA==210] <- "TIRIYO EWARHUYANA (TIRIYO, TRIO, TARONA, YAWI, PIA"
      data$PA_ETNIA[data$PA_ETNIA==211] <- "TIRIYO KAH'YANA (TIRIYO, TRIO, TARONA, YAWI, PIANO"
      data$PA_ETNIA[data$PA_ETNIA==212] <- "TIRIYO TSIKUYANA (TIRIYO, TRIO, TARONA, YAWI, PIAN"
      data$PA_ETNIA[data$PA_ETNIA==213] <- "TORA"
      data$PA_ETNIA[data$PA_ETNIA==214] <- "TREMEMBE"
      data$PA_ETNIA[data$PA_ETNIA==215] <- "TRUKA"
      data$PA_ETNIA[data$PA_ETNIA==216] <- "TRUMAI"
      data$PA_ETNIA[data$PA_ETNIA==217] <- "TSOHOM DJAPA (TSUNHUM-DJAPA)"
      data$PA_ETNIA[data$PA_ETNIA==218] <- "TUKANO (TUCANO, YE'PA-MASA, DASEA)"
      data$PA_ETNIA[data$PA_ETNIA==219] <- "TUMBALALA"
      data$PA_ETNIA[data$PA_ETNIA==220] <- "TUNAYANA"
      data$PA_ETNIA[data$PA_ETNIA==221] <- "TUPARI"
      data$PA_ETNIA[data$PA_ETNIA==222] <- "TUPINAMBA"
      data$PA_ETNIA[data$PA_ETNIA==223] <- "TUPINIQUIM"
      data$PA_ETNIA[data$PA_ETNIA==224] <- "TURIWARA"
      data$PA_ETNIA[data$PA_ETNIA==225] <- "TUXA"
      data$PA_ETNIA[data$PA_ETNIA==226] <- "TUYUKA (TUIUCA, DOKAPUARA, UTAPINOMAKAPHONA)"
      data$PA_ETNIA[data$PA_ETNIA==227] <- "TXIKAO (TXICAO, IKPENG)"
      data$PA_ETNIA[data$PA_ETNIA==228] <- "UMUTINA (OMOTINA, BARBADOS)"
      data$PA_ETNIA[data$PA_ETNIA==229] <- "URU-EU-WAU-WAU (URUEU-UAU-UAU, URUPAIN, URUPA)"
      data$PA_ETNIA[data$PA_ETNIA==230] <- "WAI WAI HIXKARYANA (HIXKARYANA)"
      data$PA_ETNIA[data$PA_ETNIA==231] <- "WAI WAI KARAFAWYANA (KARAFAWYANA, KARAPAWYANA)"
      data$PA_ETNIA[data$PA_ETNIA==232] <- "WAI WAI XEREU (XEREU)"
      data$PA_ETNIA[data$PA_ETNIA==233] <- "WAI WAI KATUENA (KATUENA)"
      data$PA_ETNIA[data$PA_ETNIA==234] <- "WAI WAI MAWAYANA (MAWAYANA)"
      data$PA_ETNIA[data$PA_ETNIA==235] <- "WAIAPI (WAYAMPI, OYAMPI, WAYAPY, )"
      data$PA_ETNIA[data$PA_ETNIA==236] <- "WAIMIRI ATROARI (KINA)"
      data$PA_ETNIA[data$PA_ETNIA==237] <- "WANANO (UANANO, WANANA)"
      data$PA_ETNIA[data$PA_ETNIA==238] <- "WAPIXANA (UAPIXANA, VAPIDIANA, WAPISIANA, WAPISHAN"
      data$PA_ETNIA[data$PA_ETNIA==239] <- "WAREKENA (UAREQUENA, WEREKENA)"
      data$PA_ETNIA[data$PA_ETNIA==240] <- "WASSU"
      data$PA_ETNIA[data$PA_ETNIA==241] <- "WAURA (UAURA, WAUJA)"
      data$PA_ETNIA[data$PA_ETNIA==242] <- "WAYANA (WAIANA, UAIANA)"
      data$PA_ETNIA[data$PA_ETNIA==243] <- "WITOTO (UITOTO, HUITOTO)"
      data$PA_ETNIA[data$PA_ETNIA==244] <- "XAKRIABA (XACRIABA)"
      data$PA_ETNIA[data$PA_ETNIA==245] <- "XAVANTE (A'UWE, AKWE, AWEN, AKWEN)"
      data$PA_ETNIA[data$PA_ETNIA==246] <- "XERENTE (AKWE, AWEN, AKWEN)"
      data$PA_ETNIA[data$PA_ETNIA==247] <- "XETA"
      data$PA_ETNIA[data$PA_ETNIA==248] <- "XIPAIA (SHIPAYA, XIPAYA)"
      data$PA_ETNIA[data$PA_ETNIA==249] <- "XOKLENG (SHOKLENG, XOCLENG)"
      data$PA_ETNIA[data$PA_ETNIA==250] <- "XOKO (XOCO, CHOCO)"
      data$PA_ETNIA[data$PA_ETNIA==251] <- "XUKURU (XUCURU)"
      data$PA_ETNIA[data$PA_ETNIA==252] <- "XUKURU KARIRI (XUCURU-KARIRI)"
      data$PA_ETNIA[data$PA_ETNIA==253] <- "YAIPIYANA"
      data$PA_ETNIA[data$PA_ETNIA==254] <- "YAMINAWA (JAMINAWA, IAMINAWA)"
      data$PA_ETNIA[data$PA_ETNIA==255] <- "YANOMAMI NINAM (IANOMAMI, IANOAMA, XIRIANA)"
      data$PA_ETNIA[data$PA_ETNIA==256] <- "YANOMAMI SANUMA (IANOMAMI, IANOAMA, XIRIANA)"
      data$PA_ETNIA[data$PA_ETNIA==257] <- "YANOMAMI YANOMAM (IANOMAMI, IANOAMA, XIRIANA)"
      data$PA_ETNIA[data$PA_ETNIA==258] <- "YAWALAPITI (IAUALAPITI)"
      data$PA_ETNIA[data$PA_ETNIA==259] <- "YAWANAWA (IAUANAUA)"
      data$PA_ETNIA[data$PA_ETNIA==260] <- "YEKUANA (MAIONGON, YE'KUANA, YEKWANA, MAYONGONG)"
      data$PA_ETNIA[data$PA_ETNIA==261] <- "YUDJA (JURUNA, YURUNA)"
      data$PA_ETNIA[data$PA_ETNIA==262] <- "ZO'E (POTURU)"
      data$PA_ETNIA[data$PA_ETNIA==263] <- "ZORO (PAGEYN)"
      data$PA_ETNIA[data$PA_ETNIA==264] <- "ZURUAHA (SOROWAHA, SURUWAHA)"
      data$PA_ETNIA[data$PA_ETNIA==0] <- NA
      data$PA_ETNIA[data$PA_ETNIA==9999] <- NA
      data$PA_ETNIA <- factor(data$PA_ETNIA)
    }

    # PA_VL_CF
    if("PA_VL_CF" %in% variables_names){
      data$PA_VL_CF <- as.numeric(data$PA_VL_CF)
    }

    # PA_VL_CL
    if("PA_VL_CL" %in% variables_names){
      data$PA_VL_CL <- as.numeric(data$PA_VL_CL)
    }

    # PA_VL_INC
    if("PA_VL_INC" %in% variables_names){
      data$PA_VL_INC <- as.numeric(data$PA_VL_INC)
    }

    # PA_SRC_C
    if("PA_VL_INC" %in% variables_names){
      data$PA_VL_INC <- as.character(data$PA_VL_INC)
    }

    # PA_INE
    if("PA_INE" %in% variables_names){
      data$PA_INE <- as.character(data$PA_INE)
      data <- dplyr::left_join(data, equipe, by = c("PA_INE" = "COD"))
    }

  }

  # Purge levels
  data <- droplevels(data)

  # Return
  return(data)

}
