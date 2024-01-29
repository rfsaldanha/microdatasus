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
#' @param nome_proced optional logical. \code{TRUE} by default, add  \code{PA_PROCED_NOME} to the dataset. This setting will start to download a file from DataSUS to retrive the updates list of procesures (SIGTAB).
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
#' @export

process_sia <- function(data, information_system = "SIA-PA", nome_proced = TRUE, nome_ocupacao = TRUE, nome_equipe = TRUE, municipality_data = TRUE) {
  # Check information system
  available_information_system <- "SIA-PA"
  if(!(information_system %in% available_information_system)) stop("Health informaton system unknown.")

  # Variables names
  variables_names <- names(data)

  if(information_system == "SIA-PA"){

    # PA_REGCT
    if("PA_REGCT" %in% variables_names){
      data$PA_REGCT[data$PA_REGCT=="7100"] <- "TAB.DE N\u00c3O GERA\u00c7\u00c3O CR\u00c9DITO P/PROD.INTERN./AMBULAT."
      data$PA_REGCT[data$PA_REGCT=="7101"] <- "ESTAB.S/CR\u00c9DITO NA MEDIA COMPLEXIDADE AMBULATORIAL"
      data$PA_REGCT[data$PA_REGCT=="7102"] <- "ESTAB.S/CR\u00c9DITO NA MEDIA COMPLEXIDADE HOSPITALAR"
      data$PA_REGCT[data$PA_REGCT=="7103"] <- "ESTAB.S/CR\u00c9DITO NA ALTA COMPLEXIDADE AMBULATORIAL"
      data$PA_REGCT[data$PA_REGCT=="7104"] <- "ESTAB.S/CR\u00c9DITO NA ALTA COMPLEXIDADE HOSPITALAR"
      data$PA_REGCT[data$PA_REGCT=="7105"] <- "ESTAB.S/CRED.PROCED.FINANC.FD.A\u00c7\u00d5ES ESTRAT/COMPENS."
      data$PA_REGCT[data$PA_REGCT=="7106"] <- "ESTABELECIM. DE SA\u00daDE SEM GERA\u00c7\u00c3O DE CR\u00c9DITO TOTAL"
      data$PA_REGCT[data$PA_REGCT=="7107"] <- "ESTAB.S/CR\u00c9DITO ACOES ESPEC.ODONT(INC.CEO I,II,III)"
      data$PA_REGCT[data$PA_REGCT=="7108"] <- "ESTAB.S/GER.CR\u00c9DITO(INCENTIVO SAUDE DO TRABALHADOR)"
      data$PA_REGCT[data$PA_REGCT=="7109"] <- "ESTAB.SA\u00daDE (HU/MEC) SEM GERA\u00c7\u00c3O DE CR\u00c9DITO TOTAL"
      data$PA_REGCT[data$PA_REGCT=="7110"] <- "ESTAB.SAUDE (MIN.SAUDE)SEM GERA\u00c7\u00c3O DE CR\u00c9DITO TOTAL"
      data$PA_REGCT[data$PA_REGCT=="7111"] <- "ESTAB.SAUDE SEM GERA\u00c7\u00c3O DE CR\u00c9DITO-NASF,exceto FAEC"
      data$PA_REGCT[data$PA_REGCT=="7112"] <- "ESTAB.S/CRED.TOTAL-INCL.FAEC-EXCLUSIVO P/REDE SARAH"
      data$PA_REGCT[data$PA_REGCT=="7113"] <- "ESTAB.S/CRED.TOTAL,INCL.FAEC-OUTROS ESTAB. FEDERAIS"
      data$PA_REGCT[data$PA_REGCT=="7116"] <- "ESTAB.SA\u00daDE S/GER DE CR\u00c9DITO NA M\u00c9DIA COMPLEX-LRPD"
      data$PA_REGCT[data$PA_REGCT=="7117"] <- "ESTAB.SA\u00daDE S/GER DE CR\u00c9D. M\u00c9D COMP(EXCETO OPM)-CER"
      data$PA_REGCT[data$PA_REGCT=="0000"] <- "SEM REGRA CONTRATUAL"
    }

    # PA_INCOUT
    if("PA_INCOUT" %in% variables_names){
      data$PA_INCOUT[data$PA_INCOUT!="0000"] <- "Com incremento"
      data$PA_INCOUT[data$PA_INCOUT=="0000"] <- "Sem incremento"
    }

    # PA_INCURG
    if("PA_INCURG" %in% variables_names){
      data$PA_INCURG[data$PA_INCURG!="0000"] <- "Com incremento"
      data$PA_INCURG[data$PA_INCURG=="0000"] <- "Sem incremento"
    }

    # PA_TPUPS
    if("PA_TPUPS" %in% variables_names){
      data$PA_TPUPS[data$PA_TPUPS=="74"] <- "ACADEMIA DA SA\u00daDE"
      data$PA_TPUPS[data$PA_TPUPS=="81"] <- "CENTRAL DE REGULA\u00c7\u00c3O"
      data$PA_TPUPS[data$PA_TPUPS=="76"] <- "CENTRAL DE REGULA\u00c7\u00c3O M\u00c9DICA DAS URG\u00caNCIAS"
      data$PA_TPUPS[data$PA_TPUPS=="71"] <- "CENTRO DE APOIO A SA\u00daDE DA FAM\u00cdLIA-CASF"
      data$PA_TPUPS[data$PA_TPUPS=="69"] <- "CENTRO DE ATEN\u00c7\u00c3O HEMOTER\u00c1PICA E/OU HEMATOL\u00d3GICA"
      data$PA_TPUPS[data$PA_TPUPS=="70"] <- "CENTRO DE ATEN\u00c7\u00c3O PSICOSSOCIAL-CAPS"
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
      data$PA_TPUPS[data$PA_TPUPS=="72"] <- "UNIDADE DE ATEN\u00c7\u00c3O \u00c0 SA\u00daDE IND\u00cdGENA"
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
      data$PA_TPUPS[data$PA_TPUPS=="75"] <- "TELESA\u00daDE"
      data$PA_TPUPS[data$PA_TPUPS=="09"] <- "PRONTO SOCORRO DE HOSPITAL GERAL (ANTIGO)"
      data$PA_TPUPS[data$PA_TPUPS=="12"] <- "PRONTO SOCORRO TRAUMATO-ORTOPEDICO (ANTIGO)"
      data$PA_TPUPS <- factor(data$PA_TPUPS)
    }

    # PA_TIPPRE
    if("PA_TIPPRE" %in% variables_names){
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
      data$PA_MN_IND[data$PA_MN_IND=="M"] <- "Mantida"
      data$PA_MN_IND[data$PA_MN_IND=="I"] <- "Individual"
      data$PA_MN_IND <- factor(data$PA_MN_IND)
    }

    # PA_PROC_NOME
    if(nome_proced == TRUE){
      sigtab_temp <- microdatasus::fetch_sigtab()
      data <- dplyr::left_join(data, sigtab_temp, by = c("PA_PROC_ID" = "COD"))
    }

    # PA_TPFIN
    if("PA_TPFIN" %in% variables_names){
      data$PA_TPFIN[data$PA_TPFIN=="01"] <- "Aten\u00e7\u00e3o B\u00e1sica (PAB)"
      data$PA_TPFIN[data$PA_TPFIN=="02"] <- "Assist\u00eancia Farmac\u00eautica"
      data$PA_TPFIN[data$PA_TPFIN=="04"] <- "Fundo de A\u00e7\u00f5es Estrat\u00e9gicas e Compensa\u00e7\u00f5es FAEC"
      data$PA_TPFIN[data$PA_TPFIN=="05"] <- "Incentivo - MAC"
      data$PA_TPFIN[data$PA_TPFIN=="06"] <- "M\u00e9dia e Alta Complexidade (MAC)"
      data$PA_TPFIN[data$PA_TPFIN=="07"] <- "Vigil\u00e2ncia em Sa\u00fade"
      data$PA_TPFIN <- factor(data$PA_TPFIN)
    }

    # PA_NIVCPL
    if("PA_NIVCPL" %in% variables_names){
      data$PA_NIVCPL[data$PA_NIVCPL=="0"] <- "N\u00e3o se Aplica"
      data$PA_NIVCPL[data$PA_NIVCPL=="1"] <- "Aten\u00e7\u00e3o B\u00e1sica"
      data$PA_NIVCPL[data$PA_NIVCPL=="2"] <- "M\u00e9dia Complexidade"
      data$PA_NIVCPL[data$PA_NIVCPL=="3"] <- "Alta Complexidade"
      data$PA_NIVCPL <- factor(data$PA_NIVCPL)
    }

    # PA_DOCORIG
    if("PA_DOCORIG" %in% variables_names){
      data$PA_DOCORIG[data$PA_DOCORIG=="C"] <- "BPA-C"
      data$PA_DOCORIG[data$PA_DOCORIG=="I"] <- "BPA-I"
      data$PA_DOCORIG[data$PA_DOCORIG=="P"] <- "APAC - Procedimento Principal"
      data$PA_DOCORIG[data$PA_DOCORIG=="S"] <- "APAC - Procedimento Secund\u00e1rio"
      data$PA_DOCORIG[data$PA_DOCORIG=="A"] <- "RAAS - Aten\u00e7\u00e3o Domiciliar"
      data$PA_DOCORIG[data$PA_DOCORIG=="B"] <- "RAAS - Psicossocial"
      data$PA_DOCORIG <- factor(data$PA_DOCORIG)
    }

    # Nome OCUPACAO
    if(nome_ocupacao == TRUE){
      data <- dplyr::left_join(data, microdatasus::tabCBO, by = c("PA_CBOCOD" = "cod"))
      data <- dplyr::rename(data, "ocupacao" = "nome")
    }

    # PA_MOTSAI
    if("PA_MOTSAI" %in% variables_names){
      data$PA_MOTSAI[data$PA_MOTSAI=="11"] <- "ALTA CURADO"
      data$PA_MOTSAI[data$PA_MOTSAI=="12"] <- "ALTA MELHORADO"
      data$PA_MOTSAI[data$PA_MOTSAI=="13"] <- "ALTA DA PU\u00c9RPERA E PERMAN\u00caNCIA DO REC\u00c9M NASCIDO"
      data$PA_MOTSAI[data$PA_MOTSAI=="14"] <- "ALTA A PEDIDO"
      data$PA_MOTSAI[data$PA_MOTSAI=="15"] <- "ALTA COM PREVIS\u00c3O DE RETORNO P/ ACOMPAN. DO PACIENT"
      data$PA_MOTSAI[data$PA_MOTSAI=="16"] <- "ALTA POR EVAS\u00c3O"
      data$PA_MOTSAI[data$PA_MOTSAI=="17"] <- "ALTA DA PU\u00c9RPERA E REC\u00c9M NASCIDO"
      data$PA_MOTSAI[data$PA_MOTSAI=="18"] <- "ALTA POR OUTROS MOTIVOS"
      data$PA_MOTSAI[data$PA_MOTSAI=="21"] <- "PERMAN\u00caNCIA POR CARACTER\u00cdSTICAS PR\u00d3PRIAS DA DOEN\u00c7A"
      data$PA_MOTSAI[data$PA_MOTSAI=="22"] <- "PERMAN\u00caNCIA POR INTERCORR\u00caNCIA"
      data$PA_MOTSAI[data$PA_MOTSAI=="23"] <- "PERMAN\u00caNCIA POR IMPOSSIBILIDADE S\u00d3CIO-FAMILIAR"
      data$PA_MOTSAI[data$PA_MOTSAI=="24"] <- "PERMAN. POR PROCESSO-DOA\u00c7\u00c3O DE \u00d3RG\u00c3OS-DOADOR VIVO"
      data$PA_MOTSAI[data$PA_MOTSAI=="25"] <- "PERMAN. POR PROCESSO-DOA\u00c7\u00c3O DE \u00d3RG\u00c3OS-DOADOR MORTO"
      data$PA_MOTSAI[data$PA_MOTSAI=="26"] <- "PERMAN\u00caNCIA POR MUDAN\u00c7A DE PROCEDIMENTO"
      data$PA_MOTSAI[data$PA_MOTSAI=="27"] <- "PERMAN\u00caNCIA POR REOPERA\u00c7\u00c3O"
      data$PA_MOTSAI[data$PA_MOTSAI=="28"] <- "PERMAN\u00caNCIA POR OUTROS MOTIVOS"
      data$PA_MOTSAI[data$PA_MOTSAI=="31"] <- "TRANSFERIDO PARA OUTRO ESTABELECIMENTO"
      data$PA_MOTSAI[data$PA_MOTSAI=="41"] <- "\u00d3BITO COM DECLARA\u00c7\u00c3O DE \u00d3BITO FORNEC. M\u00c9DICO ASSIST"
      data$PA_MOTSAI[data$PA_MOTSAI=="42"] <- "\u00d3BITO COM DECLARA\u00c7\u00c3O DE \u00d3BITO FORNECIDA PELO I.M.L"
      data$PA_MOTSAI[data$PA_MOTSAI=="43"] <- "\u00d3BITO COM DECLARA\u00c7\u00c3O DE \u00d3BITO FORNECIDA PELO I.M.L"
      data$PA_MOTSAI[data$PA_MOTSAI=="51"] <- "ENCERRAMENTO ADMINSTRATIVO"
      data$PA_MOTSAI[data$PA_MOTSAI=="00"] <- "PRODU\u00c7\u00c3O SEM MOTIVO DE SA\u00cdDA (BPA-C / BPA-I)"
      data$PA_MOTSAI <- factor(data$PA_MOTSAI)
    }

    # PA_OBITO
    if("PA_OBITO" %in% variables_names){
      data$PA_OBITO[data$PA_OBITO=="1"] <- "Sim (motivo de sa\u00edda-\u00d3BITO)"
      data$PA_OBITO[data$PA_OBITO=="0"] <- "Nao houve \u00d3BITO"
      data$PA_OBITO <- factor(data$PA_OBITO)
    }

    # PA_ENCERR
    if("PA_ENCERR" %in% variables_names){
      data$PA_ENCERR[data$PA_ENCERR=="1"] <- "Sim (motivo de sa\u00edda-ENCERRAMENTO)"
      data$PA_ENCERR[data$PA_ENCERR=="0"] <- "Nao houve ENCERRAMENTO"
      data$PA_ENCERR <- factor(data$PA_ENCERR)
    }

    # PA_PERMAN
    if("PA_PERMAN" %in% variables_names){
      data$PA_PERMAN[data$PA_PERMAN=="1"] <- "Sim (motivo de sa\u00edda-PERMAN\u00caNCIA)"
      data$PA_PERMAN[data$PA_PERMAN=="0"] <- "Nao houve a PERMAN\u00caNCIA do paciente na unidade"
      data$PA_PERMAN <- factor(data$PA_PERMAN)
    }

    # PA_ALTA
    if("PA_ALTA" %in% variables_names){
      data$PA_ALTA[data$PA_ALTA=="1"] <- "Sim (motivo de sa\u00edda-PERMAN\u00caNCIA)"
      data$PA_ALTA[data$PA_ALTA=="0"] <- "Nao houve a PERMAN\u00caNCIA do paciente na unidade"
      data$PA_ALTA <- factor(data$PA_ALTA)
    }

    # PA_TRANSF
    if("PA_TRANSF" %in% variables_names){
      data$PA_TRANSF[data$PA_TRANSF=="1"] <- "Sim (motivo de sa\u00edda-TRANSFER\u00caNCIA)"
      data$PA_TRANSF[data$PA_TRANSF=="0"] <- "Nao houve TRANSFER\u00caNCIA do paciente"
      data$PA_TRANSF <- factor(data$PA_TRANSF)
    }

    # PA_CATEND
    if("PA_CATEND" %in% variables_names){
      data$PA_CATEND[data$PA_CATEND=="01"] <- "ELETIVO"
      data$PA_CATEND[data$PA_CATEND=="02"] <- "URG\u00caNCIA"
      data$PA_CATEND[data$PA_CATEND=="03"] <- "ACIDENTE NO LOCAL TRABALHO OU A SERVi\u00c7O DA EMPRESA"
      data$PA_CATEND[data$PA_CATEND=="04"] <- "ACIDENTE NO TRAJETO PARA O TRABALHO"
      data$PA_CATEND[data$PA_CATEND=="05"] <- "OUTROS TIPOS DE ACIDENTE DE TR\u00c2NSITO"
      data$PA_CATEND[data$PA_CATEND=="06"] <- "OUTROS TIPOS LES\u00d5ES/ENVENENAMENTOS(AGENT.FIS./QUIM."
      data$PA_CATEND[data$PA_CATEND=="99"] <- "INFORMA\u00c7\u00c3O INEXISTENTE  (BPA-C)"
      data$PA_CATEND[data$PA_CATEND=="00"] <- "CARATER DE ATENDIMENTO N\u00c3O INFORMADO"
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
      data$PA_IDADE <- as.numeric(data$PA_IDADE)
      data$PA_IDADE[data$PA_IDADE==999] <- NA
    }

    # IDADEMIN
    if("IDADEMIN" %in% variables_names){
      data$IDADEMIN <- as.numeric(data$IDADEMIN)
    }

    # IDADEMAX
    if("IDADEMAX" %in% variables_names){
      data$IDADEMAX <- as.numeric(data$IDADEMAX)
    }

    # PA_FLIDADE
    if("PA_FLIDADE" %in% variables_names){
      data$PA_FLIDADE[data$PA_FLIDADE=="0"] <- "IDADE N\u00c3O EXIGIDA"
      data$PA_FLIDADE[data$PA_FLIDADE=="1"] <- "IDADE COMPATIVEL COM O SIGTAP"
      data$PA_FLIDADE[data$PA_FLIDADE=="2"] <- "IDADE FORA DA FAIXA DO SIGTAP"
      data$PA_FLIDADE[data$PA_FLIDADE=="3"] <- "IDADE INEXISTENTE"
      data$PA_FLIDADE[data$PA_FLIDADE=="4"] <- "IDADE EM BRANCO"
      data$PA_FLIDADE <- factor(data$PA_FLIDADE)
    }

    # PA_SEXO
    if("PA_SEXO" %in% variables_names){
      data$PA_SEXO[data$PA_SEXO=="0"] <- "N\u00e3o exigido"
      data$PA_SEXO[data$PA_SEXO=="M"] <- "Masculino"
      data$PA_SEXO[data$PA_SEXO=="F"] <- "Feminino"
      data$PA_SEXO <- factor(data$PA_SEXO)
    }

    # PA_RACACOR
    if("PA_RACACOR" %in% variables_names){
      data$PA_RACACOR[data$PA_RACACOR=="00"] <- "RA\u00c7A/COR N\u00c3O EXIGIDO"
      data$PA_RACACOR[data$PA_RACACOR=="01"] <- "BRANCA"
      data$PA_RACACOR[data$PA_RACACOR=="02"] <- "PRETA"
      data$PA_RACACOR[data$PA_RACACOR=="03"] <- "PARDA"
      data$PA_RACACOR[data$PA_RACACOR=="04"] <- "AMARELA"
      data$PA_RACACOR[data$PA_RACACOR=="05"] <- "INDIGENA"
      data$PA_RACACOR[data$PA_RACACOR=="99"] <- "SEM INFORMA\u00c7\u00c3O"
      data$PA_RACACOR[data$PA_RACACOR=="06"] <- "RA\u00c7A/COR=06 (INDEVIDO)"
      data$PA_RACACOR[data$PA_RACACOR=="09"] <- "RA\u00c7A/COR=09 (INDEVIDO)"
      data$PA_RACACOR[data$PA_RACACOR=="1M"] <- "RA\u00c7A/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR[data$PA_RACACOR=="1G"] <- "RA\u00c7A/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR[data$PA_RACACOR=="1C"] <- "RA\u00c7A/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR[data$PA_RACACOR=="DE"] <- "RA\u00c7A/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR[data$PA_RACACOR=="D"] <- "RA\u00c7A/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR[data$PA_RACACOR=="87"] <- "RA\u00c7A/COR  (OUTROS INDEVIDOS)"
      data$PA_RACACOR <- factor(data$PA_RACACOR)
    }

    # PA_MUNPCN
    if("PA_MUNPCN" %in% variables_names & municipality_data == TRUE){
      colnames(tabMun)[1] <- "PA_MUNPCN"
      data$PA_MUNPCN <- as.integer(data$PA_MUNPCN)
      data <- dplyr::left_join(data, tabMun, by = "PA_MUNPCN")
    }

    # PA_QTDPRO
    if("PA_QTDPRO" %in% variables_names){
      data$PA_QTDPRO <- as.integer(data$PA_QTDPRO)
    }

    # PA_QTDAPR
    if("PA_QTDAPR" %in% variables_names){
      data$PA_QTDAPR <- as.numeric(data$PA_QTDAPR)
    }

    # PA_VALPRO
    if("PA_VALPRO" %in% variables_names){
      data$PA_VALPRO <- as.numeric(data$PA_VALPRO)
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
      data$PA_UFDIF[data$PA_UFDIF=="1"] <- "Sim (houve invas\u00e3o)"
      data$PA_UFDIF[data$PA_UFDIF=="0"] <- "N\u00e3o houve invas\u00e3o"
      data$PA_UFDIF <- factor(data$PA_UFDIF)
    }

    # PA_MNDIF
    if("PA_MNDIF" %in% variables_names){
      data$PA_MNDIF[data$PA_MNDIF=="1"] <- "Sim (houve invas\u00e3o)"
      data$PA_MNDIF[data$PA_MNDIF=="0"] <- "N\u00e3o houve invas\u00e3o"
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
      data$PA_INDICA[data$PA_INDICA=="5"] <- "Aprovado totalmente"
      data$PA_INDICA[data$PA_INDICA=="6"] <- "Aprovado parcialmente"
      data$PA_INDICA[data$PA_INDICA=="0"] <- "N\u00e3o aprovado"
      data$PA_INDICA <- factor(data$PA_INDICA)
    }

    # PA_ETNIA
    if("PA_ETNIA" %in% variables_names){
      data$PA_ETNIA[data$PA_ETNIA == "0001"] <- "ACONA (WAKONAS, NACONAS, JAKONA, ACORANES)"
      data$PA_ETNIA[data$PA_ETNIA == "0002"] <-	"AIKANA (AIKANA, MAS SAKA,TUBARAO)"
      data$PA_ETNIA[data$PA_ETNIA == "0003"] <-	"AJURU"
      data$PA_ETNIA[data$PA_ETNIA == "0004"] <-	"AKUNSU (AKUNT'SU)"
      data$PA_ETNIA[data$PA_ETNIA == "0005"] <-	"AMANAYE"
      data$PA_ETNIA[data$PA_ETNIA == "0006"] <-	"AMONDAWA"
      data$PA_ETNIA[data$PA_ETNIA == "0007"] <-	"ANAMBE"
      data$PA_ETNIA[data$PA_ETNIA == "0008"] <-	"APARAI (APALAI)"
      data$PA_ETNIA[data$PA_ETNIA == "0009"] <-	"APIAKA (APIACA)"
      data$PA_ETNIA[data$PA_ETNIA == "0010"] <-	"APINAYE (APINAJE/APINAIE/APINAGE)"
      data$PA_ETNIA[data$PA_ETNIA == "0011"] <-	"APURINA (APORINA, IPURINA, IPURINA, IPURINAN)"
      data$PA_ETNIA[data$PA_ETNIA == "0012"] <-	"ARANA (ARACUAI DO VALE DO JEQUITINHONHA)"
      data$PA_ETNIA[data$PA_ETNIA == "0013"] <-	"ARAPASO (ARAPACO)"
      data$PA_ETNIA[data$PA_ETNIA == "0014"] <-	"ARARA DE RONDONIA (KARO, URUCU, URUKU)"
      data$PA_ETNIA[data$PA_ETNIA == "0015"] <-	"ARARA DO ACRE (SHAWANAUA, AMAWAKA)"
      data$PA_ETNIA[data$PA_ETNIA == "0016"] <-	"ARARA DO ARIPUANA (ARARA DO BEIRADAO/ARI-PUANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0017"] <-	"ARARA DO PARA (UKARAGMA, UKARAMMA)"
      data$PA_ETNIA[data$PA_ETNIA == "0018"] <-	"ARAWETE (ARAUETE)"
      data$PA_ETNIA[data$PA_ETNIA == "0019"] <-	"ARIKAPU (ARICAPU, ARIKAPO, MASUBI, MAXUBI)"
      data$PA_ETNIA[data$PA_ETNIA == "0020"] <-	"ARIKEM (ARIQUEN, ARIQUEME, ARIKEME)"
      data$PA_ETNIA[data$PA_ETNIA == "0021"] <-	"ARIKOSE (ARICOBE)"
      data$PA_ETNIA[data$PA_ETNIA == "0022"] <-	"ARUA"
      data$PA_ETNIA[data$PA_ETNIA == "0023"] <-	"ARUAK (ARAWAK)"
      data$PA_ETNIA[data$PA_ETNIA == "0024"] <-	"ASHANINKA (KAMPA)"
      data$PA_ETNIA[data$PA_ETNIA == "0025"] <-	"ASURINI DO TOCANTINS (AKUAWA/AKWAWA)"
      data$PA_ETNIA[data$PA_ETNIA == "0026"] <-	"ASURINI DO XINGU (AWAETE)"
      data$PA_ETNIA[data$PA_ETNIA == "0027"] <-	"ATIKUM (ATICUM)"
      data$PA_ETNIA[data$PA_ETNIA == "0028"] <-	"AVA - CANOEIRO"
      data$PA_ETNIA[data$PA_ETNIA == "0029"] <-	"AWETI (AUETI/AUETO)"
      data$PA_ETNIA[data$PA_ETNIA == "0030"] <-	"BAKAIRI (KURA, BACAIRI)"
      data$PA_ETNIA[data$PA_ETNIA == "0031"] <-	"BANAWA YAFI (BANAWA, BANAWA-JAFI)"
      data$PA_ETNIA[data$PA_ETNIA == "0032"] <-	"BANIWA (BANIUA, BANIVA, WALIMANAI, WAKUENAI)"
      data$PA_ETNIA[data$PA_ETNIA == "0033"] <-	"BARA (WAIPINOMAKA)"
      data$PA_ETNIA[data$PA_ETNIA == "0034"] <-	"BARASANA (HANERA)"
      data$PA_ETNIA[data$PA_ETNIA == "0035"] <-	"BARE"
      data$PA_ETNIA[data$PA_ETNIA == "0036"] <-	"BORORO (BOE)"
      data$PA_ETNIA[data$PA_ETNIA == "0037"] <-	"BOTOCUDO (GEREN)"
      data$PA_ETNIA[data$PA_ETNIA == "0038"] <-	"CANOE"
      data$PA_ETNIA[data$PA_ETNIA == "0039"] <-	"CASSUPA"
      data$PA_ETNIA[data$PA_ETNIA == "0040"] <-	"CHAMACOCO"
      data$PA_ETNIA[data$PA_ETNIA == "0041"] <-	"CHIQUITANO (XIQUITANO)"
      data$PA_ETNIA[data$PA_ETNIA == "0042"] <-	"CIKIYANA (SIKIANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0043"] <-	"CINTA LARGA (MATETAMAE)"
      data$PA_ETNIA[data$PA_ETNIA == "0044"] <-	"COLUMBIARA (CORUMBIARA)"
      data$PA_ETNIA[data$PA_ETNIA == "0045"] <-	"DENI"
      data$PA_ETNIA[data$PA_ETNIA == "0046"] <-	"DESANA (DESANA, DESANO, DESSANO, WIRA, UMUKOMASA)"
      data$PA_ETNIA[data$PA_ETNIA == "0047"] <-	"DIAHUI (JAHOI, JAHUI, DIARROI)"
      data$PA_ETNIA[data$PA_ETNIA == "0048"] <-	"ENAWENE-NAWE (SALUMA)"
      data$PA_ETNIA[data$PA_ETNIA == "0049"] <-	"FULNI-O"
      data$PA_ETNIA[data$PA_ETNIA == "0050"] <-	"GALIBI (GALIBI DO OIAPOQUE, KARINHA)"
      data$PA_ETNIA[data$PA_ETNIA == "0051"] <-	"GALIBI MARWORNO (GALIBI DO UACA, ARUA)"
      data$PA_ETNIA[data$PA_ETNIA == "0052"] <-	"GAVIAO DE RONDONIA (DIGUT)"
      data$PA_ETNIA[data$PA_ETNIA == "0053"] <-	"GAVIAO KRIKATEJE"
      data$PA_ETNIA[data$PA_ETNIA == "0054"] <-	"GAVIAO PARKATEJE (PARKATEJE)"
      data$PA_ETNIA[data$PA_ETNIA == "0055"] <-	"GAVIAO PUKOBIE (PUKOBIE, PYKOPJE, GAVIAO DO MARANHAO)"
      data$PA_ETNIA[data$PA_ETNIA == "0056"] <-	"GUAJA (AWA, AVA)"
      data$PA_ETNIA[data$PA_ETNIA == "0057"] <-	"GUAJAJARA (TENETEHARA)"
      data$PA_ETNIA[data$PA_ETNIA == "0058"] <-	"GUARANI KAIOWA (PAI TAVYTERA)"
      data$PA_ETNIA[data$PA_ETNIA == "0059"] <-	"GUARANI M'BYA"
      data$PA_ETNIA[data$PA_ETNIA == "0060"] <-	"GUARANI NANDEVA (AVAKATUETE, CHIRIPA,NHANDEWA, AVA GUARANI)"
      data$PA_ETNIA[data$PA_ETNIA == "0061"] <-	"GUATO"
      data$PA_ETNIA[data$PA_ETNIA == "0062"] <-	"HIMARIMA (HIMERIMA)"
      data$PA_ETNIA[data$PA_ETNIA == "0063"] <-	"INGARIKO (INGARICO, AKAWAIO, KAPON)"
      data$PA_ETNIA[data$PA_ETNIA == "0064"] <-	"IRANXE (IRANTXE)"
      data$PA_ETNIA[data$PA_ETNIA == "0065"] <-	"ISSE"
      data$PA_ETNIA[data$PA_ETNIA == "0066"] <-	"JABOTI (JABUTI, KIPIU, YABYTI)"
      data$PA_ETNIA[data$PA_ETNIA == "0067"] <-	"JAMAMADI (YAMAMADI, DJEOROMITXI)"
      data$PA_ETNIA[data$PA_ETNIA == "0068"] <-	"JARAWARA"
      data$PA_ETNIA[data$PA_ETNIA == "0069"] <-	"JIRIPANCO (JERIPANCO, GERIPANCO)"
      data$PA_ETNIA[data$PA_ETNIA == "0070"] <-	"JUMA (YUMA)"
      data$PA_ETNIA[data$PA_ETNIA == "0071"] <-	"JURUNA"
      data$PA_ETNIA[data$PA_ETNIA == "0072"] <-	"JURUTI (YURITI)"
      data$PA_ETNIA[data$PA_ETNIA == "0073"] <-	"KAAPOR (URUBU-KAAPOR, KA'APOR, KAAPORTE)"
      data$PA_ETNIA[data$PA_ETNIA == "0074"] <-	"KADIWEU (CADUVEO, CADIUEU)"
      data$PA_ETNIA[data$PA_ETNIA == "0075"] <-	"KAIABI (CAIABI, KAYABI)"
      data$PA_ETNIA[data$PA_ETNIA == "0076"] <-	"KAIMBE (CAIMBE)"
      data$PA_ETNIA[data$PA_ETNIA == "0077"] <-	"KAINGANG (CAINGANGUE)"
      data$PA_ETNIA[data$PA_ETNIA == "0078"] <-	"KAIXANA (CAIXANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0079"] <-	"KALABASSA (CALABASSA, CALABACAS)"
      data$PA_ETNIA[data$PA_ETNIA == "0080"] <-	"KALANCO"
      data$PA_ETNIA[data$PA_ETNIA == "0081"] <-	"KALAPALO (CALAPALO)"
      data$PA_ETNIA[data$PA_ETNIA == "0082"] <-	"KAMAYURA (CAMAIURA, KAMAIURA)"
      data$PA_ETNIA[data$PA_ETNIA == "0083"] <-	"KAMBA (CAMBA)"
      data$PA_ETNIA[data$PA_ETNIA == "0084"] <-	"KAMBEBA (CAMBEBA, OMAGUA)"
      data$PA_ETNIA[data$PA_ETNIA == "0085"] <-	"KAMBIWA (CAMBIUA)"
      data$PA_ETNIA[data$PA_ETNIA == "0086"] <-	"KAMBIWA PIPIPA (PIPIPA)"
      data$PA_ETNIA[data$PA_ETNIA == "0087"] <-	"KAMPE"
      data$PA_ETNIA[data$PA_ETNIA == "0088"] <-	"KANAMANTI (KANAMATI, CANAMANTI)"
      data$PA_ETNIA[data$PA_ETNIA == "0089"] <-	"KANAMARI (CANAMARI, KANAMARY, TUKUNA)"
      data$PA_ETNIA[data$PA_ETNIA == "0090"] <-	"KANELA APANIEKRA (CANELA)"
      data$PA_ETNIA[data$PA_ETNIA == "0091"] <-	"KANELA RANKOKAMEKRA (CANELA)"
      data$PA_ETNIA[data$PA_ETNIA == "0092"] <-	"KANINDE"
      data$PA_ETNIA[data$PA_ETNIA == "0093"] <-	"KANOE (CANOE)"
      data$PA_ETNIA[data$PA_ETNIA == "0094"] <-	"KANTARURE (CANTARURE)"
      data$PA_ETNIA[data$PA_ETNIA == "0095"] <-	"KAPINAWA (CAPINAUA)"
      data$PA_ETNIA[data$PA_ETNIA == "0096"] <-	"KARAJA (CARAJA)"
      data$PA_ETNIA[data$PA_ETNIA == "0097"] <-	"KARAJA/JAVAE (JAVAE)"
      data$PA_ETNIA[data$PA_ETNIA == "0098"] <-	"KARAJA/XAMBIOA (KARAJA DO NORTE)"
      data$PA_ETNIA[data$PA_ETNIA == "0099"] <-	"KARAPANA (CARAPANA, MUTEAMASA, UKOPINOPONA)"
      data$PA_ETNIA[data$PA_ETNIA == "0100"] <-	"KARAPOTO (CARAPOTO)"
      data$PA_ETNIA[data$PA_ETNIA == "0101"] <-	"KARIPUNA (CARIPUNA)"
      data$PA_ETNIA[data$PA_ETNIA == "0102"] <-	"KARIPUNA DO AMAPA (CARIPUNA)"
      data$PA_ETNIA[data$PA_ETNIA == "0103"] <-	"KARIRI (CARIRI)"
      data$PA_ETNIA[data$PA_ETNIA == "0104"] <-	"KARIRI-XOCO (CARIRI-CHOCO)"
      data$PA_ETNIA[data$PA_ETNIA == "0105"] <-	"KARITIANA (CARITIANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0106"] <-	"KATAWIXI (KATAUIXI,KATAWIN, KATAWISI, CATAUICHI)"
      data$PA_ETNIA[data$PA_ETNIA == "0107"] <-	"KATUENA (CATUENA, KATWENA)"
      data$PA_ETNIA[data$PA_ETNIA == "0108"] <-	"KATUKINA (PEDA DJAPA)"
      data$PA_ETNIA[data$PA_ETNIA == "0109"] <-	"KATUKINA DO ACRE"
      data$PA_ETNIA[data$PA_ETNIA == "0110"] <-	"KAXARARI (CAXARARI)"
      data$PA_ETNIA[data$PA_ETNIA == "0111"] <-	"KAXINAWA (HUNI-KUIN, CASHINAUA, CAXINAUA)"
      data$PA_ETNIA[data$PA_ETNIA == "0112"] <-	"KAXIXO"
      data$PA_ETNIA[data$PA_ETNIA == "0113"] <-	"KAXUYANA (CAXUIANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0114"] <-	"KAYAPO (CAIAPO)"
      data$PA_ETNIA[data$PA_ETNIA == "0115"] <-	"KAYAPO KARARAO (KARARAO)"
      data$PA_ETNIA[data$PA_ETNIA == "0116"] <-	"KAYAPO TXUKAHAMAE (TXUKAHAMAE)"
      data$PA_ETNIA[data$PA_ETNIA == "0117"] <-	"KAYAPO XICRIM (XIKRIN)"
      data$PA_ETNIA[data$PA_ETNIA == "0118"] <-	"KAYUISANA (CAIXANA, CAUIXANA, KAIXANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0119"] <-	"KINIKINAWA (GUAN, KOINUKOEN, KINIKINAO)"
      data$PA_ETNIA[data$PA_ETNIA == "0120"] <-	"KIRIRI"
      data$PA_ETNIA[data$PA_ETNIA == "0121"] <-	"KOCAMA (COCAMA, KOKAMA)"
      data$PA_ETNIA[data$PA_ETNIA == "0122"] <-	"KOKUIREGATEJE"
      data$PA_ETNIA[data$PA_ETNIA == "0123"] <-	"KORUBO"
      data$PA_ETNIA[data$PA_ETNIA == "0124"] <-	"KRAHO (CRAO, KRAO)"
      data$PA_ETNIA[data$PA_ETNIA == "0125"] <-	"KREJE (KRENYE)"
      data$PA_ETNIA[data$PA_ETNIA == "0126"] <-	"KRENAK (BORUN, CRENAQUE)"
      data$PA_ETNIA[data$PA_ETNIA == "0127"] <-	"KRIKATI (KRINKATI)"
      data$PA_ETNIA[data$PA_ETNIA == "0128"] <-	"KUBEO (CUBEO, COBEWA, KUBEWA, PAMIWA, CUBEU)"
      data$PA_ETNIA[data$PA_ETNIA == "0129"] <-	"KUIKURO (KUIKURU, CUICURO)"
      data$PA_ETNIA[data$PA_ETNIA == "0130"] <-	"KUJUBIM (KUYUBI, CUJUBIM)"
      data$PA_ETNIA[data$PA_ETNIA == "0131"] <-	"KULINA PANO (CULINA)"
      data$PA_ETNIA[data$PA_ETNIA == "0132"] <-	"KULINA/MADIHA (CULINA, MADIJA, MADIHA)"
      data$PA_ETNIA[data$PA_ETNIA == "0133"] <-	"KURIPAKO (CURIPACO, CURRIPACO, CORIPACO, WAKUENAI)"
      data$PA_ETNIA[data$PA_ETNIA == "0134"] <-	"KURUAIA (CURUAIA)"
      data$PA_ETNIA[data$PA_ETNIA == "0135"] <-	"KWAZA (COAIA, KOAIA)"
      data$PA_ETNIA[data$PA_ETNIA == "0136"] <-	"MACHINERI (MANCHINERI, MANXINERI)"
      data$PA_ETNIA[data$PA_ETNIA == "0137"] <-	"MACURAP (MAKURAP)"
      data$PA_ETNIA[data$PA_ETNIA == "0138"] <-	"MAKU DOW (DOW)"
      data$PA_ETNIA[data$PA_ETNIA == "0139"] <-	"MAKU HUPDA (HUPDA)"
      data$PA_ETNIA[data$PA_ETNIA == "0140"] <-	"MAKU NADEB (NADEB)"
      data$PA_ETNIA[data$PA_ETNIA == "0141"] <-	"MAKU YUHUPDE (YUHUPDE)"
      data$PA_ETNIA[data$PA_ETNIA == "0142"] <-	"MAKUNA (MACUNA, YEBA-MASA)"
      data$PA_ETNIA[data$PA_ETNIA == "0143"] <-	"MAKUXI (MACUXI, MACHUSI, PEMON)"
      data$PA_ETNIA[data$PA_ETNIA == "0144"] <-	"MARIMAM (MARIMA)"
      data$PA_ETNIA[data$PA_ETNIA == "0145"] <-	"MARUBO"
      data$PA_ETNIA[data$PA_ETNIA == "0146"] <-	"MATIPU"
      data$PA_ETNIA[data$PA_ETNIA == "0147"] <-	"MATIS"
      data$PA_ETNIA[data$PA_ETNIA == "0148"] <-	"MATSE (MAYORUNA)"
      data$PA_ETNIA[data$PA_ETNIA == "0149"] <-	"MAXAKALI (MAXACALI)"
      data$PA_ETNIA[data$PA_ETNIA == "0150"] <-	"MAYA (MAYA)"
      data$PA_ETNIA[data$PA_ETNIA == "0151"] <-	"MAYTAPU"
      data$PA_ETNIA[data$PA_ETNIA == "0152"] <-	"MEHINAKO (MEINAKU, MEINACU)"
      data$PA_ETNIA[data$PA_ETNIA == "0153"] <-	"MEKEN (MEQUEM, MEKHEM, MICHENS)"
      data$PA_ETNIA[data$PA_ETNIA == "0154"] <-	"MENKY (MYKY, MUNKU, MENKI, MYNKY)"
      data$PA_ETNIA[data$PA_ETNIA == "0155"] <-	"MIRANHA (MIRANHA, MIRANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0156"] <-	"MIRITI TAPUIA (MIRITI-TAPUYA, BUIA-TAPUYA)"
      data$PA_ETNIA[data$PA_ETNIA == "0157"] <-	"MUNDURUKU (MUNDURUCU)"
      data$PA_ETNIA[data$PA_ETNIA == "0158"] <-	"MURA"
      data$PA_ETNIA[data$PA_ETNIA == "0159"] <-	"NAHUKWA (NAFUQUA)"
      data$PA_ETNIA[data$PA_ETNIA == "0160"] <-	"NAMBIKWARA DO CAMPO (HALOTESU, KITHAULU, WAKALITESU, SAWENTES, MANDUKA)"
      data$PA_ETNIA[data$PA_ETNIA == "0161"] <-	"NAMBIKWARA DO NORTE (NEGAROTE ,MAMAINDE, LATUNDE, SABANE E MANDUKA, TAWANDE)"
      data$PA_ETNIA[data$PA_ETNIA == "0162"] <-	"NAMBIKWARA DO SUL (WASUSU ,HAHAINTESU, ALANTESU, WAIKISU, ALAKETESU, WASUSU, SARARE)"
      data$PA_ETNIA[data$PA_ETNIA == "0163"] <-	"NARAVUTE (NARUVOTO)"
      data$PA_ETNIA[data$PA_ETNIA == "0164"] <-	"NAWA (NAUA)"
      data$PA_ETNIA[data$PA_ETNIA == "0165"] <-	"NUKINI (NUQUINI, NUKUINI)"
      data$PA_ETNIA[data$PA_ETNIA == "0166"] <-	"OFAIE (OFAYE-XAVANTE)"
      data$PA_ETNIA[data$PA_ETNIA == "0167"] <-	"ORO WIN"
      data$PA_ETNIA[data$PA_ETNIA == "0168"] <-	"PAIAKU (JENIPAPO-KANINDE)"
      data$PA_ETNIA[data$PA_ETNIA == "0169"] <-	"PAKAA NOVA (WARI, PACAAS NOVOS)"
      data$PA_ETNIA[data$PA_ETNIA == "0170"] <-	"PALIKUR (AUKWAYENE, AUKUYENE, PALIKU'ENE)"
      data$PA_ETNIA[data$PA_ETNIA == "0171"] <-	"PANARA (KRENHAKARORE , KRENAKORE, KRENA-KARORE)"
      data$PA_ETNIA[data$PA_ETNIA == "0172"] <-	"PANKARARE (PANCARARE)"
      data$PA_ETNIA[data$PA_ETNIA == "0173"] <-	"PANKARARU (PANCARARU)"
      data$PA_ETNIA[data$PA_ETNIA == "0174"] <-	"PANKARARU KALANKO (KALANKO)"
      data$PA_ETNIA[data$PA_ETNIA == "0175"] <-	"PANKARARU KARUAZU (KARUAZU)"
      data$PA_ETNIA[data$PA_ETNIA == "0176"] <-	"PANKARU (PANCARU)"
      data$PA_ETNIA[data$PA_ETNIA == "0177"] <-	"PARAKANA (PARACANA, APITEREWA, AWAETE)"
      data$PA_ETNIA[data$PA_ETNIA == "0178"] <-	"PARECI (PARESI, HALITI)"
      data$PA_ETNIA[data$PA_ETNIA == "0179"] <-	"PARINTINTIN"
      data$PA_ETNIA[data$PA_ETNIA == "0180"] <-	"PATAMONA (KAPON)"
      data$PA_ETNIA[data$PA_ETNIA == "0181"] <-	"PATAXO"
      data$PA_ETNIA[data$PA_ETNIA == "0182"] <-	"PATAXO HA-HA-HAE"
      data$PA_ETNIA[data$PA_ETNIA == "0183"] <-	"PAUMARI (PALMARI)"
      data$PA_ETNIA[data$PA_ETNIA == "0184"] <-	"PAUMELENHO"
      data$PA_ETNIA[data$PA_ETNIA == "0185"] <-	"PIRAHA (MURA PIRAHA)"
      data$PA_ETNIA[data$PA_ETNIA == "0186"] <-	"PIRATUAPUIA (PIRATAPUYA, PIRATAPUYO, PIRA-TAPUYA, WAIKANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0187"] <-	"PITAGUARI"
      data$PA_ETNIA[data$PA_ETNIA == "0188"] <-	"POTIGUARA"
      data$PA_ETNIA[data$PA_ETNIA == "0189"] <-	"POYANAWA (POIANAUA)"
      data$PA_ETNIA[data$PA_ETNIA == "0190"] <-	"RIKBAKTSA (CANOEIROS, ERIGPAKTSA)"
      data$PA_ETNIA[data$PA_ETNIA == "0191"] <-	"SAKURABIAT(MEKENS, SAKIRABIAP, SAKIRABIAR)"
      data$PA_ETNIA[data$PA_ETNIA == "0192"] <-	"SATERE-MAWE (SATERE-MAUE)"
      data$PA_ETNIA[data$PA_ETNIA == "0193"] <-	"SHANENAWA (KATUKINA)"
      data$PA_ETNIA[data$PA_ETNIA == "0194"] <-	"SIRIANO (SIRIA-MASA)"
      data$PA_ETNIA[data$PA_ETNIA == "0195"] <-	"SURIANA"
      data$PA_ETNIA[data$PA_ETNIA == "0196"] <-	"SURUI DE RONDONIA (PAITER)"
      data$PA_ETNIA[data$PA_ETNIA == "0197"] <-	"SURUI DO PARA (AIKEWARA)"
      data$PA_ETNIA[data$PA_ETNIA == "0198"] <-	"SUYA (SUIA/KISEDJE)"
      data$PA_ETNIA[data$PA_ETNIA == "0199"] <-	"TAPAYUNA (BEICO-DE-PAU)"
      data$PA_ETNIA[data$PA_ETNIA == "0200"] <-	"TAPEBA"
      data$PA_ETNIA[data$PA_ETNIA == "0201"] <-	"TAPIRAPE (TAPI'IRAPE)"
      data$PA_ETNIA[data$PA_ETNIA == "0202"] <-	"TAPUIA (TAPUIA-XAVANTE, TAPUIO)"
      data$PA_ETNIA[data$PA_ETNIA == "0203"] <-	"TARIANO (TARIANA, TALIASERI)"
      data$PA_ETNIA[data$PA_ETNIA == "0204"] <-	"TAUREPANG (TAULIPANG, PEMON, AREKUNA, PAGEYN)"
      data$PA_ETNIA[data$PA_ETNIA == "0205"] <-	"TEMBE"
      data$PA_ETNIA[data$PA_ETNIA == "0206"] <-	"TENHARIM"
      data$PA_ETNIA[data$PA_ETNIA == "0207"] <-	"TERENA"
      data$PA_ETNIA[data$PA_ETNIA == "0208"] <-	"TICUNA (TIKUNA, TUKUNA, MAGUTA)"
      data$PA_ETNIA[data$PA_ETNIA == "0209"] <-	"TINGUI BOTO"
      data$PA_ETNIA[data$PA_ETNIA == "0210"] <-	"TIRIYO EWARHUYANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)"
      data$PA_ETNIA[data$PA_ETNIA == "0211"] <-	"TIRIYO KAH'YANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)"
      data$PA_ETNIA[data$PA_ETNIA == "0212"] <-	"TIRIYO TSIKUYANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)"
      data$PA_ETNIA[data$PA_ETNIA == "0213"] <-	"TORA"
      data$PA_ETNIA[data$PA_ETNIA == "0214"] <-	"TREMEMBE"
      data$PA_ETNIA[data$PA_ETNIA == "0215"] <-	"TRUKA"
      data$PA_ETNIA[data$PA_ETNIA == "0216"] <-	"TRUMAI"
      data$PA_ETNIA[data$PA_ETNIA == "0217"] <-	"TSOHOM DJAPA (TSUNHUM-DJAPA)"
      data$PA_ETNIA[data$PA_ETNIA == "0218"] <-	"TUKANO (TUCANO, YE'PA-MASA, DASEA)"
      data$PA_ETNIA[data$PA_ETNIA == "0219"] <-	"TUMBALALA"
      data$PA_ETNIA[data$PA_ETNIA == "0220"] <-	"TUNAYANA"
      data$PA_ETNIA[data$PA_ETNIA == "0221"] <-	"TUPARI"
      data$PA_ETNIA[data$PA_ETNIA == "0222"] <-	"TUPINAMBA"
      data$PA_ETNIA[data$PA_ETNIA == "0223"] <-	"TUPINIQUIM"
      data$PA_ETNIA[data$PA_ETNIA == "0224"] <-	"TURIWARA"
      data$PA_ETNIA[data$PA_ETNIA == "0225"] <-	"TUXA"
      data$PA_ETNIA[data$PA_ETNIA == "0226"] <-	"TUYUKA (TUIUCA, DOKAPUARA, UTAPINOMAKAPHONA)"
      data$PA_ETNIA[data$PA_ETNIA == "0227"] <-	"TXIKAO (TXICAO, IKPENG)"
      data$PA_ETNIA[data$PA_ETNIA == "0228"] <-	"UMUTINA (OMOTINA, BARBADOS)"
      data$PA_ETNIA[data$PA_ETNIA == "0229"] <-	"URU-EU-WAU-WAU (URUEU-UAU-UAU, URUPAIN, URUPA)"
      data$PA_ETNIA[data$PA_ETNIA == "0230"] <-	"WAI WAI HIXKARYANA (HIXKARYANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0231"] <-	"WAI WAI KARAFAWYANA (KARAFAWYANA, KARA-PAWYANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0232"] <-	"WAI WAI XEREU (XEREU)"
      data$PA_ETNIA[data$PA_ETNIA == "0233"] <-	"WAI WAI KATUENA (KATUENA)"
      data$PA_ETNIA[data$PA_ETNIA == "0234"] <-	"WAI WAI MAWAYANA (MAWAYANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0235"] <-	"WAIAPI (WAYAMPI, OYAMPI, WAYAPY, )"
      data$PA_ETNIA[data$PA_ETNIA == "0236"] <-	"WAIMIRI ATROARI (KINA)"
      data$PA_ETNIA[data$PA_ETNIA == "0237"] <-	"WANANO (UANANO, WANANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0238"] <-	"WAPIXANA (UAPIXANA, VAPIDIANA, WAPISIANA, WAPISHANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0239"] <-	"WAREKENA (UAREQUENA, WEREKENA)"
      data$PA_ETNIA[data$PA_ETNIA == "0240"] <-	"WASSU"
      data$PA_ETNIA[data$PA_ETNIA == "0241"] <-	"WAURA (UAURA, WAUJA)"
      data$PA_ETNIA[data$PA_ETNIA == "0242"] <-	"WAYANA (WAIANA, UAIANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0243"] <-	"WITOTO (UITOTO, HUITOTO)"
      data$PA_ETNIA[data$PA_ETNIA == "0244"] <-	"XAKRIABA (XACRIABA)"
      data$PA_ETNIA[data$PA_ETNIA == "0245"] <-	"XAVANTE (A'UWE, AKWE, AWEN, AKWEN)"
      data$PA_ETNIA[data$PA_ETNIA == "0246"] <-	"XERENTE (AKWE, AWEN, AKWEN)"
      data$PA_ETNIA[data$PA_ETNIA == "0247"] <-	"XETA"
      data$PA_ETNIA[data$PA_ETNIA == "0248"] <-	"XIPAIA (SHIPAYA, XIPAYA)"
      data$PA_ETNIA[data$PA_ETNIA == "0249"] <-	"XOKLENG (SHOKLENG, XOCLENG)"
      data$PA_ETNIA[data$PA_ETNIA == "0250"] <-	"XOKO (XOCO, CHOCO)"
      data$PA_ETNIA[data$PA_ETNIA == "0251"] <-	"XUKURU (XUCURU)"
      data$PA_ETNIA[data$PA_ETNIA == "0252"] <-	"XUKURU KARIRI (XUCURU-KARIRI)"
      data$PA_ETNIA[data$PA_ETNIA == "0253"] <-	"YAIPIYANA"
      data$PA_ETNIA[data$PA_ETNIA == "0254"] <-	"YAMINAWA (JAMINAWA, IAMINAWA)"
      data$PA_ETNIA[data$PA_ETNIA == "0255"] <-	"YANOMAMI NINAM (IANOMAMI, IANOAMA, XIRIANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0256"] <-	"YANOMAMI SANUMA (IANOMAMI, IANOAMA, XIRIANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0257"] <-	"YANOMAMI YANOMAM (IANOMAMI, IANOAMA, XIRIANA)"
      data$PA_ETNIA[data$PA_ETNIA == "0258"] <-	"YAWALAPITI (IAUALAPITI)"
      data$PA_ETNIA[data$PA_ETNIA == "0259"] <-	"YAWANAWA (IAUANAUA)"
      data$PA_ETNIA[data$PA_ETNIA == "0260"] <-	"YEKUANA (MAIONGON, YE'KUANA, YEKWANA, MAYONGONG)"
      data$PA_ETNIA[data$PA_ETNIA == "0261"] <-	"YUDJA (JURUNA, YURUNA)"
      data$PA_ETNIA[data$PA_ETNIA == "0262"] <-	"ZO'E (POTURU)"
      data$PA_ETNIA[data$PA_ETNIA == "0263"] <-	"ZORO (PAGEYN)"
      data$PA_ETNIA[data$PA_ETNIA == "0264"] <-	"ZURUAHA (SOROWAHA, SURUWAHA)"
      data$PA_ETNIA[data$PA_ETNIA == "X265"] <-	"AHANENAWA"
      data$PA_ETNIA[data$PA_ETNIA == "X266"] <-	"AICABA"
      data$PA_ETNIA[data$PA_ETNIA == "X267"] <-	"AIKAN\\u00c3-KWAS\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X268"] <-	"AKUNTSU"
      data$PA_ETNIA[data$PA_ETNIA == "X269"] <-	"ALANTESU"
      data$PA_ETNIA[data$PA_ETNIA == "X271"] <-	"AMAW\\u00c1KA"
      data$PA_ETNIA[data$PA_ETNIA == "X272"] <-	"ANAC\\u00c9"
      data$PA_ETNIA[data$PA_ETNIA == "X273"] <-	"APURIN\\u00c3"
      data$PA_ETNIA[data$PA_ETNIA == "X274"] <-	"ARAN\\u00c3"
      data$PA_ETNIA[data$PA_ETNIA == "X275"] <-	"ARAPA\\u00c7O"
      data$PA_ETNIA[data$PA_ETNIA == "X276"] <-	"ARARA APOLIMA"
      data$PA_ETNIA[data$PA_ETNIA == "X277"] <-	"ARARA DO ARIPUANA"
      data$PA_ETNIA[data$PA_ETNIA == "X278"] <-	"ARIPUAN\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X279"] <-	"ASSURINI"
      data$PA_ETNIA[data$PA_ETNIA == "X280"] <-	"AWUAR\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X281"] <-	"BORBA"
      data$PA_ETNIA[data$PA_ETNIA == "X282"] <-	"CABIXI"
      data$PA_ETNIA[data$PA_ETNIA == "X283"] <-	"CAMARAR\\u00c9"
      data$PA_ETNIA[data$PA_ETNIA == "X284"] <-	"CAMASURI"
      data$PA_ETNIA[data$PA_ETNIA == "X285"] <-	"CARA PRETA"
      data$PA_ETNIA[data$PA_ETNIA == "X286"] <-	"CHARRUA"
      data$PA_ETNIA[data$PA_ETNIA == "X287"] <-	"CUJUBIM"
      data$PA_ETNIA[data$PA_ETNIA == "X288"] <-	"DAW"
      data$PA_ETNIA[data$PA_ETNIA == "X289"] <-	"GAVI\\u00c3O"
      data$PA_ETNIA[data$PA_ETNIA == "X290"] <-	"GUARANI"
      data$PA_ETNIA[data$PA_ETNIA == "X291"] <-	"HALANTESU"
      data$PA_ETNIA[data$PA_ETNIA == "X292"] <-	"HALOTESU"
      data$PA_ETNIA[data$PA_ETNIA == "X293"] <-	"HENGAT\\u00da"
      data$PA_ETNIA[data$PA_ETNIA == "X294"] <-	"HIXKARYANA"
      data$PA_ETNIA[data$PA_ETNIA == "X295"] <-	"HUPDE"
      data$PA_ETNIA[data$PA_ETNIA == "X296"] <-	"HUPDES"
      data$PA_ETNIA[data$PA_ETNIA == "X297"] <-	"IAUANAUA"
      data$PA_ETNIA[data$PA_ETNIA == "X298"] <-	"IAUARETE A\\u00c7U"
      data$PA_ETNIA[data$PA_ETNIA == "X299"] <-	"IKPENG"
      data$PA_ETNIA[data$PA_ETNIA == "X300"] <-	"INAMBU"
      data$PA_ETNIA[data$PA_ETNIA == "X301"] <-	"INHABARANA"
      data$PA_ETNIA[data$PA_ETNIA == "X302"] <-	"JAVAE"
      data$PA_ETNIA[data$PA_ETNIA == "X303"] <-	"JENIPAPO"
      data$PA_ETNIA[data$PA_ETNIA == "X304"] <-	"JENIPAPO-KANINDE"
      data$PA_ETNIA[data$PA_ETNIA == "X305"] <-	"JIAHOI"
      data$PA_ETNIA[data$PA_ETNIA == "X306"] <-	"KAIOWA"
      data$PA_ETNIA[data$PA_ETNIA == "X307"] <-	"KAMPA"
      data$PA_ETNIA[data$PA_ETNIA == "X308"] <-	"KANELA"
      data$PA_ETNIA[data$PA_ETNIA == "X309"] <-	"KARAFAWYANA"
      data$PA_ETNIA[data$PA_ETNIA == "X310"] <-	"KARARAO"
      data$PA_ETNIA[data$PA_ETNIA == "X311"] <-	"KARUBO"
      data$PA_ETNIA[data$PA_ETNIA == "X312"] <-	"KASSUP\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X313"] <-	"KATITH\\u00c3ULU"
      data$PA_ETNIA[data$PA_ETNIA == "X314"] <-	"KATOKIN"
      data$PA_ETNIA[data$PA_ETNIA == "X315"] <-	"KATUKINA PANO"
      data$PA_ETNIA[data$PA_ETNIA == "X316"] <-	"KATUKINA PEDA DJAPA"
      data$PA_ETNIA[data$PA_ETNIA == "X317"] <-	"KATUKINA SHANENAUWA"
      data$PA_ETNIA[data$PA_ETNIA == "X318"] <-	"KAXAGO"
      data$PA_ETNIA[data$PA_ETNIA == "X319"] <-	"KAYABI"
      data$PA_ETNIA[data$PA_ETNIA == "X320"] <-	"KIN\\u00c3 (WAIMIRI-ATROARI)"
      data$PA_ETNIA[data$PA_ETNIA == "X321"] <-	"KIRIRI-BARRA"
      data$PA_ETNIA[data$PA_ETNIA == "X322"] <-	"KITH\\u00c3ULU"
      data$PA_ETNIA[data$PA_ETNIA == "X323"] <-	"KOIAI\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X324"] <-	"KOIUPANK\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X325"] <-	"KONTANAWA"
      data$PA_ETNIA[data$PA_ETNIA == "X326"] <-	"KRAH\\u00d4 KANELA"
      data$PA_ETNIA[data$PA_ETNIA == "X327"] <-	"KULINA"
      data$PA_ETNIA[data$PA_ETNIA == "X328"] <-	"LATUND\\u00ca"
      data$PA_ETNIA[data$PA_ETNIA == "X329"] <-	"MAKU"
      data$PA_ETNIA[data$PA_ETNIA == "X330"] <-	"MAKUNAMB\\u00c9"
      data$PA_ETNIA[data$PA_ETNIA == "X331"] <-	"MAMAIND\\u00ca"
      data$PA_ETNIA[data$PA_ETNIA == "X332"] <-	"MAMURI"
      data$PA_ETNIA[data$PA_ETNIA == "X333"] <-	"MANACAPURU"
      data$PA_ETNIA[data$PA_ETNIA == "X334"] <-	"MANAIRISSU"
      data$PA_ETNIA[data$PA_ETNIA == "X335"] <-	"MANCHINERI"
      data$PA_ETNIA[data$PA_ETNIA == "X336"] <-	"MANDUCA"
      data$PA_ETNIA[data$PA_ETNIA == "X337"] <-	"MARIBONDO"
      data$PA_ETNIA[data$PA_ETNIA == "X338"] <-	"MASSAKA"
      data$PA_ETNIA[data$PA_ETNIA == "X339"] <-	"MAWAYANA"
      data$PA_ETNIA[data$PA_ETNIA == "X340"] <-	"MAW\\u00c9"
      data$PA_ETNIA[data$PA_ETNIA == "X341"] <-	"MAYORUNA"
      data$PA_ETNIA[data$PA_ETNIA == "X342"] <-	"MIQUELENO"
      data$PA_ETNIA[data$PA_ETNIA == "X343"] <-	"MOKURI\\u00d1"
      data$PA_ETNIA[data$PA_ETNIA == "X344"] <-	"MON ORO WARAM"
      data$PA_ETNIA[data$PA_ETNIA == "X345"] <-	"MUTUM"
      data$PA_ETNIA[data$PA_ETNIA == "X346"] <-	"MYKY"
      data$PA_ETNIA[data$PA_ETNIA == "X347"] <-	"NADEB"
      data$PA_ETNIA[data$PA_ETNIA == "X348"] <-	"NAMBIKWARA"
      data$PA_ETNIA[data$PA_ETNIA == "X349"] <-	"NEGAROT\\u00ca"
      data$PA_ETNIA[data$PA_ETNIA == "X350"] <-	"NHENGATU"
      data$PA_ETNIA[data$PA_ETNIA == "X351"] <-	"OFAIE XAVANTE"
      data$PA_ETNIA[data$PA_ETNIA == "X352"] <-	"ON\\u00c7A"
      data$PA_ETNIA[data$PA_ETNIA == "X353"] <-	"ORO AT"
      data$PA_ETNIA[data$PA_ETNIA == "X354"] <-	"ORO EO"
      data$PA_ETNIA[data$PA_ETNIA == "X355"] <-	"ORO JOWIN"
      data$PA_ETNIA[data$PA_ETNIA == "X356"] <-	"ORO MIYLIN"
      data$PA_ETNIA[data$PA_ETNIA == "X357"] <-	"ORO MON"
      data$PA_ETNIA[data$PA_ETNIA == "X358"] <-	"ORO N\\u00c1O"
      data$PA_ETNIA[data$PA_ETNIA == "X359"] <-	"ORO WAM"
      data$PA_ETNIA[data$PA_ETNIA == "X360"] <-	"ORO WARAM"
      data$PA_ETNIA[data$PA_ETNIA == "X361"] <-	"ORO WARAM XIJEIN"
      data$PA_ETNIA[data$PA_ETNIA == "X362"] <-	"PACA"
      data$PA_ETNIA[data$PA_ETNIA == "X363"] <-	"PANKAR\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X364"] <-	"PAPAGAIO"
      data$PA_ETNIA[data$PA_ETNIA == "X365"] <-	"PAYAY\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X366"] <-	"PIPIPAN"
      data$PA_ETNIA[data$PA_ETNIA == "X367"] <-	"PIRATA"
      data$PA_ETNIA[data$PA_ETNIA == "X368"] <-	"PUROBOR\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X369"] <-	"SABAN\\u00ca"
      data$PA_ETNIA[data$PA_ETNIA == "X370"] <-	"SANUMA"
      data$PA_ETNIA[data$PA_ETNIA == "X371"] <-	"SAWENTES\\u00da"
      data$PA_ETNIA[data$PA_ETNIA == "X372"] <-	"SILCY-TAPUYA"
      data$PA_ETNIA[data$PA_ETNIA == "X373"] <-	"SIUCI"
      data$PA_ETNIA[data$PA_ETNIA == "X374"] <-	"TABAJARA"
      data$PA_ETNIA[data$PA_ETNIA == "X375"] <-	"TAKUARA"
      data$PA_ETNIA[data$PA_ETNIA == "X376"] <-	"TATU"
      data$PA_ETNIA[data$PA_ETNIA == "X377"] <-	"TAWAND\\u00ca"
      data$PA_ETNIA[data$PA_ETNIA == "X378"] <-	"TEF\\u00c9"
      data$PA_ETNIA[data$PA_ETNIA == "X379"] <-	"TIMBIRA"
      data$PA_ETNIA[data$PA_ETNIA == "X380"] <-	"TOR\\u00c1 DO BAIXO GRANDE"
      data$PA_ETNIA[data$PA_ETNIA == "X381"] <-	"TSUNHUM-DJAP\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X382"] <-	"TUBAR\\u00c3O"
      data$PA_ETNIA[data$PA_ETNIA == "X383"] <-	"TUPAIU"
      data$PA_ETNIA[data$PA_ETNIA == "X384"] <-	"TUPI"
      data$PA_ETNIA[data$PA_ETNIA == "X385"] <-	"TUPINAMB\\u00c1 DE BELMONTE"
      data$PA_ETNIA[data$PA_ETNIA == "X386"] <-	"URUBU"
      data$PA_ETNIA[data$PA_ETNIA == "X387"] <-	"URUBU KAAPOR"
      data$PA_ETNIA[data$PA_ETNIA == "X388"] <-	"URUP\\u00c1"
      data$PA_ETNIA[data$PA_ETNIA == "X389"] <-	"WAI WAI"
      data$PA_ETNIA[data$PA_ETNIA == "X390"] <-	"WAIKISU"
      data$PA_ETNIA[data$PA_ETNIA == "X391"] <-	"WAKALITES\\u00da"
      data$PA_ETNIA[data$PA_ETNIA == "X392"] <-	"WASSUSU"
      data$PA_ETNIA[data$PA_ETNIA == "X393"] <-	"XEREU"
      data$PA_ETNIA[data$PA_ETNIA == "X394"] <-	"XI EIN"
      data$PA_ETNIA[data$PA_ETNIA == "X395"] <-	"XICRIN"
      data$PA_ETNIA[data$PA_ETNIA == "X396"] <-	"XIPAYA"
      data$PA_ETNIA[data$PA_ETNIA == "X397"] <-	"XIRIANA"
      data$PA_ETNIA[data$PA_ETNIA == "X398"] <-	"XIRUAI"
      data$PA_ETNIA[data$PA_ETNIA == "X399"] <-	"YEPAMASS\\u00c3"
      data$PA_ETNIA[data$PA_ETNIA == "X400"] <-	"TIRIY\\u00d3"
      data$PA_ETNIA[data$PA_ETNIA == "X401"] <-	"YANOMAMI"
      data$PA_ETNIA[data$PA_ETNIA == "X402"] <-	"ARARA"
      data$PA_ETNIA[data$PA_ETNIA == "X403"] <-	"SAKIRIABAR"
      data$PA_ETNIA[data$PA_ETNIA == "X404"] <-	"TATZ"
      data$PA_ETNIA[data$PA_ETNIA == "X405"] <-	"SEM INFORMACAO"
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

    # PA_INE
    if("PA_INE" %in% variables_names){
      data$PA_INE <- as.character(data$PA_INE)
      data <- dplyr::left_join(data, microdatasus::equipe, by = c("PA_INE" = "COD"))
    }

  }

  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  data <- as.data.frame(lapply(X = data, FUN = stringi::stri_unescape_unicode))

  # Return
  return(data)

}
