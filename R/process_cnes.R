#' Process CNES variables from DataSUS
#'
#' \code{process_cnes} processes CNES variables retrieved by \code{fetch_datasus()}.
#'
#' This function processes CNES-ST (Estabelecimentos) or CNES-PF (Pessoa f\\u00edsica) variables retrieved by \code{fetch_datasus()}, informing labels for categoric variables including NA values.
#'
#' @param data \code{data.frame} created by \code{fetch_datasus()}.
#' @param information_system \code{string}. \code{CNES-ST} or \code{CNES-PF}
#' @param nomes optional logical. \code{TRUE} by default, add  \code{FANTASIA} and \code{RAZÃO SOCIAL} names to the dataset.
#' @param municipality_data optional logical. \code{TRUE} by default, creates new variables in the dataset informing the full name and other details about the municipality of residence.
#'
#' @examples
#' process_cnes(cnes_st_sample, information_system = "CNES-ST")
#' process_cnes(cnes_pf_sample, information_system = "CNES-PF")
#'
#' @return a \code{data.frame} with the processed data.
#'
#' @export

process_cnes <- function(data, information_system = c("CNES-ST", "CNES-PF"), nomes = TRUE, municipality_data = TRUE) {
  # Check information system
  checkmate::assert_choice(x = information_system, choices = c("CNES-ST", "CNES-PF"))

  # Variables names
  variables_names <- names(data)

  # Use dtplyr
  data <- dtplyr::lazy_dt(data)

  if(information_system == "CNES-ST"){
    # CNES
    if("CNES" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CNES = as.character(.data$CNES))
    }

    # CODUFMUN
    if ("CODUFMUN" %in% variables_names & municipality_data == TRUE) {
      colnames(tabMun)[1] <- "CODUFMUN"
      tabMun$CODUFMUN <- as.character(tabMun$CODUFMUN)

      data <- data %>%
        dplyr::left_join(tabMun, by = "CODUFMUN")
    }

    # COD_CEP
    if("COD_CEP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(COD_CEP = as.integer(.data$COD_CEP))
    }

    # CPF_CNPJ
    if("CPF_CNPJ" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CPF_CNPJ = as.character(.data$CPF_CNPJ))
    }

    # PF_PJ
    if("PF_PJ" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          PF_PJ = dplyr::case_match(
            .data$PF_PJ,
            "1" ~ "Pessoa f\u00edsica",
            "3" ~ "Pessoa jur\u00eddica",
            .default = .data$PF_PJ
          )
        ) %>%
        dplyr::mutate(PF_PJ = as.factor(.data$PF_PJ))
    }

    # NIV_DEP
    if("NIV_DEP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          NIV_DEP = dplyr::case_match(
            .data$NIV_DEP,
            "1" ~ "Individual",
            "3" ~ "Mantida",
            .default = .data$NIV_DEP
          )
        ) %>%
        dplyr::mutate(NIV_DEP = as.factor(.data$NIV_DEP))
    }

    # CNPJ_MAN
    if("CNPJ_MAN" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CNPJ_MAN = as.numeric(.data$CNPJ_MAN))
    }

    # COD_IR
    if("COD_IR" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COD_IR = dplyr::case_match(
            .data$COD_IR,
            "0" ~ NA,
            "10" ~ "Estabelecimento p\u00fablico",
            "11" ~ "Estabelecimento filantr\u00f3pico",
            "12" ~ "Estabelecimento sem fins lucrativos",
            "13" ~ "Estabelecimento privado luvrativa simples",
            "14" ~ "Estabelecimento privado luvrativa",
            "15" ~ "Estabelecimento sindical",
            "16" ~ "Estabelecimento pessoa f\u00edsica",
            "19" ~ "Estabelecimento Ret.Manten.c\u00f3digo 19",
            .default = .data$COD_IR
          )
        ) %>%
        dplyr::mutate(COD_IR = as.factor(.data$COD_IR))
    }

    # REGSAUDE
    if("REGSAUDE" %in% variables_names){
      data <- data %>%
        dplyr::mutate(REGSAUDE = as.character(.data$REGSAUDE))
    }

    # MICR_REG
    if("MICR_REG" %in% variables_names){
      data <- data %>%
        dplyr::mutate(MICR_REG = as.integer(.data$MICR_REG))
    }

    # DISTRSAN
    if("DISTRSAN" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DISTRSAN = as.integer(.data$DISTRSAN))
    }

    # VINC_SUS
    if("VINC_SUS" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          VINC_SUS = dplyr::case_match(
            .data$VINC_SUS,
            "0" ~ "N\u00e3o",
            "1" ~ "Sim",
            "2" ~ "N\u00e3o",
            .default = .data$VINC_SUS
          )
        ) %>%
        dplyr::mutate(VINC_SUS = as.factor(.data$VINC_SUS))
    }

    # TPGESTAO
    if("TPGESTAO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          TPGESTAO = dplyr::case_match(
            .data$TPGESTAO,
            "D" ~ "Dupla",
            "E" ~ "Estadual",
            "M" ~ "Municipal",
            "Z" ~ "Sem gest\u00e3o",
            "S" ~ "Sem gest\u00e3o",
            .default = .data$TPGESTAO
          )
        ) %>%
        dplyr::mutate(TPGESTAO = as.factor(.data$TPGESTAO))
    }

    # ESFERA_A
    if("ESFERA_A" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          ESFERA_A = dplyr::case_match(
            .data$ESFERA_A,
            "1" ~ "Federal",
            "2" ~ "Estadual",
            "3" ~ "Municipal",
            "4" ~ "Privada",
            "-99" ~ NA,
            .default = .data$ESFERA_A
          )
        ) %>%
        dplyr::mutate(ESFERA_A = as.factor(.data$ESFERA_A))
    }

    # RETENCAO
    if("RETENCAO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          RETENCAO = dplyr::case_match(
            .data$RETENCAO,
            "0" ~ NA,
            "10" ~ "Estabelecimento p\u00fablico",
            "11" ~ "Estabelecimento filantr\u00f3pico",
            "12" ~ "Estabelecimento sem fins lucrativos",
            "13" ~ "Estabelecimento privado luvrativa simples",
            "14" ~ "Estabelecimento privado luvrativa",
            "15" ~ "Estabelecimento sindical",
            "16" ~ "Estabelecimento pessoa f\u00edsica",
            .default = .data$RETENCAO
          )
        ) %>%
        dplyr::mutate(RETENCAO = as.factor(.data$RETENCAO))
    }

    # ATIVIDAD
    if("ATIVIDAD" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          ATIVIDAD = dplyr::case_match(
            .data$ATIVIDAD,
            "-99" ~ NA,
            "1" ~ "Unidade Universit\u00e1ria",
            "2" ~ "Unidade Escola Superior Isolada",
            "3" ~ "Unidade Auxiliar de Ensino",
            "4" ~ "Unidade sem atividade de Ensino",
            "5" ~ "Hospital de ensino",
            .default = .data$ATIVIDAD
          )
        ) %>%
        dplyr::mutate(ATIVIDAD = as.factor(.data$ATIVIDAD))
    }

    # NATUREZA
    if("NATUREZA" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          NATUREZA = dplyr::case_match(
            .data$NATUREZA,
            "-99" ~ NA,
            "1" ~ "Administra\u00e7\u00e3o Direta da Sa\u00fade (MS, SES, e SMS)",
            "2" ~ "Adm Direta outros org\u00e3os (MEX, MEx, Marinha,...)",
            "3" ~ "Adm Indireta - Autarquias",
            "4" ~ "Adm Indireta - Funda\u00e7\u00e3o P\u00fablica",
            "5" ~ "Adm Indireta - Empresa P\u00fablica",
            "6" ~ "Adm Indireta - Organiza\u00e7\u00e3o Social P\u00fablica",
            "7" ~ "Empresa Privada",
            "8" ~ "Funda\u00e7\u00e3o Privada",
            "9" ~ "Cooperativa",
            "10" ~ "Servi\u00e7o Social Aut\u00f4nomo",
            "11" ~ "Entidade Beneficente sem fins lucrativos",
            "12" ~ "Economia Mista",
            "13" ~ "Sindicato",
            "0" ~ "Natureza inexistente",
            .default = .data$NATUREZA
          )
        ) %>%
        dplyr::mutate(NATUREZA = as.factor(.data$NATUREZA))
    }

    # NAT_JUR
    if("NAT_JUR" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          NAT_JUR = dplyr::case_match(
            .data$NAT_JUR,
            "1015" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Federal",
            "1023" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Estadual ou do Distrito Federal",
            "1031" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Municipal",
            "1040" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Federal",
            "1058" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Estadual ou do Distrito Federal",
            "1066" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Municipal",
            "1074" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Federal",
            "1082" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Estadual",
            "1104" ~ "Autarquia Federal",
            "1112" ~ "Autarquia Estadual ou do Distrito Federal",
            "1120" ~ "Autarquia Municipal",
            "1139" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Federal",
            "1147" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Estadual ou do Distrito Federal",
            "1155" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Municipal",
            "1163" ~ "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Federal",
            "1171" ~ "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Estadual ou do Distrito Federal",
            "1180" ~ "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Municipal",
            "1198" ~ "Comiss\u00e3o Polinacional",
            "1201" ~ "Fundo P\u00fablico",
            "1210" ~ "Cons\u00f3rcio P\u00fablico de Direito P\u00fablico (Associa\u00e7\u00e3o P\u00fablica)",
            "1228" ~ "Cons\u00f3rcio P\u00fablico de Direito Privado",
            "1236" ~ "Estado ou Distrito Federal",
            "1244" ~ "Munic\u00edpio",
            "1252" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Federal",
            "1260" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Estadual ou do Distrito Federal",
            "1279" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Municipal",
            "2011" ~ "Empresa P\u00fablica",
            "2038" ~ "Sociedade de Economia Mista",
            "2046" ~ "Sociedade An\u00f4nima Aberta",
            "2054" ~ "Sociedade An\u00f4nima Fechada",
            "2062" ~ "Sociedade Empres\u00e1ria Limitada",
            "2070" ~ "Sociedade Empres\u00e1ria em Nome Coletivo",
            "2089" ~ "Sociedade Empres\u00e1ria em Comandita Simples",
            "2097" ~ "Sociedade Empres\u00e1ria em Comandita por A\u00e7\u00f5es",
            "2127" ~ "Sociedade em Conta de Participa\u00e7\u00e3o",
            "2135" ~ "Empres\u00e1rio (Individual)",
            "2143" ~ "Cooperativa",
            "2151" ~ "Cons\u00f3rcio de Sociedades",
            "2160" ~ "Grupo de Sociedades",
            "2178" ~ "Estabelecimento, no Brasil, de Sociedade Estrangeira",
            "2194" ~ "Estabelecimento, no Brasil, de Empresa Binacional Argentino-Brasileira",
            "2216" ~ "Empresa Domiciliada no Exterior",
            "2224" ~ "Clube/Fundo de Investimento",
            "2232" ~ "Sociedade Simples Pura",
            "2240" ~ "Sociedade Simples Limitada",
            "2259" ~ "Sociedade Simples em Nome Coletivo",
            "2267" ~ "Sociedade Simples em Comandita Simples",
            "2275" ~ "Empresa Binacional",
            "2283" ~ "Cons\u00f3rcio de Empregadores",
            "2291" ~ "Cons\u00f3rcio Simples",
            "2305" ~ "Empresa Individual de Responsabilidade Limitada (de Natureza Empres\u00e1ria)",
            "2313" ~ "Empresa Individual de Responsabilidade Limitada (de Natureza Simples)",
            "2321" ~ "Sociedade Unipessoal de Advogados",
            "2330" ~ "Cooperativas de Consumo",
            "3034" ~ "Servi\u00e7o Notarial e Registral (Cart\u00f3rio)",
            "3069" ~ "Funda\u00e7\u00e3o Privada",
            "3077" ~ "Servi\u00e7o Social Aut\u00f4nomo",
            "3085" ~ "Condom\u00ednio Edil\u00edcio",
            "3107" ~ "Comiss\u00e3o de Concilia\u00e7\u00e3o Pr\u00e9via",
            "3115" ~ "Entidade de Media\u00e7\u00e3o e Arbitragem",
            "3131" ~ "Entidade Sindical",
            "3204" ~ "Estabelecimento, no Brasil, de Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Estrangeiras",
            "3212" ~ "Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Domiciliada no Exterior",
            "3220" ~ "Organiza\u00e7\u00e3o Religiosa",
            "3239" ~ "Comunidade Ind\u00edgena",
            "3247" ~ "Fundo Privado",
            "3255" ~ "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Nacional de Partido Pol\u00edtico",
            "3263" ~ "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Regional de Partido Pol\u00edtico",
            "3271" ~ "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Local de Partido Pol\u00edtico",
            "3280" ~ "Comit\u00ea Financeiro de Partido Pol\u00edtico",
            "3298" ~ "Frente Plebiscit\u00e1ria ou Referend\u00e1ria",
            "3306" ~ "Organiza\u00e7\u00e3o Social (OS)",
            "3310" ~ "Demais Condom\u00ednios",
            "3999" ~ "Associa\u00e7\u00e3o Privada",
            "4014" ~ "Empresa Individual Imobili\u00e1ria",
            "4022" ~ "Segurado Especial",
            "4081" ~ "Contribuinte individual",
            "4090" ~ "Candidato a Cargo Pol\u00edtico Eletivo",
            "4111" ~ "Leiloeiro",
            "4124" ~ "Produtor Rural (Pessoa F\u00edsica)",
            "5010" ~ "Organiza\u00e7\u00e3o Internacional",
            "5029" ~ "Representa\u00e7\u00e3o Diplom\u00e1tica Estrangeira",
            "5037" ~ "Outras Institui\u00e7\u00f5es Extraterritoriais",
            "0" ~ "N\u00e3o especificado ou ignorado",
            .default = .data$NAT_JUR
          )
        ) %>%
        dplyr::mutate(NAT_JUR = as.factor(.data$NAT_JUR))
    }

    # CLIENTEL
    if("CLIENTEL" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          CLIENTEL = dplyr::case_match(
            .data$CLIENTEL,
            "-99" ~ NA,
            "1" ~ "Atendimento de demanda espont\u00e2nea",
            "2" ~ "Atendimento de demanda referenciada",
            "3" ~ "Atendimento de demanda espont\u00e2nea e referenciada",
            "0" ~ "Fluxo de Clientela n\u00e3o exigido",
            .default = .data$CLIENTEL
          )
        ) %>%
        dplyr::mutate(CLIENTEL = as.factor(.data$CLIENTEL))
    }

    # TP_UNID
    if("TP_UNID" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          TP_UNID = dplyr::case_match(
            .data$TP_UNID,
            "1" ~ "Posto de sa\u00fade",
            "01" ~ "Posto de sa\u00fade",
            "2" ~ "Centro de sa\u00fade / Unidade b\u00e1sica",
            "02" ~ "Centro de sa\u00fade / Unidade b\u00e1sica",
            "4" ~ "Policl\u00ednica",
            "04" ~ "Policl\u00ednica",
            "5" ~ "Hospital geral",
            "05" ~ "Hospital geral",
            "7" ~ "Hospital Especializado",
            "07" ~ "Hospital Especializado",
            "9" ~ "Pronto socorro de hospital geral (antigo)",
            "09" ~ "Pronto socorro de hospital geral (antigo)",
            "12" ~ "Pronto socorro traumato-ortop\u00e9dico (antigo)",
            "15" ~ "Unidade mista",
            "20" ~ "Pronto socorro geral",
            "21" ~ "Pronto socorro especializado",
            "22" ~ "Consult\u00f3rio isolado",
            "32" ~ "Unidade m\u00f3vel fluvial",
            "36" ~ "Cl\u00ednica / Centro de sa\u00fade de especialidade",
            "39" ~ "Unidade de apoio diagnose e terapia (SADT isolado)",
            "40" ~ "Unidade m\u00f3vel terrestre",
            "42" ~ "Unidade m\u00f3vel de n\u00edvel pr\u00e9-hospitalar na \u00e1rea de urg\u00eancia",
            "43" ~ "Farm\u00e1cia",
            "45" ~ "Unidade de sa\u00fade da fam\u00edlia",
            "50" ~ "Unidade de vigil\u00e2ncia em sa\u00fade",
            "60" ~ "Cooperativa ou empresa de cess\u00e3o de trabalhadores na sa\u00fade",
            "61" ~ "Centro de parto normal - isolado",
            "62" ~ "Hospital / Dia - Isolado",
            "63" ~ "Unidade autorizadora",
            "64" ~ "Central de regula\u00e7\u00e3o de servi\u00e7os de sa\u00fade",
            "65" ~ "Unidade de vigil\u00e2ncia epidemiol\u00f3gica (antigo)",
            "66" ~ "Unidade de vigil\u00e2ncia sanit\u00e1ria (antigo)",
            "67" ~ "Laborat\u00f3rio central de sa\u00fade p\u00fablica LACEN",
            "68" ~ "Central de gest\u00e3o em sa\u00fade",
            "69" ~ "Centro de aten\u00e7\u00e3o hemoterapia e/ou hematologica",
            "70" ~ "Centro de aten\u00e7\u00e3o psicosocial",
            "71" ~ "Centro de apoio a sa\u00fade da fam\u00edlia",
            "72" ~ "Unidade de aten\u00e7\u00e3o a sa\u00fade ind\u00edgena",
            "73" ~ "Pronto atendimento",
            "74" ~ "P\u00f3lo academia da sa\u00fade",
            "75" ~ "Telessa\u00fade",
            "76" ~ "Central de regula\u00e7\u00e3o m\u00e9dica das urg\u00eancias",
            "77" ~ "Servi\u00e7o de aten\u00e7\u00e3o domiciliar isolado (Home care)",
            "78" ~ "Unidade de aten\u00e7\u00e3o em regime residencial",
            "79" ~ "Oficina ortop\u00e9dica",
            "80" ~ "Laborat\u00f3rio de sa\u00fade p\u00fablica",
            "81" ~ "Central de regula\u00e7\u00e3o do acesso",
            "82" ~ "Central de notifica\u00e7\u00e3o, capta\u00e7\u00e3o e distribui\u00e7\u00e3o de \u00f3rg\u00e3os estadual",
            "83" ~ "P\u00f3lo de preven\u00e7\u00e3o de doen\u00e7as e agravos e promo\u00e7\u00e3o da sa\u00fade",
            "84" ~ "Central de abastecimento",
            "85" ~ "Centro de imuniza\u00e7\u00e3o",
            .default = .data$TP_UNID
          )
        ) %>%
        dplyr::mutate(TP_UNID = as.factor(.data$TP_UNID))
    }

    # TURNO_AT
    if("TURNO_AT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          TURNO_AT = dplyr::case_match(
            .data$TURNO_AT,
            "-99" ~ NA,
            "1" ~ "Turnos intermitentes",
            "2" ~ "Cont\u00ednuo 24h/dia (Pl Sab Dom Fer)",
            "3" ~ "Manh\u00e3 / Tarde / Noite",
            "4" ~ "Manh\u00e3",
            "5" ~ "Tarde",
            "6" ~ "Manh\u00e3 / Tarde",
            "7" ~ "Noite",
            .default = .data$TURNO_AT
          )
        ) %>%
        dplyr::mutate(TURNO_AT = as.factor(.data$TURNO_AT))
    }

    # NIV_HIER
    if("NIV_HIER" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          NIV_HIER = dplyr::case_match(
            .data$NIV_HIER,
            "0" ~ NA,
            "-99" ~ NA,
            "1" ~ "PAB-PABA",
            "2" ~ "M\u00e9dia M1",
            "3" ~ "M\u00e9dia M2 e M3",
            "4" ~ "Alta complexidade ambulatorial",
            "5" ~ "Baixa M1 e M2",
            "6" ~ "M\u00e9dia M2 e M3",
            "7" ~ "M\u00e9dia M3",
            "8" ~ "Alta complexidade hospitalar / ambulatorial",
            .default = .data$NIV_HIER
          )
        ) %>%
        dplyr::mutate(NIV_HIER = as.factor(.data$NIV_HIER))
    }

    # TP_PREST
    if("TP_PREST" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          TP_PREST = dplyr::case_match(
            .data$TP_PREST,
            "-99" ~ NA,
            "30" ~ "P\u00fablico federal",
            "40" ~ "P\u00fablico estadual",
            "50" ~ "P\u00fablico municipal",
            "61" ~ "Filantr\u00f3pico com CNAS v\u00e1lido",
            "80" ~ "Sindicato",
            "20" ~ "Privado com fins lucrativos",
            "22" ~ "Privado optantes pelo simples",
            "60" ~ "Privado sem fins lucrativos",
            .default = .data$TP_PREST
          )
        ) %>%
        dplyr::mutate(TP_PREST = as.factor(.data$TP_PREST))
    }

    # CO_BANCO
    if("CO_BANCO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CO_BANCO = as.character(.data$CO_BANCO))
    }

    # CO_AGENC
    if("CO_AGENC" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CO_AGENC = as.character(.data$CO_AGENC))
    }

    # C_CORREN
    if("C_CORREN" %in% variables_names){
      data <- data %>%
        dplyr::mutate(C_CORREN = as.character(.data$C_CORREN))
    }

    # CONTRATM
    if("CONTRATM" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CONTRATM = as.character(.data$CONTRATM))
    }

    # DT_PUBLM
    if("DT_PUBLM" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DT_PUBLM = as.character(.data$DT_PUBLM))
    }

    # CONTRATE
    if("CONTRATE" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CONTRATE = as.character(.data$CONTRATE))
    }

    # DT_PUBLE
    if("DT_PUBLE" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DT_PUBLE = as.character(.data$DT_PUBLE))
    }

    # ALVARA
    if("ALVARA" %in% variables_names){
      data <- data %>%
        dplyr::mutate(ALVARA = as.character(.data$ALVARA))
    }

    # DT_EXPED
    if("DT_EXPED" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DT_EXPED = as.character(.data$DT_EXPED))
    }

    # ORGEXPED
    if("ORGEXPED" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          ORGEXPED = dplyr::case_match(
            .data$ORGEXPED,
            "1" ~ "SES",
            "2" ~ "SMS",
            .default = .data$ORGEXPED
          )
        ) %>%
        dplyr::mutate(ORGEXPED = as.factor(.data$ORGEXPED))
    }

    # AV_ACRED
    if("AV_ACRED" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AV_ACRED = dplyr::case_match(
            .data$AV_ACRED,
            "1" ~ "Sim",
            "2" ~ "N\u00e3o",
            "0" ~ "N\u00e3o",
            .default = .data$AV_ACRED
          )
        ) %>%
        dplyr::mutate(AV_ACRED = as.factor(.data$AV_ACRED))
    }

    # CLASAVAL
    if("CLASAVAL" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          CLASAVAL = dplyr::case_match(
            .data$CLASAVAL,
            "-9" ~ NA,
            "1" ~ "Acreditado no n\u00edvel 1",
            "2" ~ "Acreditado no n\u00edvel 2",
            "3" ~ "Acreditado no n\u00edvel 3",
            "0" ~ "N\u00e3o atendeu aos padr\u00f5es m\u00ednimos",
            .default = .data$CLASAVAL
          )
        ) %>%
        dplyr::mutate(CLASAVAL = as.factor(.data$CLASAVAL))
    }

    # DT_ACRED
    if("DT_ACRED" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DT_ACRED = as.integer(.data$DT_ACRED))
    }

    # AV_PNASS
    if("AV_PNASS" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AV_PNASS = dplyr::case_match(
            .data$AV_PNASS,
            "1" ~ "Sim",
            "2" ~ "N\u00e3o",
            "0" ~ "N\u00e3o",
            .default = .data$AV_PNASS
          )
        ) %>%
        dplyr::mutate(AV_PNASS = as.factor(.data$AV_PNASS))
    }

    # DT_PNASS
    if("DT_PNASS" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DT_PNASS = as.integer(.data$DT_PNASS))
    }

    # GESPRG1E
    if("GESPRG1E" %in% variables_names){
      data$GESPRG1E <- as.numeric(levels(data$GESPRG1E))[data$GESPRG1E]
      data$GESPRG1E[data$GESPRG1E==1] <- "Sim"
      data$GESPRG1E[data$GESPRG1E==0] <- "N\u00e3o"
      data$GESPRG1E <- factor(data$GESPRG1E)
    }

    # GESPRG1M
    if("GESPRG1M" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG1M = dplyr::case_match(
            .data$GESPRG1M,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG1M
          )
        ) %>%
        dplyr::mutate(GESPRG1M = as.factor(.data$GESPRG1M))
    }

    # GESPRG2E
    if("GESPRG2E" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG2E = dplyr::case_match(
            .data$GESPRG2E,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG2E
          )
        ) %>%
        dplyr::mutate(GESPRG2E = as.factor(.data$GESPRG2E))
    }

    # GESPRG2M
    if("GESPRG2M" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG2M = dplyr::case_match(
            .data$GESPRG2M,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG2M
          )
        ) %>%
        dplyr::mutate(GESPRG2M = as.factor(.data$GESPRG2M))
    }

    # GESPRG4E
    if("GESPRG4E" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG4E = dplyr::case_match(
            .data$GESPRG4E,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG4E
          )
        ) %>%
        dplyr::mutate(GESPRG4E = as.factor(.data$GESPRG4E))
    }

    # GESPRG4M
    if("GESPRG4M" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG4M = dplyr::case_match(
            .data$GESPRG4M,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG4M
          )
        ) %>%
        dplyr::mutate(GESPRG4M = as.factor(.data$GESPRG4M))
    }

    # NIVATE_A
    if("NIVATE_A" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          NIVATE_A = dplyr::case_match(
            .data$NIVATE_A,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$NIVATE_A
          )
        ) %>%
        dplyr::mutate(NIVATE_A = as.factor(.data$NIVATE_A))
    }

    # GESPRG3E
    if("GESPRG3E" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG3E = dplyr::case_match(
            .data$GESPRG3E,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG3E
          )
        ) %>%
        dplyr::mutate(GESPRG3E = as.factor(.data$GESPRG3E))
    }

    # GESPRG3M
    if("GESPRG3M" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG3M = dplyr::case_match(
            .data$GESPRG3M,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG3M
          )
        ) %>%
        dplyr::mutate(GESPRG3M = as.factor(.data$GESPRG3M))
    }

    # GESPRG5E
    if("GESPRG5E" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG5E = dplyr::case_match(
            .data$GESPRG5E,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG5E
          )
        ) %>%
        dplyr::mutate(GESPRG5E = as.factor(.data$GESPRG5E))
    }

    # GESPRG5M
    if("GESPRG5M" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG5M = dplyr::case_match(
            .data$GESPRG5M,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG5M
          )
        ) %>%
        dplyr::mutate(GESPRG5M = as.factor(.data$GESPRG5M))
    }

    # GESPRG6E
    if("GESPRG6E" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG6E = dplyr::case_match(
            .data$GESPRG6E,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG6E
          )
        ) %>%
        dplyr::mutate(GESPRG6E = as.factor(.data$GESPRG6E))
    }

    # GESPRG6M
    if("GESPRG6M" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          GESPRG6M = dplyr::case_match(
            .data$GESPRG6M,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$GESPRG6M
          )
        ) %>%
        dplyr::mutate(GESPRG6M = as.factor(.data$GESPRG6M))
    }

    # NIVATE_H
    if("NIVATE_H" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          NIVATE_H = dplyr::case_match(
            .data$NIVATE_H,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$NIVATE_H
          )
        ) %>%
        dplyr::mutate(NIVATE_H = as.factor(.data$NIVATE_H))
    }

    # URGEMERG
    if("URGEMERG" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          URGEMERG = dplyr::case_match(
            .data$URGEMERG,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$URGEMERG
          )
        ) %>%
        dplyr::mutate(URGEMERG = as.factor(.data$URGEMERG))
    }

    # ATENDAMB
    if("ATENDAMB" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          ATENDAMB = dplyr::case_match(
            .data$ATENDAMB,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$ATENDAMB
          )
        ) %>%
        dplyr::mutate(ATENDAMB = as.factor(.data$ATENDAMB))
    }

    # CENTROBS
    if("CENTROBS" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          CENTROBS = dplyr::case_match(
            .data$CENTROBS,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$CENTROBS
          )
        ) %>%
        dplyr::mutate(CENTROBS = as.factor(.data$CENTROBS))
    }

    # CENTRNEO
    if("CENTRNEO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          CENTRNEO = dplyr::case_match(
            .data$CENTRNEO,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$CENTRNEO
          )
        ) %>%
        dplyr::mutate(CENTRNEO = as.factor(.data$CENTRNEO))
    }

    # ATENDHOS
    if("ATENDHOS" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          ATENDHOS = dplyr::case_match(
            .data$ATENDHOS,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$ATENDHOS
          )
        ) %>%
        dplyr::mutate(ATENDHOS = as.factor(.data$ATENDHOS))
    }

    # SERAP01P
    if("SERAP01P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP01P = dplyr::case_match(
            .data$SERAP01P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP01P
          )
        ) %>%
        dplyr::mutate(SERAP01P = as.factor(.data$SERAP01P))
    }

    # SERAP01T
    if("SERAP01T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP01T = dplyr::case_match(
            .data$SERAP01T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP01T
          )
        ) %>%
        dplyr::mutate(SERAP01T = as.factor(.data$SERAP01T))
    }

    # SERAP02P
    if("SERAP02P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP02P = dplyr::case_match(
            .data$SERAP02P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP02P
          )
        ) %>%
        dplyr::mutate(SERAP02P = as.factor(.data$SERAP02P))
    }

    # SERAP02T
    if("SERAP02T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP02T = dplyr::case_match(
            .data$SERAP02T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP02T
          )
        ) %>%
        dplyr::mutate(SERAP02T = as.factor(.data$SERAP02T))
    }

    # SERAP03P
    if("SERAP03P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP03P = dplyr::case_match(
            .data$SERAP03P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP03P
          )
        ) %>%
        dplyr::mutate(SERAP03P = as.factor(.data$SERAP03P))
    }

    # SERAP03T
    if("SERAP03T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP03T = dplyr::case_match(
            .data$SERAP03T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP03T
          )
        ) %>%
        dplyr::mutate(SERAP03T = as.factor(.data$SERAP03T))
    }

    # SERAP04P
    if("SERAP04P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP04P = dplyr::case_match(
            .data$SERAP04P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP04P
          )
        ) %>%
        dplyr::mutate(SERAP04P = as.factor(.data$SERAP04P))
    }

    # SERAP04T
    if("SERAP04T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP04T = dplyr::case_match(
            .data$SERAP04T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP04T
          )
        ) %>%
        dplyr::mutate(SERAP04T = as.factor(.data$SERAP04T))
    }

    # SERAP05P
    if("SERAP05P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP05P = dplyr::case_match(
            .data$SERAP05P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP05P
          )
        ) %>%
        dplyr::mutate(SERAP05P = as.factor(.data$SERAP05P))
    }

    # SERAP05T
    if("SERAP05T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP05T = dplyr::case_match(
            .data$SERAP05T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP05T
          )
        ) %>%
        dplyr::mutate(SERAP05T = as.factor(.data$SERAP05T))
    }

    # SERAP06P
    if("SERAP06P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP06P = dplyr::case_match(
            .data$SERAP06P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP06P
          )
        ) %>%
        dplyr::mutate(SERAP06P = as.factor(.data$SERAP06P))
    }

    # SERAP06T
    if("SERAP06T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP06T = dplyr::case_match(
            .data$SERAP06T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP06T
          )
        ) %>%
        dplyr::mutate(SERAP06T = as.factor(.data$SERAP06T))
    }

    # SERAP07P
    if("SERAP07P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP07P = dplyr::case_match(
            .data$SERAP07P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP07P
          )
        ) %>%
        dplyr::mutate(SERAP07P = as.factor(.data$SERAP07P))
    }

    # SERAP07T
    if("SERAP07T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP07T = dplyr::case_match(
            .data$SERAP07T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP07T
          )
        ) %>%
        dplyr::mutate(SERAP07T = as.factor(.data$SERAP07T))
    }

    # SERAP08P
    if("SERAP08P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP08P = dplyr::case_match(
            .data$SERAP08P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP08P
          )
        ) %>%
        dplyr::mutate(SERAP08P = as.factor(.data$SERAP08P))
    }

    # SERAP08T
    if("SERAP08T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP08T = dplyr::case_match(
            .data$SERAP08T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP08T
          )
        ) %>%
        dplyr::mutate(SERAP08T = as.factor(.data$SERAP08T))
    }

    # SERAP09P
    if("SERAP09P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP09P = dplyr::case_match(
            .data$SERAP09P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP09P
          )
        ) %>%
        dplyr::mutate(SERAP09P = as.factor(.data$SERAP09P))
    }

    # SERAP09T
    if("SERAP09T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP09T = dplyr::case_match(
            .data$SERAP09T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP09T
          )
        ) %>%
        dplyr::mutate(SERAP09T = as.factor(.data$SERAP09T))
    }

    # SERAP10P
    if("SERAP10P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP10P = dplyr::case_match(
            .data$SERAP10P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP10P
          )
        ) %>%
        dplyr::mutate(SERAP10P = as.factor(.data$SERAP10P))
    }

    # SERAP10T
    if("SERAP10T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP10T = dplyr::case_match(
            .data$SERAP10T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP10T
          )
        ) %>%
        dplyr::mutate(SERAP10T = as.factor(.data$SERAP10T))
    }

    # SERAP11P
    if("SERAP11P" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP11P = dplyr::case_match(
            .data$SERAP11P,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP11P
          )
        ) %>%
        dplyr::mutate(SERAP11P = as.factor(.data$SERAP11P))
    }

    # SERAP11T
    if("SERAP11T" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAP11T = dplyr::case_match(
            .data$SERAP11T,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAP11T
          )
        ) %>%
        dplyr::mutate(SERAP11T = as.factor(.data$SERAP11T))
    }

    # SERAPOIO
    if("SERAPOIO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          SERAPOIO = dplyr::case_match(
            .data$SERAPOIO,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$SERAPOIO
          )
        ) %>%
        dplyr::mutate(SERAPOIO = as.factor(.data$SERAPOIO))
    }

    # RES_BIOL
    if("RES_BIOL" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          RES_BIOL = dplyr::case_match(
            .data$RES_BIOL,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$RES_BIOL
          )
        ) %>%
        dplyr::mutate(RES_BIOL = as.factor(.data$RES_BIOL))
    }

    # RES_QUIM
    if("RES_QUIM" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          RES_QUIM = dplyr::case_match(
            .data$RES_QUIM,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$RES_QUIM
          )
        ) %>%
        dplyr::mutate(RES_QUIM = as.factor(.data$RES_QUIM))
    }

    # RES_RADI
    if("RES_RADI" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          RES_RADI = dplyr::case_match(
            .data$RES_RADI,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$RES_RADI
          )
        ) %>%
        dplyr::mutate(RES_RADI = as.factor(.data$RES_RADI))
    }

    # RES_COMU
    if("RES_COMU" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          RES_COMU = dplyr::case_match(
            .data$RES_COMU,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$RES_COMU
          )
        ) %>%
        dplyr::mutate(RES_COMU = as.factor(.data$RES_COMU))
    }

    # COLETRES
    if("COLETRES" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COLETRES = dplyr::case_match(
            .data$COLETRES,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COLETRES
          )
        ) %>%
        dplyr::mutate(COLETRES = as.factor(.data$COLETRES))
    }

    # COMISS01
    if("COMISS01" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS01 = dplyr::case_match(
            .data$COMISS01,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS01
          )
        ) %>%
        dplyr::mutate(COMISS01 = as.factor(.data$COMISS01))
    }

    # COMISS02
    if("COMISS02" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS02 = dplyr::case_match(
            .data$COMISS02,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS02
          )
        ) %>%
        dplyr::mutate(COMISS02 = as.factor(.data$COMISS02))
    }

    # COMISS03
    if("COMISS03" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS03 = dplyr::case_match(
            .data$COMISS03,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS03
          )
        ) %>%
        dplyr::mutate(COMISS03 = as.factor(.data$COMISS03))
    }

    # COMISS04
    if("COMISS04" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS04 = dplyr::case_match(
            .data$COMISS04,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS04
          )
        ) %>%
        dplyr::mutate(COMISS04 = as.factor(.data$COMISS04))
    }

    # COMISS05
    if("COMISS05" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS05 = dplyr::case_match(
            .data$COMISS05,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS05
          )
        ) %>%
        dplyr::mutate(COMISS05 = as.factor(.data$COMISS05))
    }

    # COMISS06
    if("COMISS06" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS06 = dplyr::case_match(
            .data$COMISS06,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS06
          )
        ) %>%
        dplyr::mutate(COMISS06 = as.factor(.data$COMISS06))
    }

    # COMISS07
    if("COMISS07" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS07 = dplyr::case_match(
            .data$COMISS07,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS07
          )
        ) %>%
        dplyr::mutate(COMISS07 = as.factor(.data$COMISS07))
    }

    # COMISS08
    if("COMISS08" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS08 = dplyr::case_match(
            .data$COMISS08,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS08
          )
        ) %>%
        dplyr::mutate(COMISS08 = as.factor(.data$COMISS08))
    }

    # COMISS09
    if("COMISS09" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS09 = dplyr::case_match(
            .data$COMISS09,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS09
          )
        ) %>%
        dplyr::mutate(COMISS09 = as.factor(.data$COMISS09))
    }

    # COMISS10
    if("COMISS10" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS10 = dplyr::case_match(
            .data$COMISS10,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS10
          )
        ) %>%
        dplyr::mutate(COMISS10 = as.factor(.data$COMISS10))
    }

    # COMISS11
    if("COMISS11" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS11 = dplyr::case_match(
            .data$COMISS11,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS11
          )
        ) %>%
        dplyr::mutate(COMISS11 = as.factor(.data$COMISS11))
    }

    # COMISS12
    if("COMISS12" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISS12 = dplyr::case_match(
            .data$COMISS12,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISS12
          )
        ) %>%
        dplyr::mutate(COMISS12 = as.factor(.data$COMISS12))
    }

    # COMISSAO
    if("COMISSAO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          COMISSAO = dplyr::case_match(
            .data$COMISSAO,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$COMISSAO
          )
        ) %>%
        dplyr::mutate(COMISSAO = as.factor(.data$COMISSAO))
    }

    # AP01CV01
    if("AP01CV01" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP01CV01 = dplyr::case_match(
            .data$AP01CV01,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP01CV01
          )
        ) %>%
        dplyr::mutate(AP01CV01 = as.factor(.data$AP01CV01))
    }

    # AP01CV02
    if("AP01CV02" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP01CV02 = dplyr::case_match(
            .data$AP01CV02,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP01CV02
          )
        ) %>%
        dplyr::mutate(AP01CV02 = as.factor(.data$AP01CV02))
    }

    # AP01CV05
    if("AP01CV05" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP01CV05 = dplyr::case_match(
            .data$AP01CV05,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP01CV05
          )
        ) %>%
        dplyr::mutate(AP01CV05 = as.factor(.data$AP01CV05))
    }

    # AP01CV06
    if("AP01CV06" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP01CV06 = dplyr::case_match(
            .data$AP01CV06,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP01CV06
          )
        ) %>%
        dplyr::mutate(AP01CV06 = as.factor(.data$AP01CV06))
    }

    # AP01CV03
    if("AP01CV03" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP01CV03 = dplyr::case_match(
            .data$AP01CV03,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP01CV03
          )
        ) %>%
        dplyr::mutate(AP01CV03 = as.factor(.data$AP01CV03))
    }

    # AP01CV04
    if("AP01CV04" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP01CV04 = dplyr::case_match(
            .data$AP01CV04,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP01CV04
          )
        ) %>%
        dplyr::mutate(AP01CV04 = as.factor(.data$AP01CV04))
    }

    # AP02CV01
    if("AP02CV01" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP02CV01 = dplyr::case_match(
            .data$AP02CV01,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP02CV01
          )
        ) %>%
        dplyr::mutate(AP02CV01 = as.factor(.data$AP02CV01))
    }

    # AP02CV02
    if("AP02CV02" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP02CV02 = dplyr::case_match(
            .data$AP02CV02,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP02CV02
          )
        ) %>%
        dplyr::mutate(AP02CV02 = as.factor(.data$AP02CV02))
    }

    # AP02CV05
    if("AP02CV05" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP02CV05 = dplyr::case_match(
            .data$AP02CV05,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP02CV05
          )
        ) %>%
        dplyr::mutate(AP02CV05 = as.factor(.data$AP02CV05))
    }

    # AP02CV06
    if("AP02CV06" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP02CV06 = dplyr::case_match(
            .data$AP02CV06,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP02CV06
          )
        ) %>%
        dplyr::mutate(AP02CV06 = as.factor(.data$AP02CV06))
    }

    # AP02CV03
    if("AP02CV03" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP02CV03 = dplyr::case_match(
            .data$AP02CV03,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP02CV03
          )
        ) %>%
        dplyr::mutate(AP02CV03 = as.factor(.data$AP02CV03))
    }

    # AP02CV04
    if("AP02CV04" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP02CV04 = dplyr::case_match(
            .data$AP02CV04,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP02CV04
          )
        ) %>%
        dplyr::mutate(AP02CV04 = as.factor(.data$AP02CV04))
    }

    # AP03CV01
    if("AP03CV01" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP03CV01 = dplyr::case_match(
            .data$AP03CV01,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP03CV01
          )
        ) %>%
        dplyr::mutate(AP03CV01 = as.factor(.data$AP03CV01))
    }

    # AP03CV02
    if("AP03CV02" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP03CV02 = dplyr::case_match(
            .data$AP03CV02,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP03CV02
          )
        ) %>%
        dplyr::mutate(AP03CV02 = as.factor(.data$AP03CV02))
    }

    # AP03CV05
    if("AP03CV05" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP03CV05 = dplyr::case_match(
            .data$AP03CV05,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP03CV05
          )
        ) %>%
        dplyr::mutate(AP03CV05 = as.factor(.data$AP03CV05))
    }

    # AP03CV06
    if("AP03CV06" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP03CV06 = dplyr::case_match(
            .data$AP03CV06,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP03CV06
          )
        ) %>%
        dplyr::mutate(AP03CV06 = as.factor(.data$AP03CV06))
    }

    # AP03CV03
    if("AP03CV03" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP03CV03 = dplyr::case_match(
            .data$AP03CV03,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP03CV03
          )
        ) %>%
        dplyr::mutate(AP03CV03 = as.factor(.data$AP03CV03))
    }

    # AP03CV04
    if("AP03CV04" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP03CV04 = dplyr::case_match(
            .data$AP03CV04,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP03CV04
          )
        ) %>%
        dplyr::mutate(AP03CV04 = as.factor(.data$AP03CV04))
    }

    # AP04CV01
    if("AP04CV01" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP04CV01 = dplyr::case_match(
            .data$AP04CV01,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP04CV01
          )
        ) %>%
        dplyr::mutate(AP04CV01 = as.factor(.data$AP04CV01))
    }

    # AP04CV02
    if("AP04CV02" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP04CV02 = dplyr::case_match(
            .data$AP04CV02,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP04CV02
          )
        ) %>%
        dplyr::mutate(AP04CV02 = as.factor(.data$AP04CV02))
    }

    # AP04CV05
    if("AP04CV05" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP04CV05 = dplyr::case_match(
            .data$AP04CV05,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP04CV05
          )
        ) %>%
        dplyr::mutate(AP04CV05 = as.factor(.data$AP04CV05))
    }

    # AP04CV06
    if("AP04CV06" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP04CV06 = dplyr::case_match(
            .data$AP04CV06,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP04CV06
          )
        ) %>%
        dplyr::mutate(AP04CV06 = as.factor(.data$AP04CV06))
    }

    # AP04CV03
    if("AP04CV03" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP04CV03 = dplyr::case_match(
            .data$AP04CV03,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP04CV03
          )
        ) %>%
        dplyr::mutate(AP04CV03 = as.factor(.data$AP04CV03))
    }

    # AP04CV04
    if("AP04CV04" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP04CV04 = dplyr::case_match(
            .data$AP04CV04,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP04CV04
          )
        ) %>%
        dplyr::mutate(AP04CV04 = as.factor(.data$AP04CV04))
    }

    # AP05CV01
    if("AP05CV01" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP05CV01 = dplyr::case_match(
            .data$AP05CV01,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP05CV01
          )
        ) %>%
        dplyr::mutate(AP05CV01 = as.factor(.data$AP05CV01))
    }

    # AP05CV02
    if("AP05CV02" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP05CV02 = dplyr::case_match(
            .data$AP05CV02,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP05CV02
          )
        ) %>%
        dplyr::mutate(AP05CV02 = as.factor(.data$AP05CV02))
    }

    # AP05CV05
    if("AP05CV05" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP05CV05 = dplyr::case_match(
            .data$AP05CV05,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP05CV05
          )
        ) %>%
        dplyr::mutate(AP05CV05 = as.factor(.data$AP05CV05))
    }

    # AP05CV06
    if("AP05CV06" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP05CV06 = dplyr::case_match(
            .data$AP05CV06,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP05CV06
          )
        ) %>%
        dplyr::mutate(AP05CV06 = as.factor(.data$AP05CV06))
    }

    # AP05CV03
    if("AP05CV03" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP05CV03 = dplyr::case_match(
            .data$AP05CV03,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP05CV03
          )
        ) %>%
        dplyr::mutate(AP05CV03 = as.factor(.data$AP05CV03))
    }

    # AP05CV04
    if("AP05CV04" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP05CV04 = dplyr::case_match(
            .data$AP05CV04,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP05CV04
          )
        ) %>%
        dplyr::mutate(AP05CV04 = as.factor(.data$AP05CV04))
    }

    # AP06CV01
    if("AP06CV01" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP06CV01 = dplyr::case_match(
            .data$AP06CV01,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP06CV01
          )
        ) %>%
        dplyr::mutate(AP06CV01 = as.factor(.data$AP06CV01))
    }

    # AP06CV02
    if("AP06CV02" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP06CV02 = dplyr::case_match(
            .data$AP06CV02,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP06CV02
          )
        ) %>%
        dplyr::mutate(AP06CV02 = as.factor(.data$AP06CV02))
    }

    # AP06CV05
    if("AP06CV05" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP06CV05 = dplyr::case_match(
            .data$AP06CV05,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP06CV05
          )
        ) %>%
        dplyr::mutate(AP06CV05 = as.factor(.data$AP06CV05))
    }

    # AP06CV06
    if("AP06CV06" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP06CV06 = dplyr::case_match(
            .data$AP06CV06,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP06CV06
          )
        ) %>%
        dplyr::mutate(AP06CV06 = as.factor(.data$AP06CV06))
    }

    # AP06CV03
    if("AP06CV03" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP06CV03 = dplyr::case_match(
            .data$AP06CV03,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP06CV03
          )
        ) %>%
        dplyr::mutate(AP06CV03 = as.factor(.data$AP06CV03))
    }

    # AP06CV04
    if("AP06CV04" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP06CV04 = dplyr::case_match(
            .data$AP06CV04,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP06CV04
          )
        ) %>%
        dplyr::mutate(AP06CV04 = as.factor(.data$AP06CV04))
    }

    # AP07CV01
    if("AP07CV01" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP07CV01 = dplyr::case_match(
            .data$AP07CV01,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP07CV01
          )
        ) %>%
        dplyr::mutate(AP07CV01 = as.factor(.data$AP07CV01))
    }

    # AP07CV02
    if("AP07CV02" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP07CV02 = dplyr::case_match(
            .data$AP07CV02,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP07CV02
          )
        ) %>%
        dplyr::mutate(AP07CV02 = as.factor(.data$AP07CV02))
    }

    # AP07CV05
    if("AP07CV05" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP07CV05 = dplyr::case_match(
            .data$AP07CV05,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP07CV05
          )
        ) %>%
        dplyr::mutate(AP07CV05 = as.factor(.data$AP07CV05))
    }

    # AP07CV06
    if("AP07CV06" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP07CV06 = dplyr::case_match(
            .data$AP07CV06,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP07CV06
          )
        ) %>%
        dplyr::mutate(AP07CV06 = as.factor(.data$AP07CV06))
    }

    # AP07CV03
    if("AP07CV03" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP07CV03 = dplyr::case_match(
            .data$AP07CV03,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP07CV03
          )
        ) %>%
        dplyr::mutate(AP07CV03 = as.factor(.data$AP07CV03))
    }

    # AP07CV04
    if("AP07CV04" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          AP07CV04 = dplyr::case_match(
            .data$AP07CV04,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$AP07CV04
          )
        ) %>%
        dplyr::mutate(AP07CV04 = as.factor(.data$AP07CV04))
    }

    # ATEND_PR
    if("ATEND_PR" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          ATEND_PR = dplyr::case_match(
            .data$ATEND_PR,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$ATEND_PR
          )
        ) %>%
        dplyr::mutate(ATEND_PR = as.factor(.data$ATEND_PR))
    }
  } else if(information_system == "CNES-PF"){
    # CNES
    if("CNES" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CNES = as.character(.data$CNES))
    }

    # UFMUNRES
    if ("UFMUNRES" %in% variables_names & municipality_data == TRUE) {
      colnames(tabMun)[5] <- "UFMUNRES"
      tabMun$UFMUNRES <- as.character(tabMun$UFMUNRES)

      data <- data %>%
        dplyr::left_join(tabMun, by = "UFMUNRES")
    }

    # REGSAUDE
    if("REGSAUDE" %in% variables_names){
      data <- data %>%
        dplyr::mutate(REGSAUDE = as.character(.data$REGSAUDE))
    }

    # MICR_REG
    if("MICR_REG" %in% variables_names){
      data <- data %>%
        dplyr::mutate(MICR_REG = as.integer(.data$MICR_REG))
    }

    # DISTRSAN
    if("DISTRSAN" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DISTRSAN = as.integer(.data$DISTRSAN))
    }

    # DISTRADM
    if("DISTRADM" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DISTRADM = as.integer(.data$DISTRADM))
    }

    # TPGESTAO
    if("TPGESTAO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          TPGESTAO = dplyr::case_match(
            .data$TPGESTAO,
            "D" ~ "Dupla",
            "E" ~ "Estadual",
            "M" ~ "Municipal",
            "Z" ~ "Sem gest\u00e3o",
            "S" ~ "Sem gest\u00e3o",
            .default = .data$TPGESTAO
          )
        ) %>%
        dplyr::mutate(TPGESTAO = as.factor(.data$TPGESTAO))
    }

    # PF_PJ
    if("PF_PJ" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          PF_PJ = dplyr::case_match(
            .data$PF_PJ,
            "1" ~ "Pessoa f\u00edsica",
            "3" ~ "Pessoa jur\u00eddica",
            .default = .data$PF_PJ
          )
        ) %>%
        dplyr::mutate(PF_PJ = as.factor(.data$PF_PJ))
    }

    # CPF_CNPJ
    if("CPF_CNPJ" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CPF_CNPJ = as.numeric(.data$CPF_CNPJ))
    }

    # NIV_DEP
    if("NIV_DEP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          NIV_DEP = dplyr::case_match(
            .data$NIV_DEP,
            "1" ~ "Individual",
            "3" ~ "Mantida",
            .default = .data$NIV_DEP
          )
        ) %>%
        dplyr::mutate(NIV_DEP = as.factor(.data$NIV_DEP))
    }

    # CNPJ_MAN
    if("CNPJ_MAN" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CNPJ_MAN = as.numeric(.data$CNPJ_MAN))
    }

    # ESFERA_A
    if("ESFERA_A" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          ESFERA_A = dplyr::case_match(
            .data$ESFERA_A,
            "-99" ~ NA,
            "1" ~ "Federal",
            "2" ~ "Estadual",
            "3" ~ "Municipal",
            "4" ~ "Privada",
            .default = .data$ESFERA_A
          )
        ) %>%
        dplyr::mutate(ESFERA_A = as.factor(.data$ESFERA_A))
    }

    # ATIVIDAD
    if("ATIVIDAD" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          ATIVIDAD = dplyr::case_match(
            .data$ATIVIDAD,
            "-99" ~ NA,
            "1" ~ "Unidade Universit\u00e1ria",
            "2" ~ "Unidade Escola Superior Isolada",
            "3" ~ "Unidade Auxiliar de Ensino",
            "4" ~ "Unidade sem atividade de Ensino",
            "5" ~ "Hospital de ensino",
            .default = .data$ATIVIDAD
          )
        ) %>%
        dplyr::mutate(ATIVIDAD = as.factor(.data$ATIVIDAD))
    }

    # RETENCAO
    if("RETENCAO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          RETENCAO = dplyr::case_match(
            .data$RETENCAO,
            "0" ~ NA,
            "10" ~ "Estabelecimento p\u00fablico",
            "11" ~ "Estabelecimento filantr\u00f3pico",
            "12" ~ "Estabelecimento sem fins lucrativos",
            "13" ~ "Estabelecimento privado luvrativa simples",
            "14" ~ "Estabelecimento privado luvrativa",
            "15" ~ "Estabelecimento sindical",
            "16" ~ "Estabelecimento pessoa f\u00edsica",
            .default = .data$RETENCAO
          )
        ) %>%
        dplyr::mutate(RETENCAO = as.factor(.data$RETENCAO))
    }

    # NATUREZA
    if("NATUREZA" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          NATUREZA = dplyr::case_match(
            .data$NATUREZA,
            "-99" ~ NA,
            "1" ~ "Administra\u00e7\u00e3o Direta da Sa\u00fade (MS, SES, e SMS)",
            "2" ~ "Adm Direta outros org\u00e3os (MEX, MEx, Marinha,...)",
            "3" ~ "Adm Indireta - Autarquias",
            "4" ~ "Adm Indireta - Funda\u00e7\u00e3o P\u00fablica",
            "5" ~ "Adm Indireta - Empresa P\u00fablica",
            "6" ~ "Adm Indireta - Organiza\u00e7\u00e3o Social P\u00fablica",
            "7" ~ "Empresa Privada",
            "8" ~ "Funda\u00e7\u00e3o Privada",
            "9" ~ "Cooperativa",
            "10" ~ "Servi\u00e7o Social Aut\u00f4nomo",
            "11" ~ "Entidade Beneficente sem fins lucrativos",
            "12" ~ "Economia Mista",
            "13" ~ "Sindicato",
            "0" ~ "Natureza inexistente",
            .default = .data$NATUREZA
          )
        ) %>%
        dplyr::mutate(NATUREZA = as.factor(.data$NATUREZA))
    }

    # CLIENTEL
    if("CLIENTEL" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          CLIENTEL = dplyr::case_match(
            .data$CLIENTEL,
            "-99" ~ NA,
            "1" ~ "Atendimento de demanda espont\u00e2nea",
            "2" ~ "Atendimento de demanda referenciada",
            "3" ~ "Atendimento de demanda espont\u00e2nea e referenciada",
            "0" ~ "Fluxo de Clientela n\u00e3o exigido",
            .default = .data$CLIENTEL
          )
        ) %>%
        dplyr::mutate(CLIENTEL = as.factor(.data$CLIENTEL))
    }

    # TP_UNID
    if("TP_UNID" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          TP_UNID = dplyr::case_match(
            .data$TP_UNID,
            "1" ~ "Posto de sa\u00fade",
            "2" ~ "Centro de sa\u00fade / Unidade b\u00e1sica",
            "4" ~ "Policl\u00ednica",
            "5" ~ "Hospital geral",
            "7" ~ "Hospital Especializado",
            "9" ~ "Pronto socorro de hospital geral (antigo)",
            "12" ~ "Pronto socorro traumato-ortop\u00e9dico (antigo)",
            "15" ~ "Unidade mista",
            "20" ~ "Pronto socorro geral",
            "21" ~ "Pronto socorro especializado",
            "22" ~ "Consult\u00f3rio isolado",
            "32" ~ "Unidade m\u00f3vel fluvial",
            "36" ~ "Cl\u00ednica / Centro de sa\u00fade de especialidade",
            "39" ~ "Unidade de apoio diagnose e terapia (SADT isolado)",
            "40" ~ "Unidade m\u00f3vel terrestre",
            "42" ~ "Unidade m\u00f3vel de n\u00edvel pr\u00e9-hospitalar na \u00e1rea de urg\u00eancia",
            "43" ~ "Farm\u00e1cia",
            "45" ~ "Unidade de sa\u00fade da fam\u00edlia",
            "50" ~ "Unidade de vigil\u00e2ncia em sa\u00fade",
            "60" ~ "Cooperativa ou empresa de cess\u00e3o de trabalhadores na sa\u00fade",
            "61" ~ "Centro de parto normal - isolado",
            "62" ~ "Hospital / Dia - Isolado",
            "63" ~ "Unidade autorizadora",
            "64" ~ "Central de regula\u00e7\u00e3o de servi\u00e7os de sa\u00fade",
            "65" ~ "Unidade de vigil\u00e2ncia epidemiol\u00f3gica (antigo)",
            "66" ~ "Unidade de vigil\u00e2ncia sanit\u00e1ria (antigo)",
            "67" ~ "Laborat\u00f3rio central de sa\u00fade p\u00fablica LACEN",
            "68" ~ "Central de gest\u00e3o em sa\u00fade",
            "69" ~ "Centro de aten\u00e7\u00e3o hemoterapia e/ou hematologica",
            "70" ~ "Centro de aten\u00e7\u00e3o psicosocial",
            "71" ~ "Centro de apoio a sa\u00fade da fam\u00edlia",
            "72" ~ "Unidade de aten\u00e7\u00e3o a sa\u00fade ind\u00edgena",
            "73" ~ "Pronto atendimento",
            "74" ~ "P\u00f3lo academia da sa\u00fade",
            "75" ~ "Telessa\u00fade",
            "76" ~ "Central de regula\u00e7\u00e3o m\u00e9dica das urg\u00eancias",
            "77" ~ "Servi\u00e7o de aten\u00e7\u00e3o domiciliar isolado (Home care)",
            "78" ~ "Unidade de aten\u00e7\u00e3o em regime residencial",
            "79" ~ "Oficina ortop\u00e9dica",
            "80" ~ "Laborat\u00f3rio de sa\u00fade p\u00fablica",
            "81" ~ "Central de regula\u00e7\u00e3o do acesso",
            "82" ~ "Central de notifica\u00e7\u00e3o, capta\u00e7\u00e3o e distribui\u00e7\u00e3o de \u00f3rg\u00e3os estadual",
            "83" ~ "P\u00f3lo de preven\u00e7\u00e3o de doen\u00e7as e agravos e promo\u00e7\u00e3o da sa\u00fade",
            .default = .data$TP_UNID
          )
        ) %>%
        dplyr::mutate(TP_UNID = as.factor(.data$TP_UNID))
    }

    # TURNO_AT
    if("TURNO_AT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          TURNO_AT = dplyr::case_match(
            .data$TURNO_AT,
            "-99" ~ NA,
            "1" ~ "Turnos intermitentes",
            "2" ~ "Cont\u00ednuo 24h/dia (Pl Sab Dom Fer)",
            "3" ~ "Manh\u00e3 / Tarde / Noite",
            "4" ~ "Manh\u00e3",
            "5" ~ "Tarde",
            "6" ~ "Manh\u00e3 / Tarde",
            "7" ~ "Noite",
            .default = .data$TURNO_AT
          )
        ) %>%
        dplyr::mutate(TURNO_AT = as.factor(.data$TURNO_AT))
    }

    # NIV_HIER
    if("NIV_HIER" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          NIV_HIER = dplyr::case_match(
            .data$NIV_HIER,
            "-99" ~ NA,
            "99" ~ NA,
            "0" ~ NA,
            "1"~ "PAB-PABA",
            "2"~ "M\u00e9dia M1",
            "3"~ "M\u00e9dia M2 e M3",
            "4"~ "Alta complexidade ambulatorial",
            "5"~ "Baixa M1 e M2",
            "6"~ "M\u00e9dia M2 e M3",
            "7"~ "M\u00e9dia M3",
            "8"~ "Alta complexidade hospitalar / ambulatorial",
            .default = .data$NIV_HIER
          )
        ) %>%
        dplyr::mutate(NIV_HIER = as.factor(.data$NIV_HIER))
    }

    # TERCEIRO
    if("TERCEIRO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          TERCEIRO = dplyr::case_match(
            .data$TERCEIRO,
            "1" ~ "Sim",
            "0" ~ "N\u00e3o",
            "2" ~ "N\u00e3o",
            .default = .data$TERCEIRO
          )
        ) %>%
        dplyr::mutate(TERCEIRO = as.factor(.data$TERCEIRO))
    }

    # CPF_PROF
    if("CPF_PROF" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          CPF_PROF = dplyr::case_match(
            .data$CPF_PROF,
            "99999999999" ~ NA,
            "00000000000000" ~ NA,
            .default = .data$CPF_PROF
          )
        ) %>%
        dplyr::mutate(CPF_PROF = as.factor(.data$CPF_PROF))
    }

    # CBO
    if ("CBO" %in% variables_names) {
      data <- data %>%
        dplyr::mutate(CBO = as.character(.data$CBO)) %>%
        dplyr::left_join(microdatasus::tabCBO, by = c("CBO" = "cod"))
    }

    # NOMEPROF
    if("NOMEPROF" %in% variables_names){
      data <- data %>%
        dplyr::mutate(NOMEPROF = as.character(.data$NOMEPROF))
    }

    # VINCULAC
    if("VINCULAC" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          VINCULAC = dplyr::case_match(
            .data$VINCULAC,
            "1" ~ "Profissional CONTRATADO",
            "2" ~ "Profissional AUT\u00d4NOMO",
            "3" ~ "Profissional V\u00cdNCULO N\u00c3O IDENTIFICADO",
            .default = .data$VINCULAC
          )
        ) %>%
        dplyr::mutate(VINCULAC = as.factor(.data$VINCULAC))
    }

    # NAT_JUR
    if("NAT_JUR" %in% variables_names){
      data <- data %>%
        dplyr::mutate(
          NAT_JUR = dplyr::case_match(
            .data$NAT_JUR,
            "0" ~ NA,
            "1000" ~ "Administra\u00e7\u00e3o P\u00fablica",
            "1015" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Federal",
            "1023" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Estadual ou do Distrito Federal",
            "1031" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Municipal",
            "1040" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Federal",
            "1058" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Estadual ou do Distrito Federal",
            "1066" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Municipal",
            "1074" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Federal",
            "1082" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Estadual",
            "1104" ~ "Autarquia Federal",
            "1112" ~ "Autarquia Estadual ou do Distrito Federal",
            "1120" ~ "Autarquia Municipal",
            "1139" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Federal",
            "1147" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Estadual ou do Distrito Federal",
            "1155" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Municipal",
            "1163" ~ "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Federal",
            "1171" ~ "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Estadual ou do Distrito Federal",
            "1180" ~ "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Municipal",
            "1198" ~ "Comiss\u00e3o Polinacional",
            "1201" ~ "Fundo P\u00fablico",
            "1210" ~ "Cons\u00f3rcio P\u00fablico de Direito P\u00fablico (Associa\u00e7\u00e3o P\u00fablica)",
            "1228" ~ "Cons\u00f3rcio P\u00fablico de Direito Privado",
            "1236" ~ "Estado ou Distrito Federal",
            "1244" ~ "Munic\u00edpio",
            "1252" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Federal",
            "1260" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Estadual ou do Distrito Federal",
            "1279" ~ "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Municipal",
            "2000" ~ "Entidades Empresariais",
            "2001" ~ "Empresa P\u00fablica",
            "2038" ~ "Sociedade de Economia Mista",
            "2046" ~ "Sociedade An\u00f4nima Aberta",
            "2054" ~ "Sociedade An\u00f4nima Fechada",
            "2062" ~ "Sociedade Empres\u00e1ria Limitada",
            "2070" ~ "Sociedade Empres\u00e1ria em Nome Coletivo",
            "2089" ~ "Sociedade Empres\u00e1ria em Comandita Simples",
            "2097" ~ "Sociedade Empres\u00e1ria em Comandita por A\u00e7\u00f5es",
            "2127" ~ "Sociedade em Conta de Participa\u00e7\u00e3o",
            "2135" ~ "Empres\u00e1rio (Individual)",
            "2143" ~ "Cooperativa",
            "2151" ~ "Cons\u00f3rcio de Sociedades",
            "2160" ~ "Grupo de Sociedades",
            "2178" ~ "Estabelecimento, no Brasil, de Sociedade Estrangeira",
            "2194" ~ "Estabelecimento, no Brasil, de Empresa Binacional Argentino-Brasileira",
            "2216" ~ "Empresa Domiciliada no Exterior",
            "2224" ~ "Clube/Fundo de Investimento",
            "2232" ~ "Sociedade Simples Pura",
            "2240" ~ "Sociedade Simples Limitada",
            "2259" ~ "Sociedade Simples em Nome Coletivo",
            "2267" ~ "Sociedade Simples em Comandita Simples",
            "2275" ~ "Empresa Binacional",
            "2283" ~ "Cons\u00f3rcio de Empregadores",
            "2291" ~ "Cons\u00f3rcio Simples",
            "2305" ~ "Empresa Individual de Responsabilidade Limitada (de Natureza Empres\u00e1ria)",
            "2313" ~ "Empresa Individual de Responsabilidade Limitada (de Natureza Simples)",
            "2321" ~ "Sociedade Unipessoal de Advogados",
            "2330" ~ "Cooperativas de Consumo",
            "3000" ~ "Entidades sem Fins Lucrativos",
            "3034" ~ "Servi\u00e7o Notarial e Registral (Cart\u00f3rio)",
            "3069" ~ "Funda\u00e7\u00e3o Privada",
            "3077" ~ "Servi\u00e7o Social Aut\u00f4nomo",
            "3085" ~ "Condom\u00ednio Edil\u00edcio",
            "3107" ~ "Comiss\u00e3o de Concilia\u00e7\u00e3o Pr\u00e9via",
            "3115" ~ "Entidade de Media\u00e7\u00e3o e Arbitragem",
            "3131" ~ "Entidade Sindical",
            "3204" ~ "Estabelecimento, no Brasil, de Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Estrangeiras",
            "3212" ~ "Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Domiciliada no Exterior",
            "3220" ~ "Organiza\u00e7\u00e3o Religiosa",
            "3239" ~ "Comunidade Ind\u00edgena",
            "3247" ~ "Fundo Privado",
            "3255" ~ "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Nacional de Partido Pol\u00edtico",
            "3263" ~ "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Regional de Partido Pol\u00edtico",
            "3271" ~ "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Local de Partido Pol\u00edtico",
            "3280" ~ "Comit\u00ea Financeiro de Partido Pol\u00edtico",
            "3298" ~ "Frente Plebiscit\u00e1ria ou Referend\u00e1ria",
            "3306" ~ "Organiza\u00e7\u00e3o Social (OS)",
            "3310" ~ "Demais Condom\u00ednios",
            "3999" ~ "Associa\u00e7\u00e3o Privada",
            "4000" ~ "Pessoas F\u00edsicas",
            "4014" ~ "Empresa Individual Imobili\u00e1ria",
            "4022" ~ "Segurado Especial",
            "4081" ~ "Contribuinte individual",
            "4090" ~ "Candidato a Cargo Pol\u00edtico Eletivo",
            "4111" ~ "Leiloeiro",
            "4124" ~ "Produtor Rural (Pessoa F\u00edsica)",
            "5000" ~ "Organiza\u00e7\u00f5es Internacionais e Outras Institui\u00e7\u00f5es Extraterritoriais",
            "5010" ~ "Organiza\u00e7\u00e3o Internacional",
            "5029" ~ "Representa\u00e7\u00e3o Diplom\u00e1tica Estrangeira",
            "5037" ~ "Outras Institui\u00e7\u00f5es Extraterritoriais",
            .default = .data$NAT_JUR
          )
        ) %>%
        dplyr::mutate(NAT_JUR = as.factor(.data$NAT_JUR))
    }
  }

  # From data.table to tibble
  data <- tibble::as_tibble(data)

  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  data <- suppressWarnings(tibble::as_tibble(lapply(X = data, FUN = stringi::stri_unescape_unicode)))

  # Return
  return(data)
}
