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
#' @examples \dontrun{
#' df <- fetch_datasus(year_start = 2015, month_start = 1,
#'                     year_end = 2015, month_end = 1,
#'                     uf = "RJ",
#'                     information_system = "CNES-ST")
#' df_a <- process_cnes(df, information_system = "CNES-ST")
#' df_b <- process_cnes(df, information_system = "CNES-ST", nomes = FALSE, municipality_data = FALSE)
#' }
#' @export

process_cnes2 <- function(data, information_system = c("CNES-ST", "CNES-PF"), nomes = TRUE, municipality_data = TRUE) {
  # Check information system
  available_information_system <- c("CNES-ST", "CNES-PF")
  if(!(information_system %in% available_information_system)) stop("Health informaton system unknown.")

  # Variables names
  variables_names <- names(data)

  # convert to data.table
  setDT(data)

  # Convert cols to integer
  cols_to_integer <- c("MICR_REG", "CPF_CNPJ")

  lapply( X = cols_to_integer,  FUN = convert_to_integer)



#--------------------------------------------------------------------------------------------------------------
  if(information_system == "CNES-ST"){
    # CNES
    if("CNES" %in% variables_names){
      #data$CNES <- as.integer(as.character(levels(data$CNES))[data$CNES])
      data[, CNES := as.character(CNES)]
    }

    # Nome fantasia e razão social
    if(nomes == TRUE){
      #data$CNES_integer <- as.integer(as.character(levels(data$CNES))[data$CNES])
      data[, CNES_integer := as.integer(CNES)]
      data <- dplyr::left_join(data, microdatasus::cadger, by = c("CNES_integer" = "CNES"))
      data[, CNES_integer := NULL]
    }

    # CODUFMUN
    if ("CODUFMUN" %in% variables_names & municipality_data == TRUE) {
      data[, CODUFMUN := as.numeric(CODUFMUN)]
      data <- dplyr::left_join(data, microdatasus::tabMun, by = c("CODUFMUN" = "munResCod"))
    } else {
      data[, CODUFMUN := as.character(CODUFMUN)]
    }

    # convert to data.table
    setDT(data)

    # Convert cols to character
    cols_to_character <- c("REGSAUDE", "TPGESTAO")

    lapply( X = cols_to_character,  FUN = convert_to_character)


    # Convert cols to numeric
    cols_to_numeric <- c("CLIENTEL", "TP_UNID", "TURNO_AT", "NIV_HIER",
                         "TP_PREST", "ORGEXPED", "PF_PJ", "NIV_DEP", "COD_IR",
                         "ESFERA_A", "RETENCAO", "ATIVIDAD", "NATUREZA",
                         "NAT_JUR", "CLASAVAL"
                         )


    lapply( X = cols_to_numeric,  FUN = convert_to_numeric)

    # Convert cols to integer
    cols_to_integer <- c("COD_CEP", "CPF_CNPJ", "CNPJ_MAN", "DISTRSAN",
                         "CO_BANCO", "CO_AGENC", "C_CORREN", "CONTRATM",
                         "DT_PUBLM", "CONTRATE", "DT_PUBLE", "ALVARA",
                         "DT_EXPED", "DT_ACRED", "DT_PNASS"
    )

    lapply( X = cols_to_integer,  FUN = convert_to_integer)

    # Convert cols to Sim ou Nao
    cols_to_sim_nao <- c("AV_ACRED", "AV_PNASS", "GESPRG1E", "GESPRG1M",
                         "GESPRG2E", "GESPRG2M", "GESPRG4E", "GESPRG4M",
                         "NIVATE_A", "GESPRG3E", "GESPRG3E", "GESPRG3M",
                         "GESPRG5E", "GESPRG5M", "GESPRG6E", "GESPRG6M",
                         "NIVATE_H", "GESPRG3E", "URGEMERG", "ATENDAMB",
                         "CENTROBS", "CENTRNEO", "ATENDHOS", "SERAP01P",
                         "SERAP01T", "SERAP02P", "SERAP02T", "SERAP03P",
                         "SERAP03T", "SERAP04P", "SERAP04T", "SERAP05P",
                         "SERAP05T", "SERAP06P", "SERAP06T", "SERAP07P",
                         "SERAP07T", "SERAP08P", "SERAP08T", "SERAP09P",
                         "SERAP09T", "SERAP10P", "SERAP10T", "SERAP11P",
                         "SERAP11T", "SERAPOIO", "RES_BIOL", "RES_QUIM",
                         "RES_RADI", "RES_COMU", "COLETRES", "COMISS01",
                         "COMISS02", "COMISS03", "COMISS04", "COMISS05",
                         "COMISS06", "COMISS07", "COMISS08", "COMISS09",
                         "COMISS10", "COMISS11", "COMISS12", "COMISSAO",
                         "AP01CV01", "AP01CV02", "AP01CV05", "AP01CV06",
                         "AP01CV03", "AP01CV04", "AP02CV01", "AP02CV02",
                         "AP02CV05", "AP02CV06", "AP02CV03", "AP02CV04",
                         "AP03CV01", "AP03CV02", "AP03CV05", "AP03CV06",
                         "AP03CV03", "AP03CV04", "AP04CV01", "AP04CV02",
                         "AP04CV05", "AP04CV06", "AP04CV03", "AP04CV04",
                         "AP05CV01", "AP05CV02", "AP05CV05", "AP05CV06",
                         "AP05CV03", "AP05CV04", "AP06CV01", "AP06CV02",
                         "AP06CV05", "AP06CV06", "AP06CV03", "AP06CV04",
                         "AP07CV01", "AP07CV02", "AP07CV05", "AP07CV06",
                         "AP07CV03", "AP07CV04", "ATEND_PR", "ATEND_PR",
                         "VINC_SUS"
                         )

    lapply( X = cols_to_sim_nao,  FUN = convert_dummy_to_sim_nao)



    # PF_PJ
    if("PF_PJ" %in% variables_names){

      data[, NIV_DEP := fcase(
        PF_PJ == 1, "Pessoa f\u00edsica",
        PF_PJ == 3, "Pessoa jur\u00eddica"
        )]
      data[, PF_PJ := factor(PF_PJ)]

    }

    # NIV_DEP
    if("NIV_DEP" %in% variables_names){

      data[, NIV_DEP := fcase(NIV_DEP == 1, "Individual",
                              NIV_DEP == 3, "Mantida")]
      data[, NIV_DEP := factor(NIV_DEP)]

    }



    # COD_IR
    if("COD_IR" %in% variables_names){

      data[, COD_IR := fcase(
        COD_IR == 0, as.character(NA),
        COD_IR == 10, "Estabelecimento p\u00fablico",
        COD_IR == 11, "Estabelecimento filantr\u00f3pico",
        COD_IR == 12, "Estabelecimento sem fins lucrativos",
        COD_IR == 13, "Estabelecimento privado luvrativa simples",
        COD_IR == 14, "Estabelecimento privado luvrativa",
        COD_IR == 15, "Estabelecimento sindical",
        COD_IR == 16, "Estabelecimento pessoa f\u00edsica",
        COD_IR == 19, "Estabelecimento Ret.Manten.c\u00f3digo 19"
      )]
      data[, COD_IR := factor(COD_IR)]

    }


    # TPGESTAO
    if("TPGESTAO" %in% variables_names){

      data[, TPGESTAO := fcase(
        TPGESTAO == "D", "Dupla",
        TPGESTAO == "E", "Estadual",
        TPGESTAO == "M", "Municipal",
        TPGESTAO == "Z", "Sem gest\u00e3o",
        TPGESTAO == "S", "Sem gest\u00e3o"
      )]
      data[, TPGESTAO := factor(TPGESTAO)]
    }

    # ESFERA_A
    if("ESFERA_A" %in% variables_names){
      data[, ESFERA_A := fcase(
        ESFERA_A == 1, "Federal",
        ESFERA_A == 2, "Estadual",
        ESFERA_A == 3, "Municipal",
        ESFERA_A == 4, "Privada",
        ESFERA_A == -99, as.character(NA)
        )]
      data[, ESFERA_A := factor(ESFERA_A)]

    }

    # RETENCAO
    if("RETENCAO" %in% variables_names){
      data[, RETENCAO := fcase(
        RETENCAO==0, as.character(NA),
        RETENCAO==10, "Estabelecimento p\u00fablico",
        RETENCAO==11, "Estabelecimento filantr\u00f3pico",
        RETENCAO==12, "Estabelecimento sem fins lucrativos",
        RETENCAO==13, "Estabelecimento privado luvrativa simples",
        RETENCAO==14, "Estabelecimento privado luvrativa",
        RETENCAO==15, "Estabelecimento sindical",
        RETENCAO==16, "Estabelecimento pessoa f\u00edsica"
        )]
      data[, RETENCAO := factor(RETENCAO)]

    }


    # ATIVIDAD
    if("ATIVIDAD" %in% variables_names){
      data[, ATIVIDAD := fcase(
        ATIVIDAD==-99, as.character(NA),
        ATIVIDAD==1, "Unidade Universit\u00e1ria",
        ATIVIDAD==2, "Unidade Escola Superior Isolada",
        ATIVIDAD==3, "Unidade Auxiliar de Ensino",
        ATIVIDAD==4, "Unidade sem atividade de Ensino",
        ATIVIDAD==5, "Hospital de ensino"
        )]
      data[, ATIVIDAD := factor(ATIVIDAD)]

    }

    # NATUREZA
    if("NATUREZA" %in% variables_names){
      data[, NATUREZA := fcase(
        NATUREZA==-99, as.character(NA),
        NATUREZA==1, "Administra\u00e7\u00e3o Direta da Sa\u00fade (MS, SES, e SMS)",
        NATUREZA==2, "Adm Direta outros org\u00e3os (MEX, MEx, Marinha,...)",
        NATUREZA==3, "Adm Indireta - Autarquias",
        NATUREZA==4, "Adm Indireta - Funda\u00e7\u00e3o P\u00fablica",
        NATUREZA==5, "Adm Indireta - Empresa P\u00fablica",
        NATUREZA==6, "Adm Indireta - Organiza\u00e7\u00e3o Social P\u00fablica",
        NATUREZA==7, "Empresa Privada",
        NATUREZA==8, "Funda\u00e7\u00e3o Privada",
        NATUREZA==9, "Cooperativa",
        NATUREZA==10, "Servi\u00e7o Social Aut\u00f4nomo",
        NATUREZA==11, "Entidade Beneficente sem fins lucrativos",
        NATUREZA==12, "Economia Mista",
        NATUREZA==13, "Sindicato",
        NATUREZA==0, "Natureza inexistente"
        )]
      data[, NATUREZA := factor(NATUREZA)]

    }

    # NAT_JUR
    if("NAT_JUR" %in% variables_names){
      data[, NAT_JUR := fcase(
        NAT_JUR==1015, "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Federal",
        NAT_JUR==1023, "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Estadual ou do Distrito Federal",
        NAT_JUR==1031, "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Municipal",
        NAT_JUR==1040, "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Federal",
        NAT_JUR==1058, "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Estadual ou do Distrito Federal",
        NAT_JUR==1066, "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Municipal",
        NAT_JUR==1074, "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Federal",
        NAT_JUR==1082, "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Estadual",
        NAT_JUR==1104, "Autarquia Federal",
        NAT_JUR==1112, "Autarquia Estadual ou do Distrito Federal",
        NAT_JUR==1120, "Autarquia Municipal",
        NAT_JUR==1139, "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Federal",
        NAT_JUR==1147, "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Estadual ou do Distrito Federal",
        NAT_JUR==1155, "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Municipal",
        NAT_JUR==1163, "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Federal",
        NAT_JUR==1171, "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Estadual ou do Distrito Federal",
        NAT_JUR==1180, "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Municipal",
        NAT_JUR==1198, "Comiss\u00e3o Polinacional",
        NAT_JUR==1201, "Fundo P\u00fablico",
        NAT_JUR==1210, "Cons\u00f3rcio P\u00fablico de Direito P\u00fablico (Associa\u00e7\u00e3o P\u00fablica)",
        NAT_JUR==1228, "Cons\u00f3rcio P\u00fablico de Direito Privado",
        NAT_JUR==1236, "Estado ou Distrito Federal",
        NAT_JUR==1244, "Munic\u00edpio",
        NAT_JUR==1252, "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Federal",
        NAT_JUR==1260, "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Estadual ou do Distrito Federal",
        NAT_JUR==1279, "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Municipal",
        NAT_JUR==2011, "Empresa P\u00fablica",
        NAT_JUR==2038, "Sociedade de Economia Mista",
        NAT_JUR==2046, "Sociedade An\u00f4nima Aberta",
        NAT_JUR==2054, "Sociedade An\u00f4nima Fechada",
        NAT_JUR==2062, "Sociedade Empres\u00e1ria Limitada",
        NAT_JUR==2070, "Sociedade Empres\u00e1ria em Nome Coletivo",
        NAT_JUR==2089, "Sociedade Empres\u00e1ria em Comandita Simples",
        NAT_JUR==2097, "Sociedade Empres\u00e1ria em Comandita por A\u00e7\u00f5es",
        NAT_JUR==2127, "Sociedade em Conta de Participa\u00e7\u00e3o",
        NAT_JUR==2135, "Empres\u00e1rio (Individual)",
        NAT_JUR==2143, "Cooperativa",
        NAT_JUR==2151, "Cons\u00f3rcio de Sociedades",
        NAT_JUR==2160, "Grupo de Sociedades",
        NAT_JUR==2178, "Estabelecimento, no Brasil, de Sociedade Estrangeira",
        NAT_JUR==2194, "Estabelecimento, no Brasil, de Empresa Binacional Argentino-Brasileira",
        NAT_JUR==2216, "Empresa Domiciliada no Exterior",
        NAT_JUR==2224, "Clube/Fundo de Investimento",
        NAT_JUR==2232, "Sociedade Simples Pura",
        NAT_JUR==2240, "Sociedade Simples Limitada",
        NAT_JUR==2259, "Sociedade Simples em Nome Coletivo",
        NAT_JUR==2267, "Sociedade Simples em Comandita Simples",
        NAT_JUR==2275, "Empresa Binacional",
        NAT_JUR==2283, "Cons\u00f3rcio de Empregadores",
        NAT_JUR==2291, "Cons\u00f3rcio Simples",
        NAT_JUR==2305, "Empresa Individual de Responsabilidade Limitada (de Natureza Empres\u00e1ria)",
        NAT_JUR==2313, "Empresa Individual de Responsabilidade Limitada (de Natureza Simples)",
        NAT_JUR==2321, "Sociedade Unipessoal de Advogados",
        NAT_JUR==2330, "Cooperativas de Consumo",
        NAT_JUR==3034, "Servi\u00e7o Notarial e Registral (Cart\u00f3rio)",
        NAT_JUR==3069, "Funda\u00e7\u00e3o Privada",
        NAT_JUR==3077, "Servi\u00e7o Social Aut\u00f4nomo",
        NAT_JUR==3085, "Condom\u00ednio Edil\u00edcio",
        NAT_JUR==3107, "Comiss\u00e3o de Concilia\u00e7\u00e3o Pr\u00e9via",
        NAT_JUR==3115, "Entidade de Media\u00e7\u00e3o e Arbitragem",
        NAT_JUR==3131, "Entidade Sindical",
        NAT_JUR==3204, "Estabelecimento, no Brasil, de Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Estrangeiras",
        NAT_JUR==3212, "Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Domiciliada no Exterior",
        NAT_JUR==3220, "Organiza\u00e7\u00e3o Religiosa",
        NAT_JUR==3239, "Comunidade Ind\u00edgena",
        NAT_JUR==3247, "Fundo Privado",
        NAT_JUR==3255, "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Nacional de Partido Pol\u00edtico",
        NAT_JUR==3263, "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Regional de Partido Pol\u00edtico",
        NAT_JUR==3271, "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Local de Partido Pol\u00edtico",
        NAT_JUR==3280, "Comit\u00ea Financeiro de Partido Pol\u00edtico",
        NAT_JUR==3298, "Frente Plebiscit\u00e1ria ou Referend\u00e1ria",
        NAT_JUR==3306, "Organiza\u00e7\u00e3o Social (OS)",
        NAT_JUR==3310, "Demais Condom\u00ednios",
        NAT_JUR==3999, "Associa\u00e7\u00e3o Privada",
        NAT_JUR==4014, "Empresa Individual Imobili\u00e1ria",
        NAT_JUR==4022, "Segurado Especial",
        NAT_JUR==4081, "Contribuinte individual",
        NAT_JUR==4090, "Candidato a Cargo Pol\u00edtico Eletivo",
        NAT_JUR==4111, "Leiloeiro",
        NAT_JUR==4124, "Produtor Rural (Pessoa F\u00edsica)",
        NAT_JUR==5010, "Organiza\u00e7\u00e3o Internacional",
        NAT_JUR==5029, "Representa\u00e7\u00e3o Diplom\u00e1tica Estrangeira",
        NAT_JUR==5037, "Outras Institui\u00e7\u00f5es Extraterritoriais",
        NAT_JUR==0, "N\u00e3o especificado ou ignorado"
        )]
      data[, NAT_JUR := factor(NAT_JUR)]

    }





    # CLIENTEL
    if("CLIENTEL" %in% variables_names){
      data[, CLIENTEL := fcase(
        CLIENTEL==-99, as.character(NA),
        CLIENTEL==1, "Atendimento de demanda espont\u00e2nea",
        CLIENTEL==2, "Atendimento de demanda referenciada",
        CLIENTEL==3, "Atendimento de demanda espont\u00e2nea e referenciada",
        CLIENTEL==0, "Fluxo de Clientela n\u00e3o exigido"
        )]
      data[, CLIENTEL := factor(CLIENTEL)]
    }

    # TP_UNID
    if("TP_UNID" %in% variables_names){

      data[, TP_UNID := fcase(
        TP_UNID==1, "Posto de sa\u00fade",
        TP_UNID==2, "Centro de sa\u00fade / Unidade b\u00e1sica",
        TP_UNID==4, "Policl\u00ednica",
        TP_UNID==5, "Hospital geral",
        TP_UNID==7, "Hospital Especializado",
        TP_UNID==9, "Pronto socorro de hospital geral (antigo)",
        TP_UNID==12, "Pronto socorro traumato-ortop\u00e9dico (antigo)",
        TP_UNID==15, "Unidade mista",
        TP_UNID==20, "Pronto socorro geral",
        TP_UNID==21, "Pronto socorro especializado",
        TP_UNID==22, "Consult\u00f3rio isolado",
        TP_UNID==32, "Unidade m\u00f3vel fluvial",
        TP_UNID==36, "Cl\u00ednica / Centro de sa\u00fade de especialidade",
        TP_UNID==39, "Unidade de apoio diagnose e terapia (SADT isolado)",
        TP_UNID==40, "Unidade m\u00f3vel terrestre",
        TP_UNID==42, "Unidade m\u00f3vel de n\u00edvel pr\u00e9-hospitalar na \u00e1rea de urg\u00eancia",
        TP_UNID==43, "Farm\u00e1cia",
        TP_UNID==45, "Unidade de sa\u00fade da fam\u00edlia",
        TP_UNID==50, "Unidade de vigil\u00e2ncia em sa\u00fade",
        TP_UNID==60, "Cooperativa ou empresa de cess\u00e3o de trabalhadores na sa\u00fade",
        TP_UNID==61, "Centro de parto normal - isolado",
        TP_UNID==62, "Hospital / Dia - Isolado",
        TP_UNID==63, "Unidade autorizadora",
        TP_UNID==64, "Central de regula\u00e7\u00e3o de servi\u00e7os de sa\u00fade",
        TP_UNID==65, "Unidade de vigil\u00e2ncia epidemiol\u00f3gica (antigo)",
        TP_UNID==66, "Unidade de vigil\u00e2ncia sanit\u00e1ria (antigo)",
        TP_UNID==67, "Laborat\u00f3rio central de sa\u00fade p\u00fablica LACEN",
        TP_UNID==68, "Central de gest\u00e3o em sa\u00fade",
        TP_UNID==69, "Centro de aten\u00e7\u00e3o hemoterapia e/ou hematologica",
        TP_UNID==70, "Centro de aten\u00e7\u00e3o psicosocial",
        TP_UNID==71, "Centro de apoio a sa\u00fade da fam\u00edlia",
        TP_UNID==72, "Unidade de aten\u00e7\u00e3o a sa\u00fade ind\u00edgena",
        TP_UNID==73, "Pronto atendimento",
        TP_UNID==74, "P\u00f3lo academia da sa\u00fade",
        TP_UNID==75, "Telessa\u00fade",
        TP_UNID==76, "Central de regula\u00e7\u00e3o m\u00e9dica das urg\u00eancias",
        TP_UNID==77, "Servi\u00e7o de aten\u00e7\u00e3o domiciliar isolado (Home care)",
        TP_UNID==78, "Unidade de aten\u00e7\u00e3o em regime residencial",
        TP_UNID==79, "Oficina ortop\u00e9dica",
        TP_UNID==80, "Laborat\u00f3rio de sa\u00fade p\u00fablica",
        TP_UNID==81, "Central de regula\u00e7\u00e3o do acesso",
        TP_UNID==82, "Central de notifica\u00e7\u00e3o, capta\u00e7\u00e3o e distribui\u00e7\u00e3o de \u00f3rg\u00e3os estadual",
        TP_UNID==83, "P\u00f3lo de preven\u00e7\u00e3o de doen\u00e7as e agravos e promo\u00e7\u00e3o da sa\u00fade",
        )]
      data[, TP_UNID := factor(TP_UNID)]

    }

    # TURNO_AT
    if("TURNO_AT" %in% variables_names){

      data[, TURNO_AT := fcase(
        TURNO_AT==-99, as.character(NA),
        TURNO_AT==1, "Turnos intermitentes",
        TURNO_AT==2, "Cont\u00ednuo 24h/dia (Pl Sab Dom Fer)",
        TURNO_AT==3, "Manh\u00e3 / Tarde / Noite",
        TURNO_AT==4, "Manh\u00e3",
        TURNO_AT==5, "Tarde",
        TURNO_AT==6, "Manh\u00e3 / Tarde",
        TURNO_AT==7, "Noite"
        )]
      data[, TURNO_AT := factor(TURNO_AT)]

    }


    # NIV_HIER
    if("NIV_HIER" %in% variables_names){

      data[, NIV_HIER := fcase(
        NIV_HIER==0, as.character(NA),
        NIV_HIER==99, as.character(NA),
        NIV_HIER==1, "PAB-PABA",
        NIV_HIER==2, "M\u00e9dia M1",
        NIV_HIER==3, "M\u00e9dia M2 e M3",
        NIV_HIER==4, "Alta complexidade ambulatorial",
        NIV_HIER==5, "Baixa M1 e M2",
        NIV_HIER==6, "M\u00e9dia M2 e M3",
        NIV_HIER==7, "M\u00e9dia M3",
        NIV_HIER==8, "Alta complexidade hospitalar / ambulatorial",
        )]
      data[, NIV_HIER := factor(NIV_HIER)]

    }


    # TP_PREST
    if("TP_PREST" %in% variables_names){
      data[, TP_PREST := fcase(
        TP_PREST==-99, as.character(NA),
        TP_PREST==30, "P\u00fablico federal",
        TP_PREST==40, "P\u00fablico estadual",
        TP_PREST==50, "P\u00fablico municipal",
        TP_PREST==61, "Filantr\u00f3pico com CNAS v\u00e1lido",
        TP_PREST==80, "Sindicato",
        TP_PREST==20, "Privado com fins lucrativos",
        TP_PREST==22, "Privado optantes pelo simples",
        TP_PREST==60, "Privado sem fins lucrativos"
        )]
      data[, TP_PREST := factor(TP_PREST)]

    }



    # ORGEXPED
    if("ORGEXPED" %in% variables_names){

      data[, ORGEXPED := fcase(
        ORGEXPED==1, "SES",
        ORGEXPED==2, "SMS"
        )]

      data[, TP_PREST := factor(ORGEXPED)]

    }



    # CLASAVAL
    if("CLASAVAL" %in% variables_names){

      data[, CLASAVAL := fcase(
        CLASAVAL==1, "Acreditado no n\u00edvel 1",
        CLASAVAL==2, "Acreditado no n\u00edvel 2",
        CLASAVAL==3, "Acreditado no n\u00edvel 3",
        CLASAVAL==0, "N\u00e3o atendeu aos padr\u00f5es m\u00ednimos",
        CLASAVAL==-9, as.character(NA)
      )]

      data[, CLASAVAL := factor(CLASAVAL)]

    }






    # NAT_JUR
    if("NAT_JUR" %in% variables_names){
      data[, NAT_JUR := fcase(
        NAT_JUR==0, as.character(NA),
        NAT_JUR==1000, "Administra\u00e7\u00e3o P\u00fablica",
        NAT_JUR==1015, "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Federal",
        NAT_JUR==1023, "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Estadual ou do Distrito Federal",
        NAT_JUR==1031, "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Municipal",
        NAT_JUR==1040, "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Federal",
        NAT_JUR==1058, "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Estadual ou do Distrito Federal",
        NAT_JUR==1066, "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Municipal",
        NAT_JUR==1074, "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Federal",
        NAT_JUR==1082, "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Estadual",
        NAT_JUR==1104, "Autarquia Federal",
        NAT_JUR==1112, "Autarquia Estadual ou do Distrito Federal",
        NAT_JUR==1120, "Autarquia Municipal",
        NAT_JUR==1139, "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Federal",
        NAT_JUR==1147, "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Estadual ou do Distrito Federal",
        NAT_JUR==1155, "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Municipal",
        NAT_JUR==1163, "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Federal",
        NAT_JUR==1171, "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Estadual ou do Distrito Federal",
        NAT_JUR==1180, "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Municipal",
        NAT_JUR==1198, "Comiss\u00e3o Polinacional",
        NAT_JUR==1201, "Fundo P\u00fablico",
        NAT_JUR==1210, "Cons\u00f3rcio P\u00fablico de Direito P\u00fablico (Associa\u00e7\u00e3o P\u00fablica)",
        NAT_JUR==1228, "Cons\u00f3rcio P\u00fablico de Direito Privado",
        NAT_JUR==1236, "Estado ou Distrito Federal",
        NAT_JUR==1244, "Munic\u00edpio",
        NAT_JUR==1252, "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Federal",
        NAT_JUR==1260, "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Estadual ou do Distrito Federal",
        NAT_JUR==1279, "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Municipal",
        NAT_JUR==2000, "Entidades Empresariais",
        NAT_JUR==2001, "Empresa P\u00fablica",
        NAT_JUR==2038, "Sociedade de Economia Mista",
        NAT_JUR==2046, "Sociedade An\u00f4nima Aberta",
        NAT_JUR==2054, "Sociedade An\u00f4nima Fechada",
        NAT_JUR==2062, "Sociedade Empres\u00e1ria Limitada",
        NAT_JUR==2070, "Sociedade Empres\u00e1ria em Nome Coletivo",
        NAT_JUR==2089, "Sociedade Empres\u00e1ria em Comandita Simples",
        NAT_JUR==2097, "Sociedade Empres\u00e1ria em Comandita por A\u00e7\u00f5es",
        NAT_JUR==2127, "Sociedade em Conta de Participa\u00e7\u00e3o",
        NAT_JUR==2135, "Empres\u00e1rio (Individual)",
        NAT_JUR==2143, "Cooperativa",
        NAT_JUR==2151, "Cons\u00f3rcio de Sociedades",
        NAT_JUR==2160, "Grupo de Sociedades",
        NAT_JUR==2178, "Estabelecimento, no Brasil, de Sociedade Estrangeira",
        NAT_JUR==2194, "Estabelecimento, no Brasil, de Empresa Binacional Argentino-Brasileira",
        NAT_JUR==2216, "Empresa Domiciliada no Exterior",
        NAT_JUR==2224, "Clube/Fundo de Investimento",
        NAT_JUR==2232, "Sociedade Simples Pura",
        NAT_JUR==2240, "Sociedade Simples Limitada",
        NAT_JUR==2259, "Sociedade Simples em Nome Coletivo",
        NAT_JUR==2267, "Sociedade Simples em Comandita Simples",
        NAT_JUR==2275, "Empresa Binacional",
        NAT_JUR==2283, "Cons\u00f3rcio de Empregadores",
        NAT_JUR==2291, "Cons\u00f3rcio Simples",
        NAT_JUR==2305, "Empresa Individual de Responsabilidade Limitada (de Natureza Empres\u00e1ria)",
        NAT_JUR==2313, "Empresa Individual de Responsabilidade Limitada (de Natureza Simples)",
        NAT_JUR==2321, "Sociedade Unipessoal de Advogados",
        NAT_JUR==2330, "Cooperativas de Consumo",
        NAT_JUR==3000, "Entidades sem Fins Lucrativos",
        NAT_JUR==3034, "Servi\u00e7o Notarial e Registral (Cart\u00f3rio)",
        NAT_JUR==3069, "Funda\u00e7\u00e3o Privada",
        NAT_JUR==3077, "Servi\u00e7o Social Aut\u00f4nomo",
        NAT_JUR==3085, "Condom\u00ednio Edil\u00edcio",
        NAT_JUR==3107, "Comiss\u00e3o de Concilia\u00e7\u00e3o Pr\u00e9via",
        NAT_JUR==3115, "Entidade de Media\u00e7\u00e3o e Arbitragem",
        NAT_JUR==3131, "Entidade Sindical",
        NAT_JUR==3204, "Estabelecimento, no Brasil, de Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Estrangeiras",
        NAT_JUR==3212, "Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Domiciliada no Exterior",
        NAT_JUR==3220, "Organiza\u00e7\u00e3o Religiosa",
        NAT_JUR==3239, "Comunidade Ind\u00edgena",
        NAT_JUR==3247, "Fundo Privado",
        NAT_JUR==3255, "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Nacional de Partido Pol\u00edtico",
        NAT_JUR==3263, "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Regional de Partido Pol\u00edtico",
        NAT_JUR==3271, "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Local de Partido Pol\u00edtico",
        NAT_JUR==3280, "Comit\u00ea Financeiro de Partido Pol\u00edtico",
        NAT_JUR==3298, "Frente Plebiscit\u00e1ria ou Referend\u00e1ria",
        NAT_JUR==3306, "Organiza\u00e7\u00e3o Social (OS)",
        NAT_JUR==3310, "Demais Condom\u00ednios",
        NAT_JUR==3999, "Associa\u00e7\u00e3o Privada",
        NAT_JUR==4000, "Pessoas F\u00edsicas",
        NAT_JUR==4014, "Empresa Individual Imobili\u00e1ria",
        NAT_JUR==4022, "Segurado Especial",
        NAT_JUR==4081, "Contribuinte individual",
        NAT_JUR==4090, "Candidato a Cargo Pol\u00edtico Eletivo",
        NAT_JUR==4111, "Leiloeiro",
        NAT_JUR==4124, "Produtor Rural (Pessoa F\u00edsica)",
        NAT_JUR==5000, "Organiza\u00e7\u00f5es Internacionais e Outras Institui\u00e7\u00f5es Extraterritoriais",
        NAT_JUR==5010, "Organiza\u00e7\u00e3o Internacional",
        NAT_JUR==5029, "Representa\u00e7\u00e3o Diplom\u00e1tica Estrangeira",
        NAT_JUR==5037, "Outras Institui\u00e7\u00f5es Extraterritoriais"
        )]
      data[, NAT_JUR := factor(NAT_JUR)]

    }
  } else if(information_system == "CNES-PF"){

    # Convert cols to character
    cols_to_character <- c("CNES", "REGSAUDE", "NOMEPROF", "UFMUNRES",
                           "CBO", "CPF_PROF", "TPGESTAO")
    lapply( X = cols_to_character,  FUN = convert_to_character)


    # Convert cols to numeric
    cols_to_numeric <- c("NAT_JUR", "VINCULAC", "TERCEIRO", "NIV_HIER",
                         "TURNO_AT", "TP_UNID", "CLIENTEL", "NATUREZA",
                         "RETENCAO", "ATIVIDAD", "ESFERA_A", "NIV_DEP",
                         "PF_PJ")
    lapply( X = cols_to_numeric,  FUN = convert_to_numeric)


    # Convert cols to integer
    cols_to_integer <- c("DISTRSAN", "DISTRADM", "CNPJ_MAN")
    lapply( X = cols_to_integer,  FUN = convert_to_integer)


    # UFMUNRES
    if ("UFMUNRES" %in% variables_names & municipality_data == TRUE) {
      tabMun <- microdatasus::tabMun
      colnames(tabMun)[1] <- "UFMUNRES"
      data <- dplyr::left_join(data, microdatasus::tabMun, by = c("UFMUNRES" = "munResUf"))
      setDT(data)

    } else {
      convert_to_character("UFMUNRES")
    }


    # TPGESTAO
    if("TPGESTAO" %in% variables_names){

      data[, TPGESTAO := fcase(
        TPGESTAO=="D", "Dupla",
        TPGESTAO=="E", "Estadual",
        TPGESTAO=="M", "Municipal",
        TPGESTAO=="Z", "Sem gest\u00e3o",
        TPGESTAO=="S", "Sem gest\u00e3o"
        )]
      data[, TPGESTAO := factor(TPGESTAO)]

    }

    # PF_PJ
    if("PF_PJ" %in% variables_names){

      data[, PF_PJ := fcase(
        PF_PJ==1, "Pessoa f\u00edsica",
        PF_PJ==3, "Pessoa jur\u00eddica"
      )]
      data[, PF_PJ := factor(PF_PJ)]

    }


    # NIV_DEP
    if("NIV_DEP" %in% variables_names){

      data[, NIV_DEP := fcase(
        NIV_DEP==1, "Individual",
        NIV_DEP==3, "Mantida"
        )]
      data[, NIV_DEP := factor(NIV_DEP)]

    }


    # ESFERA_A
    if ("ESFERA_A" %in% variables_names) {

      data[, ESFERA_A := fcase(
        ESFERA_A == 1, "Federal",
        ESFERA_A == 2, "Estadual",
        ESFERA_A == 3, "Municipal",
        ESFERA_A == 4, "Privada",
        ESFERA_A == -99, as.character(NA)
      )]
      data[, ESFERA_A := factor(ESFERA_A)]

    }

    # ATIVIDAD
    if("ATIVIDAD" %in% variables_names){
      data[, ATIVIDAD := fcase(
        ATIVIDAD==-99, as.character(NA),
        ATIVIDAD==1, "Unidade Universit\u00e1ria",
        ATIVIDAD==2, "Unidade Escola Superior Isolada",
        ATIVIDAD==3, "Unidade Auxiliar de Ensino",
        ATIVIDAD==4, "Unidade sem atividade de Ensino",
        ATIVIDAD==5, "Hospital de ensino"
        )]

      data[, ATIVIDAD := factor(ATIVIDAD)]

    }


    # RETENCAO
    if("RETENCAO" %in% variables_names){
      data[, RETENCAO := fcase(
        RETENCAO==0, as.character(NA),
        RETENCAO==10, "Estabelecimento p\u00fablico",
        RETENCAO==11, "Estabelecimento filantr\u00f3pico",
        RETENCAO==12, "Estabelecimento sem fins lucrativos",
        RETENCAO==13, "Estabelecimento privado luvrativa simples",
        RETENCAO==14, "Estabelecimento privado luvrativa",
        RETENCAO==15, "Estabelecimento sindical",
        RETENCAO==16, "Estabelecimento pessoa f\u00edsica"
        )]
      data[, RETENCAO := factor(RETENCAO)]

    }

    # NATUREZA
    if("NATUREZA" %in% variables_names){
      data[, NATUREZA := fcase(
        NATUREZA==-99, as.character(NA),
        NATUREZA==1, "Administra\u00e7\u00e3o Direta da Sa\u00fade (MS, SES, e SMS)",
        NATUREZA==2, "Adm Direta outros org\u00e3os (MEX, MEx, Marinha,...)",
        NATUREZA==3, "Adm Indireta - Autarquias",
        NATUREZA==4, "Adm Indireta - Funda\u00e7\u00e3o P\u00fablica",
        NATUREZA==5, "Adm Indireta - Empresa P\u00fablica",
        NATUREZA==6, "Adm Indireta - Organiza\u00e7\u00e3o Social P\u00fablica",
        NATUREZA==7, "Empresa Privada",
        NATUREZA==8, "Funda\u00e7\u00e3o Privada",
        NATUREZA==9, "Cooperativa",
        NATUREZA==10, "Servi\u00e7o Social Aut\u00f4nomo",
        NATUREZA==11, "Entidade Beneficente sem fins lucrativos",
        NATUREZA==12, "Economia Mista",
        NATUREZA==13, "Sindicato",
        NATUREZA==0, "Natureza inexistente"
        )]
      data[, NATUREZA := factor(NATUREZA)]

    }



    # CLIENTEL
    if("CLIENTEL" %in% variables_names){
      data[, CLIENTEL := fcase(
        CLIENTEL==-99, as.character(NA),
        CLIENTEL==1, "Atendimento de demanda espont\u00e2nea",
        CLIENTEL==2, "Atendimento de demanda referenciada",
        CLIENTEL==3, "Atendimento de demanda espont\u00e2nea e referenciada",
        CLIENTEL==0, "Fluxo de Clientela n\u00e3o exigido"
        )]
      data[, CLIENTEL := factor(CLIENTEL)]

    }



    # TP_UNID
    if("TP_UNID" %in% variables_names){
      data[, TP_UNID := fcase(
        TP_UNID==1, "Posto de sa\u00fade",
        TP_UNID==2, "Centro de sa\u00fade / Unidade b\u00e1sica",
        TP_UNID==4, "Policl\u00ednica",
        TP_UNID==5, "Hospital geral",
        TP_UNID==7, "Hospital Especializado",
        TP_UNID==9, "Pronto socorro de hospital geral (antigo)",
        TP_UNID==12, "Pronto socorro traumato-ortop\u00e9dico (antigo)",
        TP_UNID==15, "Unidade mista",
        TP_UNID==20, "Pronto socorro geral",
        TP_UNID==21, "Pronto socorro especializado",
        TP_UNID==22, "Consult\u00f3rio isolado",
        TP_UNID==32, "Unidade m\u00f3vel fluvial",
        TP_UNID==36, "Cl\u00ednica / Centro de sa\u00fade de especialidade",
        TP_UNID==39, "Unidade de apoio diagnose e terapia (SADT isolado)",
        TP_UNID==40, "Unidade m\u00f3vel terrestre",
        TP_UNID==42, "Unidade m\u00f3vel de n\u00edvel pr\u00e9-hospitalar na \u00e1rea de urg\u00eancia",
        TP_UNID==43, "Farm\u00e1cia",
        TP_UNID==45, "Unidade de sa\u00fade da fam\u00edlia",
        TP_UNID==50, "Unidade de vigil\u00e2ncia em sa\u00fade",
        TP_UNID==60, "Cooperativa ou empresa de cess\u00e3o de trabalhadores na sa\u00fade",
        TP_UNID==61, "Centro de parto normal - isolado",
        TP_UNID==62, "Hospital / Dia - Isolado",
        TP_UNID==63, "Unidade autorizadora",
        TP_UNID==64, "Central de regula\u00e7\u00e3o de servi\u00e7os de sa\u00fade",
        TP_UNID==65, "Unidade de vigil\u00e2ncia epidemiol\u00f3gica (antigo)",
        TP_UNID==66, "Unidade de vigil\u00e2ncia sanit\u00e1ria (antigo)",
        TP_UNID==67, "Laborat\u00f3rio central de sa\u00fade p\u00fablica LACEN",
        TP_UNID==68, "Central de gest\u00e3o em sa\u00fade",
        TP_UNID==69, "Centro de aten\u00e7\u00e3o hemoterapia e/ou hematologica",
        TP_UNID==70, "Centro de aten\u00e7\u00e3o psicosocial",
        TP_UNID==71, "Centro de apoio a sa\u00fade da fam\u00edlia",
        TP_UNID==72, "Unidade de aten\u00e7\u00e3o a sa\u00fade ind\u00edgena",
        TP_UNID==73, "Pronto atendimento",
        TP_UNID==74, "P\u00f3lo academia da sa\u00fade",
        TP_UNID==75, "Telessa\u00fade",
        TP_UNID==76, "Central de regula\u00e7\u00e3o m\u00e9dica das urg\u00eancias",
        TP_UNID==77, "Servi\u00e7o de aten\u00e7\u00e3o domiciliar isolado (Home care)",
        TP_UNID==78, "Unidade de aten\u00e7\u00e3o em regime residencial",
        TP_UNID==79, "Oficina ortop\u00e9dica",
        TP_UNID==80, "Laborat\u00f3rio de sa\u00fade p\u00fablica",
        TP_UNID==81, "Central de regula\u00e7\u00e3o do acesso",
        TP_UNID==82, "Central de notifica\u00e7\u00e3o, capta\u00e7\u00e3o e distribui\u00e7\u00e3o de \u00f3rg\u00e3os estadual",
        TP_UNID==83, "P\u00f3lo de preven\u00e7\u00e3o de doen\u00e7as e agravos e promo\u00e7\u00e3o da sa\u00fade"
        )]
      data[, TP_UNID := factor(TP_UNID)]

    }

    # TURNO_AT
    if("TURNO_AT" %in% variables_names){
      data[, TURNO_AT := fcase(
        TURNO_AT==-99, as.character(NA),
        TURNO_AT==1, "Turnos intermitentes",
        TURNO_AT==2, "Cont\u00ednuo 24h/dia (Pl Sab Dom Fer)",
        TURNO_AT==3, "Manh\u00e3 / Tarde / Noite",
        TURNO_AT==4, "Manh\u00e3",
        TURNO_AT==5, "Tarde",
        TURNO_AT==6, "Manh\u00e3 / Tarde",
        TURNO_AT==7, "Noite"
        )]
      data[, TURNO_AT := factor(TURNO_AT)]

    }


    # NIV_HIER
    if("NIV_HIER" %in% variables_names){
      data[, NIV_HIER := fcase(
        NIV_HIER==0, as.character(NA),
        NIV_HIER==99, as.character(NA),
        NIV_HIER==1, "PAB-PABA",
        NIV_HIER==2, "M\u00e9dia M1",
        NIV_HIER==3, "M\u00e9dia M2 e M3",
        NIV_HIER==4, "Alta complexidade ambulatorial",
        NIV_HIER==5, "Baixa M1 e M2",
        NIV_HIER==6, "M\u00e9dia M2 e M3",
        NIV_HIER==7, "M\u00e9dia M3",
        NIV_HIER==8, "Alta complexidade hospitalar / ambulatorial"
        )]
      data[, NIV_HIER := factor(NIV_HIER)]

    }

    # TERCEIRO
    if("TERCEIRO" %in% variables_names){
      data[, TERCEIRO := fcase(
        TERCEIRO==1, "Sim",
        TERCEIRO==0, "N\u00e3o",
        TERCEIRO==2, "N\u00e3o"
        )]
      data[, TERCEIRO := factor(TERCEIRO)]

    }

    # CPF_PROF
    if("CPF_PROF" %in% variables_names){
      data[, CPF_PROF := fcase(
        CPF_PROF=="99999999999", as.character(NA),
        CPF_PROF=="00000000000000", as.character(NA)
        )]
      data[, CPF_PROF := factor(CPF_PROF)]

    }

    # CBO
    if ("CBO" %in% variables_names) {
      data <- dplyr::left_join(data, microdatasus::tabCBO, by = c("CBO" = "cod"))
      setDT(data)
    }



    # VINCULAC
    if("VINCULAC" %in% variables_names){
      data[, VINCULAC := fcase(
        VINCULAC==1, "Profissional CONTRATADO",
        VINCULAC==2, "Profissional AUT\u00d4NOMO",
        VINCULAC==3, "Profissional V\u00cdNCULO N\u00c3O IDENTIFICADO"
        )]
      data[, VINCULAC := factor(VINCULAC)]

    }

    # NAT_JUR
    if("NAT_JUR" %in% variables_names){
      data[, NAT_JUR := fcase(
        NAT_JUR==0, as.character(NA),
        NAT_JUR==1000, "Administra\u00e7\u00e3o P\u00fablica",
        NAT_JUR==1015, "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Federal",
        NAT_JUR==1023, "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Estadual ou do Distrito Federal",
        NAT_JUR==1031, "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Municipal",
        NAT_JUR==1040, "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Federal",
        NAT_JUR==1058, "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Estadual ou do Distrito Federal",
        NAT_JUR==1066, "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Municipal",
        NAT_JUR==1074, "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Federal",
        NAT_JUR==1082, "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Estadual",
        NAT_JUR==1104, "Autarquia Federal",
        NAT_JUR==1112, "Autarquia Estadual ou do Distrito Federal",
        NAT_JUR==1120, "Autarquia Municipal",
        NAT_JUR==1139, "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Federal",
        NAT_JUR==1147, "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Estadual ou do Distrito Federal",
        NAT_JUR==1155, "Funda\u00e7\u00e3o P\u00fablica de Direito P\u00fablico Municipal",
        NAT_JUR==1163, "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Federal",
        NAT_JUR==1171, "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Estadual ou do Distrito Federal",
        NAT_JUR==1180, "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Municipal",
        NAT_JUR==1198, "Comiss\u00e3o Polinacional",
        NAT_JUR==1201, "Fundo P\u00fablico",
        NAT_JUR==1210, "Cons\u00f3rcio P\u00fablico de Direito P\u00fablico (Associa\u00e7\u00e3o P\u00fablica)",
        NAT_JUR==1228, "Cons\u00f3rcio P\u00fablico de Direito Privado",
        NAT_JUR==1236, "Estado ou Distrito Federal",
        NAT_JUR==1244, "Munic\u00edpio",
        NAT_JUR==1252, "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Federal",
        NAT_JUR==1260, "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Estadual ou do Distrito Federal",
        NAT_JUR==1279, "Funda\u00e7\u00e3o P\u00fablica de Direito Privado Municipal",
        NAT_JUR==2000, "Entidades Empresariais",
        NAT_JUR==2001, "Empresa P\u00fablica",
        NAT_JUR==2038, "Sociedade de Economia Mista",
        NAT_JUR==2046, "Sociedade An\u00f4nima Aberta",
        NAT_JUR==2054, "Sociedade An\u00f4nima Fechada",
        NAT_JUR==2062, "Sociedade Empres\u00e1ria Limitada",
        NAT_JUR==2070, "Sociedade Empres\u00e1ria em Nome Coletivo",
        NAT_JUR==2089, "Sociedade Empres\u00e1ria em Comandita Simples",
        NAT_JUR==2097, "Sociedade Empres\u00e1ria em Comandita por A\u00e7\u00f5es",
        NAT_JUR==2127, "Sociedade em Conta de Participa\u00e7\u00e3o",
        NAT_JUR==2135, "Empres\u00e1rio (Individual)",
        NAT_JUR==2143, "Cooperativa",
        NAT_JUR==2151, "Cons\u00f3rcio de Sociedades",
        NAT_JUR==2160, "Grupo de Sociedades",
        NAT_JUR==2178, "Estabelecimento, no Brasil, de Sociedade Estrangeira",
        NAT_JUR==2194, "Estabelecimento, no Brasil, de Empresa Binacional Argentino-Brasileira",
        NAT_JUR==2216, "Empresa Domiciliada no Exterior",
        NAT_JUR==2224, "Clube/Fundo de Investimento",
        NAT_JUR==2232, "Sociedade Simples Pura",
        NAT_JUR==2240, "Sociedade Simples Limitada",
        NAT_JUR==2259, "Sociedade Simples em Nome Coletivo",
        NAT_JUR==2267, "Sociedade Simples em Comandita Simples",
        NAT_JUR==2275, "Empresa Binacional",
        NAT_JUR==2283, "Cons\u00f3rcio de Empregadores",
        NAT_JUR==2291, "Cons\u00f3rcio Simples",
        NAT_JUR==2305, "Empresa Individual de Responsabilidade Limitada (de Natureza Empres\u00e1ria)",
        NAT_JUR==2313, "Empresa Individual de Responsabilidade Limitada (de Natureza Simples)",
        NAT_JUR==2321, "Sociedade Unipessoal de Advogados",
        NAT_JUR==2330, "Cooperativas de Consumo",
        NAT_JUR==3000, "Entidades sem Fins Lucrativos",
        NAT_JUR==3034, "Servi\u00e7o Notarial e Registral (Cart\u00f3rio)",
        NAT_JUR==3069, "Funda\u00e7\u00e3o Privada",
        NAT_JUR==3077, "Servi\u00e7o Social Aut\u00f4nomo",
        NAT_JUR==3085, "Condom\u00ednio Edil\u00edcio",
        NAT_JUR==3107, "Comiss\u00e3o de Concilia\u00e7\u00e3o Pr\u00e9via",
        NAT_JUR==3115, "Entidade de Media\u00e7\u00e3o e Arbitragem",
        NAT_JUR==3131, "Entidade Sindical",
        NAT_JUR==3204, "Estabelecimento, no Brasil, de Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Estrangeiras",
        NAT_JUR==3212, "Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Domiciliada no Exterior",
        NAT_JUR==3220, "Organiza\u00e7\u00e3o Religiosa",
        NAT_JUR==3239, "Comunidade Ind\u00edgena",
        NAT_JUR==3247, "Fundo Privado",
        NAT_JUR==3255, "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Nacional de Partido Pol\u00edtico",
        NAT_JUR==3263, "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Regional de Partido Pol\u00edtico",
        NAT_JUR==3271, "\u00d3rg\u00e3o de Dire\u00e7\u00e3o Local de Partido Pol\u00edtico",
        NAT_JUR==3280, "Comit\u00ea Financeiro de Partido Pol\u00edtico",
        NAT_JUR==3298, "Frente Plebiscit\u00e1ria ou Referend\u00e1ria",
        NAT_JUR==3306, "Organiza\u00e7\u00e3o Social (OS)",
        NAT_JUR==3310, "Demais Condom\u00ednios",
        NAT_JUR==3999, "Associa\u00e7\u00e3o Privada",
        NAT_JUR==4000, "Pessoas F\u00edsicas",
        NAT_JUR==4014, "Empresa Individual Imobili\u00e1ria",
        NAT_JUR==4022, "Segurado Especial",
        NAT_JUR==4081, "Contribuinte individual",
        NAT_JUR==4090, "Candidato a Cargo Pol\u00edtico Eletivo",
        NAT_JUR==4111, "Leiloeiro",
        NAT_JUR==4124, "Produtor Rural (Pessoa F\u00edsica)",
        NAT_JUR==5000, "Organiza\u00e7\u00f5es Internacionais e Outras Institui\u00e7\u00f5es Extraterritoriais",
        NAT_JUR==5010, "Organiza\u00e7\u00e3o Internacional",
        NAT_JUR==5029, "Representa\u00e7\u00e3o Diplom\u00e1tica Estrangeira",
        NAT_JUR==5037, "Outras Institui\u00e7\u00f5es Extraterritoriais"
        )]
      data[, NAT_JUR := factor(NAT_JUR)]

    }
  }


  # Purge levels
  data <- droplevels(data)

  # Unescape unicode characters
  data <- as.data.frame(lapply(X = data, FUN = stringi::stri_unescape_unicode))

  # Return
  return(data)
}
