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
#' @examples
#' process_sih(sih_rd_sample)
#'
#' @return a \code{data.frame} with the processed data.
#'
#' @export

process_sih <- function(data, information_system = "SIH-RD", municipality_data = TRUE) {
  # Check information system
  available_information_system <- "SIH-RD"
  if(!(information_system %in% available_information_system)) stop("Health informaton system unknown.")

  # Variables names
  variables_names <- names(data)

  # Use dtplyr
  data <- dtplyr::lazy_dt(data)

  if(information_system == "SIH-RD"){

    # ANO_CMPT
    if("ANO_CMPT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(ANO_CMPT = as.numeric(.data$ANO_CMPT))
    }

    # MES_CMPT
    if("MES_CMPT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(MES_CMPT = as.numeric(.data$MES_CMPT))
    }

    # ESPEC
    if("ESPEC" %in% variables_names){
      data <- data %>%
        dplyr::mutate(ESPEC = as.character(.data$ESPEC)) %>%
        dplyr::mutate(ESPEC = dplyr::case_match(
          .data$ESPEC,
          "1" ~ "Cir\u00fargico",
          "2" ~ "Obst\u00e9tricos",
          "3" ~ "Cl\u00ednicos",
          "4" ~ "Cr\u00f4nicos",
          "5" ~ "Psiquiatria",
          "6" ~ "Pneumologia sanit\u00e1ria (tsiologia)",
          "7" ~ "Pedi\u00e1tricos",
          "8" ~ "Reabilita\u00e7\u00e3o",
          "9" ~ "Leito Dia / Cir\u00fargicos",
          "10" ~ "Leito Dia / Aids",
          "11" ~ "Leito Dia / Fibrose C\u00edstica",
          "12" ~ "Leito Dia / Intercorr\u00eancia P\u00f3s-Transplante",
          "13" ~ "Leito Dia / Geriatria",
          "14" ~ "Leito Dia / Sa\u00fade Mental",
          "51" ~ "UTI II Adulto COVID 19",
          "52" ~ "UTI II Pedi\u00e1trica COVID 19",
          "64" ~ "Unidade Intermedi\u00e1ria",
          "65" ~ "Unidade Intermedi\u00e1ria Neonatal",
          "74" ~ "UTI I",
          "75" ~ "UTI Adulto II",
          "76" ~ "UTI Adulto III",
          "77" ~ "UTI Infantil I",
          "78" ~ "UTI Infantil II",
          "79" ~ "UTI Infantil III",
          "80" ~ "UTI Neonatal I",
          "81" ~ "UTI Neonatal II",
          "82" ~ "UTI Neonatal III",
          "83" ~ "UTI Queimados",
          "84" ~ "Acolhimento Noturno",
          "85" ~ "UTI Coronariana-UCO tipo II",
          "86" ~ "UTI Coronariana-UCO tipo III",
          "87" ~ "Sa\u00fade Mental (Cl\u00ednico)",
          "88" ~ "Queimado Adulto (Cl\u00ednico)",
          "89" ~ "Queimado Pedi\u00e1trico (Cl\u00ednico)",
          "90" ~ "Queimado Adulto (Cir\u00fargico)",
          "91" ~ "Queimado Pedi\u00e1trico (Cir\u00fargico)",
          "92" ~ "UCI Unidade de Cuidados Intermediarios Neonatal Convencional",
          "93" ~ "UCI Unidade de Cuidados Intermediarios Neonatal Canguru",
          "94" ~ "UCI Unidade de Cuidados Intermediarios Pediatrico",
          "95" ~ "UCI Unidade de Cuidados Intermediarios Adulto",
          "96" ~ "Suporte Ventilat\u00f3rio Pulmonar COVID-19",
          .default = .data$ESPEC
        )) %>%
        dplyr::mutate(ESPEC = as.factor(.data$ESPEC))
    }

    # IDENT
    if("IDENT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(IDENT = as.character(.data$IDENT)) %>%
        dplyr::mutate(IDENT = dplyr::case_match(
          .data$IDENT,
          "1" ~ "Principal",
          "3" ~ "Continua\u00e7\u00e3o",
          "5" ~ "Longa perman\u00eancia",
          .default = .data$IDENT
        )) %>%
        dplyr::mutate(IDENT = as.factor(.data$IDENT))
    }

    # MUNIC_RES
    if("MUNIC_RES" %in% variables_names & municipality_data == TRUE){
      colnames(tabMun)[1] <- "MUNIC_RES"
      data <- data %>%
        dplyr::mutate(MUNIC_RES = as.numeric(.data$MUNIC_RES)) %>%
        dplyr::left_join(tabMun, by = "MUNIC_RES")
    }

    # NASC
    if("NASC" %in% variables_names){
      data <- data %>%
        dplyr::mutate(NASC = as.Date(.data$NASC, format = "%Y%m%d"))
    }

    # SEXO
    if("SEXO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(SEXO = as.character(.data$SEXO)) %>%
        dplyr::mutate(SEXO = dplyr::case_match(
          .data$SEXO,
          "1" ~ "Masculino",
          "2" ~ "Feminino",
          "3" ~ "Feminino",
          "0" ~ NA,
          "9" ~ NA,
          .default = .data$SEXO
        )) %>%
        dplyr::mutate(SEXO = as.factor(.data$SEXO))
    }

    # UTI_MES_IN
    if("UTI_MES_IN" %in% variables_names){
      data <- data %>%
        dplyr::mutate(UTI_MES_IN = as.numeric(.data$UTI_MES_IN))
    }

    # UTI_MES_AN
    if("UTI_MES_AN" %in% variables_names){
      data <- data %>%
        dplyr::mutate(UTI_MES_AN = as.numeric(.data$UTI_MES_AN))
    }

    # UTI_MES_AL
    if("UTI_MES_AL" %in% variables_names){
      data <- data %>%
        dplyr::mutate(UTI_MES_AL = as.numeric(.data$UTI_MES_AL))
    }

    # UTI_MES_TO
    if("UTI_MES_TO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(UTI_MES_TO = as.numeric(.data$UTI_MES_TO))
    }

    # MARCA_UTI
    if("MARCA_UTI" %in% variables_names){
      data <- data %>%
        dplyr::mutate(MARCA_UTI = as.character(.data$MARCA_UTI)) %>%
        dplyr::mutate(MARCA_UTI = dplyr::case_match(
          .data$MARCA_UTI,
          "0" ~ "N\u00e3o utilizou UTI",
          "51" ~ "UTI adulto - tipo II COVID 19",
          "52" ~ "UTI pedi\u00e1trica - tipo II COVID 19",
          "74" ~ "UTI adulto - tipo I",
          "75" ~ "UTI adulto - tipo II",
          "76" ~ "UTI adulto - tipo III",
          "77" ~ "UTI infantil - tipo I",
          "78" ~ "UTI infantil - tipo II",
          "79" ~ "UTI infantil - tipo III",
          "80" ~ "UTI neonatal - tipo I",
          "81" ~ "UTI neonatal - tipo II",
          "82" ~ "UTI neonatal - tipo III",
          "83" ~ "UTI de queimados",
          "85" ~ "UTI coronariana tipo II - UCO tipo II",
          "86" ~ "UTI coronariana tipo III - UCO tipo III",
          "99" ~ "UTI Doador",
          "1" ~ "Utilizou mais de um tipo de UTI",
          .default = .data$MARCA_UTI
        )) %>%
        dplyr::mutate(MARCA_UTI = as.factor(.data$MARCA_UTI))
    }

    # UTI_INT_IN
    if("UTI_INT_IN" %in% variables_names){
      data <- data %>%
        dplyr::mutate(UTI_INT_IN = as.numeric(.data$UTI_INT_IN))
    }

    # UTI_INT_AN
    if("UTI_INT_AN" %in% variables_names){
      data <- data %>%
        dplyr::mutate(UTI_INT_AN = as.numeric(.data$UTI_INT_AN))
    }

    # UTI_INT_AL
    if("UTI_INT_AL" %in% variables_names){
      data <- data %>%
        dplyr::mutate(UTI_INT_AL = as.numeric(.data$UTI_INT_AL))
    }

    # UTI_INT_TO
    if("UTI_INT_TO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(UTI_INT_TO = as.numeric(.data$UTI_INT_TO))
    }

    # DIAR_ACOM
    if("DIAR_ACOM" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DIAR_ACOM = as.numeric(.data$DIAR_ACOM))
    }

    # QT_DIARIAS
    if("QT_DIARIAS" %in% variables_names){
      data <- data %>%
        dplyr::mutate(QT_DIARIAS = as.numeric(.data$QT_DIARIAS))
    }

    # VAL_SH
    if("VAL_SH" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_SH = as.numeric(.data$VAL_SH))
    }

    # VAL_SP
    if("VAL_SP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_SP = as.numeric(.data$VAL_SP))
    }

    # VAL_SADT
    if("VAL_SADT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_SADT = as.numeric(.data$VAL_SADT))
    }

    # VAL_RN
    if("VAL_RN" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_RN = as.numeric(.data$VAL_RN))
    }

    # VAL_ACOMP
    if("VAL_ACOMP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_ACOMP = as.numeric(.data$VAL_ACOMP))
    }

    # VAL_ORTP
    if("VAL_ORTP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_ORTP = as.numeric(.data$VAL_ORTP))
    }

    # VAL_SANGUE
    if("VAL_SANGUE" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_SANGUE = as.numeric(.data$VAL_SANGUE))
    }

    # VAL_SADTSR
    if("VAL_SADTSR" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_SADTSR = as.numeric(.data$VAL_SADTSR))
    }

    # VAL_TRANSP
    if("VAL_TRANSP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_TRANSP = as.numeric(.data$VAL_TRANSP))
    }

    # VAL_OBSANG
    if("VAL_OBSANG" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_OBSANG = as.numeric(.data$VAL_OBSANG))
    }

    # VAL_PED1AC
    if("VAL_PED1AC" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_PED1AC = as.numeric(.data$VAL_PED1AC))
    }

    # VAL_TOT
    if("VAL_TOT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_TOT = as.numeric(.data$VAL_TOT))
    }

    # VAL_UTI
    if("VAL_UTI" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_UTI = as.numeric(.data$VAL_UTI))
    }

    # US_TOT
    if("US_TOT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(US_TOT = as.numeric(.data$US_TOT))
    }

    # DT_INTER
    if("DT_INTER" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DT_INTER = as.Date(.data$DT_INTER, format = "%Y%m%d"))
    }

    # DT_SAIDA
    if("DT_SAIDA" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DT_SAIDA = as.Date(.data$DT_SAIDA, format = "%Y%m%d"))
    }

    # COBRANCA (motivo de saída/permanência, portaria SAS 719)
    if("COBRANCA" %in% variables_names){
      data <- data %>%
        dplyr::mutate(COBRANCA = as.character(.data$COBRANCA)) %>%
        dplyr::mutate(COBRANCA = dplyr::case_match(
          .data$COBRANCA,
          "11" ~ "Alta curado",
          "12" ~ "Alta melhorado",
          "14" ~ "Alta a pedido",
          "15" ~ "Alta com previs\u00e3o de retorno p/acomp do paciente",
          "16" ~ "Alta por evas\u00e3o",
          "18" ~ "Alta por outros motivos",
          "19" ~ "Alta de paciente agudo em psiquiatria",
          "21" ~ "Perman\u00eancia por caracter\u00edsticas pr\u00f3prias da doen\u00e7a",
          "22" ~ "Perman\u00eancia por intercorr\u00eancia",
          "23" ~ "Perman\u00eancia por impossibilidade s\u00f3cio-familiar",
          "24" ~ "Perman\u00eancia proc doa\u00e7\u00e3o \u00f3rg, tec, c\u00e9l-doador vivo",
          "25" ~ "Perman\u00eancia proc doa\u00e7\u00e3o \u00f3rg, tec, c\u00e9l-doador morto",
          "26" ~ "Perman\u00eancia por mudan\u00e7a de procedimento",
          "27" ~ "Perman\u00eancia por reopera\u00e7\u00e3o",
          "28" ~ "Perman\u00eancia por outros motivos",
          "29" ~ "Transfer\u00eancia para interna\u00e7\u00e3o domiciliar",
          "32" ~ "Transfer\u00eancia para interna\u00e7\u00e3o domiciliar",
          "31" ~ "Transfer\u00eancia para outro estabelecimento",
          "41" ~ "\u00d3bito com DO fornecida pelo m\u00e9dico assistente",
          "42" ~ "\u00d3bito com DO fornecida pelo IML",
          "43" ~ "\u00d3bito com DO fornecida pelo SVO",
          "51" ~ "Encerramento administrativo",
          "61" ~ "Alta da m\u00e3e/pu\u00e9rpera e do rec\u00e9m-nascido",
          "17" ~ "Alta da m\u00e3e/pu\u00e9rpera e do rec\u00e9m-nascido",
          "62" ~ "Alta da m\u00e3e/pu\u00e9rpera e perman\u00eancia rec\u00e9m-nascido",
          "13" ~ "Alta da m\u00e3e/pu\u00e9rpera e perman\u00eancia rec\u00e9m-nascido",
          "63" ~ "Alta da m\u00e3e/pu\u00e9rpera e \u00f3bito do rec\u00e9m-nascido",
          "64" ~ "Alta da m\u00e3e/pu\u00e9rpera com \u00f3bito fetal",
          "65" ~ "\u00d3bito da gestante e do concepto",
          "66" ~ "\u00d3bito da m\u00e3e/pu\u00e9rpera e alta do rec\u00e9m-nascido",
          "67" ~ "\u00d3bito da m\u00e3e/pu\u00e9rpera e perman\u00eancia rec\u00e9m-nascido",
          .default = .data$COBRANCA
        )) %>%
        dplyr::mutate(COBRANCA = as.factor(.data$COBRANCA))
    }

    # NATUREZA
    if("NATUREZA" %in% variables_names){
      data <- data %>%
        dplyr::mutate(NATUREZA = as.character(.data$NATUREZA)) %>%
        dplyr::mutate(NATUREZA = dplyr::case_match(
          .data$NATUREZA,
         "0" ~ NA,
         "99" ~ NA,
         "10" ~ "Pr\u00f3prio",
         "20" ~ "Contratado",
         "22" ~ "Contratado optante SIMPLES",
         "30" ~ "Federal",
         "31" ~ "Federal Verba Pr\u00f3pria",
         "40" ~ "Estadual",
         "41" ~ "Estadual Verba Pr\u00f3pria",
         "50" ~ "Municipal",
         "60" ~ "Filantr\u00f3pico",
         "61" ~ "Filantr\u00f3pico isento tributos e contr.sociais",
         "63" ~ "Filantr\u00f3pico isento IR e contr.s/lucro l\u00edquido",
         "70" ~ "Universit\u00e1rio Ensino",
         "80" ~ "Sindicato",
         "90" ~ "Universit\u00e1rio Pesquisas",
         "91" ~ "Univ. Pesquisas isento tributos e contr.sociais",
         "93" ~ "Univ. Pesquisas isento IR e contr.s/lucro l\u00edquido",
         "94" ~ "Universit\u00e1rio de ensino e pesquisa privado",
         "92" ~ "Universit\u00e1rio de ensino e pesquisa privado",
          .default = .data$NATUREZA
        )) %>%
        dplyr::mutate(NATUREZA = as.factor(.data$NATUREZA))
    }

    # NAT_JUR
    if("NAT_JUR" %in% variables_names){
      data <- data %>%
        dplyr::mutate(NAT_JUR = as.character(.data$NAT_JUR)) %>%
        dplyr::mutate(NAT_JUR = dplyr::case_match(
          .data$NAT_JUR,
          "1015" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Federal",
          "1023" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Exec Estadual ou Distr Fed",
          "1031" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Executivo Municipal",
          "1040" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Federal",
          "1058" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Legisl Estadual ou Dist Fed",
          "1066" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Legislativo Municipal",
          "1074" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Federal",
          "1082" ~ "\u00d3rg\u00e3o P\u00fablico do Poder Judici\u00e1rio Estadual",
          "1104" ~ "Autarquia Federal",
          "1112" ~ "Autarquia Estadual ou do Distrito Federal",
          "1120" ~ "Autarquia Municipal",
          "1139" ~ "Funda\u00e7\u00e3o Federal",
          "1147" ~ "Funda\u00e7\u00e3o Estadual ou do Distrito Federal",
          "1155" ~ "Funda\u00e7\u00e3o Municipal",
          "1163" ~ "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Federal",
          "1171" ~ "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Estadual ou Distr Federal",
          "1180" ~ "\u00d3rg\u00e3o P\u00fablico Aut\u00f4nomo Estadual ou Distr Federal",
          "1198" ~ "Comiss\u00e3o Polinacional",
          "1201" ~ "Fundo P\u00fablico",
          "1210" ~ "Associa\u00e7\u00e3o P\u00fablica",
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
          "2178" ~ "Estabelecimento no Brasil de Sociedade Estrangeira",
          "2194" ~ "Estab no Brasil Empr Binacional Argentina-Brasil",
          "2216" ~ "Empresa Domiciliada no Exterior",
          "2224" ~ "Clube/Fundo de Investimento",
          "2232" ~ "Sociedade Simples Pura",
          "2240" ~ "Sociedade Simples Limitada",
          "2259" ~ "Sociedade Simples em Nome Coletivo",
          "2267" ~ "Sociedade Simples em Comandita Simples",
          "2275" ~ "Empresa Binacional",
          "2283" ~ "Cons\u00f3rcio de Empregadores",
          "2291" ~ "Cons\u00f3rcio Simples",
          "2305" ~ "Empr Individ Responsab Limitada (Natur Empres\u00e1ria)",
          "2313" ~ "Empr Individ Responsab Limitada (Natureza Simples)",
          "3034" ~ "Servi\u00e7o Notarial e Registral (Cart\u00f3rio)",
          "3069" ~ "Funda\u00e7\u00e3o Privada",
          "3077" ~ "Servi\u00e7o Social Aut\u00f4nomo",
          "3085" ~ "Condom\u00ednio Edil\u00edcio",
          "3107" ~ "Comiss\u00e3o de Concilia\u00e7\u00e3o Pr\u00e9via",
          "3115" ~ "Entidade de Media\u00e7\u00e3o e Arbitragem",
          "3123" ~ "Partido Pol\u00edtico",
          "3131" ~ "Entidade Sindical",
          "3204" ~ "Estab no Brasil de Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Estrang",
          "3212" ~ "Funda\u00e7\u00e3o ou Associa\u00e7\u00e3o Domiciliada no Exterior",
          "3220" ~ "Organiza\u00e7\u00e3o Religiosa",
          "3239" ~ "Comunidade Ind\u00edgena",
          "3247" ~ "Fundo Privado",
          "3999" ~ "Associa\u00e7\u00e3o Privada",
          "4014" ~ "Empresa Individual Imobili\u00e1ria",
          "4022" ~ "Segurado Especial",
          "4081" ~ "Contribuinte Individual",
          "4090" ~ "Candidato a Cargo Pol\u00edtico Eletivo",
          "4111" ~ "Leiloeiro",
          "5010" ~ "Organiza\u00e7\u00e3o Internacional",
          "5029" ~ "Representa\u00e7\u00e3o Diplom\u00e1tica Estrangeira",
          "5037" ~ "Outras Institui\u00e7\u00f5es Extraterritoriais",
          "0" ~ NA,
          .default = .data$NAT_JUR
        )) %>%
        dplyr::mutate(NAT_JUR = as.factor(.data$NAT_JUR))
    }

    # GESTAO
    if("GESTAO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(GESTAO = as.character(.data$GESTAO)) %>%
        dplyr::mutate(GESTAO = dplyr::case_match(
          .data$GESTAO,
          "0" ~ "Estadual",
          "2" ~ "Estadual plena",
          "1" ~ "Municipal plena assist",
          "3" ~ NA,
          "9" ~ NA,
          .default = .data$GESTAO
        )) %>%
        dplyr::mutate(GESTAO = as.factor(.data$GESTAO))
    }

    # RUBRICA
    if("RUBRICA" %in% variables_names){
      data <- data %>%
        dplyr::mutate(RUBRICA = as.numeric(.data$RUBRICA))
    }

    # IND_VDRL
    if("IND_VDRL" %in% variables_names){
      data <- data %>%
        dplyr::mutate(IND_VDRL = as.character(.data$IND_VDRL)) %>%
        dplyr::mutate(IND_VDRL = dplyr::case_match(
          .data$IND_VDRL,
          "0" ~ "N\u00e3o",
          "1" ~ "Sim",
          .default = .data$IND_VDRL
        )) %>%
        dplyr::mutate(IND_VDRL = as.factor(.data$IND_VDRL))
    }

    # MUNIC_MOV
    if("MUNIC_MOV" %in% variables_names){
      data$MUNIC_MOV <- as.numeric(data$MUNIC_MOV)
    }

    # COD_IDADE
    if("COD_IDADE" %in% variables_names){
      data <- data %>%
        dplyr::mutate(COD_IDADE = as.character(.data$COD_IDADE)) %>%
        dplyr::mutate(COD_IDADE = dplyr::case_match(
          .data$COD_IDADE,
         "0" ~ NA,
         "2" ~ "Dias",
         "3" ~ "Meses",
         "4" ~ "Anos",
         "5" ~ "Centena de anos (100 + idade)",
          .default = .data$COD_IDADE
        )) %>%
        dplyr::mutate(COD_IDADE = as.factor(.data$COD_IDADE))
    }

    # IDADE
    if("IDADE" %in% variables_names){
      data <- data %>%
        dplyr::mutate(IDADE = as.numeric(.data$IDADE))
    }

    # DIAS_PERM
    if("DIAS_PERM" %in% variables_names){
      data <- data %>%
        dplyr::mutate(DIAS_PERM = as.numeric(.data$DIAS_PERM))
    }

    # MORTE
    if("MORTE" %in% variables_names){
      data <- data %>%
        dplyr::mutate(MORTE = as.character(.data$MORTE)) %>%
        dplyr::mutate(MORTE = dplyr::case_match(
          .data$MORTE,
          "0" ~ "N\u00e3o",
          "1" ~ "Sim",
          .default = .data$MORTE
        )) %>%
        dplyr::mutate(MORTE = as.factor(.data$MORTE))
    }

    # NACIONAL
    if("NACIONAL" %in% variables_names){
      data <- data %>%
        dplyr::mutate(NACIONAL = as.character(.data$NACIONAL)) %>%
        dplyr::mutate(NACIONAL = dplyr::case_match(
          .data$NACIONAL,
          "170" ~ "Abissinia",
          "171" ~ "Acores",
          "172" ~ "Afar frances",
          "241" ~ "Afeganistao",
          "93" ~ "Albania",
          "30" ~ "Alemanha",
          "174" ~ "Alto volta",
          "94" ~ "Andorra",
          "175" ~ "Angola",
          "334" ~ "Antartica francesa",
          "337" ~ "Antartico argentino",
          "333" ~ "Antartico britanico, territorio",
          "336" ~ "Antartico chileno",
          "338" ~ "Antartico noruegues",
          "28" ~ "Antigua e. dep. barbuda",
          "29" ~ "Antilhas holandesas",
          "339" ~ "Apatrida",
          "242" ~ "Arabia saudita",
          "176" ~ "Argelia",
          "21" ~ "Argentina",
          "347" ~ "Armenia",
          "289" ~ "Arquipelago de bismark",
          "175" ~ "Angola",
          "285" ~ "Arquipelago manahiki",
          "286" ~ "Arquipelago midway",
          "33" ~ "Aruba",
          "175" ~ "Angola",
          "198" ~ "Ascensao e tristao da cunha,is",
          "287" ~ "Ashmore e cartier",
          "288" ~ "Australia",
          "95" ~ "Austria",
          "138" ~ "Azerbaijao",
          "243" ~ "Bahrein",
          "342" ~ "Bangladesh",
          "44" ~ "Barbados",
          "139" ~ "Bashkista",
          "177" ~ "Bechuanalandia",
          "31" ~ "Belgica",
          "46" ~ "Belize",
          "178" ~ "Benin",
          "83" ~ "Bermudas",
          "246" ~ "Bhutan",
          "244" ~ "Birmania",
          "22" ~ "Bolivia",
          "134" ~ "Bosnia herzegovina",
          "179" ~ "Botsuana",
          "10" ~ "Brasil",
          "245" ~ "Brunei",
          "96" ~ "Bulgaria",
          "238" ~ "Burkina fasso",
          "180" ~ "Burundi",
          "141" ~ "Buryat",
          "343" ~ "Cabo verde",
          "181" ~ "Camaroes",
          "34" ~ "Canada",
          "142" ~ "Carelia",
          "247" ~ "Catar",
          "143" ~ "Cazaquistao",
          "248" ~ "Ceilao",
          "182" ~ "Ceuta e melilla",
          "183" ~ "Chade",
          "144" ~ "Chechen ingusth",
          "23" ~ "Chile",
          "42" ~ "China",
          "249" ~ "China (taiwan)",
          "97" ~ "Chipre",
          "145" ~ "Chuvash",
          "275" ~ "Cingapura",
          "26" ~ "Colombia",
          "40" ~ "Comunidade das bahamas",
          "54" ~ "Comunidade dominicana",
          "185" ~ "Congo",
          "43" ~ "Coreia",
          "186" ~ "Costa do marfim",
          "51" ~ "Costa rica",
          "250" ~ "Coveite",
          "130" ~ "Croacia",
          "52" ~ "Cuba",
          "53" ~ "Curacao",
          "146" ~ "Dagesta",
          "187" ~ "Daome",
          "340" ~ "Dependencia de ross",
          "98" ~ "Dinamarca",
          "188" ~ "Djibuti",
          "99" ~ "Eire",
          "251" ~ "Emirados arabes unidos",
          "27" ~ "Equador",
          "100" ~ "Escocia",
          "136" ~ "Eslovaquia",
          "132" ~ "Eslovenia",
          "35" ~ "Espanha",
          "129" ~ "Estado da cidade do vaticano",
          "57" ~ "Estados assoc. das antilhas",
          "36" ~ "Estados unidos da america (eua)",
          "147" ~ "Estonia",
          "190" ~ "Etiopia",
          "252" ~ "Filipinas",
          "102" ~ "Finlandia",
          "37" ~ "Franca",
          "192" ~ "Gambia",
          "193" ~ "Gana",
          "194" ~ "Gaza",
          "148" ~ "Georgia",
          "103" ~ "Gibraltar",
          "149" ~ "Gorno altai",
          "32" ~ "Gra-bretanha",
          "59" ~ "Granada",
          "104" ~ "Grecia",
          "84" ~ "Groenlandia",
          "292" ~ "Guam",
          "61" ~ "Guatemala",
          "87" ~ "Guiana francesa",
          "195" ~ "Guine",
          "344" ~ "Guine bissau",
          "196" ~ "Guine equatorial",
          "105" ~ "Holanda",
          "64" ~ "Honduras",
          "63" ~ "Honduras britanicas",
          "253" ~ "Hong-kong",
          "106" ~ "Hungria",
          "254" ~ "Iemen",
          "345" ~ "Iemen do sul",
          "197" ~ "Ifni",
          "300" ~ "Ilha johnston e sand",
          "69" ~ "Ilha milhos",
          "293" ~ "Ilhas baker",
          "107" ~ "Ilhas baleares",
          "199" ~ "Ilhas canarias",
          "294" ~ "Ilhas cantao e enderburg",
          "295" ~ "Ilhas carolinas",
          "297" ~ "Ilhas christmas",
          "184" ~ "Ilhas comores",
          "290" ~ "Ilhas cook",
          "108" ~ "Ilhas cosmoledo (lomores)",
          "117" ~ "Ilhas de man",
          "109" ~ "Ilhas do canal",
          "296" ~ "Ilhas do pacifico",
          "58" ~ "Ilhas falklands",
          "101" ~ "Ilhas faroes",
          "298" ~ "Ilhas gilbert",
          "60" ~ "Ilhas guadalupe",
          "299" ~ "Ilhas howland e jarvis",
          "301" ~ "Ilhas kingman reef",
          "305" ~ "Ilhas macdonal e heard",
          "302" ~ "Ilhas macquaire",
          "67" ~ "Ilhas malvinas",
          "303" ~ "Ilhas marianas",
          "304" ~ "Ilhas marshall",
          "306" ~ "Ilhas niue",
          "307" ~ "Ilhas norfolk",
          "315" ~ "Ilhas nova caledonia",
          "318" ~ "Ilhas novas hebridas",
          "308" ~ "Ilhas palau",
          "320" ~ "Ilhas pascoa",
          "321" ~ "Ilhas pitcairin",
          "309" ~ "Ilhas salomao",
          "326" ~ "Ilhas santa cruz",
          "65" ~ "Ilhas serranas",
          "310" ~ "Ilhas tokelau",
          "80" ~ "Ilhas turca",
          "47" ~ "Ilhas turks e caicos",
          "82" ~ "Ilhas virgens americanas",
          "81" ~ "Ilhas virgens britanicas",
          "311" ~ "Ilhas wake",
          "332" ~ "Ilhas wallis e futuna",
          "255" ~ "India",
          "256" ~ "Indonesia",
          "110" ~ "Inglaterra",
          "257" ~ "Ira",
          "258" ~ "Iraque",
          "112" ~ "Irlanda",
          "111" ~ "Irlanda do norte",
          "113" ~ "Islandia",
          "259" ~ "Israel",
          "39" ~ "Italia",
          "114" ~ "Iugoslavia",
          "66" ~ "Jamaica",
          "41" ~ "Japao",
          "260" ~ "Jordania",
          "150" ~ "Kabardino balkar",
          "312" ~ "Kalimatan",
          "151" ~ "Kalmir",
          "346" ~ "Kara kalpak",
          "152" ~ "Karachaevocherkess",
          "153" ~ "Khakass",
          "261" ~ "Kmer/camboja",
          "154" ~ "Komi",
          "262" ~ "Kuwait",
          "263" ~ "Laos",
          "200" ~ "Lesoto",
          "155" ~ "Letonia",
          "264" ~ "Libano",
          "201" ~ "Liberia",
          "202" ~ "Libia",
          "115" ~ "Liechtenstein",
          "156" ~ "Lituania",
          "116" ~ "Luxemburgo",
          "265" ~ "Macau",
          "205" ~ "Madagascar",
          "203" ~ "Madeira",
          "266" ~ "Malasia",
          "204" ~ "Malawi",
          "267" ~ "Maldivas,is",
          "206" ~ "Mali",
          "157" ~ "Mari",
          "207" ~ "Marrocos",
          "68" ~ "Martinica",
          "268" ~ "Mascate",
          "208" ~ "Mauricio",
          "209" ~ "Mauritania",
          "85" ~ "Mexico",
          "284" ~ "Mianma",
          "210" ~ "Mocambique",
          "158" ~ "Moldavia",
          "118" ~ "Monaco",
          "269" ~ "Mongolia",
          "70" ~ "Monte serrat",
          "137" ~ "Montenegro",
          "240" ~ "Namibia",
          "314" ~ "Nauru",
          "270" ~ "Nepal",
          "211" ~ "Nguane",
          "71" ~ "Nicaragua",
          "213" ~ "Nigeria",
          "119" ~ "Noruega",
          "316" ~ "Nova guine",
          "317" ~ "Nova zelandia",
          "271" ~ "Oman",
          "159" ~ "Ossetia setentrional",
          "121" ~ "Pais de gales",
          "122" ~ "Paises baixos",
          "272" ~ "Palestina",
          "72" ~ "Panama",
          "73" ~ "Panama(zona do canal)",
          "214" ~ "Papua nova guine",
          "273" ~ "Paquistao",
          "24" ~ "Paraguai",
          "89" ~ "Peru",
          "322" ~ "Polinesia francesa",
          "123" ~ "Polonia",
          "74" ~ "Porto rico",
          "45" ~ "Portugal",
          "215" ~ "Pracas norte africanas",
          "216" ~ "Protetor do sudoeste africano",
          "217" ~ "Quenia",
          "160" ~ "Quirguistao",
          "75" ~ "Quitasueno",
          "189" ~ "Republica arabe do egito",
          "218" ~ "Republica centro africana",
          "173" ~ "Republica da africa do sul",
          "140" ~ "Republica da bielorrussia",
          "133" ~ "Republica da macedonia",
          "56" ~ "Republica de el salvador",
          "291" ~ "Republica de fiji",
          "120" ~ "Republica de malta",
          "191" ~ "Republica do gabao",
          "62" ~ "Republica do haiti",
          "212" ~ "Republica do niger",
          "55" ~ "Republica dominicana",
          "88" ~ "Republica guiana",
          "135" ~ "Republica tcheca",
          "20" ~ "Reservado",
          "48" ~ "Reservado",
          "49" ~ "Reservado",
          "50" ~ "Reservado",
          "219" ~ "Reuniao",
          "220" ~ "Rodesia (zimbabwe)",
          "124" ~ "Romenia",
          "76" ~ "Roncador",
          "221" ~ "Ruanda",
          "274" ~ "Ruiquiu,is",
          "348" ~ "Russia",
          "222" ~ "Saara espanhol",
          "323" ~ "Sabah",
          "324" ~ "Samoa americana",
          "325" ~ "Samoa ocidental",
          "125" ~ "San marino",
          "223" ~ "Santa helena",
          "77" ~ "Santa lucia",
          "78" ~ "Sao cristovao",
          "224" ~ "Sao tome e principe",
          "79" ~ "Sao vicente",
          "327" ~ "Sarawak",
          "349" ~ "Senegal",
          "276" ~ "Sequin",
          "226" ~ "Serra leoa",
          "131" ~ "Servia",
          "225" ~ "Seychelles",
          "277" ~ "Siria",
          "227" ~ "Somalia, republica",
          "278" ~ "Sri-lanka",
          "86" ~ "St. pierre et miquelon",
          "228" ~ "Suazilandia",
          "229" ~ "Sudao",
          "126" ~ "Suecia",
          "38" ~ "Suica",
          "90" ~ "Suriname",
          "127" ~ "Svalbard e jan mayer,is",
          "161" ~ "Tadjiquistao",
          "279" ~ "Tailandia",
          "230" ~ "Tanganica",
          "350" ~ "Tanzania",
          "162" ~ "Tartaria",
          "128" ~ "Tchecoslovaquia",
          "335" ~ "Terr. antartico da australia",
          "341" ~ "Terras austrais",
          "231" ~ "Territ. britanico do oceano indico",
          "328" ~ "Territorio de cocos",
          "319" ~ "Territorio de papua",
          "329" ~ "Timor",
          "233" ~ "Togo",
          "330" ~ "Tonga",
          "232" ~ "Transkei",
          "280" ~ "Tregua, estado",
          "91" ~ "Trinidad e tobago",
          "234" ~ "Tunisia",
          "163" ~ "Turcomenistao",
          "281" ~ "Turquia",
          "331" ~ "Tuvalu",
          "164" ~ "Tuvin",
          "165" ~ "Ucrania",
          "166" ~ "Udmurt",
          "235" ~ "Uganda",
          "167" ~ "Uniao sovietica",
          "25" ~ "Uruguai",
          "168" ~ "Uzbequistao",
          "92" ~ "Venezuela",
          "282" ~ "Vietna do norte",
          "283" ~ "Vietna do sul",
          "169" ~ "Yakut",
          "236" ~ "Zaire",
          "237" ~ "Zambia",
          "239" ~ "Zimbabwe",
          .default = .data$NACIONAL
        )) %>%
        dplyr::mutate(NACIONAL = as.factor(.data$NACIONAL))
    }

    # NUM_PROC
    if("NUM_PROC" %in% variables_names){
      data <- data %>%
        dplyr::mutate(NUM_PROC = as.numeric(.data$NUM_PROC))
    }

    # CAR_INT
    if("CAR_INT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CAR_INT = as.character(.data$CAR_INT)) %>%
        dplyr::mutate(CAR_INT = dplyr::case_match(
          .data$CAR_INT,
          "1" ~ "Eletivo",
          "2" ~ "Urg\u00eancia",
          "3" ~ "Acidente no local trabalho ou a serv da empresa",
          "4" ~ "Acidente no trajeto para o trabalho",
          "5" ~ "Outros tipo de acidente de tr\u00e2nsito",
          "6" ~ "Out tp les\u00f5es e envenen por agent qu\u00edm f\u00edsicos",
          .default = .data$CAR_INT
        )) %>%
        dplyr::mutate(CAR_INT = as.factor(.data$CAR_INT))
    }

    # TOT_PT_SP
    if("TOT_PT_SP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(TOT_PT_SP = as.numeric(.data$TOT_PT_SP))
    }

    # CPF_AUT
    if("CPF_AUT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CPF_AUT = as.numeric(.data$CPF_AUT))
    }

    # HOMONIMO
    if("HOMONIMO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(HOMONIMO = as.character(.data$HOMONIMO)) %>%
        dplyr::mutate(HOMONIMO = dplyr::case_match(
          .data$HOMONIMO,
          "0" ~ "N\u00e3o",
          "1" ~ "Sim",
          .default = .data$HOMONIMO
        )) %>%
        dplyr::mutate(HOMONIMO = as.factor(.data$HOMONIMO))
    }

    # NUM_FILHOS
    if("NUM_FILHOS" %in% variables_names){
      data <- data %>%
        dplyr::mutate(NUM_FILHOS = as.numeric(.data$NUM_FILHOS))
    }

    # INSTRU
    if("INSTRU" %in% variables_names){
      data <- data %>%
        dplyr::mutate(INSTRU = as.character(.data$INSTRU)) %>%
        dplyr::mutate(INSTRU = dplyr::case_match(
          .data$INSTRU,
          "1" ~ "Analfabeto",
          "2" ~ "1\u00ba grau",
          "3" ~ "2\u00ba grau",
          "4" ~ "3\u00ba grau",
          "0" ~ NA,
          "9" ~ NA,
          .default = .data$INSTRU
        )) %>%
        dplyr::mutate(INSTRU = as.factor(.data$INSTRU))
    }

    # CONTRACEP1
    if("CONTRACEP1" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CONTRACEP1 = as.character(.data$CONTRACEP1)) %>%
        dplyr::mutate(CONTRACEP1 = dplyr::case_match(
          .data$CONTRACEP1,
          "1" ~ "LAM",
          "2" ~ "Ogino Kaus",
          "3" ~ "Temperatura basal",
          "4" ~ "Billings",
          "5" ~ "Cinto t\u00e9rmico",
          "6" ~ "DIU",
          "7" ~ "Diafragma",
          "8" ~ "Preservativo",
          "9" ~ "Espermicida",
          "10" ~ "Horm\u00f4nio oral",
          "11" ~ "Horm\u00f4nio injet\u00e1vel",
          "12" ~ "Coito interrompido",
          "0" ~ NA,
          "99" ~ NA,
          .default = .data$CONTRACEP1
        )) %>%
        dplyr::mutate(CONTRACEP1 = as.factor(.data$CONTRACEP1))
    }

    # CONTRACEP2
    if("CONTRACEP2" %in% variables_names){
      data <- data %>%
        dplyr::mutate(CONTRACEP2 = as.character(.data$CONTRACEP2)) %>%
        dplyr::mutate(CONTRACEP2 = dplyr::case_match(
          .data$CONTRACEP2,
          "1" ~ "LAM",
          "2" ~ "Ogino Kaus",
          "3" ~ "Temperatura basal",
          "4" ~ "Billings",
          "5" ~ "Cinto t\u00e9rmico",
          "6" ~ "DIU",
          "7" ~ "Diafragma",
          "8" ~ "Preservativo",
          "9" ~ "Espermicida",
          "10" ~ "Horm\u00f4nio oral",
          "11" ~ "Horm\u00f4nio injet\u00e1vel",
          "12" ~ "Coito interrompido",
          "0" ~ NA,
          "99" ~ NA,
          .default = .data$CONTRACEP2
        )) %>%
        dplyr::mutate(CONTRACEP2 = as.factor(.data$CONTRACEP2))
    }

    # GESTRISCO
    if("GESTRISCO" %in% variables_names){
      data <- data %>%
        dplyr::mutate(GESTRISCO = as.character(.data$GESTRISCO)) %>%
        dplyr::mutate(GESTRISCO = dplyr::case_match(
          .data$GESTRISCO,
          "0" ~ "N\u00e3o",
          "1" ~ "Sim",
          .default = .data$GESTRISCO
        )) %>%
        dplyr::mutate(GESTRISCO = as.factor(.data$GESTRISCO))
    }

    # SEQ_AIH5
    if("SEQ_AIH5" %in% variables_names){
      data <- data %>%
        dplyr::mutate(SEQ_AIH5 = as.character(.data$SEQ_AIH5)) %>%
        dplyr::mutate(SEQ_AIH5 = dplyr::case_match(
          .data$SEQ_AIH5,
          "0" ~ "Sequencial zerado",
          "1" ~ "Seq 1",
          "2" ~ "Seq 2",
          "3" ~ "Seq 3",
          "4" ~ NA,
          "999" ~ NA,
          .default = .data$SEQ_AIH5
        )) %>%
        dplyr::mutate(SEQ_AIH5 = as.factor(.data$SEQ_AIH5))
    }

    # CBOR
    if ("CBOR" %in% variables_names) {
      colnames(tabCBO)[1] <- "CBOR"
      data$CBOR <- factor(dplyr::left_join(data, tabCBO, by = "CBOR")$nome)
    }

    # VINCPREV
    if("VINCPREV" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VINCPREV = as.character(.data$VINCPREV)) %>%
        dplyr::mutate(VINCPREV = dplyr::case_match(
          .data$VINCPREV,
          "1" ~ "Aut\u00f4nomo",
          "2" ~ "Desempregado",
          "3" ~ "Aposentado",
          "4" ~ "N\u00e3o segurado",
          "5" ~ "Empregado",
          "6" ~ "Empregador",
          "0" ~ NA,
          "9" ~ NA,
          .default = .data$VINCPREV
        )) %>%
        dplyr::mutate(VINCPREV = as.factor(.data$VINCPREV))
    }

    # GESTOR_COD
    if("GESTOR_COD" %in% variables_names){
      data <- data %>%
        dplyr::mutate(GESTOR_COD = as.character(.data$GESTOR_COD)) %>%
        dplyr::mutate(GESTOR_COD = dplyr::case_match(
          .data$GESTOR_COD,
          "1" ~ "TEMPO DE PERMANENCIA",
          "2" ~ "IDADE MENOR",
          "3" ~ "IDADE MAIOR",
          "4" ~ "TEMPO DE PERMANENCIA E IDADE",
          "5" ~ "QUANTIDADE MAXIMA",
          "7" ~ "PERM.MENOR",
          "8" ~ "ID.MENOR",
          "9" ~ "ID.MENOR E PERM.MENOR",
          "10" ~ "ID.MAIOR",
          "11" ~ "ID.MAIOR E PERM.MENOR",
          "14" ~ "QTD",
          "15" ~ "QTD E PERM.MENOR",
          "16" ~ "QTD E ID.MENOR",
          "17" ~ "QTD E ID.MENOR E PERM.MENOR",
          "18" ~ "QTD E ID.MAIOR",
          "19" ~ "QTD E ID.MAIOR E PERM.MENOR",
          "38" ~ "CBO",
          "39" ~ "CBO E PERM.MENOR",
          "40" ~ "CBO E ID.MENOR",
          "41" ~ "CBO E ID.MENOR E PERM.MENOR",
          "42" ~ "CBO E ID.MAIOR",
          "43" ~ "CBO E ID.MAIOR E PERM.MENOR",
          "46" ~ "CBO E QTD",
          "47" ~ "CBO E QTD E PERM.MENOR",
          "48" ~ "CBO E QTD E ID.MENOR",
          "49" ~ "CBO E QTD E ID.MENOR E PERM.MENOR",
          "50" ~ "CBO E QTD E ID.MAIOR",
          "51" ~ "CBO E QTD E ID.MAIOR E PERM.MENOR",
          "70" ~ "TELEFONE",
          "71" ~ "TELEFONE E PERM.MENOR",
          "72" ~ "TELEFONE E ID.MENOR",
          "73" ~ "TELEFONE E ID.MENOR E PERM.MENOR",
          "74" ~ "TELEFONE E ID.MAIOR",
          "75" ~ "TELEFONE E ID.MAIOR E PERM.MENOR",
          "78" ~ "TELEFONE E QTD",
          "79" ~ "TELEFONE E QTD E PERM.MENOR",
          "80" ~ "TELEFONE E QTD E ID.MENOR",
          "81" ~ "TELEFONE E QTD E ID.MENOR E PERM.MENOR",
          "82" ~ "TELEFONE E QTD E ID.MAIOR",
          "83" ~ "TELEFONE E QTD E ID.MAIOR E PERM.MENOR",
          "102" ~ "TELEFONE E CBO",
          "103" ~ "TELEFONE E CBO E PERM.MENOR",
          "104" ~ "TELEFONE E CBO E ID.MENOR",
          "105" ~ "TELEFONE E CBO E ID.MENOR E PERM.MENOR",
          "106" ~ "TELEFONE E CBO E ID.MAIOR",
          "107" ~ "TELEFONE E CBO E ID.MAIOR E PERM.MENOR",
          "110" ~ "TELEFONE E CBO E QTD",
          "111" ~ "TELEFONE E CBO E QTD E PERM.MENOR",
          "112" ~ "TELEFONE E CBO E QTD E ID.MENOR",
          "113" ~ "TELEFONE E CBO E QTD E ID.MENOR E PERM.MENOR",
          "114" ~ "TELEFONE E CBO E QTD E ID.MAIOR",
          "115" ~ "TELEFONE E CBO E QTD E ID.MAIOR E PERM.MENOR",
          "134" ~ "CNS",
          "136" ~ "CNS E ID. MENOR",
          "137" ~ "CNS E ID. MENOR E PERM. MENOR",
          "138" ~ "CNS E ID. MAIOR",
          "139" ~ "CNS E ID. MAIOR E PERM. MENOR",
          "142" ~ "CNS E QTD",
          "143" ~ "CNS E QTD E PERM. MENOR",
          "144" ~ "CNS E QTD E ID. MENOR",
          "145" ~ "CNS E QTD E ID. MENOR E PERM. MENOR",
          "146" ~ "CNS E QTD E ID. MAIOR",
          "147" ~ "CNS E QTD E ID. MAIOR E PERM. MENOR",
          "166" ~ "CNS E CBO",
          "167" ~ "CNS E CBO E PERM. MENOR",
          "168" ~ "CNS E CBO E ID. MENOR",
          "169" ~ "CNS E CBO E ID. MENOR E PERM. MENOR",
          "170" ~ "CNS E CBO E ID. MAIOR",
          "171" ~ "CNS E CBO E ID. MAIOR E PERM. MENOR",
          "174" ~ "CNS E CBO E QTD",
          "175" ~ "CNS E CBO E QTD E PERM. MENOR",
          "176" ~ "CNS E CBO E QTD E ID. MENOR",
          "177" ~ "CNS E CBO E QTD E ID. MENOR E PERM. MENOR",
          "178" ~ "CNS E CBO E QTD E ID. MAIOR",
          "179" ~ "CNS E CBO E QTD E ID. MAIOR E PERM. MENOR",
          "198" ~ "CNS E TELEFONE",
          "199" ~ "CNS E TELEFONE E PERM. MENOR",
          "200" ~ "CNS E TELEFONE E ID. MENOR",
          "201" ~ "CNS E TELEFONE E ID. MENOR E PERM. MENOR",
          "202" ~ "CNS E TELEFONE E ID. MAIOR",
          "203" ~ "CNS E TELEFONE E ID. MAIOR E PERM. MENOR",
          "206" ~ "CNS E TELEFONE E QTD",
          "207" ~ "CNS E TELEFONE E QTD E PERM. MENOR",
          "208" ~ "CNS E TELEFONE E QTD E ID. MENOR",
          "209" ~ "CNS E TELEFONE E QTD E ID. MENOR E PERM. MENOR",
          "210" ~ "CNS E TELEFONE E QTD E ID. MAIOR",
          "211" ~ "CNS E TELEFONE E QTD E ID. MAIOR E PERM. MENOR",
          "230" ~ "CNS E TELEFONE E CBO",
          "231" ~ "CNS E TELEFONE E CBO E PERM. MENOR",
          "232" ~ "CNS E TELEFONE E CBO E ID. MENOR",
          "233" ~ "CNS E TELEFONE E CBO E ID. MENOR E PERM. MENOR",
          "234" ~ "CNS E TELEFONE E CBO E ID. MAIOR",
          "235" ~ "CNS E TELEFONE E CBO E ID. MAIOR E PERM. MENOR",
          "238" ~ "CNS E TELEFONE E CBO E QTD",
          "239" ~ "CNS E TELEFONE E CBO E QTD E PERM. MENOR",
          "240" ~ "CNS E TELEFONE E CBO E QTD E ID. MENOR",
          "241" ~ "CNS E TELEFONE E CBO E QTD E ID. MENOR E PERM. MENOR",
          "242" ~ "CNS E TELEFONE E CBO E QTD E ID. MAIOR",
          "243" ~ "CNS E TELEFONE E CBO E QTD E ID. MAIOR E PERM. MENOR",
          .default = .data$GESTOR_COD
        )) %>%
        dplyr::mutate(GESTOR_COD = as.factor(.data$GESTOR_COD))
    }

    # GESTOR_TP
    if("GESTOR_TP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(GESTOR_TP = as.numeric(.data$GESTOR_TP))
    }

    # GESTOR_CPF
    if("GESTOR_CPF" %in% variables_names){
      data <- data %>%
        dplyr::mutate(GESTOR_CPF = as.numeric(.data$GESTOR_CPF))
    }

    # GESTOR_DT
    if("GESTOR_DT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(GESTOR_DT = as.numeric(.data$GESTOR_DT))
    }

    # INFEHOSP
    if("INFEHOSP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(INFEHOSP = as.character(.data$INFEHOSP)) %>%
        dplyr::mutate(INFEHOSP = dplyr::case_match(
          .data$INFEHOSP,
          "0" ~ "N\u00e3o",
          "1" ~ "Sim",
          .default = .data$INFEHOSP
        )) %>%
        dplyr::mutate(INFEHOSP = as.factor(.data$INFEHOSP))
    }

    # COMPLEX
    if("COMPLEX" %in% variables_names){
      data <- data %>%
        dplyr::mutate(COMPLEX = as.character(.data$COMPLEX)) %>%
        dplyr::mutate(COMPLEX = dplyr::case_match(
          .data$COMPLEX,
          "1" ~ "Aten\u00e7\u00e3o B\u00e1sica",
          "2" ~ "M\u00e9dia complexidade",
          "3" ~ "Alta complexidade",
          "0" ~ NA,
          "99" ~ NA,
          .default = .data$COMPLEX
        )) %>%
        dplyr::mutate(COMPLEX = as.factor(.data$COMPLEX))
    }

    # FINANC
    if("FINANC" %in% variables_names){
      data <- data %>%
        dplyr::mutate(FINANC = as.character(.data$FINANC)) %>%
        dplyr::mutate(FINANC = dplyr::case_match(
          .data$FINANC,
          "1" ~ "Aten\u00e7\u00e3o B\u00e1sica (PAB)",
          "2" ~ "Assist\u00eancia Farmac\u00eautica",
          "4" ~ "Fundo de A\u00e7\u00f5es Estrat\u00e9gicas e Compensa\u00e7\u00f5es FAEC",
          "5" ~ "Incentivo - MAC",
          "6" ~ "M\u00e9dia e Alta Complexidade (MAC)",
          "7" ~ "Vigil\u00e2ncia em Sa\u00fade",
          "0" ~ NA,
          "99" ~ NA,
          .default = .data$FINANC
        )) %>%
        dplyr::mutate(FINANC = as.factor(.data$FINANC))
    }

    # FAEC_TP
    if("FAEC_TP" %in% variables_names){
      data <- data %>%
        dplyr::mutate(FAEC_TP = as.character(.data$FAEC_TP)) %>%
        dplyr::mutate(FAEC_TP = dplyr::case_match(
          .data$FAEC_TP,
          "10000" ~ "Aten\u00e7\u00e3o B\u00e1sica (PAB)",
          "20000" ~ "Assist\u00eancia Farmac\u00eautica",
          "40001" ~ "Coleta de material",
          "40002" ~ "Diagn\u00f3stico em laborat\u00f3rio cl\u00ednico",
          "40003" ~ "Coleta/exame an\u00e1tomo-patol\u00f3gico colo uterino",
          "40004" ~ "Diagn\u00f3stico em neurologia",
          "40005" ~ "Diagn\u00f3stico em otorrinolaringologia/fonoaudiologia",
          "40006" ~ "Diagn\u00f3stico em psicologia/psiquiatria",
          "40007" ~ "Consultas m\u00e9dicas/outros profissionais de n\u00edvel superior",
          "40008" ~ "Aten\u00e7\u00e3o domiciliar",
          "40009" ~ "Atendimento/acompanhamento em reabilita\u00e7\u00e3o f\u00edsica, mental, visual, auditiva e m\u00faltiplas defic",
          "40010" ~ "Atendimento/acompanhamento psicossocial",
          "40011" ~ "Atendimento/acompanhamento em sa\u00fade do idoso",
          "40012" ~ "Atendimento/acompanhamento de queimados",
          "40013" ~ "Atendimento/acompanhamento de diagn\u00f3stico de doen\u00e7as endocrinas/metab\u00f3licas e nutricionais",
          "40014" ~ "Tratamento de doen\u00e7as do sistema nervoso central e perif\u00e9rico",
          "40015" ~ "Tratamento de doen\u00e7as do aparelho da vis\u00e3o",
          "40016" ~ "Tratamento em oncologia",
          "40017" ~ "Nefrologia",
          "40018" ~ "Tratamentos odontol\u00f3gicos",
          "40019" ~ "Cirurgia do sistema nervoso central e perif\u00e9rico",
          "40020" ~ "Cirurgias de ouvido, nariz e garganta",
          "40021" ~ "Deformidade labio-palatal e cr\u00e2nio-facial",
          "40022" ~ "Cirurgia do aparelho da vis\u00e3o",
          "40023" ~ "Cirurgia do aparelho circulat\u00f3rio",
          "40024" ~ "Cirurgia do aparelho digestivo, org\u00e3os anexos e parede abdominal(inclui pr\u00e9 e p\u00f3s operat\u00f3rio)",
          "40025" ~ "Cirurgia do aparelho geniturin\u00e1rio",
          "40026" ~ "Tratamento de queimados",
          "40027" ~ "Cirurgia reparadora para lipodistrofia",
          "40028" ~ "Outras cirurgias pl\u00e1sticas/reparadoras",
          "40029" ~ "Cirurgia orofacial",
          "40030" ~ "Sequenciais",
          "40031" ~ "Cirurgias em nefrologia",
          "40032" ~ "Transplantes de org\u00e3os, tecidos e c\u00e9lulas",
          "40033" ~ "Medicamentos para transplante",
          "40034" ~ "OPM auditivas",
          "40035" ~ "OPM em odontologia",
          "40036" ~ "OPM em queimados",
          "40037" ~ "OPM em nefrologia",
          "40038" ~ "OPM para transplantes",
          "40039" ~ "Incentivos ao pr\u00e9-natal e nascimento",
          "40040" ~ "Incentivo ao registro c\u00edvil de nascimento",
          "40041" ~ "Central Nacional de Regula\u00e7\u00e3o de Alta Complexidade (CNRAC)",
          "40042" ~ "Reguladores de Atividade hormonal - Inibidores de prolactina",
          "40043" ~ "Pol\u00edtica Nacional de Cirurgias Eletivas",
          "40044" ~ "Redesigna\u00e7\u00e3o e Acompanhamento",
          "40045" ~ "Projeto Olhar Brasil",
          "40046" ~ "Mamografia para Rastreamento",
          "40047" ~ "Projeto Olhar Brasil - Consulta",
          "40048" ~ "Projeto Olhar Brasil - \u00d3culos",
          "40049" ~ "Implementar Cirg. CV Pedi\u00e1trica",
          "40050" ~ "Cirurgias Eletivas - Componente I",
          "40051" ~ "Cirurgias Eletivas - Componente II",
          "40052" ~ "Cirurgias Eletivas - Componente III",
          "40053" ~ "Pr\u00f3tese Mam\u00e1ria - Exames",
          "40054" ~ "Pr\u00f3tese Mam\u00e1ria - Cirurgia",
          "40055" ~ "Transplante - Histocompatibilidade",
          "40056" ~ "Triagem Neonatal",
          "40057" ~ "Controle de qualidade do exame citopatol\u00f3gico do colo de \u00fatero",
          "40058" ~ "Exames do Leite Materno",
          "40059" ~ "Aten\u00e7\u00e3o as Pessoas em Situa\u00e7\u00e3o de Viol\u00eancia Sexual",
          "40060" ~ "Sangue e Hemoderivados",
          "40061" ~ "Mamografia para rastreamento em faixa et\u00e1ria recomendada",
          "40062" ~ "Doen\u00e7as Raras",
          "40063" ~ "Cadeiras de Rodas",
          "40064" ~ "Sistema de Frequencia Modulada Pessoal-FM",
          "40065" ~ "Medicamentos em Urg\u00eancia",
          "40066" ~ "Cirurgias Eletivas - Componente \u00danico",
          "40067" ~ "Aten\u00e7\u00e3o Especializada em Sa\u00fade Auditiva",
          "40068" ~ "Terapias Especializadas em Angiologia",
          "21012" ~ "FAEC CNRAC (21012-c\u00f3d ant \u00e0 tab unif-v\u00e1l p/2008-01)",
          "21014" ~ "FAEC Eletiv(21014-c\u00f3d ant \u00e0 tab unif-v\u00e1l p/2008-01)",
          "50000" ~ "Incentivo - MAC",
          "60000" ~ "M\u00e9dia e Alta Complexidade (MAC)",
          "70000" ~ "Vigil\u00e2ncia em Sa\u00fade",
          "80000" ~ "Gest\u00e3o do SUS",
          .default = .data$FAEC_TP
        )) %>%
        dplyr::mutate(FAEC_TP = as.factor(.data$FAEC_TP))
    }

    # REGCT
    if("REGCT" %in% variables_names){
      data <- data %>%
        dplyr::mutate(REGCT = as.character(.data$REGCT)) %>%
        dplyr::mutate(REGCT = dplyr::case_match(
          .data$REGCT,
          "7100" ~ "TABELA DE NAO GERACAO DE CREDITO POR PRODUCAO NA INTERNACAO E/OU AMBULATORIO",
          "7101" ~ "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO NA MEDIA COMPLEXIDADE AMBULATORIAL",
          "7102" ~ "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO NA MEDIA COMPLEXIDADE HOSPITALAR",
          "7103" ~ "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO NA ALTA COMPLEXIDADE AMBULATORIAL",
          "7104" ~ "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO NA ALTA COMPLEXIDADE HOSPITALAR",
          "7105" ~ "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO PARA OS PROCEDIMENTOS FINANCIADOS COM O FAEC",
          "7106" ~ "ESTABELECIMENTO SEM GERA\u00c7\u00c3O DE CREDITO TOTAL - EXCLUINDO FAEC",
          "7107" ~ "ESTABELECIMENTO SEM GERACAO DE CREDITO NAS ACOES ESPEC. DE ODONTOLOGIA(INCENTIVO CEO I,II E III)",
          "7108" ~ "ESTABELECIMENTO SEM GERACAO DE CREDITO(INCENTIVO A SAUDE DO TRABALHADOR)",
          "7109" ~ "ESTABELECIMENTO SEM GERACAO DE CREDITO TOTAL-MEC",
          "7110" ~ "ESTABELECIMENTO DE SAUDE DA ESTRUTURA DO MINISTERIO DA SAUDE - SEM GERA\u00c7AO DE CREDITO TOTAL",
          "7111" ~ "ESTABELECIMENTO DE SAUDE SEM GERACAO DE CREDITO - NASF, EXCETO FAEC",
          "7112" ~ "ESTABELECIMENTO SEM GERA\u00c7\u00c3O DE CREDITO TOTAL - INCLUINDO FAEC  - EXCLUSIVO PARA REDE SARAH",
          "7113" ~ "ESTABELECIMENTO SEM GERA\u00c7\u00c3O DE CREDITO TOTAL - INCLUINDO FAEC - OUTROS ESTABELECIMENTOS FEDERAIS",
          "7114" ~ "ESTABELECIMENTO DE SA\u00daDE SEM GERA\u00c7\u00c3O DE CR\u00c9DITO TOTAL, INCLUSIVE FAEC - PRONTO ATENDIMENTO",
          "7115" ~ "ESTABELECIMENTO DE SA\u00daDE SEM GERA\u00c7\u00c3O DE CR\u00c9DITO NA M\u00c9DIA COMPLEXIDADE - HU/MEC",
          "7116" ~ "ESTABELECIMENTO DE SA\u00daDE SEM GERA\u00c7\u00c3O DE CR\u00c9DITO NA M\u00c9DIA COMPLEXIDADE - LRPD",
          "7117" ~ "Estabelecimento de Sa\u00fade sem gera\u00e7\u00e3o de cr\u00e9dito na m\u00e9dia complexidade (exceto OPM) - CER",
          "0" ~ "Sem regra contratual",
          .default = .data$REGCT
        )) %>%
        dplyr::mutate(REGCT = as.factor(.data$REGCT))



    }

    # RACA_COR
    if("RACA_COR" %in% variables_names){
      data <- data %>%
        dplyr::mutate(RACA_COR = as.character(.data$RACA_COR)) %>%
        dplyr::mutate(RACA_COR = dplyr::case_match(
          .data$RACA_COR,
          "1" ~ "Branca",
          "2" ~ "Preta",
          "3" ~ "Parda",
          "4" ~ "Amarela",
          "5" ~ "Ind\u00edgena",
          "0" ~ NA,
          "99" ~ NA,
          .default = .data$RACA_COR
        )) %>%
        dplyr::mutate(RACA_COR = as.factor(.data$RACA_COR))
    }

    # ETNIA
    if("ETNIA" %in% variables_names){
      data <- data %>%
        dplyr::mutate(ETNIA = as.character(.data$ETNIA)) %>%
        dplyr::mutate(ETNIA = dplyr::case_match(
          .data$ETNIA,
          "0001" ~ "ACONA (WAKONAS, NACONAS, JAKONA, ACORANES)",
          "0002" ~ "AIKANA (AIKANA, MAS SAKA,TUBARAO)",
          "0003" ~ "AJURU",
          "0004" ~ "AKUNSU (AKUNT'SU)",
          "0005" ~ "AMANAYE",
          "0006" ~ "AMONDAWA",
          "0007" ~ "ANAMBE",
          "0008" ~ "APARAI (APALAI)",
          "0009" ~ "APIAKA (APIACA)",
          "0010" ~ "APINAYE (APINAJE/APINAIE/APINAGE)",
          "0011" ~ "APURINA (APORINA, IPURINA, IPURINA, IPURINAN)",
          "0012" ~ "ARANA (ARACUAI DO VALE DO JEQUITINHONHA)",
          "0013" ~ "ARAPASO (ARAPACO)",
          "0014" ~ "ARARA DE RONDONIA (KARO, URUCU, URUKU)",
          "0015" ~ "ARARA DO ACRE (SHAWANAUA, AMAWAKA)",
          "0016" ~ "ARARA DO ARIPUANA (ARARA DO BEIRADAO/ARI-PUANA)",
          "0017" ~ "ARARA DO PARA (UKARAGMA, UKARAMMA)",
          "0018" ~ "ARAWETE (ARAUETE)",
          "0019" ~ "ARIKAPU (ARICAPU, ARIKAPO, MASUBI, MAXUBI)",
          "0020" ~ "ARIKEM (ARIQUEN, ARIQUEME, ARIKEME)",
          "0021" ~ "ARIKOSE (ARICOBE)",
          "0022" ~ "ARUA",
          "0023" ~ "ARUAK (ARAWAK)",
          "0024" ~ "ASHANINKA (KAMPA)",
          "0025" ~ "ASURINI DO TOCANTINS (AKUAWA/AKWAWA)",
          "0026" ~ "ASURINI DO XINGU (AWAETE)",
          "0027" ~ "ATIKUM (ATICUM)",
          "0028" ~ "AVA - CANOEIRO",
          "0029" ~ "AWETI (AUETI/AUETO)",
          "0030" ~ "BAKAIRI (KURA, BACAIRI)",
          "0031" ~ "BANAWA YAFI (BANAWA, BANAWA-JAFI)",
          "0032" ~ "BANIWA (BANIUA, BANIVA, WALIMANAI, WAKUENAI)",
          "0033" ~ "BARA (WAIPINOMAKA)",
          "0034" ~ "BARASANA (HANERA)",
          "0035" ~ "BARE",
          "0036" ~ "BORORO (BOE)",
          "0037" ~ "BOTOCUDO (GEREN)",
          "0038" ~ "CANOE",
          "0039" ~ "CASSUPA",
          "0040" ~ "CHAMACOCO",
          "0041" ~ "CHIQUITANO (XIQUITANO)",
          "0042" ~ "CIKIYANA (SIKIANA)",
          "0043" ~ "CINTA LARGA (MATETAMAE)",
          "0044" ~ "COLUMBIARA (CORUMBIARA)",
          "0045" ~ "DENI",
          "0046" ~ "DESANA (DESANA, DESANO, DESSANO, WIRA, UMUKOMASA)",
          "0047" ~ "DIAHUI (JAHOI, JAHUI, DIARROI)",
          "0048" ~ "ENAWENE-NAWE (SALUMA)",
          "0049" ~ "FULNI-O",
          "0050" ~ "GALIBI (GALIBI DO OIAPOQUE, KARINHA)",
          "0051" ~ "GALIBI MARWORNO (GALIBI DO UACA, ARUA)",
          "0052" ~ "GAVIAO DE RONDONIA (DIGUT)",
          "0053" ~ "GAVIAO KRIKATEJE",
          "0054" ~ "GAVIAO PARKATEJE (PARKATEJE)",
          "0055" ~ "GAVIAO PUKOBIE (PUKOBIE, PYKOPJE, GAVIAO DO MARANHAO)",
          "0056" ~ "GUAJA (AWA, AVA)",
          "0057" ~ "GUAJAJARA (TENETEHARA)",
          "0058" ~ "GUARANI KAIOWA (PAI TAVYTERA)",
          "0059" ~ "GUARANI M'BYA",
          "0060" ~ "GUARANI NANDEVA (AVAKATUETE, CHIRIPA,NHANDEWA, AVA GUARANI)",
          "0061" ~ "GUATO",
          "0062" ~ "HIMARIMA (HIMERIMA)",
          "0063" ~ "INGARIKO (INGARICO, AKAWAIO, KAPON)",
          "0064" ~ "IRANXE (IRANTXE)",
          "0065" ~ "ISSE",
          "0066" ~ "JABOTI (JABUTI, KIPIU, YABYTI)",
          "0067" ~ "JAMAMADI (YAMAMADI, DJEOROMITXI)",
          "0068" ~ "JARAWARA",
          "0069" ~ "JIRIPANCO (JERIPANCO, GERIPANCO)",
          "0070" ~ "JUMA (YUMA)",
          "0071" ~ "JURUNA",
          "0072" ~ "JURUTI (YURITI)",
          "0073" ~ "KAAPOR (URUBU-KAAPOR, KA'APOR, KAAPORTE)",
          "0074" ~ "KADIWEU (CADUVEO, CADIUEU)",
          "0075" ~ "KAIABI (CAIABI, KAYABI)",
          "0076" ~ "KAIMBE (CAIMBE)",
          "0077" ~ "KAINGANG (CAINGANGUE)",
          "0078" ~ "KAIXANA (CAIXANA)",
          "0079" ~ "KALABASSA (CALABASSA, CALABACAS)",
          "0080" ~ "KALANCO",
          "0081" ~ "KALAPALO (CALAPALO)",
          "0082" ~ "KAMAYURA (CAMAIURA, KAMAIURA)",
          "0083" ~ "KAMBA (CAMBA)",
          "0084" ~ "KAMBEBA (CAMBEBA, OMAGUA)",
          "0085" ~ "KAMBIWA (CAMBIUA)",
          "0086" ~ "KAMBIWA PIPIPA (PIPIPA)",
          "0087" ~ "KAMPE",
          "0088" ~ "KANAMANTI (KANAMATI, CANAMANTI)",
          "0089" ~ "KANAMARI (CANAMARI, KANAMARY, TUKUNA)",
          "0090" ~ "KANELA APANIEKRA (CANELA)",
          "0091" ~ "KANELA RANKOKAMEKRA (CANELA)",
          "0092" ~ "KANINDE",
          "0093" ~ "KANOE (CANOE)",
          "0094" ~ "KANTARURE (CANTARURE)",
          "0095" ~ "KAPINAWA (CAPINAUA)",
          "0096" ~ "KARAJA (CARAJA)",
          "0097" ~ "KARAJA/JAVAE (JAVAE)",
          "0098" ~ "KARAJA/XAMBIOA (KARAJA DO NORTE)",
          "0099" ~ "KARAPANA (CARAPANA, MUTEAMASA, UKOPINOPONA)",
          "0100" ~ "KARAPOTO (CARAPOTO)",
          "0101" ~ "KARIPUNA (CARIPUNA)",
          "0102" ~ "KARIPUNA DO AMAPA (CARIPUNA)",
          "0103" ~ "KARIRI (CARIRI)",
          "0104" ~ "KARIRI-XOCO (CARIRI-CHOCO)",
          "0105" ~ "KARITIANA (CARITIANA)",
          "0106" ~ "KATAWIXI (KATAUIXI,KATAWIN, KATAWISI, CATAUICHI)",
          "0107" ~ "KATUENA (CATUENA, KATWENA)",
          "0108" ~ "KATUKINA (PEDA DJAPA)",
          "0109" ~ "KATUKINA DO ACRE",
          "0110" ~ "KAXARARI (CAXARARI)",
          "0111" ~ "KAXINAWA (HUNI-KUIN, CASHINAUA, CAXINAUA)",
          "0112" ~ "KAXIXO",
          "0113" ~ "KAXUYANA (CAXUIANA)",
          "0114" ~ "KAYAPO (CAIAPO)",
          "0115" ~ "KAYAPO KARARAO (KARARAO)",
          "0116" ~ "KAYAPO TXUKAHAMAE (TXUKAHAMAE)",
          "0117" ~ "KAYAPO XICRIM (XIKRIN)",
          "0118" ~ "KAYUISANA (CAIXANA, CAUIXANA, KAIXANA)",
          "0119" ~ "KINIKINAWA (GUAN, KOINUKOEN, KINIKINAO)",
          "0120" ~ "KIRIRI",
          "0121" ~ "KOCAMA (COCAMA, KOKAMA)",
          "0122" ~ "KOKUIREGATEJE",
          "0123" ~ "KORUBO",
          "0124" ~ "KRAHO (CRAO, KRAO)",
          "0125" ~ "KREJE (KRENYE)",
          "0126" ~ "KRENAK (BORUN, CRENAQUE)",
          "0127" ~ "KRIKATI (KRINKATI)",
          "0128" ~ "KUBEO (CUBEO, COBEWA, KUBEWA, PAMIWA, CUBEU)",
          "0129" ~ "KUIKURO (KUIKURU, CUICURO)",
          "0130" ~ "KUJUBIM (KUYUBI, CUJUBIM)",
          "0131" ~ "KULINA PANO (CULINA)",
          "0132" ~ "KULINA/MADIHA (CULINA, MADIJA, MADIHA)",
          "0133" ~ "KURIPAKO (CURIPACO, CURRIPACO, CORIPACO, WAKUENAI)",
          "0134" ~ "KURUAIA (CURUAIA)",
          "0135" ~ "KWAZA (COAIA, KOAIA)",
          "0136" ~ "MACHINERI (MANCHINERI, MANXINERI)",
          "0137" ~ "MACURAP (MAKURAP)",
          "0138" ~ "MAKU DOW (DOW)",
          "0139" ~ "MAKU HUPDA (HUPDA)",
          "0140" ~ "MAKU NADEB (NADEB)",
          "0141" ~ "MAKU YUHUPDE (YUHUPDE)",
          "0142" ~ "MAKUNA (MACUNA, YEBA-MASA)",
          "0143" ~ "MAKUXI (MACUXI, MACHUSI, PEMON)",
          "0144" ~ "MARIMAM (MARIMA)",
          "0145" ~ "MARUBO",
          "0146" ~ "MATIPU",
          "0147" ~ "MATIS",
          "0148" ~ "MATSE (MAYORUNA)",
          "0149" ~ "MAXAKALI (MAXACALI)",
          "0150" ~ "MAYA (MAYA)",
          "0151" ~ "MAYTAPU",
          "0152" ~ "MEHINAKO (MEINAKU, MEINACU)",
          "0153" ~ "MEKEN (MEQUEM, MEKHEM, MICHENS)",
          "0154" ~ "MENKY (MYKY, MUNKU, MENKI, MYNKY)",
          "0155" ~ "MIRANHA (MIRANHA, MIRANA)",
          "0156" ~ "MIRITI TAPUIA (MIRITI-TAPUYA, BUIA-TAPUYA)",
          "0157" ~ "MUNDURUKU (MUNDURUCU)",
          "0158" ~ "MURA",
          "0159" ~ "NAHUKWA (NAFUQUA)",
          "0160" ~ "NAMBIKWARA DO CAMPO (HALOTESU, KITHAULU, WAKALITESU, SAWENTES, MANDUKA)",
          "0161" ~ "NAMBIKWARA DO NORTE (NEGAROTE ,MAMAINDE, LATUNDE, SABANE E MANDUKA, TAWANDE)",
          "0162" ~ "NAMBIKWARA DO SUL (WASUSU ,HAHAINTESU, ALANTESU, WAIKISU, ALAKETESU, WASUSU, SARARE)",
          "0163" ~ "NARAVUTE (NARUVOTO)",
          "0164" ~ "NAWA (NAUA)",
          "0165" ~ "NUKINI (NUQUINI, NUKUINI)",
          "0166" ~ "OFAIE (OFAYE-XAVANTE)",
          "0167" ~ "ORO WIN",
          "0168" ~ "PAIAKU (JENIPAPO-KANINDE)",
          "0169" ~ "PAKAA NOVA (WARI, PACAAS NOVOS)",
          "0170" ~ "PALIKUR (AUKWAYENE, AUKUYENE, PALIKU'ENE)",
          "0171" ~ "PANARA (KRENHAKARORE , KRENAKORE, KRENA-KARORE)",
          "0172" ~ "PANKARARE (PANCARARE)",
          "0173" ~ "PANKARARU (PANCARARU)",
          "0174" ~ "PANKARARU KALANKO (KALANKO)",
          "0175" ~ "PANKARARU KARUAZU (KARUAZU)",
          "0176" ~ "PANKARU (PANCARU)",
          "0177" ~ "PARAKANA (PARACANA, APITEREWA, AWAETE)",
          "0178" ~ "PARECI (PARESI, HALITI)",
          "0179" ~ "PARINTINTIN",
          "0180" ~ "PATAMONA (KAPON)",
          "0181" ~ "PATAXO",
          "0182" ~ "PATAXO HA-HA-HAE",
          "0183" ~ "PAUMARI (PALMARI)",
          "0184" ~ "PAUMELENHO",
          "0185" ~ "PIRAHA (MURA PIRAHA)",
          "0186" ~ "PIRATUAPUIA (PIRATAPUYA, PIRATAPUYO, PIRA-TAPUYA, WAIKANA)",
          "0187" ~ "PITAGUARI",
          "0188" ~ "POTIGUARA",
          "0189" ~ "POYANAWA (POIANAUA)",
          "0190" ~ "RIKBAKTSA (CANOEIROS, ERIGPAKTSA)",
          "0191" ~ "SAKURABIAT(MEKENS, SAKIRABIAP, SAKIRABIAR)",
          "0192" ~ "SATERE-MAWE (SATERE-MAUE)",
          "0193" ~ "SHANENAWA (KATUKINA)",
          "0194" ~ "SIRIANO (SIRIA-MASA)",
          "0195" ~ "SURIANA",
          "0196" ~ "SURUI DE RONDONIA (PAITER)",
          "0197" ~ "SURUI DO PARA (AIKEWARA)",
          "0198" ~ "SUYA (SUIA/KISEDJE)",
          "0199" ~ "TAPAYUNA (BEICO-DE-PAU)",
          "0200" ~ "TAPEBA",
          "0201" ~ "TAPIRAPE (TAPI'IRAPE)",
          "0202" ~ "TAPUIA (TAPUIA-XAVANTE, TAPUIO)",
          "0203" ~ "TARIANO (TARIANA, TALIASERI)",
          "0204" ~ "TAUREPANG (TAULIPANG, PEMON, AREKUNA, PAGEYN)",
          "0205" ~ "TEMBE",
          "0206" ~ "TENHARIM",
          "0207" ~ "TERENA",
          "0208" ~ "TICUNA (TIKUNA, TUKUNA, MAGUTA)",
          "0209" ~ "TINGUI BOTO",
          "0210" ~ "TIRIYO EWARHUYANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)",
          "0211" ~ "TIRIYO KAH'YANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)",
          "0212" ~ "TIRIYO TSIKUYANA (TIRIYO, TRIO, TARONA, YAWI, PIANOKOTO)",
          "0213" ~ "TORA",
          "0214" ~ "TREMEMBE",
          "0215" ~ "TRUKA",
          "0216" ~ "TRUMAI",
          "0217" ~ "TSOHOM DJAPA (TSUNHUM-DJAPA)",
          "0218" ~ "TUKANO (TUCANO, YE'PA-MASA, DASEA)",
          "0219" ~ "TUMBALALA",
          "0220" ~ "TUNAYANA",
          "0221" ~ "TUPARI",
          "0222" ~ "TUPINAMBA",
          "0223" ~ "TUPINIQUIM",
          "0224" ~ "TURIWARA",
          "0225" ~ "TUXA",
          "0226" ~ "TUYUKA (TUIUCA, DOKAPUARA, UTAPINOMAKAPHONA)",
          "0227" ~ "TXIKAO (TXICAO, IKPENG)",
          "0228" ~ "UMUTINA (OMOTINA, BARBADOS)",
          "0229" ~ "URU-EU-WAU-WAU (URUEU-UAU-UAU, URUPAIN, URUPA)",
          "0230" ~ "WAI WAI HIXKARYANA (HIXKARYANA)",
          "0231" ~ "WAI WAI KARAFAWYANA (KARAFAWYANA, KARA-PAWYANA)",
          "0232" ~ "WAI WAI XEREU (XEREU)",
          "0233" ~ "WAI WAI KATUENA (KATUENA)",
          "0234" ~ "WAI WAI MAWAYANA (MAWAYANA)",
          "0235" ~ "WAIAPI (WAYAMPI, OYAMPI, WAYAPY, )",
          "0236" ~ "WAIMIRI ATROARI (KINA)",
          "0237" ~ "WANANO (UANANO, WANANA)",
          "0238" ~ "WAPIXANA (UAPIXANA, VAPIDIANA, WAPISIANA, WAPISHANA)",
          "0239" ~ "WAREKENA (UAREQUENA, WEREKENA)",
          "0240" ~ "WASSU",
          "0241" ~ "WAURA (UAURA, WAUJA)",
          "0242" ~ "WAYANA (WAIANA, UAIANA)",
          "0243" ~ "WITOTO (UITOTO, HUITOTO)",
          "0244" ~ "XAKRIABA (XACRIABA)",
          "0245" ~ "XAVANTE (A'UWE, AKWE, AWEN, AKWEN)",
          "0246" ~ "XERENTE (AKWE, AWEN, AKWEN)",
          "0247" ~ "XETA",
          "0248" ~ "XIPAIA (SHIPAYA, XIPAYA)",
          "0249" ~ "XOKLENG (SHOKLENG, XOCLENG)",
          "0250" ~ "XOKO (XOCO, CHOCO)",
          "0251" ~ "XUKURU (XUCURU)",
          "0252" ~ "XUKURU KARIRI (XUCURU-KARIRI)",
          "0253" ~ "YAIPIYANA",
          "0254" ~ "YAMINAWA (JAMINAWA, IAMINAWA)",
          "0255" ~ "YANOMAMI NINAM (IANOMAMI, IANOAMA, XIRIANA)",
          "0256" ~ "YANOMAMI SANUMA (IANOMAMI, IANOAMA, XIRIANA)",
          "0257" ~ "YANOMAMI YANOMAM (IANOMAMI, IANOAMA, XIRIANA)",
          "0258" ~ "YAWALAPITI (IAUALAPITI)",
          "0259" ~ "YAWANAWA (IAUANAUA)",
          "0260" ~ "YEKUANA (MAIONGON, YE'KUANA, YEKWANA, MAYONGONG)",
          "0261" ~ "YUDJA (JURUNA, YURUNA)",
          "0262" ~ "ZO'E (POTURU)",
          "0263" ~ "ZORO (PAGEYN)",
          "0264" ~ "ZURUAHA (SOROWAHA, SURUWAHA)",
          "X265" ~ "AHANENAWA",
          "X266" ~ "AICABA",
          "X267" ~ "AIKAN\\u00c3-KWAS\\u00c1",
          "X268" ~ "AKUNTSU",
          "X269" ~ "ALANTESU",
          "X271" ~ "AMAW\\u00c1KA",
          "X272" ~ "ANAC\\u00c9",
          "X273" ~ "APURIN\\u00c3",
          "X274" ~ "ARAN\\u00c3",
          "X275" ~ "ARAPA\\u00c7O",
          "X276" ~ "ARARA APOLIMA",
          "X277" ~ "ARARA DO ARIPUANA",
          "X278" ~ "ARIPUAN\\u00c1",
          "X279" ~ "ASSURINI",
          "X280" ~ "AWUAR\\u00c1",
          "X281" ~ "BORBA",
          "X282" ~ "CABIXI",
          "X283" ~ "CAMARAR\\u00c9",
          "X284" ~ "CAMASURI",
          "X285" ~ "CARA PRETA",
          "X286" ~ "CHARRUA",
          "X287" ~ "CUJUBIM",
          "X288" ~ "DAW",
          "X289" ~ "GAVI\\u00c3O",
          "X290" ~ "GUARANI",
          "X291" ~ "HALANTESU",
          "X292" ~ "HALOTESU",
          "X293" ~ "HENGAT\\u00da",
          "X294" ~ "HIXKARYANA",
          "X295" ~ "HUPDE",
          "X296" ~ "HUPDES",
          "X297" ~ "IAUANAUA",
          "X298" ~ "IAUARETE A\\u00c7U",
          "X299" ~ "IKPENG",
          "X300" ~ "INAMBU",
          "X301" ~ "INHABARANA",
          "X302" ~ "JAVAE",
          "X303" ~ "JENIPAPO",
          "X304" ~ "JENIPAPO-KANINDE",
          "X305" ~ "JIAHOI",
          "X306" ~ "KAIOWA",
          "X307" ~ "KAMPA",
          "X308" ~ "KANELA",
          "X309" ~ "KARAFAWYANA",
          "X310" ~ "KARARAO",
          "X311" ~ "KARUBO",
          "X312" ~ "KASSUP\\u00c1",
          "X313" ~ "KATITH\\u00c3ULU",
          "X314" ~ "KATOKIN",
          "X315" ~ "KATUKINA PANO",
          "X316" ~ "KATUKINA PEDA DJAPA",
          "X317" ~ "KATUKINA SHANENAUWA",
          "X318" ~ "KAXAGO",
          "X319" ~ "KAYABI",
          "X320" ~ "KIN\\u00c3 (WAIMIRI-ATROARI)",
          "X321" ~ "KIRIRI-BARRA",
          "X322" ~ "KITH\\u00c3ULU",
          "X323" ~ "KOIAI\\u00c1",
          "X324" ~ "KOIUPANK\\u00c1",
          "X325" ~ "KONTANAWA",
          "X326" ~ "KRAH\\u00d4 KANELA",
          "X327" ~ "KULINA",
          "X328" ~ "LATUND\\u00ca",
          "X329" ~ "MAKU",
          "X330" ~ "MAKUNAMB\\u00c9",
          "X331" ~ "MAMAIND\\u00ca",
          "X332" ~ "MAMURI",
          "X333" ~ "MANACAPURU",
          "X334" ~ "MANAIRISSU",
          "X335" ~ "MANCHINERI",
          "X336" ~ "MANDUCA",
          "X337" ~ "MARIBONDO",
          "X338" ~ "MASSAKA",
          "X339" ~ "MAWAYANA",
          "X340" ~ "MAW\\u00c9",
          "X341" ~ "MAYORUNA",
          "X342" ~ "MIQUELENO",
          "X343" ~ "MOKURI\\u00d1",
          "X344" ~ "MON ORO WARAM",
          "X345" ~ "MUTUM",
          "X346" ~ "MYKY",
          "X347" ~ "NADEB",
          "X348" ~ "NAMBIKWARA",
          "X349" ~ "NEGAROT\\u00ca",
          "X350" ~ "NHENGATU",
          "X351" ~ "OFAIE XAVANTE",
          "X352" ~ "ON\\u00c7A",
          "X353" ~ "ORO AT",
          "X354" ~ "ORO EO",
          "X355" ~ "ORO JOWIN",
          "X356" ~ "ORO MIYLIN",
          "X357" ~ "ORO MON",
          "X358" ~ "ORO N\\u00c1O",
          "X359" ~ "ORO WAM",
          "X360" ~ "ORO WARAM",
          "X361" ~ "ORO WARAM XIJEIN",
          "X362" ~ "PACA",
          "X363" ~ "PANKAR\\u00c1",
          "X364" ~ "PAPAGAIO",
          "X365" ~ "PAYAY\\u00c1",
          "X366" ~ "PIPIPAN",
          "X367" ~ "PIRATA",
          "X368" ~ "PUROBOR\\u00c1",
          "X369" ~ "SABAN\\u00ca",
          "X370" ~ "SANUMA",
          "X371" ~ "SAWENTES\\u00da",
          "X372" ~ "SILCY-TAPUYA",
          "X373" ~ "SIUCI",
          "X374" ~ "TABAJARA",
          "X375" ~ "TAKUARA",
          "X376" ~ "TATU",
          "X377" ~ "TAWAND\\u00ca",
          "X378" ~ "TEF\\u00c9",
          "X379" ~ "TIMBIRA",
          "X380" ~ "TOR\\u00c1 DO BAIXO GRANDE",
          "X381" ~ "TSUNHUM-DJAP\\u00c1",
          "X382" ~ "TUBAR\\u00c3O",
          "X383" ~ "TUPAIU",
          "X384" ~ "TUPI",
          "X385" ~ "TUPINAMB\\u00c1 DE BELMONTE",
          "X386" ~ "URUBU",
          "X387" ~ "URUBU KAAPOR",
          "X388" ~ "URUP\\u00c1",
          "X389" ~ "WAI WAI",
          "X390" ~ "WAIKISU",
          "X391" ~ "WAKALITES\\u00da",
          "X392" ~ "WASSUSU",
          "X393" ~ "XEREU",
          "X394" ~ "XI EIN",
          "X395" ~ "XICRIN",
          "X396" ~ "XIPAYA",
          "X397" ~ "XIRIANA",
          "X398" ~ "XIRUAI",
          "X399" ~ "YEPAMASS\\u00c3",
          "X400" ~ "TIRIY\\u00d3",
          "X401" ~ "YANOMAMI",
          "X402" ~ "ARARA",
          "X403" ~ "SAKIRIABAR",
          "X404" ~ "TATZ",
          "X405" ~ "SEM INFORMACAO",
          .default = .data$ETNIA
        )) %>%
        dplyr::mutate(ETNIA = as.factor(.data$ETNIA))
    }

    # VAL_SH_FED
    if("VAL_SH_FED" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_SH_FED = as.numeric(.data$VAL_SH_FED))
    }

    # VAL_SP_FED
    if("VAL_SP_FED" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_SP_FED = as.numeric(.data$VAL_SP_FED))
    }

    # VAL_SH_GES
    if("VAL_SH_GES" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_SH_GES = as.numeric(.data$VAL_SH_GES))
    }

    # VAL_SP_GES
    if("VAL_SP_GES" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_SP_GES = as.numeric(.data$VAL_SP_GES))
    }

    # VAL_UCI
    if("VAL_UCI" %in% variables_names){
      data <- data %>%
        dplyr::mutate(VAL_UCI = as.numeric(.data$VAL_UCI))
    }

    # MARCA_UCI
    if("MARCA_UCI" %in% variables_names){
      data <- data %>%
        dplyr::mutate(MARCA_UCI = as.character(.data$MARCA_UCI)) %>%
        dplyr::mutate(MARCA_UCI = dplyr::case_match(
          .data$MARCA_UCI,
          "0" ~ "N\u00e3o utilizou UCI",
          "1" ~ "Unidade de cuidados intermed neonatal convencional",
          "2" ~ "Unidade de cuidados intermed neonatal canguru",
          "3" ~ "Unidade intermedi\u00e1ria neonatal",
          "88" ~  "Utilizou dois tipos de leitos UCI",
          .default = .data$MARCA_UCI
        )) %>%
        dplyr::mutate(MARCA_UCI = as.factor(.data$MARCA_UCI))
    }

    # TPDISEC1
    if("TPDISEC1" %in% variables_names){
      data <- data %>%
        dplyr::mutate(TPDISEC1 = as.character(.data$TPDISEC1)) %>%
        dplyr::mutate(TPDISEC1 = dplyr::case_match(
          .data$TPDISEC1,
          "0" ~ NA,
          "1" ~ "Pr\u00e9-existente",
          "2" ~ "Adquirido",
          .default = .data$TPDISEC1
        )) %>%
        dplyr::mutate(TPDISEC1 = as.factor(.data$TPDISEC1))
    }

    # TPDISEC2
    if("TPDISEC2" %in% variables_names){
      data <- data %>%
        dplyr::mutate(TPDISEC2 = as.character(.data$TPDISEC2)) %>%
        dplyr::mutate(TPDISEC2 = dplyr::case_match(
          .data$TPDISEC2,
          "0" ~ NA,
          "1" ~ "Pr\u00e9-existente",
          "2" ~ "Adquirido",
          .default = .data$TPDISEC2
        )) %>%
        dplyr::mutate(TPDISEC2 = as.factor(.data$TPDISEC2))
    }

    # TPDISEC3
    if("TPDISEC3" %in% variables_names){
      data <- data %>%
        dplyr::mutate(TPDISEC3 = as.character(.data$TPDISEC3)) %>%
        dplyr::mutate(TPDISEC3 = dplyr::case_match(
          .data$TPDISEC3,
          "0" ~ NA,
          "1" ~ "Pr\u00e9-existente",
          "2" ~ "Adquirido",
          .default = .data$TPDISEC3
        )) %>%
        dplyr::mutate(TPDISEC3 = as.factor(.data$TPDISEC3))
    }

    # TPDISEC4
    if("TPDISEC4" %in% variables_names){
      data <- data %>%
        dplyr::mutate(TPDISEC4 = as.character(.data$TPDISEC4)) %>%
        dplyr::mutate(TPDISEC4 = dplyr::case_match(
          .data$TPDISEC4,
          "0" ~ NA,
          "1" ~ "Pr\u00e9-existente",
          "2" ~ "Adquirido",
          .default = .data$TPDISEC4
        )) %>%
        dplyr::mutate(TPDISEC4 = as.factor(.data$TPDISEC4))
      data$TPDISEC4 <- factor(data$TPDISEC4)
    }

    # TPDISEC5
    if("TPDISEC5" %in% variables_names){
      data <- data %>%
        dplyr::mutate(TPDISEC5 = as.character(.data$TPDISEC5)) %>%
        dplyr::mutate(TPDISEC5 = dplyr::case_match(
          .data$TPDISEC5,
          "0" ~ NA,
          "1" ~ "Pr\u00e9-existente",
          "2" ~ "Adquirido",
          .default = .data$TPDISEC5
        )) %>%
        dplyr::mutate(TPDISEC5 = as.factor(.data$TPDISEC5))
    }

    # TPDISEC6
    if("TPDISEC6" %in% variables_names){
      data <- data %>%
        dplyr::mutate(TPDISEC6 = as.character(.data$TPDISEC6)) %>%
        dplyr::mutate(TPDISEC6 = dplyr::case_match(
          .data$TPDISEC6,
          "0" ~ NA,
          "1" ~ "Pr\u00e9-existente",
          "2" ~ "Adquirido",
          .default = .data$TPDISEC6
        )) %>%
        dplyr::mutate(TPDISEC6 = as.factor(.data$TPDISEC6))
    }

    # TPDISEC7
    if("TPDISEC7" %in% variables_names){
      data <- data %>%
        dplyr::mutate(TPDISEC7 = as.character(.data$TPDISEC7)) %>%
        dplyr::mutate(TPDISEC7 = dplyr::case_match(
          .data$TPDISEC7,
          "0" ~ NA,
          "1" ~ "Pr\u00e9-existente",
          "2" ~ "Adquirido",
          .default = .data$TPDISEC7
        )) %>%
        dplyr::mutate(TPDISEC7 = as.factor(.data$TPDISEC7))
    }

    # TPDISEC8
    if("TPDISEC8" %in% variables_names){
      data <- data %>%
        dplyr::mutate(TPDISEC8 = as.character(.data$TPDISEC8)) %>%
        dplyr::mutate(TPDISEC8 = dplyr::case_match(
          .data$TPDISEC8,
          "0" ~ NA,
          "1" ~ "Pr\u00e9-existente",
          "2" ~ "Adquirido",
          .default = .data$TPDISEC8
        )) %>%
        dplyr::mutate(TPDISEC8 = as.factor(.data$TPDISEC8))
    }

    # TPDISEC9
    if("TPDISEC9" %in% variables_names){
      data <- data %>%
        dplyr::mutate(TPDISEC9 = as.character(.data$TPDISEC9)) %>%
        dplyr::mutate(TPDISEC9 = dplyr::case_match(
          .data$TPDISEC9,
          "0" ~ NA,
          "1" ~ "Pr\u00e9-existente",
          "2" ~ "Adquirido",
          .default = .data$TPDISEC9
        )) %>%
        dplyr::mutate(TPDISEC9 = as.factor(.data$TPDISEC9))
    }
  }

  # From data.table to tibble
  data <- tibble::as_tibble(data)

  # Purge levels
  data <- droplevels(data.table::as.data.table(data))

  # Unescape unicode characters
  data <- suppressWarnings(tibble::as_tibble(lapply(X = data, FUN = stringi::stri_unescape_unicode)))

  # Return
  return(data)

}
