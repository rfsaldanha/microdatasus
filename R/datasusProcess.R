# datasusProcess.R
# Pré-processamento de dados do DataSUS

datasusProcess <- function(data, sistema, dadosMunRes = TRUE){
  # Verifica sistema
  sisSIM <- c("SIM-DO","SIM-DOFET","SIM-DOEXT","SIM-DOINF","SIM-DOMAT")
  sistemas <- c("SIM",sisSIM,"SINASC","SIH-RD")
  if(!(sistema %in% sistemas)) stop("Sistema de informação desconhecido ou não implementado na função.")

  # Coleta nome dos campos
  campos <- names(data)

  # Trata campos

  ###################################################
  # SIM
  ###################################################

  if(sistema %in% c("SIM",sisSIM)){

    # Declara objetos
    ano <- NULL
    unidade <- NULL

    # CODMUNRES
    if("CODMUNRES" %in% campos & dadosMunRes == TRUE){
      data$CODMUNRES <- as.integer(as.character(data$CODMUNRES))
      colnames(tabMun)[1] <- "CODMUNRES"
      data <- dplyr::left_join(data, tabMun, by = "CODMUNRES")
    }

    # NUMERODO
    if("NUMERODO" %in% campos){
      data$NUMERODO <- as.character(data$NUMERODO)
    }

    # TIPOBITO.CNV
    if("TIPOBITO" %in% campos){
      data$TIPOBITO <- as.numeric(levels(data$TIPOBITO))[data$TIPOBITO]
      data$TIPOBITO[data$TIPOBITO==0] <- NA
      data$TIPOBITO[data$TIPOBITO==9] <- NA
      data$TIPOBITO[data$TIPOBITO==1] <- "Fetal"
      data$TIPOBITO[data$TIPOBITO==2] <- "Não Fetal"
      data$TIPOBITO <- factor(data$TIPOBITO)
    }

    # DTOBITO
    if("DTOBITO" %in% campos){
      data$DTOBITO <- as.character(data$DTOBITO)
      data$DTOBITO <- as.Date(data$DTOBITO, format = "%d%m%Y")
    }

    # HORAOBITO
    if("HORAOBITO" %in% campos){
      data$HORAOBITO <- as.character(data$HORAOBITO)
    }

    # CODCART
    if("CODCART" %in% campos){
      data$CODCART <- as.character(data$CODCART)
    }

    # CODMUNCART
    if("CODMUNCART" %in% campos){
      data$CODMUNCART <- as.numeric(data$CODMUNCART)
    }

    # NATURAL.CNV
    if("NATURAL" %in% campos){
      data$NATURAL <- as.character(data$NATURAL)
      colnames(tabNaturalidade)[1] <- "NATURAL"
      data$NATURAL <- factor(dplyr::left_join(data, tabNaturalidade, by = "NATURAL")$nome)
    }

    # DTNASC
    if("DTNASC" %in% campos){
      data$DTNASC <- as.character(data$DTNASC)
      data$DTNASC <- as.Date(data$DTNASC, format = "%d%m%Y")
    }

    # IDADE.CNV
    if("IDADE" %in% campos){
      data$IDADE <- as.character(data$IDADE)
      data$IDADE[data$IDADE=="000" | data$IDADE=="999"] <- NA
      unidade <- substr(data$IDADE,1,1)
      # Horas
      data$IDADEhoras <- as.numeric(ifelse(unidade == 1, substr(data$IDADE, 2,3), NA))
      # Dias
      data$IDADEdias <- as.numeric(ifelse(unidade == 2, substr(data$IDADE, 2,3), NA))
      # Meses
      data$IDADEmeses <- as.numeric(ifelse(unidade == 3, substr(data$IDADE, 2,3), NA))
      # Anos
      data$IDADEanos <- as.numeric(ifelse(unidade == 4, substr(data$IDADE, 2,3), ifelse(unidade == 5, 100 + as.numeric(substr(data$IDADE, 2,3)), NA)))
      # Apaga campo original
      data$IDADE <- NULL
    }

    # SEXO.CNV
    if("SEXO" %in% campos){
      data$SEXO <- as.numeric(levels(data$SEXO))[data$SEXO]
      data$SEXO[data$SEXO==0] <- NA
      data$SEXO[data$SEXO==9] <- NA
      data$SEXO[data$SEXO==1] <- "Masculino"
      data$SEXO[data$SEXO==2] <- "Feminino"
      data$SEXO <- factor(data$SEXO)
    }

    # RACACOR.CNV
    if("RACACOR" %in% campos){
      data$RACACOR <- as.numeric(levels(data$RACACOR))[data$RACACOR]
      data$RACACOR[data$RACACOR==0] <- NA
      data$RACACOR[data$RACACOR==1] <- "Branca"
      data$RACACOR[data$RACACOR==2] <- "Preta"
      data$RACACOR[data$RACACOR==3] <- "Amarela"
      data$RACACOR[data$RACACOR==4] <- "Parda"
      data$RACACOR[data$RACACOR==5] <- "Indígena"
      data$RACACOR[data$RACACOR==6] <- NA
      data$RACACOR[data$RACACOR==7] <- NA
      data$RACACOR[data$RACACOR==8] <- NA
      data$RACACOR[data$RACACOR==9] <- NA
      data$RACACOR <- factor(data$RACACOR)
    }

    # ESTCIV.CNV
    if("ESTCIV" %in% campos){
      data$ESTCIV <- as.numeric(levels(data$ESTCIV))[data$ESTCIV]
      data$ESTCIV[data$ESTCIV==0] <- NA
      data$ESTCIV[data$ESTCIV==1] <- "Solteiro"
      data$ESTCIV[data$ESTCIV==2] <- "Casado"
      data$ESTCIV[data$ESTCIV==3] <- "Viúvo"
      data$ESTCIV[data$ESTCIV==4] <- "Separado judicialmente"
      data$ESTCIV[data$ESTCIV==5] <- "União consensual"
      data$ESTCIV[data$ESTCIV==6] <- NA
      data$ESTCIV[data$ESTCIV==7] <- NA
      data$ESTCIV[data$ESTCIV==8] <- NA
      data$ESTCIV[data$ESTCIV==9] <- NA
      data$ESTCIV <- factor(data$ESTCIV)
    }

    # ESC (INSTRUC.CNV)
    if("ESC" %in% campos){
      data$ESC <- as.character(levels(data$ESC))[data$ESC]
      data$ESC[data$ESC=="0"] <- NA
      data$ESC[data$ESC=="6"] <- NA
      data$ESC[data$ESC=="7"] <- NA
      data$ESC[data$ESC=="9"] <- NA
      data$ESC[data$ESC=="A"] <- NA
      data$ESC[data$ESC=="1"] <- "Nenhuma"
      data$ESC[data$ESC=="2"] <- "1 a 3 anos"
      data$ESC[data$ESC=="3"] <- "4 a 7 anos"
      data$ESC[data$ESC=="4"] <- "8 a 11 anos"
      data$ESC[data$ESC=="5"] <- "12 anos ou mais"
      data$ESC[data$ESC=="8"] <- "De 9 a 11 anos"
      data$ESC[data$ESC=="9"] <- NA
      data$ESC <- factor(data$ESC)
    }

    # OCUP (OCUPA.CNV ou CBO2002.CNV)
    if("OCUP" %in% campos){
      if(!("DTOBITO" %in% campos)) stop("Necessário incluir o campo DTOBITO para o pré-processamento da variável OCUP.")
      data$OCUP <- as.character(data$OCUP)
      colnames(tabOcupacao)[1] <- "OCUP"
      colnames(tabCBO)[1] <- "OCUP"
      ano <- lubridate::year(data$DTOBITO)
      data$OCUP <- factor(ifelse(ano <= 2005, plyr::join(data, tabOcupacao, by = "OCUP", match = "first")$nome, dplyr::left_join(data, tabCBO, by = "OCUP")$nome))
    }

    # LOCOCOR.CNV
    if("LOCOCOR" %in% campos){
      data$LOCOCOR <- as.numeric(levels(data$LOCOCOR))[data$LOCOCOR]
      data$LOCOCOR[data$LOCOCOR==1] <- "Hospital"
      data$LOCOCOR[data$LOCOCOR==2] <- "Outro estabelecimento de saúde"
      data$LOCOCOR[data$LOCOCOR==3] <- "Domicílio"
      data$LOCOCOR[data$LOCOCOR==4] <- "Via pública"
      data$LOCOCOR[data$LOCOCOR==5] <- "Outros"
      data$LOCOCOR[data$LOCOCOR==9] <- NA
      data$LOCOCOR <- factor(data$LOCOCOR)
    }

    # IDADEMAE
    if("IDADEMAE" %in% campos){
      data$IDADEMAE <- as.numeric(data$IDADEMAE)
      data$IDADEMAE[data$IDADEMAE==0] <- NA
    }

    # ESCMAE.CNV
    if("ESCMAE" %in% campos){
      data$ESCMAE <- as.character(levels(data$ESCMAE))[data$ESCMAE]
      data$ESCMAE[data$ESCMAE=="0"] <- NA
      data$ESCMAE[data$ESCMAE=="6"] <- NA
      data$ESCMAE[data$ESCMAE=="7"] <- NA
      data$ESCMAE[data$ESCMAE=="9"] <- NA
      data$ESCMAE[data$ESCMAE=="A"] <- NA
      data$ESCMAE[data$ESCMAE=="1"] <- "Nenhuma"
      data$ESCMAE[data$ESCMAE=="2"] <- "1 a 3 anos"
      data$ESCMAE[data$ESCMAE=="3"] <- "4 a 7 anos"
      data$ESCMAE[data$ESCMAE=="4"] <- "8 a 11 anos"
      data$ESCMAE[data$ESCMAE=="5"] <- "12 anos ou mais"
      data$ESCMAE[data$ESCMAE=="8"] <- "De 9 a 11 anos"
      data$ESCMAE[data$ESCMAE=="9"] <- NA
      data$ESCMAE <- factor(data$ESCMAE)
    }

    # OCUPMAE (OCUPA.CNV ou CBO2002.CNV.)
    if("OCUPMAE" %in% campos){
      if(!("DTOBITO" %in% campos)) stop("Necessário incluir o campo DTOBITO para o pré-processamento da variável OCUP.")
      data$OCUPMAE <- as.character(data$OCUPMAE)
      colnames(tabOcupacao)[1] <- "OCUPMAE"
      colnames(tabCBO)[1] <- "OCUPMAE"
      ano <- lubridate::year(data$DTOBITO)
      data$OCUP <- factor(ifelse(ano <= 2005, plyr::join(data, tabOcupacao, by = "OCUPMAE", match = "first")$nome, dplyr::left_join(data, tabCBO, by = "OCUPMAE")$nome))
    }

    # QTDFILVIVO
    if("QTDFILVIVO" %in% campos){
      data$QTDFILVIVO <- as.numeric(data$QTDFILVIVO)
    }

    # QTDFILMORT
    if("QTDFILMORT" %in% campos){
      data$QTDFILMORT <- as.numeric(data$QTDFILMORT)
    }

    # GRAVIDEZ.CNV
    if("GRAVIDEZ" %in% campos){
      data$GRAVIDEZ <- as.numeric(levels(data$GRAVIDEZ))[data$GRAVIDEZ]
      data$GRAVIDEZ[data$GRAVIDEZ==1] <- "Única"
      data$GRAVIDEZ[data$GRAVIDEZ==2] <- "Dupla"
      data$GRAVIDEZ[data$GRAVIDEZ==3] <- "Tríplice e mais"
      data$GRAVIDEZ[data$GRAVIDEZ==9] <- NA
      data$GRAVIDEZ <- factor(data$GRAVIDEZ)
    }

    # GESTACAO.CNV
    if("GESTACAO" %in% campos){
      data$GESTACAO <- as.character(levels(data$GESTACAO))[data$GESTACAO]
      data$GESTACAO[data$GESTACAO=="0"] <- NA
      data$GESTACAO[data$GESTACAO=="A"] <- "21 a 27 semanas"
      data$GESTACAO[data$GESTACAO=="1"] <- "Menos de 22 semanas"
      data$GESTACAO[data$GESTACAO=="2"] <- "22 a 27 semanas"
      data$GESTACAO[data$GESTACAO=="3"] <- "28 a 31 semanas"
      data$GESTACAO[data$GESTACAO=="4"] <- "32 a 36 semanas"
      data$GESTACAO[data$GESTACAO=="5"] <- "37 a 41 semanas"
      data$GESTACAO[data$GESTACAO=="6"] <- "42 semanas e mais"
      data$GESTACAO[data$GESTACAO=="7"] <- "28 semanas e mais"
      data$GESTACAO[data$GESTACAO=="8"] <- "28 a 36 semanas"
      data$GESTACAO[data$GESTACAO=="9"] <- NA
      data$GESTACAO <- factor(data$GESTACAO)
    }

    # PARTO.CNV
    if("PARTO" %in% campos){
      data$PARTO <- as.numeric(levels(data$PARTO))[data$PARTO]
      data$PARTO[data$PARTO==0] <- NA
      data$PARTO[data$PARTO==1] <- "Vaginal"
      data$PARTO[data$PARTO==2] <- "Cesáreo"
      data$PARTO[data$PARTO==3] <- NA
      data$PARTO[data$PARTO==4] <- NA
      data$PARTO[data$PARTO==5] <- NA
      data$PARTO[data$PARTO==6] <- NA
      data$PARTO[data$PARTO==7] <- NA
      data$PARTO[data$PARTO==8] <- NA
      data$PARTO[data$PARTO==9] <- NA
      data$PARTO <- factor(data$PARTO)
    }

    # OBITOPARTO.CNV
    if("OBITOPARTO" %in% campos){
      data$OBITOPARTO <- as.numeric(levels(data$OBITOPARTO))[data$OBITOPARTO]
      data$OBITOPARTO[data$OBITOPARTO==0] <- NA
      data$OBITOPARTO[data$OBITOPARTO==1] <- "Antes"
      data$OBITOPARTO[data$OBITOPARTO==2] <- "Durante"
      data$OBITOPARTO[data$OBITOPARTO==3] <- "Depois"
      data$OBITOPARTO[data$OBITOPARTO==4] <- NA
      data$OBITOPARTO[data$OBITOPARTO==5] <- NA
      data$OBITOPARTO[data$OBITOPARTO==6] <- NA
      data$OBITOPARTO[data$OBITOPARTO==7] <- NA
      data$OBITOPARTO[data$OBITOPARTO==8] <- NA
      data$OBITOPARTO[data$OBITOPARTO==9] <- NA
      data$OBITOPARTO <- factor(data$OBITOPARTO)
    }

    # PESO.CNV
    if("PESO" %in% campos){
      data$PESO <- as.numeric(data$PESO)
      data$PESO[data$PESO==0] <- NA
    }

    # NUMERODN
    if("NUMERODN" %in% campos){
      data$NUMERODN <- as.numeric(data$NUMERODN)
    }

    # CODESTAB
    if("CODESTAB" %in% campos){
      data$CODESTAB <- as.character(data$CODESTAB)
    }

    # OBITOGRAV.CNV
    if("OBITOGRAV" %in% campos){
      data$OBITOGRAV <- as.numeric(levels(data$OBITOGRAV))[data$OBITOGRAV]
      data$OBITOGRAV[data$OBITOGRAV==1] <- "Sim"
      data$OBITOGRAV[data$OBITOGRAV==2] <- "Não"
      data$OBITOGRAV[data$OBITOGRAV==3] <- NA
      data$OBITOGRAV[data$OBITOGRAV==4] <- NA
      data$OBITOGRAV[data$OBITOGRAV==5] <- NA
      data$OBITOGRAV[data$OBITOGRAV==6] <- NA
      data$OBITOGRAV[data$OBITOGRAV==7] <- NA
      data$OBITOGRAV[data$OBITOGRAV==8] <- NA
      data$OBITOGRAV[data$OBITOGRAV==9] <- NA
      data$OBITOGRAV <- factor(data$OBITOGRAV)
    }

    # OBITOPUERP.CNV
    if("OBITOPUERP" %in% campos){
      data$OBITOPUERP <- as.numeric(levels(data$OBITOPUERP))[data$OBITOPUERP]
      data$OBITOPUERP[data$OBITOPUERP==1] <- "De 0 a 42 dias"
      data$OBITOPUERP[data$OBITOPUERP==2] <- "De 43 dias a 1 ano"
      data$OBITOPUERP[data$OBITOPUERP==3] <- "Não"
      data$OBITOPUERP[data$OBITOPUERP==4] <- NA
      data$OBITOPUERP[data$OBITOPUERP==5] <- NA
      data$OBITOPUERP[data$OBITOPUERP==6] <- NA
      data$OBITOPUERP[data$OBITOPUERP==7] <- NA
      data$OBITOPUERP[data$OBITOPUERP==8] <- NA
      data$OBITOPUERP[data$OBITOPUERP==9] <- NA
      data$OBITOPUERP <- factor(data$OBITOPUERP)
    }

    # ASSISTMED.CNV
    if("ASSISTMED" %in% campos){
      data$ASSISTMED <- as.numeric(levels(data$ASSISTMED))[data$ASSISTMED]
      data$ASSISTMED[data$ASSISTMED==1] <- "Sim"
      data$ASSISTMED[data$ASSISTMED==2] <- "Não"
      data$ASSISTMED[data$ASSISTMED==9] <- NA
      data$ASSISTMED <- factor(data$ASSISTMED)
    }

    # EXAME.CNV
    if("EXAME" %in% campos){
      data$EXAME <- as.numeric(levels(data$EXAME))[data$EXAME]
      data$EXAME[data$EXAME==1] <- "Sim"
      data$EXAME[data$EXAME==2] <- "Não"
      data$EXAME[data$EXAME==9] <- NA
      data$EXAME <- factor(data$EXAME)
    }

    # CIRURGIA.CNV
    if("CIRURGIA" %in% campos){
      data$CIRURGIA <- as.numeric(levels(data$CIRURGIA))[data$CIRURGIA]
      data$CIRURGIA[data$CIRURGIA==1] <- "Sim"
      data$CIRURGIA[data$CIRURGIA==2] <- "Não"
      data$CIRURGIA[data$CIRURGIA==9] <- NA
      data$CIRURGIA <- factor(data$CIRURGIA)
    }

    # NECROPSIA (NECROPS.CNV)
    if("NECROPSIA" %in% campos){
      data$NECROPSIA <- as.numeric(levels(data$NECROPSIA))[data$NECROPSIA]
      data$NECROPSIA[data$NECROPSIA==1] <- "Sim"
      data$NECROPSIA[data$NECROPSIA==2] <- "Não"
      data$NECROPSIA[data$NECROPSIA==9] <- NA
      data$NECROPSIA <- factor(data$NECROPSIA)
    }

    # DTATESTADO
    if("DTATESTADO" %in% campos){
      data$DTATESTADO <- as.character(data$DTATESTADO)
      data$DTATESTADO <- as.Date(data$DTATESTADO, format = "%d%m%Y")
    }

    # CIRCOBITO (TIPOVIOL.CNV)
    if("CIRCOBITO" %in% campos){
      data$CIRCOBITO <- as.numeric(levels(data$CIRCOBITO))[data$CIRCOBITO]
      data$CIRCOBITO[data$CIRCOBITO==0] <- NA
      data$CIRCOBITO[data$CIRCOBITO==1] <- "Acidente"
      data$CIRCOBITO[data$CIRCOBITO==2] <- "Suicídio"
      data$CIRCOBITO[data$CIRCOBITO==3] <- "Homicídio"
      data$CIRCOBITO[data$CIRCOBITO==4] <- "Outro"
      data$CIRCOBITO[data$CIRCOBITO==5] <- NA
      data$CIRCOBITO[data$CIRCOBITO==6] <- NA
      data$CIRCOBITO[data$CIRCOBITO==7] <- NA
      data$CIRCOBITO[data$CIRCOBITO==8] <- NA
      data$CIRCOBITO[data$CIRCOBITO==9] <- NA
      data$CIRCOBITO <- factor(data$CIRCOBITO)
    }

    # ACIDTRAB.CNV
    if("ACIDTRAB" %in% campos){
      data$ACIDTRAB <- as.numeric(levels(data$ACIDTRAB))[data$ACIDTRAB]
      data$ACIDTRAB[data$ACIDTRAB==1] <- "Sim"
      data$ACIDTRAB[data$ACIDTRAB==2] <- "Não"
      data$ACIDTRAB[data$ACIDTRAB==9] <- NA
      data$ACIDTRAB <- factor(data$ACIDTRAB)
    }

    # FONTE (FONTINFO.CNV)
    if("FONTE" %in% campos){
      data$FONTE <- as.numeric(levels(data$FONTE))[data$FONTE]
      data$FONTE[data$FONTE==1] <- "Boletim de Ocorrência"
      data$FONTE[data$FONTE==2] <- "Hospital"
      data$FONTE[data$FONTE==3] <- "Família"
      data$FONTE[data$FONTE==4] <- "Outro"
      data$FONTE[data$FONTE==9] <- NA
      data$FONTE <- factor(data$FONTE)
    }

    # TPPOS (INVESTIG.CNV)
    if("TPPOS" %in% campos){
      data$TPPOS <- plyr::revalue(data$TPPOS, c("N"="Não investigado", "S"="Investigado"))
    }

    # DTINVESTIG
    if("DTINVESTIG" %in% campos){
      data$DTINVESTIG <- as.character(data$DTINVESTIG)
      data$DTINVESTIG <- as.Date(data$DTINVESTIG, format = "%d%m%Y")
    }

    # DTCADASTRO
    if("DTCADASTRO" %in% campos){
      data$DTCADASTRO <- as.character(data$DTCADASTRO)
      data$DTCADASTRO <- as.Date(data$DTCADASTRO, format = "%d%m%Y")
    }

    # ATESTANTE (ATESTANT.CNV)
    if("ATESTANTE" %in% campos){
      data$ATESTANTE <- as.numeric(levels(data$ATESTANTE))[data$ATESTANTE]
      data$ATESTANTE[data$ATESTANTE==0] <- NA
      data$ATESTANTE[data$ATESTANTE==1] <- "Sim"
      data$ATESTANTE[data$ATESTANTE==2] <- "Substituto"
      data$ATESTANTE[data$ATESTANTE==3] <- "IML"
      data$ATESTANTE[data$ATESTANTE==4] <- "SVO"
      data$ATESTANTE[data$ATESTANTE==5] <- "Outro"
      data$ATESTANTE[data$ATESTANTE==6] <- NA
      data$ATESTANTE[data$ATESTANTE==7] <- NA
      data$ATESTANTE[data$ATESTANTE==8] <- NA
      data$ATESTANTE[data$ATESTANTE==9] <- NA
      data$ATESTANTE <- factor(data$ATESTANTE)
    }

    # FONTEINV.CNV
    if("FONTEINV" %in% campos){
      data$FONTEINV <- as.numeric(levels(data$FONTEINV))[data$FONTEINV]
      data$FONTEINV[data$FONTEINV==1] <- "Comitê de Mortalidade Materna e/ou Infantil"
      data$FONTEINV[data$FONTEINV==2] <- "Visita familiar / Entrevista família"
      data$FONTEINV[data$FONTEINV==3] <- "Estabelecimento de saúde / Prontuário"
      data$FONTEINV[data$FONTEINV==4] <- "Relacionamento com outros bancos de dados"
      data$FONTEINV[data$FONTEINV==5] <- "SVO"
      data$FONTEINV[data$FONTEINV==6] <- "IML"
      data$FONTEINV[data$FONTEINV==7] <- "Outra fonte"
      data$FONTEINV[data$FONTEINV==8] <- "Múltiplas fontes"
      data$FONTEINV[data$FONTEINV==9] <- NA
      data$FONTEINV <- factor(data$FONTEINV)
    }

    # DTRECEBIM
    if("DTRECEBIM" %in% campos){
      data$DTRECEBIM <- as.character(data$DTRECEBIM)
      data$DTRECEBIM <- as.Date(data$DTRECEBIM, format = "%d%m%Y")
    }

    # UFINFORM (UF.CNV)
    if("UFINFORM" %in% campos){
      data$UFINFORM <- as.numeric(levels(data$UFINFORM))[data$UFINFORM]
      data$UFINFORM[data$UFINFORM==0] <- NA
      data$UFINFORM[data$UFINFORM==11] <- "Rondônia"
      data$UFINFORM[data$UFINFORM==12] <- "Acre"
      data$UFINFORM[data$UFINFORM==13] <- "Amazonas"
      data$UFINFORM[data$UFINFORM==14] <- "Roraima"
      data$UFINFORM[data$UFINFORM==15] <- "Pará"
      data$UFINFORM[data$UFINFORM==16] <- "Amapá"
      data$UFINFORM[data$UFINFORM==17] <- "Tocantins"
      data$UFINFORM[data$UFINFORM==21] <- "Maranhão"
      data$UFINFORM[data$UFINFORM==22] <- "Piauí"
      data$UFINFORM[data$UFINFORM==23] <- "Ceará"
      data$UFINFORM[data$UFINFORM==24] <- "Rio Grande do Norte"
      data$UFINFORM[data$UFINFORM==25] <- "Paraíba"
      data$UFINFORM[data$UFINFORM==26] <- "Pernambuco"
      data$UFINFORM[data$UFINFORM==20] <- "Pernambuco"
      data$UFINFORM[data$UFINFORM==27] <- "Alagoas"
      data$UFINFORM[data$UFINFORM==28] <- "Sergipe"
      data$UFINFORM[data$UFINFORM==29] <- "Bahia"
      data$UFINFORM[data$UFINFORM==31] <- "Minas Gerais"
      data$UFINFORM[data$UFINFORM==32] <- "Espírito Santo"
      data$UFINFORM[data$UFINFORM==33] <- "Rio de Janeiro"
      data$UFINFORM[data$UFINFORM==35] <- "São Paulo"
      data$UFINFORM[data$UFINFORM==41] <- "Paraná"
      data$UFINFORM[data$UFINFORM==42] <- "Santa Catarina"
      data$UFINFORM[data$UFINFORM==43] <- "Rio Grande do Sul"
      data$UFINFORM[data$UFINFORM==50] <- "Mato Grosso do Sul"
      data$UFINFORM[data$UFINFORM==51] <- "Mato Grosso"
      data$UFINFORM[data$UFINFORM==52] <- "Goiás"
      data$UFINFORM[data$UFINFORM==53] <- "Distrito Federal"
      data$UFINFORM[data$UFINFORM==99] <- NA
      data$UFINFORM <- factor(data$UFINFORM)
    }

    # CODINST.CNV
    if("CODINST" %in% campos){
      data$CODINST <- as.character(levels(data$CODINST))[data$CODINST]
      data$CODINST[data$CODINST=="E"] <- "Estado"
      data$CODINST[data$CODINST=="R"] <- "Regional"
      data$CODINST[data$CODINST=="M"] <- "Municipal"
      data$CODINST <- factor(data$CODINST)
    }







  ###################################################
  # SINASC
  ###################################################

  } else if(sistema == "SINASC"){

    # CODMUNRES
    if("CODMUNRES" %in% campos & dadosMunRes == TRUE){
      data$CODMUNRES <- as.integer(as.character(data$CODMUNRES))
      colnames(tabMun)[1] <- "CODMUNRES"
      data <- dplyr::left_join(data, tabMun, by = "CODMUNRES")
    }

    # NUMERODN
    if("NUMERODN" %in% campos){
      data$NUMERODN <- as.numeric(data$NUMERODN)
    }

    # LOCNASC
    if("LOCNASC" %in% campos){
      data$LOCNASC <- as.numeric(levels(data$LOCNASC))[data$LOCNASC]
      data$LOCNASC[data$LOCNASC==1] <- "Hospital"
      data$LOCNASC[data$LOCNASC==2] <- "Outro estabelecimento de saúde"
      data$LOCNASC[data$LOCNASC==3] <- "Domicílio"
      data$LOCNASC[data$LOCNASC==4] <- "Outros"
      data$LOCNASC[data$LOCNASC==5] <- NA
      data$LOCNASC[data$LOCNASC==6] <- NA
      data$LOCNASC[data$LOCNASC==7] <- NA
      data$LOCNASC[data$LOCNASC==8] <- NA
      data$LOCNASC[data$LOCNASC==9] <- NA
      data$LOCNASC <- factor(data$LOCNASC)
    }

    # IDADEMAE
    if("IDADEMAE" %in% campos){
      data$IDADEMAE <- as.numeric(data$IDADEMAE)
      data$IDADEMAE[data$IDADEMAE==0] <- NA
      data$IDADEMAE[data$IDADEMAE==99] <- NA
    }

    # ESTCIVMAE
    if("ESTCIVMAE" %in% campos){
      data$ESTCIVMAE <- as.numeric(levels(data$ESTCIVMAE))[data$ESTCIVMAE]
      data$ESTCIVMAE[data$ESTCIVMAE==0] <- NA
      data$ESTCIVMAE[data$ESTCIVMAE==1] <- "Solteira"
      data$ESTCIVMAE[data$ESTCIVMAE==2] <- "Casada"
      data$ESTCIVMAE[data$ESTCIVMAE==3] <- "Viúva"
      data$ESTCIVMAE[data$ESTCIVMAE==4] <- "Separado judicialmente"
      data$ESTCIVMAE[data$ESTCIVMAE==5] <- "União Consensual"
      data$ESTCIVMAE[data$ESTCIVMAE==6] <- NA
      data$ESTCIVMAE[data$ESTCIVMAE==7] <- NA
      data$ESTCIVMAE[data$ESTCIVMAE==8] <- NA
      data$ESTCIVMAE[data$ESTCIVMAE==9] <- NA
      data$ESTCIVMAE <- factor(data$ESTCIVMAE)
    }

    # ESCMAE
    if("ESCMAE" %in% campos){
      data$ESCMAE <- as.numeric(levels(data$ESCMAE))[data$ESCMAE]
      data$ESCMAE[data$ESCMAE==1] <- "Nenhuma"
      data$ESCMAE[data$ESCMAE==2] <- "1 a 3 anos"
      data$ESCMAE[data$ESCMAE==3] <- "4 a 7 anos"
      data$ESCMAE[data$ESCMAE==4] <- "8 a 11 anos"
      data$ESCMAE[data$ESCMAE==5] <- "12 e mais"
      data$ESCMAE[data$ESCMAE==6] <- NA
      data$ESCMAE[data$ESCMAE==7] <- NA
      data$ESCMAE[data$ESCMAE==8] <- "9 a 11"
      data$ESCMAE[data$ESCMAE==9] <- NA
      data$ESCMAE <- factor(data$ESCMAE)
    }

    # QTDFILVIVO
    if("QTDFILVIVO" %in% campos){
      data$QTDFILVIVO <- as.numeric(data$QTDFILVIVO)
      data$QTDFILVIVO[data$QTDFILVIVO==99] <- NA
    }

    # QTDFILMORT
    if("QTDFILMORT" %in% campos){
      data$QTDFILMORT <- as.numeric(data$QTDFILMORT)
      data$QTDFILMORT[data$QTDFILMORT==99] <- NA
    }

    # GESTACAO
    if("GESTACAO" %in% campos){
      data$GESTACAO <- as.numeric(levels(data$GESTACAO))[data$GESTACAO]
      data$GESTACAO[data$GESTACAO==0] <- NA
      data$GESTACAO[data$GESTACAO==1] <- "Menos de 22 semanas"
      data$GESTACAO[data$GESTACAO==2] <- "22 a 27 semanas"
      data$GESTACAO[data$GESTACAO==3] <- "28 a 31 semanas"
      data$GESTACAO[data$GESTACAO==4] <- "32 a 36 semanas"
      data$GESTACAO[data$GESTACAO==5] <- "37 a 41 semanas"
      data$GESTACAO[data$GESTACAO==6] <- "42 semanas e mais"
      data$GESTACAO[data$GESTACAO==7] <- NA
      data$GESTACAO[data$GESTACAO==8] <- NA
      data$GESTACAO[data$GESTACAO==9] <- NA
      data$GESTACAO <- factor(data$GESTACAO)
    }

    # GRAVIDEZ
    if("GRAVIDEZ" %in% campos){
      data$GRAVIDEZ <- as.numeric(levels(data$GRAVIDEZ))[data$GRAVIDEZ]
      data$GRAVIDEZ[data$GRAVIDEZ==0] <- NA
      data$GRAVIDEZ[data$GRAVIDEZ==1] <- "Única"
      data$GRAVIDEZ[data$GRAVIDEZ==2] <- "Dupla"
      data$GRAVIDEZ[data$GRAVIDEZ==3] <- "Tripla e mais"
      data$GRAVIDEZ[data$GRAVIDEZ==4] <- NA
      data$GRAVIDEZ[data$GRAVIDEZ==5] <- NA
      data$GRAVIDEZ[data$GRAVIDEZ==6] <- NA
      data$GRAVIDEZ[data$GRAVIDEZ==7] <- NA
      data$GRAVIDEZ[data$GRAVIDEZ==8] <- NA
      data$GRAVIDEZ[data$GRAVIDEZ==9] <- NA
      data$GRAVIDEZ <- factor(data$GRAVIDEZ)
    }

    # PARTO
    if("PARTO" %in% campos){
      data$PARTO <- as.numeric(levels(data$PARTO))[data$PARTO]
      data$PARTO[data$PARTO==0] <- NA
      data$PARTO[data$PARTO==1] <- "Vaginal"
      data$PARTO[data$PARTO==2] <- "Cesáreo"
      data$PARTO[data$PARTO==3] <- NA
      data$PARTO[data$PARTO==4] <- NA
      data$PARTO[data$PARTO==5] <- NA
      data$PARTO[data$PARTO==6] <- NA
      data$PARTO[data$PARTO==7] <- NA
      data$PARTO[data$PARTO==8] <- NA
      data$PARTO[data$PARTO==9] <- NA
      data$PARTO <- factor(data$PARTO)
    }

    # CONSULTAS
    if("CONSULTAS" %in% campos){
      data$CONSULTAS <- as.numeric(levels(data$CONSULTAS))[data$CONSULTAS]
      data$CONSULTAS[data$CONSULTAS==0] <- NA
      data$CONSULTAS[data$CONSULTAS==1] <- "Nenhuma"
      data$CONSULTAS[data$CONSULTAS==2] <- "de 1 a 3"
      data$CONSULTAS[data$CONSULTAS==3] <- "de 4 a 6"
      data$CONSULTAS[data$CONSULTAS==4] <- "7 e mais"
      data$CONSULTAS[data$CONSULTAS==5] <- NA
      data$CONSULTAS[data$CONSULTAS==6] <- NA
      data$CONSULTAS[data$CONSULTAS==7] <- NA
      data$CONSULTAS[data$CONSULTAS==8] <- NA
      data$CONSULTAS[data$CONSULTAS==9] <- NA
      data$CONSULTAS <- factor(data$CONSULTAS)
    }

    # DTNASC
    if("DTNASC" %in% campos){
      data$DTNASC <- as.character(data$DTNASC)
      data$DTNASC <- as.Date(data$DTNASC, format = "%d%m%Y")
    }

    # HORANASC
    if("HORANASC" %in% campos){
      data$HORANASC <- as.character(data$HORANASC)
    }

    # SEXO
    if("SEXO" %in% campos){
      data$SEXO <- as.numeric(levels(data$SEXO))[data$SEXO]
      data$SEXO[data$SEXO==0] <- NA
      data$SEXO[data$SEXO==1] <- "Masculino"
      data$SEXO[data$SEXO==2] <- "Feminino"
      data$SEXO[data$SEXO==9] <- NA
      data$SEXO <- factor(data$SEXO)
    }

    # APGAR1
    if("APGAR1" %in% campos){
      data$APGAR1 <- as.numeric(data$APGAR1)
      data$APGAR1[data$APGAR1==99] <- NA
    }

    # APGAR5
    if("APGAR5" %in% campos){
      data$APGAR5 <- as.numeric(data$APGAR5)
      data$APGAR5[data$APGAR5==99] <- NA
    }

    # RACACOR
    if("RACACOR" %in% campos){
      data$RACACOR <- as.numeric(levels(data$RACACOR))[data$RACACOR]
      data$RACACOR[data$RACACOR==1] <- "Branca"
      data$RACACOR[data$RACACOR==2] <- "Preta"
      data$RACACOR[data$RACACOR==3] <- "Amarela"
      data$RACACOR[data$RACACOR==4] <- "Parda"
      data$RACACOR[data$RACACOR==5] <- "Indígena"
      data$RACACOR <- factor(data$RACACOR)
    }

    # PESO
    if("PESO" %in% campos){
      data$PESO <- as.numeric(data$PESO)
      data$PESO[data$PESO==0] <- NA
      data$PESO[data$PESO==9999] <- NA
    }

    # IDANOMAL
    if("IDANOMAL" %in% campos){
      data$IDANOMAL <- as.numeric(levels(data$IDANOMAL))[data$IDANOMAL]
      data$IDANOMAL[data$IDANOMAL==1] <- "Sim"
      data$IDANOMAL[data$IDANOMAL==2] <- "Não"
      data$IDANOMAL[data$IDANOMAL==3] <- NA
      data$IDANOMAL[data$IDANOMAL==4] <- NA
      data$IDANOMAL[data$IDANOMAL==5] <- NA
      data$IDANOMAL[data$IDANOMAL==6] <- NA
      data$IDANOMAL[data$IDANOMAL==7] <- NA
      data$IDANOMAL[data$IDANOMAL==8] <- NA
      data$IDANOMAL[data$IDANOMAL==9] <- NA
      data$IDANOMAL <- factor(data$IDANOMAL)
    }

    # DTCADASTRO
    if("DTCADASTRO" %in% campos){
      data$DTCADASTRO <- as.character(data$DTCADASTRO)
      data$DTCADASTRO <- as.Date(data$DTCADASTRO, format = "%d%m%Y")
    }

    # DTRECEBIM
    if("DTRECEBIM" %in% campos){
      data$DTRECEBIM <- as.character(data$DTRECEBIM)
      data$DTRECEBIM <- as.Date(data$DTRECEBIM, format = "%d%m%Y")
    }
  }











  ###################################################
  # SIH-RD
  ###################################################

  if(sistema == "SIH-RD"){

    # UF_ZI
    if("UF_ZI" %in% campos){
      data$UF_ZI <- as.character(data$UF_ZI)
    }

    # ANO_CMPT
    if("ANO_CMPT" %in% campos){
      data$ANO_CMPT <- as.integer(data$ANO_CMPT)
    }

    # MES_CMPT
    if("MES_CMPT" %in% campos){
      data$MES_CMPT <- as.integer(data$MES_CMPT)
    }

    # ESPEC
    if("ESPEC" %in% campos){
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
    if("CGC_HOSP" %in% campos){
      data$CGC_HOSP <- as.character(data$CGC_HOSP)
    }

    # N_AIH
    if("N_AIH" %in% campos){
      data$N_AIH <- as.character(data$N_AIH)
    }

    # IDENT
    if("IDENT" %in% campos){
      data$IDENT <- as.numeric(levels(data$IDENT))[data$IDENT]
      data$IDENT[data$IDENT==1] <- "Principal"
      data$IDENT[data$IDENT==3] <- "Continuação"
      data$IDENT[data$IDENT==5] <- "Longa permanência"
      data$IDENT <- factor(data$IDENT)
    }

    # CEP
    if("CEP" %in% campos){
      data$CEP <- as.character(data$CEP)
    }

    # MUNIC_RES
    if("MUNIC_RES" %in% campos & dadosMunRes == TRUE){
      data$MUNIC_RES <- as.integer(as.character(data$MUNIC_RES))
      colnames(tabMun)[1] <- "MUNIC_RES"
      data <- dplyr::left_join(data, tabMun, by = "MUNIC_RES")
    }

    # NASC
    if("NASC" %in% campos){
      data$NASC <- as.character(data$NASC)
      data$NASC <- as.Date(data$NASC, format = "%Y%m%d")
    }

    # SEXO
    if("SEXO" %in% campos){
      data$SEXO <- as.numeric(levels(data$SEXO))[data$SEXO]
      data$SEXO[data$SEXO==1] <- "Masculino"
      data$SEXO[data$SEXO==2] <- "Feminino"
      data$SEXO[data$SEXO==3] <- "Feminino"
      data$SEXO <- factor(data$SEXO)
    }

    # UTI_MES_IN
    if("UTI_MES_IN" %in% campos){
      data$UTI_MES_IN <- as.integer(data$UTI_MES_IN)
    }

    # UTI_MES_AN
    if("UTI_MES_AN" %in% campos){
      data$UTI_MES_AN <- as.integer(data$UTI_MES_AN)
    }

    # UTI_MES_AL
    if("UTI_MES_AL" %in% campos){
      data$UTI_MES_AL <- as.integer(data$UTI_MES_AL)
    }

    # UTI_MES_TO
    if("UTI_MES_TO" %in% campos){
      data$UTI_MES_TO <- as.integer(data$UTI_MES_TO)
    }

    # MARCA_UTI
    if("MARCA_UTI" %in% campos){
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
    if("UTI_INT_IN" %in% campos){
      data$UTI_INT_IN <- as.integer(data$UTI_INT_IN)
    }

    # UTI_INT_AN
    if("UTI_INT_AN" %in% campos){
      data$UTI_INT_AN <- as.integer(data$UTI_INT_AN)
    }

    # UTI_INT_AL
    if("UTI_INT_AL" %in% campos){
      data$UTI_INT_AL <- as.integer(data$UTI_INT_AL)
    }

    # UTI_INT_TO
    if("UTI_INT_TO" %in% campos){
      data$UTI_INT_TO <- as.integer(data$UTI_INT_TO)
    }

    # DIAR_ACOM
    if("DIAR_ACOM" %in% campos){
      data$DIAR_ACOM <- as.integer(data$DIAR_ACOM)
    }

    # QT_DIARIAS
    if("QT_DIARIAS" %in% campos){
      data$QT_DIARIAS <- as.integer(data$QT_DIARIAS)
    }

    # PROC_SOLIC
    if("PROC_SOLIC" %in% campos){
      data$PROC_SOLIC <- as.character(data$PROC_SOLIC)
    }

    # PROC_REA
    if("PROC_REA" %in% campos){
      data$PROC_REA <- as.character(data$PROC_REA)
    }

    # PROC_REA
    if("PROC_REA" %in% campos){
      data$PROC_REA <- as.character(data$PROC_REA)
    }

    # VAL_SH
    if("VAL_SH" %in% campos){
      data$VAL_SH <- as.numeric(data$VAL_SH)
    }

    # VAL_SP
    if("VAL_SP" %in% campos){
      data$VAL_SP <- as.numeric(data$VAL_SP)
    }

    # VAL_SADT
    if("VAL_SADT" %in% campos){
      data$VAL_SADT <- as.numeric(data$VAL_SADT)
    }

    # VAL_RN
    if("VAL_RN" %in% campos){
      data$VAL_RN <- as.numeric(data$VAL_RN)
    }

    # VAL_ACOMP
    if("VAL_ACOMP" %in% campos){
      data$VAL_RN <- as.numeric(data$VAL_ACOMP)
    }

    # VAL_ORTP
    if("VAL_ORTP" %in% campos){
      data$VAL_ORTP <- as.numeric(data$VAL_ORTP)
    }

    # VAL_SANGUE
    if("VAL_SANGUE" %in% campos){
      data$VAL_SANGUE <- as.numeric(data$VAL_SANGUE)
    }

    # VAL_SADTSR
    if("VAL_SADTSR" %in% campos){
      data$VAL_SADTSR <- as.numeric(data$VAL_SADTSR)
    }

    # VAL_TRANSP
    if("VAL_TRANSP" %in% campos){
      data$VAL_TRANSP <- as.numeric(data$VAL_TRANSP)
    }

    # VAL_OBSANG
    if("VAL_OBSANG" %in% campos){
      data$VAL_OBSANG <- as.numeric(data$VAL_OBSANG)
    }

    # VAL_PED1AC
    if("VAL_PED1AC" %in% campos){
      data$VAL_PED1AC <- as.numeric(data$VAL_PED1AC)
    }

    # VAL_TOT
    if("VAL_TOT" %in% campos){
      data$VAL_TOT <- as.numeric(data$VAL_TOT)
    }

    # VAL_UTI
    if("VAL_UTI" %in% campos){
      data$VAL_UTI <- as.numeric(data$VAL_UTI)
    }

    # US_TOT
    if("US_TOT" %in% campos){
      data$US_TOT <- as.numeric(data$US_TOT)
    }

    # DT_INTER
    if("DT_INTER" %in% campos){
      data$DT_INTER <- as.character(data$DT_INTER)
      data$DT_INTER <- as.Date(data$DT_INTER, format = "%Y%m%d")
    }

    # DT_SAIDA
    if("DT_SAIDA" %in% campos){
      data$DT_SAIDA <- as.character(data$DT_SAIDA)
      data$DT_SAIDA <- as.Date(data$DT_SAIDA, format = "%Y%m%d")
    }

    # DIAG_PRINC
    if("DIAG_PRINC" %in% campos){
      data$DIAG_PRINC <- as.character(data$DIAG_PRINC)
    }

    # DIAG_SECUN
    if("DIAG_SECUN" %in% campos){
      data$DIAG_SECUN <- as.character(data$DIAG_SECUN)
    }

    # COBRANCA (motivo de saída/permanência, portaria SAS 719)
    if("COBRANCA" %in% campos){
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

    # GESTAO
    if("GESTAO" %in% campos){
      data$GESTAO <- as.numeric(levels(data$GESTAO))[data$GESTAO]
      data$GESTAO[data$GESTAO==0] <- "Gestão estadual"
      data$GESTAO[data$GESTAO==2] <- "Gestão plena municipal"
      data$GESTAO[data$GESTAO==3] <- "Gestão plena estadual"
      data$GESTAO <- factor(data$GESTAO)
    }

    # IND_VDRL
    if("IND_VDRL" %in% campos){
      data$IND_VDRL <- as.numeric(levels(data$IND_VDRL))[data$IND_VDRL]
      data$IND_VDRL[data$IND_VDRL==0] <- "Não"
      data$IND_VDRL[data$IND_VDRL==1] <- "Sim"
      data$IND_VDRL <- factor(data$IND_VDRL)
    }

    # COD_IDADE
    if("COD_IDADE" %in% campos){
      data$COD_IDADE <- as.numeric(levels(data$COD_IDADE))[data$COD_IDADE]
      data$COD_IDADE[data$COD_IDADE==0] <- NA
      data$COD_IDADE[data$COD_IDADE==2] <- "Dias"
      data$COD_IDADE[data$COD_IDADE==3] <- "Meses"
      data$COD_IDADE[data$COD_IDADE==4] <- "Anos"
      data$COD_IDADE <- factor(data$COD_IDADE)
    }

    # MORTE
    if("MORTE" %in% campos){
      data$MORTE[data$MORTE==0] <- "Não"
      data$MORTE[data$MORTE==1] <- "Sim"
      data$MORTE <- factor(data$MORTE)
    }

    # INSTRU
    if("INSTRU" %in% campos){
      data$INSTRU <- as.numeric(levels(data$INSTRU))[data$INSTRU]
      data$INSTRU[data$INSTRU==1] <- "Analfabeto"
      data$INSTRU[data$INSTRU==2] <- "Primeiro grau"
      data$INSTRU[data$INSTRU==3] <- "Segundo grau"
      data$INSTRU[data$INSTRU==4] <- "Terceiro grau"
      data$INSTRU[data$INSTRU==0] <- NA
      data$INSTRU <- factor(data$INSTRU)
    }

    # CONTRACEP1
    if("CONTRACEP1" %in% campos){
      data$CONTRACEP1 <- as.numeric(levels(data$CONTRACEP1))[data$CONTRACEP1]
      data$CONTRACEP1[data$CONTRACEP1==1] <- "LAM (amamentação)"
      data$CONTRACEP1[data$CONTRACEP1==2] <- "Ogino Kaus"
      data$CONTRACEP1[data$CONTRACEP1==3] <- "Temperatura basal"
      data$CONTRACEP1[data$CONTRACEP1==4] <- "Billings"
      data$CONTRACEP1[data$CONTRACEP1==5] <- "Sinto-térmico"
      data$CONTRACEP1[data$CONTRACEP1==6] <- "DIU"
      data$CONTRACEP1[data$CONTRACEP1==7] <- "Diafragma"
      data$CONTRACEP1[data$CONTRACEP1==8] <- "Preservativo"
      data$CONTRACEP1[data$CONTRACEP1==9] <- "Espermicida"
      data$CONTRACEP1[data$CONTRACEP1==10] <- "Hormônio oral"
      data$CONTRACEP1[data$CONTRACEP1==11] <- "Hormônio injetável"
      data$CONTRACEP1[data$CONTRACEP1==12] <- "Coito interrompido"
      data$CONTRACEP1[data$CONTRACEP1==0] <- NA
      data$CONTRACEP1 <- factor(data$CONTRACEP1)
    }

    # CONTRACEP2
    if("CONTRACEP2" %in% campos){
      data$CONTRACEP2 <- as.numeric(levels(data$CONTRACEP2))[data$CONTRACEP2]
      data$CONTRACEP2[data$CONTRACEP2==1] <- "LAM (amamentação)"
      data$CONTRACEP2[data$CONTRACEP2==2] <- "Ogino Kaus"
      data$CONTRACEP2[data$CONTRACEP2==3] <- "Temperatura basal"
      data$CONTRACEP2[data$CONTRACEP2==4] <- "Billings"
      data$CONTRACEP2[data$CONTRACEP2==5] <- "Sinto-térmico"
      data$CONTRACEP2[data$CONTRACEP2==6] <- "DIU"
      data$CONTRACEP2[data$CONTRACEP2==7] <- "Diafragma"
      data$CONTRACEP2[data$CONTRACEP2==8] <- "Preservativo"
      data$CONTRACEP2[data$CONTRACEP2==9] <- "Espermicida"
      data$CONTRACEP2[data$CONTRACEP2==10] <- "Hormônio oral"
      data$CONTRACEP2[data$CONTRACEP2==11] <- "Hormônio injetável"
      data$CONTRACEP2[data$CONTRACEP2==12] <- "Coito interrompido"
      data$CONTRACEP2[data$CONTRACEP2==0] <- NA
      data$CONTRACEP2 <- factor(data$CONTRACEP2)
    }

    # VINCPREV
    if("VINCPREV" %in% campos){
      data$VINCPREV <- as.numeric(levels(data$VINCPREV))[data$VINCPREV]
      data$VINCPREV[data$VINCPREV==1] <- "Autônomo"
      data$VINCPREV[data$VINCPREV==2] <- "Desempregado"
      data$VINCPREV[data$VINCPREV==3] <- "Aposentado"
      data$VINCPREV[data$VINCPREV==4] <- "Não segurado"
      data$VINCPREV[data$VINCPREV==5] <- "Empregado"
      data$VINCPREV[data$VINCPREV==6] <- "Empregador"
      data$VINCPREV[data$VINCPREV==0] <- NA
      data$VINCPREV <- factor(data$VINCPREV)
    }

    # GESTOR_DT
    if("GESTOR_DT" %in% campos){
      data$GESTOR_DT <- as.character(data$GESTOR_DT)
      data$GESTOR_DT <- as.Date(data$GESTOR_DT, format = "%Y%m%d")
      data$GESTOR_DT <- format(data$GESTOR_DT, format = "%d-%m-%Y")
    }

    # INFEHOSP
    if("INFEHOSP" %in% campos){
      data$INFEHOSP <- as.numeric(levels(data$INFEHOSP))[data$INFEHOSP]
      data$INFEHOSP[data$INFEHOSP==0] <- "Não"
      data$INFEHOSP[data$INFEHOSP==1] <- "Sim"
      data$INFEHOSP <- factor(data$INFEHOSP)
    }

    # RACA_COR
    if("RACA_COR" %in% campos){
      data$RACA_COR <- as.numeric(levels(data$RACA_COR))[data$RACA_COR]
      data$RACA_COR[data$RACA_COR==1] <- "Branca"
      data$RACA_COR[data$RACA_COR==2] <- "Negra"
      data$RACA_COR[data$RACA_COR==3] <- "Parda"
      data$RACA_COR[data$RACA_COR==4] <- "Amarela"
      data$RACA_COR[data$RACA_COR==5] <- "Indígena"
      data$RACA_COR[data$RACA_COR==99] <- NA
      data$RACA_COR <- factor(data$RACA_COR)
    }

  }

  # Purge de levels não utilizados
  data <- droplevels(data)

  # Remove objetos
  rm(ano, unidade)

  # Retorna resultado
  return(data)
}
