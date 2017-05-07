# funPreProcessa
# Pré-processamento de dados do DataSUS

datasusProcess <- function(data, sistema, dadosMunRes = TRUE){
  # Verifica sistema
  sistemas <- c("SIM","SINASC","SIH-RD")
  if(!(sistema %in% sistemas)) stop("Sistema de informação desconhecido ou não implementado.")

  # Coleta nome dos campos
  campos <- names(data)

  # Trata campos

  # SIM

  if(sistema == "SIM"){

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

    # TIPOBITO
    if("TIPOBITO" %in% campos){
      data$TIPOBITO <- as.numeric(levels(data$TIPOBITO))[data$TIPOBITO]
      data$TIPOBITO[data$TIPOBITO==1] <- "Óbito fetal"
      data$TIPOBITO[data$TIPOBITO==2] <- "Óbito não fetal"
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

    # NATURAL
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

    # IDADE
    if("IDADE" %in% campos){
      data$IDADE <- as.character(data$IDADE)
      unidade <- substr(data$IDADE,1,1)
      data$IDADEhoras <- as.numeric(ifelse(unidade == 1, substr(data$IDADE, 2,3), NA))
      data$IDADEdias <- as.numeric(ifelse(unidade == 2, substr(data$IDADE, 2,3), NA))
      data$IDADEmeses <- as.numeric(ifelse(unidade == 3, substr(data$IDADE, 2,3), NA))
      data$IDADEanos <- as.numeric(ifelse(unidade == 4, substr(data$IDADE, 2,3), ifelse(unidade == 5, 100 + as.numeric(substr(data$IDADE, 2,3)), NA)))
      data$IDADE <- NULL
    }

    # SEXO
    if("SEXO" %in% campos){
      data$SEXO <- as.numeric(levels(data$SEXO))[data$SEXO]
      data$SEXO[data$SEXO==1] <- "Masculino"
      data$SEXO[data$SEXO==2] <- "Feminino"
      data$SEXO[data$SEXO==0] <- NA
      data$SEXO <- factor(data$SEXO)
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

    # ESTCIVIL
    if("ESTCIVIL" %in% campos){
      data$ESTCIVIL <- as.numeric(levels(data$ESTCIVIL))[data$ESTCIVIL]
      data$ESTCIVIL[data$ESTCIVIL==1] <- "Solteiro"
      data$ESTCIVIL[data$ESTCIVIL==2] <- "Casado"
      data$ESTCIVIL[data$ESTCIVIL==3] <- "Viúvo"
      data$ESTCIVIL[data$ESTCIVIL==4] <- "Separado judicialmente"
      data$ESTCIVIL[data$ESTCIVIL==5] <- "União consensual"
      data$ESTCIVIL[data$ESTCIVIL==9] <- NA
      data$ESTCIVIL <- factor(data$ESTCIVIL)
    }

    # ESC
    if("ESC" %in% campos){
      data$ESC <- as.numeric(levels(data$ESC))[data$ESC]
      data$ESC[data$ESC==1] <- "Nenhuma"
      data$ESC[data$ESC==2] <- "1 a 3 anos"
      data$ESC[data$ESC==3] <- "4 a 7 anos"
      data$ESC[data$ESC==4] <- "8 a 11 anos"
      data$ESC[data$ESC==5] <- "12 e mais"
      data$ESC[data$ESC==9] <- NA
      data$ESC <- factor(data$ESC)
    }

    # LOCOCOR
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
    }

    # ESCMAE
    if("ESCMAE" %in% campos){
      data$ESCMAE <- as.numeric(levels(data$ESCMAE))[data$ESCMAE]
      data$ESCMAE[data$ESCMAE==1] <- "Nenhuma"
      data$ESCMAE[data$ESCMAE==2] <- "1 a 3 anos"
      data$ESCMAE[data$ESCMAE==3] <- "4 a 7 anos"
      data$ESCMAE[data$ESCMAE==4] <- "8 a 11 anos"
      data$ESCMAE[data$ESCMAE==5] <- "12 e mais"
      data$ESCMAE[data$ESCMAE==9] <- NA
      data$ESCMAE <- factor(data$ESCMAE)
    }

    # QTDFILVIVO
    if("QTDFILVIVO" %in% campos){
      data$QTDFILVIVO <- as.numeric(data$QTDFILVIVO)
    }

    # QTDFILMORT
    if("QTDFILMORT" %in% campos){
      data$QTDFILMORT <- as.numeric(data$QTDFILMORT)
    }

    # GRAVIDEZ
    if("GRAVIDEZ" %in% campos){
      data$GRAVIDEZ <- as.numeric(levels(data$GRAVIDEZ))[data$GRAVIDEZ]
      data$GRAVIDEZ[data$GRAVIDEZ==1] <- "Única"
      data$GRAVIDEZ[data$GRAVIDEZ==2] <- "Dupla"
      data$GRAVIDEZ[data$GRAVIDEZ==3] <- "Tripla e mais"
      data$GRAVIDEZ[data$GRAVIDEZ==9] <- NA
      data$GRAVIDEZ <- factor(data$GRAVIDEZ)
    }

    # GESTACAO
    if("GESTACAO" %in% campos){
      data$GESTACAO <- as.numeric(levels(data$GESTACAO))[data$GESTACAO]
      data$GESTACAO[data$GESTACAO==1] <- "Menos de 22 semanas"
      data$GESTACAO[data$GESTACAO==2] <- "22 a 27 semanas"
      data$GESTACAO[data$GESTACAO==3] <- "28 a 31 semanas"
      data$GESTACAO[data$GESTACAO==4] <- "32 a 36 semanas"
      data$GESTACAO[data$GESTACAO==5] <- "37 a 41 semanas"
      data$GESTACAO[data$GESTACAO==6] <- "42 semanas e mais"
      data$GESTACAO[data$GESTACAO==9] <- NA
      data$GESTACAO <- factor(data$GESTACAO)
    }

    # PARTO
    if("PARTO" %in% campos){
      data$PARTO <- as.numeric(levels(data$PARTO))[data$PARTO]
      data$PARTO[data$PARTO==1] <- "Vaginal"
      data$PARTO[data$PARTO==2] <- "Cesáreo"
      data$PARTO[data$PARTO==9] <- NA
      data$PARTO <- factor(data$PARTO)
    }

    # OBITOPARTO
    if("PARTO" %in% campos){
      data$OBITOPARTO <- as.numeric(levels(data$OBITOPARTO))[data$OBITOPARTO]
      data$OBITOPARTO[data$OBITOPARTO==1] <- "Antes"
      data$OBITOPARTO[data$OBITOPARTO==2] <- "Durante"
      data$OBITOPARTO[data$OBITOPARTO==3] <- "Depois"
      data$OBITOPARTO[data$OBITOPARTO==9] <- NA
      data$OBITOPARTO <- factor(data$OBITOPARTO)
    }

    # PESO
    if("PESO" %in% campos){
      data$PESO <- as.numeric(data$PESO)
    }

    # NUMERODN
    if("NUMERODN" %in% campos){
      data$NUMERODN <- as.numeric(data$NUMERODN)
    }

    # OBITOGRAV
    if("OBITOGRAV" %in% campos){
      data$OBITOGRAV <- as.numeric(levels(data$OBITOGRAV))[data$OBITOGRAV]
      data$OBITOGRAV[data$OBITOGRAV==1] <- "Sim"
      data$OBITOGRAV[data$OBITOGRAV==2] <- "Não"
      data$OBITOGRAV[data$OBITOGRAV==9] <- NA
      data$OBITOGRAV <- factor(data$OBITOGRAV)
    }

    # OBITOPUERP
    if("OBITOPUERP" %in% campos){
      data$OBITOPUERP <- as.numeric(levels(data$OBITOPUERP))[data$OBITOPUERP]
      data$OBITOPUERP[data$OBITOPUERP==1] <- "Sim, até 42 dias"
      data$OBITOPUERP[data$OBITOPUERP==2] <- "Sim, de 43 dias a 1 ano"
      data$OBITOPUERP[data$OBITOPUERP==3] <- "Não"
      data$OBITOPUERP[data$OBITOPUERP==9] <- NA
      data$OBITOPUERP <- factor(data$OBITOPUERP)
    }

    # ASSISTMED
    if("ASSISTMED" %in% campos){
      data$ASSISTMED <- as.numeric(levels(data$ASSISTMED))[data$ASSISTMED]
      data$ASSISTMED[data$ASSISTMED==1] <- "Com assistência"
      data$ASSISTMED[data$ASSISTMED==2] <- "Sem assistência"
      data$ASSISTMED[data$ASSISTMED==9] <- NA
      data$ASSISTMED <- factor(data$ASSISTMED)
    }

    # EXAME
    if("EXAME" %in% campos){
      data$EXAME <- as.numeric(levels(data$EXAME))[data$EXAME]
      data$EXAME[data$EXAME==1] <- "Sim"
      data$EXAME[data$EXAME==2] <- "Não"
      data$EXAME[data$EXAME==9] <- NA
      data$EXAME <- factor(data$EXAME)
    }

    # CIRURGIA
    if("CIRURGIA" %in% campos){
      data$CIRURGIA <- as.numeric(levels(data$CIRURGIA))[data$CIRURGIA]
      data$CIRURGIA[data$CIRURGIA==1] <- "Sim"
      data$CIRURGIA[data$CIRURGIA==2] <- "Não"
      data$CIRURGIA[data$CIRURGIA==9] <- NA
      data$CIRURGIA <- factor(data$CIRURGIA)
    }

    # NECROPSIA
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

    # CIRCOBITO
    if("CIRCOBITO" %in% campos){
      data$CIRCOBITO <- as.numeric(levels(data$CIRCOBITO))[data$CIRCOBITO]
      data$CIRCOBITO[data$CIRCOBITO==1] <- "Acidente"
      data$CIRCOBITO[data$CIRCOBITO==2] <- "Suicídio"
      data$CIRCOBITO[data$CIRCOBITO==3] <- "Homicídio"
      data$CIRCOBITO[data$CIRCOBITO==4] <- "Outros"
      data$CIRCOBITO[data$CIRCOBITO==9] <- NA
      data$CIRCOBITO <- factor(data$CIRCOBITO)
    }

    # ACIDTRAB
    if("ACIDTRAB" %in% campos){
      data$ACIDTRAB <- as.numeric(levels(data$ACIDTRAB))[data$ACIDTRAB]
      data$ACIDTRAB[data$ACIDTRAB==1] <- "Sim"
      data$ACIDTRAB[data$ACIDTRAB==2] <- "Não"
      data$ACIDTRAB[data$ACIDTRAB==9] <- NA
      data$ACIDTRAB <- factor(data$ACIDTRAB)
    }

    # FONTE
    if("FONTE" %in% campos){
      data$FONTE <- as.numeric(levels(data$FONTE))[data$FONTE]
      data$FONTE[data$FONTE==1] <- "Boletim de Ocorrência"
      data$FONTE[data$FONTE==2] <- "Hospital"
      data$FONTE[data$FONTE==3] <- "Família"
      data$FONTE[data$FONTE==4] <- "Outra"
      data$FONTE[data$FONTE==9] <- NA
      data$FONTE <- factor(data$FONTE)
    }

    # TPPOS
    if("TPPOS" %in% campos){
      data$TPPOS <- plyr::revalue(data$TPPOS, c("N"="Não", "S"="Sim"))
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

    # ATESTANTE
    if("ATESTANTE" %in% campos){
      data$ATESTANTE <- as.numeric(levels(data$ATESTANTE))[data$ATESTANTE]
      data$ATESTANTE[data$ATESTANTE==1] <- "Sim"
      data$ATESTANTE[data$ATESTANTE==2] <- "Substituto"
      data$ATESTANTE[data$ATESTANTE==3] <- "IML"
      data$ATESTANTE[data$ATESTANTE==4] <- "SVO"
      data$ATESTANTE[data$ATESTANTE==5] <- "Outros"
      data$ATESTANTE <- factor(data$ATESTANTE)
    }

    # FONTEINV
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

  # SINASC

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
      data$LOCNASC[data$LOCNASC==9] <- NA
      data$LOCNASC <- factor(data$LOCNASC)
    }

    # IDADEMAE
    if("IDADEMAE" %in% campos){
      data$IDADEMAE <- as.numeric(data$IDADEMAE)
    }

    # ESTCIVMAE
    if("ESTCIVMAE" %in% campos){
      data$ESTCIVMAE <- as.numeric(levels(data$ESTCIVMAE))[data$ESTCIVMAE]
      data$ESTCIVMAE[data$ESTCIVMAE==1] <- "Solteira"
      data$ESTCIVMAE[data$ESTCIVMAE==2] <- "Casada"
      data$ESTCIVMAE[data$ESTCIVMAE==3] <- "Viúva"
      data$ESTCIVMAE[data$ESTCIVMAE==4] <- "Separado judicialmente / divorciado"
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
      data$ESCMAE[data$ESCMAE==9] <- NA
      data$ESCMAE <- factor(data$ESCMAE)
    }

    # QTDFILVIVO
    if("QTDFILVIVO" %in% campos){
      data$QTDFILVIVO <- as.numeric(data$QTDFILVIVO)
    }

    # QTDFILMORT
    if("QTDFILMORT" %in% campos){
      data$QTDFILMORT <- as.numeric(data$QTDFILMORT)
    }

    # GESTACAO
    if("GESTACAO" %in% campos){
      data$GESTACAO <- as.numeric(levels(data$GESTACAO))[data$GESTACAO]
      data$GESTACAO[data$GESTACAO==1] <- "Menos de 22 semanas"
      data$GESTACAO[data$GESTACAO==2] <- "22 a 27 semanas"
      data$GESTACAO[data$GESTACAO==3] <- "28 a 31 semanas"
      data$GESTACAO[data$GESTACAO==4] <- "32 a 36 semanas"
      data$GESTACAO[data$GESTACAO==5] <- "37 a 41 semanas"
      data$GESTACAO[data$GESTACAO==6] <- "42 semanas e mais"
      data$GESTACAO[data$GESTACAO==9] <- NA
      data$GESTACAO <- factor(data$GESTACAO)
    }

    # GRAVIDEZ
    if("GRAVIDEZ" %in% campos){
      data$GRAVIDEZ <- as.numeric(levels(data$GRAVIDEZ))[data$GRAVIDEZ]
      data$GRAVIDEZ[data$GRAVIDEZ==1] <- "Única"
      data$GRAVIDEZ[data$GRAVIDEZ==2] <- "Dupla"
      data$GRAVIDEZ[data$GRAVIDEZ==3] <- "Tripla e mais"
      data$GRAVIDEZ[data$GRAVIDEZ==9] <- NA
      data$GRAVIDEZ <- factor(data$GRAVIDEZ)
    }

    # PARTO
    if("PARTO" %in% campos){
      data$PARTO <- as.numeric(levels(data$PARTO))[data$PARTO]
      data$PARTO[data$PARTO==1] <- "Vaginal"
      data$PARTO[data$PARTO==2] <- "Cesáreo"
      data$PARTO[data$PARTO==9] <- NA
      data$PARTO <- factor(data$PARTO)
    }

    # CONSULTAS
    if("CONSULTAS" %in% campos){
      data$CONSULTAS <- as.numeric(levels(data$CONSULTAS))[data$CONSULTAS]
      data$CONSULTAS[data$CONSULTAS==1] <- "Nenhuma"
      data$CONSULTAS[data$CONSULTAS==2] <- "de 1 a 3"
      data$CONSULTAS[data$CONSULTAS==3] <- "de 4 a 6"
      data$CONSULTAS[data$CONSULTAS==4] <- "7 e mais"
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
      data$SEXO[data$SEXO==1] <- "Masculino"
      data$SEXO[data$SEXO==2] <- "Feminino"
      data$SEXO[data$SEXO==0] <- NA
      data$SEXO <- factor(data$SEXO)
    }

    # APGAR1
    if("APGAR1" %in% campos){
      data$APGAR1 <- as.numeric(data$APGAR1)
    }

    # APGAR5
    if("APGAR5" %in% campos){
      data$APGAR5 <- as.numeric(data$APGAR5)
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
    }

    # IDANOMAL
    if("IDANOMAL" %in% campos){
      data$IDANOMAL <- as.numeric(levels(data$IDANOMAL))[data$IDANOMAL]
      data$IDANOMAL[data$IDANOMAL==1] <- "Sim"
      data$IDANOMAL[data$IDANOMAL==2] <- "Não"
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

  # SIH-RD
  if(sistema == "SIH-RD"){

    # MUNIC_RES
    if("MUNIC_RES" %in% campos & dadosMunRes == TRUE){
      data$MUNIC_RES <- as.integer(as.character(data$MUNIC_RES))
      colnames(tabMun)[1] <- "MUNIC_RES"
      data <- dplyr::left_join(data, tabMun, by = "MUNIC_RES")
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

    # IDENT
    if("IDENT" %in% campos){
      data$IDENT <- as.numeric(levels(data$IDENT))[data$IDENT]
      data$IDENT[data$IDENT==1] <- "Principal"
      data$IDENT[data$IDENT==3] <- "Continuação"
      data$IDENT[data$IDENT==5] <- "Longa permanência"
      data$IDENT <- factor(data$IDENT)
    }

    # NASC
    if("NASC" %in% campos){
      data$NASC <- as.character(data$NASC)
      data$NASC <- as.Date(data$NASC, format = "%Y%m%d")
      data$NASC <- format(data$NASC, format = "%d-%m-%Y")
    }

    # SEXO
    if("SEXO" %in% campos){
      data$SEXO <- as.numeric(levels(data$SEXO))[data$SEXO]
      data$SEXO[data$SEXO==1] <- "Masculino"
      data$SEXO[data$SEXO==3] <- "Feminino"
      data$SEXO <- factor(data$SEXO)
    }

    # UTI_MES_IN
    if("UTI_MES_IN" %in% campos){
      data$UTI_MES_IN <- NULL
    }

    # UTI_MES_AN
    if("UTI_MES_AN" %in% campos){
      data$UTI_MES_AN <- NULL
    }

    # UTI_MES_AL
    if("UTI_MES_AL" %in% campos){
      data$UTI_MES_AL <- NULL
    }

    # UTI_INT_IN
    if("UTI_INT_IN" %in% campos){
      data$UTI_INT_IN <- NULL
    }

    # UTI_INT_AN
    if("UTI_INT_AN" %in% campos){
      data$UTI_INT_AN <- NULL
    }

    # UTI_INT_AL
    if("UTI_INT_AL" %in% campos){
      data$UTI_INT_AL <- NULL
    }

    # VAL_SADT
    if("VAL_SADT" %in% campos){
      data$VAL_SADT <- NULL
    }

    # VAL_RN
    if("VAL_RN" %in% campos){
      data$VAL_RN <- NULL
    }

    # VAL_ACOMP
    if("VAL_ACOMP" %in% campos){
      data$VAL_ACOMP <- NULL
    }

    # VAL_ORTP
    if("VAL_ORTP" %in% campos){
      data$VAL_ORTP <- NULL
    }

    # VAL_SANGUE
    if("VAL_SANGUE" %in% campos){
      data$VAL_SANGUE <- NULL
    }

    # VAL_SADTSR
    if("VAL_SADTSR" %in% campos){
      data$VAL_SADTSR <- NULL
    }

    # VAL_TRANSP
    if("VAL_TRANSP" %in% campos){
      data$VAL_TRANSP <- NULL
    }

    # VAL_OBSANG
    if("VAL_OBSANG" %in% campos){
      data$VAL_OBSANG <- NULL
    }

    # VAL_PED1AC
    if("VAL_PED1AC" %in% campos){
      data$VAL_PED1AC <- NULL
    }

    # RUBRICA
    if("RUBRICA" %in% campos){
      data$RUBRICA <- NULL
    }

    # NUM_PROC
    if("NUM_PROC" %in% campos){
      data$NUM_PROC <- NULL
    }

    # TOT_PT_SP
    if("TOT_PT_SP" %in% campos){
      data$TOT_PT_SP <- NULL
    }

    # CPF_AUT
    if("CPF_AUT" %in% campos){
      data$CPF_AUT <- NULL
    }

    # MARCA_UTI
    if("MARCA_UTI" %in% campos){
      data$MARCA_UTI <- as.numeric(levels(data$MARCA_UTI))[data$MARCA_UTI]
      data$MARCA_UTI[data$MARCA_UTI==0] <- "Leito sem especialidade ou não utilizou UTI"
      data$MARCA_UTI[data$MARCA_UTI==1] <- "UTI adulto nível II"
      data$MARCA_UTI[data$MARCA_UTI==2] <- "UTI adulto nível III"
      data$MARCA_UTI[data$MARCA_UTI==3] <- "UTI neonatal nível III"
      data$MARCA_UTI[data$MARCA_UTI==4] <- "UTI neonatal nível II"
      data$MARCA_UTI[data$MARCA_UTI==5] <- "UTI pediátrica nível II"
      data$MARCA_UTI[data$MARCA_UTI==6] <- "UTI pediátrica nível III"
      data$MARCA_UTI[data$MARCA_UTI==7] <- "Transplante pediátrico"
      data$MARCA_UTI[data$MARCA_UTI==8] <- "Transplante adulto"
      data$MARCA_UTI <- factor(data$MARCA_UTI)
    }

    # UTI_INT_TO
    if("UTI_INT_TO" %in% campos){
      data$UTI_INT_TO <- as.numeric(data$UTI_INT_TO)
    }

    # DIAR_ACOM
    if("DIAR_ACOM" %in% campos){
      data$DIAR_ACOM <- as.numeric(data$DIAR_ACOM)
    }

    # QT_DIARIAS
    if("QT_DIARIAS" %in% campos){
      data$QT_DIARIAS <- as.numeric(data$QT_DIARIAS)
    }

    # VAL_UT
    if("VAL_UT" %in% campos){
      data$VAL_UT[data$MARCA_UTI=="Leito sem especialidade ou não utilizou UTI"] <- NA
    }

    # DI_INTER
    if("DI_INTER" %in% campos){
      data$DI_INTER <- as.character(data$DI_INTER)
      data$DI_INTER <- as.Date(data$DI_INTER, format = "%Y%m%d")
      data$DI_INTER <- format(data$DI_INTER, format = "%d-%m-%Y")
    }

    # DT_SAIDA
    if("DT_SAIDA" %in% campos){
      data$DT_SAIDA <- as.character(data$DT_SAIDA)
      data$DT_SAIDA <- as.Date(data$DT_SAIDA, format = "%Y%m%d")
      data$DT_SAIDA <- format(data$DT_SAIDA, format = "%d-%m-%Y")
    }

    # DIAG_SECUN
    if("DIAG_SECUN" %in% campos){
      data$DIAG_SECUN[data$DIAG_SECUN=="0000"] <- NA
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

  # Retorna resultado
  return(data)
}
