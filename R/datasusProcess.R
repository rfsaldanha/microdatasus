# funPreProcessa
# Pré-processamento de dados do DataSUS

datasusProcess <- function(data, sistema){
  # Verifica sistema
  sistemas <- c("SIM")
  if(!(sistema %in% sistemas)) stop("Sistema de informação desconhecido ou não implementado.")

  # Campos
  campos <- names(data)

  # Trata campos

  if(sistema == "SIM"){
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
      data$NATURAL <- data.table:::merge.data.table(data.frame(cod=data$NATURAL), tabNaturalidade, all.x = TRUE)$nome
      data$NATURAL <- factor(data$NATURAL)
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
      data$TPPOS <- revalue(data$TPPOS, c("N"="Não", "S"="Sim"))
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
  }

  # Purge de levels não utilizados
  data <- droplevels(data)

  # Retorna resultado
  return(data)
}
