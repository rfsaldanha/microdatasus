process_sim <- function(data, municipality_data = TRUE) {
  # Variables names
  variables_names <- names(data)
  
  # Declara objetos
  ano <- NULL
  unidade <- NULL
  
  # CODMUNRES
  if ("CODMUNRES" %in% variables_names & municipality_data == TRUE) {
    data$CODMUNRES <- as.integer(as.character(data$CODMUNRES))
    colnames(tabMun)[1] <- "CODMUNRES"
    data <- dplyr::left_join(data, tabMun, by = "CODMUNRES")
  } else {
    data$CODMUNRES <- as.integer(as.character(data$CODMUNRES))
  }
  
  # NUMERODO
  if ("NUMERODO" %in% variables_names) {
    data$NUMERODO <- as.character(data$NUMERODO)
  }
  
  # TIPOBITO.CNV
  if ("TIPOBITO" %in% variables_names) {
    data$TIPOBITO <- as.numeric(levels(data$TIPOBITO))[data$TIPOBITO]
    data$TIPOBITO[data$TIPOBITO == 0] <- NA
    data$TIPOBITO[data$TIPOBITO == 9] <- NA
    data$TIPOBITO[data$TIPOBITO == 1] <- "Fetal"
    data$TIPOBITO[data$TIPOBITO == 2] <- "Não Fetal"
    data$TIPOBITO <- factor(data$TIPOBITO)
  }
  
  # DTOBITO
  if ("DTOBITO" %in% variables_names) {
    data$DTOBITO <- as.character(data$DTOBITO)
    data$DTOBITO <- as.Date(data$DTOBITO, format = "%d%m%Y")
  }
  
  # HORAOBITO
  if ("HORAOBITO" %in% variables_names) {
    data$HORAOBITO <- as.character(data$HORAOBITO)
  }
  
  # CODCART
  if ("CODCART" %in% variables_names) {
    data$CODCART <- as.character(data$CODCART)
  }
  
  # CODMUNCART
  if ("CODMUNCART" %in% variables_names) {
    data$CODMUNCART <- as.numeric(data$CODMUNCART)
  }
  
  # NATURAL.CNV
  if ("NATURAL" %in% variables_names) {
    data$NATURAL <- as.character(data$NATURAL)
    colnames(tabNaturalidade)[1] <- "NATURAL"
    data$NATURAL <- factor(dplyr::left_join(data, tabNaturalidade, by = "NATURAL")$nome)
  }
  
  # DTNASC
  if ("DTNASC" %in% variables_names) {
    data$DTNASC <- as.character(data$DTNASC)
    data$DTNASC <- as.Date(data$DTNASC, format = "%d%m%Y")
  }
  
  # IDADE.CNV
  if ("IDADE" %in% variables_names) {
    data$IDADE <- as.character(data$IDADE)
    data$IDADE[data$IDADE == "000" | data$IDADE == "999"] <- NA
    unidade <- substr(data$IDADE, 1, 1)
    # Horas
    data$IDADEhoras <-
      as.numeric(ifelse(unidade == 1, substr(data$IDADE, 2, 3), NA))
    # Dias
    data$IDADEdias <-
      as.numeric(ifelse(unidade == 2, substr(data$IDADE, 2, 3), NA))
    # Meses
    data$IDADEmeses <-
      as.numeric(ifelse(unidade == 3, substr(data$IDADE, 2, 3), NA))
    # Anos
    data$IDADEanos <-
      as.numeric(ifelse(
        unidade == 4,
        substr(data$IDADE, 2, 3),
        ifelse(unidade == 5, 100 + as.numeric(substr(data$IDADE, 2, 3)), NA)
      ))
    # Apaga campo original
    data$IDADE <- NULL
  }
  
  # SEXO.CNV
  if ("SEXO" %in% variables_names) {
    data$SEXO <- as.numeric(levels(data$SEXO))[data$SEXO]
    data$SEXO[data$SEXO == 0] <- NA
    data$SEXO[data$SEXO == 9] <- NA
    data$SEXO[data$SEXO == 1] <- "Masculino"
    data$SEXO[data$SEXO == 2] <- "Feminino"
    data$SEXO <- factor(data$SEXO)
  }
  
  # RACACOR.CNV
  if ("RACACOR" %in% variables_names) {
    data$RACACOR <- as.numeric(levels(data$RACACOR))[data$RACACOR]
    data$RACACOR[data$RACACOR == 0] <- NA
    data$RACACOR[data$RACACOR == 1] <- "Branca"
    data$RACACOR[data$RACACOR == 2] <- "Preta"
    data$RACACOR[data$RACACOR == 3] <- "Amarela"
    data$RACACOR[data$RACACOR == 4] <- "Parda"
    data$RACACOR[data$RACACOR == 5] <- "Indígena"
    data$RACACOR[data$RACACOR == 6] <- NA
    data$RACACOR[data$RACACOR == 7] <- NA
    data$RACACOR[data$RACACOR == 8] <- NA
    data$RACACOR[data$RACACOR == 9] <- NA
    data$RACACOR <- factor(data$RACACOR)
  }
  
  # ESTCIV.CNV
  if ("ESTCIV" %in% variables_names) {
    data$ESTCIV <- as.numeric(levels(data$ESTCIV))[data$ESTCIV]
    data$ESTCIV[data$ESTCIV == 0] <- NA
    data$ESTCIV[data$ESTCIV == 1] <- "Solteiro"
    data$ESTCIV[data$ESTCIV == 2] <- "Casado"
    data$ESTCIV[data$ESTCIV == 3] <- "Viúvo"
    data$ESTCIV[data$ESTCIV == 4] <- "Separado judicialmente"
    data$ESTCIV[data$ESTCIV == 5] <- "União consensual"
    data$ESTCIV[data$ESTCIV == 6] <- NA
    data$ESTCIV[data$ESTCIV == 7] <- NA
    data$ESTCIV[data$ESTCIV == 8] <- NA
    data$ESTCIV[data$ESTCIV == 9] <- NA
    data$ESTCIV <- factor(data$ESTCIV)
  }
  
  # ESC (INSTRUC.CNV)
  if ("ESC" %in% variables_names) {
    data$ESC <- as.character(levels(data$ESC))[data$ESC]
    data$ESC[data$ESC == "0"] <- NA
    data$ESC[data$ESC == "6"] <- NA
    data$ESC[data$ESC == "7"] <- NA
    data$ESC[data$ESC == "9"] <- NA
    data$ESC[data$ESC == "A"] <- NA
    data$ESC[data$ESC == "1"] <- "Nenhuma"
    data$ESC[data$ESC == "2"] <- "1 a 3 anos"
    data$ESC[data$ESC == "3"] <- "4 a 7 anos"
    data$ESC[data$ESC == "4"] <- "8 a 11 anos"
    data$ESC[data$ESC == "5"] <- "12 anos ou mais"
    data$ESC[data$ESC == "8"] <- "9 a 11 anos"
    data$ESC[data$ESC == "9"] <- NA
    data$ESC <- factor(data$ESC)
  }
  
  # OCUP (OCUPA.CNV ou CBO2002.CNV)
  if ("OCUP" %in% variables_names) {
    if (!("DTOBITO" %in% variables_names))
      stop("Necessário incluir o campo DTOBITO para o pré-processamento da variável OCUP.")
    data$OCUP <- as.character(data$OCUP)
    colnames(tabOcupacao)[1] <- "OCUP"
    colnames(tabCBO)[1] <- "OCUP"
    ano <- lubridate::year(data$DTOBITO)
    data$OCUP <-
      factor(ifelse(
        ano <= 2005,
        plyr::join(data, tabOcupacao, by = "OCUP", match = "first")$nome,
        dplyr::left_join(data, tabCBO, by = "OCUP")$nome
      ))
  }
  
  # LOCOCOR.CNV
  if ("LOCOCOR" %in% variables_names) {
    data$LOCOCOR <- as.numeric(levels(data$LOCOCOR))[data$LOCOCOR]
    data$LOCOCOR[data$LOCOCOR == 1] <- "Hospital"
    data$LOCOCOR[data$LOCOCOR == 2] <-
      "Outro estabelecimento de saúde"
    data$LOCOCOR[data$LOCOCOR == 3] <- "Domicílio"
    data$LOCOCOR[data$LOCOCOR == 4] <- "Via pública"
    data$LOCOCOR[data$LOCOCOR == 5] <- "Outros"
    data$LOCOCOR[data$LOCOCOR == 9] <- NA
    data$LOCOCOR <- factor(data$LOCOCOR)
  }
  
  # IDADEMAE
  if ("IDADEMAE" %in% variables_names) {
    data$IDADEMAE <- as.numeric(data$IDADEMAE)
    data$IDADEMAE[data$IDADEMAE == 0] <- NA
  }
  
  # ESCMAE.CNV
  if ("ESCMAE" %in% variables_names) {
    data$ESCMAE <- as.character(levels(data$ESCMAE))[data$ESCMAE]
    data$ESCMAE[data$ESCMAE == "0"] <- NA
    data$ESCMAE[data$ESCMAE == "6"] <- NA
    data$ESCMAE[data$ESCMAE == "7"] <- NA
    data$ESCMAE[data$ESCMAE == "9"] <- NA
    data$ESCMAE[data$ESCMAE == "A"] <- NA
    data$ESCMAE[data$ESCMAE == "1"] <- "Nenhuma"
    data$ESCMAE[data$ESCMAE == "2"] <- "1 a 3 anos"
    data$ESCMAE[data$ESCMAE == "3"] <- "4 a 7 anos"
    data$ESCMAE[data$ESCMAE == "4"] <- "8 a 11 anos"
    data$ESCMAE[data$ESCMAE == "5"] <- "12 anos ou mais"
    data$ESCMAE[data$ESCMAE == "8"] <- "9 a 11 anos"
    data$ESCMAE[data$ESCMAE == "9"] <- NA
    data$ESCMAE <- factor(data$ESCMAE)
  }
  
  # OCUPMAE (OCUPA.CNV ou CBO2002.CNV.)
  if ("OCUPMAE" %in% variables_names) {
    if (!("DTOBITO" %in% variables_names))
      stop("Necessário incluir o campo DTOBITO para o pré-processamento da variável OCUP.")
    data$OCUPMAE <- as.character(data$OCUPMAE)
    colnames(tabOcupacao)[1] <- "OCUPMAE"
    colnames(tabCBO)[1] <- "OCUPMAE"
    ano <- lubridate::year(data$DTOBITO)
    data$OCUP <-
      factor(ifelse(
        ano <= 2005,
        plyr::join(data, tabOcupacao, by = "OCUPMAE", match = "first")$nome,
        dplyr::left_join(data, tabCBO, by = "OCUPMAE")$nome
      ))
  }
  
  # QTDFILVIVO
  if ("QTDFILVIVO" %in% variables_names) {
    data$QTDFILVIVO <- as.numeric(data$QTDFILVIVO)
  }
  
  # QTDFILMORT
  if ("QTDFILMORT" %in% variables_names) {
    data$QTDFILMORT <- as.numeric(data$QTDFILMORT)
  }
  
  # GRAVIDEZ.CNV
  if ("GRAVIDEZ" %in% variables_names) {
    data$GRAVIDEZ <- as.numeric(levels(data$GRAVIDEZ))[data$GRAVIDEZ]
    data$GRAVIDEZ[data$GRAVIDEZ == 1] <- "Única"
    data$GRAVIDEZ[data$GRAVIDEZ == 2] <- "Dupla"
    data$GRAVIDEZ[data$GRAVIDEZ == 3] <- "Tríplice e mais"
    data$GRAVIDEZ[data$GRAVIDEZ == 9] <- NA
    data$GRAVIDEZ <- factor(data$GRAVIDEZ)
  }
  
  # GESTACAO.CNV
  if ("GESTACAO" %in% variables_names) {
    data$GESTACAO <- as.character(levels(data$GESTACAO))[data$GESTACAO]
    data$GESTACAO[data$GESTACAO == "0"] <- NA
    data$GESTACAO[data$GESTACAO == "A"] <- "21 a 27 semanas"
    data$GESTACAO[data$GESTACAO == "1"] <- "Menos de 22 semanas"
    data$GESTACAO[data$GESTACAO == "2"] <- "22 a 27 semanas"
    data$GESTACAO[data$GESTACAO == "3"] <- "28 a 31 semanas"
    data$GESTACAO[data$GESTACAO == "4"] <- "32 a 36 semanas"
    data$GESTACAO[data$GESTACAO == "5"] <- "37 a 41 semanas"
    data$GESTACAO[data$GESTACAO == "6"] <- "42 semanas e mais"
    data$GESTACAO[data$GESTACAO == "7"] <- "28 semanas e mais"
    data$GESTACAO[data$GESTACAO == "8"] <- "28 a 36 semanas"
    data$GESTACAO[data$GESTACAO == "9"] <- NA
    data$GESTACAO <- factor(data$GESTACAO)
  }
  
  # PARTO.CNV
  if ("PARTO" %in% variables_names) {
    data$PARTO <- as.numeric(levels(data$PARTO))[data$PARTO]
    data$PARTO[data$PARTO == 0] <- NA
    data$PARTO[data$PARTO == 1] <- "Vaginal"
    data$PARTO[data$PARTO == 2] <- "Cesáreo"
    data$PARTO[data$PARTO == 3] <- NA
    data$PARTO[data$PARTO == 4] <- NA
    data$PARTO[data$PARTO == 5] <- NA
    data$PARTO[data$PARTO == 6] <- NA
    data$PARTO[data$PARTO == 7] <- NA
    data$PARTO[data$PARTO == 8] <- NA
    data$PARTO[data$PARTO == 9] <- NA
    data$PARTO <- factor(data$PARTO)
  }
  
  # OBITOPARTO.CNV
  if ("OBITOPARTO" %in% variables_names) {
    data$OBITOPARTO <- as.numeric(levels(data$OBITOPARTO))[data$OBITOPARTO]
    data$OBITOPARTO[data$OBITOPARTO == 0] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 1] <- "Antes"
    data$OBITOPARTO[data$OBITOPARTO == 2] <- "Durante"
    data$OBITOPARTO[data$OBITOPARTO == 3] <- "Depois"
    data$OBITOPARTO[data$OBITOPARTO == 4] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 5] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 6] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 7] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 8] <- NA
    data$OBITOPARTO[data$OBITOPARTO == 9] <- NA
    data$OBITOPARTO <- factor(data$OBITOPARTO)
  }
  
  # PESO.CNV
  if ("PESO" %in% variables_names) {
    data$PESO <- as.numeric(data$PESO)
    data$PESO[data$PESO == 0] <- NA
  }
  
  # NUMERODN
  if ("NUMERODN" %in% variables_names) {
    data$NUMERODN <- as.numeric(data$NUMERODN)
  }
  
  # CODESTAB
  if ("CODESTAB" %in% variables_names) {
    data$CODESTAB <- as.character(data$CODESTAB)
  }
  
  # OBITOGRAV.CNV
  if ("OBITOGRAV" %in% variables_names) {
    data$OBITOGRAV <- as.numeric(levels(data$OBITOGRAV))[data$OBITOGRAV]
    data$OBITOGRAV[data$OBITOGRAV == 1] <- "Sim"
    data$OBITOGRAV[data$OBITOGRAV == 2] <- "Não"
    data$OBITOGRAV[data$OBITOGRAV == 3] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 4] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 5] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 6] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 7] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 8] <- NA
    data$OBITOGRAV[data$OBITOGRAV == 9] <- NA
    data$OBITOGRAV <- factor(data$OBITOGRAV)
  }
  
  # OBITOPUERP.CNV
  if ("OBITOPUERP" %in% variables_names) {
    data$OBITOPUERP <- as.numeric(levels(data$OBITOPUERP))[data$OBITOPUERP]
    data$OBITOPUERP[data$OBITOPUERP == 1] <- "De 0 a 42 dias"
    data$OBITOPUERP[data$OBITOPUERP == 2] <- "De 43 dias a 1 ano"
    data$OBITOPUERP[data$OBITOPUERP == 3] <- "Não"
    data$OBITOPUERP[data$OBITOPUERP == 4] <- NA
    data$OBITOPUERP[data$OBITOPUERP == 5] <- NA
    data$OBITOPUERP[data$OBITOPUERP == 6] <- NA
    data$OBITOPUERP[data$OBITOPUERP == 7] <- NA
    data$OBITOPUERP[data$OBITOPUERP == 8] <- NA
    data$OBITOPUERP[data$OBITOPUERP == 9] <- NA
    data$OBITOPUERP <- factor(data$OBITOPUERP)
  }
  
  # ASSISTMED.CNV
  if ("ASSISTMED" %in% variables_names) {
    data$ASSISTMED <- as.numeric(levels(data$ASSISTMED))[data$ASSISTMED]
    data$ASSISTMED[data$ASSISTMED == 1] <- "Sim"
    data$ASSISTMED[data$ASSISTMED == 2] <- "Não"
    data$ASSISTMED[data$ASSISTMED == 9] <- NA
    data$ASSISTMED <- factor(data$ASSISTMED)
  }
  
  # EXAME.CNV
  if ("EXAME" %in% variables_names) {
    data$EXAME <- as.numeric(levels(data$EXAME))[data$EXAME]
    data$EXAME[data$EXAME == 1] <- "Sim"
    data$EXAME[data$EXAME == 2] <- "Não"
    data$EXAME[data$EXAME == 9] <- NA
    data$EXAME <- factor(data$EXAME)
  }
  
  # CIRURGIA.CNV
  if ("CIRURGIA" %in% variables_names) {
    data$CIRURGIA <- as.numeric(levels(data$CIRURGIA))[data$CIRURGIA]
    data$CIRURGIA[data$CIRURGIA == 1] <- "Sim"
    data$CIRURGIA[data$CIRURGIA == 2] <- "Não"
    data$CIRURGIA[data$CIRURGIA == 9] <- NA
    data$CIRURGIA <- factor(data$CIRURGIA)
  }
  
  # NECROPSIA (NECROPS.CNV)
  if ("NECROPSIA" %in% variables_names) {
    data$NECROPSIA <- as.numeric(levels(data$NECROPSIA))[data$NECROPSIA]
    data$NECROPSIA[data$NECROPSIA == 1] <- "Sim"
    data$NECROPSIA[data$NECROPSIA == 2] <- "Não"
    data$NECROPSIA[data$NECROPSIA == 9] <- NA
    data$NECROPSIA <- factor(data$NECROPSIA)
  }
  
  # DTATESTADO
  if ("DTATESTADO" %in% variables_names) {
    data$DTATESTADO <- as.character(data$DTATESTADO)
    data$DTATESTADO <- as.Date(data$DTATESTADO, format = "%d%m%Y")
  }
  
  # CIRCOBITO (TIPOVIOL.CNV)
  if ("CIRCOBITO" %in% variables_names) {
    data$CIRCOBITO <- as.numeric(levels(data$CIRCOBITO))[data$CIRCOBITO]
    data$CIRCOBITO[data$CIRCOBITO == 0] <- NA
    data$CIRCOBITO[data$CIRCOBITO == 1] <- "Acidente"
    data$CIRCOBITO[data$CIRCOBITO == 2] <- "Suicídio"
    data$CIRCOBITO[data$CIRCOBITO == 3] <- "Homicídio"
    data$CIRCOBITO[data$CIRCOBITO == 4] <- "Outro"
    data$CIRCOBITO[data$CIRCOBITO == 5] <- NA
    data$CIRCOBITO[data$CIRCOBITO == 6] <- NA
    data$CIRCOBITO[data$CIRCOBITO == 7] <- NA
    data$CIRCOBITO[data$CIRCOBITO == 8] <- NA
    data$CIRCOBITO[data$CIRCOBITO == 9] <- NA
    data$CIRCOBITO <- factor(data$CIRCOBITO)
  }
  
  # ACIDTRAB.CNV
  if ("ACIDTRAB" %in% variables_names) {
    data$ACIDTRAB <- as.numeric(levels(data$ACIDTRAB))[data$ACIDTRAB]
    data$ACIDTRAB[data$ACIDTRAB == 1] <- "Sim"
    data$ACIDTRAB[data$ACIDTRAB == 2] <- "Não"
    data$ACIDTRAB[data$ACIDTRAB == 9] <- NA
    data$ACIDTRAB <- factor(data$ACIDTRAB)
  }
  
  # FONTE (FONTINFO.CNV)
  if ("FONTE" %in% variables_names) {
    data$FONTE <- as.numeric(levels(data$FONTE))[data$FONTE]
    data$FONTE[data$FONTE == 1] <- "Boletim de Ocorrência"
    data$FONTE[data$FONTE == 2] <- "Hospital"
    data$FONTE[data$FONTE == 3] <- "Família"
    data$FONTE[data$FONTE == 4] <- "Outro"
    data$FONTE[data$FONTE == 9] <- NA
    data$FONTE <- factor(data$FONTE)
  }
  
  # TPPOS (INVESTIG.CNV)
  if ("TPPOS" %in% variables_names) {
    data$TPPOS <- plyr::revalue(data$TPPOS, c("N" = "Não investigado", "S" = "Investigado"))
  }
  
  # DTINVESTIG
  if ("DTINVESTIG" %in% variables_names) {
    data$DTINVESTIG <- as.character(data$DTINVESTIG)
    data$DTINVESTIG <- as.Date(data$DTINVESTIG, format = "%d%m%Y")
  }
  
  # DTCADASTRO
  if ("DTCADASTRO" %in% variables_names) {
    data$DTCADASTRO <- as.character(data$DTCADASTRO)
    data$DTCADASTRO <- as.Date(data$DTCADASTRO, format = "%d%m%Y")
  }
  
  # ATESTANTE (ATESTANT.CNV)
  if ("ATESTANTE" %in% variables_names) {
    data$ATESTANTE <- as.numeric(levels(data$ATESTANTE))[data$ATESTANTE]
    data$ATESTANTE[data$ATESTANTE == 0] <- NA
    data$ATESTANTE[data$ATESTANTE == 1] <- "Sim"
    data$ATESTANTE[data$ATESTANTE == 2] <- "Substituto"
    data$ATESTANTE[data$ATESTANTE == 3] <- "IML"
    data$ATESTANTE[data$ATESTANTE == 4] <- "SVO"
    data$ATESTANTE[data$ATESTANTE == 5] <- "Outro"
    data$ATESTANTE[data$ATESTANTE == 6] <- NA
    data$ATESTANTE[data$ATESTANTE == 7] <- NA
    data$ATESTANTE[data$ATESTANTE == 8] <- NA
    data$ATESTANTE[data$ATESTANTE == 9] <- NA
    data$ATESTANTE <- factor(data$ATESTANTE)
  }
  
  # FONTEINV.CNV
  if ("FONTEINV" %in% variables_names) {
    data$FONTEINV <- as.numeric(levels(data$FONTEINV))[data$FONTEINV]
    data$FONTEINV[data$FONTEINV == 1] <- "Comitê de Mortalidade Materna e/ou Infantil"
    data$FONTEINV[data$FONTEINV == 2] <- "Visita familiar / Entrevista família"
    data$FONTEINV[data$FONTEINV == 3] <- "Estabelecimento de saúde / Prontuário"
    data$FONTEINV[data$FONTEINV == 4] <- "Relacionamento com outros bancos de dados"
    data$FONTEINV[data$FONTEINV == 5] <- "SVO"
    data$FONTEINV[data$FONTEINV == 6] <- "IML"
    data$FONTEINV[data$FONTEINV == 7] <- "Outra fonte"
    data$FONTEINV[data$FONTEINV == 8] <- "Múltiplas fontes"
    data$FONTEINV[data$FONTEINV == 9] <- NA
    data$FONTEINV <- factor(data$FONTEINV)
  }
  
  # DTRECEBIM
  if ("DTRECEBIM" %in% variables_names) {
    data$DTRECEBIM <- as.character(data$DTRECEBIM)
    data$DTRECEBIM <- as.Date(data$DTRECEBIM, format = "%d%m%Y")
  }
  
  # UFINFORM (UF.CNV)
  if ("UFINFORM" %in% variables_names) {
    data$UFINFORM <- as.numeric(levels(data$UFINFORM))[data$UFINFORM]
    data$UFINFORM[data$UFINFORM == 0] <- NA
    data$UFINFORM[data$UFINFORM == 11] <- "Rondônia"
    data$UFINFORM[data$UFINFORM == 12] <- "Acre"
    data$UFINFORM[data$UFINFORM == 13] <- "Amazonas"
    data$UFINFORM[data$UFINFORM == 14] <- "Roraima"
    data$UFINFORM[data$UFINFORM == 15] <- "Pará"
    data$UFINFORM[data$UFINFORM == 16] <- "Amapá"
    data$UFINFORM[data$UFINFORM == 17] <- "Tocantins"
    data$UFINFORM[data$UFINFORM == 21] <- "Maranhão"
    data$UFINFORM[data$UFINFORM == 22] <- "Piauí"
    data$UFINFORM[data$UFINFORM == 23] <- "Ceará"
    data$UFINFORM[data$UFINFORM == 24] <- "Rio Grande do Norte"
    data$UFINFORM[data$UFINFORM == 25] <- "Paraíba"
    data$UFINFORM[data$UFINFORM == 26] <- "Pernambuco"
    data$UFINFORM[data$UFINFORM == 20] <- "Pernambuco"
    data$UFINFORM[data$UFINFORM == 27] <- "Alagoas"
    data$UFINFORM[data$UFINFORM == 28] <- "Sergipe"
    data$UFINFORM[data$UFINFORM == 29] <- "Bahia"
    data$UFINFORM[data$UFINFORM == 31] <- "Minas Gerais"
    data$UFINFORM[data$UFINFORM == 32] <- "Espírito Santo"
    data$UFINFORM[data$UFINFORM == 33] <- "Rio de Janeiro"
    data$UFINFORM[data$UFINFORM == 35] <- "São Paulo"
    data$UFINFORM[data$UFINFORM == 41] <- "Paraná"
    data$UFINFORM[data$UFINFORM == 42] <- "Santa Catarina"
    data$UFINFORM[data$UFINFORM == 43] <- "Rio Grande do Sul"
    data$UFINFORM[data$UFINFORM == 50] <- "Mato Grosso do Sul"
    data$UFINFORM[data$UFINFORM == 51] <- "Mato Grosso"
    data$UFINFORM[data$UFINFORM == 52] <- "Goiás"
    data$UFINFORM[data$UFINFORM == 53] <- "Distrito Federal"
    data$UFINFORM[data$UFINFORM == 99] <- NA
    data$UFINFORM <- factor(data$UFINFORM)
  }
  
  # CODINST.CNV
  if ("CODINST" %in% variables_names) {
    data$CODINST <- as.character(levels(data$CODINST))[data$CODINST]
    data$CODINST[data$CODINST == "E"] <- "Estado"
    data$CODINST[data$CODINST == "R"] <- "Regional"
    data$CODINST[data$CODINST == "M"] <- "Municipal"
    data$CODINST <- factor(data$CODINST)
  }
  
  # Purge levels
  data <- droplevels(data)
  
  # Remove objects
  rm(ano, unidade)
  
  # Return
  return(data)
  
}