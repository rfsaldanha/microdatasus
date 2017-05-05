# funPreProcessa
# Pré-processamento de dados do DataSUS

funPreProcessa <- function(data, sistema){
  # Pacotes necessários
  require(lubridate)
  require(data.table)
  
  # Verifica sistema
  sistemas <- c("SIM")
  if(!(sistema %in% sistemas)) stop("Sistema de informação desconhecido!")
  
  # Carrega tabelas de refência
  tabNaturalidade <- read.csv2("tabNaturalidade.csv", colClasses = c("character","character"))
  
  data <- dadosSIM ### Remover
  
  
  
  # Campos
  campos <- names(data)
  
  # Trata campos
  
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
    data$NATURAL <- merge(data.frame(cod=data$NATURAL), tabNaturalidade, all.x = TRUE)
  }
  
  # Purge de levels não utilizados
  data <- droplevels(data)
  
  # Retorna resultado
  return(data)
}