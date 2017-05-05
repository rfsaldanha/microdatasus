# downloadDataSUS
Download de dados do DataSUS no R

## Objetivo

Pacote no R para baixar dados do DataSUS, transformá-los em um data.frame através do pacote read.dbc e realizar pré-processamento nos dados.

## Download

A função download aceita como argumentos o período de tempo desejado, o sistema de informações, unidade federativa (quando possível), e uma lista de variáveis.

## Pré-processamento

Prepara o data.frame dos dados recém baixados para uso, informando rótulos de fatores e tratando campos de data e idade, entre outras ações.
