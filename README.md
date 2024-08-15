# microdatasus

<!-- badges: start -->
[![R-CMD-check](https://github.com/rfsaldanha/microdatasus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rfsaldanha/microdatasus/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/microdatasus)](https://CRAN.R-project.org/package=microdatasus)
<!-- badges: end -->

Este pacote para o R apresenta funções para download e pré-processamento dos arquivos de microdados do DataSUS (formato *DBC*). Nesta última etapa, os rótulos e formato das variáveis são atribuídos e tratados.

## Instalação

```r
# install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")
```

## Utilização

A utilizaço do pacote consiste, em geral, no uso de duas funções: uma realiza o download dos dados e outra o pré-processamento dos mesmos.

### Exemplo

```r
library(microdatasus)
dados <- fetch_datasus(year_start = 2013, year_end = 2014, uf = "RJ", information_system = "SIM-DO")
dados <- process_sim(dados)
```

## Sistemas de Informação em Saúde suportados

* Download: SIM, SINASC, SIH, CNES, SIA, SINAN-DENGUE, SINAN-CHIKUNGUNYA, SINAN-ZIKA, SINAN-MALARIA.
* Pré-processamento: SIM, SINASC, SIH-RD, CNES-ST, CNES-PF, SINAN-DENGUE, SINAN-CHIKUNGUNYA, SINAN-ZIKA, SINAN-MALARIA.

## Agradecimento

O desenvolvimento deste pacote não seria possível sem o pacote [*read.dbc*](https://cran.r-project.org/web/packages/read.dbc/index.html), criado por Daniela Petruzalek.

## Como citar

Peço aos usuários que citem o pacote sempre que ele for utilizado.

> SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pré-processamento de microdados do Departamento de Informática do SUS (DATASUS). Cad. Saúde Pública,  Rio de Janeiro ,  v. 35, n. 9,  e00032419,    2019.  Available from <https://doi.org/10.1590/0102-311x00032419>.


## Dúvidas e sugestões

Crie uma [issue](https://github.com/rfsaldanha/microdatasus/issues) no projeto ou envie um e-mail para `raphael.saldanha@fiocruz.br`
