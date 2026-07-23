# microdatasus

<!-- badges: start -->
[![R-CMD-check](https://github.com/rfsaldanha/microdatasus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rfsaldanha/microdatasus/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/microdatasus)](https://CRAN.R-project.org/package=microdatasus)
<!-- badges: end -->

Este pacote para o R apresenta funções para download e pré-processamento dos arquivos de microdados do DataSUS (formato *DBC*). Nesta última etapa, os rótulos e formato das variáveis são atribuídos e tratados.

Para saber mais sobre os Sistemas de Informação em Saúde do Brasil, [acesse o e-book](https://rfsaldanha.github.io/sis/).

## Instalação

```r
# Versão estável
install.packages("microdatasus")

# Versão de desenvolvimento
# install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus")
```

A leitura de arquivos DBC foi internalizada e não depende mais do pacote
`read.dbc`. A implementação foi adaptada do pacote
[healthbR](https://github.com/SidneyBissoli/healthbR).

No Windows, a instalação da versão de desenvolvimento pelo GitHub requer uma
versão do [Rtools](https://cran.r-project.org/bin/windows/Rtools/) compatível
com a versão instalada do R. A versão binária distribuída pelo CRAN não requer
ferramentas de compilação.

## Utilização

A utilização do pacote consiste, em geral, no uso de duas funções: uma realiza
o download dos dados e outra faz o pré-processamento.

### Exemplo

```r
library(microdatasus)
dados <- fetch_datasus(year_start = 2013, year_end = 2014, uf = "RJ", information_system = "SIM-DO")
dados <- process_sim(dados)
```

## Sistemas de Informação em Saúde suportados

* Download: SIM, SINASC, SIH, CNES, SIA, SINAN-DENGUE, SINAN-CHIKUNGUNYA, SINAN-ZIKA, SINAN-MALARIA, SINAN-CHAGAS, SINAN-LEISHMANIOSE-VISCERAL, SINAN-LEISHMANIOSE-TEGUMENTAR, SINAN-LEPTOSPIROSE.
* Pré-processamento: SIM, SINASC, SIH, CNES, SIA, SINAN-DENGUE, SINAN-CHIKUNGUNYA, SINAN-ZIKA, SINAN-MALARIA, SINAN-CHAGAS, SINAN-LEISHMANIOSE-TEGUMENTAR, SINAN-LEISHMANNIOSE-VISCERAL.

## Agradecimento

O desenvolvimento e manutenção deste pacote não seria possível sem os pacotes [*read.dbc*](https://CRAN.R-project.org/package=read.dbc), criado por Daniela Petruzalek, e [*healthbR*](https://github.com/SidneyBissoli/healthbR), criado por Sidney Bissoli.

## Como citar

Peço aos usuários que citem o pacote sempre que ele for utilizado.

> SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pré-processamento de microdados do Departamento de Informática do SUS (DATASUS). Cad. Saúde Pública,  Rio de Janeiro ,  v. 35, n. 9,  e00032419,    2019.  Available from <https://doi.org/10.1590/0102-311x00032419>.


## Dúvidas e sugestões

Crie uma [issue](https://github.com/rfsaldanha/microdatasus/issues) no projeto ou envie um e-mail para `raphael.saldanha@fiocruz.br`
