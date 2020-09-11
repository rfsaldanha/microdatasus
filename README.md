# microdatasus

O pacote para o R *microdatasus* apresenta funções para download dos arquivos de microdados do DataSUS (formato *DBC*), leitura dos arquivos através do pacote [*read.dbc*](https://cran.r-project.org/web/packages/read.dbc/index.html) e pré-processamento para utilização. Nesta última etapa, os rótulos e formato das variáveis são atribuídos e tratados.

## Instalação

A versão de *desenvolvimento* pode ser instalada no R desta forma:

```r
install.packages("devtools")
devtools::install_github("rfsaldanha/microdatasus")
```

## Utilização

A utilizaço do pacote consiste, em geral, no uso de duas funções: uma realiza o download dos dados e outra o pré-processamento dos mesmos.

### Exemplo

```r
library(microdatasus)
dados <- fetch_datasus(year_start = 2013, year_end = 2014, uf = "RJ", information_system = "SIM-DO")
dados <- process_sim(dados)
```

O manual para a utilização do pacote encontra-se na [Wiki do projeto](https://github.com/rfsaldanha/microdatasus/wiki).

### Sistemas de Informação em Saúde suportados
* Download: SIM, SINASC, SIH, CNES, SIA.
* Pré-processamento: SIM, SINASC, SIH-RD.

## Agradecimento

O desenvolvimento deste pacote não seria possível sem a função `read.dbc` criada por Daniela Petruzalek, através do pacote [*read.dbc*](https://cran.r-project.org/web/packages/read.dbc/index.html).

## Como citar

Peço aos usuários que citem o pacote sempre que ele for utilizado.

SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pré-processamento de microdados do Departamento de Informática do SUS (DATASUS). Cad. Saúde Pública,  Rio de Janeiro ,  v. 35, n. 9,  e00032419,    2019 .   Available from <http://ref.scielo.org/dhcq3y>.


## Dúvidas e sugestões

Crie uma [issue](https://github.com/rfsaldanha/microdatasus/issues) no projeto ou envie um e-mail para `raphael.saldanha@icict.fiocruz.br`
