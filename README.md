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
dados <- datasus_fetch(anoIni = 2013, anoFim = 2014, uf = "RJ", sistema = "SIM-DO")
dados <- process_sim(dados, "SIM-DO")
```

O manual para a utilização do pacote encontra-se na [Wiki do projeto](https://github.com/rfsaldanha/downloadDataSUS/wiki).

### Sistemas de Informação em Saúde suportados
* Download: SIM, SINASC, SIH, CNES, SIA.
* Pré-processamento: SIM, SINASC, SIH-RD.

## Agradecimento

O desenvolvimento deste pacote não seria possível sem a função `read.dbc` criada por Daniela Petruzalek, através do pacote [*read.dbc*](https://cran.r-project.org/web/packages/read.dbc/index.html).

## Como citar

Peço aos usuários que citem o pacote sempre que ele for utilizado.

SALDANHA, Raphael de Freitas. Pacote para o R 'downloadDataSUS', 2017. Disponível em <https://github.com/rfsaldanha/downloadDataSUS>.

## Dúvidas e sugestões

Crie uma [issue](https://github.com/rfsaldanha/downloadDataSUS/issues) no projeto ou envie um e-mail para `raphael.saldanha@icict.fiocruz.br`
