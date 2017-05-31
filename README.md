# downloadDataSUS

O pacote para o R *downloadDataSUS* apresenta funções para download dos arquivos de microdados do DataSUS (formato *DBC*), leitura dos arquivos através do pacote [*read.dbc*](https://cran.r-project.org/web/packages/read.dbc/index.html) e pré-processamento para utilização. Nesta última etapa, os rótulos e formato das variáveis são atribuídos e tratados.

## Novidades

* 31/05/2017: implementação completa do SIM

## Instalação

A versão de *desenvolvimento* pode ser instalada no R desta forma:

```r
install.packages("devtools")
devtools::install_github("rfsaldanha/downloadDataSUS")
```

## Utilização

O pacote consiste de suas funções básicas: `datasusFetch` para o download de dados e `datasusProcess` para o processamento dos dados baixados.

### Exemplo

```r
library(downloadDataSUS)
dados <- datasusFetch(anoIni = 2013, anoFim = 2014, uf = "RJ", sistema = "SIM-DO")
dados <- datasusProcess(dados, "SIM-DO")
```

O manual para a utilização do pacote encontra-se na [Wiki do projeto](https://github.com/rfsaldanha/downloadDataSUS/wiki).

## Agradecimento

O desenvolvimento deste pacote não seria possível sem a função `read.dbc` criada por Daniela Petruzalek, através do pacote [*read.dbc*](https://cran.r-project.org/web/packages/read.dbc/index.html).

## Como citar

Peço aos usuários que citem o pacote sempre que ele for utilizado.

SALDANHA, Raphael de Freitas. Pacote para o R 'downloadDataSUS', 2017. Disponível em <https://github.com/rfsaldanha/downloadDataSUS>.

## Dúvidas e sugestões

Crie uma [issue](https://github.com/rfsaldanha/downloadDataSUS/issues) no projeto ou envie um e-mail para `rfsaldanha@gmail.com`
