# downloadDataSUS

O pacote para o R *downloadDataSUS* apresenta funções para download dos arquivos de microdados do DataSUS (formato *DBC*), leitura através do pacote [*read.dbc*](https://cran.r-project.org/web/packages/read.dbc/index.html) e pré-processamento.

## Instalação

A versão de desenvolvimento pode ser instalada no R desta forma:

```r
install.packages("devtools")
devtools::install_github("rfsaldanha/downloadDataSUS")
```

O pacote está nos primeiros estágios de desenvolvimento e será disponibilizado através do CRAN futuramente.

## Utilização

### Download

É possível fazer o download de dados do Sistema de Informações de Mortalidade (SIM), Sistema de Informações de Nascidos Vivos (SINASC) e do Sistema de Informações de Internações Hospitalares (SIH).

Download de dados do SIM dos anos de 2013 e 2014, apenas do estado do Rio de Janeiro.

```r
dados <- datasusFetch(anoIni = 2013, anoFim = 2014, uf = "RJ", sistema = "SIM")
```

Download de dados do SINASC do anos de 2013, estado do Rio de Janeiro e Espírito Santo.

```r
dados <- datasusFetch(anoIni = 2013, anoFim = 2013, uf = c("RJ","ES"), sistema = "SINASC")
```

Download de dados do SINASC do ano de 2014, todos os estados.

```r
dados <- datasusFetch(anoIni = 2014, anoFim = 2014, uf = "all", sistema = "SINASC")
```

Download de dados do SIM do ano de 2013, apenas do estado do Rio de Janeiro, variáveis selecionadas. Apesar de ser possível filtrar as variáveis desejados após o download, a vantagem deste método é reduzir a necessidade de memória. [Veja a lista de variáveis neste link](ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/Docs/Estrutura_SIM_para_CD.pdf).

```r
dados <- datasusFetch(anoIni = 2013, anoFim = 2014, uf = "RJ", sistema = "SIM", vars = c("SEXO", "CAUSABAS"))
```

Download de dados do SIH (AIH reduzida) entre abril de 2015 a maio de 2016, estado do Rio de Janeiro, todas as variáveis.

```r
dados <- datasusFetch(anoIni = 2015, mesIni = 4, anoFim = 2016, mesFim = 5, uf = "RJ", sistema = "SIH-RD")
```

### Pré-processamento

Prepara o `data.frame` dos dados recém baixados para uso, informando rótulos de fatores e tratando campos de data e idade entre outras ações, conforme a documentação do sistema de informações.

**Atualmente, apenas o SIM tem a opção de pré-processamento.**

```r
dados <- datasusProcess(dados, "SIM")
```

### Notas de utilização

* Ao tentar realizar o download de dados de um período em que haja dados não disponíveis ou se ocorrem  alguma falha da Internet durante o processo de download, a função `datasusFetch` irá realizar o download dos dados que estejam disponíveis e informar quais arquivos não puderam ser baixados. Esteja atento às mensagens do processo de download.
* O download dos arquvios DBC é realizado para uma pasta temporária do sistema e apagados assim que são lidos pela função `read.dbc`.
* A especificação da Unidade Federativa através do argumento `uf` realiza o download dos arquivos conforme separados pelo DataSUS, de acordo com o local de **ocorrência**. Para obter dados de acordo com o local de **residência**, faça o download de todos os estados (`uf = "all"`) e filtre posteriormente os dados de acordo com o município ou estado de residência.
* Alguns sistemas de informação apresentam variáveis diferentes no decorrer dos anos. Ao realizar o download de dados nestes períodos, a função `datasusFetch` acrescenta as variáveis novas preenchendo com `NA` os registros de anos em que ela não existia.
* O Sistema de Informações Hospitalares apresenta 4 subdivisões. O download pode ser feito desta maneira: AIH reduzida `sistema = "SIH-RD"`; AIH rejeitadas `sistema = "SIH-RJ"`; AIH serviços profisionais `sistema = "SIH-SP"`; AIH rejeitas com código de erro `sistema = "SIH-ER"`.
* Variável **idade**. A função `datasusProcess` cria três campos de idade: `IDADEhoras`, `IDADEdias`, `IDADEmeses` e `IDADEanos` de acordo com o código de tipo de idade informado pelo DataSUS.
* Os nomes de variáveis foram mantidos conforme o original do DataSUS sempre que possível. Apenas o campo `IDADE` sofre alterações. 

### Dicionário de variáveis do DataSUS

* [SIM](ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/Docs/Estrutura_SIM_para_CD.pdf)
* [SINASC](ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/NOV/Docs/Estrutura_SINASC_para_CD.pdf)
* [SIH](ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Doc/IT_SIHSUS_1603.pdf)
