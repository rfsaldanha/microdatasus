# downloadDataSUS

O pacote para o R *downloadDataSUS* apresenta funções para download dos arquivos de microdados do DataSUS (formato *DBC*), leitura dos arquivos através do pacote [*read.dbc*](https://cran.r-project.org/web/packages/read.dbc/index.html) e pré-processamento para utilização.

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

Download de dados do SINASC dos anos de 2013, estado do Rio de Janeiro e Espírito Santo.

```r
dados <- datasusFetch(anoIni = 2013, anoFim = 2013, uf = c("RJ","ES"), sistema = "SINASC")
```

Download de dados do SINASC do ano de 2014, todos os estados.

```r
dados <- datasusFetch(anoIni = 2014, anoFim = 2014, uf = "all", sistema = "SINASC")
```

Download de dados do SIM do ano de 2013, apenas do estado do Rio de Janeiro, variáveis selecionadas. Apesar de ser possível filtrar as variáveis desejadas após o download, a vantagem deste método é reduzir a necessidade de memória. O dicionário de variáveis encontra-se no final desta página.

```r
dados <- datasusFetch(anoIni = 2013, anoFim = 2014, uf = "RJ", sistema = "SIM", vars = c("SEXO", "CAUSABAS"))
```

Download de dados do SIH (AIH reduzida) entre abril de 2015 a maio de 2016, estado do Rio de Janeiro, todas as variáveis.

```r
dados <- datasusFetch(anoIni = 2015, mesIni = 4, anoFim = 2016, mesFim = 5, uf = "RJ", sistema = "SIH-RD")
```

Além das AIH reduzida (`sistema = "SIH-RD"`), é possível realizar o download das AIH rejeitadas (`sistema = "SIH-RJ"`), AIH de serviços profissionais (`sistema = "SIH-SP"`) e AIH rejeitadas com código de erro (`sistema = "SIH-ER"`).

### Pré-processamento

Prepara o `data.frame` dos dados recém baixados para uso, informando rótulos de fatores e tratando campos de data e idade entre outras ações, conforme a documentação do sistema de informações.

```r
dados <- datasusProcess(dados, "SIM")
```
```r
dados <- datasusProcess(dados, "SINASC")
```
```r
dados <- datasusProcess(dados, "SIH-RD")
```

Por padrão, a função `datasusProcess` inclui informações referentes ao município de **residência** (nome completo, latitude e longitude da sede e outras). Para não incluir estas informações use o argumento `dadosMunRes = FALSE`.

```r
dados <- datasusProcess(dados, "SIM", dadosMunRes = FALSE)
```

**O processamento dos dados de SIH-RJ, SIH-SP e SIH-ER não estão implementados.**

### Notas de utilização

* Ao tentar realizar o download de dados de um período em que haja dados não disponíveis ou se ocorrer  alguma falha na Internet durante o processo de download, a função `datasusFetch` irá realizar o download dos dados que estejam disponíveis e informar quais arquivos não puderam ser baixados. **Esteja atento às mensagens do processo de download**.
* O download dos arquivos DBC é realizado em uma pasta temporária do sistema e apagados assim que são lidos pela função `read.dbc`.
* A especificação da Unidade Federativa através do argumento `uf` realiza o download dos arquivos conforme separados pelo DataSUS. Para obter dados de acordo com o local de **residência**, é recomendável realizar o download de todos os estados (`uf = "all"`) e filtrar posteriormente os dados de acordo com o município ou estado de residência.
* Alguns sistemas de informação apresentam variáveis diferentes no decorrer dos anos. Ao realizar o download de dados nestes períodos, a função `datasusFetch` acrescenta as variáveis novas preenchendo com `NA` os registros de anos em que ela não existia (através da função `plyr::rbind.fill`).
* Variável **idade** no SIM. A função `datasusProcess` cria três campos de idade: `IDADEhoras`, `IDADEdias`, `IDADEmeses` e `IDADEanos` de acordo com o código de tipo de idade informado pelo DataSUS.
* Variável **idade** no SIH. Atentar para código da idade (`COD_IDADE`).
* Os nomes de variáveis foram mantidos conforme o original do DataSUS sempre que possível. Apenas o campo `IDADE` sofre alterações. 

### Dicionário de variáveis do DataSUS

* [SIM](https://goo.gl/fO7uuK)
* [SINASC](https://goo.gl/csl8x1)
* [SIH](https://goo.gl/j0bZZV)

## Agradecimento

O desenvolvimento deste pacote não seria possível sem a função `read.dbc` criada por Daniela Petruzalek, através do pacote [*read.dbc*](https://cran.r-project.org/web/packages/read.dbc/index.html).

## Como citar

Peço aos usuários que citem o pacote sempre que possível.

SALDANHA, Raphael de Freitas. Pacote para o R 'downloadDataSUS'. Disponível em <https://github.com/rfsaldanha/downloadDataSUS>.

## Dúvidas e sugestões

Crie uma [issue](https://github.com/rfsaldanha/downloadDataSUS/issues) no projeto ou envie um e-mail para `rfsaldanha@gmail.com`
