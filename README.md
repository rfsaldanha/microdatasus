# microdatasus

<!-- badges: start -->
[![R-CMD-check](https://github.com/rfsaldanha/microdatasus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rfsaldanha/microdatasus/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/microdatasus)](https://CRAN.R-project.org/package=microdatasus)
<!-- badges: end -->

O `microdatasus` baixa, lê e prepara microdados dos Sistemas de Informação em
Saúde publicados pelo DataSUS. O pacote trabalha diretamente com arquivos DBC,
seleciona os arquivos efetivamente disponíveis no servidor e oferece funções
específicas para recodificar variáveis de SIM, SINASC, SIH, SIA, CNES e SINAN.

O pacote resolve a parte operacional do acesso aos dados. Para compreender a
unidade de análise, a cobertura, os fluxos de produção e os cuidados de
interpretação de cada sistema, consulte o livro
[*Sistemas de Informação em Saúde no Brasil*](https://rfsaldanha.github.io/sis/).

## O que o pacote oferece

- `fetch_datasus()` localiza, baixa e combina microdados publicados pelo
  DataSUS; também pode processar e salvar cada arquivo separadamente.
- `read_dbc()` lê um arquivo DBC já disponível no computador.
- As funções `process_*()` usam os dicionários oficiais do TabWin, padronizam
  tipos e podem relatar códigos ainda não mapeados.
- `datasus_variables()` consulta relações oficiais, `datasus_schema()` cria
  contratos por campo, `validate_datasus_schema()` confronta DBC, DEF e tipos
  produzidos, e `audit_datasus_dictionaries()` verifica todas as
  definições e `compare_datasus_dictionary()` identifica mudanças entre versões.
- `fetch_cadger()` e `fetch_sigtab()` obtêm tabelas auxiliares atuais de CNES e
  SIA.
- `datasus_reference_tables()` torna explícita a origem das tabelas legadas;
  `datasus_lockfile()` registra e `verify_datasus_lockfile()` confere os
  arquivos usados em uma análise.

Os downloads fazem novas tentativas em falhas transitórias e podem usar um
cache persistente, com checksum e proveniência. Quando
`stop_on_error = FALSE`, os arquivos válidos podem ser retornados mesmo que
parte da solicitação falhe.

## Instalação

Instale a versão estável publicada no CRAN:

```r
install.packages("microdatasus")
```

Ou instale a versão de desenvolvimento:

```r
# install.packages("remotes")
remotes::install_github("rfsaldanha/microdatasus", ref = "dev")
```

A leitura de DBC é interna ao pacote e não depende mais de `read.dbc`. A
implementação foi adaptada do pacote
[healthbR](https://github.com/SidneyBissoli/healthbR).

No Windows, a instalação pelo GitHub requer uma versão do
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) compatível com o R
instalado. A versão binária distribuída pelo CRAN não requer ferramentas de
compilação.

## Primeiro download

O fluxo mais comum tem duas etapas: baixar os arquivos e aplicar o processador
do sistema.

```r
library(microdatasus)

sim_raw <- fetch_datasus(
  year_start = 2022,
  year_end = 2022,
  uf = "AC",
  information_system = "SIM-DO",
  vars = c("CODMUNRES", "DTOBITO", "CAUSABAS"),
  track_source = TRUE
)

sim <- process_sim(sim_raw)
```

`process_sim()` usa `SIM-DO` por padrão. Para os subconjuntos nacionais,
informe o mesmo tipo usado no download, por exemplo
`process_sim(sim_fetal_raw, information_system = "SIM-DOFET")`. O resultado
padroniza datas como `Date`, quantidades como inteiros e variáveis rotuladas
como fatores.

Os anos, meses e UFs solicitados identificam as partições publicadas pelo
DataSUS. A data e o local analíticos devem ser escolhidos nas variáveis do
registro. O [capítulo sobre o
SIM](https://rfsaldanha.github.io/sis/sim.html), por exemplo, distingue
residência, ocorrência e outras dimensões territoriais e temporais do óbito.

Sistemas mensais, como SIH, SIA e CNES, exigem os meses inicial e final:

```r
sih_raw <- fetch_datasus(
  year_start = 2023,
  month_start = 1,
  year_end = 2023,
  month_end = 2,
  uf = c("AC", "RO"),
  information_system = "SIH-RD",
  timeout = 600
)

sih <- process_sih(sih_raw)
```

## Sistemas suportados

| Sistema | Periodicidade dos arquivos | Download | Processamento | Livro de SIS |
|---|---|---|---|---|
| SIM | Anual | `SIM-DO`, `SIM-DOFET`, `SIM-DOEXT`, `SIM-DOINF`, `SIM-DOMAT` | `process_sim()` | [SIM](https://rfsaldanha.github.io/sis/sim.html) |
| SINASC | Anual | `SINASC` | `process_sinasc()` | [SINASC](https://rfsaldanha.github.io/sis/sinasc.html) |
| SIH | Mensal | `SIH-RD`, `SIH-RJ`, `SIH-SP`, `SIH-ER` | `process_sih()` | [SIH](https://rfsaldanha.github.io/sis/sih.html) |
| SIA | Mensal | Doze layouts `SIA-*` | `process_sia()` | [SIA](https://rfsaldanha.github.io/sis/sia.html) |
| CNES | Mensal | Treze layouts `CNES-*` | `process_cnes()` para os treze layouts | [CNES](https://rfsaldanha.github.io/sis/cnes.html) |
| SINAN | Anual e nacional | 58 famílias oficiais `SINAN-*` | `process_sinan()` para as 58 famílias | [SINAN](https://rfsaldanha.github.io/sis/sinan.html) |

Use `datasus_information_systems()` para consultar todos os 93 valores aceitos
em `information_system`, seus nomes, periodicidade, abrangência, siglas usadas
nos arquivos DBC e aliases.

A lista completa dos identificadores está na
[referência de `fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.html).

## Controle do download

Algumas opções úteis de `fetch_datasus()`:

- `vars` limita as colunas lidas e reduz o uso de memória.
- `track_source = TRUE` acrescenta o nome do DBC de origem de cada registro.
- `timeout` controla o limite de cada operação de rede, sem alterar opções
  globais do R.
- o nome do arquivo, o progresso da transferência, a leitura e o resumo final
  são exibidos; use `quiet = TRUE` para ocultar o progresso e todas as mensagens
  de status.
- `stop_on_error = FALSE` preserva sucessos parciais; `TRUE` interrompe a
  solicitação na primeira falha.
- `uf` aceita uma UF, várias UFs ou `"all"`. Arquivos nacionais, como os do
  SINAN, ignoram esse argumento com um alerta.

Consulte o artigo [Download e
rastreabilidade](https://rfsaldanha.github.io/microdatasus/articles/download-e-rastreabilidade.html)
para exemplos de todas essas opções.

## Dicionários, cache e tabelas grandes

Um diretório explícito reutiliza DBC e ZIP do TabWin entre sessões:

```r
cache <- datasus_cache_dir(create = TRUE)
variables <- datasus_variables("SIM-DO", cache_dir = cache)
schema <- datasus_schema("SIM-DO", cache_dir = cache)
contract <- validate_datasus_schema(sim_do_sample, "SIM-DO", period = 2020, cache_dir = cache)
audit <- audit_datasus_dictionaries(c("SIM-DO", "SINASC"), cache_dir = cache)
datasus_cache_info(cache)
```

Os processadores aceitam `labels = "factor"`, `"character"` ou `"none"`.
Com `diagnostics = TRUE`, `processing_diagnostics()` informa códigos ausentes
das conversões, campos esperados ou não mapeados, falhas de coerção e a
proveniência — fonte, definição e checksum — de cada dicionário usado.

Para solicitações grandes, combine `destination`, `collect = FALSE` e
`process = TRUE`: cada DBC é processado e gravado antes da leitura do próximo.
O retorno é um manifesto com caminhos, número de linhas, origem e checksum.

Use `row_filter` para descartar linhas de cada DBC antes do processamento e
reduzir memória e tempo. Novos manifests usam SHA-256; espelhos HTTP/FTP podem
ser informados por `options(microdatasus.mirrors = c("https://..."))`.
Com `provenance = TRUE`, `datasus_lockfile(dados, "datasus.lock.rds")`
registra a consulta, os DBC e os dicionários usados.

O guia [Dicionários, cache e processamento em
escala](https://rfsaldanha.github.io/microdatasus/articles/dicionarios-cache-e-escala.html)
apresenta o fluxo completo. O suporte do SIM nesta versão é restrito a CID-10.

## Arquivos DBC locais

Use `read_dbc()` quando o arquivo já estiver no computador:

```r
dados <- read_dbc("arquivo.dbc")

# Preserva os tipos inferidos dos metadados DBF
dados_tipados <- read_dbc("arquivo.dbc", as_character = FALSE)

# Lê somente as colunas necessárias, sem alocar as demais
dados_selecionados <- read_dbc(
  "arquivo.dbc",
  vars = c("CODMUNRES", "DTOBITO")
)

# Para arquivos cujo marcador de code page esteja ausente ou incorreto
dados_latin1 <- read_dbc("arquivo.dbc", encoding = "latin1")

# Alguns arquivos históricos usam a página de código DOS CP850
dados_cp850 <- read_dbc("arquivo_historico.dbc", encoding = "CP850")
```

`read_dbc()` descomprime e interpreta os registros diretamente, sem criar um
DBF intermediário. O texto é convertido para UTF-8; sequências inválidas geram
um erro explícito em vez de substituição silenciosa. `encoding = "auto"` usa o
marcador DBF e assume Windows-1252 nos arquivos sem marcador; use o argumento
explícito para arquivos históricos em CP850 ou outra codificação.

## Guias

- [Primeiros passos e exemplos por
  sistema](https://rfsaldanha.github.io/microdatasus/articles/exemplos.html)
- [Download e
  rastreabilidade](https://rfsaldanha.github.io/microdatasus/articles/download-e-rastreabilidade.html)
- [Dicionários, cache e processamento em
  escala](https://rfsaldanha.github.io/microdatasus/articles/dicionarios-cache-e-escala.html)
- [Perguntas
  frequentes](https://rfsaldanha.github.io/microdatasus/articles/FAQ.html)
- [Livro *Sistemas de Informação em Saúde no
  Brasil*](https://rfsaldanha.github.io/sis/)

## Como citar

Ao utilizar o pacote, cite:

> SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam.
> Microdatasus: pacote para download e pré-processamento de microdados do
> Departamento de Informática do SUS (DATASUS). *Cadernos de Saúde Pública*,
> v. 35, n. 9, e00032419, 2019.
> <https://doi.org/10.1590/0102-311x00032419>.

Quando o livro apoiar a descrição ou a interpretação dos sistemas, cite também:

> SALDANHA, Raphael de Freitas. *Sistemas de Informação em Saúde no Brasil*.
> Rio de Janeiro: Edição do autor, 2026. ISBN 978-65-01-37841-1.
> <https://rfsaldanha.github.io/sis/>.

## Agradecimentos

O suporte a DBC foi construído a partir de contribuições de código aberto dos
projetos [read.dbc](https://CRAN.R-project.org/package=read.dbc), de Daniela
Petruzalek, e [healthbR](https://github.com/SidneyBissoli/healthbR), de Sidney
Bissoli.

## Dúvidas e sugestões

Crie uma [issue](https://github.com/rfsaldanha/microdatasus/issues) ou envie um
e-mail para `raphael.saldanha@fiocruz.br`.
