# Dicionários, cache e processamento em escala

O `microdatasus` pode usar diretamente os arquivos DEF, CNV e DBF
publicados para o TabWin. O suporte atual do SIM considera apenas os
arquivos em CID-10. As regras sintáticas, de precedência e de
recuperação auditável são detalhadas em [Formatos DBC, DEF e
CNV](https://rfsaldanha.github.io/microdatasus/articles/formatos-dbc-def-cnv.md).

## Consultar e comparar dicionários

[`datasus_variables()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_variables.md)
apresenta campos, tipos, arquivos de relação e, opcionalmente, pares
código-rótulo e intervalos simbólicos. Relações muito grandes não são
descartadas nem expandidas em memória: `status`, `labels_complete` e
`range_rules` tornam explícito o que foi interpretado. Um diretório
explícito mantém o ZIP, seu manifesto e as relações já analisadas entre
sessões; a tabela montada também é reutilizada durante a sessão.

``` r

library(microdatasus)
cache <- datasus_cache_dir(create = TRUE)
options(microdatasus.cache_dir = cache)

sim_variables <- datasus_variables(
  "SIM-DO",
  cache_dir = cache
)

sim_variables[, c(
  "field", "type", "description", "categories",
  "range_rules", "status"
)]

# Restringe o trabalho e agrupa declarações repetidas por campo
datasus_variables(
  "SIM-DO", fields = c("SEXO", "CAUSABAS"),
  view = "fields", cache_dir = cache
)
datasus_cache_info(cache)
```

`datasus_cache_dir(create = TRUE)` cria e retorna o diretório, mas não
ativa o cache por si só. A opção acima faz com que chamadas diretas a
`process_*()` usem esse caminho. Em
`fetch_datasus(process = TRUE, cache_dir = cache)`, o diretório
informado já é repassado ao processamento daquela chamada.

É possível guardar essa tabela como referência e compará-la com uma
publicação posterior do DataSUS:

``` r

reference <- sim_variables
changes <- compare_datasus_dictionary(
  "SIM-DO",
  previous = reference,
  cache_dir = cache,
  refresh = TRUE
)
```

`changes` informa inclusões, remoções e mudanças em campos ou rótulos.
Nenhuma diferença é inventada: uma tabela vazia significa que os dois
conteúdos comparados são equivalentes.

Contratos derivados do DEF e auditorias permitem verificar um sistema,
um grupo ou todas as 105 definições atuais e históricas. Na auditoria
completa, os 15 ZIP físicos compartilhados são baixados apenas uma vez
por cache. Esses números são derivados do registro interno atual do
pacote.

``` r

schema <- datasus_schema("SINAN-DENGUE", inspect = TRUE, cache_dir = cache)

contract <- validate_datasus_schema(
  sim_do_sample, "SIM-DO", period = 2020, cache_dir = cache
)

audit <- audit_datasus_dictionaries(
  c("SIM-DO", "SINASC", "SIH-RD", "SIA-PA", "CNES-ST",
    "SINAN-DENGUE"),
  cache_dir = cache
)

# Auditoria completa, indicada para CI ou manutenção periódica
all_dictionaries <- audit_datasus_dictionaries(cache_dir = cache)
```

As colunas `status` e `issues` distinguem ausência de arquivo, conteúdo
inválido, relação não enumerável e falha de leitura; essas situações não
são convertidas silenciosamente em tabelas vazias.

Duplicidades seguem a precedência do TabWin: a última definição física
de um código CNV ou de uma chave DBF relacionada é a efetiva. Um
conflito em DBF ou uma contagem de categorias CNV incompatível com as
linhas físicas é mantido como `fallback` auditável, não como sucesso
indistinguível. Correções de erros conhecidos nos arquivos oficiais são
exatas e limitadas ao nome do arquivo; relações ambíguas continuam
inválidas.

## Rótulos e diagnóstico

Todas as funções `process_*()` aceitam a mesma política de rótulos:
`"factor"` (padrão), `"character"` ou `"none"`. O diagnóstico opcional
registra campos tratados, códigos ausentes, campos esperados ou não
mapeados, falhas de coerção e a fonte, definição e checksum dos
dicionários efetivamente usados.

``` r

library(microdatasus)

sim <- process_sim(
  sim_do_sample,
  municipality_data = FALSE,
  labels = "character",
  diagnostics = TRUE
)

diagnostic <- processing_diagnostics(sim)
diagnostic$unknown_codes
diagnostic$missing_expected_fields
diagnostic$dictionaries
diagnostic$coercion_failures
```

Os argumentos antigos e seus padrões foram mantidos; os novos argumentos
foram acrescentados ao fim das assinaturas.

## Desempenho das funções de processamento

As otimizações são usadas automaticamente pelas seis funções
`process_*()`:

- Datas repetidas são analisadas uma vez por campo e formato e
  remapeadas às linhas originais. Datas inválidas e códigos de ausência
  continuam seguindo as regras de cada sistema, e o diagnóstico conta
  todas as ocorrências.
- O desescape de texto só é executado nos valores com barras invertidas.
  A conversão para UTF-8 continua ativa; identificadores com codificação
  `"bytes"` são preservados pela normalização textual.
- O preenchimento de códigos e a busca dos limites superiores do modo
  CNV `F` são vetorizados, conservando larguras, limites inclusivos e
  precedência.
- A seleção de relações históricas recebe os índices das linhas e acessa
  as colunas necessárias à relação, evitando copiar toda a tabela para
  cada campo.

O ganho depende do número de colunas, dos valores distintos, dos
períodos representados e das opções escolhidas. Uma tabela com muitas
datas repetidas pode se beneficiar mais do que outra dominada por texto
livre.

Escolha as opções pelo resultado que a análise precisa:

| Opção | Efeito e limite |
|----|----|
| `labels = "none"` | Mantém os códigos categóricos; datas e quantidades continuam sendo convertidas. Dicionários ainda podem ser necessários. |
| `labels = "character"` | Retorna descrições como texto. Não é garantia de menor tempo ou memória do que fatores. |
| `municipality_data = FALSE` | Omite o acréscimo de nomes e atributos territoriais. |
| `diagnostics = FALSE` | Evita construir o relatório opcional; use `TRUE` quando precisar auditar a execução ou registrar seus dicionários no lockfile. |
| `nomes = FALSE` no CNES | Omite a busca de nomes dos estabelecimentos. |
| `nome_proced`, `nome_ocupacao`, `nome_equipe` no SIA | Permitem omitir os grupos de descrições correspondentes. |

`labels = "none"` não significa processamento sem dicionários: SIA, CNES
e SINAN consultam DEFs para distinguir campos e podem avaliar relações
mesmo sem substituir os códigos. Quando um dicionário necessário ainda
não estiver no cache, haverá acesso à rede. Também não existe um
argumento `cache_dir` nas funções `process_*()`; para chamadas diretas,
use a opção do pacote.

## Medir com os próprios dados

Separe a leitura do DBC, a primeira chamada do processador e as chamadas
seguintes. O exemplo mede SIM-DO com rótulos e diagnósticos, usando
sempre a mesma entrada bruta. Ajuste a função e suas opções para o
sistema em análise.

``` r

medir_sim <- function(brutos, cache) {
  opcoes <- options(microdatasus.cache_dir = cache)
  on.exit(options(opcoes), add = TRUE)

  executar <- function() {
    process_sim(
      brutos,
      information_system = "SIM-DO",
      municipality_data = FALSE,
      labels = "factor",
      diagnostics = TRUE
    )
  }

  gc()
  primeira <- system.time(resultado <- executar())[["elapsed"]]
  seguintes <- replicate(3L, {
    gc()
    system.time(executar())[["elapsed"]]
  })

  list(
    linhas = nrow(brutos),
    colunas = ncol(brutos),
    primeira_chamada_s = unname(primeira),
    mediana_seguintes_s = unname(median(seguintes)),
    tamanho_resultado_mib = as.numeric(object.size(resultado)) / 1024^2,
    diagnostico = processing_diagnostics(resultado),
    sessao = sessionInfo()
  )
}

brutos <- read_dbc("arquivo_sim_do.dbc")
medicao <- medir_sim(brutos, cache)
medicao[c("linhas", "colunas", "primeira_chamada_s", "mediana_seguintes_s")]
```

A primeira chamada só mede cache vazio se os arquivos e relações ainda
não estiverem disponíveis; ela pode incluir download, extração e análise
dos dicionários. O tamanho de `resultado` mede apenas o objeto final,
não o pico de memória. A entrada, os dicionários e os objetos
temporários também ocupam memória durante o processamento. Compare
versões com os mesmos DBCs, dicionários e opções, preservando os dados
brutos para cada execução.

O benchmark do repositório replica amostras empacotadas e usa
dicionários vazios para evitar rede. Ele desativa rótulos e municípios
nos seis processadores e testa conversões separadamente. Serve para
detectar regressões, mas não estima o tempo de uma execução completa com
dicionários oficiais, layouts misturados ou arquivos de alta
cardinalidade. Veja as instruções em
[benchmarks/README.md](https://github.com/rfsaldanha/microdatasus/blob/dev/benchmarks/README.md).

## Downloads grandes

Para limitar o uso de memória,
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
pode processar e salvar cada DBC separadamente. Com `collect = FALSE`, o
pacote evita acumular os resultados de todos os arquivos. Ainda é
necessário espaço para um DBC descomprimido, seus dicionários e as
cópias temporárias do processamento; um arquivo nacional muito grande
pode continuar exigindo bastante memória.

``` r

manifest <- fetch_datasus(
  year_start = 2022,
  year_end = 2023,
  uf = c("AC", "AM"),
  information_system = "SIM-DO",
  cache_dir = cache,
  destination = "dados/sim",
  collect = FALSE,
  process = TRUE,
  process_args = list(
    municipality_data = FALSE,
    labels = "character",
    diagnostics = TRUE
  )
)

manifest[, c("file", "rows", "checksum", "data_path")]
```

Com `collect = TRUE`, o comportamento histórico é preservado. Use
`provenance = TRUE` para anexar o manifesto à tabela combinada e
[`datasus_provenance()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_provenance.md)
para recuperá-lo.

O argumento `row_filter` recebe uma função lógica executada em cada DBC
bruto, antes de rótulos e conversões. Ela deve devolver um valor lógico
por linha, sem `NA`. Isso limita o custo do processamento sem alterar
quais arquivos são baixados. Para registrar também os dicionários
usados, habilite os diagnósticos:

``` r

dados <- fetch_datasus(
  2022, year_end = 2022, uf = "AC", information_system = "SIM-DO",
  row_filter = function(x) !is.na(x$SEXO) & x$SEXO == "1",
  process = TRUE,
  process_args = list(diagnostics = TRUE),
  provenance = TRUE,
  cache_dir = cache
)
datasus_lockfile(dados, "datasus.lock.rds")
verify_datasus_lockfile("datasus.lock.rds")
```

Sem `process` e sem `row_filter`, `vars` permite projetar a leitura nas
colunas solicitadas. Com qualquer uma dessas etapas, o layout completo é
lido, o filtro é aplicado, os dados são processados quando solicitado e
só então `vars` seleciona a saída. Para processar bases históricas,
mantenha os campos de competência e as colunas adjacentes necessárias às
relações DEF. Usar `process = TRUE` evita removê-los antes dessa etapa.

Novos manifests usam SHA-256 e os manifests MD5 de versões anteriores
continuam legíveis.
`options(microdatasus.mirrors = c("https://espelho.example"))` define
transportes alternativos.
[`datasus_reference_tables()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_reference_tables.md)
lista checksum, dimensões, papel e disponibilidade da data de origem das
tabelas empacotadas.

Para remover o conteúdo persistente gerenciado pelo pacote, use
`clear_datasus_cache(cache)`. A função preserva o diretório raiz e
arquivos que não pertençam ao `microdatasus`.
