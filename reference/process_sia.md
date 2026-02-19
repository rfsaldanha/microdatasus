# Process SIA variables from DataSUS

`process_sia` processes SIA variables retrieved by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

## Usage

``` r
process_sia(
  data,
  information_system = "SIA-PA",
  nome_proced = TRUE,
  nome_ocupacao = TRUE,
  nome_equipe = TRUE,
  municipality_data = TRUE
)
```

## Arguments

- data:

  `data.frame` created by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

- information_system:

  string. The abbreviation of the health information system. See
  *Details*.

- nome_proced:

  optional logical. `TRUE` by default, add `PA_PROCED_NOME` to the
  dataset. This setting will start to download a file from DataSUS to
  retrive the updates list of procesures (SIGTAB).

- nome_ocupacao:

  optional logical. `TRUE` by default, add `OCUPACAO` name to the
  dataset.

- nome_equipe:

  optional logical. `TRUE` by default, add `EQUIPE` name to the dataset.

- municipality_data:

  optional logical. `TRUE` by default, creates new variables in the
  dataset informing the full name and other details about the
  municipality of residence.

## Value

a `data.frame` with the processed data.

## Details

This function processes SIA variables retrieved by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md),
informing labels for categoric variables including NA values.

Currently, only "SIA-PA" is supported.

## Examples

``` r
process_sia(sia_pa_sample, nome_proced = FALSE)
#> # A tibble: 100 × 81
#>    PA_CODUNI PA_GESTAO PA_CONDIC PA_UFMUN PA_REGCT  PA_INCOUT PA_INCURG PA_TPUPS
#>    <chr>     <chr>     <chr>     <chr>    <chr>     <chr>     <chr>     <chr>   
#>  1 7334710   120000    EP        120020   SEM REGR… Sem incr… 0000      FARMACIA
#>  2 7334710   120000    EP        120020   SEM REGR… Sem incr… 0000      FARMACIA
#>  3 7334710   120000    EP        120020   SEM REGR… Sem incr… 0000      FARMACIA
#>  4 7334710   120000    EP        120020   SEM REGR… Sem incr… 0000      FARMACIA
#>  5 7334710   120000    EP        120020   SEM REGR… Sem incr… 0000      FARMACIA
#>  6 7334710   120000    EP        120020   SEM REGR… Sem incr… 0000      FARMACIA
#>  7 7334710   120000    EP        120020   SEM REGR… Sem incr… 0000      FARMACIA
#>  8 7334710   120000    EP        120020   SEM REGR… Sem incr… 0000      FARMACIA
#>  9 7334710   120000    EP        120020   SEM REGR… Sem incr… 0000      FARMACIA
#> 10 7334710   120000    EP        120020   SEM REGR… Sem incr… 0000      FARMACIA
#> # ℹ 90 more rows
#> # ℹ 73 more variables: PA_TIPPRE <chr>, PA_MN_IND <chr>, PA_CNPJCPF <chr>,
#> #   PA_CNPJMNT <chr>, PA_CNPJ_CC <chr>, PA_MVM <chr>, PA_CMP <chr>,
#> #   PA_PROC_ID <chr>, PA_TPFIN <chr>, PA_SUBFIN <chr>, PA_NIVCPL <chr>,
#> #   PA_DOCORIG <chr>, PA_AUTORIZ <chr>, PA_CNSMED <chr>, PA_CBOCOD <chr>,
#> #   PA_MOTSAI <chr>, PA_OBITO <chr>, PA_ENCERR <chr>, PA_PERMAN <chr>,
#> #   PA_ALTA <chr>, PA_TRANSF <chr>, PA_CIDPRI <chr>, PA_CIDSEC <chr>, …
```
