# Prepare SIA outpatient-production microdata

Recodes supported fields from SIA individual outpatient-production
records (`"SIA-PA"`) into descriptive values and normalizes escaped
Unicode text. Lookup joins can add procedure, occupation, team, and
municipality descriptions.

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

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  with `information_system = "SIA-PA"`, or a compatible layout.

- information_system:

  A single character string. Currently only `"SIA-PA"` is supported.

- nome_proced:

  Logical scalar. If `TRUE`, download the current SIGTAB table with
  [`fetch_sigtab()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_sigtab.md)
  and join procedure names. This requires network access.

- nome_ocupacao:

  Logical scalar. If `TRUE`, join occupation descriptions for supported
  occupation-code columns.

- nome_equipe:

  Logical scalar retained for API compatibility. Team descriptions are
  currently joined whenever `PA_INE` is present, regardless of this
  value.

- municipality_data:

  Logical scalar. If `TRUE`, add municipality names and available
  territorial attributes for supported municipality-code columns.

## Value

A tibble with character columns. Supported codes are replaced with
descriptions, and requested lookup fields are added where applicable.

## Details

Columns not explicitly recoded are retained, but Unicode normalization
is applied to every column and consequently the returned tibble contains
character columns. Other SIA layouts downloadable with
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
are not currently supported by this processing function.

## References

Saldanha, R. F. (2026). [SIA – Sistema de Informações Ambulatoriais do
SUS](https://rfsaldanha.github.io/sis/sia.html).

## See also

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md),
[`fetch_sigtab()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_sigtab.md)

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
