# Prepare SIM mortality microdata

Recodes supported SIM mortality fields into descriptive values and
normalizes escaped Unicode text. Codes without a documented conversion
are retained.

## Usage

``` r
process_sim(data, municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  for a SIM mortality system, or another data frame with a compatible
  layout.

- municipality_data:

  Logical scalar. If `TRUE`, add municipality names and available
  territorial attributes for supported municipality-code columns.

## Value

A tibble with character columns. Supported codes are replaced with
descriptions, and municipality fields are added when requested and
available.

## Details

Columns not explicitly recoded are retained, but Unicode normalization
is applied to every column and consequently the returned tibble contains
character columns.

## References

Saldanha, R. F. (2026). [SIM – Sistema de Informação sobre
Mortalidade](https://rfsaldanha.github.io/sis/sim.html).

## See also

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)

## Examples

``` r
process_sim(sim_do_sample)
#> # A tibble: 100 × 101
#>    CONTADOR ORIGEM TIPOBITO  DTOBITO    HORAOBITO CODMUNNATU DTNASC  IDADE SEXO 
#>    <chr>    <chr>  <chr>     <chr>      <chr>     <chr>      <chr>   <chr> <chr>
#>  1 1        1      Não Fetal 2016-01-01 NA        120033     1988-1… 427   Masc…
#>  2 2        1      Não Fetal 2016-01-01 NA        120030     2002-1… 413   Femi…
#>  3 3        1      Não Fetal 2016-01-01 NA        120030     1985-1… 430   Masc…
#>  4 4        1      Não Fetal 2016-01-01 2100      NA         1959-1… 456   Masc…
#>  5 5        1      Não Fetal 2016-01-01 2050      120040     2014-1… 401   Femi…
#>  6 6        1      Não Fetal 2016-01-01 2040      120050     1974-0… 441   Masc…
#>  7 7        1      Não Fetal 2016-01-01 0530      120040     1970-0… 445   Masc…
#>  8 8        1      Não Fetal 2016-01-01 0230      130350     1978-0… 437   Masc…
#>  9 9        1      Não Fetal 2016-01-01 0600      120040     1989-0… 426   Masc…
#> 10 10       1      Não Fetal 2016-01-01 0450      120060     1958-0… 457   Femi…
#> # ℹ 90 more rows
#> # ℹ 92 more variables: RACACOR <chr>, ESTCIV <chr>, ESC <chr>, ESC2010 <chr>,
#> #   SERIESCFAL <chr>, CODMUNRES <chr>, LOCOCOR <chr>, CODESTAB <chr>,
#> #   ESTABDESCR <chr>, CODMUNOCOR <chr>, IDADEMAE <chr>, ESCMAE <chr>,
#> #   ESCMAE2010 <chr>, SERIESCMAE <chr>, QTDFILVIVO <chr>, QTDFILMORT <chr>,
#> #   GRAVIDEZ <chr>, SEMAGESTAC <chr>, GESTACAO <chr>, PARTO <chr>,
#> #   OBITOPARTO <chr>, PESO <chr>, TPMORTEOCO <chr>, OBITOGRAV <chr>, …
```
