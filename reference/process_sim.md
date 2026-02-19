# Process SIM variables from DataSUS

`process_sim` processes SIM variables retrieved by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

## Usage

``` r
process_sim(data, municipality_data = TRUE)
```

## Arguments

- data:

  `data.frame` created by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

- municipality_data:

  optional logical. `TRUE` by default, creates new variables in the
  dataset informing the full name and other details about the
  municipality of residence.

## Value

a `data.frame` with the processed data.

## Details

This function processes SIM variables retrieved by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md),
informing labels for categoric variables including NA values.

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
