# Prepare SINASC live-birth microdata

Recodes supported SINASC live-birth fields into descriptive values and
normalizes escaped Unicode text. Codes without a documented conversion
are retained.

## Usage

``` r
process_sinasc(data, municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  with `information_system = "SINASC"`, or a compatible layout.

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

Saldanha, R. F. (2026). [SINASC – Sistema de Informação sobre Nascidos
Vivos](https://rfsaldanha.github.io/sis/sinasc.html).

## See also

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)

## Examples

``` r
process_sinasc(sinasc_sample)
#> # A tibble: 100 × 69
#>    contador ORIGEM CODESTAB CODMUNNASC LOCNASC  IDADEMAE ESTCIVMAE        ESCMAE
#>    <chr>    <chr>  <chr>    <chr>      <chr>    <chr>    <chr>            <chr> 
#>  1 1        1      5618347  110020     Hospital 29       União consensual 4 a 7…
#>  2 2        1      5618347  110020     Hospital 14       União consensual 4 a 7…
#>  3 3        1      4001303  110020     Hospital 30       Casada           4 a 7…
#>  4 4        1      5618347  110020     Hospital 23       Solteira         8 a 1…
#>  5 5        1      5618347  110020     Hospital 30       Casada           8 a 1…
#>  6 6        1      3970442  110020     Hospital 25       União consensual 8 a 1…
#>  7 7        1      5701929  120001     Hospital 21       União consensual 1 a 3…
#>  8 8        1      5701929  120001     Hospital 16       União consensual 8 a 1…
#>  9 9        1      5701929  120001     Hospital 25       NA               8 a 1…
#> 10 10       1      5701929  120001     Hospital 25       Solteira         4 a 7…
#> # ℹ 90 more rows
#> # ℹ 61 more variables: QTDFILVIVO <chr>, QTDFILMORT <chr>, CODMUNRES <chr>,
#> #   GESTACAO <chr>, GRAVIDEZ <chr>, PARTO <chr>, CONSULTAS <chr>, DTNASC <chr>,
#> #   HORANASC <chr>, SEXO <chr>, APGAR1 <chr>, APGAR5 <chr>, RACACOR <chr>,
#> #   PESO <chr>, IDANOMAL <chr>, DTCADASTRO <chr>, CODANOMAL <chr>,
#> #   NUMEROLOTE <chr>, VERSAOSIST <chr>, DTRECEBIM <chr>, DIFDATA <chr>,
#> #   DTRECORIGA <chr>, NATURALMAE <chr>, CODMUNNATU <chr>, CODUFNATU <chr>, …
```
