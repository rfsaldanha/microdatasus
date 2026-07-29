# Prepare SINAN chikungunya microdata

Recodes supported fields from SINAN chikungunya notifications into
descriptive values and normalizes escaped Unicode text. Columns not
explicitly recoded are retained, but the returned tibble contains
character columns.

## Usage

``` r
process_sinan_chikungunya(data, municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  with `information_system = "SINAN-CHIKUNGUNYA"`, or a compatible
  layout.

- municipality_data:

  Logical scalar. If `TRUE`, add municipality names and available
  territorial attributes for supported municipality-code columns.

## Value

A tibble with character columns. Supported codes are replaced with
descriptions, and municipality fields are added when requested and
available.

## References

Saldanha, R. F. (2026). [SINAN – Sistema de Informação de Agravos de
Notificação](https://rfsaldanha.github.io/sis/sinan.html).

## See also

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)

## Examples

``` r
process_sinan_chikungunya(sinan_chikungunya_sample)
#> # A tibble: 100 × 135
#>    TP_NOT    ID_AGRAVO DT_NOTIFIC SEM_NOT NU_ANO SG_UF_NOT ID_MUNICIP ID_REGIONA
#>    <chr>     <chr>     <chr>      <chr>   <chr>  <chr>     <chr>      <chr>     
#>  1 Individu… A92.0     2022-01-08 202201  2022   Acre      120033     1941      
#>  2 Individu… A92.0     2022-02-16 202207  2022   Acre      120033     1941      
#>  3 Individu… A92.0     2022-01-26 202204  2022   Acre      120033     1941      
#>  4 Individu… A92.0     2022-01-08 202201  2022   Acre      120033     1941      
#>  5 Individu… A92.0     2022-01-26 202204  2022   Acre      120033     1941      
#>  6 Individu… A92.0     2022-01-05 202201  2022   Acre      120033     1941      
#>  7 Individu… A92.0     2022-12-30 202252  2022   Acre      120020     1941      
#>  8 Individu… A92.0     2022-11-14 202246  2022   Acre      120020     1941      
#>  9 Individu… A92.0     2022-05-19 202220  2022   Acre      120020     1941      
#> 10 Individu… A92.0     2022-01-31 202205  2022   Acre      120020     1941      
#> # ℹ 90 more rows
#> # ℹ 127 more variables: ID_UNIDADE <chr>, DT_SIN_PRI <chr>, SEM_PRI <chr>,
#> #   ANO_NASC <chr>, NU_IDADE_N <chr>, CS_SEXO <chr>, CS_GESTANT <chr>,
#> #   CS_RACA <chr>, CS_ESCOL_N <chr>, SG_UF <chr>, ID_MN_RESI <chr>,
#> #   ID_RG_RESI <chr>, ID_PAIS <chr>, DT_INVEST <chr>, ID_OCUPA_N <chr>,
#> #   FEBRE <chr>, MIALGIA <chr>, CEFALEIA <chr>, EXANTEMA <chr>, VOMITO <chr>,
#> #   NAUSEA <chr>, DOR_COSTAS <chr>, CONJUNTVIT <chr>, ARTRITE <chr>, …
```
