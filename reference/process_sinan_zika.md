# Prepare SINAN Zika virus disease microdata

Recodes supported fields from SINAN Zika virus disease notifications
into descriptive values and normalizes escaped Unicode text. Columns not
explicitly recoded are retained, but the returned tibble contains
character columns.

## Usage

``` r
process_sinan_zika(data, municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  with `information_system = "SINAN-ZIKA"`, or a compatible layout.

- municipality_data:

  Logical scalar retained for API compatibility. It is not currently
  used by this processing function.

## Value

A tibble with character columns. Supported codes are replaced with
descriptions.

## References

Saldanha, R. F. (2026). [SINAN – Sistema de Informação de Agravos de
Notificação](https://rfsaldanha.github.io/sis/sinan.html).

## See also

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)

## Examples

``` r
process_sinan_zika(sinan_zika_sample)
#> # A tibble: 100 × 43
#>    TP_NOT    ID_AGRAVO CS_SUSPEIT DT_NOTIFIC SEM_NOT NU_ANO SG_UF_NOT ID_MUNICIP
#>    <chr>     <chr>     <chr>      <chr>      <chr>   <chr>  <chr>     <chr>     
#>  1 Individu… A928      NA         2016-01-01 201552  2016   Tocantins 170230    
#>  2 Individu… A928      NA         2016-01-01 201552  2016   Bahia     292040    
#>  3 Individu… A928      NA         2016-01-01 201552  2016   Bahia     292040    
#>  4 Individu… A928      NA         2016-01-01 201552  2016   Bahia     293240    
#>  5 Individu… A928      NA         2016-01-01 201552  2016   Espírito… 320530    
#>  6 Individu… A928      NA         2016-01-01 201552  2016   Espírito… 320530    
#>  7 Individu… A928      NA         2016-01-01 201552  2016   Espírito… 320530    
#>  8 Individu… A928      NA         2016-01-01 201552  2016   Espírito… 320530    
#>  9 Individu… A928      NA         2016-01-01 201552  2016   Pará      150613    
#> 10 Individu… A928      NA         2016-01-01 201552  2016   Pará      150613    
#> # ℹ 90 more rows
#> # ℹ 35 more variables: ID_REGIONA <chr>, DT_SIN_PRI <chr>, SEM_PRI <chr>,
#> #   NU_IDADE_N <chr>, CS_SEXO <chr>, CS_GESTANT <chr>, CS_RACA <chr>,
#> #   CS_ESCOL_N <chr>, SG_UF <chr>, ID_MN_RESI <chr>, ID_RG_RESI <chr>,
#> #   ID_PAIS <chr>, NDUPLIC_N <chr>, IN_VINCULA <chr>, DT_INVEST <chr>,
#> #   ID_OCUPA_N <chr>, CLASSI_FIN <chr>, CRITERIO <chr>, TPAUTOCTO <chr>,
#> #   COUFINF <chr>, COPAISINF <chr>, COMUNINF <chr>, DOENCA_TRA <chr>, …
```
