# Prepare SINAN visceral leishmaniasis microdata

Recodes supported fields from SINAN visceral leishmaniasis notifications
into descriptive values and normalizes escaped Unicode text. Columns not
explicitly recoded are retained, but the returned tibble contains
character columns.

## Usage

``` r
process_sinan_leishmaniose_visceral(data, municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  with `information_system = "SINAN-LEISHMANIOSE-VISCERAL"`, or a
  compatible layout.

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
process_sinan_leishmaniose_visceral(sinan_leishmaniose_visceral_sample)
#> # A tibble: 100 × 81
#>    TP_NOT    ID_AGRAVO DT_NOTIFIC SEM_NOT NU_ANO SG_UF_NOT ID_MUNICIP ID_REGIONA
#>    <chr>     <chr>     <chr>      <chr>   <chr>  <chr>     <chr>      <chr>     
#>  1 Individu… B550      2023-09-06 202336  2023   Rondônia  110028     1482      
#>  2 Individu… B550      2023-01-06 202301  2023   Maranhão  210530     1440      
#>  3 Individu… B550      2023-05-08 202319  2023   Maranhão  210530     1440      
#>  4 Individu… B550      2023-06-09 202323  2023   Maranhão  210530     1440      
#>  5 Individu… B550      2023-07-04 202327  2023   Maranhão  210530     1440      
#>  6 Individu… B550      2023-04-12 202315  2023   Roraima   140010     NA        
#>  7 Individu… B550      2023-04-05 202314  2023   Roraima   140010     NA        
#>  8 Individu… B550      2023-07-24 202330  2023   Minas Ge… 316860     1465      
#>  9 Individu… B550      2023-01-29 202305  2023   Roraima   140010     NA        
#> 10 Individu… B550      2023-02-12 202307  2023   Roraima   140040     NA        
#> # ℹ 90 more rows
#> # ℹ 73 more variables: ID_UNIDADE <chr>, DT_SIN_PRI <chr>, SEM_PRI <chr>,
#> #   ANO_NASC <chr>, NU_IDADE_N <chr>, CS_SEXO <chr>, CS_GESTANT <chr>,
#> #   CS_RACA <chr>, CS_ESCOL_N <chr>, SG_UF <chr>, ID_MN_RESI <chr>,
#> #   ID_RG_RESI <chr>, ID_PAIS <chr>, NDUPLIC_N <chr>, DT_DIGITA <chr>,
#> #   CS_FLXRET <chr>, FLXRECEBI <chr>, MIGRADO_W <chr>, DT_INVEST <chr>,
#> #   ID_OCUPA_N <chr>, FEBRE <chr>, FRAQUEZA <chr>, EDEMA <chr>, EMAGRA <chr>, …
```
