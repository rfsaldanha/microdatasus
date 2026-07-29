# Prepare SINAN Chagas disease microdata

Recodes supported fields from SINAN Chagas disease notifications into
descriptive values and normalizes escaped Unicode text. Columns not
explicitly recoded are retained, but the returned tibble contains
character columns.

## Usage

``` r
process_sinan_chagas(data, municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  with `information_system = "SINAN-CHAGAS"`, or a compatible layout.

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
process_sinan_chagas(sinan_chagas_sample)
#> # A tibble: 100 × 113
#>    TP_NOT    ID_AGRAVO DT_NOTIFIC SEM_NOT NU_ANO SG_UF_NOT ID_MUNICIP ID_REGIONA
#>    <chr>     <chr>     <chr>      <chr>   <chr>  <chr>     <chr>      <chr>     
#>  1 Individu… B571      2023-05-05 202318  2023   Bahia     292740     1380      
#>  2 Individu… B571      2023-11-30 202348  2023   Pará      150210     1496      
#>  3 Individu… B571      2023-03-07 202310  2023   Pará      150150     1484      
#>  4 Individu… B571      2023-02-28 202309  2023   Pará      150140     1484      
#>  5 Individu… B571      2023-06-12 202324  2023   Pará      150140     1484      
#>  6 Individu… B571      2023-03-28 202313  2023   Pará      150210     1496      
#>  7 Individu… B571      2023-07-03 202327  2023   Pará      150442     1484      
#>  8 Individu… B571      2023-10-03 202340  2023   Rio de J… 330455     NA        
#>  9 Individu… B571      2023-01-31 202305  2023   Pará      150210     1496      
#> 10 Individu… B571      2023-07-02 202327  2023   Ceará     231290     1521      
#> # ℹ 90 more rows
#> # ℹ 105 more variables: ID_UNIDADE <chr>, DT_SIN_PRI <chr>, SEM_PRI <chr>,
#> #   ANO_NASC <chr>, NU_IDADE_N <chr>, CS_SEXO <chr>, CS_GESTANT <chr>,
#> #   CS_RACA <chr>, CS_ESCOL_N <chr>, SG_UF <chr>, ID_MN_RESI <chr>,
#> #   ID_RG_RESI <chr>, ID_PAIS <chr>, NDUPLIC_N <chr>, DT_INVEST <chr>,
#> #   ID_OCUPA_N <chr>, ANT_UF_1 <chr>, MUN_1 <chr>, ANT_UF_2 <chr>, MUN_2 <chr>,
#> #   ANT_UF_3 <chr>, MUN_3 <chr>, PRESENCA <chr>, PARASITO <chr>, …
```
