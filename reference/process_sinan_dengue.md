# Prepare SINAN dengue microdata

Recodes supported fields from SINAN dengue notifications into
descriptive values and normalizes escaped Unicode text. Columns not
explicitly recoded are retained, but the returned tibble contains
character columns.

## Usage

``` r
process_sinan_dengue(data, municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  with `information_system = "SINAN-DENGUE"`, or a compatible layout.

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
process_sinan_dengue(sinan_dengue_sample)
#> # A tibble: 100 × 79
#>    TP_NOT    ID_AGRAVO DT_NOTIFIC SEM_NOT NU_ANO SG_UF_NOT ID_MUNICIP ID_REGIONA
#>    <chr>     <chr>     <chr>      <chr>   <chr>  <chr>     <chr>      <chr>     
#>  1 Individu… A90       2010-01-26 201004  2010   Pernambu… 260190     1499      
#>  2 Individu… A90       2010-01-26 201004  2010   Rio de J… 330455     NA        
#>  3 Individu… A90       2010-01-11 201002  2010   Tocantins 170950     NA        
#>  4 Individu… A90       2010-01-22 201003  2010   Tocantins 170950     NA        
#>  5 Individu… A90       2010-01-25 201004  2010   Minas Ge… 314330     1473      
#>  6 Individu… A90       2010-01-25 201004  2010   Minas Ge… 314330     1473      
#>  7 Individu… A90       2010-01-29 201004  2010   Minas Ge… 314330     1473      
#>  8 Individu… A90       2010-01-21 201003  2010   Minas Ge… 314330     1473      
#>  9 Individu… A90       2010-01-25 201004  2010   Minas Ge… 314330     1473      
#> 10 Individu… A90       2010-01-25 201004  2010   Minas Ge… 314330     1473      
#> # ℹ 90 more rows
#> # ℹ 71 more variables: ID_UNIDADE <chr>, DT_SIN_PRI <chr>, SEM_PRI <chr>,
#> #   NU_IDADE_N <chr>, CS_SEXO <chr>, CS_GESTANT <chr>, CS_RACA <chr>,
#> #   CS_ESCOL_N <chr>, SG_UF <chr>, ID_MN_RESI <chr>, ID_RG_RESI <chr>,
#> #   ID_PAIS <chr>, NDUPLIC_N <chr>, DT_DIGITA <chr>, CS_FLXRET <chr>,
#> #   FLXRECEBI <chr>, MIGRADO_W <chr>, DT_INVEST <chr>, ID_OCUPA_N <chr>,
#> #   DT_SORO <chr>, RESUL_SORO <chr>, DT_NS1 <chr>, RESUL_NS1 <chr>, …
```
