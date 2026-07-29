# Prepare SINAN tegumentary leishmaniasis microdata

Recodes supported fields from SINAN tegumentary leishmaniasis
notifications into descriptive values and normalizes escaped Unicode
text. Columns not explicitly recoded are retained, but the returned
tibble contains character columns.

## Usage

``` r
process_sinan_leishmaniose_tegumentar(data, municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  with `information_system = "SINAN-LEISHMANIOSE-TEGUMENTAR"`, or a
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
process_sinan_leishmaniose_tegumentar(sinan_leishmaniose_tegumentar_sample)
#> # A tibble: 100 × 79
#>    TP_NOT    ID_AGRAVO DT_NOTIFIC SEM_NOT NU_ANO SG_UF_NOT ID_MUNICIP ID_REGIONA
#>    <chr>     <chr>     <chr>      <chr>   <chr>  <chr>     <chr>      <chr>     
#>  1 Individu… B551      2023-09-21 202338  2023   Espírito… 320265     NA        
#>  2 Individu… B551      2023-07-19 202329  2023   Espírito… 320115     NA        
#>  3 Individu… B551      2023-03-17 202311  2023   Espírito… 320450     NA        
#>  4 Individu… B551      2023-02-17 202307  2023   Espírito… 320370     NA        
#>  5 Individu… B551      2023-12-26 202352  2023   Espírito… 320450     NA        
#>  6 Individu… B551      2023-10-11 202341  2023   Espírito… 320450     NA        
#>  7 Individu… B551      2023-09-25 202339  2023   Espírito… 320313     NA        
#>  8 Individu… B551      2023-07-25 202330  2023   Espírito… 320360     NA        
#>  9 Individu… B551      2023-02-02 202305  2023   Espírito… 320450     NA        
#> 10 Individu… B551      2023-05-11 202319  2023   Espírito… 320450     NA        
#> # ℹ 90 more rows
#> # ℹ 71 more variables: ID_UNIDADE <chr>, DT_DIAG <chr>, SEM_DIAG <chr>,
#> #   ANO_NASC <chr>, NU_IDADE_N <chr>, CS_SEXO <chr>, CS_GESTANT <chr>,
#> #   CS_RACA <chr>, CS_ESCOL_N <chr>, SG_UF <chr>, ID_MN_RESI <chr>,
#> #   ID_RG_RESI <chr>, ID_PAIS <chr>, NDUPLIC_N <chr>, DT_DIGITA <chr>,
#> #   DT_TRANSUS <chr>, DT_TRANSDM <chr>, DT_TRANSSM <chr>, DT_TRANSRM <chr>,
#> #   DT_TRANSRS <chr>, DT_TRANSSE <chr>, NU_LOTE_V <chr>, NU_LOTE_H <chr>, …
```
