# Prepare SINAN malaria microdata

Recodes supported fields from SINAN malaria notifications into
descriptive values and normalizes escaped Unicode text. Columns not
explicitly recoded are retained, but the returned tibble contains
character columns.

## Usage

``` r
process_sinan_malaria(data, municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  with `information_system = "SINAN-MALARIA"`, or a compatible layout.

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
process_sinan_malaria(sinan_malaria_sample)
#> # A tibble: 100 × 55
#>    TP_NOT    ID_AGRAVO DT_NOTIFIC SEM_NOT NU_ANO SG_UF_NOT ID_MUNICIP ID_REGIONA
#>    <chr>     <chr>     <chr>      <chr>   <chr>  <chr>     <chr>      <chr>     
#>  1 Individu… B54       2016-01-01 201552  2016   São Paulo 355030     1331      
#>  2 Individu… B54       2016-01-02 201552  2016   Bahia     292740     1380      
#>  3 Individu… B54       2016-01-02 201552  2016   Bahia     291072     1388      
#>  4 Individu… B54       2016-01-02 201552  2016   Espírito… 320130     1510      
#>  5 Individu… B54       2016-01-02 201552  2016   Bahia     292860     1380      
#>  6 Individu… B54       2016-01-03 201601  2016   Paraíba   250750     1377      
#>  7 Individu… B54       2016-01-04 201601  2016   Paraná    410830     1363      
#>  8 Individu… B54       2016-01-04 201601  2016   Goiás     520870     1779      
#>  9 Individu… B54       2016-01-04 201601  2016   Rio Gran… 430510     1608      
#> 10 Individu… B54       2016-01-04 201601  2016   São Paulo 350950     1342      
#> # ℹ 90 more rows
#> # ℹ 47 more variables: ID_UNIDADE <chr>, DT_SIN_PRI <chr>, SEM_PRI <chr>,
#> #   ANO_NASC <chr>, NU_IDADE_N <chr>, CS_SEXO <chr>, CS_GESTANT <chr>,
#> #   CS_RACA <chr>, CS_ESCOL_N <chr>, SG_UF <chr>, ID_MN_RESI <chr>,
#> #   ID_RG_RESI <chr>, ID_PAIS <chr>, DT_INVEST <chr>, ID_OCUPA_N <chr>,
#> #   CLASSI_FIN <chr>, AT_ATIVIDA <chr>, AT_LAMINA <chr>, AT_SINTOMA <chr>,
#> #   TPAUTOCTO <chr>, COUFINF <chr>, COPAISINF <chr>, COMUNINF <chr>, …
```
