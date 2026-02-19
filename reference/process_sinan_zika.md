# Process SINAN Zika variables from DataSUS

`process_sinan_zika` processes SINAN Zika variables retrieved by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

## Usage

``` r
process_sinan_zika(data, municipality_data = TRUE)
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

This function processes SINAN Zika variables retrieved by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md),
informing labels for categoric variables including NA values.

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
