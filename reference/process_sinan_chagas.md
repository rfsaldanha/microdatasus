# Process SINAN Chagas variables from DataSUS

`process_sinan_chagas` processes SINAN Chagas variables retrieved by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

## Usage

``` r
process_sinan_chagas(data, municipality_data = TRUE)
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

This function processes SINAN Chagas variables retrieved by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md),
informing labels for categoric variables including NA values.

## Examples

``` r
process_sinan_malaria(sinan_chagas_sample)
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
