# Process CNES variables from DataSUS

`process_cnes` processes CNES variables retrieved by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

## Usage

``` r
process_cnes(
  data,
  information_system = c("CNES-ST", "CNES-PF"),
  nomes = FALSE,
  municipality_data = TRUE
)
```

## Arguments

- data:

  `data.frame` created by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

- information_system:

  `string`. `CNES-ST` or `CNES-PF`

- nomes:

  optional logical. `FALSE` by default, downloads extra data and add
  `FANTASIA` names to the dataset.

- municipality_data:

  optional logical. `TRUE` by default, creates new variables in the
  dataset informing the full name and other details about the
  municipality of residence.

## Value

a `data.frame` with the processed data.

## Details

This function processes CNES-ST (Estabelecimentos) or CNES-PF (Pessoa
f\u00edsica) variables retrieved by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md),
informing labels for categoric variables including NA values.

## Examples

``` r
process_cnes(cnes_st_sample, information_system = "CNES-ST")
#> # A tibble: 100 × 209
#>    CNES    CODUFMUN COD_CEP  CPF_CNPJ     PF_PJ NIV_DEP CNPJ_MAN COD_IR REGSAUDE
#>    <chr>   <chr>    <chr>    <chr>        <chr> <chr>   <chr>    <chr>  <chr>   
#>  1 2002043 120001   69945000 00000000000… Pess… Mantida 8430673… NA     NA      
#>  2 2002159 120001   69945000 00000000000… Pess… Mantida 8430673… NA     NA      
#>  3 3006166 120001   69945000 00000000000… Pess… Mantida 8430673… NA     NA      
#>  4 3382745 120001   69945000 00000000000… Pess… Mantida 8430673… NA     NA      
#>  5 3393984 120001   69945000 00000000000… Pess… Mantida 8430673… NA     NA      
#>  6 3638685 120001   69945000 00000000000… Pess… Mantida 8430673… NA     NA      
#>  7 5403669 120001   69945000 00000000000… Pess… Mantida 8430673… NA     NA      
#>  8 5701929 120001   69945000 00000000000… Pess… Mantida 4034526… NA     NA      
#>  9 6514669 120001   69945000 00000000000… Pess… Mantida 8430673… NA     NA      
#> 10 7026641 120001   69945000 00000000000… Pess… Mantida 4034526… NA     NA      
#> # ℹ 90 more rows
#> # ℹ 200 more variables: MICR_REG <chr>, DISTRSAN <chr>, DISTRADM <chr>,
#> #   VINC_SUS <chr>, TPGESTAO <chr>, ESFERA_A <chr>, RETENCAO <chr>,
#> #   ATIVIDAD <chr>, NATUREZA <chr>, CLIENTEL <chr>, TP_UNID <chr>,
#> #   TURNO_AT <chr>, NIV_HIER <chr>, TP_PREST <chr>, CO_BANCO <chr>,
#> #   CO_AGENC <chr>, C_CORREN <chr>, CONTRATM <chr>, DT_PUBLM <chr>,
#> #   CONTRATE <chr>, DT_PUBLE <chr>, ALVARA <chr>, DT_EXPED <chr>, …
process_cnes(cnes_pf_sample, information_system = "CNES-PF")
#> # A tibble: 100 × 48
#>    CNES    CODUFMUN REGSAUDE MICR_REG DISTRSAN DISTRADM TPGESTAO  PF_PJ CPF_CNPJ
#>    <chr>   <chr>    <chr>    <chr>    <chr>    <chr>    <chr>     <chr> <chr>   
#>  1 2002043 120001   NA       NA       NA       NA       Municipal Pess… 0       
#>  2 2002159 120001   NA       NA       NA       NA       Municipal Pess… 0       
#>  3 3006166 120001   NA       NA       NA       NA       Municipal Pess… 0       
#>  4 3006166 120001   NA       NA       NA       NA       Municipal Pess… 0       
#>  5 3006166 120001   NA       NA       NA       NA       Municipal Pess… 0       
#>  6 3006166 120001   NA       NA       NA       NA       Municipal Pess… 0       
#>  7 3006166 120001   NA       NA       NA       NA       Municipal Pess… 0       
#>  8 3006166 120001   NA       NA       NA       NA       Municipal Pess… 0       
#>  9 3006166 120001   NA       NA       NA       NA       Municipal Pess… 0       
#> 10 3006166 120001   NA       NA       NA       NA       Municipal Pess… 0       
#> # ℹ 90 more rows
#> # ℹ 39 more variables: NIV_DEP <chr>, CNPJ_MAN <chr>, ESFERA_A <chr>,
#> #   ATIVIDAD <chr>, RETENCAO <chr>, NATUREZA <chr>, CLIENTEL <chr>,
#> #   TP_UNID <chr>, TURNO_AT <chr>, NIV_HIER <chr>, TERCEIRO <chr>,
#> #   CPFUNICO <chr>, CBO <chr>, CBOUNICO <chr>, NOMEPROF <chr>, CNS_PROF <chr>,
#> #   CONSELHO <chr>, REGISTRO <chr>, VINCULAC <chr>, VINCUL_C <chr>,
#> #   VINCUL_A <chr>, VINCUL_N <chr>, PROF_SUS <chr>, PROFNSUS <chr>, …
```
