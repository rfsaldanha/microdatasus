# Prepare CNES microdata

Recodes supported CNES fields into descriptive values and normalizes
escaped Unicode text. Establishment (`"CNES-ST"`) and professional
(`"CNES-PF"`) records have different layouts and are processed
accordingly.

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

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  or another data frame with a compatible CNES layout.

- information_system:

  A single character string: `"CNES-ST"` for establishments or
  `"CNES-PF"` for professionals.

- nomes:

  Logical scalar. For `"CNES-ST"` data, if `TRUE`, download the current
  CADGER table with
  [`fetch_cadger()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_cadger.md)
  and add establishment trade names. This requires network access. It
  has no effect for `"CNES-PF"`.

- municipality_data:

  Logical scalar. If `TRUE`, add municipality names and available
  territorial attributes for supported municipality-code columns.

## Value

A tibble with character columns. Supported codes are replaced with
descriptions, and requested lookup fields are added where applicable.

## Details

Columns not explicitly recoded are retained, but Unicode normalization
is applied to every column and consequently the returned tibble contains
character columns. Lookup joins can add establishment, occupation, and
municipality information.

## References

Saldanha, R. F. (2026). [CNES – Cadastro Nacional de Estabelecimentos de
Saúde](https://rfsaldanha.github.io/sis/cnes.html).

## See also

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md),
[`fetch_cadger()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_cadger.md)

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
