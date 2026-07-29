# Prepare SIH hospital-admission microdata

Recodes supported fields from reduced hospital-admission records
(`"SIH-RD"`) into descriptive values and normalizes escaped Unicode
text.

## Usage

``` r
process_sih(data, information_system = "SIH-RD", municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  with `information_system = "SIH-RD"`, or a compatible layout.

- information_system:

  A single character string. Currently only `"SIH-RD"` is supported.

- municipality_data:

  Logical scalar. If `TRUE`, add municipality names and available
  territorial attributes for supported municipality-code columns.

## Value

A tibble with character columns. Supported codes are replaced with
descriptions, and municipality fields are added when requested and
available.

## Details

Columns not explicitly recoded are retained, but Unicode normalization
is applied to every column and consequently the returned tibble contains
character columns. Other SIH layouts downloadable with
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
are not currently supported by this processing function.

## References

Saldanha, R. F. (2026). [SIH – Sistema de Informações Hospitalares do
SUS](https://rfsaldanha.github.io/sis/sih.html).

## See also

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)

## Examples

``` r
process_sih(sih_rd_sample)
#> # A tibble: 100 × 121
#>    UF_ZI  ANO_CMPT MES_CMPT ESPEC CGC_HOSP     N_AIH IDENT CEP   MUNIC_RES NASC 
#>    <chr>  <chr>    <chr>    <chr> <chr>        <chr> <chr> <chr> <chr>     <chr>
#>  1 120000 2016     6        03    04034526001… 1216… Prin… 6994… 120050    1934…
#>  2 120000 2016     6        03    04034526001… 1216… Prin… 6994… 120050    1982…
#>  3 120000 2016     6        03    04034526001… 1216… Prin… 6994… 120050    1973…
#>  4 120000 2016     6        03    04034526001… 1216… Prin… 6994… 120050    1938…
#>  5 120000 2016     6        03    04034526001… 1216… Prin… 6994… 120050    1943…
#>  6 120000 2016     6        03    04034526001… 1216… Prin… 6994… 120050    1947…
#>  7 120000 2016     6        01    04034526002… 1216… Prin… 6998… 120020    2006…
#>  8 120000 2016     6        01    04034526002… 1216… Prin… 6998… 120020    1978…
#>  9 120000 2016     6        02    04034526001… 1216… Prin… 6994… 120050    1993…
#> 10 120000 2016     6        02    04034526001… 1216… Prin… 6994… 120050    1992…
#> # ℹ 90 more rows
#> # ℹ 111 more variables: SEXO <chr>, UTI_MES_IN <chr>, UTI_MES_AN <chr>,
#> #   UTI_MES_AL <chr>, UTI_MES_TO <chr>, MARCA_UTI <chr>, UTI_INT_IN <chr>,
#> #   UTI_INT_AN <chr>, UTI_INT_AL <chr>, UTI_INT_TO <chr>, DIAR_ACOM <chr>,
#> #   QT_DIARIAS <chr>, PROC_SOLIC <chr>, PROC_REA <chr>, VAL_SH <chr>,
#> #   VAL_SP <chr>, VAL_SADT <chr>, VAL_RN <chr>, VAL_ACOMP <chr>,
#> #   VAL_ORTP <chr>, VAL_SANGUE <chr>, VAL_SADTSR <chr>, VAL_TRANSP <chr>, …
```
