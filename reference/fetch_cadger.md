# Download the current CADGER table

Downloads and reads the current CNES establishment-name table
distributed by DataSUS.
[`process_cnes()`](https://rfsaldanha.github.io/microdatasus/reference/process_cnes.md)
can use this table to add establishment names.

## Usage

``` r
fetch_cadger(timeout = 240)
```

## Arguments

- timeout:

  A positive numeric scalar. Download and connection timeout, in
  seconds.

## Value

A data frame with character columns `CNES` (establishment code) and
`FANTASIA` (trade name).

## Network access

This function downloads the current `TAB_CNES.zip` archive from DataSUS.
Transfer progress is displayed by default. The temporary archive and
extracted files are removed before the function returns or aborts.

## References

Saldanha, R. F. (2026). [CNES – Cadastro Nacional de Estabelecimentos de
Saúde](https://rfsaldanha.github.io/sis/cnes.html).

## See also

[`process_cnes()`](https://rfsaldanha.github.io/microdatasus/reference/process_cnes.md),
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
