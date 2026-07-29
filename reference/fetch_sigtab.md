# Download the current SIGTAB table

Downloads and reads the current SIA procedure table distributed by
DataSUS.
[`process_sia()`](https://rfsaldanha.github.io/microdatasus/reference/process_sia.md)
can use this table to add procedure descriptions.

## Usage

``` r
fetch_sigtab(timeout = 240)
```

## Arguments

- timeout:

  A positive numeric scalar. Download and connection timeout, in
  seconds.

## Value

A data frame with character columns `COD` (procedure code) and
`nome_proced` (procedure name).

## Network access

This function downloads the current `TAB_SIA.zip` archive from DataSUS.
Transfer progress is displayed by default. The temporary archive and
extracted files are removed before the function returns or aborts.

## References

Saldanha, R. F. (2026). [SIA – Sistema de Informações Ambulatoriais do
SUS](https://rfsaldanha.github.io/sis/sia.html).

## See also

[`process_sia()`](https://rfsaldanha.github.io/microdatasus/reference/process_sia.md),
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
