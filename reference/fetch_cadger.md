# Download the current CADGER table

Downloads and reads the current CNES establishment-name table
distributed by DataSUS. This remains available as a standalone helper;
[`process_cnes()`](https://rfsaldanha.github.io/microdatasus/reference/process_cnes.md)
reads the same DBF through its session-cached TabWin dictionary.

## Usage

``` r
fetch_cadger(
  timeout = 240,
  cache_dir = getOption("microdatasus.cache_dir", NULL),
  refresh = FALSE,
  quiet = FALSE
)
```

## Arguments

- timeout:

  A positive numeric scalar. Download and connection timeout, in
  seconds.

- cache_dir:

  Optional persistent cache root. The default uses the
  `microdatasus.cache_dir` option when set.

- refresh:

  Logical scalar. If `TRUE`, redownload the ZIP archive.

- quiet:

  Logical scalar. If `TRUE`, suppress progress messages.

## Value

A data frame with character columns `CNES` (establishment code) and
`FANTASIA` (trade name).

## Network access

This function downloads the current `TAB_CNES.zip` archive from DataSUS.
Transfer progress is displayed by default. Without `cache_dir`, the
archive and extracted files are removed before return; persistent cache
entries are validated.

## References

Saldanha, R. F. (2026). [CNES – Cadastro Nacional de Estabelecimentos de
Saúde](https://rfsaldanha.github.io/sis/cnes.html).

## See also

[`process_cnes()`](https://rfsaldanha.github.io/microdatasus/reference/process_cnes.md),
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
