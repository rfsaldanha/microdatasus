# Download the current SIGTAB table

Downloads and reads the current SIA procedure table distributed by
DataSUS. This standalone table is useful when procedure metadata is
needed outside
[`process_sia()`](https://rfsaldanha.github.io/microdatasus/reference/process_sia.md),
which now reads the tables declared by each TabWin DEF.

## Usage

``` r
fetch_sigtab(
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

A data frame with character columns `COD` (procedure code) and
`nome_proced` (procedure name).

## Network access

This function downloads the current `TAB_SIA.zip` archive from DataSUS.
Transfer progress is displayed by default. Without `cache_dir`, the
archive and extracted files are removed before return; persistent cache
entries are validated.

## References

Saldanha, R. F. (2026). [SIA – Sistema de Informações Ambulatoriais do
SUS](https://rfsaldanha.github.io/sis/sia.html).

## See also

[`process_sia()`](https://rfsaldanha.github.io/microdatasus/reference/process_sia.md),
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
