# Create a reproducibility lockfile for a DataSUS download

Records the exact request, source-file SHA-256 checksums, selected
TabWin dictionaries, parser version, and packaged reference tables
associated with an object returned by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

## Usage

``` r
datasus_lockfile(x, file = NULL)
```

## Arguments

- x:

  An object returned with `provenance = TRUE`, or with
  `collect = FALSE`, by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).

- file:

  Optional path where the RDS lockfile is written atomically.

## Value

A `microdatasus_lockfile` list, invisibly when `file` is supplied.

## Details

Dictionary and reference-table entries are extracted from processing
diagnostics. To record them, use `process = TRUE` and
`process_args = list(diagnostics = TRUE)` in
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).
Download provenance alone records the source files; a table returned
only by
[`read_dbc()`](https://rfsaldanha.github.io/microdatasus/reference/read_dbc.md)
has no download provenance for this function.
