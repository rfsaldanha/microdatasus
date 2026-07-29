# Read a DBC file

Decompresses a DataSUS DBC file to a temporary DBF file and reads it
into a tibble. Use this function for a DBC file already available
locally; use
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
to discover and download files from DataSUS.

## Usage

``` r
read_dbc(file, as_character = TRUE)
```

## Arguments

- file:

  A single character string with the path to a readable, non-empty DBC
  file.

- as_character:

  If `TRUE` (the default), converts every column to character. If
  `FALSE`, preserves the types inferred from the DBF metadata.

## Value

A tibble with one column per DBF field. By default, all columns are
character vectors; with `as_character = FALSE`, DBF-inferred types are
retained.

## Details

Decompression is performed through the package's bundled DBC
implementation. The intermediate DBF file is created in the R temporary
directory and removed before the function returns or aborts. The
implementation was adapted from the `healthbR` package.

Invalid input files, decompression failures, and DBF reading failures
abort with errors in the `microdatasus_dbc_error` family.

## References

Saldanha, R. F. (2026). [*Sistemas de Informação em Saúde no
Brasil*](https://rfsaldanha.github.io/sis/).

## See also

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
