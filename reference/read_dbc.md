# Read a DBC file

Reads a DataSUS DBC file directly into a tibble, without creating an
intermediate DBF file. Use this function for a DBC file already
available locally; use
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
to discover and download files from DataSUS.

## Usage

``` r
read_dbc(file, as_character = TRUE, vars = NULL, encoding = "auto")
```

## Arguments

- file:

  A single character string with the path to a readable, non-empty DBC
  file.

- as_character:

  If `TRUE` (the default), converts every column to character. If
  `FALSE`, preserves the types inferred from the DBF metadata.

- vars:

  `NULL` (the default), or a character vector containing the columns to
  read. Unselected columns are neither allocated nor parsed.

- encoding:

  Character scalar naming the source encoding, or `"auto"` (the default)
  to use the DBF language-driver byte together with byte-level evidence
  from each character field. Unmarked DataSUS files start with
  Windows-1252 and switch to UTF-8, CP850, or CP860 only when the data
  support it. Explicit encodings are strict and never replace invalid
  bytes silently.

## Value

A tibble with one column per DBF field. By default, all columns are
character vectors; with `as_character = FALSE`, DBF-inferred types are
retained. The `dbc_encoding` and `dbf_language_driver` attributes record
the header-derived source encoding and the original header byte;
`dbc_column_encodings` records the encoding used for each column. The
`dbf_field_types`, `dbf_field_widths`, and `dbf_field_decimals`
attributes retain the complete physical DBF layout so DEF conversions
whose fixed-width code crosses adjacent fields can be reproduced.

## Details

Decompression is performed through the package's bundled DBC
implementation. The DBF header and decompressed fixed-width records are
parsed directly into R columns without creating an intermediate DBF
file. The decompressor was adapted from the `healthbR` package. The DBC
CRC32 is verified against the complete decompressed DBF contents before
a result is returned.

In automatic mode, character fields are converted to UTF-8 when their
byte encoding can be identified safely. Undecodable mixed data and the
obfuscated byte representation used by some CPF/CNS fields are preserved
losslessly as strings marked with encoding `"bytes"`, with a warning. An
explicit `encoding` requests strict decoding and invalid byte sequences
then abort with a `microdatasus_dbc_encoding_error`. The five undefined
Windows-1252 byte values (`81`, `8D`, `8F`, `90`, and `9D` in
hexadecimal) are treated as invalid on every operating system; this
avoids platform-dependent results from different `iconv`
implementations.

Column projection affects allocation and cell parsing, not structural
validation: the complete compressed stream, DBF layout, record markers,
and CRC32 are still checked when `vars` selects only part of the table.
Invalid text in an unselected field is not decoded and therefore does
not make a projected read fail.

Header layout, field widths, record markers, compressed-stream
termination, numeric syntax, finite numeric values, calendar dates, and
the complete DBF CRC32 are validated. Structural corruption and internal
contradictions abort instead of returning a partial table. Malformed
numeric or date values found in otherwise valid official files are
converted to `NA` with a warning that reports their count and first
location.

The native parser accepts the fixed-width DBF field types used by
DataSUS: character (`C`), date (`D`), floating point (`F`), logical
(`L`), and numeric (`N`). As in
[`foreign::read.dbf()`](https://rdrr.io/pkg/foreign/man/read.dbf.html),
records carrying the DBF deleted marker are retained; no DataSUS row is
discarded implicitly.

Invalid input files, decompression failures, and record parsing failures
abort with errors in the `microdatasus_dbc_error` family.

## References

Saldanha, R. F. (2026). [*Sistemas de Informação em Saúde no
Brasil*](https://rfsaldanha.github.io/sis/).

## See also

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
