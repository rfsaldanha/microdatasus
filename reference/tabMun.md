# Fixed municipal and special territorial-code reference

Reconstructed from the exact `TB_MUNICIP` and `TB_UF` TXT members of the
official DataSUS territorial base 2023 archive. Their modification date
in that ZIP is 2022-05-16; it does not establish a single validity date
for all attributes. The snapshot version is
`datasus-territorio-2023-txt-20220516`.

## Usage

``` r
tabMun
```

## Format

A data frame with 5659 rows and 9 variables:

- munResCod:

  Municipality IBGE code with 6 numbers

- munResStatus:

  Status

- munResTipo:

  Type

- munResNome:

  Name

- munResUf:

  UF (state)

- munResLat:

  Latitude

- munResLon:

  Longitude

- munResAlt:

  Altitude

- munResArea:

  Area

## Source

DataSUS, territorial base 2023. See the official [file-transfer
portal](https://datasus.saude.gov.br/transferencia-de-arquivos/). The
original archive location is recorded for provenance:
`ftp://ftp.datasus.gov.br/territorio/tabelas/2023/base_territorial_2023.zip`.
Access to this FTP server can be unavailable from some networks or
countries. The four unmodified TXT/layout members and reconstruction
notes are supplied under
`system.file("extdata", "territory", package = "microdatasus")`, so
using this reference and rebuilding it from those members require no
download.

## Details

This reconstruction preserves the existing table's values, classes, and
row order. It includes extinct, transferred, and unknown territorial
codes, so the row count is not the number of active municipalities.
Processors use the same fixed reference for every observation period
when territorial enrichment is enabled; they do not harmonize historical
boundaries automatically.

The `legacy-compatible-v1` transformation converts decimal commas,
replaces zero geographical placeholders with `NA` for unknown units and
former territory `200010`, and retains two historical UF-display
conventions: empty text for code `0`, and successor state `Pernambuco`
for `200010`. The latter retains status `TRANSF` and type `TERRIT`; the
original UF name is available in the shipped source TXT. Genuine zero
values elsewhere remain.
[`datasus_reference_tables()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_reference_tables.md)
reports the version and source checksum.
