# Prepare SIH hospital-admission microdata

Uses the official DataSUS TabWin definitions to label all four SIH file
families available from
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md):
reduced admissions (`"SIH-RD"`), rejected admissions (`"SIH-RJ"`),
professional services (`"SIH-SP"`), and rejected/error records
(`"SIH-ER"`). Dictionaries are downloaded on first use and cached for
the rest of the R session.

## Usage

``` r
process_sih(
  data,
  information_system = "SIH-RD",
  municipality_data = TRUE,
  labels = c("factor", "character", "none"),
  diagnostics = FALSE
)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  for a supported SIH file family, or another data frame with a
  compatible layout.

- information_system:

  SIH file family represented by `data`. One of `"SIH-RD"`, `"SIH-RJ"`,
  `"SIH-SP"`, or `"SIH-ER"`. The default preserves previous calls to
  `process_sih()`.

- municipality_data:

  Logical scalar. If `TRUE`, add municipality names and available
  territorial attributes for the residence municipality in RD, RJ, and
  ER files.

- labels:

  Output type for categorical labels: `"factor"` (the default),
  `"character"`, or `"none"` to retain the original codes.

- diagnostics:

  Logical scalar. If `TRUE`, attach a processing report, including codes
  absent from official conversion tables. Retrieve it with
  [`processing_diagnostics()`](https://rfsaldanha.github.io/microdatasus/reference/processing_diagnostics.md).

## Value

A tibble. Dates are returned as `Date`, counts, quantities, and derived
`IDADEdias`, `IDADEmeses`, and `IDADEanos` fields as integer, monetary
values as double, labelled categorical fields as factors, and
identifiers and free text as character. Derived age fields are added
when the source contains both `COD_IDADE` and `IDADE`.

## Details

For RD and RJ, the definition is selected from the official historical
archives according to each row's competence. This supports data sets
concatenated across the 1997, July 2003, and 2007 layout boundaries.
Codes absent from a conversion table remain visible as factor levels.

## Performance and cache

Processing uses vectorized code padding and CNV thresholds, parses
repeated dates once per field and format, and unescapes only text
containing backslashes. UTF-8 conversion is still performed for text;
values marked as `"bytes"` bypass text normalization. Historical
relation selection subsets only the source columns it needs. These
optimizations are automatic.

Dictionaries are reused within the R session. For reuse across sessions,
set
`options(microdatasus.cache_dir = datasus_cache_dir(create = TRUE))`;
calling
[`datasus_cache_dir()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_cache_dir.md)
alone does not enable persistent caching. The first processing call can
include dictionary downloads and parsing. `labels = "none"` controls
categorical output, not network access: some processors still need DEF
metadata or relations for field semantics.

`diagnostics = FALSE` avoids collecting the optional report, and
`municipality_data = FALSE` omits territorial enrichment when it is not
needed. For requests spanning many files, use
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
with `process = TRUE`, `collect = FALSE`, and `destination` to save each
file separately. A processor called directly still holds its input and
output in memory. See the [processing
guide](https://rfsaldanha.github.io/microdatasus/articles/dicionarios-cache-e-escala.html).

Territorial enrichment uses the fixed
[tabMun](https://rfsaldanha.github.io/microdatasus/reference/tabMun.md)
snapshot identified by
[`datasus_reference_tables()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_reference_tables.md),
not an automatically selected edition for each observation year. Enable
diagnostics to record that version in the report.

## References

Saldanha, R. F. (2026). [SIH – Sistema de Informações Hospitalares do
SUS](https://rfsaldanha.github.io/sis/sih.html).

## See also

[`fetch_tabwin_dictionary()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_tabwin_dictionary.md),
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
process_sih(sih_rd_sample)
}
```
