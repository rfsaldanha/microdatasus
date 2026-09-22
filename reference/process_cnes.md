# Prepare CNES microdata

Uses the official DataSUS TabWin definitions to label all thirteen CNES
file families supported by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).
The shared ZIP is downloaded on first use and cached for the rest of the
R session. For `"CNES-SR"`, the service/classification definition is
selected by record competence because DataSUS changed that table in
March 2008.

## Usage

``` r
process_cnes(
  data,
  information_system = c("CNES-ST", "CNES-PF"),
  nomes = FALSE,
  municipality_data = TRUE,
  labels = c("factor", "character", "none"),
  diagnostics = FALSE
)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  for a supported CNES file family, or another data frame with a
  compatible layout.

- information_system:

  CNES file family represented by `data`. If omitted, `"CNES-ST"` is
  used, preserving the first value of the historical default.

- nomes:

  Logical scalar. If `TRUE`, add `FANTASIA` using the establishment-name
  DBF declared by the official definition. The `CNES` identifier itself
  is preserved.

- municipality_data:

  Logical scalar. If `TRUE`, add municipality names and available
  territorial attributes. Professional files prefer `UFMUNRES`; other
  layouts use the establishment field `CODUFMUN`.

- labels:

  Output type for categorical labels: `"factor"` (the default),
  `"character"`, or `"none"` to retain the original codes.

- diagnostics:

  Logical scalar. If `TRUE`, attach a processing report, including codes
  absent from official conversion tables. Retrieve it with
  [`processing_diagnostics()`](https://rfsaldanha.github.io/microdatasus/reference/processing_diagnostics.md).

## Value

A tibble. Full dates are returned as `Date`, quantities as integer,
labelled categorical fields as factors, and identifiers, reference
months, and free text as character.

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

Saldanha, R. F. (2026). [CNES – Cadastro Nacional de Estabelecimentos de
Saúde](https://rfsaldanha.github.io/sis/cnes.html).

## See also

[`fetch_tabwin_dictionary()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_tabwin_dictionary.md),
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md),
[`fetch_cadger()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_cadger.md)

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
process_cnes(cnes_st_sample, information_system = "CNES-ST")
process_cnes(cnes_pf_sample, information_system = "CNES-PF")
}
```
