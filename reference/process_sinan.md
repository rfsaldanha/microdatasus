# Prepare SINAN notification microdata

Uses the official DataSUS TabWin definitions to label all SINAN file
families supported by
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md).
The corresponding `TAB_SINANNET.zip` or `TAB_SINANONLINE.zip` archive is
downloaded on first use and cached for the rest of the R session. When
DataSUS publishes no disease-specific DEF, the official
`NotIndiviNet.def` supplies labels for common notification fields;
unmapped disease-specific codes remain visible. Historical chikungunya
records that use the former generic classification domain additionally
reuse its official relation from `TAB_SINANNET.zip`.

## Usage

``` r
process_sinan(
  data,
  information_system = "SINAN-DENGUE",
  municipality_data = TRUE,
  labels = c("factor", "character", "none"),
  diagnostics = FALSE
)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  for a supported SINAN file family, or another data frame with a
  compatible layout.

- information_system:

  SINAN file family represented by `data`. Preferred values use readable
  names such as `"SINAN-DENGUE"` and `"SINAN-TUBERCULOSE"`. All former
  acronym-based values remain accepted as aliases. Use
  [`datasus_information_systems()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_information_systems.md)
  and filter `system == "SINAN"` to consult both forms.

- municipality_data:

  Logical scalar. If `TRUE`, add municipality names and available
  territorial attributes. The historical `MUNICIPIO` field is preferred
  when present, followed by residence and notification fields.

- labels:

  Output type for categorical labels: `"factor"` (the default),
  `"character"`, or `"none"` to retain the original codes.

- diagnostics:

  Logical scalar. If `TRUE`, attach a processing report, including codes
  absent from official conversion tables. Retrieve it with
  [`processing_diagnostics()`](https://rfsaldanha.github.io/microdatasus/reference/processing_diagnostics.md).

## Value

A tibble. Dates are returned as `Date`, DEF increment fields and derived
age components as integer, labelled categorical fields as factors, and
identifiers and free text as character.

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

Saldanha, R. F. (2026). [SINAN – Sistema de Informação de Agravos de
Notificação](https://rfsaldanha.github.io/sis/sinan.html).

## See also

[`datasus_information_systems()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_information_systems.md),
[`fetch_tabwin_dictionary()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_tabwin_dictionary.md),
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
process_sinan(sinan_dengue_sample, "SINAN-DENGUE")
process_sinan(sinan_chagas_sample, "SINAN-DOENCA-DE-CHAGAS-AGUDA")
}
```
