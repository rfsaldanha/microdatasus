# Prepare SIM mortality microdata

Uses the official DataSUS TabWin CID-10 and historical CID-9
dictionaries to label supported SIM mortality fields with period-correct
domains. The dictionary is downloaded on first use and cached for the
rest of the R session. Dates, integer quantities, categorical variables,
and identifier fields retain distinct and consistent types.

## Usage

``` r
process_sim(
  data,
  municipality_data = TRUE,
  information_system = "SIM-DO",
  labels = c("factor", "character", "none"),
  diagnostics = FALSE
)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  for a supported SIM mortality type, or another data frame with a
  compatible layout.

- municipality_data:

  Logical scalar. If `TRUE`, add municipality names and available
  territorial attributes for `CODMUNRES`.

- information_system:

  SIM data type represented by `data`. One of `"SIM-DO"`, `"SIM-DOFET"`,
  `"SIM-DOEXT"`, `"SIM-DOINF"`, or `"SIM-DOMAT"`. The default preserves
  the previous `process_sim()` call.

- labels:

  Output type for categorical labels: `"factor"` (the default),
  `"character"`, or `"none"` to retain the original codes.

- diagnostics:

  Logical scalar. If `TRUE`, attach a processing report, including codes
  absent from official conversion tables. Retrieve it with
  [`processing_diagnostics()`](https://rfsaldanha.github.io/microdatasus/reference/processing_diagnostics.md).

## Value

A tibble. Dates are returned as `Date`, quantities as integer, labelled
categorical fields as factors, and identifiers and free text as
character.

## Details

Codes not covered by the applicable TabWin conversion are retained as
factor levels instead of being silently discarded. Historical fields
kept in early CID-10 fetal-death files use their original CID-9
definitions. Official numeric missing sentinels and out-of-domain
measurements are returned as `NA`; malformed values are included in
diagnostics.

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

Saldanha, R. F. (2026). [SIM – Sistema de Informação sobre
Mortalidade](https://rfsaldanha.github.io/sis/sim.html).

## See also

[`fetch_tabwin_dictionary()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_tabwin_dictionary.md),
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
process_sim(sim_do_sample)
}
```
