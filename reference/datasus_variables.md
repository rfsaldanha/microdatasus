# Consult variables in an official DataSUS dictionary

Downloads or reuses a TabWin dictionary and presents its variable
metadata and code-label maps as a rectangular lookup table.

## Usage

``` r
datasus_variables(
  information_system,
  include_labels = TRUE,
  timeout = 240,
  refresh = FALSE,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL),
  fields = NULL,
  view = c("definitions", "fields"),
  include_ranges = TRUE
)
```

## Arguments

- information_system:

  A value accepted by fetch_tabwin_dictionary().

- include_labels:

  Logical scalar. If TRUE, parse CNV and DBF relations and include their
  code-label tables in the labels list-column.

- timeout:

  A positive numeric scalar. Download and connection timeout, in
  seconds.

- refresh:

  Logical scalar. If `TRUE`, discard the session cache and download the
  archive again.

- quiet:

  Logical scalar. If `TRUE`, suppress download progress and status
  messages.

- cache_dir:

  Optional persistent cache root. The package option
  `microdatasus.cache_dir` is used by default; `NULL` uses only the
  current session cache.

- fields:

  Optional character vector restricting the returned fields.

- view:

  Either `"definitions"`, with one row per DEF declaration, or
  `"fields"`, with repeated declarations grouped in a list-column.

- include_ranges:

  Logical scalar. If TRUE, include symbolic CNV interval rules in the
  `ranges` list-column.

## Value

A tibble with one row per categorical definition or numeric field.

## Details

Large analytical CNV ranges are retained as symbolic rules instead of
being expanded into millions of rows. `status` distinguishes complete,
fallback, non-enumerable, missing, invalid, and failed relations. A
`issue_class` independently identifies upstream absence/content drift,
archive ambiguity, parser/I/O errors, analytical ranges, or definition
fallback, so severity is not confused with origin. Fallback is reported
when an official two-column DBF renamed its sole description field while
the DEF retained the previous name. DBFs with duplicate keys and
conflicting labels are also explicit fallbacks: the last physical record
is retained, following TabWin precedence. A CNV whose declared category
count differs from its physical definitions is reported as an upstream
fallback, while every physical category is retained. Parsed CNV codes
likewise use the last physical definition; repeated category rows use
the last non-blank description. Relations persist on disk when
`cache_dir` is set, and completed result tables are reused during the R
session.

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
variables <- datasus_variables("SIM-DO", include_labels = FALSE)
variables[, c("field", "type", "description")]
}
```
