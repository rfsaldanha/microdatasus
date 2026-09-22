# Audit all supported DataSUS TabWin dictionaries

Downloads each physical archive only once per cache and inspects every
selected DEF/CNV/DBF relation. With a NULL selection, all current and
historical keys are audited (105 keys over 15 physical archives).

## Usage

``` r
audit_datasus_dictionaries(
  information_system = NULL,
  timeout = 240,
  refresh = FALSE,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL),
  fail_on_error = FALSE,
  fail_on_issues = FALSE
)
```

## Arguments

- information_system:

  NULL for every dictionary, or selected keys.

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

- fail_on_error:

  Logical scalar. If `TRUE`, abort after the audit when a dictionary
  download or unexpected parser/I/O error occurs. Known missing or
  invalid upstream relations remain represented in the result.

- fail_on_issues:

  Logical scalar. If `TRUE`, also abort when official relations are
  missing or invalid. Fallbacks and symbolic analytical ranges remain
  non-fatal.

## Value

A tibble with one row per dictionary and an `issues` list-column.
