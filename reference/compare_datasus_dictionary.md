# Compare cached and current DataSUS dictionaries

Compare cached and current DataSUS dictionaries

## Usage

``` r
compare_datasus_dictionary(
  information_system,
  previous = NULL,
  refresh = TRUE,
  timeout = 240,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL)
)
```

## Arguments

- information_system:

  A value accepted by datasus_variables().

- previous:

  Optional table previously returned by datasus_variables(). When NULL,
  the currently cached dictionary is used as the baseline.

- refresh:

  Logical scalar. If TRUE, download the current archive after capturing
  the baseline.

- timeout:

  A positive numeric scalar. Download and connection timeout, in
  seconds.

- quiet:

  Logical scalar. If `TRUE`, suppress download progress and status
  messages.

- cache_dir:

  Optional persistent cache root. The package option
  `microdatasus.cache_dir` is used by default; `NULL` uses only the
  current session cache.

## Value

A tibble describing added, removed, and changed fields, labels, symbolic
ranges, or relation states. Column `kind` identifies the item.

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
changes <- compare_datasus_dictionary("SIM-DO")
}
```
