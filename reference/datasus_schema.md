# Build a dictionary-derived DataSUS schema contract

Summarises all fields declared by one official TabWin DEF. The contract
describes dictionary roles and relations; fields absent from the DEF,
such as free text, remain discoverable only in the corresponding DBC
layout.

## Usage

``` r
datasus_schema(
  information_system,
  inspect = FALSE,
  timeout = 240,
  refresh = FALSE,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL)
)
```

## Arguments

- information_system:

  One dictionary key accepted by
  [`fetch_tabwin_dictionary()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_tabwin_dictionary.md).

- inspect:

  Logical scalar. If `TRUE`, parse relations and include their status;
  otherwise build the contract from DEF metadata only.

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

## Value

A tibble with one row per field and list-columns containing every
description, relation, file, command, and status declared for that
field.
