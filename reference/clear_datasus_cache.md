# Clear persistent DataSUS cache contents

Only the dbc, tabwin, and auxiliary subdirectories managed by
microdatasus are removed; the supplied cache root and unrelated files
are preserved.

## Usage

``` r
clear_datasus_cache(cache_dir = datasus_cache_dir())
```

## Arguments

- cache_dir:

  Cache root. The default is datasus_cache_dir().

## Value

The cache root, invisibly.
