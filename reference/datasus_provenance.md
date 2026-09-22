# Extract download provenance

Extract download provenance

## Usage

``` r
datasus_provenance(x)
```

## Arguments

- x:

  An object returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md),
  [`fetch_cadger()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_cadger.md),
  or
  [`fetch_sigtab()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_sigtab.md).

## Value

A tibble with one row per successfully read file, or NULL when
provenance was not requested.
