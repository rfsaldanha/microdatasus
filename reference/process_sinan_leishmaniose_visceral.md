# Prepare SINAN visceral leishmaniasis microdata (deprecated)

`process_sinan_leishmaniose_visceral()` is retained for backward
compatibility. New code should call
[`process_sinan()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinan.md)
with `information_system = "SINAN-LEISHMANIOSE-VISCERAL"`.

## Usage

``` r
process_sinan_leishmaniose_visceral(data, municipality_data = TRUE)
```

## Arguments

- data:

  A data frame returned by
  [`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
  for a supported SINAN file family, or another data frame with a
  compatible layout.

- municipality_data:

  Logical scalar. If `TRUE`, add municipality names and available
  territorial attributes. The historical `MUNICIPIO` field is preferred
  when present, followed by residence and notification fields.

## Value

The tibble returned by
[`process_sinan()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinan.md).

## See also

[`process_sinan()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinan.md)

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
process_sinan_leishmaniose_visceral(sinan_leishmaniose_visceral_sample)
}
```
