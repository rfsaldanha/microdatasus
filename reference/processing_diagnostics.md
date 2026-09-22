# Extract processing diagnostics

Returns the optional report attached by a processing function called
with `diagnostics = TRUE`. The report includes input/output fields,
dictionary provenance and checksum, expected and unmapped fields,
unknown codes, and failed numeric or date coercions.

## Usage

``` r
processing_diagnostics(x)
```

## Arguments

- x:

  An object returned by a microdatasus processing function.

## Value

A `microdatasus_processing_diagnostics` list, or `NULL` when diagnostics
were not requested.

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
processed <- process_sim(
  sim_do_sample,
  municipality_data = FALSE,
  labels = "none",
  diagnostics = TRUE
)
processing_diagnostics(processed)
}
```
