# Inspect packaged reference-table provenance

Lists packaged lookup objects and identifies whether each is still used
internally. The municipal reference has a pinned official source and an
explicit snapshot version. The other six legacy objects retain unknown
source dates rather than inferred provenance.

## Usage

``` r
datasus_reference_tables()
```

## Value

A tibble with source, date basis, source version and archive checksum
where known, dimensions, role, and a SHA-256 checksum of each serialized
table.

## Details

For `tabMun`, `source_date` is the TXT members' modification date inside
the official ZIP, not the validity date of all territorial attributes or
the package's original extraction date. Processors apply this fixed
snapshot; they do not select territorial boundaries from observation
dates. The original TXT members are supplied in
`system.file("extdata", "territory", "tabmun-source.zip", package = "microdatasus")`.
See
[tabMun](https://rfsaldanha.github.io/microdatasus/reference/tabMun.md)
for the documented compatibility transformations.
