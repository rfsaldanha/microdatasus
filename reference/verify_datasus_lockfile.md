# Verify files pinned by a DataSUS reproducibility lockfile

Recomputes retained DBC and TabWin archive checksums and the checksums
of packaged reference tables. A status of `unavailable` means the
checksum remains pinned but the corresponding downloaded file is no
longer local; it does not imply a mismatch.

## Usage

``` r
verify_datasus_lockfile(lockfile)
```

## Arguments

- lockfile:

  A lockfile object or path returned by
  [`datasus_lockfile()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_lockfile.md).

## Value

A tibble with one row per pinned component and verification status.
