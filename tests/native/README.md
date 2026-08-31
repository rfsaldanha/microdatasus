# Native DBC validation

These checks supplement the CRAN-safe `testthat` suite. Run them from the
repository root against an installed development build.

```sh
Rscript tests/native/fuzz-dbc.R
MICRODATASUS_GCTORTURE=true Rscript tests/native/fuzz-dbc.R
Rscript tests/native/check-dbc-corpus.R path/to/dbc/files
Rscript benchmarks/dbc-reader.R path/to/file.dbc FIELD1 FIELD2
```

`fuzz-dbc.R` exercises random inputs, 500 mutations of a valid DBC, and every
truncation of that seed. `check-dbc-corpus.R` compares every cell with the
legacy DBC-to-DBF-to-`foreign` path and verifies projected reads. The benchmark
reports median/minimum elapsed time and result size for full, projected, and
legacy reads.

For C memory and undefined-behavior checks, build with GCC's
`-fsanitize=address,undefined`, preload `libasan`, disable leak detection for
the uninstrumented R runtime, and run both native scripts. Any sanitizer report
is a failure.
