# Native DBC validation

These checks supplement the CRAN-safe `testthat` suite. Run them from the
repository root against an installed development build.

```sh
Rscript tests/native/fuzz-dbc.R
MICRODATASUS_GCTORTURE=true Rscript tests/native/fuzz-dbc.R
Rscript tests/native/check-dbc-corpus.R path/to/dbc/files
Rscript benchmarks/dbc-reader.R path/to/file.dbc FIELD1 FIELD2
MICRODATASUS_DBC_MATRIX_CACHE=.cache/dbc-matrix \
  MICRODATASUS_DBC_MATRIX_RESULTS=historical-results/dbc-matrix.csv \
  Rscript tests/smoke/historical-dbc-matrix.R
```

`fuzz-dbc.R` exercises random inputs, 500 mutations of a valid DBC, and every
truncation of that seed. `check-dbc-corpus.R` compares every cell with the
legacy DBC-to-DBF-to-`foreign` path and verifies projected reads. The benchmark
reports median/minimum elapsed time and result size for full, projected, and
legacy reads.

The ordinary test suite separately exercises UTF-8, CP1252, CP850, CP860,
mixed-row recovery, undefined CP1252 bytes, and lossless binary identifiers.
The Windows R CMD check is part of this contract because the platform `iconv`
implementation must not change strict or automatic decoding results.

`historical-dbc-matrix.R` discovers the DBC files actually published for every
registered DataSUS system and audits an old, middle, and recent period when
available. State-indexed systems use Acre as a deterministic representative;
this is not an all-state census. The script reads every field (in bounded
batches for large DBFs), verifies the full-file CRC and legacy converter, and
compares representative columns of safely decodable files cell by cell with
`foreign::read.dbf()`. The live network audit writes an incremental CSV and is
deliberately outside `R CMD check`.

For C memory and undefined-behavior checks, build with GCC's
`-fsanitize=address,undefined`, preload `libasan`, disable leak detection for
the uninstrumented R runtime, and run both native scripts. Any sanitizer report
is a failure.
