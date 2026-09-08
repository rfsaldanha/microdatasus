# Performance benchmarks

Run these scripts from the repository root. They measure different parts of
the pipeline: `processing.R` exercises processors and conversion helpers;
`dbc-reader.R` measures reading a local DBC. Neither is an end-to-end download
benchmark or a substitute for correctness tests.

## Processing regression checks

Use a fresh R process so that the current checkout is loaded, rather than an
older installed release:

```sh
Rscript -e 'devtools::load_all(".", quiet = TRUE); source("benchmarks/processing.R")'
```

The CI workflow installs the checkout first and then runs
`Rscript benchmarks/processing.R`. The script populates the package's session
dictionary cache with **synthetic empty dictionaries** to avoid network access.
Do not source it in an R session used for real analyses: those cache entries
are not official DataSUS dictionaries.

The current cases are:

| Cases | Input | Scope |
|---|---|---|
| `SIM_DO`, `SINASC`, `SIH_RD`, `SIA_PA`, `CNES_ST`, `SINAN_DENGUE` | Each packaged sample repeated 1,000 times | Six processors with `labels = "none"` and `municipality_data = FALSE` |
| `LABEL_CHARACTER`, `LABEL_FACTOR` | 1,000,000 values repeating three codes | Exact synthetic CNV mapping, with and without factor construction |
| `LABEL_THRESHOLD` | 1,000,000 values repeating six codes | Three inclusive upper thresholds, including boundaries and an unmatched code |
| `DATE_DMY` | 1,000,000 values repeating five strings | Valid dates, a missing-date code and invalid text |
| `TEXT_NORMALIZE` | 1,000,000 values repeating four strings | Plain text, accented text, a Unicode escape and `NA` |

Each case is timed once after `gc()`. The script writes
`benchmarks/results.csv`, replacing any previous result at that path. This
generated file is ignored by Git. Its columns are:

- `case`: benchmark identifier;
- `rows`: input rows or vector length;
- `elapsed_seconds`: elapsed time for the timed operation;
- `output_rows`: output rows or vector length.

The run fails if a case exceeds its limit in `budgets.csv`. The optional
`MICRODATASUS_BENCHMARK_MAX_SECONDS` environment variable adds a common
per-case limit; it does not replace the CSV budgets. These broad limits guard
against large regressions and are not speed guarantees or measurements of
statistical significance.

Repeated samples favor low-cardinality paths, such as repeated-date parsing.
Empty dictionaries exclude the cost of downloading, parsing and applying
official relations in the six processor cases. The separate conversion cases
do not represent the full variety of official CNV files. These checks do not
measure cold-cache behavior, default processing with labels and territorial
enrichment, mixed historical layouts, high-cardinality fields or peak memory.
They record row counts but do not prove output equivalence; the unit and
integration tests cover correctness separately.

For a representative comparison, use the same original DBCs, dictionaries and
processing options for both versions. Keep raw input separate from processed
output, distinguish first-call costs from later calls, repeat measurements,
and record the R session and machine configuration. The
[processing guide](https://rfsaldanha.github.io/microdatasus/articles/dicionarios-cache-e-escala.html)
includes a public-API example for measuring time with your own data. Final
object size is not peak memory: input, dictionaries and temporary objects also
consume memory.

## DBC reader comparison

Supply an existing local DBC and, optionally, column names actually present in
that file. For example, for a SIM-DO file containing these fields:

```sh
Rscript benchmarks/dbc-reader.R /path/to/SIM-DO.dbc DTOBITO CODMUNRES
```

This script loads the checkout with `devtools::load_all()` and measures:

- `direct`: the complete table using `read_dbc()`;
- `projected`: only the requested columns, when supplied;
- `legacy`: temporary DBC-to-DBF conversion followed by `foreign::read.dbf()`.

Each operation runs five times, with garbage collection before each timing.
Output contains the median and minimum elapsed seconds and `object_mb`, the
final object's size in MiB. It does not measure peak memory, and filesystem
caches can affect the timings. The direct path uses the public reader's
defaults, including character output; the legacy path uses
`foreign::read.dbf(as.is = TRUE)`. These are different output policies, so the
timings alone do not establish semantic equivalence.

For value comparisons, `tests/native/check-dbc-corpus.R` separately checks
complete and projected reads against the legacy path with typed output and
explicit encoding normalization. Use that checker with the intended package
version installed and the corpus under investigation; success only establishes
agreement for those files.
