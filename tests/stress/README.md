# Opt-in stress tests

Run the live audit from the repository root:

```sh
Rscript --vanilla tests/stress/live-intensive.R
```

This command accesses the public DataSUS FTP service. It installs commit
`c1ad20b2034df021e3eb8105455efb72b8cc5aa5` into an isolated library, samples
approximately 150 complete DBC files, and tests raw downloads, processors,
cache reuse, provenance, and multi-file aggregation. It does not modify the
package implementation or use the globally installed `microdatasus` package.

The seed is `20260922`. The sample covers the six system families, all available
non-SINAN identifiers, 20 randomly selected SINAN identifiers, and the 16
historical layout-transition cases. Coverage extensions may increase the
sample to 180 files. Selection is reproducible against the saved directory
listings; future publications can change a newly generated sample.

Limits are four hours overall, three hours for new source-file requests,
20 GB of artifacts, 6 GB RSS per worker, and ten minutes per case. Workers
run sequentially. Network operations explicitly performed by the harness
use 60-second timeouts and the package's existing retry policy. Processors
use their normal defaults; required dictionaries are prefetched with the
60-second timeout. A supervisor also bounds calls with internal defaults.
Keep the host awake and connected during a run: operating-system sleep also
suspends the supervisor, so it can enforce expired deadlines only after wake.
Recorded wall-clock durations can include that suspension.

Artifacts are retained under `.cache/intensive/<UTC timestamp>/`. Set
`MICRODATASUS_INTENSIVE_ROOT` to choose another new run directory. The main
outputs are:

- `report.md`: summary, coverage, failures, and reproduction instructions.
- `sample.csv`, `groups.csv`, `catalog.csv`: frozen selection and discovery.
- `file-results.csv`, `results.csv`, `checks.csv`: file, case, and check results.
- `failures.csv`, `data-anomalies.csv`: failed checks versus source-data findings.
- `cases/<id>/`: arguments, logs, checkpoints, diagnostics, types, and lockfiles.
- `cache/`: retained source DBC files and official dictionaries.
- `runner.R`, `source/`, `library/`, `session-info.txt`: execution environment.

The audit compares every column exactly, including classes, values, attributes,
factor levels, and order. Table-level execution metadata is excluded. Unknown
source codes and reported conversion failures are diagnostic findings, not
automatic test failures. Cases stopped by a resource limit do not count as
passes. Intermediate per-file RDS outputs are discarded after comparison.

To replay a case without overwriting its original evidence:

```sh
Rscript --vanilla .cache/intensive/<run>/runner.R \
  --replay .cache/intensive/<run> file-001
```

Replay uses the pinned library and existing cache and writes a separate case
directory. It may retry missing downloads or dictionary retrievals.

To continue an interrupted run with the remaining cases and its original
deadline, run `Rscript --vanilla tests/stress/live-intensive.R --resume <run>`.
Completed cases are preserved; interrupted evidence is moved under
`interrupted/` before retrying the affected case.

Saved plans can also include `supplemental` cases with `download_only = TRUE`
and an explicit column selection. They compare all rows, aggregation, cache
reuse, and source integrity, but do not count as full processing passes.
Their independent reads use the same column selection. A projected case is
skipped when its corresponding complete case already failed during transport
at the same timeout: selecting fewer columns cannot change that transfer.

An optional `dictionary_failures_before_skip` value in the saved run
configuration bounds duplicate attempts for a shared dictionary archive after
that many completed timeout failures. Dependent cases still test their source
files and record processing as blocked; the original errors remain available.
A valid cached dictionary allows processing to continue. This safeguard is
disabled unless configured for the run, and `--replay` retries the dependency.

The separate `multipart-systems.R <run>` script exercises controlled multipart
downloads for every registered identifier using local DBC fixtures at the
network boundary. It checks discovery, reading, aggregation, cache reuse,
and `collect = FALSE`. Its default scenario sizes are 2, 3, 4, 32, and 100
parts. Set `MICRODATASUS_MULTIPART_COUNTS` to any comma-separated list of positive
integer counts of at least two to exercise other sizes; these are test sizes,
not limits imposed by the package. This controlled check does not establish
that every system currently publishes multipart files.

Run the harness's small offline checks with:

```sh
Rscript --vanilla tests/stress/live-intensive.R --self-test
```
