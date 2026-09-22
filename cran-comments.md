## Development validation (2026-09-22)

These notes describe version 3.0.0.9000 on the `dev` branch. Refresh them for
the release tarball before submitting to CRAN; they are not a release submission.
The package reads DBC files using vendored C source and does not require
`read.dbc`.

## Local checks

Environment: macOS 26.6.2 (arm64), R 4.6.1.

`R CMD check --as-cran --no-manual`:

0 errors | 0 warnings | 1 note

The incoming-check note includes `Version contains large components
(3.0.0.9000)`. This is the development version identifier; choose the release
version before submission. The documentation URLs fixed in commit 5e125d7 no
longer appear as invalid.

The latest check also reported transient transport failures when checking the
maintainer's ORCID (connection reset) and DOI 10.1590/0102-311x00032419 (broken
pipe). Both returned HTTP 200 in a subsequent `curl --head --location` check,
following the DOI's redirects. These identifiers have not been changed; repeat
their checks with the release tarball. An earlier attempt was interrupted by
DNS failures contacting CRAN/Bioconductor and is not counted as a passed check.

An additional check with unavailable network access passed with 0 errors,
0 warnings and 0 notes. Its test suite reported 5,278 passes and one skipped
opt-in live test. This validates the examples and tests without relying on
the DataSUS server, not the availability of downloads for end users.

## Cross-platform checks

The CI run for commit 5e125d7 passed on Windows and macOS with R-release, and
on Linux with R-release, R-devel and R-oldrel-1:
https://github.com/rfsaldanha/microdatasus/actions/runs/35777711657

## Internet resources

Download examples are conditional on an interactive session with Internet
access. Automated tests use local fixtures and mocked connections; live tests
are opt-in and skipped on CRAN. URL availability is checked separately.

The municipal reference cites the official DataSUS HTTPS file-transfer portal.
Its original FTP archive address is retained as provenance text, with an
explanation of network-dependent access. The original TXT/layout members
needed to reconstruct the reference are bundled for offline use.

The processing guide is now published by the documentation workflow, which
also runs on pushes to `dev`.

## Release preparation

Recheck reverse dependencies and record the results for the chosen release;
the previous submission's dependency status is not assumed to remain current.
