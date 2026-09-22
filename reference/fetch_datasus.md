# Download DataSUS microdata

Downloads published DBC files from DataSUS, reads them with
[`read_dbc()`](https://rfsaldanha.github.io/microdatasus/reference/read_dbc.md),
and combines the records in deterministic period, state, and file-part
order.

## Usage

``` r
fetch_datasus(
  year_start,
  month_start = NULL,
  year_end,
  month_end = NULL,
  uf = "all",
  information_system,
  vars = NULL,
  stop_on_error = FALSE,
  timeout = 240,
  track_source = FALSE,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL),
  refresh = FALSE,
  destination = NULL,
  collect = TRUE,
  process = FALSE,
  process_args = list(),
  provenance = FALSE,
  keep_files = FALSE,
  row_filter = NULL
)
```

## Arguments

- year_start, year_end:

  Numeric scalars giving the first and last requested years, inclusive.

- month_start, month_end:

  Numeric scalars giving the first and last requested months, inclusive.
  Months are required for SIH, SIA, and CNES systems and ignored, with a
  warning, for annual systems.

- uf:

  A Brazilian state abbreviation, a character vector of abbreviations,
  or `"all"`. `"all"` cannot be combined with individual states. A
  warning alert is displayed when this argument is ignored for systems
  published only as national files.

- information_system:

  A single system identifier listed in **Supported systems**.

- vars:

  `NULL`, or a character vector of column names to retain. When neither
  `process` nor `row_filter` needs a complete row, selection is pushed
  into the DBC reader so unselected fields are not allocated or parsed.
  With `process = TRUE` or `row_filter`, the full row is read; `vars`
  selects output columns after filtering and processing. It can
  therefore include derived fields when `process = TRUE`. Selection is
  always applied before files are combined.

- stop_on_error:

  Logical scalar. If `TRUE`, abort after any listing, download, or read
  failure. If `FALSE`, warn and return the files that could be read
  successfully.

- timeout:

  A positive numeric scalar giving the connection and transfer timeout,
  in seconds, for each network attempt.

- track_source:

  Logical scalar. If `TRUE`, append a `source` column with the original
  DBC file name. This column is retained even when `vars` is supplied.
  The function aborts if the downloaded data already contain a column
  named `source`.

- quiet:

  Logical scalar. If `FALSE` (the default), display the transfer
  progress reported by
  [`curl::curl_download()`](https://jeroen.r-universe.dev/curl/reference/curl_download.html)
  and announce each file before downloading it. If `TRUE`, suppress
  status messages, per-file announcements, and progress meters. Warnings
  and errors are not suppressed.

- cache_dir:

  Optional directory used as a persistent cache for downloaded DBC files
  and TabWin dictionaries. The `microdatasus.cache_dir` option supplies
  the default; `NULL` preserves the historical session-temporary
  behavior.

- refresh:

  Logical scalar. If `TRUE`, download files again even when a valid
  cached copy is available.

- destination:

  Optional directory in which each prepared file is saved separately as
  RDS. This supports requests too large to combine in memory.

- collect:

  Logical scalar. If `TRUE` (the default), combine and return records as
  before. If `FALSE`, write per-file RDS outputs and return their
  provenance table; in that case `destination` is required.

- process:

  Logical scalar. If `TRUE`, run the processor matching
  `information_system` independently on each downloaded file.

- process_args:

  Named list of additional arguments passed to the matching
  `process_*()` function. It cannot replace `data` or
  `information_system`.

- provenance:

  Logical scalar. If `TRUE`, attach download URLs, checksums, sizes,
  timestamps, cache status, and paths. Retrieve the table with
  [`datasus_provenance()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_provenance.md).

- keep_files:

  Logical scalar. If `TRUE` and `destination` is supplied, retain a copy
  of each raw DBC file under `destination/dbc`.

- row_filter:

  Optional function called on each raw DBC table immediately after
  reading and before processing or column selection. It must return one
  non-missing logical value per row. This bounds downstream processing
  and output without changing which source files are downloaded.

## Value

With `collect = TRUE`, a tibble containing all successfully read
records, or `NULL` if no requested file could be read. With
`collect = FALSE`, a provenance tibble with one row per output file.

## Details

The function first lists the relevant DataSUS directories and downloads
only files present in those listings. If a state-period or national
period is published in multiple parts, every listed part is downloaded
and combined; there is no fixed limit on their number or suffix length.
Part names are taken from the listing, including their original case,
rather than generated from a sequence of letters. Parts are discovered
independently for each state and period, so different states need not
have the same number of files. When more than one publication represents
the same system, period, state, and file part, definitive/current data
take precedence over preliminary data, and current data take precedence
over historical copies.

Files are handled sequentially and, when requested, processed and
written before the next file is read. Thus `collect = FALSE` bounds
working memory to approximately one source file. Unless `quiet = TRUE`,
transfer progress is displayed by
[`curl::curl_download()`](https://jeroen.r-universe.dev/curl/reference/curl_download.html).
Transient network failures are retried up to two times; missing, empty,
invalid DBC, and incompatible-schema files are not retried. Partial
files and other temporary files are removed before the function returns
or aborts.

When `cache_dir` is supplied, complete DBC files and dictionaries
persist across R sessions. Cache entries include a manifest and SHA-256
checksum (while still accepting legacy MD5 manifests).
[`datasus_cache_info()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_cache_info.md)
inspects them and
[`clear_datasus_cache()`](https://rfsaldanha.github.io/microdatasus/reference/clear_datasus_cache.md)
removes only files managed by microdatasus.

For processed downloads, `cache_dir` also supplies the dictionary cache
to the processor for that call. `row_filter` sees raw codes and must
handle missing values explicitly. Use
`process_args = list(diagnostics = TRUE)` alongside `provenance = TRUE`
when a
[`datasus_lockfile()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_lockfile.md)
should also record the dictionaries and reference tables actually used
in processing.

Years and state abbreviations refer to DataSUS processing periods and
places of processing, which may differ from dates or places of
occurrence and residence contained in the records.

## Supported systems

- **SIH:** `"SIH-RD"`, `"SIH-RJ"`, `"SIH-SP"`, and `"SIH-ER"`.

- **SIM:** `"SIM-DO"`, `"SIM-DOFET"`, `"SIM-DOEXT"`, `"SIM-DOINF"`, and
  `"SIM-DOMAT"`.

- **SINASC:** `"SINASC"`.

- **CNES:** `"CNES-LT"`, `"CNES-ST"`, `"CNES-DC"`, `"CNES-EQ"`,
  `"CNES-SR"`, `"CNES-HB"`, `"CNES-PF"`, `"CNES-EP"`, `"CNES-RC"`,
  `"CNES-IN"`, `"CNES-EE"`, `"CNES-EF"`, and `"CNES-GM"`.

- **SIA:** `"SIA-AB"`, `"SIA-ABO"`, `"SIA-ACF"`, `"SIA-AD"`, `"SIA-AN"`,
  `"SIA-AM"`, `"SIA-AQ"`, `"SIA-AR"`, `"SIA-ATD"`, `"SIA-PA"`,
  `"SIA-PS"`, and `"SIA-SAD"`.

- **SINAN:** 58 readable identifiers, including `"SINAN-DENGUE"`,
  `"SINAN-TUBERCULOSE"`, and `"SINAN-ACIDENTE-POR-ANIMAIS-PECONHENTOS"`.
  Former acronym-based identifiers remain accepted as aliases. The
  complete lookup table is returned by
  [`datasus_information_systems()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_information_systems.md).

## Network access

An Internet connection and FTP access to DataSUS are required. DataSUS
may restrict FTP access from some countries. Interrupted transfers are
resumed when supported. Alternative base URLs can be configured with the
`microdatasus.mirrors` option; the official DataSUS URL is always tried
first. General Internet connectivity does not guarantee access to the
DataSUS server. With `stop_on_error = FALSE`, failed listings or
downloads are reported as warnings and successful files are retained;
the result is `NULL` if no file can be read. Check for `NULL` before
processing the result. Set `stop_on_error = TRUE` when an incomplete
download must interrupt an analysis.

## References

Saldanha, R. F. (2026). [*Sistemas de Informação em Saúde no
Brasil*](https://rfsaldanha.github.io/sis/), especially the chapters on
[SIM](https://rfsaldanha.github.io/sis/sim.html),
[SINASC](https://rfsaldanha.github.io/sis/sinasc.html),
[SIH](https://rfsaldanha.github.io/sis/sih.html),
[SIA](https://rfsaldanha.github.io/sis/sia.html),
[SINAN](https://rfsaldanha.github.io/sis/sinan.html), and
[CNES](https://rfsaldanha.github.io/sis/cnes.html).

## See also

[`read_dbc()`](https://rfsaldanha.github.io/microdatasus/reference/read_dbc.md)
for local DBC files;
[`datasus_information_systems()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_information_systems.md)
for the complete identifier lookup;
[`process_sim()`](https://rfsaldanha.github.io/microdatasus/reference/process_sim.md),
[`process_sinasc()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinasc.md),
[`process_sih()`](https://rfsaldanha.github.io/microdatasus/reference/process_sih.md),
[`process_sia()`](https://rfsaldanha.github.io/microdatasus/reference/process_sia.md),
[`process_cnes()`](https://rfsaldanha.github.io/microdatasus/reference/process_cnes.md),
and
[`process_sinan()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinan.md)
for system-specific recoding.

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
sim <- fetch_datasus(
  year_start = 2014,
  year_end = 2014,
  uf = "AC",
  information_system = "SIM-DO",
  vars = c("CODMUNRES", "DTOBITO", "CAUSABAS")
)

sih <- fetch_datasus(
  year_start = 2014,
  month_start = 1,
  year_end = 2014,
  month_end = 2,
  uf = c("AC", "RR"),
  information_system = "SIH-RD"
)
}
```
