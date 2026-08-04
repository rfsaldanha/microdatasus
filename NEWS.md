# microdatasus 3.0.0.9000

## Development

* Adds per-file `row_filter` execution before processing, deterministic schema
  sampling across historical layouts, and richer coercion/unknown-code counts.
* Uses SHA-256 for new cache manifests while reading legacy MD5 manifests,
  supports resumable transfers and optional `microdatasus.mirrors`, and brings
  CADGER/SIGTAB downloads into the persistent cache and provenance model.
* Adds reproducibility lockfiles with request, source, dictionary, parser, and
  packaged-reference checksums through `datasus_lockfile()` and verification
  helpers.
* Makes packaged lookup-table provenance explicit with
  `datasus_reference_tables()`, normalizes escaped UTF-8 values, and reports
  legacy reference use in processing diagnostics.
* Classifies dictionary issues by origin, adds strict `fail_on_issues` audits,
  rotating coverage of every registered subsystem, and live historical-layout
  transition checks.

# microdatasus 3.0.0

## Downloads

* Adds an optional persistent cache for DBC and TabWin ZIP files, with
  manifests, MD5 integrity checks, inspection through `datasus_cache_info()`,
  and targeted cleanup through `clear_datasus_cache()`.
* Extends `fetch_datasus()` with per-file processing and RDS output. Setting
  `collect = FALSE` bounds memory use to roughly one source file and returns a
  provenance manifest; existing calls retain their combined-tibble behavior.
* Adds opt-in download provenance with source URL, size, checksum, timestamp,
  cache status, and local/output paths.

* Reworks `fetch_datasus()` around an internal registry of supported systems and
  manifests built from the files actually published by DataSUS. Each relevant
  FTP directory is listed only once per call, avoiding redundant connections
  and URLs for files that do not exist.
* Selects a single file for each system, period, state, and file part.
  Definitive/current data take precedence over preliminary data, and current
  data take precedence over historical copies. Results retain deterministic
  period, state, and file-part order.
* Uses `curl` directly for directory listings and downloads, with per-operation
  timeouts and up to two retries for transient network failures. Downloads
  remain sequential, display transfer progress by default, and no longer modify
  the global `options("timeout")`.
* Downloads through temporary partial files, validates file size and DBC
  contents, and reliably removes temporary files. With `stop_on_error = FALSE`,
  successfully read files are returned after a consolidated warning; with
  `stop_on_error = TRUE`, any failure aborts the operation.
* Applies `vars` before accumulating results and combines files only once,
  reducing repeated copies. When `track_source = TRUE`, the `source` column is
  retained even when `vars` is supplied, and an existing `source` column now
  produces a clear error.
* Strengthens validation of years, months, states, logical arguments, `vars`,
  and fractional `timeout` values. Monthly systems require months, annual
  systems ignore them with one warning, and `"all"` can no longer be combined
  with individual states.
* Replaces the former global 1996 lower-year restriction with historical limits
  specific to each information system. Existing public arguments, their order,
  and their defaults remain unchanged.
* Adds `quiet` at the end of the `fetch_datasus()` signature. Transfer progress
  and status messages, including each file name, are displayed by default; set
  `quiet = TRUE` to hide them. Warnings and errors remain visible.
* Uses immediate `cli` warning alerts when month arguments or state selections
  are ignored, including when `quiet = TRUE`.
* Standardizes download messages across microdata and auxiliary tables, with
  consistent status verbs, semantic `cli` formatting, indexed file progress,
  and one diagnostic item per failed directory or file.
* Hardens `fetch_cadger()` and `fetch_sigtab()` with shared timeout, retry, and
  temporary-file cleanup logic, and adds ZIP integrity and schema validation.

## DBC support

* Removes the dependency on `read.dbc` and reads DBC files with vendored C code
  adapted from `healthbR`. Thanks to Sidney Bissoli for the open-source code.
* Adds `read_dbc()` for local DBC files, with optional preservation of the
  column types inferred from DBF metadata.
* Hardens `read_dbc()` by rejecting directories and empty or unreadable input
  files before decompression, validating missing, unknown-size, and empty
  decompression output, and wrapping DBF read failures in contextual errors
  with stable `microdatasus_dbc_error` subclasses.
* Expands local, CRAN-safe DBC tests for argument validation, type preservation,
  leading zeros, paths with spaces, repeat reads, temporary-file cleanup, and
  specific malformed header and compressed-stream errors.

## Processing, documentation, and testing

* Adds `datasus_variables()` to inspect official DEF/CNV/DBF metadata and
  `compare_datasus_dictionary()` to report added, removed, or changed fields
  and labels between dictionary versions.
* Supports the compact, `s`, and long-description `N` CNV dialects found
  in current official archives, legacy-encoded ZIP member names, codes wider
  than 32-bit integers, and audited two-column DBF description fallbacks.
* Adds `validate_datasus_schema()` to join observed DBC fields, selected
  current/historical DEF declarations, and the types produced by the matching
  processor for all 93 downloadable families.
* Preserves very large CNV intervals as symbolic rules instead of expanding
  them in memory, reports missing, invalid, non-enumerable, and failed
  relations explicitly, and compares relation states and interval changes.
* Adds `datasus_schema()` for dictionary-derived field contracts and
  `audit_datasus_dictionaries()` for all 104 current and historical TabWin
  definitions, while downloading each of the 14 physical archives only once.
* Persists parsed CNV/DBF relations by archive checksum and parser version,
  caches assembled variable tables in the R session, and serializes competing
  cache writes with atomic locks and uniquely named partial files.
* Expands processing diagnostics with dictionary provenance, missing expected
  fields, unmapped input fields, and numeric/date coercion failures.
* Gives every unified `process_*()` function the same `labels` policy
  (`"factor"`, `"character"`, or `"none"`) and optional diagnostics for
  unmapped codes, while appending all new arguments to preserve established
  calls.
* Expands coverage gates, processing benchmarks and scheduled live DataSUS
  smoke tests across SIM, SINASC, SIH, SIA, CNES, and SINAN; adds a monthly
  all-dictionary audit plus deterministic malformed-DBC, Valgrind, and UBSan
  safety workflows.

* Replaces short SINAN acronym identifiers with readable canonical names,
  while retaining every previous value as a silent backward-compatible alias.
  The new `datasus_information_systems()` lookup table lists all 93 supported
  file families and keeps names, DBC acronyms, periodicity, geography, and
  aliases synchronized with download and TabWin registries.
* Adds the unified `process_sinan()` based on the official
  `TAB_SINANNET.zip` and `TAB_SINANONLINE.zip` definitions and expands
  `fetch_datasus()` from eight to all 58 SINAN families listed by the transfer
  portal. The seven former processors retain their original signatures as
  deprecated wrappers. Dates, encoded ages, municipality codes, labels, and
  identifiers now follow one tested type policy.
* Rebuilds `process_cnes()` from the official DataSUS TabWin definitions. It
  now covers all thirteen downloadable CNES layouts, selects the historical or
  current service-classification definition row by row, shares the large
  `TAB_CNES.zip` download for the R session, reads numeric increment metadata
  from DEF, and preserves identifiers and all existing arguments.
* Rebuilds `process_sia()` from the official DataSUS TabWin definitions. It now
  covers all twelve downloadable SIA layouts, selects three historical PA
  definitions by record competence, shares archives in the session cache, and
  preserves all existing arguments while making their procedure, occupation,
  and team switches effective.
* Rebuilds `process_sih()` from the official DataSUS TabWin definitions. The
  function now processes `SIH-RD`, `SIH-RJ`, `SIH-SP`, and `SIH-ER`, selects
  the three historical RD/RJ archives by record competence, caches shared
  downloads for the R session, and standardizes dates, quantities, values,
  labels, and identifiers while preserving its existing arguments.
* Standardizes patient age across processors without changing the source
  fields. SIH-RD/RJ and every applicable SIA layout now add integer
  `IDADEdias`, `IDADEmeses`, or `IDADEanos` columns from the official TabWin
  unit conventions; SIM and SINAN use the same shared internal decoder.
* Speeds processing of large tables by applying row-specific historical
  dictionaries in one pass per field, factorizing only once, avoiding a second
  full-table UTF-8 normalization, and using direct numeric coercion when no
  special missing-value codes need preprocessing.
* Adopts `dplyr::recode_values()` because `dplyr::case_match()` is deprecated.
* Expands and corrects the documentation for all public functions, including
  network behavior, actual return types, compatibility arguments, and links to
  the corresponding chapters of the book *Sistemas de Informação em Saúde no
  Brasil*.
* Replaces skipped download tests with deterministic simulated listings and
  transfers. The suite now covers supported systems, historical, current, and
  preliminary files, multipart data, complete argument validation, retries,
  timeouts, empty listings and manifests, cleanup, corrupt and empty files,
  partial success, `vars`, `track_source`, multiple states, and auxiliary-table
  schema validation. Local `file://` fixtures exercise `curl` without network
  access; live DataSUS smoke tests remain opt-in and are always skipped on CRAN.
* Runs the active GitHub Actions R CMD check workflow for pushes and pull
  requests involving the `dev` branch as well as `main` and `master`.
* Requires R 4.1.0 or later.

# microdatasus 2.5.0
* Function `process_sinan_malaria`, fix variable type when pre-processing the variable `NU_IDADE_N` (#132)
* Function `process_sinasc`, fix typo when pre-processing the variable `PESO` (#141).
* Function `process_sia`, fix typo when pre-processing the variable `PA_MUNPCN` (#137).
* Function `process_cnes` for CNES-ST, fix `MICR_REG` and `DISTRSAN` conversion.
* Function `process_sih`, fix `NACIONAL` and `HOMONIMO` codes, add more `FAEC_TP` codes (#131).
* Remove `dtplyr` package dependence.

# microdatasus 2.4.3
* Remove `dtplyr` usage due incompatibility on recent version.

# microdatasus 2.4.2
* Fix error when processing data with `process_sinan_chikungunya()`.

# microdatasus 2.4.1
* Fix typo in process functions that was changing variable names (`process_sim()`, `process_sinasc()`,`process_sinan_zika()`, `process_sinan_malaria()`, `process_sinan_leishmaniose_visceral()`, `process_sinan_leishmaniose_tegumentar()`, `process_sinan_dengue()`, `process_sinan_chikungunya()`, `process_sinan_chagas()`).

# microdatasus 2.4.0
* Update documentation on `fetch_datasus()`.
* Fetch SINAN-CHAGAS data.
* Process SINAN-CHAGAS data.
* Fetch SINAN-LEISHMANIOSE-VISCERAL and SINAN-LEISHMANIOSE-TEGUMENTAR data.
* Process SINAN-LEISHMANIOSE-VISCERAL and SINAN-LEISHMANIOSE-TEGUMENTAR data.
* Fetch SINAN-LEPTOSPIROSE data.
* Fix message when downloading data for all UFs.

# microdatasus 2.3.5
* Treat age field `NU_IDADE` on pre-processing SINAN on old files.

# microdatasus 2.3.4
* Fix error on tabNaturalidade, issue #123. Thanks @SophiaDamianoRovere and @hafermoraes for the suggestions.
* Fix FANTASIA names on CNES, issue #125.

# microdatasus 2.3.3
* Add leading zeros to CNES TIPO_UNID processing.

# microdatasus 2.3.2
* Fix processing of `ESPEC` variable on `process_sih` function (#113).
* Use read.dbc package from Github repo.
* Update TP_UNID processing on `process_cnes` function (PR #112).

# microdatasus 2.3.1
* Import read.dbc package from CRAN.
* Remove CADGER data.
* General package adjustments for CRAN submission.
* Examples of process functions use data samples.
* Update package citation.

# microdatasus 2.3.0
* All process functions uses dplyr verbs and dtplyr for fast processing.
* All process functions returns a tibble.
* Add `track_source` argument to `fetch_datasus` function to create a variable called `source` with the file name that originated the row.
* Updated CBO (CNES), CADGER (CNES), EQUIPE (SIA), SIGTAB (SIA) reference tables.
* Sample data for health information systems.
* Data files with XZ compression.
* Internal tests restructured.
* Progress bar removed from `process_sim` due increased processing time.

# microdatasus 2.2.7
* Enhance messages of `fetch_datasus`.
* Timeout the connection check when DataSUS FTP is down or not reachable.
* If there is no Internet or the DataSUS FTP is down or not reachable, the `fetch_datasus` will return `NULL`.
* `process_sim` shows a progress bar.

# microdatasus 2.2.6
* Age correction for SINAN data.

# microdatasus 2.2.5
* Patch to correct Invalid multibyte string error on chikungunya processing. 

# microdatasus 2.2.4
* process_sia and process_sih internally uses now data.table and dtplyr

# microdatasus 2.2.3
* As the package {read.dbc} is not available on CRAN, this patch uses the Github version.
* Issue #89 points out that files from SIM-EXT older than 2006 present 7-digits variable lengths for CODMUNRES and CODMUNOCOR. Those codes are now truncated to 6-digits standard by process_sim function.
* Related to #66, #84 and #86. Some files are very big to download, especially those from SIA and SIH. A timeout argument was added to the fetch_datasus with a default of 240 seconds.
* process_sinasc pull request #91 fixes issue #90, related to CODOCUPMAE variable
* Related to #79, now the function process_sia downloads an updated version from the SIGTAB table from DataSUS when nome_proced is TRUE. 

# microdatasus 2.2.2
* process_sinasc correct old code for missing on ESCMAE

# microdatasus 2.2.1
* process_sinasc with new codes to process sex variable, avoiding missing results.
* process_cnes corrections to avoid NA introduction by coercion.
* process_sih corrects MUNIC_RES type

# microdatasus 2.2.0
* fetch_datasus function uses read.dbc function with as.is set to TRUE for better performance.
* Minor bugs corrections.

# microdatasus 2.1.3
* process_cnes uses {data.table} backend for performance

# microdatasus 2.1.2
* read.dbc is back on CRAN

# microdatasus 2.1.1
* Patch to use development version of read.dbc package.

# microdatasus 2.1.0
* Bug correction that impacted download of all UFs from monthly data health systems.

# microdatasus 2.0.6
* Updated codes of tabNaturalidade
* SIA-PA fetch files bug corrected.

# microdatasus 2.0.5
* Bug correction to download CNES-LT files.

# microdatasus 2.0.4
* Bug correction to download all UFs.

# microdatasus 2.0.3
* SINAN-Dengue bug correction.

# microdatasus 2.0.2
* SINASC bug correction (ESCMAE2010).

# microdatasus 2.0.1
* Ages in minute unit.

# microdatasus 2.0.0
* Integrated download of old, current, and preliminar data.

# microdatasus 1.4.8
* Tests correction
* Typo correcntion

# microdatasus 1.4.7
* Bug correction for download with newer R versions.

# microdatasus 1.4.6
* Bug correction at process_sia function.

# microdatasus 1.4.5
* Bug correction at process_sim function.

# microdatasus 1.4.4
* Updates SINAN functions for DataSUS changes in file structures, from per UF basis to all data (BR).

# microdatasus 1.4.3
* Correct SIH COD_IDADE value 5 for ages above 100 years.

# microdatasus 1.4.2
* Correct codmun handling for CNEST-ST data. Issue #38

# microdatasus 1.4.1
* Check local Internet connection and DataSUS FTP server availability before download.
* Argument to fetch_datasus to stop download if there is an error.
* Now is possible to download preliminar data from SIM-DO and SIM-DOFET with "SIM-DO-PRELIM" as information system at fetch_datasus.

# microdatasus 1.4.0
* Download and preprocess SINAN Malaria files.

# microdatasus 1.3.1
* Update functions documentation.

# microdatasus 1.3.0
* Download and preprocess SINAN Zika files.
* Minor error message corrections.

# microdatasus 1.2.0
* Download and preprocess SINAN Chikungunya files.

# microdatasus 1.1.4
* Minor correctiont at process_sinan_dengue function.

# microdatasus 1.1.3
* Documentation correction process_sinan_dengue function.

# microdatasus 1.1.2
* Document and export process_sinan_dengue function.

# microdatasus 1.1.1
* Fix NAT_JUR field at CNES files.

# microdatasus 1.1.0
* SINAN DENGUE files download and processing.

# microdatasus 1.0.0

* Complete overhaul of the package, meeting current R package standards.
* All functions revised.
* SIH code tables updated for COVID-19.
* Process CNES ST and PF.
* Added a `NEWS.md` file to track changes to the package.
