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
  quiet = FALSE
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

  `NULL`, or a character vector of column names to retain. Selection is
  applied to each file before the files are combined.

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

## Value

A tibble containing all successfully read records, or `NULL` if no
requested file could be read. No diagnostic attributes are added.

## Details

The function first lists the relevant DataSUS directories and downloads
only files present in those listings. When more than one publication
represents the same system, period, state, and file part,
definitive/current data take precedence over preliminary data, and
current data take precedence over historical copies.

Downloads are sequential. Unless `quiet = TRUE`, transfer progress is
displayed by
[`curl::curl_download()`](https://jeroen.r-universe.dev/curl/reference/curl_download.html).
Transient network failures are retried up to two times; missing, empty,
invalid DBC, and incompatible-schema files are not retried. Partial
files and other temporary files are removed before the function returns
or aborts.

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

- **SINAN:** `"SINAN-DENGUE"`, `"SINAN-CHIKUNGUNYA"`, `"SINAN-ZIKA"`,
  `"SINAN-MALARIA"`, `"SINAN-CHAGAS"`, `"SINAN-LEISHMANIOSE-VISCERAL"`,
  `"SINAN-LEISHMANIOSE-TEGUMENTAR"`, and `"SINAN-LEPTOSPIROSE"`.

## Network access

An Internet connection and FTP access to DataSUS are required. DataSUS
may restrict FTP access from some countries.

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
[`process_sim()`](https://rfsaldanha.github.io/microdatasus/reference/process_sim.md),
[`process_sinasc()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinasc.md),
[`process_sih()`](https://rfsaldanha.github.io/microdatasus/reference/process_sih.md),
[`process_sia()`](https://rfsaldanha.github.io/microdatasus/reference/process_sia.md),
[`process_cnes()`](https://rfsaldanha.github.io/microdatasus/reference/process_cnes.md),
and the `process_sinan_*()` functions for system-specific recoding.

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
