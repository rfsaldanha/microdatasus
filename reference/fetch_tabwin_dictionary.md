# Download a TabWin data dictionary

Downloads and parses official TabWin definition archives published by
DataSUS. Archive files, DEF metadata, and conversion tables used during
processing are cached in memory and optionally persisted across R
sessions. SIM support is limited to CID-10 files; SINASC supports both
its 1994-1995 and current layouts. SIH supports its current and
historical RD/RJ definitions; SIA supports all twelve current layouts
plus the three historical PA definitions; and CNES supports all thirteen
layouts plus both service-classification periods. SINAN supports all 58
transfer-page file families.

## Usage

``` r
fetch_tabwin_dictionary(
  information_system = "SIM-DO",
  timeout = 240,
  refresh = FALSE,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL)
)
```

## Arguments

- information_system:

  Information system whose dictionary should be downloaded. Supported
  values include the five SIM mortality types, `"SINASC"` for files from
  1996 onward, `"SINASC-1994-1995"` for the original SINASC layout, and
  `"SIH-RD"`, `"SIH-RJ"`, `"SIH-SP"`, and `"SIH-ER"` and the twelve
  `"SIA-*"` file families. Historical SIH and SIA-PA keys are selected
  internally by their processing functions. All thirteen `"CNES-*"`
  families are also supported; the historical CNES-SR key is selected
  internally by
  [`process_cnes()`](https://rfsaldanha.github.io/microdatasus/reference/process_cnes.md).
  The 58 readable SINAN identifiers and their aliases, listed by
  [`datasus_information_systems()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_information_systems.md)
  under `system == "SINAN"`, are also accepted here.

- timeout:

  A positive numeric scalar. Download and connection timeout, in
  seconds.

- refresh:

  Logical scalar. If `TRUE`, discard the session cache and download the
  archive again.

- quiet:

  Logical scalar. If `TRUE`, suppress download progress and status
  messages.

- cache_dir:

  Optional persistent cache root. The package option
  `microdatasus.cache_dir` is used by default; `NULL` uses only the
  current session cache.

## Value

An object of class `microdatasus_tabwin_dictionary`. Its `definitions`
element describes the conversions found in the official DEF file.

## Network access

A call without a valid cached archive downloads the relevant TabWin ZIP
from DataSUS. Systems that share an archive reuse one copy. With
`cache_dir`, the ZIP, its checksum, and manifest persist across
sessions. A valid cached copy can be used without downloading again when
`refresh = FALSE`. `refresh = TRUE` requests a new download. If a
download is required and all configured transports fail, the function
raises an informative error. The `stop_on_error` argument belongs to
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
and does not apply to a direct call to this function or to a processor.

## DEF, CNV, and related DBF semantics

Active DEF declarations identify source fields, commands, substring
positions, and CNV or DBF relations. CNV files are parsed as fixed-width
definitions, including compact and long-description dialects, literal
and numeric ranges, continuation rows, and inline comments. Large
analytical ranges remain symbolic instead of being expanded into
millions of labels.

When a CNV code, CNV category description, or related DBF key is
repeated, the last applicable physical definition is used, following
TabWin precedence. Conflicting DBF keys and declared-versus-observed CNV
category counts remain visible as auditable fallbacks through
[`datasus_variables()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_variables.md)
and
[`audit_datasus_dictionaries()`](https://rfsaldanha.github.io/microdatasus/reference/audit_datasus_dictionaries.md).
Known defects in official archives are recovered only by exact,
file-scoped rules; ambiguous files or relations produce explicit
statuses or errors rather than guessed labels.

## See also

[`datasus_information_systems()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_information_systems.md),
[`process_sim()`](https://rfsaldanha.github.io/microdatasus/reference/process_sim.md),
[`process_sinasc()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinasc.md),
[`process_sih()`](https://rfsaldanha.github.io/microdatasus/reference/process_sih.md),
[`process_sia()`](https://rfsaldanha.github.io/microdatasus/reference/process_sia.md),
[`process_cnes()`](https://rfsaldanha.github.io/microdatasus/reference/process_cnes.md),
[`process_sinan()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinan.md),
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)

## Examples

``` r
if (FALSE) { # interactive() && curl::has_internet()
dictionary <- fetch_tabwin_dictionary("SIM-DO")
dictionary$definitions
sinasc_dictionary <- fetch_tabwin_dictionary("SINASC")
sih_dictionary <- fetch_tabwin_dictionary("SIH-RD")
sia_dictionary <- fetch_tabwin_dictionary("SIA-PA")
cnes_dictionary <- fetch_tabwin_dictionary("CNES-ST")
sinan_dictionary <- fetch_tabwin_dictionary("SINAN-DENGUE")
}
```
