# microdatasus: Download and prepare DataSUS microdata

Provides a reproducible workflow for DataSUS microdata:
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
discovers, downloads, and combines published DBC files;
[`read_dbc()`](https://rfsaldanha.github.io/microdatasus/reference/read_dbc.md)
reads a local DBC file directly, with structural and checksum
validation; and the `process_*()` functions interpret official TabWin
DEF/CNV/DBF dictionaries to produce period-correct values, labels, and
column types.

## Details

Start with
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
and then use the processor corresponding to the selected system, such as
[`process_sim()`](https://rfsaldanha.github.io/microdatasus/reference/process_sim.md),
[`process_sinasc()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinasc.md),
[`process_sih()`](https://rfsaldanha.github.io/microdatasus/reference/process_sih.md),
[`process_sia()`](https://rfsaldanha.github.io/microdatasus/reference/process_sia.md),
[`process_cnes()`](https://rfsaldanha.github.io/microdatasus/reference/process_cnes.md),
or
[`process_sinan()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinan.md).
[`fetch_cadger()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_cadger.md)
and
[`fetch_sigtab()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_sigtab.md)
also retrieve current auxiliary tables for standalone use.

Use
[`datasus_variables()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_variables.md)
to inspect parsed dictionary definitions and relations,
[`validate_datasus_schema()`](https://rfsaldanha.github.io/microdatasus/reference/validate_datasus_schema.md)
to compare raw DBC fields with the selected historical definitions and
processed types, and
[`processing_diagnostics()`](https://rfsaldanha.github.io/microdatasus/reference/processing_diagnostics.md)
to retrieve unknown codes, coercion failures, and dictionary provenance
from an individual processing call.

Processing automatically reuses repeated date values and vectorizes code
padding and CNV threshold lookup. For large requests,
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
can process and save one file at a time with `process = TRUE`,
`collect = FALSE`, and `destination`. Configure `microdatasus.cache_dir`
for persistent dictionary reuse by processors called directly; see
[`datasus_cache_dir()`](https://rfsaldanha.github.io/microdatasus/reference/datasus_cache_dir.md)
and the performance section of
[`process_sim()`](https://rfsaldanha.github.io/microdatasus/reference/process_sim.md).

## Network access and local data

[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
needs access to DataSUS directory listings even when DBC files are
cached. By default it warns about connection failures and retains
successful downloads; with `collect = TRUE`, check for `NULL` before
passing the result to a processor. Use `stop_on_error = TRUE` to abort
on failures. A direct call to a processor or dictionary helper can fail
if a required dictionary cannot be downloaded and no valid cached copy
is available.

[`read_dbc()`](https://rfsaldanha.github.io/microdatasus/reference/read_dbc.md)
reads local files without Internet access. Packaged reference tables,
including
[tabMun](https://rfsaldanha.github.io/microdatasus/reference/tabMun.md),
are also available offline. Saved processing results can be reopened
with [`readRDS()`](https://rdrr.io/r/base/readRDS.html). Persistent
dictionaries are reused when valid and `refresh = FALSE`; see
[`fetch_tabwin_dictionary()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_tabwin_dictionary.md).

## References

For concepts, coverage, data flows, and caveats of each Brazilian health
information system, see Saldanha (2026), [*Sistemas de Informação em
Saúde no Brasil*](https://rfsaldanha.github.io/sis/).

## See also

Useful links:

- <https://github.com/rfsaldanha/microdatasus>

- <https://rfsaldanha.github.io/microdatasus/>

- Report bugs at <https://github.com/rfsaldanha/microdatasus/issues>

## Author

**Maintainer**: Raphael Saldanha <raphael.saldanha@fiocruz.br>
([ORCID](https://orcid.org/0000-0003-0652-8466))

Authors:

- Raphael Saldanha <raphael.saldanha@fiocruz.br>
  ([ORCID](https://orcid.org/0000-0003-0652-8466))

Other contributors:

- Sidney da Silva Pereira Bissoli (DBC decompression code)
  \[contributor, copyright holder\]

- Mark Adler (blast decompressor) \[contributor, copyright holder\]
