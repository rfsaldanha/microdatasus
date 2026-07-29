# microdatasus: Download and prepare DataSUS microdata

Provides a reproducible workflow for DataSUS microdata:
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
discovers, downloads, and combines published DBC files;
[`read_dbc()`](https://rfsaldanha.github.io/microdatasus/reference/read_dbc.md)
reads a local DBC file; and the `process_*()` functions convert
system-specific fields into analysis-ready values and labels.

## Details

Start with
[`fetch_datasus()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_datasus.md)
and then use the processor corresponding to the selected system, such as
[`process_sim()`](https://rfsaldanha.github.io/microdatasus/reference/process_sim.md),
[`process_sinasc()`](https://rfsaldanha.github.io/microdatasus/reference/process_sinasc.md),
[`process_sih()`](https://rfsaldanha.github.io/microdatasus/reference/process_sih.md),
[`process_sia()`](https://rfsaldanha.github.io/microdatasus/reference/process_sia.md),
[`process_cnes()`](https://rfsaldanha.github.io/microdatasus/reference/process_cnes.md),
or one of the `process_sinan_*()` functions.
[`fetch_cadger()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_cadger.md)
and
[`fetch_sigtab()`](https://rfsaldanha.github.io/microdatasus/reference/fetch_sigtab.md)
retrieve current auxiliary tables used by CNES and SIA processing.

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
