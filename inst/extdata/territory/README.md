# Source of the packaged municipality reference

Version: `datasus-territorio-2023-txt-20220516`, policy `legacy-compatible-v1`.

Original archive:
`ftp://ftp.datasus.gov.br/territorio/tabelas/2023/base_territorial_2023.zip`

Original archive SHA-256:
`798be2f62a53dd1af8e335a44a1916154f36ee3a3051a7375864a74ef47c3bc4`

`tabmun-source.zip` contains only the unmodified `tb_municip.txt`,
`tb_municip_layout.txt`, `tb_uf.txt` and `tb_uf_layout.txt` members. Their
individual checksums are pinned in the reconstruction helper. These TXT files
are dated 2022-05-16 in the 2023 ZIP. This is a member modification date, **not**
a claim that every territorial attribute describes 2022 or 2023. The package's
original extraction date remains unknown; this is an independently verified
source from which its existing table can now be reconstructed.

The official field documentation is
`ftp://ftp.datasus.gov.br/territorio/doc/bases_territoriais.pdf` (retrieved copy
SHA-256 `98a2c740720bf6fb5be6db01d8c80452de9d8735058cca675714ea34cf5f2e34`).
The table includes municipalities and special, extinct, transferred and unknown
territorial codes; its 5,659 rows are not a count of currently active municipalities.

## Transformation and compatibility

- Decode the fixed-width TXT files as ISO-8859-1 using their supplied layouts.
- Join the UF name through `CO_UF`. Rename the selected columns to the established
  nine `munRes*` names, retain status/type factor levels, and order by code.
- Parse decimal commas explicitly. Retain integer municipality codes and
  altitudes to preserve the existing exported object interface.
- Replace the four zero geographical placeholders with `NA` for the 27 unknown
  units and former federal territory `200010`; do not erase genuine zero values
  from other records.
- Preserve two legacy UF-display conventions: empty text for unknown code `0`
  and the successor state `Pernambuco` for former territory `200010`. The latter
  is a compatibility label, not its historical status; status `TRANSF` and type
  `TERRIT` remain explicit. The official UF name is retained in the source TXT.

These steps reproduce all values, types and row order of the existing `tabMun`.
No data from a newer CSV/DBF have been mixed into the TXT snapshot. In particular,
the 2023 ZIP's other formats are not interchangeable versions of these TXT files.

All processors use this fixed snapshot when `municipality_data = TRUE`; they do
not select a territorial edition from the observation date or harmonize historical
boundaries. Use `municipality_data = FALSE` when applying another territorial
reference. Processing diagnostics and lockfiles identify this exact version.

From a repository checkout, rebuild offline with `Rscript data-raw/tabMun.R`.
To reconstruct the source-only ZIP as well, supply the original archive path;
its checksum is verified before extraction. Never infer or silently update a
publication date or source pin when an upstream archive changes.
