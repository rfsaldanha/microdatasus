# Fetch and read microdata files from DataSUS

`fetch_datasus` downloads microdata (DBC) files from DataSUS and reads
them.

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
  track_source = FALSE
)
```

## Arguments

- year_start, year_end:

  numeric. Start and end year of files in the format yyyy.

- month_start, month_end:

  numeric. Start and end month in the format mm. Those parameters are
  only used with the healh information systems SIH, CNES and SIA. There
  parameter are ignored if the information health system is SIM or
  SINASC.

- uf:

  an optional string or a vector of strings. By default all UFs
  ("Unidades Federativas") are download. See *Details*.

- information_system:

  string. The abbreviation of the health information system to be
  accessed. See *Details*.

- vars:

  an optional string or a vector of strings. By default, all variables
  read and stored, unless a list of desired variables is informed by
  this parameter.

- stop_on_error:

  logical. If TRUE, the download process will be stopped if an error
  occurs.

- timeout:

  numeric (seconds). Sets a timeout tolerance for downloads, usefull on
  large files and/or slow connections. Defaults to 240 seconds.

- track_source:

  logical. If `TRUE`, adds a column called `source` with the downloaded
  file name.

## Value

a `data.frame` with the contents of the DBC files.

## Details

This function downloads DBC files from DataSUS following parameters
about start date, end date, UF and health information system
abbreviation. After the download process, the files are merged into a
unique `data.frame` object.

A specific UF or a vector of UFs can be informed using the following
abbreviations: "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
"MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS",
"RO", "RR", "SC", "SP", "SE", "TO".

The following systems are implemented: "SIH-RD", "SIH-RJ", "SIH-SP",
"SIH-ER", "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT",
"SINASC", "CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR",
"CNES-HB", "CNES-PF", "CNES-EP", "CNES-RC", "CNES-IN", "CNES-EE",
"CNES-EF", "CNES-GM", "SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD",
"SIA-AN", "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", "SIA-PS",
"SIA-SAD", "SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SINAN-ZIKA",
"SINAN-MALARIA", "SINAN-CHAGAS", "SINAN-LEISHMANIOSE-VISCERAL",
"SINAN-LEISHMANIOSE-TEGUMENTAR", "SINAN-LEPTOSPIROSE".

## Warning

A Internet connection is needed to use this function.

Currently, DataSUS FTP server is restricting download calls from some
countries, except Brazil.

The year and month used to download the files regards the processing
month and year of the cases by DataSUS.

The UF regards where the cases were processed by DataSUS.

The files are downloaded to a temporary folder and deleted after the
reading process.

## Examples

``` r
# \donttest{
# Fetch two years of data from SIM-DO
res <- fetch_datasus(year_start = 2010, year_end = 2011, uf = "AC",
                     information_system = "SIM-DO")
#> ℹ Your local Internet connection seems to be ok.
#> ℹ DataSUS FTP server seems to be up and reachable.
#> ℹ Starting download...

# Fetch one year of data from SIM-DO and keep only three variables
res <- fetch_datasus(year_start = 2014, year_end = 2014,
                     information_system = "SIM-DO", uf = "AC",
                     vars = c("CODMUNRES", "DTOBITO", "CAUSABAS"))
#> ℹ Your local Internet connection seems to be ok.
#> ℹ DataSUS FTP server seems to be up and reachable.
#> ℹ Starting download...

# Fetch some months' data from SIH-RD for four states
res <- fetch_datasus(year_start = 2014, month_start = 1,
                     year_end = 2014, month_end = 2,
                     uf = c("AC", "RR"),
                     information_system = "SIH-RD")
#> ℹ Your local Internet connection seems to be ok.
#> ℹ DataSUS FTP server seems to be up and reachable.
#> ℹ Starting download...
# }
```
