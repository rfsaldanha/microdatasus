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
