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
