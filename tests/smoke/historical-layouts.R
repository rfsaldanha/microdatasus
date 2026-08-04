# Monthly live checks at every processor-definition transition.
library(microdatasus)

cases <- data.frame(
  information_system = c(
    "SINASC", "SINASC",
    rep("SIH-RD", 6L), rep("SIA-PA", 6L), rep("CNES-SR", 2L)
  ),
  year = c(
    1995, 1996,
    1997, 1998, 2003, 2003, 2007, 2008,
    1999, 1999, 2003, 2003, 2007, 2008,
    2008, 2008
  ),
  month = c(
    NA, NA,
    12, 1, 7, 8, 12, 1,
    10, 11, 7, 8, 12, 1,
    2, 3
  ),
  expected_dictionary = c(
    "SINASC-1994-1995", "SINASC",
    "SIH-RD-1992-1997", "SIH-RD-1998-2003-07",
    "SIH-RD-1998-2003-07", "SIH-RD-2003-08-2007",
    "SIH-RD-2003-08-2007", "SIH-RD",
    "SIA-PA-1994-07-1999-10", "SIA-PA-1999-11-2003-07",
    "SIA-PA-1999-11-2003-07", "SIA-PA-2003-08-2007",
    "SIA-PA-2003-08-2007", "SIA-PA",
    "CNES-SR-2005-08-2008-02", "CNES-SR"
  ),
  stringsAsFactors = FALSE
)

cache <- Sys.getenv("MICRODATASUS_HISTORICAL_CACHE", unset = "")
if (!nzchar(cache)) cache <- tempfile("microdatasus-historical-cache-")
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
dir.create("historical-results", showWarnings = FALSE)
results <- list()

for (index in seq_len(nrow(cases))) {
  case <- cases[index, ]
  args <- list(
    year_start = case$year, year_end = case$year, uf = "AC",
    information_system = case$information_system,
    cache_dir = cache, process = TRUE,
    process_args = list(
      municipality_data = FALSE, labels = "none", diagnostics = TRUE
    ),
    provenance = TRUE, quiet = TRUE, stop_on_error = TRUE,
    row_filter = function(data) seq_len(nrow(data)) <= 50L
  )
  if (!is.na(case$month)) {
    args$month_start <- case$month
    args$month_end <- case$month
  }
  value <- do.call(fetch_datasus, args)
  stopifnot(inherits(value, "data.frame"), nrow(value) > 0L)
  report <- processing_diagnostics(value)
  dictionaries <- unique(unlist(lapply(
    report$files,
    function(file) file$dictionaries$information_system
  )))
  if (!case$expected_dictionary %in% dictionaries) {
    stop(
      case$information_system, " ", case$year, "-", case$month,
      " selected ", paste(dictionaries, collapse = ", "),
      " instead of ", case$expected_dictionary
    )
  }
  results[[index]] <- data.frame(
    case, rows = nrow(value),
    checksum_algorithm = datasus_provenance(value)$checksum_algorithm[[1L]]
  )
}
utils::write.csv(
  do.call(rbind, results),
  "historical-results/layout-transitions.csv",
  row.names = FALSE
)
