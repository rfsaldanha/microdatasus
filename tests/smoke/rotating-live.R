# Rotating live smoke coverage for every registered system without changing
# the registry. One member of each family is selected weekly.
library(microdatasus)

metadata <- datasus_information_systems()
registry <- microdatasus:::.datasus_registry()
metadata$family <- sub("-.*$", "", metadata$information_system)
families <- unique(metadata$family)
week <- as.integer(format(Sys.Date(), "%V"))
cache <- Sys.getenv("MICRODATASUS_SMOKE_CACHE", unset = "")
if (!nzchar(cache)) cache <- tempfile("microdatasus-rotating-cache-")
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
dir.create("smoke-results", showWarnings = FALSE)
results <- list()

for (family in families) {
  candidates <- metadata$information_system[metadata$family == family]
  information_system <- candidates[((week - 1L) %% length(candidates)) + 1L]
  spec <- registry[[information_system]]
  minimum_year <- as.integer(format(spec$minimum, "%Y"))
  years <- seq.int(minimum_year, as.integer(format(Sys.Date(), "%Y")))
  periods <- if (identical(spec$granularity, "month")) {
    format(seq(
      as.Date(sprintf("%04d-01-01", minimum_year)),
      as.Date(format(Sys.Date(), "%Y-%m-01")),
      by = "month"
    ), "%y%m")
  } else {
    as.character(years)
  }
  ufs <- if (identical(spec$geography, "state")) "AC" else "all"
  discovery <- microdatasus:::.datasus_build_manifest(
    spec, periods,
    if (identical(ufs, "all")) microdatasus:::.datasus_ufs else ufs,
    timeout = 240
  )
  manifest <- discovery$manifest
  if (!nrow(manifest)) {
    stop("No published file found for rotating smoke system ", information_system)
  }
  remote <- manifest[[nrow(manifest), "period"]]
  year <- as.integer(if (identical(spec$granularity, "month")) {
    value <- as.integer(substr(remote, 1L, 2L))
    if (value >= 90L) 1900L + value else 2000L + value
  } else {
    remote
  })
  args <- list(
    year_start = year, year_end = year, uf = ufs,
    information_system = information_system,
    cache_dir = cache, process = TRUE,
    process_args = list(
      municipality_data = FALSE, labels = "none", diagnostics = TRUE
    ),
    provenance = TRUE, quiet = TRUE, stop_on_error = TRUE,
    row_filter = function(data) seq_len(nrow(data)) <= 500L
  )
  if (identical(spec$granularity, "month")) {
    month <- as.integer(substr(remote, 3L, 4L))
    args$month_start <- month
    args$month_end <- month
  }
  value <- do.call(fetch_datasus, args)
  stopifnot(
    inherits(value, "data.frame"), nrow(value) > 0L,
    !is.null(datasus_provenance(value)),
    !is.null(processing_diagnostics(value))
  )
  lock <- datasus_lockfile(value)
  stopifnot(
    inherits(lock, "microdatasus_lockfile"),
    nrow(lock$files) >= 1L,
    all(lock$files$checksum_algorithm == "sha256")
  )
  results[[family]] <- data.frame(
    family = family, information_system = information_system,
    period = remote, rows = nrow(value),
    dictionary_count = nrow(lock$dictionaries),
    stringsAsFactors = FALSE
  )
}
utils::write.csv(
  do.call(rbind, results),
  "smoke-results/rotating-systems.csv",
  row.names = FALSE
)
