# This scheduled test is deliberately separate from R CMD check because it
# verifies services controlled by DataSUS and therefore requires live network.
library(microdatasus)

cache <- tempfile("microdatasus-smoke-cache-")
on.exit(unlink(cache, recursive = TRUE), add = TRUE)

result <- fetch_datasus(
  year_start = 2022,
  year_end = 2022,
  uf = "AC",
  information_system = "SIM-DO",
  vars = c("DTOBITO", "SEXO", "CODMUNRES"),
  cache_dir = cache,
  process = TRUE,
  process_args = list(
    municipality_data = FALSE,
    diagnostics = TRUE
  ),
  provenance = TRUE,
  quiet = TRUE,
  stop_on_error = TRUE
)

stopifnot(
  inherits(result, "data.frame"),
  nrow(result) > 0L,
  inherits(result$DTOBITO, "Date"),
  !is.null(datasus_provenance(result)),
  !is.null(processing_diagnostics(result))
)
