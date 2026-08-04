# Live smoke test for one family selected by the workflow matrix.
library(microdatasus)

information_system <- Sys.getenv("MICRODATASUS_SMOKE_SYSTEM", "SIM-DO")
configs <- list(
  `SIM-DO` = list(vars = c("DTOBITO", "SEXO", "CODMUNRES"), date = "DTOBITO"),
  SINASC = list(vars = c("DTNASC", "SEXO", "CODMUNRES"), date = "DTNASC"),
  `SIH-RD` = list(vars = c("DT_INTER", "SEXO", "MUNIC_RES", "IDADE", "COD_IDADE"), date = "DT_INTER"),
  `SIA-PA` = list(vars = c("PA_CMP", "PA_SEXO", "PA_MUNPCN", "PA_IDADE"), date = NULL),
  `CNES-ST` = list(vars = c("COMPETEN", "CNES", "CODUFMUN"), date = NULL),
  `SINAN-DENGUE` = list(vars = c("DT_NOTIFIC", "CS_SEXO", "ID_MN_RESI", "NU_IDADE_N"), date = "DT_NOTIFIC")
)
if (!information_system %in% names(configs)) stop("Unknown smoke-test system.")
config <- configs[[information_system]]
metadata <- datasus_information_systems()
metadata <- metadata[metadata$information_system == information_system, ]
cache <- tempfile("microdatasus-smoke-cache-")
on.exit(unlink(cache, recursive = TRUE), add = TRUE)
args <- list(
  year_start = 2022, year_end = 2022,
  uf = if (metadata$geography[[1L]] == "national") "all" else "AC",
  information_system = information_system, vars = config$vars,
  cache_dir = cache, process = TRUE,
  process_args = list(municipality_data = FALSE, labels = "none", diagnostics = TRUE),
  provenance = TRUE, quiet = TRUE, stop_on_error = TRUE
)
if (metadata$periodicity[[1L]] == "monthly") {
  args$month_start <- 1L
  args$month_end <- 1L
}
result <- do.call(fetch_datasus, args)
stopifnot(
  inherits(result, "data.frame"), nrow(result) > 0L,
  !is.null(datasus_provenance(result)),
  !is.null(processing_diagnostics(result))
)
if (!is.null(config$date)) stopifnot(inherits(result[[config$date]], "Date"))
