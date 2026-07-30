# Reproducible local/CI benchmark based only on data shipped with the package.
# It records observations instead of enforcing machine-dependent time limits.
library(microdatasus)

cases <- list(
  SIM_DO = list(
    data = sim_do_sample[rep(seq_len(nrow(sim_do_sample)), 1000L), ],
    run = function(data) process_sim(
      data,
      municipality_data = FALSE,
      labels = "none"
    )
  ),
  SINASC = list(
    data = sinasc_sample[rep(seq_len(nrow(sinasc_sample)), 1000L), ],
    run = function(data) process_sinasc(
      data,
      municipality_data = FALSE,
      labels = "none"
    )
  ),
  SIH_RD = list(
    data = sih_rd_sample[rep(seq_len(nrow(sih_rd_sample)), 1000L), ],
    run = function(data) process_sih(
      data,
      municipality_data = FALSE,
      labels = "none"
    )
  )
)

results <- do.call(rbind, lapply(names(cases), function(name) {
  case <- cases[[name]]
  timing <- system.time(output <- case$run(case$data))
  data.frame(
    case = name,
    rows = nrow(case$data),
    elapsed_seconds = unname(timing[["elapsed"]]),
    output_rows = nrow(output)
  )
}))

dir.create("benchmarks", showWarnings = FALSE)
utils::write.csv(results, "benchmarks/results.csv", row.names = FALSE)
print(results)
