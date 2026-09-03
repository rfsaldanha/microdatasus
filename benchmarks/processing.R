# Reproducible local/CI benchmarks using only data shipped with the package.
library(microdatasus)

# CNES and SINAN consult a DEF even with labels disabled to infer field roles.
# Empty cached dictionaries keep this benchmark network-independent.
empty_definitions <- data.frame(
  order = integer(), command = character(), description = character(),
  field = character(), argument = character(), position = integer(),
  file = character(), extension = character(), stringsAsFactors = FALSE
)
registry <- microdatasus:::.tabwin_registry()
for (key in names(registry)) {
  dictionary <- structure(list(
    information_system = key, archive_key = registry[[key]]$archive_key,
    definition = registry[[key]]$definition, source = registry[[key]]$url,
    archive_checksum = NA_character_, definitions = empty_definitions,
    numeric_fields = character(), conversions = new.env(parent = emptyenv())
  ), class = "microdatasus_tabwin_dictionary")
  assign(paste(key, "session", sep = "::"), dictionary,
         envir = microdatasus:::.tabwin_cache)
}

expand <- function(data) data[rep(seq_len(nrow(data)), 1000L), , drop = FALSE]
cases <- list(
  SIM_DO = list(data = expand(sim_do_sample), run = function(data) process_sim(data, FALSE, labels = "none")),
  SINASC = list(data = expand(sinasc_sample), run = function(data) process_sinasc(data, FALSE, labels = "none")),
  SIH_RD = list(data = expand(sih_rd_sample), run = function(data) process_sih(data, municipality_data = FALSE, labels = "none")),
  SIA_PA = list(data = expand(sia_pa_sample), run = function(data) process_sia(data, municipality_data = FALSE, labels = "none")),
  CNES_ST = list(data = expand(cnes_st_sample), run = function(data) process_cnes(data, "CNES-ST", municipality_data = FALSE, labels = "none")),
  SINAN_DENGUE = list(data = expand(sinan_dengue_sample), run = function(data) process_sinan(data, "SINAN-DENGUE", FALSE, labels = "none"))
)

# Exercise the common conversion engine separately in both public label modes.
codes <- rep(c("1", "2", "9"), length.out = 1000000L)
conversion <- structure(list(
  type = "cnv", code_width = 1L, category_count = 2L,
  map = c(`1` = "One", `2` = "Two"),
  map_priority = c(`1` = 1L, `2` = 2L),
  ranges = microdatasus:::.tabwin_empty_ranges()
), class = "microdatasus_tabwin_conversion")
selected <- list(definition = data.frame(position = 1L), conversion = conversion)
cases$LABEL_CHARACTER <- list(data = codes, run = function(data) microdatasus:::.tabwin_apply_conversion_values(data, selected))
cases$LABEL_FACTOR <- list(data = codes, run = function(data) factor(microdatasus:::.tabwin_apply_conversion_values(data, selected)))

# Guard the optimized low-cardinality paths that dominate large processors.
thresholds <- data.frame(
  upper = c(0, 7, 9999),
  label = c("Zero", "One to seven", "Eight or more"),
  priority = 1:3,
  stringsAsFactors = FALSE
)
threshold_conversion <- structure(list(
  type = "cnv", mode = "F", code_width = 5L, category_count = 3L,
  map = stats::setNames(character(), character()),
  map_priority = stats::setNames(integer(), character()),
  ranges = microdatasus:::.tabwin_empty_ranges(), thresholds = thresholds
), class = "microdatasus_tabwin_conversion")
threshold_selected <- list(
  definition = data.frame(position = 1L),
  conversion = threshold_conversion
)
threshold_codes <- rep(
  c("0", "1", "7", "8", "9999", "10000"),
  length.out = 1000000L
)
cases$LABEL_THRESHOLD <- list(
  data = threshold_codes,
  run = function(data) microdatasus:::.tabwin_apply_conversion_values(
    data, threshold_selected
  )
)
date_values <- rep(
  c("01012020", "29022020", "31122020", "00000000", "invalid"),
  length.out = 1000000L
)
cases$DATE_DMY <- list(
  data = date_values,
  run = function(data) microdatasus:::.process_as_date(data)
)
text_values <- rep(
  c("São Paulo", "texto simples", "\\u00e1rvore", NA_character_),
  length.out = 1000000L
)
cases$TEXT_NORMALIZE <- list(
  data = text_values,
  run = function(data) microdatasus:::.process_normalize_character(data)
)

results <- do.call(rbind, lapply(names(cases), function(name) {
  case <- cases[[name]]
  gc()
  timing <- system.time(output <- case$run(case$data))
  data.frame(
    case = name, rows = if (is.data.frame(case$data)) nrow(case$data) else length(case$data),
    elapsed_seconds = unname(timing[["elapsed"]]),
    output_rows = if (is.data.frame(output)) nrow(output) else length(output)
  )
}))
dir.create("benchmarks", showWarnings = FALSE)
utils::write.csv(results, "benchmarks/results.csv", row.names = FALSE)
print(results)
# Per-case budgets are intentionally several times slower than the recorded
# local baseline, catching large regressions without comparing different CPUs.
budgets <- utils::read.csv("benchmarks/budgets.csv", stringsAsFactors = FALSE)
budget_index <- match(results$case, budgets$case)
if (anyNA(budget_index)) stop("A benchmark case has no performance budget.")
exceeded <- results$elapsed_seconds > budgets$max_seconds[budget_index]
if (any(exceeded)) {
  stop(paste(
    "Performance budget exceeded:", paste(results$case[exceeded], collapse = ", ")
  ))
}

limit <- suppressWarnings(as.numeric(Sys.getenv("MICRODATASUS_BENCHMARK_MAX_SECONDS", "Inf")))
if (!is.na(limit) && any(results$elapsed_seconds > limit)) {
  stop("At least one benchmark exceeded the configured performance budget.")
}
