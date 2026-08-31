# Usage: Rscript benchmarks/dbc-reader.R FILE.dbc [COLUMN ...]
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) == 0L) {
  stop("Supply a DBC path and, optionally, columns for projection.")
}

devtools::load_all(".", quiet = TRUE)
path <- normalizePath(arguments[[1L]], mustWork = TRUE)
vars <- if (length(arguments) > 1L) arguments[-1L] else NULL

measure <- function(operation, repetitions = 5L) {
  timings <- numeric(repetitions)
  result <- NULL
  for (index in seq_len(repetitions)) {
    gc()
    timings[[index]] <- system.time(result <- operation())[["elapsed"]]
  }
  list(
    median_seconds = unname(stats::median(timings)),
    minimum_seconds = unname(min(timings)),
    object_mb = unname(as.numeric(object.size(result)) / 1024^2)
  )
}

direct <- measure(function() read_dbc(path))
projected <- if (is.null(vars)) NULL else measure(function() {
  read_dbc(path, vars = vars)
})
legacy <- measure(function() {
  output <- tempfile(fileext = ".dbf")
  tryCatch(
    {
      microdatasus:::.dbc2dbf(path, output)
      foreign::read.dbf(output, as.is = TRUE)
    },
    finally = unlink(output)
  )
})

results <- data.frame(
  implementation = c("direct", if (!is.null(projected)) "projected", "legacy"),
  do.call(rbind, c(list(direct), if (!is.null(projected)) list(projected), list(legacy))),
  row.names = NULL,
  check.names = FALSE
)
print(results)
