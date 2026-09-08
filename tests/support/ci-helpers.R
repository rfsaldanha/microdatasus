# Shared by CI scripts and offline unit tests; not part of the package API.
.ci_rotating_system <- function(systems, date = Sys.Date()) {
  if (!is.character(systems) || !length(systems) || anyNA(systems) ||
      any(!nzchar(systems)) || anyDuplicated(systems)) {
    stop("systems must contain unique, non-empty identifiers.")
  }
  if (!inherits(date, "Date") || length(date) != 1L ||
      is.na(date) || !is.finite(as.numeric(date))) {
    stop("date must be one finite, non-missing Date.")
  }
  # Count complete Monday-based weeks, without restarting in January.
  week <- floor(as.numeric(date - as.Date("1970-01-05")) / 7)
  systems[[as.integer(week %% length(systems)) + 1L]]
}
