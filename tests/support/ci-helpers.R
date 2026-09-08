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

.ci_check_coverage <- function(total, by_file, critical,
                               min_total = 92.25, min_critical = 80) {
  percentage <- function(value) {
    is.numeric(value) && length(value) == 1L && is.finite(value) &&
      value >= 0 && value <= 100
  }
  if (!percentage(total) || !percentage(min_total) || !percentage(min_critical)) {
    stop("Coverage totals and limits must be finite percentages between 0 and 100.")
  }
  if (!is.character(critical) || !length(critical) || anyNA(critical) ||
      any(!nzchar(critical)) || anyDuplicated(critical)) {
    stop("Critical coverage files must be unique, non-empty paths.")
  }
  if (!is.numeric(by_file) || is.null(names(by_file)) ||
      anyNA(names(by_file)) || any(!nzchar(names(by_file))) ||
      anyDuplicated(names(by_file))) {
    stop("Per-file coverage must be numeric with unique, non-empty file names.")
  }
  missing <- setdiff(critical, names(by_file))
  if (length(missing)) {
    stop("Critical files missing from coverage report: ", paste(missing, collapse = ", "))
  }
  values <- by_file[critical]
  invalid <- !is.finite(values) | values < 0 | values > 100
  if (any(invalid)) {
    stop("Invalid coverage for critical files: ", paste(critical[invalid], collapse = ", "))
  }
  if (total < min_total) stop("Total test coverage fell below ", min_total, "%.")
  below <- values < min_critical
  if (any(below)) {
    stop("Coverage below ", min_critical, "% for critical files: ",
         paste(critical[below], collapse = ", "))
  }
  invisible(TRUE)
}
