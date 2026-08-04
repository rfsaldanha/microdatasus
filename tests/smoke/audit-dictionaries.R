# Scheduled full audit kept outside R CMD check because DataSUS is external.
library(microdatasus)

cache <- Sys.getenv("MICRODATASUS_AUDIT_CACHE", unset = "")
if (!nzchar(cache)) cache <- tempfile("microdatasus-audit-cache-")
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
dir.create("audit-results", showWarnings = FALSE)

audit <- audit_datasus_dictionaries(
  cache_dir = cache, refresh = TRUE, quiet = FALSE, fail_on_error = FALSE
)
saveRDS(audit, "audit-results/dictionary-audit.rds", version = 2)
summary <- audit[setdiff(names(audit), "issues")]
utils::write.csv(summary, "audit-results/dictionary-audit.csv", row.names = FALSE)
issue_rows <- lapply(seq_len(nrow(audit)), function(index) {
  issues <- audit$issues[[index]]
  if (!nrow(issues)) return(NULL)
  data.frame(information_system = audit$information_system[[index]], issues,
             stringsAsFactors = FALSE)
})
issue_rows <- Filter(Negate(is.null), issue_rows)
issues <- if (length(issue_rows)) do.call(rbind, issue_rows) else data.frame()
utils::write.csv(issues, "audit-results/dictionary-issues.csv", row.names = FALSE)
baseline_dir <- Sys.getenv("MICRODATASUS_AUDIT_BASELINE", unset = "")
current <- summary[, c("information_system", "archive_checksum", "status")]
if (nzchar(baseline_dir)) {
  dir.create(baseline_dir, recursive = TRUE, showWarnings = FALSE)
  baseline_file <- file.path(baseline_dir, "dictionary-audit.csv")
  changes <- data.frame()
  if (file.exists(baseline_file)) {
    previous <- utils::read.csv(baseline_file, stringsAsFactors = FALSE)
    comparison <- merge(
      previous, current, by = "information_system", all = TRUE,
      suffixes = c("_previous", "_current")
    )
    changed <- with(comparison,
      is.na(status_previous) | is.na(status_current) |
      status_previous != status_current |
      archive_checksum_previous != archive_checksum_current
    )
    changed[is.na(changed)] <- TRUE
    changes <- comparison[changed, , drop = FALSE]
  }
  utils::write.csv(
    changes, "audit-results/dictionary-changes.csv", row.names = FALSE
  )
  utils::write.csv(current, baseline_file, row.names = FALSE)
}
print(summary)
failed <- audit$status %in% c("dictionary_error", "error")
if (any(failed)) {
  stop("At least one official DataSUS dictionary or relation failed audit.")
}
