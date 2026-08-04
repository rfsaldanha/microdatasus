# Dictionary-derived contracts and scheduled audit helpers.

.datasus_validate_system_selection <- function(information_system) {
  registry <- .tabwin_registry()
  if (is.null(information_system)) return(names(registry))
  if (!is.character(information_system) || anyNA(information_system) ||
      any(!nzchar(information_system))) {
    cli::cli_abort("{.arg information_system} must be NULL or a character vector.")
  }
  resolved <- vapply(information_system, .sinan_resolve_information_system,
                     character(1))
  invalid <- setdiff(resolved, names(registry))
  if (length(invalid)) {
    cli::cli_abort("Unsupported dictionary keys: {.val {invalid}}.")
  }
  unique(resolved)
}

# Return the most severe relation state without discarding mixed outcomes.
.datasus_worst_dictionary_status <- function(status) {
  rank <- c(ok = 1L, fallback = 2L, not_requested = 3L, non_enumerable = 4L,
            missing = 5L, invalid = 6L, error = 7L, dictionary_error = 8L)
  status[[which.max(replace(unname(rank[status]), is.na(rank[status]), 0L))]]
}

#' Build a dictionary-derived DataSUS schema contract
#'
#' Summarises all fields declared by one official TabWin DEF. The contract
#' describes dictionary roles and relations; fields absent from the DEF, such as
#' free text, remain discoverable only in the corresponding DBC layout.
#'
#' @param information_system One dictionary key accepted by
#'   [fetch_tabwin_dictionary()].
#' @param inspect Logical scalar. If `TRUE`, parse relations and include their
#'   status; otherwise build the contract from DEF metadata only.
#' @inheritParams fetch_tabwin_dictionary
#' @return A tibble with one row per field and list-columns containing every
#'   description, relation, file, command, and status declared for that field.
#' @export
datasus_schema <- function(
  information_system,
  inspect = FALSE,
  timeout = 240,
  refresh = FALSE,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL)
) {
  .datasus_assert_flag(inspect, "inspect")
  keys <- .datasus_validate_system_selection(information_system)
  if (length(keys) != 1L) {
    cli::cli_abort("{.arg information_system} must identify exactly one dictionary.")
  }
  variables <- datasus_variables(
    keys[[1L]], include_labels = inspect, timeout = timeout, refresh = refresh,
    quiet = quiet, cache_dir = cache_dir
  )
  groups <- split(variables, variables$field)
  tibble::tibble(
    information_system = keys[[1L]],
    archive_checksum = variables$archive_checksum[[1L]],
    definition = variables$definition[[1L]],
    field = names(groups),
    dictionary_type = vapply(groups, function(x) x$type[[1L]], character(1)),
    definitions = vapply(groups, nrow, integer(1)),
    descriptions = lapply(groups, function(x) unique(stats::na.omit(x$description))),
    relations = lapply(groups, function(x) unique(stats::na.omit(x$relation))),
    files = lapply(groups, function(x) unique(stats::na.omit(x$file))),
    commands = lapply(groups, function(x) unique(stats::na.omit(x$command))),
    status = vapply(groups, function(x) .datasus_worst_dictionary_status(x$status),
                    character(1))
  )
}

#' Audit all supported DataSUS TabWin dictionaries
#'
#' Downloads each physical archive only once per cache and inspects every
#' selected DEF/CNV/DBF relation. With a NULL selection, all current and
#' historical keys are audited (104 keys over 14 physical archives).
#'
#' @param information_system NULL for every dictionary, or selected keys.
#' @param fail_on_error Logical scalar. If `TRUE`, abort after the audit when a
#'   dictionary download or unexpected parser/I/O error occurs. Known missing
#'   or invalid upstream relations remain represented in the result.
#' @param fail_on_issues Logical scalar. If `TRUE`, also abort when official
#'   relations are missing or invalid. Fallbacks and symbolic analytical ranges
#'   remain non-fatal.
#' @inheritParams fetch_tabwin_dictionary
#' @return A tibble with one row per dictionary and an `issues` list-column.
#' @export
audit_datasus_dictionaries <- function(
  information_system = NULL,
  timeout = 240,
  refresh = FALSE,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL),
  fail_on_error = FALSE,
  fail_on_issues = FALSE
) {
  .datasus_assert_flag(refresh, "refresh")
  .datasus_assert_flag(quiet, "quiet")
  .datasus_assert_flag(fail_on_error, "fail_on_error")
  .datasus_assert_flag(fail_on_issues, "fail_on_issues")
  keys <- .datasus_validate_system_selection(information_system)
  registry <- .tabwin_registry()
  refreshed_archives <- character()
  rows <- lapply(seq_along(keys), function(index) {
    key <- keys[[index]]
    if (!quiet) cli::cli_alert_info(
      "Auditing [{index}/{length(keys)}] {.val {key}}..."
    )
    started <- proc.time()[["elapsed"]]
    archive_key <- registry[[key]]$archive_key
    refresh_key <- refresh && !archive_key %in% refreshed_archives
    if (refresh_key) refreshed_archives <<- c(refreshed_archives, archive_key)
    value <- tryCatch(
      datasus_variables(
        key, include_labels = TRUE, timeout = timeout, refresh = refresh_key,
        quiet = TRUE, cache_dir = cache_dir
      ),
      error = identity
    )
    elapsed <- proc.time()[["elapsed"]] - started
    if (inherits(value, "error")) {
      issues <- tibble::tibble(
        field = NA_character_, file = NA_character_, status = "dictionary_error",
        issue_class = "dictionary_access_or_parser",
        message = conditionMessage(value)
      )
      return(tibble::tibble(
        information_system = key, archive_key = registry[[key]]$archive_key,
        definition = registry[[key]]$definition, archive_checksum = NA_character_,
        fields = NA_integer_, relations = NA_integer_, range_rules = NA_integer_,
        status = "dictionary_error", elapsed_seconds = elapsed, issues = list(issues)
      ))
    }
    issues <- value[value$status %in% c(
      "fallback", "non_enumerable", "missing", "invalid", "error"
    ), c("field", "file", "status", "issue_class", "message")]
    tibble::tibble(
      information_system = key, archive_key = registry[[key]]$archive_key,
      definition = value$definition[[1L]],
      archive_checksum = value$archive_checksum[[1L]],
      fields = length(unique(value$field)), relations = nrow(value),
      range_rules = sum(value$range_rules),
      status = if (nrow(issues)) .datasus_worst_dictionary_status(issues$status) else "ok",
      elapsed_seconds = elapsed, issues = list(tibble::as_tibble(issues))
    )
  })
  result <- do.call(rbind, rows)
  failed <- result$status %in% c("dictionary_error", "error")
  issue_failed <- result$status %in% c(
    "dictionary_error", "error", "missing", "invalid"
  )
  if (fail_on_error && any(failed)) {
    cli::cli_abort("One or more DataSUS dictionaries could not be audited.")
  }
  if (fail_on_issues && any(issue_failed)) {
    cli::cli_abort(
      "One or more DataSUS dictionaries contain missing or invalid relations."
    )
  }
  result
}
