# Flatten dictionary and packaged-reference provenance from either one processor
# report or the per-file report attached by fetch_datasus().
.datasus_lockfile_diagnostics <- function(report, component) {
  if (is.null(report)) return(data.frame())
  direct <- report[[component]]
  nested <- report$files
  values <- list()
  if (is.data.frame(direct) && nrow(direct)) values[[1L]] <- direct
  if (is.list(nested) && length(nested)) {
    values <- c(values, lapply(
      nested, .datasus_lockfile_diagnostics, component = component
    ))
  }
  values <- Filter(
    function(value) is.data.frame(value) && nrow(value),
    values
  )
  if (!length(values)) return(data.frame())
  unique(as.data.frame(data.table::rbindlist(
    values, use.names = TRUE, fill = TRUE
  )))
}

.datasus_lockfile_value <- function(lockfile) {
  if (is.character(lockfile) && length(lockfile) == 1L && !is.na(lockfile)) {
    if (!file.exists(lockfile)) {
      cli::cli_abort("Lockfile does not exist: {.path {lockfile}}.")
    }
    lockfile <- tryCatch(
      readRDS(lockfile),
      error = function(error) {
        cli::cli_abort(c(
          "Could not read the DataSUS lockfile.",
          "i" = conditionMessage(error)
        ))
      }
    )
  }
  if (!inherits(lockfile, "microdatasus_lockfile")) {
    cli::cli_abort(
      "{.arg lockfile} must be a microdatasus lockfile or its path."
    )
  }
  lockfile
}

# Write through a sibling temporary file so an interrupted process cannot leave
# a valid-looking, truncated reproducibility record.
.datasus_write_lockfile <- function(lockfile, file) {
  if (!is.character(file) || length(file) != 1L ||
      is.na(file) || !nzchar(file)) {
    cli::cli_abort("{.arg file} must be one non-empty path.")
  }
  directory <- dirname(file)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE)) {
    cli::cli_abort(
      "Could not create lockfile directory {.path {directory}}."
    )
  }
  temporary <- .datasus_temporary_path(file)
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(lockfile, temporary, version = 2)
  .datasus_commit_file(temporary, file)
  invisible(file)
}

#' Create a reproducibility lockfile for a DataSUS download
#'
#' Records the exact request, source-file SHA-256 checksums, selected TabWin
#' dictionaries, parser version, and packaged reference tables associated with
#' an object returned by [fetch_datasus()].
#'
#' @param x An object returned with `provenance = TRUE`, or with
#'   `collect = FALSE`, by [fetch_datasus()].
#' @param file Optional path where the RDS lockfile is written atomically.
#' @return A `microdatasus_lockfile` list, invisibly when `file` is supplied.
#' @export
datasus_lockfile <- function(x, file = NULL) {
  provenance <- datasus_provenance(x)
  if (is.null(provenance)) {
    cli::cli_abort(paste(
      "No provenance is attached; call fetch_datasus() with",
      "provenance = TRUE or collect = FALSE."
    ))
  }
  report <- processing_diagnostics(x)
  lockfile <- structure(
    list(
      format_version = 1L,
      package_version = as.character(utils::packageVersion("microdatasus")),
      created_at = Sys.time(),
      checksum_algorithm = "sha256",
      parser_version = .tabwin_parser_version,
      request = attr(x, "microdatasus_request", exact = TRUE),
      files = provenance,
      dictionaries = .datasus_lockfile_diagnostics(
        report, "dictionaries"
      ),
      reference_tables = .datasus_lockfile_diagnostics(
        report, "reference_tables"
      )
    ),
    class = "microdatasus_lockfile"
  )
  if (is.null(file)) return(lockfile)
  .datasus_write_lockfile(lockfile, file)
  invisible(lockfile)
}

#' Read a DataSUS reproducibility lockfile
#'
#' @param file Path to an RDS lockfile created by [datasus_lockfile()].
#' @return A `microdatasus_lockfile` list.
#' @export
read_datasus_lockfile <- function(file) {
  .datasus_lockfile_value(file)
}

#' Verify files pinned by a DataSUS reproducibility lockfile
#'
#' Recomputes retained DBC and TabWin archive checksums and the checksums of
#' packaged reference tables. A status of `unavailable` means the checksum
#' remains pinned but the corresponding downloaded file is no longer local; it
#' does not imply a mismatch.
#'
#' @param lockfile A lockfile object or path returned by [datasus_lockfile()].
#' @return A tibble with one row per pinned component and verification status.
#' @export
verify_datasus_lockfile <- function(lockfile) {
  lockfile <- .datasus_lockfile_value(lockfile)
  records <- list()
  files <- lockfile$files
  if (is.data.frame(files) && nrow(files)) {
    records[[length(records) + 1L]] <- tibble::tibble(
      component = "dbc",
      name = as.character(files$file),
      path = if ("dbc_path" %in% names(files)) {
        as.character(files$dbc_path)
      } else {
        NA_character_
      },
      expected = as.character(files$checksum),
      checksum_algorithm = if ("checksum_algorithm" %in% names(files)) {
        as.character(files$checksum_algorithm)
      } else {
        "md5"
      }
    )
  }
  dictionaries <- lockfile$dictionaries
  if (is.data.frame(dictionaries) && nrow(dictionaries)) {
    records[[length(records) + 1L]] <- tibble::tibble(
      component = "dictionary",
      name = as.character(dictionaries$information_system),
      path = if ("archive_path" %in% names(dictionaries)) {
        as.character(dictionaries$archive_path)
      } else {
        NA_character_
      },
      expected = as.character(dictionaries$archive_checksum),
      checksum_algorithm =
        if ("archive_checksum_algorithm" %in% names(dictionaries)) {
          as.character(dictionaries$archive_checksum_algorithm)
        } else {
          "md5"
        }
    )
  }
  references <- lockfile$reference_tables
  if (is.data.frame(references) && nrow(references)) {
    current <- .datasus_reference_table_metadata(
      intersect(references$table, .datasus_reference_specs()$table)
    )
    actual <- current$checksum[match(references$table, current$table)]
    records[[length(records) + 1L]] <- tibble::tibble(
      component = "reference",
      name = as.character(references$table),
      path = NA_character_,
      expected = as.character(references$checksum),
      checksum_algorithm =
        if ("checksum_algorithm" %in% names(references)) {
          as.character(references$checksum_algorithm)
        } else {
          "sha256"
        },
      actual = actual
    )
  }
  if (!length(records)) {
    return(tibble::tibble(
      component = character(), name = character(), path = character(),
      expected = character(), actual = character(),
      checksum_algorithm = character(), status = character()
    ))
  }
  result <- tibble::as_tibble(data.table::rbindlist(
    records, use.names = TRUE, fill = TRUE
  ))
  if (!"actual" %in% names(result)) {
    result$actual <- NA_character_
  }
  downloaded <- result$component != "reference"
  result$actual[downloaded] <- vapply(
    which(downloaded),
    function(index) {
      path <- result$path[[index]]
      if (is.na(path) || !nzchar(path) || !file.exists(path)) {
        return(NA_character_)
      }
      .datasus_checksum(path, result$checksum_algorithm[[index]])
    },
    character(1)
  )
  result$status <- ifelse(
    is.na(result$actual), "unavailable",
    ifelse(result$actual == result$expected, "ok", "mismatch")
  )
  result[, c(
    "component", "name", "path", "expected", "actual",
    "checksum_algorithm", "status"
  )]
}
