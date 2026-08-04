.datasus_validate_row_filter <- function(row_filter) {
  if (!is.null(row_filter) && !is.function(row_filter)) {
    cli::cli_abort("{.arg row_filter} must be NULL or a function.")
  }
  invisible(row_filter)
}

.datasus_apply_row_filter <- function(data, row_filter) {
  keep <- tryCatch(
    row_filter(data),
    error = function(error) {
      cli::cli_abort(
        c("{.arg row_filter} failed.", "i" = conditionMessage(error)),
        class = "microdatasus_row_filter_error"
      )
    }
  )
  if (!is.logical(keep) || length(keep) != nrow(data) || anyNA(keep)) {
    cli::cli_abort(
      "{.arg row_filter} must return one non-missing logical value per row.",
      class = "microdatasus_row_filter_error"
    )
  }
  data[keep, , drop = FALSE]
}

.datasus_validate_process_args <- function(process, process_args) {
  .datasus_assert_flag(process, "process")
  if (!is.list(process_args)) {
    cli::cli_abort("{.arg process_args} must be a named list.")
  }
  argument_names <- names(process_args)
  invalid_names <- length(process_args) && (
    is.null(argument_names) ||
      anyNA(argument_names) ||
      any(!nzchar(argument_names)) ||
      anyDuplicated(argument_names)
  )
  if (invalid_names) {
    cli::cli_abort(
      "{.arg process_args} must have unique, non-empty names."
    )
  }
  invisible(process_args)
}

# Dispatch one downloaded file while forwarding only processor-specific options.
.datasus_process_file <- function(
  data,
  information_system,
  process_args,
  cache_dir = NULL
) {
  old_cache <- getOption("microdatasus.cache_dir")
  on.exit(options(microdatasus.cache_dir = old_cache), add = TRUE)
  if (!is.null(cache_dir)) {
    options(microdatasus.cache_dir = cache_dir)
  }
  system <- sub("-.*$", "", information_system)
  call <- switch(
    system,
    "SIM" = c(
      list(data = data, information_system = information_system),
      process_args
    ),
    "SINASC" = c(list(data = data), process_args),
    "SIH" = c(
      list(data = data, information_system = information_system),
      process_args
    ),
    "SIA" = c(
      list(data = data, information_system = information_system),
      process_args
    ),
    "CNES" = c(
      list(data = data, information_system = information_system),
      process_args
    ),
    "SINAN" = c(
      list(data = data, information_system = information_system),
      process_args
    )
  )
  processor <- switch(
    system,
    "SIM" = process_sim,
    "SINASC" = process_sinasc,
    "SIH" = process_sih,
    "SIA" = process_sia,
    "CNES" = process_cnes,
    "SINAN" = process_sinan
  )
  do.call(processor, call)
}

.datasus_output_path <- function(destination, remote_file) {
  stem <- tools::file_path_sans_ext(basename(remote_file))
  file.path(destination, paste0(stem, ".rds"))
}

.datasus_save_rds <- function(data, path) {
  temporary <- .datasus_temporary_path(path)
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(data, temporary, version = 2, compress = "xz")
  tryCatch(
    .datasus_commit_file(temporary, path),
    error = function(error) {
      cli::cli_abort("Could not save processed data to {.path {path}}.")
    }
  )
  invisible(path)
}

.datasus_provenance_table <- function(records) {
  if (!length(records)) {
    return(tibble::tibble(
      file = character(),
      url = character(),
      period = character(),
      uf = character(),
      release = character(),
      source_rows = integer(),
      rows = integer(),
      size = numeric(),
      checksum = character(),
      checksum_algorithm = character(),
      downloaded_at = as.POSIXct(character()),
      cached = logical(),
      dbc_path = character(),
      data_path = character()
    ))
  }
  tibble::as_tibble(do.call(rbind, records))
}
