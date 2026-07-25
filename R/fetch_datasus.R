#' Fetch and read microdata files from DataSUS
#'
#' `fetch_datasus()` downloads DBC microdata files published by DataSUS,
#' reads them and combines their rows in a single data frame.
#'
#' The supported state abbreviations are "AC", "AL", "AP", "AM", "BA", "CE",
#' "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
#' "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE" and "TO".
#'
#' The following systems are implemented: "SIH-RD", "SIH-RJ", "SIH-SP",
#' "SIH-ER", "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT",
#' "SINASC", "CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR",
#' "CNES-HB", "CNES-PF", "CNES-EP", "CNES-RC", "CNES-IN", "CNES-EE",
#' "CNES-EF", "CNES-GM", "SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD",
#' "SIA-AN", "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", "SIA-PS",
#' "SIA-SAD", "SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SINAN-ZIKA",
#' "SINAN-MALARIA", "SINAN-CHAGAS", "SINAN-LEISHMANIOSE-VISCERAL",
#' "SINAN-LEISHMANIOSE-TEGUMENTAR" and "SINAN-LEPTOSPIROSE".
#'
#' @param year_start,year_end Numeric scalars. First and last requested years.
#' @param month_start,month_end Numeric scalars. First and last requested
#'   months. Required for SIH, CNES and SIA and ignored for annual systems.
#' @param uf A state abbreviation, a vector of abbreviations, or `"all"`.
#' @param information_system A supported information-system abbreviation.
#' @param vars An optional character vector containing variables to retain.
#' @param stop_on_error If `TRUE`, stop when a file cannot be downloaded or
#'   read. Otherwise, return all files that were successfully read.
#' @param timeout Download and connection timeout, in seconds.
#' @param track_source If `TRUE`, append a `source` column containing the
#'   original DBC file name.
#'
#' @return A tibble containing the combined DBC files, or `NULL` when no file
#'   could be read.
#'
#' @section Warning:
#' An Internet connection is required. DataSUS may restrict FTP access from
#' some countries. Dates refer to DataSUS processing periods and state
#' abbreviations refer to the place where records were processed. Downloaded
#' files are temporary and are removed after being read.
#'
#' @examplesIf interactive() && curl::has_internet()
#' sim <- fetch_datasus(
#'   year_start = 2014,
#'   year_end = 2014,
#'   uf = "AC",
#'   information_system = "SIM-DO",
#'   vars = c("CODMUNRES", "DTOBITO", "CAUSABAS")
#' )
#'
#' sih <- fetch_datasus(
#'   year_start = 2014,
#'   month_start = 1,
#'   year_end = 2014,
#'   month_end = 2,
#'   uf = c("AC", "RR"),
#'   information_system = "SIH-RD"
#' )
#'
#' @export
fetch_datasus <- function(
  year_start,
  month_start = NULL,
  year_end,
  month_end = NULL,
  uf = "all",
  information_system,
  vars = NULL,
  stop_on_error = FALSE,
  timeout = 240,
  track_source = FALSE
) {
  request <- .datasus_validate_arguments(
    year_start = year_start,
    month_start = month_start,
    year_end = year_end,
    month_end = month_end,
    uf = uf,
    information_system = information_system,
    vars = vars,
    stop_on_error = stop_on_error,
    timeout = timeout,
    track_source = track_source
  )
  spec <- request$spec

  if (
    identical(spec$geography, "national") &&
      !identical(uf, "all")
  ) {
    cli::cli_alert_info(
      "{information_system} files are national; ignoring {.arg uf}."
    )
  }

  cli::cli_alert_info("Discovering files available from DataSUS...")
  discovery <- .datasus_build_manifest(
    spec = spec,
    periods = request$periods,
    ufs = request$ufs,
    timeout = timeout
  )
  manifest <- discovery$manifest

  if (length(discovery$errors)) {
    message <- c(
      "Some DataSUS directories could not be listed.",
      "i" = paste(discovery$errors, collapse = "\n")
    )
    if (stop_on_error) {
      cli::cli_abort(message)
    } else {
      cli::cli_warn(message)
    }
  }
  if (!nrow(manifest) && length(discovery$errors)) {
    return(NULL)
  }

  available_periods <- if (nrow(manifest)) unique(manifest$period) else character()
  missing_periods <- request$periods[!request$periods %in% available_periods]
  if (length(missing_periods)) {
    cli::cli_warn(
      "Periods unavailable from DataSUS: {paste(missing_periods, collapse = ', ')}."
    )
  }
  if (nrow(manifest) && identical(spec$geography, "state")) {
    requested_keys <- as.vector(outer(
      request$periods,
      request$ufs,
      function(period, state) paste0(state, "-", period)
    ))
    available_keys <- paste0(manifest$uf, "-", manifest$period)
    missing_keys <- requested_keys[!requested_keys %in% available_keys]
    if (length(missing_keys)) {
      shown <- utils::head(missing_keys, 10L)
      suffix <- if (length(missing_keys) > length(shown)) {
        paste0(" and ", length(missing_keys) - length(shown), " more")
      } else {
        ""
      }
      cli::cli_warn(
        "State-period combinations unavailable from DataSUS: {paste(shown, collapse = ', ')}{suffix}."
      )
    }
  }
  if (!nrow(manifest)) {
    return(NULL)
  }

  preliminary <- unique(manifest$period[manifest$release == "preliminary"])
  if (length(preliminary)) {
    cli::cli_alert_info(
      "Preliminary periods: {paste(preliminary, collapse = ', ')}."
    )
  }
  old <- unique(manifest$period[manifest$release == "old"])
  if (length(old)) {
    cli::cli_alert_info(
      paste0(
        "Periods from historical directories may contain incompatible codes: ",
        paste(old, collapse = ", "),
        "."
      )
    )
  }

  cli::cli_alert_info(
    "Downloading and reading {nrow(manifest)} DataSUS file{?s}..."
  )
  parts <- list()
  failures <- character()

  for (index in seq_len(nrow(manifest))) {
    remote <- manifest[index, , drop = FALSE]
    temporary <- tempfile(fileext = ".dbc.part")

    result <- tryCatch(
      {
        .datasus_download_file(remote$url, temporary, timeout)
        partial <- read_dbc(file = temporary, as_character = TRUE)

        if (track_source && "source" %in% names(partial)) {
          cli::cli_abort(
            "Cannot add source tracking because the DBC already has a {.field source} column.",
            class = "microdatasus_source_conflict"
          )
        }

        requested_vars <- vars
        if (track_source && !is.null(requested_vars)) {
          requested_vars <- setdiff(requested_vars, "source")
        }
        if (
          !is.null(requested_vars) &&
            !all(requested_vars %in% names(partial))
        ) {
          unknown <- setdiff(requested_vars, names(partial))
          cli::cli_abort(
            "Unknown variable name{?s}: {paste(unknown, collapse = ', ')}.",
            class = "microdatasus_unknown_vars"
          )
        }
        if (!is.null(requested_vars)) {
          partial <- partial[, requested_vars, drop = FALSE]
        }
        if (track_source) {
          partial$source <- remote$file
        }
        partial
      },
      error = identity,
      finally = unlink(temporary)
    )

    if (inherits(result, "error")) {
      if (inherits(
        result,
        c("microdatasus_source_conflict", "microdatasus_unknown_vars")
      )) {
        stop(result)
      }
      detail <- paste0(remote$file, ": ", conditionMessage(result))
      if (stop_on_error) {
        cli::cli_abort(c(
          "Failed to process DataSUS file {.file {remote$file}}.",
          "i" = conditionMessage(result)
        ))
      }
      failures <- c(failures, detail)
      next
    }
    if (nrow(result) > 0L) {
      parts[[length(parts) + 1L]] <- result
    }
  }

  .datasus_summarize_failures(failures)
  if (!length(parts)) {
    return(NULL)
  }

  combined <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  tibble::as_tibble(combined)
}
