#' Download DataSUS microdata
#'
#' Downloads published DBC files from DataSUS, reads them with [read_dbc()],
#' and combines the records in deterministic period, state, and file-part order.
#'
#' @param year_start,year_end Numeric scalars giving the first and last requested
#'   years, inclusive.
#' @param month_start,month_end Numeric scalars giving the first and last
#'   requested months, inclusive. Months are required for SIH, SIA, and CNES
#'   systems and ignored, with a warning, for annual systems.
#' @param uf A Brazilian state abbreviation, a character vector of
#'   abbreviations, or `"all"`. `"all"` cannot be combined with individual
#'   states. A warning alert is displayed when this argument is ignored for
#'   systems published only as national files.
#' @param information_system A single system identifier listed in
#'   **Supported systems**.
#' @param vars `NULL`, or a character vector of column names to retain. Selection
#'   is applied to each file before the files are combined.
#' @param stop_on_error Logical scalar. If `TRUE`, abort after any listing,
#'   download, or read failure. If `FALSE`, warn and return the files that could
#'   be read successfully.
#' @param timeout A positive numeric scalar giving the connection and transfer
#'   timeout, in seconds, for each network attempt.
#' @param track_source Logical scalar. If `TRUE`, append a `source` column with
#'   the original DBC file name. This column is retained even when `vars` is
#'   supplied. The function aborts if the downloaded data already contain a
#'   column named `source`.
#' @param quiet Logical scalar. If `FALSE` (the default), display the transfer
#'   progress reported by [curl::curl_download()] and announce each file before
#'   downloading it. If `TRUE`, suppress status messages, per-file
#'   announcements, and progress meters. Warnings and errors are not suppressed.
#'
#' @return A tibble containing all successfully read records, or `NULL` if no
#'   requested file could be read. No diagnostic attributes are added.
#'
#' @details
#' The function first lists the relevant DataSUS directories and downloads only
#' files present in those listings. When more than one publication represents
#' the same system, period, state, and file part, definitive/current data take
#' precedence over preliminary data, and current data take precedence over
#' historical copies.
#'
#' Downloads are sequential. Unless `quiet = TRUE`, transfer progress is
#' displayed by [curl::curl_download()]. Transient network failures are retried
#' up to two times; missing, empty, invalid DBC, and incompatible-schema files
#' are not retried. Partial files and other temporary files are removed before
#' the function returns or aborts.
#'
#' Years and state abbreviations refer to DataSUS processing periods and places
#' of processing, which may differ from dates or places of occurrence and
#' residence contained in the records.
#'
#' @section Supported systems:
#' - **SIH:** `"SIH-RD"`, `"SIH-RJ"`, `"SIH-SP"`, and `"SIH-ER"`.
#' - **SIM:** `"SIM-DO"`, `"SIM-DOFET"`, `"SIM-DOEXT"`, `"SIM-DOINF"`, and
#'   `"SIM-DOMAT"`.
#' - **SINASC:** `"SINASC"`.
#' - **CNES:** `"CNES-LT"`, `"CNES-ST"`, `"CNES-DC"`, `"CNES-EQ"`,
#'   `"CNES-SR"`, `"CNES-HB"`, `"CNES-PF"`, `"CNES-EP"`, `"CNES-RC"`,
#'   `"CNES-IN"`, `"CNES-EE"`, `"CNES-EF"`, and `"CNES-GM"`.
#' - **SIA:** `"SIA-AB"`, `"SIA-ABO"`, `"SIA-ACF"`, `"SIA-AD"`, `"SIA-AN"`,
#'   `"SIA-AM"`, `"SIA-AQ"`, `"SIA-AR"`, `"SIA-ATD"`, `"SIA-PA"`, `"SIA-PS"`,
#'   and `"SIA-SAD"`.
#' - **SINAN:** 58 readable identifiers, including `"SINAN-DENGUE"`,
#'   `"SINAN-TUBERCULOSE"`, and
#'   `"SINAN-ACIDENTE-POR-ANIMAIS-PECONHENTOS"`. Former acronym-based
#'   identifiers remain accepted as aliases. The complete lookup table is
#'   returned by [sinan_information_systems()].
#'
#' @section Network access:
#' An Internet connection and FTP access to DataSUS are required. DataSUS may
#' restrict FTP access from some countries.
#'
#' @references
#' Saldanha, R. F. (2026). [*Sistemas de Informação em Saúde no
#' Brasil*](https://rfsaldanha.github.io/sis/), especially the chapters on
#' [SIM](https://rfsaldanha.github.io/sis/sim.html),
#' [SINASC](https://rfsaldanha.github.io/sis/sinasc.html),
#' [SIH](https://rfsaldanha.github.io/sis/sih.html),
#' [SIA](https://rfsaldanha.github.io/sis/sia.html),
#' [SINAN](https://rfsaldanha.github.io/sis/sinan.html), and
#' [CNES](https://rfsaldanha.github.io/sis/cnes.html).
#'
#' @seealso
#' [read_dbc()] for local DBC files; [sinan_information_systems()] for the
#' SINAN identifier lookup; [process_sim()], [process_sinasc()],
#' [process_sih()], [process_sia()], [process_cnes()], and [process_sinan()]
#' for system-specific recoding.
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
  track_source = FALSE,
  quiet = FALSE
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
    track_source = track_source,
    quiet = quiet
  )
  spec <- request$spec
  information_system <- request$information_system

  if (
    identical(spec$geography, "national") &&
      !identical(uf, "all")
  ) {
    cli::cli_alert_warning(
      "{.arg uf} is ignored because {.val {information_system}} publishes national files."
    )
  }

  if (!quiet) {
    cli::cli_alert_info("Discovering available files on DataSUS...")
  }
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
      .datasus_cli_bullets(discovery$errors)
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
    cli::cli_warn(c(
      "{length(missing_periods)} requested period{?s} unavailable from DataSUS.",
      "i" = "Unavailable: {.val {missing_periods}}."
    ))
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
      cli::cli_warn(c(
        "{length(missing_keys)} requested state-period combination{?s} unavailable from DataSUS.",
        "i" = "Unavailable: {.val {shown}}{suffix}."
      ))
    }
  }
  if (!nrow(manifest)) {
    return(NULL)
  }

  preliminary <- unique(manifest$period[manifest$release == "preliminary"])
  if (!quiet && length(preliminary)) {
    cli::cli_alert_info(
      "Using preliminary data for {length(preliminary)} period{?s}: {.val {preliminary}}."
    )
  }
  old <- unique(manifest$period[manifest$release == "old"])
  if (!quiet && length(old)) {
    cli::cli_alert_info(
      "Using historical data for {length(old)} period{?s}: {.val {old}}. Codes may be incompatible with current files."
    )
  }

  if (!quiet) {
    cli::cli_alert_info(
      "Preparing to download and read {nrow(manifest)} DataSUS file{?s}..."
    )
  }
  parts <- list()
  failures <- character()
  processed_files <- 0L

  for (index in seq_len(nrow(manifest))) {
    remote <- manifest[index, , drop = FALSE]
    temporary <- tempfile(fileext = ".dbc.part")

    if (!quiet) {
      cli::cli_alert_info(
        "Downloading [{index}/{nrow(manifest)}] {.file {remote$file}}..."
      )
    }

    result <- tryCatch(
      {
        .datasus_download_file(
          remote$url,
          temporary,
          timeout,
          quiet = quiet
        )
        if (!quiet) {
          cli::cli_alert_info(
            "Reading [{index}/{nrow(manifest)}] {.file {remote$file}}..."
          )
        }
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
            "Unknown variable name{?s}: {.field {unknown}}.",
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
    processed_files <- processed_files + 1L
    if (nrow(result) > 0L) {
      parts[[length(parts) + 1L]] <- result
    }
  }

  if (!quiet && processed_files > 0L) {
    completion <- paste0(
      "Downloaded and read {processed_files} of {nrow(manifest)} ",
      "DataSUS file{?s}."
    )
    if (length(failures)) {
      cli::cli_alert_info(completion)
    } else {
      cli::cli_alert_success(completion)
    }
  }
  .datasus_summarize_failures(failures)
  if (!length(parts)) {
    return(NULL)
  }

  combined <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  tibble::as_tibble(combined)
}
