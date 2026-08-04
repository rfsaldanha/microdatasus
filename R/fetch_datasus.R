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
#' @param cache_dir Optional directory used as a persistent cache for downloaded
#'   DBC files and TabWin dictionaries. The `microdatasus.cache_dir` option
#'   supplies the default; `NULL` preserves the historical session-temporary
#'   behavior.
#' @param refresh Logical scalar. If `TRUE`, download files again even when a
#'   valid cached copy is available.
#' @param destination Optional directory in which each prepared file is saved
#'   separately as RDS. This supports requests too large to combine in memory.
#' @param collect Logical scalar. If `TRUE` (the default), combine and return
#'   records as before. If `FALSE`, write per-file RDS outputs and return their
#'   provenance table; in that case `destination` is required.
#' @param process Logical scalar. If `TRUE`, run the processor matching
#'   `information_system` independently on each downloaded file.
#' @param process_args Named list of additional arguments passed to the matching
#'   `process_*()` function. It cannot replace `data` or `information_system`.
#' @param provenance Logical scalar. If `TRUE`, attach download URLs, checksums,
#'   sizes, timestamps, cache status, and paths. Retrieve the table with
#'   [datasus_provenance()].
#' @param keep_files Logical scalar. If `TRUE` and `destination` is supplied,
#'   retain a copy of each raw DBC file under `destination/dbc`.
#' @param row_filter Optional function called on each raw DBC table immediately
#'   after reading and before processing or column selection. It must return one
#'   non-missing logical value per row. This bounds downstream processing and
#'   output without changing which source files are downloaded.
#'
#' @return With `collect = TRUE`, a tibble containing all successfully read
#'   records, or `NULL` if no requested file could be read. With
#'   `collect = FALSE`, a provenance tibble with one row per output file.
#'
#' @details
#' The function first lists the relevant DataSUS directories and downloads only
#' files present in those listings. When more than one publication represents
#' the same system, period, state, and file part, definitive/current data take
#' precedence over preliminary data, and current data take precedence over
#' historical copies.
#'
#' Files are handled sequentially and, when requested, processed and written
#' before the next file is read. Thus `collect = FALSE` bounds working memory
#' to approximately one source file. Unless `quiet = TRUE`, transfer progress is
#' displayed by [curl::curl_download()]. Transient network failures are retried
#' up to two times; missing, empty, invalid DBC, and incompatible-schema files
#' are not retried. Partial files and other temporary files are removed before
#' the function returns or aborts.
#'
#' When `cache_dir` is supplied, complete DBC files and dictionaries persist
#' across R sessions. Cache entries include a manifest and SHA-256 checksum (while still accepting legacy MD5 manifests).
#' [datasus_cache_info()] inspects them and [clear_datasus_cache()] removes only
#' files managed by microdatasus.
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
#'   returned by [datasus_information_systems()].
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
#' [read_dbc()] for local DBC files; [datasus_information_systems()] for the
#' complete identifier lookup; [process_sim()], [process_sinasc()],
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
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL),
  refresh = FALSE,
  destination = NULL,
  collect = TRUE,
  process = FALSE,
  process_args = list(),
  provenance = FALSE,
  keep_files = FALSE,
  row_filter = NULL
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
  .datasus_assert_flag(refresh, "refresh")
  .datasus_assert_flag(collect, "collect")
  .datasus_assert_flag(provenance, "provenance")
  .datasus_assert_flag(keep_files, "keep_files")
  .datasus_validate_row_filter(row_filter)
  .datasus_validate_process_args(process, process_args)
  if (any(names(process_args) %in% c("data", "information_system"))) {
    cli::cli_abort(
      "{.arg process_args} cannot replace {.arg data} or {.arg information_system}."
    )
  }
  request_record <- list(
    year_start = year_start, month_start = month_start, year_end = year_end,
    month_end = month_end, uf = uf, information_system = information_system,
    vars = vars, process = process, process_args = process_args,
    row_filter = if (is.null(row_filter)) NULL else paste(
      deparse(body(row_filter)), collapse = " "
    )
  )
  cache_root <- .datasus_cache_path(
    cache_dir,
    create = !is.null(cache_dir)
  )
  if (!is.null(destination)) {
    destination <- .datasus_cache_path(destination, create = TRUE)
  }
  if (!collect && is.null(destination)) {
    cli::cli_abort(
      "{.arg destination} is required when {.arg collect} is FALSE."
    )
  }

  spec <- request$spec
  information_system <- request$information_system
  if (identical(spec$geography, "national") && !identical(uf, "all")) {
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
    if (stop_on_error) cli::cli_abort(message) else cli::cli_warn(message)
  }
  if (!nrow(manifest) && length(discovery$errors)) return(NULL)

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
      } else ""
      cli::cli_warn(c(
        "{length(missing_keys)} requested state-period combination{?s} unavailable from DataSUS.",
        "i" = "Unavailable: {.val {shown}}{suffix}."
      ))
    }
  }
  if (!nrow(manifest)) return(NULL)

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
  provenance_records <- list()
  diagnostic_records <- list()
  failures <- character()
  processed_files <- 0L
  for (index in seq_len(nrow(manifest))) {
    remote <- manifest[index, , drop = FALSE]
    cache_file_dir <- if (is.null(cache_root)) NULL else file.path(
      cache_root,
      "dbc",
      .datasus_cache_component(information_system),
      .datasus_cache_component(remote$file)
    )
    if (!is.null(cache_file_dir) && !dir.exists(cache_file_dir)) {
      dir.create(cache_file_dir, recursive = TRUE)
    }
    temporary <- if (is.null(cache_file_dir)) {
      tempfile(fileext = ".dbc.part")
    } else {
      file.path(cache_file_dir, "data.dbc")
    }
    manifest_path <- if (is.null(cache_file_dir)) NULL else {
      file.path(cache_file_dir, "manifest.rds")
    }
    cache_hit <- !refresh &&
      .datasus_cache_valid(temporary, manifest_path)

    if (!quiet) {
      verb <- if (cache_hit) "Reading cached" else "Downloading"
      cli::cli_alert_info(
        "{verb} [{index}/{nrow(manifest)}] {.file {remote$file}}..."
      )
    }
    result <- tryCatch(
      {
        if (!cache_hit) {
          .datasus_download_file(
            remote$url,
            temporary,
            timeout,
            quiet = quiet
          )
        }
        file_metadata <- if (cache_hit && !is.null(manifest_path) &&
            file.exists(manifest_path)) {
          tryCatch(readRDS(manifest_path), error = function(error) NULL)
        } else NULL
        if (is.null(file_metadata)) {
          file_metadata <- .datasus_file_provenance(
            temporary,
            remote$url,
            cached = cache_hit
          )
        } else {
          file_metadata$cached <- TRUE
        }
        file_metadata$type <- "dbc"
        file_metadata$information_system <- information_system
        file_metadata$file <- remote$file

        if (!quiet) {
          cli::cli_alert_info(
            "Reading [{index}/{nrow(manifest)}] {.file {remote$file}}..."
          )
        }
        partial <- read_dbc(file = temporary, as_character = TRUE)
        source_rows <- nrow(partial)
        if (!is.null(row_filter)) {
          partial <- .datasus_apply_row_filter(partial, row_filter)
        }
        # Publish a persistent manifest only after the DBC was read
        # successfully, so interrupted or invalid downloads are never trusted.
        if (!is.null(manifest_path) &&
            (!cache_hit || !file.exists(manifest_path))) {
          .datasus_write_manifest(file_metadata, manifest_path)
        }
        if (track_source && "source" %in% names(partial)) {
          cli::cli_abort(
            "Cannot add source tracking because the DBC already has a {.field source} column.",
            class = "microdatasus_source_conflict"
          )
        }
        if (track_source) partial$source <- remote$file
        file_diagnostics <- NULL
        if (process) {
          partial <- if (quiet) {
            suppressMessages(.datasus_process_file(
              partial,
              information_system,
              process_args,
              cache_dir = cache_root
            ))
          } else {
            .datasus_process_file(
              partial, information_system, process_args, cache_root
            )
          }
          file_diagnostics <- processing_diagnostics(partial)
        }

        requested_vars <- vars
        if (track_source && !is.null(requested_vars)) {
          requested_vars <- setdiff(requested_vars, "source")
        }
        if (!is.null(requested_vars) &&
            !all(requested_vars %in% names(partial))) {
          unknown <- setdiff(requested_vars, names(partial))
          cli::cli_abort(
            "Unknown variable name{?s}: {.field {unknown}}.",
            class = "microdatasus_unknown_vars"
          )
        }
        if (!is.null(requested_vars)) {
          partial <- partial[, requested_vars, drop = FALSE]
        }
        if (track_source && !"source" %in% names(partial)) {
          partial$source <- remote$file
        }
        if (!is.null(file_diagnostics)) {
          attr(partial, "microdatasus_diagnostics") <- file_diagnostics
        }
        data_path <- NA_character_
        if (!is.null(destination)) {
          data_path <- .datasus_output_path(destination, remote$file)
          .datasus_save_rds(partial, data_path)
        }
        dbc_path <- if (!is.null(cache_file_dir)) temporary else NA_character_
        if (keep_files && !is.null(destination)) {
          raw_dir <- file.path(destination, "dbc")
          if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)
          kept <- file.path(raw_dir, basename(remote$file))
          if (!file.copy(temporary, kept, overwrite = TRUE)) {
            cli::cli_abort("Could not retain {.file {remote$file}}.")
          }
          dbc_path <- kept
        }
        record <- data.frame(
          file = remote$file,
          url = remote$url,
          period = remote$period,
          uf = remote$uf,
          release = remote$release,
          source_rows = source_rows,
          rows = nrow(partial),
          size = file_metadata$size,
          checksum = file_metadata$checksum,
          checksum_algorithm = if (is.null(file_metadata$checksum_algorithm)) "md5" else file_metadata$checksum_algorithm,
          downloaded_at = as.POSIXct(file_metadata$downloaded_at),
          cached = isTRUE(file_metadata$cached),
          dbc_path = dbc_path,
          data_path = data_path,
          stringsAsFactors = FALSE
        )
        list(
          data = partial,
          provenance = record,
          diagnostics = file_diagnostics
        )
      },
      error = identity,
      finally = if (is.null(cache_file_dir)) unlink(temporary)
    )

    if (inherits(result, "error")) {
      if (cache_hit && !is.null(cache_file_dir)) {
        unlink(c(temporary, manifest_path))
      }
      if (inherits(
        result,
        c("microdatasus_source_conflict", "microdatasus_unknown_vars",
          "microdatasus_row_filter_error")
      )) stop(result)
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
    provenance_records[[length(provenance_records) + 1L]] <- result$provenance
    if (!is.null(result$diagnostics)) {
      diagnostic_records[[remote$file]] <- result$diagnostics
    }
    if (collect && nrow(result$data) > 0L) {
      parts[[length(parts) + 1L]] <- result$data
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
  provenance_table <- .datasus_provenance_table(provenance_records)
  if (!collect) {
    attr(provenance_table, "microdatasus_provenance") <- provenance_table
    attr(provenance_table, "microdatasus_request") <- request_record
    return(provenance_table)
  }
  if (!length(parts)) return(NULL)

  combined <- tibble::as_tibble(data.table::rbindlist(
    parts,
    use.names = TRUE,
    fill = TRUE
  ))
  attr(combined, "microdatasus_request") <- request_record
  if (provenance) {
    attr(combined, "microdatasus_provenance") <- provenance_table
  }
  if (length(diagnostic_records)) {
    attr(combined, "microdatasus_diagnostics") <- structure(
      list(
        information_system = information_system,
        input_rows = sum(provenance_table$rows),
        output_rows = nrow(combined),
        files = diagnostic_records
      ),
      class = "microdatasus_processing_diagnostics"
    )
  }
  combined
}
