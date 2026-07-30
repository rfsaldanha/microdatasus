# Cache lifecycle and public dictionary download entry point.
# Parsing and conversion helpers remain in tabwin_dictionary.R.

.tabwin_remove_cache_dir <- function(cache_dir) {
  # Only remove directories created below the current R temporary directory.
  if (
    is.character(cache_dir) &&
      length(cache_dir) == 1L &&
      startsWith(cache_dir, tempdir()) &&
      dir.exists(cache_dir)
  ) {
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }
}

.tabwin_clear_cache <- function(information_system = NULL) {
  dictionary_keys <- ls(envir = .tabwin_cache, all.names = TRUE)
  archive_keys <- ls(envir = .tabwin_archive_cache, all.names = TRUE)

  if (is.null(information_system)) {
    if (length(dictionary_keys)) {
      rm(list = dictionary_keys, envir = .tabwin_cache)
    }
    for (archive_key in archive_keys) {
      archive <- get(
        archive_key,
        envir = .tabwin_archive_cache,
        inherits = FALSE
      )
      if (!isTRUE(archive$persistent)) {
        .tabwin_remove_cache_dir(archive$cache_dir)
      }
    }
    if (length(archive_keys)) {
      rm(list = archive_keys, envir = .tabwin_archive_cache)
    }
    return(invisible(NULL))
  }

  information_system <- .sinan_resolve_information_system(information_system)
  registry <- .tabwin_registry()
  physical_key <- if (information_system %in% names(registry)) {
    registry[[information_system]]$archive_key
  } else {
    NULL
  }
  if (is.null(physical_key)) {
    return(invisible(NULL))
  }

  # Every dictionary built from the same physical archive is invalidated in
  # memory, while explicitly persistent files remain available on disk.
  for (dictionary_key in dictionary_keys) {
    dictionary <- get(
      dictionary_key,
      envir = .tabwin_cache,
      inherits = FALSE
    )
    if (identical(dictionary$archive_key, physical_key)) {
      rm(list = dictionary_key, envir = .tabwin_cache)
    }
  }
  matching_archives <- archive_keys[startsWith(
    archive_keys,
    paste0(physical_key, "::")
  )]
  for (archive_key in matching_archives) {
    archive <- get(
      archive_key,
      envir = .tabwin_archive_cache,
      inherits = FALSE
    )
    if (!isTRUE(archive$persistent)) {
      .tabwin_remove_cache_dir(archive$cache_dir)
    }
    rm(list = archive_key, envir = .tabwin_archive_cache)
  }
  invisible(NULL)
}

#' Download a TabWin data dictionary
#'
#' Downloads and parses official TabWin definition archives published by
#' DataSUS. Archive files, DEF metadata, and conversion tables used during
#' processing are cached in memory and optionally persisted across R
#' sessions. SIM support is limited to CID-10 files; SINASC supports both its
#' 1994-1995 and current layouts. SIH supports its current and historical RD/RJ
#' definitions; SIA supports all twelve current layouts plus the three
#' historical PA definitions; and CNES supports all thirteen layouts plus both
#' service-classification periods. SINAN supports all 58 transfer-page file
#' families.
#'
#' @param information_system Information system whose dictionary should be
#'   downloaded. Supported values include the five SIM mortality types,
#'   `"SINASC"` for files from 1996 onward, `"SINASC-1994-1995"` for the
#'   original SINASC layout, and `"SIH-RD"`, `"SIH-RJ"`, `"SIH-SP"`, and
#'   `"SIH-ER"` and the twelve `"SIA-*"` file families. Historical SIH and
#'   SIA-PA keys are selected internally by their processing functions. All
#'   thirteen `"CNES-*"` families are also supported; the historical CNES-SR
#'   key is selected internally by [process_cnes()]. The 58 readable SINAN
#'   identifiers and their aliases, listed by [datasus_information_systems()]
#'   under `system == "SINAN"`, are also accepted here.
#' @param timeout A positive numeric scalar. Download and connection timeout,
#'   in seconds.
#' @param refresh Logical scalar. If `TRUE`, discard the session cache and
#'   download the archive again.
#' @param quiet Logical scalar. If `TRUE`, suppress download progress and
#'   status messages.
#' @param cache_dir Optional persistent cache root. The package option
#'   `microdatasus.cache_dir` is used by default; `NULL` uses only the current
#'   session cache.
#'
#' @return An object of class `microdatasus_tabwin_dictionary`. Its
#'   `definitions` element describes the conversions found in the official DEF
#'   file.
#'
#' @section Network access:
#' The first call downloads the relevant TabWin ZIP from DataSUS. Systems that
#' share an archive reuse one copy. With `cache_dir`, the ZIP, its checksum, and
#' manifest persist across sessions; `refresh = TRUE` replaces the cached copy.
#'
#' @examplesIf interactive() && curl::has_internet()
#' dictionary <- fetch_tabwin_dictionary("SIM-DO")
#' dictionary$definitions
#' sinasc_dictionary <- fetch_tabwin_dictionary("SINASC")
#' sih_dictionary <- fetch_tabwin_dictionary("SIH-RD")
#' sia_dictionary <- fetch_tabwin_dictionary("SIA-PA")
#' cnes_dictionary <- fetch_tabwin_dictionary("CNES-ST")
#' sinan_dictionary <- fetch_tabwin_dictionary("SINAN-DENGUE")
#'
#' @seealso [datasus_information_systems()], [process_sim()],
#'   [process_sinasc()], [process_sih()],
#'   [process_sia()], [process_cnes()], [process_sinan()], [fetch_datasus()]
#' @export
fetch_tabwin_dictionary <- function(
  information_system = "SIM-DO",
  timeout = 240,
  refresh = FALSE,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL)
) {
  information_system <- .sinan_resolve_information_system(
    information_system
  )
  registry <- .tabwin_registry()
  if (
    !is.character(information_system) ||
      length(information_system) != 1L ||
      is.na(information_system) ||
      !information_system %in% names(registry)
  ) {
    cli::cli_abort(
      "{.arg information_system} must be one of: {.val {names(registry)}}."
    )
  }
  .datasus_assert_number(timeout, "timeout", lower = .Machine$double.eps)
  .datasus_assert_flag(refresh, "refresh")
  .datasus_assert_flag(quiet, "quiet")

  cache_root <- .datasus_cache_path(
    cache_dir,
    create = !is.null(cache_dir)
  )
  cache_identity <- if (is.null(cache_root)) "session" else cache_root
  key <- paste(toupper(information_system), cache_identity, sep = "::")

  # The in-memory cache remains the fastest layer. A cache root adds a durable
  # ZIP and manifest that can be reused by later R sessions.
  if (!refresh && exists(key, envir = .tabwin_cache, inherits = FALSE)) {
    return(get(key, envir = .tabwin_cache, inherits = FALSE))
  }
  if (refresh) {
    .tabwin_clear_cache()
  }
  spec <- registry[[information_system]]
  archive_session_key <- paste(
    spec$archive_key,
    cache_identity,
    sep = "::"
  )
  archive_created <- FALSE
  if (
    exists(
      archive_session_key,
      envir = .tabwin_archive_cache,
      inherits = FALSE
    )
  ) {
    archive_cache <- get(
      archive_session_key,
      envir = .tabwin_archive_cache,
      inherits = FALSE
    )
    # Reusing a parsed archive is a cache hit for this dictionary request.
    archive_cache$cached <- TRUE
  } else {
    work_dir <- if (is.null(cache_root)) {
      tempfile("microdatasus-tabwin-")
    } else {
      file.path(
        cache_root,
        "tabwin",
        .datasus_cache_component(spec$archive_key)
      )
    }
    if (!dir.exists(work_dir) && !dir.create(work_dir, recursive = TRUE)) {
      cli::cli_abort("Failed to create the TabWin cache directory.")
    }
    complete <- FALSE
    on.exit({
      if (!complete && is.null(cache_root)) {
        .tabwin_remove_cache_dir(work_dir)
      }
    }, add = TRUE)

    archive <- file.path(work_dir, "dictionary.zip")
    manifest_path <- file.path(work_dir, "manifest.rds")
    disk_hit <- !refresh &&
      .datasus_cache_valid(archive, manifest_path)
    if (!disk_hit) {
      if (!quiet) {
        cli::cli_alert_info(
          "Downloading the DataSUS TabWin dictionary for {.val {information_system}}..."
        )
      }
      .datasus_download_file(
        spec$url,
        archive,
        timeout = timeout,
        quiet = quiet
      )
    }
    entries <- tryCatch(
      zip::zip_list(archive)$filename,
      error = function(error) {
        cli::cli_abort(c(
          "Failed to list the downloaded TabWin archive.",
          "i" = conditionMessage(error)
        ))
      }
    )
    provenance <- if (disk_hit && file.exists(manifest_path)) {
      tryCatch(readRDS(manifest_path), error = function(error) NULL)
    } else {
      NULL
    }
    if (is.null(provenance)) {
      provenance <- .datasus_file_provenance(
        archive,
        spec$url,
        cached = disk_hit
      )
    } else {
      provenance$cached <- TRUE
    }
    provenance$type <- "tabwin"
    provenance$information_system <- information_system
    if (!is.null(cache_root) && (!disk_hit || !file.exists(manifest_path))) {
      .datasus_write_manifest(provenance, manifest_path)
    }
    archive_cache <- c(
      list(
        archive_key = spec$archive_key,
        source = spec$url,
        archive = archive,
        entries = entries,
        cache_dir = work_dir,
        persistent = !is.null(cache_root)
      ),
      provenance[c("downloaded_at", "size", "checksum", "cached")]
    )
    assign(
      archive_session_key,
      archive_cache,
      envir = .tabwin_archive_cache
    )
    archive_created <- TRUE
    complete <- TRUE
  }
  extract_all <- isTRUE(spec$extract_all)
  if (extract_all) {
    # Base R's internal unzip handles the legacy directory encodings used by
    # some DataSUS packages. Flattening is safe for these single-folder ZIPs.
    extracted <- suppressWarnings(utils::unzip(
      zipfile = archive_cache$archive,
      exdir = archive_cache$cache_dir,
      junkpaths = TRUE,
      overwrite = TRUE
    ))
    definition_entry <- spec$definition
    candidates <- list.files(archive_cache$cache_dir, full.names = TRUE)
    definition_matches <- which(
      .tabwin_filename_key(candidates) ==
        .tabwin_filename_key(spec$definition)
    )
    definition_path <- if (length(definition_matches) == 1L) {
      candidates[[definition_matches]]
    } else {
      file.path(archive_cache$cache_dir, basename(spec$definition))
    }
  } else {
    # Archives with portable paths keep lazy extraction of CNV/DBF files.
    definition_entry <- .tabwin_find_entry(
      archive_cache$entries,
      spec$definition
    )
    extracted <- zip::unzip(
      zipfile = archive_cache$archive,
      files = definition_entry,
      exdir = archive_cache$cache_dir,
      junkpaths = TRUE,
      overwrite = TRUE
    )
    definition_path <- file.path(
      archive_cache$cache_dir,
      basename(definition_entry)
    )
  }
  if (!length(extracted) || !file.exists(definition_path)) {
    if (archive_created) {
      .tabwin_clear_cache()
    }
    cli::cli_abort("Failed to extract the TabWin DEF file.")
  }

  dictionary <- structure(
    list(
      information_system = information_system,
      archive_key = spec$archive_key,
      archive_session_key = archive_session_key,
      source = archive_cache$source,
      downloaded_at = archive_cache$downloaded_at,
      archive_size = archive_cache$size,
      archive_checksum = archive_cache$checksum,
      cache_hit = archive_cache$cached,
      definition = definition_entry,
      definitions = .tabwin_parse_def(definition_path),
      numeric_fields = .tabwin_parse_increment_fields(definition_path),
      archive = archive_cache$archive,
      entries = archive_cache$entries,
      definition_dir = dirname(gsub("\\\\", "/", definition_entry)),
      cache_dir = archive_cache$cache_dir,
      extracted_all = extract_all,
      conversions = new.env(parent = emptyenv())
    ),
    class = "microdatasus_tabwin_dictionary"
  )
  assign(key, dictionary, envir = .tabwin_cache)
  if (!quiet) {
    cli::cli_alert_success(
      "Cached the DataSUS TabWin dictionary for {.val {information_system}}."
    )
  }
  dictionary
}
