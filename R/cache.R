.datasus_cache_path <- function(cache_dir, create = FALSE) {
  if (is.null(cache_dir)) {
    return(NULL)
  }
  if (!is.character(cache_dir) || length(cache_dir) != 1L ||
      is.na(cache_dir) || !nzchar(cache_dir)) {
    cli::cli_abort("{.arg cache_dir} must be NULL or one non-empty path.")
  }
  path <- normalizePath(path.expand(cache_dir), mustWork = FALSE)
  if (file.exists(path) && !dir.exists(path)) {
    cli::cli_abort("{.arg cache_dir} must identify a directory.")
  }
  if (create && !dir.exists(path) && !dir.create(path, recursive = TRUE)) {
    cli::cli_abort("Could not create cache directory {.path {path}}.")
  }
  path
}

.datasus_cache_component <- function(value) {
  gsub("[^A-Za-z0-9._-]", "_", value)
}

.datasus_checksum <- function(path, algorithm = c("sha256", "md5")) {
  algorithm <- match.arg(algorithm)
  if (identical(algorithm, "md5")) {
    return(unname(tools::md5sum(path)))
  }
  digest::digest(path, algo = "sha256", file = TRUE, serialize = FALSE)
}

.datasus_file_provenance <- function(path, source, cached = FALSE) {
  info <- file.info(path)
  list(
    source = source,
    local_path = normalizePath(path, mustWork = FALSE),
    size = unname(info$size),
    checksum = .datasus_checksum(path, "sha256"),
    checksum_algorithm = "sha256",
    downloaded_at = Sys.time(),
    cached = isTRUE(cached)
  )
}

.datasus_temporary_path <- function(destination) {
  # Unique sibling files prevent concurrent R processes from sharing .part.
  tempfile(paste0(basename(destination), "-"),
           tmpdir = dirname(destination))
}

.datasus_with_cache_lock <- function(path, code, timeout = 60) {
  lock <- paste0(path, ".lock")
  started <- Sys.time()
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) break
    info <- file.info(lock)
    stale <- !is.na(info$mtime) &&
      as.numeric(difftime(Sys.time(), info$mtime, units = "secs")) > 600
    if (stale) {
      unlink(lock, recursive = TRUE, force = TRUE)
      next
    }
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= timeout) {
      cli::cli_abort("Timed out waiting for cache lock {.file {basename(lock)}}.")
    }
    Sys.sleep(0.05)
  }
  on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
  force(code)
}

.datasus_write_manifest <- function(manifest, path) {
  temporary <- .datasus_temporary_path(path)
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(manifest, temporary, version = 2)
  tryCatch(
    .datasus_commit_file(temporary, path),
    error = function(error) {
      cli::cli_abort(
        "Could not write cache manifest {.file {basename(path)}}."
      )
    }
  )
  invisible(path)
}

.datasus_cache_valid <- function(path, manifest_path = NULL) {
  # Session files need only be non-empty; persistent entries also require a
  # readable manifest whose size and checksum match the payload.
  if (!file.exists(path) || is.na(file.size(path)) || file.size(path) <= 0) {
    return(FALSE)
  }
  if (is.null(manifest_path)) {
    return(TRUE)
  }
  if (!file.exists(manifest_path)) {
    return(FALSE)
  }
  manifest <- tryCatch(readRDS(manifest_path), error = function(error) NULL)
  if (is.null(manifest)) {
    return(FALSE)
  }
  size_ok <- is.null(manifest$size) ||
    is.na(manifest$size) ||
    identical(as.numeric(file.size(path)), as.numeric(manifest$size))
  algorithm <- if (is.null(manifest$checksum_algorithm)) "md5" else {
    manifest$checksum_algorithm
  }
  checksum_ok <- is.null(manifest$checksum) ||
    is.na(manifest$checksum) ||
    identical(
      .datasus_checksum(path, algorithm),
      unname(as.character(manifest$checksum))
    )
  isTRUE(size_ok && checksum_ok)
}

#' Cache directory used by microdatasus
#'
#' Returns a platform-appropriate directory that users may pass to cache_dir or
#' set in options(microdatasus.cache_dir = ...).
#'
#' @param create Logical scalar. If TRUE, create the directory.
#'
#' @return A normalized directory path.
#'
#' @export
datasus_cache_dir <- function(create = FALSE) {
  .datasus_assert_flag(create, "create")
  configured <- getOption("microdatasus.cache_dir")
  path <- if (is.null(configured)) {
    tools::R_user_dir("microdatasus", "cache")
  } else {
    configured
  }
  .datasus_cache_path(path, create = create)
}

#' Inspect the persistent DataSUS cache
#'
#' @param cache_dir Cache root. The default is datasus_cache_dir().
#'
#' @return A tibble with one row per readable cache manifest.
#'
#' @export
datasus_cache_info <- function(cache_dir = datasus_cache_dir()) {
  root <- .datasus_cache_path(cache_dir, create = FALSE)
  empty <- tibble::tibble(
    type = character(),
    information_system = character(),
    source = character(),
    local_path = character(),
    size = numeric(),
    checksum = character(),
    checksum_algorithm = character(),
    downloaded_at = as.POSIXct(character()),
    cached = logical()
  )
  if (is.null(root) || !dir.exists(root)) {
    return(empty)
  }
  files <- list.files(
    root,
    pattern = "manifest[.]rds$",
    recursive = TRUE,
    full.names = TRUE
  )
  records <- lapply(files, function(file) {
    value <- tryCatch(readRDS(file), error = function(error) NULL)
    if (is.null(value)) {
      return(NULL)
    }
    fallback <- function(x, default) {
      if (is.null(x)) default else x
    }
    data.frame(
      type = fallback(value$type, NA_character_),
      information_system = fallback(value$information_system, NA_character_),
      source = fallback(value$source, NA_character_),
      local_path = fallback(value$local_path, NA_character_),
      size = fallback(value$size, NA_real_),
      checksum = fallback(value$checksum, NA_character_),
      checksum_algorithm = fallback(value$checksum_algorithm, "md5"),
      downloaded_at = as.POSIXct(value$downloaded_at, origin = "1970-01-01"),
      cached = TRUE,
      stringsAsFactors = FALSE
    )
  })
  records <- Filter(Negate(is.null), records)
  if (!length(records)) {
    return(empty)
  }
  tibble::as_tibble(do.call(rbind, records))
}

#' Clear persistent DataSUS cache contents
#'
#' Only the dbc, tabwin, and auxiliary subdirectories managed by microdatasus are removed;
#' the supplied cache root and unrelated files are preserved.
#'
#' @inheritParams datasus_cache_info
#'
#' @return The cache root, invisibly.
#'
#' @export
clear_datasus_cache <- function(cache_dir = datasus_cache_dir()) {
  root <- .datasus_cache_path(cache_dir, create = FALSE)
  if (is.null(root) || !dir.exists(root)) {
    return(invisible(root))
  }
  targets <- file.path(root, c("dbc", "tabwin", "auxiliary"))
  for (target in targets[dir.exists(targets)]) {
    unlink(target, recursive = TRUE, force = TRUE)
  }
  .tabwin_clear_cache()
  invisible(root)
}

#' Extract download provenance
#'
#' @param x An object returned by fetch_datasus().
#'
#' @return A tibble with one row per successfully read file, or NULL when
#' provenance was not requested.
#'
#' @export
datasus_provenance <- function(x) {
  attr(x, "microdatasus_provenance", exact = TRUE)
}
