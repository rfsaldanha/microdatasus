# Controlled multipart download/aggregation tests for every registered system.
# The network boundary is replaced with local DBC fixtures. The real filename
# parser, manifest selection, DBC reader, cache, and public fetch API are used.
# Rscript tests/stress/multipart-systems.R <intensive-run-directory>

multipart_controlled <- function(root) {
  config <- readRDS(file.path(root, "config.rds"))
  .libPaths(c(config$library, .libPaths()))
  library(microdatasus)
  registry <- microdatasus:::.datasus_registry()
  part_counts <- as.integer(strsplit(Sys.getenv("MICRODATASUS_MULTIPART_COUNTS", "2,3,4,32,100"), ",", fixed = TRUE)[[1L]])
  stopifnot(length(part_counts) > 0L, !anyNA(part_counts), all(part_counts >= 2L))
  fixture <- file.path(root, "source/tests/fixtures/historical/RDAC9801/input.dbc")
  reference <- read_dbc(fixture)
  results <- list()
  test_one <- function(system, parts) {
    spec <- registry[[system]]
    repository <- spec$repositories[[1L]]
    national <- spec$geography == "national"
    period <- if (spec$granularity == "month") "2201" else if (!is.null(spec$year_digits) && spec$year_digits == 2L) "22" else "2022"
    prefix <- paste0(repository$prefix, if (national) "" else "AC", period)
    suffixes <- paste0("part", formatC(seq_len(parts), width = nchar(parts), flag = "0", format = "d"))
    files <- paste0(prefix, suffixes, c(".dbc", ".DBC")[1L + seq_len(parts) %% 2L])
    directory <- tempfile("multipart-control-", tmpdir = config$tmp)
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    downloaded <- character()
    args <- list(year_start = 2022, year_end = 2022,
      uf = if (national) "all" else "AC", information_system = system,
      cache_dir = file.path(directory, "cache"), quiet = TRUE,
      track_source = TRUE, provenance = TRUE, stop_on_error = TRUE)
    if (spec$granularity == "month") args <- c(args, list(month_start = 1, month_end = 1))
    testthat::with_mocked_bindings({
      # Reversed listing and duplicate entries also exercise deterministic
      # ordering and publication deduplication across current/old repositories.
      value <- do.call(fetch_datasus, args)
      stopifnot(nrow(value) == nrow(reference) * parts,
        identical(value$source, rep(files, each = nrow(reference))),
        identical(names(value), c(names(reference), "source")))
      for (field in names(reference)) stopifnot(identical(value[[field]], rep(reference[[field]], parts)))
      p <- datasus_provenance(value)
      stopifnot(nrow(p) == parts, all(!p$cached), all(p$rows == nrow(reference)),
        identical(downloaded, paste0(repository$url, files)),
        identical(p$file, files))
      cached <- do.call(fetch_datasus, args)
      stopifnot(length(downloaded) == parts, all(datasus_provenance(cached)$cached))
      for (field in names(value)) stopifnot(identical(value[[field]], cached[[field]]))
      written <- do.call(fetch_datasus, c(args, list(collect = FALSE, destination = file.path(directory, "output"))))
      stopifnot(nrow(written) == parts, all(written$cached), all(written$rows == nrow(reference)))
      for (i in seq_len(parts)) {
        saved <- readRDS(written$data_path[[i]])
        stopifnot(nrow(saved) == nrow(reference), all(saved$source == files[[i]]))
        for (field in names(reference)) stopifnot(identical(saved[[field]], reference[[field]]))
      }
      TRUE
    },
    .datasus_list_directory = function(url, timeout) paste(c(rev(files), files[[1L]]), collapse = "\n"),
    .datasus_download_file = function(url, destination, ...) {
      downloaded <<- c(downloaded, url)
      stopifnot(file.copy(fixture, destination, overwrite = TRUE))
      invisible(destination)
    }, .package = "microdatasus")
  }
  for (system in names(registry)) for (parts in part_counts) {
    start <- proc.time()[["elapsed"]]
    error <- tryCatch({ test_one(system, parts); NULL }, error = identity)
    results[[length(results) + 1L]] <- data.frame(information_system = system,
      parts = parts, status = if (is.null(error)) "ok" else "fail",
      seconds = proc.time()[["elapsed"]] - start,
      message = if (is.null(error)) "" else conditionMessage(error))
  }
  result <- do.call(rbind, results)
  write.csv(result, file.path(root, "multipart-controlled.csv"), row.names = FALSE)
  cat(nrow(result), "controlled scenarios across", length(registry), "identifiers:",
      sum(result$status == "ok"), "passed,", sum(result$status == "fail"), "failed.\n")
  invisible(result)
}

if (sys.nframe() == 0L) multipart_controlled(normalizePath(commandArgs(TRUE)[[1L]]))
