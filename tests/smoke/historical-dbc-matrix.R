# Live, opt-in audit of old, middle, and recent DBC files for every registered
# information system. This script is intentionally not part of R CMD check.
library(microdatasus)

cache_dir <- Sys.getenv(
  "MICRODATASUS_DBC_MATRIX_CACHE",
  file.path(tempdir(), "microdatasus-dbc-matrix")
)
result_file <- Sys.getenv(
  "MICRODATASUS_DBC_MATRIX_RESULTS",
  file.path(cache_dir, "results.csv")
)
timeout <- as.numeric(Sys.getenv("MICRODATASUS_DBC_MATRIX_TIMEOUT", "600"))
full_limit <- as.numeric(Sys.getenv(
  "MICRODATASUS_DBC_MATRIX_FULL_LIMIT",
  as.character(64 * 1024^2)
))
foreign_limit <- as.numeric(Sys.getenv(
  "MICRODATASUS_DBC_MATRIX_FOREIGN_LIMIT",
  as.character(32 * 1024^2)
))
batch_columns <- as.integer(Sys.getenv(
  "MICRODATASUS_DBC_MATRIX_BATCH_COLUMNS",
  "16"
))
requested <- Sys.getenv("MICRODATASUS_DBC_MATRIX_SYSTEMS", "")
requested <- if (nzchar(requested)) {
  trimws(strsplit(requested, ",", fixed = TRUE)[[1L]])
} else {
  character()
}

dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(result_file), recursive = TRUE, showWarnings = FALSE)
stopifnot(
  is.finite(timeout), timeout > 0,
  is.finite(full_limit), full_limit > 0,
  is.finite(foreign_limit), foreign_limit >= 0,
  !is.na(batch_columns), batch_columns > 0L
)

registry <- microdatasus:::.datasus_registry()
systems <- names(registry)
if (length(requested)) {
  unknown <- setdiff(requested, systems)
  if (length(unknown)) stop("Unknown systems: ", paste(unknown, collapse = ", "))
  systems <- requested
}

listing_cache <- new.env(parent = emptyenv())
results <- list()

write_results <- function() {
  if (!length(results)) return(invisible(NULL))
  value <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
  utils::write.csv(as.data.frame(value), result_file, row.names = FALSE)
  invisible(value)
}

add_result <- function(...) {
  results[[length(results) + 1L]] <<- data.frame(
    ...,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  write_results()
}

on.exit(write_results(), add = TRUE)

cached_listing <- function(url) {
  if (!exists(url, envir = listing_cache, inherits = FALSE)) {
    message("Listing ", url)
    value <- microdatasus:::.datasus_list_directory(url, timeout)
    assign(url, value, envir = listing_cache)
  }
  get(url, envir = listing_cache, inherits = FALSE)
}

chronological_period <- function(period, granularity) {
  if (identical(granularity, "year")) return(as.integer(period))
  year <- as.integer(substr(period, 1L, 2L))
  month <- as.integer(substr(period, 3L, 4L))
  year <- ifelse(year >= 90L, 1900L + year, 2000L + year)
  year * 100L + month
}

system_manifest <- function(system) {
  spec <- registry[[system]]
  manifests <- list()
  for (index in seq_along(spec$repositories)) {
    repository <- spec$repositories[[index]]
    parsed <- microdatasus:::.datasus_parse_listing(
      cached_listing(repository$url),
      repository,
      spec
    )
    if (nrow(parsed)) {
      parsed$repository_order <- index
      manifests[[length(manifests) + 1L]] <- parsed
    }
  }
  if (!length(manifests)) return(data.frame())

  manifest <- as.data.frame(data.table::rbindlist(
    manifests,
    use.names = TRUE,
    fill = TRUE
  ))
  if (identical(spec$geography, "state")) {
    manifest <- manifest[manifest$uf == "AC", , drop = FALSE]
  }
  if (!nrow(manifest)) return(manifest)

  precedence <- order(
    manifest$priority,
    manifest$repository_order,
    manifest$file
  )
  manifest <- manifest[precedence, , drop = FALSE]
  identity <- paste(manifest$period, manifest$uf, manifest$fragment, sep = "\r")
  manifest <- manifest[!duplicated(identity), , drop = FALSE]
  manifest$chronological <- chronological_period(
    manifest$period,
    spec$granularity
  )
  manifest <- manifest[
    order(manifest$chronological, manifest$fragment, manifest$file),
    ,
    drop = FALSE
  ]
  rownames(manifest) <- NULL
  manifest
}

select_eras <- function(manifest) {
  periods <- unique(manifest$chronological)
  if (!length(periods)) return(manifest[FALSE, , drop = FALSE])
  if (length(periods) == 1L) {
    positions <- 1L
    era_names <- "only"
  } else if (length(periods) == 2L) {
    positions <- c(1L, 2L)
    era_names <- c("old", "recent")
  } else {
    positions <- c(1L, ceiling(length(periods) / 2), length(periods))
    era_names <- c("old", "middle", "recent")
  }
  chosen <- lapply(seq_along(positions), function(index) {
    candidates <- manifest[
      manifest$chronological == periods[[positions[[index]]]],
      ,
      drop = FALSE
    ]
    candidates <- candidates[order(candidates$fragment, candidates$file), , drop = FALSE]
    candidates <- candidates[1L, , drop = FALSE]
    candidates$era <- era_names[[index]]
    candidates
  })
  as.data.frame(data.table::rbindlist(chosen, use.names = TRUE, fill = TRUE))
}

download_file <- function(url, destination) {
  microdatasus:::.datasus_retry(
    function() {
      microdatasus:::.datasus_transfer_file(
        url,
        destination,
        timeout,
        quiet = TRUE
      )
    },
    retry_if = microdatasus:::.datasus_is_transient_curl_error
  )
  destination
}

capture_warnings <- function(expression, warnings) {
  withCallingHandlers(
    expression,
    warning = function(condition) {
      warnings$value <- c(warnings$value, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
}

compare_columns <- function(direct, legacy, types, source_encoding) {
  for (index in seq_along(direct)) {
    legacy_value <- legacy[[index]]
    if (identical(types[[index]], "C")) {
      encoding_used <- attr(direct, "dbc_column_encodings")[[index]]
      if (identical(encoding_used, "bytes") ||
          startsWith(encoding_used, "mixed:")) {
        next
      }
      legacy_value <- iconv(
        legacy_value,
        from = encoding_used,
        to = "UTF-8",
        sub = NA
      )
      if (any(!is.na(legacy[[index]]) & is.na(legacy_value))) {
        stop("foreign reference contains undecodable character bytes")
      }
    }
    legacy_value <- as.character(legacy_value)
    if (!identical(direct[[index]], legacy_value)) {
      different <- which(
        is.na(direct[[index]]) != is.na(legacy_value) |
          (!is.na(direct[[index]]) & !is.na(legacy_value) &
            direct[[index]] != legacy_value)
      )
      row <- if (length(different)) different[[1L]] else NA_integer_
      stop(
        "foreign comparison differs in field ", names(direct)[[index]],
        if (!is.na(row)) paste0(" at record ", row) else ""
      )
    }
  }
  invisible(TRUE)
}

audit_file <- function(system, target) {
  spec <- registry[[system]]
  safe_system <- gsub("[^A-Za-z0-9_-]", "_", system)
  path <- file.path(
    cache_dir,
    paste0(safe_system, "-", target$era, "-", target$file)
  )
  warnings <- new.env(parent = emptyenv())
  warnings$value <- character()
  started <- proc.time()[["elapsed"]]
  foreign_checked <- FALSE
  dbf <- tempfile("microdatasus-dbc-matrix-", fileext = ".dbf")
  on.exit(unlink(dbf), add = TRUE)

  if (!file.exists(path) || is.na(file.size(path)) || file.size(path) == 0) {
    download_file(target$url, path)
  }

  info <- .Call(microdatasus:::microdatasus_dbc_info, normalizePath(path))
  header <- info$header
  header_size <- length(header)
  record_size <- sum(as.integer(header[11:12]) * c(1L, 256L))
  expected_size <- header_size + as.numeric(info$rows) * record_size
  source_encoding <- capture_warnings(
    microdatasus:::.dbc_resolve_encoding("auto", info$language_driver),
    warnings
  )
  decoded_names <- microdatasus:::.dbc_decode_text(
    info$names,
    source_encoding,
    "DBF field names",
    path
  )
  repaired_names <- make.names(decoded_names, unique = TRUE)

  groups <- if (expected_size <= full_limit) {
    list(seq_along(repaired_names))
  } else {
    # Bound the vector payload of a batch to roughly 128 MiB. Character
    # contents add overhead, so the user-configured column cap still applies.
    row_bytes <- max(1, as.numeric(info$rows)) * 8
    memory_columns <- max(1L, floor((128 * 1024^2) / row_bytes))
    effective_columns <- min(batch_columns, memory_columns)
    split(
      seq_along(repaired_names),
      ceiling(seq_along(repaired_names) / effective_columns)
    )
  }
  for (indices in groups) {
    value <- capture_warnings(
      read_dbc(
        path,
        as_character = FALSE,
        vars = repaired_names[indices]
      ),
      warnings
    )
    stopifnot(
      nrow(value) == info$rows,
      identical(names(value), repaired_names[indices])
    )
    rm(value)
    gc(FALSE)
  }

  microdatasus:::.dbc2dbf(path, dbf)
  actual_dbf_size <- file.size(dbf)
  if (!actual_dbf_size %in% c(expected_size, expected_size + 1)) {
    stop("decompressed DBF size contradicts its header")
  }
  if (actual_dbf_size == expected_size + 1) {
    connection <- file(dbf, "rb")
    seek(connection, where = expected_size, origin = "start")
    marker <- readBin(connection, "raw", n = 1L)
    close(connection)
    if (!identical(marker, as.raw(26L))) {
      stop("unexpected trailing byte in decompressed DBF")
    }
  }

  header_extension <- header_size - (33L + 32L * length(info$names))
  foreign_safe <- header_extension <= 1L
  if (expected_size <= foreign_limit && foreign_safe) {
    indices <- unique(c(
      match(unique(info$data_types), info$data_types),
      length(info$names)
    ))
    indices <- indices[!is.na(indices)]
    direct <- capture_warnings(
      read_dbc(
        path,
        as_character = TRUE,
        vars = repaired_names[indices]
      ),
      warnings
    )
    legacy <- foreign::read.dbf(dbf, as.is = TRUE)[indices]
    compare_columns(
      direct,
      legacy,
      info$data_types[indices],
      source_encoding
    )
    foreign_checked <- TRUE
  }

  list(
    compressed_bytes = file.size(path),
    decompressed_bytes = actual_dbf_size,
    rows = info$rows,
    fields = length(info$names),
    language_driver = info$language_driver,
    encoding = source_encoding,
    terminator = sprintf("0x%02x", as.integer(tail(header, 1L))),
    batches = length(groups),
    foreign_checked = foreign_checked,
    warnings = paste(unique(warnings$value), collapse = " | "),
    elapsed_seconds = proc.time()[["elapsed"]] - started
  )
}

targets <- list()
for (system in systems) {
  manifest <- tryCatch(system_manifest(system), error = identity)
  if (inherits(manifest, "error")) {
    add_result(
      information_system = system,
      era = NA_character_, period = NA_character_, file = NA_character_,
      url = NA_character_, status = "discovery_error",
      message = conditionMessage(manifest)
    )
    next
  }
  selected <- select_eras(manifest)
  if (!nrow(selected)) {
    add_result(
      information_system = system,
      era = NA_character_, period = NA_character_, file = NA_character_,
      url = NA_character_, status = "unavailable",
      message = "No published DBC matched the registry entry"
    )
    next
  }
  targets[[system]] <- selected
}

target_count <- sum(vapply(targets, nrow, integer(1)))
message("Auditing ", target_count, " DBC files across ", length(targets), " systems")
completed <- 0L
for (system in names(targets)) {
  selected <- targets[[system]]
  for (index in seq_len(nrow(selected))) {
    target <- selected[index, , drop = FALSE]
    completed <- completed + 1L
    message(
      sprintf(
        "[%d/%d] %s %s %s",
        completed,
        target_count,
        system,
        target$era,
        target$file
      )
    )
    outcome <- tryCatch(audit_file(system, target), error = identity)
    if (inherits(outcome, "error")) {
      add_result(
        information_system = system,
        era = target$era,
        period = target$period,
        file = target$file,
        url = target$url,
        status = "error",
        message = conditionMessage(outcome)
      )
      message("ERROR: ", conditionMessage(outcome))
    } else {
      add_result(
        information_system = system,
        era = target$era,
        period = target$period,
        file = target$file,
        url = target$url,
        status = "ok",
        message = "",
        compressed_bytes = outcome$compressed_bytes,
        decompressed_bytes = outcome$decompressed_bytes,
        rows = outcome$rows,
        fields = outcome$fields,
        language_driver = outcome$language_driver,
        encoding = outcome$encoding,
        terminator = outcome$terminator,
        batches = outcome$batches,
        foreign_checked = outcome$foreign_checked,
        warnings = outcome$warnings,
        elapsed_seconds = outcome$elapsed_seconds
      )
    }
  }
}

summary <- write_results()
failures <- summary$status %in% c("error", "discovery_error")
message(
  "Completed: ", sum(summary$status == "ok"), " passed; ",
  sum(summary$status == "unavailable"), " unavailable; ", sum(failures), " failed; results: ",
  result_file
)
if (any(failures)) stop("Historical DBC matrix contains failures.")
