# Opt-in live audit. Run from the repository root:
# Rscript tests/stress/live-intensive.R
# Artifacts are kept under .cache/intensive/<UTC timestamp>.
# This script installs the pinned source into an isolated library; it never
# changes package implementation or runs as part of R CMD check.

audit_commit <- "c1ad20b2034df021e3eb8105455efb72b8cc5aa5"
audit_families <- c("SIM", "SINASC", "SIH", "SIA", "CNES", "SINAN")

audit_save <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  saveRDS(value, temporary)
  stopifnot(file.rename(temporary, path))
}

audit_csv <- function(value, path) {
  utils::write.csv(value, path, row.names = FALSE, na = "")
}

audit_rows <- function(values) {
  if (!length(values)) return(data.frame())
  as.data.frame(data.table::rbindlist(values, fill = TRUE, use.names = TRUE))
}

audit_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
  invisible(TRUE)
}

# Compare every column, including types, factor levels, and column attributes.
# Only table-level execution metadata and row names are outside the comparison.
audit_equal <- function(actual, expected) {
  audit_assert(is.data.frame(actual) && is.data.frame(expected), "Expected two data frames")
  audit_assert(nrow(actual) == nrow(expected), paste("Row count:", nrow(actual), "!=", nrow(expected)))
  audit_assert(identical(names(actual), names(expected)), paste(
    "Column names/order differ; actual-only:", paste(setdiff(names(actual), names(expected)), collapse = ","),
    "expected-only:", paste(setdiff(names(expected), names(actual)), collapse = ",")
  ))
  differences <- names(actual)[!vapply(names(actual), function(field) {
    identical(actual[[field]], expected[[field]])
  }, logical(1))]
  if (length(differences)) {
    detail <- vapply(head(differences, 12L), function(field) {
      a <- actual[[field]]; b <- expected[[field]]
      paste0(field, " [", paste(class(a), collapse = "/"), " vs ",
             paste(class(b), collapse = "/"), "]: ",
             paste(head(all.equal(a, b, tolerance = 0), 2L), collapse = "; "))
    }, character(1))
    stop(paste(c(paste(length(differences), "columns differ"), detail), collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

audit_bind <- function(values) {
  # Independent of fetch_datasus(), which uses data.table::rbindlist().
  dplyr::bind_rows(lapply(values, function(x) {
    attributes(x) <- list(names = names(x), row.names = .set_row_names(nrow(x)), class = "data.frame")
    x
  }))
}

audit_period_date <- function(period, granularity) {
  if (granularity == "year") return(as.Date(paste0(period, "-01-01")))
  year <- as.integer(substr(period, 1L, 2L))
  year <- ifelse(year >= 90L, 1900L + year, 2000L + year)
  as.Date(sprintf("%04d-%s-01", year, substr(period, 3L, 4L)))
}

audit_args <- function(rows, config) {
  monthly <- rows$granularity[[1L]] == "month"
  first <- min(as.Date(rows$date)); last <- max(as.Date(rows$date))
  args <- list(
    year_start = as.integer(format(first, "%Y")),
    year_end = as.integer(format(last, "%Y")),
    uf = if (all(is.na(rows$uf))) "all" else sort(unique(rows$uf)),
    information_system = rows$information_system[[1L]],
    timeout = 60, stop_on_error = TRUE, quiet = TRUE,
    track_source = TRUE, provenance = TRUE, cache_dir = config$cache
  )
  if (monthly) {
    args$month_start <- as.integer(format(first, "%m"))
    args$month_end <- as.integer(format(last, "%m"))
  }
  args
}

audit_expected <- function(args, catalog) {
  catalog <- catalog[catalog$information_system == args$information_system, , drop = FALSE]
  first <- as.Date(sprintf("%04d-%02d-01", args$year_start,
                          if (is.null(args$month_start)) 1L else args$month_start))
  last <- as.Date(sprintf("%04d-%02d-01", args$year_end,
                         if (is.null(args$month_end)) 1L else args$month_end))
  keep <- catalog$date >= as.character(first) & catalog$date <= as.character(last)
  if (!identical(args$uf, "all")) keep <- keep & catalog$uf %in% args$uf
  rows <- catalog[keep, , drop = FALSE]
  rows[order(rows$date, match(rows$uf, args$uf), rows$fragment, rows$file), , drop = FALSE]
}

audit_discover <- function(config) {
  registry <- microdatasus:::.datasus_registry()
  listings <- new.env(parent = emptyenv())
  failures <- list(); catalog <- list()
  dir.create(file.path(config$root, "listings"), showWarnings = FALSE)
  for (system in names(registry)) {
    spec <- registry[[system]]
    entries <- list()
    for (i in seq_along(spec$repositories)) {
      repository <- spec$repositories[[i]]
      url <- repository$url
      if (!exists(url, listings, inherits = FALSE)) {
        message("Discovering ", url)
        value <- tryCatch(microdatasus:::.datasus_list_directory(url, 60), error = identity)
        assign(url, value, listings)
        if (inherits(value, "error")) {
          failures[[length(failures) + 1L]] <- data.frame(url = url, error = conditionMessage(value))
        } else {
          writeLines(value, file.path(config$root, "listings", paste0(digest::digest(url), ".txt")))
        }
      }
      listing <- get(url, listings)
      if (inherits(listing, "error")) next
      parsed <- microdatasus:::.datasus_parse_listing(listing, repository, spec)
      if (!nrow(parsed)) next
      parsed$repository_order <- i
      entries[[length(entries) + 1L]] <- parsed
    }
    if (!length(entries)) next
    rows <- audit_rows(entries)
    if (spec$geography == "state") rows <- rows[rows$uf %in% microdatasus:::.datasus_ufs, ]
    rows <- rows[order(rows$priority, rows$repository_order, rows$file), ]
    rows <- rows[!duplicated(paste(rows$period, rows$uf, rows$fragment)), ]
    rows$date <- as.character(audit_period_date(rows$period, spec$granularity))
    rows <- rows[as.Date(rows$date) >= spec$minimum & as.Date(rows$date) <= Sys.Date(), ]
    rows$information_system <- system
    rows$family <- sub("-.*$", "", system)
    rows$granularity <- spec$granularity
    periods <- sort(unique(rows$date))
    rows$era <- c("old", "middle", "recent")[pmin(3L, ceiling(3L * match(rows$date, periods) / length(periods)))]
    rows$key <- paste(system, rows$file, sep = "/")
    rows$unit <- paste(system, rows$period, ifelse(is.na(rows$uf), "BR", rows$uf), sep = "/")
    catalog[[length(catalog) + 1L]] <- rows
  }
  result <- audit_rows(catalog)
  audit_save(result, file.path(config$root, "catalog.rds"))
  audit_csv(result, file.path(config$root, "catalog.csv"))
  audit_csv(audit_rows(failures), file.path(config$root, "discovery-failures.csv"))
  metadata <- as.data.frame(microdatasus::datasus_information_systems())
  metadata$aliases <- vapply(metadata$aliases, paste, character(1), collapse = ";")
  metadata$available_files <- vapply(metadata$information_system, function(s) sum(result$information_system == s), integer(1))
  audit_csv(metadata, file.path(config$root, "systems.csv"))
  invisible(result)
}

audit_select <- function(config) {
  set.seed(config$seed)
  catalog <- readRDS(file.path(config$root, "catalog.rds"))
  catalog <- catalog[is.na(catalog$uf) | catalog$uf %in% microdatasus:::.datasus_ufs, ]
  available <- unique(catalog$information_system)
  sinan <- sort(available[startsWith(available, "SINAN-")])
  chosen_systems <- c(sort(available[!startsWith(available, "SINAN-")]), sample(sinan, min(20L, length(sinan))))
  pool <- catalog[catalog$information_system %in% chosen_systems, ]
  selected <- character(); reasons <- list(); groups <- list(); expectations <- list()
  add <- function(rows, reason) {
    selected <<- unique(c(selected, rows$key))
    for (key in rows$key) reasons[[key]] <<- unique(c(reasons[[key]], reason))
  }
  add_group <- function(rows, reason, extended = FALSE) {
    args <- audit_args(rows, config)
    expanded <- audit_expected(args, catalog)
    if (nrow(expanded) < 2L || nrow(expanded) > 4L) return(FALSE)
    if (any(vapply(groups, function(g) identical(sort(g$expected$key), sort(expanded$key)), logical(1)))) return(FALSE)
    id <- sprintf("group-%02d", length(groups) + 1L)
    groups[[length(groups) + 1L]] <<- list(id = id, kind = "group", family = rows$family[[1L]],
      args = args, expected = expanded, reason = reason, extended = extended)
    add(expanded, paste0(id, ":", reason))
    TRUE
  }
  # The exact 16 cases and dictionary expectations already covered by the
  # existing historical-layouts.R smoke script, now with complete files.
  boundaries <- list(
    list("SINASC", "1995-01-01", "1996-01-01", "SINASC-1994-1995", "SINASC"),
    list("SIH-RD", "1997-12-01", "1998-01-01", "SIH-RD-1992-1997", "SIH-RD-1998-2003-07"),
    list("SIH-RD", "2003-07-01", "2003-08-01", "SIH-RD-1998-2003-07", "SIH-RD-2003-08-2007"),
    list("SIH-RD", "2007-12-01", "2008-01-01", "SIH-RD-2003-08-2007", "SIH-RD"),
    list("SIA-PA", "1999-10-01", "1999-11-01", "SIA-PA-1994-07-1999-10", "SIA-PA-1999-11-2003-07"),
    list("SIA-PA", "2003-07-01", "2003-08-01", "SIA-PA-1999-11-2003-07", "SIA-PA-2003-08-2007"),
    list("SIA-PA", "2007-12-01", "2008-01-01", "SIA-PA-2003-08-2007", "SIA-PA"),
    list("CNES-SR", "2008-02-01", "2008-03-01", "CNES-SR-2005-08-2008-02", "CNES-SR")
  )
  for (b in boundaries) {
    rows <- pool[pool$information_system == b[[1L]] & pool$date %in% c(b[[2L]], b[[3L]]) & pool$uf %in% "AC", ]
    if (length(unique(rows$date)) == 2L) add_group(rows, "layout_transition")
    add(rows, "directed_layout")
    for (j in 2:3) for (key in rows$key[rows$date == b[[j]]]) expectations[[key]] <- b[[j + 2L]]
  }
  # Three real multi-file requests per family. Alternate adjacent periods and
  # multiple states; national systems necessarily aggregate adjacent years.
  for (family in audit_families) {
    attempts <- 0L
    repeat {
      count <- sum(vapply(groups, function(g) g$family == family, logical(1)))
      if (count >= 3L || attempts >= 1000L) break
      attempts <- attempts + 1L
      systems <- chosen_systems[sub("-.*$", "", chosen_systems) == family]
      system <- sample(systems, 1L)
      entries <- pool[pool$information_system == system, ]
      units <- entries[!duplicated(entries$unit), ]
      if (nrow(units) < 2L) next
      anchor <- units[sample.int(nrow(units), 1L), ]
      if (count %% 2L == 1L && !is.na(anchor$uf)) {
        partners <- units[units$date == anchor$date & units$uf != anchor$uf, ]
        if (!nrow(partners)) next
        rows <- rbind(anchor, partners[sample.int(nrow(partners), 1L), ])
      } else {
        partners <- units[if (is.na(anchor$uf)) is.na(units$uf) else units$uf == anchor$uf, ]
        partners <- partners[order(partners$date), ]
        pos <- match(anchor$unit, partners$unit)
        other <- if (pos < nrow(partners)) pos + 1L else pos - 1L
        if (other < 1L) next
        rows <- rbind(anchor, partners[other, ])
        # Do not request absent months/years between sampled neighbors.
        dates <- seq(as.Date(min(rows$date)), as.Date(max(rows$date)), by = anchor$granularity)
        if (length(dates) != 2L) next
      }
      add_group(rows, if (length(unique(rows$date)) == 1L) "multiple_states" else "multiple_periods")
    }
  }
  for (family in audit_families) {
    indexes <- which(vapply(groups, function(g) g$family == family, logical(1)))
    if (length(indexes)) groups[[indexes[[1L]]]]$extended <- TRUE
  }
  # One random published unit per selected identifier before balancing eras.
  for (system in chosen_systems) {
    if (any(pool$key[pool$information_system == system] %in% selected)) next
    entries <- pool[pool$information_system == system, ]
    unit <- sample(unique(entries$unit), 1L)
    add(entries[entries$unit == unit, ], "identifier_coverage")
  }
  for (family in audit_families) {
    repeat {
      current <- pool[pool$key %in% selected & pool$family == family, ]
      if (nrow(current) >= 25L) break
      counts <- table(factor(current$era, levels = c("old", "middle", "recent")))
      era <- names(counts)[which.min(counts)]
      entries <- pool[pool$family == family & pool$era == era & !pool$key %in% selected, ]
      if (!nrow(entries)) entries <- pool[pool$family == family & !pool$key %in% selected, ]
      if (!nrow(entries)) break
      unit <- sample(unique(entries$unit), 1L)
      add(pool[pool$unit == unit, ], "random_stratified")
    }
  }
  # Complete years, UFs/regions, and months while keeping the 180-file ceiling.
  for (iteration in seq_len(100L)) {
    current <- pool[pool$key %in% selected, ]
    entries <- pool[!pool$key %in% selected, ]
    if (nrow(current) >= 180L || !nrow(entries)) break
    candidates <- entries[FALSE, ]
    for (family in audit_families) {
      years <- unique(substr(current$date[current$family == family], 1L, 4L))
      if (length(years) < 5L) {
        candidates <- entries[entries$family == family & !substr(entries$date, 1L, 4L) %in% years, ]
        if (nrow(candidates)) break
      }
    }
    if (!nrow(candidates)) {
      months <- substr(current$date[current$granularity == "month"], 6L, 7L)
      candidates <- entries[entries$granularity == "month" & !substr(entries$date, 6L, 7L) %in% months, ]
    }
    regions <- list(N = c("AC", "AM", "AP", "PA", "RO", "RR", "TO"), NE = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
                    CO = c("DF", "GO", "MT", "MS"), SE = c("ES", "MG", "RJ", "SP"), S = c("PR", "RS", "SC"))
    if (!nrow(candidates)) {
      missing_regions <- Filter(function(ufs) !any(current$uf %in% ufs), regions)
      if (length(missing_regions)) candidates <- entries[entries$uf %in% unlist(missing_regions), ]
    }
    if (!nrow(candidates) && length(unique(na.omit(current$uf))) < 10L) candidates <- entries[!is.na(entries$uf) & !entries$uf %in% current$uf, ]
    if (!nrow(candidates)) break
    unit <- sample(unique(candidates$unit), 1L)
    rows <- pool[pool$unit == unit, ]
    if (length(unique(c(selected, rows$key))) <= 180L) add(rows, "coverage_extension")
  }
  manifest <- pool[match(selected, pool$key), ]
  manifest$reason <- vapply(manifest$key, function(k) paste(reasons[[k]], collapse = ";"), character(1))
  manifest$expected_dictionary <- vapply(manifest$key, function(k) if (is.null(expectations[[k]])) "" else expectations[[k]], character(1))
  for (i in seq_along(groups)) groups[[i]]$expected$expected_dictionary <-
    manifest$expected_dictionary[match(groups[[i]]$expected$key, manifest$key)]
  units <- unique(manifest$unit)
  cases <- lapply(seq_along(units), function(i) {
    rows <- manifest[manifest$unit == units[[i]], ]
    args <- audit_args(rows, config)
    expected <- audit_expected(args, catalog)
    audit_assert(setequal(expected$key, rows$key), "Individual request expands beyond the sample")
    list(id = sprintf("file-%03d", i), kind = "file", family = rows$family[[1L]],
         args = args, expected = rows, extended = FALSE)
  })
  # Round-robin families so an early resource limit does not exclude a family.
  order_within <- ave(seq_along(cases), vapply(cases, `[[`, character(1), "family"), FUN = seq_along)
  cases <- cases[order(order_within, match(vapply(cases, `[[`, character(1), "family"), audit_families))]
  plan <- list(files = cases, groups = groups, selected_systems = chosen_systems)
  audit_save(plan, file.path(config$root, "plan.rds"))
  audit_csv(manifest, file.path(config$root, "sample.csv"))
  audit_csv(audit_rows(lapply(groups, function(g) data.frame(id = g$id, family = g$family,
    information_system = g$args$information_system, files = paste(g$expected$file, collapse = ";"),
    reason = g$reason, extended = g$extended))), file.path(config$root, "groups.csv"))
  message("Selected ", nrow(manifest), " unique files, ", length(cases), " file requests, ", length(groups), " aggregation groups")
  plan
}

audit_processor <- function(data, system, labels = "factor") {
  family <- sub("-.*$", "", system)
  fun <- getExportedValue("microdatasus", paste0("process_", tolower(family)))
  args <- list(data = data, labels = labels, diagnostics = TRUE)
  if (family != "SINASC") args$information_system <- system
  do.call(fun, args)
}

audit_cached_path <- function(row, config) {
  component <- microdatasus:::.datasus_cache_component
  file.path(config$cache, "dbc", component(row$information_system), component(row$file), "data.dbc")
}

audit_case <- function(config, case) {
  case_dir <- file.path(config$root, "cases", case$id)
  dir.create(case_dir, recursive = TRUE, showWarnings = FALSE)
  options(microdatasus.cache_dir = config$cache, timeout = 60)
  data.table::setDTthreads(1L)
  state <- list(id = case$id, kind = case$kind, family = case$family,
                information_system = case$args$information_system,
                events = list(), warnings = list(), metrics = list(),
                started_at = Sys.time(), finished_at = NULL, status = "running")
  active_stage <- "starting"
  checkpoint <- function() {
    state$active_stage <<- active_stage
    audit_save(state, file.path(case_dir, "state.rds"))
    audit_csv(audit_rows(state$events), file.path(case_dir, "checks.csv"))
  }
  check <- function(stage, expression) {
    active_stage <<- stage
    checkpoint()
    start <- proc.time()[["elapsed"]]
    error <- NULL
    value <- tryCatch(withCallingHandlers(force(expression), warning = function(w) {
      state$warnings[[length(state$warnings) + 1L]] <<- data.frame(stage = stage, message = conditionMessage(w))
      invokeRestart("muffleWarning")
    }), error = function(e) { error <<- e; NULL })
    state$events[[length(state$events) + 1L]] <<- data.frame(
      case_id = case$id, stage = stage, status = if (is.null(error)) "ok" else "fail",
      seconds = proc.time()[["elapsed"]] - start,
      error_class = if (is.null(error)) "" else paste(class(error), collapse = ";"),
      message = if (is.null(error)) "" else conditionMessage(error))
    checkpoint()
    value
  }
  finish <- function() {
    state$finished_at <<- Sys.time()
    state$status <<- if (any(vapply(state$events, function(x) x$status == "fail", logical(1)))) "fail" else "ok"
    audit_csv(audit_rows(state$warnings), file.path(case_dir, "warnings.csv"))
    checkpoint()
    invisible(state)
  }
  checkpoint()
  ready <- check("discovery_preflight", {
    spec <- microdatasus:::.datasus_registry()[[case$args$information_system]]
    found <- microdatasus:::.datasus_build_manifest(spec, unique(case$expected$period),
      if (identical(case$args$uf, "all")) microdatasus:::.datasus_ufs else case$args$uf, 60)
    audit_assert(!length(found$errors), paste(found$errors, collapse = "\n"))
    audit_assert(setequal(found$manifest$url, case$expected$url), "Remote publication changed since sampling")
    if (case$kind == "group") {
      paths <- vapply(seq_len(nrow(case$expected)), function(i) audit_cached_path(case$expected[i, ], config), character(1))
      audit_assert(all(file.exists(paths)), "Group requires a source file that did not download")
      audit_assert(all(vapply(paths, function(p) microdatasus:::.datasus_cache_valid(p, file.path(dirname(p), "manifest.rds")), logical(1))),
                   "Group requires a source file without a valid cache entry")
    }
    TRUE
  })
  if (is.null(ready)) return(finish())
  raw <- check("download_read_raw", do.call(microdatasus::fetch_datasus, case$args))
  if (is.null(raw)) {
    if (tail(state$events, 1L)[[1L]]$status == "ok") check("raw_nonnull", stop("fetch_datasus returned NULL", call. = FALSE))
    return(finish())
  }
  provenance <- microdatasus::datasus_provenance(raw)
  audit_save(provenance, file.path(case_dir, "raw-provenance.rds"))
  audit_csv(provenance, file.path(case_dir, "raw-provenance.csv"))
  state$metrics$raw_rows <- nrow(raw)
  state$metrics$raw_columns <- ncol(raw)
  state$metrics$bytes <- sum(provenance$size)
  check("raw_provenance", {
    audit_assert(setequal(provenance$url, case$expected$url), "Downloaded URLs differ from planned files")
    audit_assert(all(provenance$checksum_algorithm == "sha256"), "Missing SHA-256 provenance")
    audit_assert(sum(provenance$source_rows) == nrow(raw), "Source row counts do not sum to raw row count")
    if (case$kind == "group") audit_assert(all(provenance$cached), "Group redownloaded a raw file")
    TRUE
  })
  independent <- check("independent_read", {
    lapply(seq_len(nrow(provenance)), function(i) {
      data <- microdatasus::read_dbc(provenance$dbc_path[[i]])
      data$source <- provenance$file[[i]]
      audit_assert(nrow(data) == provenance$source_rows[[i]], "Individual row count differs from provenance")
      data
    })
  })
  if (is.null(independent)) return(finish())
  check("raw_aggregation_equal", audit_equal(raw, audit_bind(independent)))
  check("source_counts", {
    counts <- table(factor(raw$source, levels = provenance$file))
    audit_assert(identical(as.integer(counts), as.integer(provenance$source_rows)), "Per-source row counts differ")
    TRUE
  })
  dictionaries_ready <- check("dictionary_download", {
    keys <- unique(unlist(lapply(independent, function(x) microdatasus:::.datasus_contract_dictionary_keys(x, case$args$information_system))))
    if (case$family == "SIM") keys <- unique(c(keys, "SIM-DO-CID9"))
    if (case$family == "CNES") keys <- unique(c(keys, "CNES-ST"))
    if (case$args$information_system == "SINAN-CHIKUNGUNYA") keys <- unique(c(keys, "SINAN-FEBRE-TIFOIDE"))
    for (key in keys) microdatasus::fetch_tabwin_dictionary(key, timeout = 60, quiet = TRUE, cache_dir = config$cache)
    TRUE
  })
  if (is.null(dictionaries_ready)) return(finish())
  modes <- if (isTRUE(case$extended)) c("factor", "character", "none") else "factor"
  for (mode in modes) {
    tag <- paste0("_", mode)
    processed_parts <- check(paste0("process_individual", tag), {
      lapply(independent, audit_processor, system = case$args$information_system, labels = mode)
    })
    if (is.null(processed_parts)) next
    state$metrics$processed_rows <- sum(vapply(processed_parts, nrow, integer(1)))
    reports <- lapply(processed_parts, microdatasus::processing_diagnostics)
    audit_save(reports, file.path(case_dir, paste0("diagnostics", tag, ".rds")))
    check(paste0("processing_invariants", tag), {
      for (i in seq_along(processed_parts)) {
        audit_assert(nrow(processed_parts[[i]]) == nrow(independent[[i]]), "Processor changed the number of rows")
        audit_assert(identical(processed_parts[[i]]$source, independent[[i]]$source), "Processor changed source tracking")
        audit_assert(!is.null(reports[[i]]), "Missing processing diagnostics")
        date_fields <- switch(case$family,
          SIM = c("DTOBITO", "DTNASC"), SINASC = c("DTNASC", "DATA_NASC", "DATA_CART"),
          SIH = c("DT_INTER", "DT_SAIDA"), SINAN = c("DT_NOTIFIC", "DT_SIN_PRI", "DT_NASC"),
          character())
        for (field in intersect(date_fields, names(processed_parts[[i]]))) {
          audit_assert(inherits(processed_parts[[i]][[field]], "Date"), paste(field, "is not a Date"))
        }
        expected <- case$expected$expected_dictionary[match(provenance$file[[i]], case$expected$file)]
        if (length(expected) && !is.na(expected) && nzchar(expected)) {
          audit_assert(expected %in% reports[[i]]$dictionaries$information_system, paste("Wrong historical dictionary; expected", expected))
        }
      }
      TRUE
    })
    expected <- check(paste0("independent_bind", tag), audit_bind(processed_parts))
    rm(processed_parts); gc(FALSE)
    if (is.null(expected)) next
    state$metrics$processed_columns <- ncol(expected)
    types <- data.frame(field = names(expected), type = vapply(expected, function(x) paste(class(x), collapse = "/"), character(1)))
    audit_csv(types, file.path(case_dir, paste0("types", tag, ".csv")))
    args <- c(case$args, list(process = TRUE, process_args = list(labels = mode, diagnostics = TRUE)))
    integrated <- check(paste0("fetch_processed", tag), do.call(microdatasus::fetch_datasus, args))
    if (!is.null(integrated)) {
      state$metrics$integrated_rows <- nrow(integrated)
      check(paste0("processed_equal", tag), audit_equal(integrated, expected))
      check(paste0("cache_reuse", tag), {
        p <- microdatasus::datasus_provenance(integrated)
        audit_assert(all(p$cached), "Repeated fetch did not reuse raw cache")
        audit_assert(identical(p$checksum, provenance$checksum), "Source checksums changed during processing")
        TRUE
      })
      check(paste0("lockfile_integrity", tag), {
        lock <- microdatasus::datasus_lockfile(integrated)
        audit_save(lock, file.path(case_dir, paste0("lockfile", tag, ".rds")))
        verified <- microdatasus::verify_datasus_lockfile(lock)
        audit_csv(verified, file.path(case_dir, paste0("integrity", tag, ".csv")))
        audit_assert(nrow(lock$dictionaries) > 0L, "Lockfile is missing processing dictionaries")
        audit_assert(all(verified$status == "ok"), "Lockfile includes missing or changed components")
        TRUE
      })
      rm(integrated); gc(FALSE)
    }
    if (case$kind == "group" || isTRUE(case$aggregate)) {
      aggregate_first <- check(paste0("process_aggregated", tag), audit_processor(raw, case$args$information_system, mode))
      if (!is.null(aggregate_first)) {
        check(paste0("aggregate_then_process_equal", tag), audit_equal(aggregate_first, expected))
        rm(aggregate_first); gc(FALSE)
      }
    }
    if (isTRUE(case$extended)) {
      destination <- file.path(case_dir, paste0("outputs", tag))
      separate <- check(paste0("collect_false", tag), do.call(microdatasus::fetch_datasus,
        c(args, list(collect = FALSE, destination = destination))))
      if (!is.null(separate)) {
        check(paste0("collect_false_equal", tag), {
          audit_assert(all(separate$cached), "collect=FALSE redownloaded raw files")
          saved <- lapply(separate$data_path, readRDS)
          audit_equal(audit_bind(saved), expected)
        })
        audit_csv(separate, file.path(case_dir, paste0("collect_false", tag, ".csv")))
      }
      # RDS outputs are reproducible intermediates; retain DBC/dictionaries.
      unlink(destination, recursive = TRUE)
    }
    rm(expected); gc(FALSE)
  }
  finish()
}

audit_size <- function(directory) {
  files <- list.files(directory, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  sum(file.info(files)$size, na.rm = TRUE)
}

audit_supervise <- function(config, case = NULL, action = "case", limit = 600) {
  id <- if (is.null(case)) action else case$id
  directory <- file.path(config$root, "cases", id)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(case)) audit_save(case, file.path(directory, "case.rds"))
  args <- c(config$runner, "--worker", config$root, action, id)
  child <- processx::process$new(file.path(R.home("bin"), "Rscript"), args,
    stdout = file.path(directory, "stdout.log"), stderr = file.path(directory, "stderr.log"),
    cleanup_tree = TRUE, env = c("R_LIBS_USER" = config$library, "TMPDIR" = config$tmp))
  started <- Sys.time(); peak <- 0; reason <- NULL
  repeat {
    child$poll_io(1000)
    if (!child$is_alive()) break
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    rss <- tryCatch(unname(ps::ps_memory_info(ps::ps_handle(child$get_pid()))[["rss"]]), error = function(e) 0)
    peak <- max(peak, rss)
    size <- audit_size(config$root)
    if (elapsed >= limit) reason <- "case_timeout"
    if (rss > config$memory_limit) reason <- "memory_limit"
    if (size >= config$disk_limit) reason <- "disk_limit"
    if (Sys.time() >= config$deadline) reason <- "run_deadline"
    if (!is.null(reason)) {
      child$kill_tree(); child$wait(5000)
      break
    }
    Sys.sleep(1)
  }
  status_path <- file.path(directory, "state.rds")
  state <- if (file.exists(status_path)) readRDS(status_path) else list(id = id, events = list(), metrics = list())
  if (!is.null(reason) || child$get_exit_status() != 0L || identical(state$status, "running")) {
    message <- if (is.null(reason)) paste("Worker exited with status", child$get_exit_status()) else reason
    state$status <- if (is.null(reason)) "fail" else "resource_limit"
    state$events[[length(state$events) + 1L]] <- data.frame(case_id = id, stage = "supervisor", status = "fail",
      seconds = as.numeric(difftime(Sys.time(), started, units = "secs")), error_class = "audit_worker_limit", message = message)
  }
  state$peak_rss <- peak
  state$finished_at <- Sys.time()
  state$elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  audit_save(state, status_path)
  message(format(Sys.time(), "%H:%M:%S"), " ", id, " ", if (is.null(state$status)) child$get_exit_status() else state$status,
          " (", round(state$elapsed), "s; peak ", round(peak / 1024^2), " MiB)")
  state
}

audit_report <- function(config, plan) {
  cases <- c(plan$files, plan$groups)
  events <- list(); rows <- list(); anomalies <- list(); files <- list()
  for (case in cases) {
    directory <- file.path(config$root, "cases", case$id)
    state_path <- file.path(directory, "state.rds")
    state <- if (file.exists(state_path)) readRDS(state_path) else list(status = "not_run", metrics = list(), events = list())
    rows[[length(rows) + 1L]] <- data.frame(id = case$id, kind = case$kind, family = case$family,
      information_system = case$args$information_system, status = state$status,
      files = nrow(case$expected), raw_rows = if (is.null(state$metrics$raw_rows)) NA else state$metrics$raw_rows,
      processed_rows = if (is.null(state$metrics$processed_rows)) NA else state$metrics$processed_rows,
      integrated_rows = if (is.null(state$metrics$integrated_rows)) NA else state$metrics$integrated_rows,
      seconds = if (is.null(state$elapsed)) NA else state$elapsed,
      peak_rss_bytes = if (is.null(state$peak_rss)) NA else state$peak_rss)
    events <- c(events, state$events)
    for (path in list.files(directory, pattern = "^diagnostics.*[.]rds$", full.names = TRUE)) {
      reports <- readRDS(path)
      for (i in seq_along(reports)) for (component in c("unknown_codes", "coercion_failures")) {
        value <- reports[[i]][[component]]
        if (!is.null(value) && nrow(value)) {
          value$case_id <- case$id; value$part <- i; value$mode <- basename(path); value$component <- component
          anomalies[[length(anomalies) + 1L]] <- value
        }
      }
    }
    if (case$kind != "file") next
    provenance_path <- file.path(directory, "raw-provenance.rds")
    provenance <- if (file.exists(provenance_path)) readRDS(provenance_path) else data.frame()
    for (i in seq_len(nrow(case$expected))) {
      row <- case$expected[i, ]
      p <- if (nrow(provenance)) provenance[provenance$file == row$file, ] else data.frame()
      row$case_id <- case$id; row$status <- state$status
      path <- audit_cached_path(row, config)
      row$downloaded <- file.exists(path) && file.info(path)$size > 0
      row$read <- nrow(p) == 1L
      row$processed <- !is.null(state$metrics$processed_rows)
      row$integrated <- !is.null(state$metrics$integrated_rows)
      row$bytes <- if (row$downloaded) file.info(path)$size else NA
      row$rows <- if (nrow(p)) p$rows[[1L]] else NA
      row$sha256 <- if (nrow(p)) p$checksum[[1L]] else NA_character_
      files[[length(files) + 1L]] <- row
    }
  }
  results <- audit_rows(rows); checks <- audit_rows(events); file_results <- audit_rows(files)
  audit_csv(results, file.path(config$root, "results.csv"))
  audit_csv(checks, file.path(config$root, "checks.csv"))
  audit_csv(file_results, file.path(config$root, "file-results.csv"))
  audit_csv(audit_rows(anomalies), file.path(config$root, "data-anomalies.csv"))
  if (nrow(checks)) {
    failures <- checks[checks$status == "fail", ]
    failures$category <- ifelse(grepl("Group requires", failures$message), "upstream_dependency",
      ifelse(grepl("timeout|memory_limit|disk_limit|deadline", failures$message), "resources",
      ifelse(grepl("resolve|connect|FTP|transfer|download|listing|listed|curl", paste(failures$message, failures$error_class), ignore.case = TRUE), "transport",
      ifelse(grepl("dbc|decompress|CRC|checksum", paste(failures$message, failures$error_class), ignore.case = TRUE), "dbc_or_integrity",
      ifelse(grepl("aggregat|bind|collect|processed_equal", failures$stage), "aggregation_or_equivalence", "processing")))))
    audit_csv(failures, file.path(config$root, "failures.csv"))
  } else failures <- data.frame()
  lines <- c("# Teste intensivo do microdatasus", "",
    paste("Commit:", config$commit, "| versão: 3.0.0.9000 | semente:", config$seed),
    paste("Início:", config$started, "| atualização:", Sys.time()),
    "Limites: 4 horas, 20 GB de artefatos, 6 GB de memória por subprocesso, 10 minutos por caso.", "",
    "## Resultado", "",
    sprintf("Arquivos únicos: %d selecionados; %d baixados; %d lidos; %d processados; %d pelo caminho integrado; %d aprovados; %d com falhas/limites; %d não executados.",
      nrow(file_results), sum(file_results$downloaded), sum(file_results$read), sum(file_results$processed), sum(file_results$integrated),
      sum(file_results$status == "ok"), sum(file_results$status %in% c("fail", "resource_limit")), sum(file_results$status == "not_run")),
    sprintf("Grupos de agregação: %d planejados; %d aprovados; %d com falhas/limites; %d não executados.", length(plan$groups),
      sum(results$kind == "group" & results$status == "ok"), sum(results$kind == "group" & results$status %in% c("fail", "resource_limit")),
      sum(results$kind == "group" & results$status == "not_run")),
    sprintf("Armazenamento: %.3f GB. Tempo decorrido: %.1f minutos.", audit_size(config$root) / 1e9,
      as.numeric(difftime(Sys.time(), config$started, units = "mins"))), "",
    "## Cobertura por família", "", "| Família | Selecionados | Baixados | Processados | Aprovados | Identificadores | Anos | UFs |", "|---|---:|---:|---:|---:|---:|---:|---:|")
  for (family in audit_families) {
    x <- file_results[file_results$family == family, ]
    lines <- c(lines, sprintf("| %s | %d | %d | %d | %d | %d | %d | %d |", family, nrow(x), sum(x$downloaded), sum(x$processed),
      sum(x$status == "ok"), length(unique(x$information_system)), length(unique(substr(x$date, 1L, 4L))), length(unique(na.omit(x$uf)))))
  }
  lines <- c(lines, "", "## Falhas", "")
  if (nrow(failures)) {
    for (i in seq_len(nrow(failures))) lines <- c(lines,
      paste0("- **", failures$case_id[[i]], " / ", failures$stage[[i]], "** (", failures$category[[i]], "): ",
             gsub("\n", " ", failures$message[[i]])))
  } else lines <- c(lines, "Nenhuma falha registrada até o momento.")
  lines <- c(lines, "", "## Reprodução e interpretação", "",
    "Os downloads são reais, completos e sem filtros de linhas ou colunas. As opções padrão de enriquecimento foram mantidas.",
    "Os códigos desconhecidos e falhas de conversão estão em data-anomalies.csv; não são automaticamente classificados como defeitos do pacote.",
    "A igualdade é exata por coluna, incluindo classes e níveis de fatores. Atributos da tabela relacionados à execução não entram na comparação.",
    "sample.csv contém a seleção, os motivos e os dicionários esperados nas transições históricas. file-results.csv registra a cobertura efetivamente executada.",
    "Cada caso tem argumentos, logs, checks, proveniência, diagnósticos e lockfiles em cases/<id>/.", "",
    "Para repetir um caso com a mesma biblioteca e cache:", "", "```sh",
    paste("Rscript", shQuote(config$runner), "--replay", shQuote(config$root), "<case-id>"), "```", "",
    "Casos não executados ou interrompidos por limites não contam como aprovação. Os arquivos brutos e dicionários permanecem no cache.")
  writeLines(lines, file.path(config$root, "report.md"))
  invisible(results)
}

audit_setup <- function(root) {
  repository <- normalizePath(".")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(root)
  config <- list(root = root, commit = audit_commit, seed = 20260922L,
    library = file.path(root, "library"), cache = file.path(root, "cache"), tmp = file.path(root, "tmp"),
    runner = file.path(root, "runner.R"), memory_limit = 6e9, disk_limit = 20e9)
  for (path in c(config$library, config$cache, config$tmp, file.path(root, "source"))) dir.create(path, showWarnings = FALSE)
  file.copy(file.path(repository, "tests/stress/live-intensive.R"), config$runner, overwrite = TRUE)
  archive <- file.path(root, "source.tar")
  processx::run("git", c("archive", "--format=tar", paste0("--output=", archive), config$commit), echo = FALSE)
  utils::untar(archive, exdir = file.path(root, "source"))
  unlink(archive)
  message("Installing pinned source in ", config$library)
  install <- processx::run(file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-multiarch", paste0("--library=", config$library), file.path(root, "source")),
    error_on_status = FALSE, timeout = 600000)
  writeLines(c(install$stdout, install$stderr), file.path(root, "installation.log"))
  audit_assert(install$status == 0L, "Isolated installation failed; see installation.log")
  config$started <- Sys.time()
  config$download_deadline <- config$started + 3 * 3600
  config$deadline <- config$started + 4 * 3600
  audit_save(config, file.path(root, "config.rds"))
  config
}

audit_main <- function(args = commandArgs(TRUE)) {
  if (length(args) && args[[1L]] == "--self-test") {
    x <- data.frame(a = 1:2, f = factor(c("x", "y")), d = as.Date(c("2000-01-01", "2000-01-02")))
    y <- x; attr(y, "microdatasus_provenance") <- list(time = Sys.time())
    audit_equal(x, y)
    bad <- x; bad$a <- as.numeric(bad$a)
    stopifnot(inherits(tryCatch(audit_equal(x, bad), error = identity), "error"))
    bad <- x; bad$f <- factor(bad$f, levels = rev(levels(bad$f)))
    stopifnot(inherits(tryCatch(audit_equal(x, bad), error = identity), "error"))
    stopifnot(identical(audit_period_date(c("9712", "9801", "2601"), "month"), as.Date(c("1997-12-01", "1998-01-01", "2026-01-01"))))
    audit_equal(audit_bind(list(x, x)), dplyr::bind_rows(x, x))
    cat("Comparator, factor-level, type, and historical-period checks passed.\n")
    return(invisible(NULL))
  }
  if (length(args) && args[[1L]] %in% c("--worker", "--replay")) {
    config <- readRDS(file.path(args[[2L]], "config.rds"))
    .libPaths(c(config$library, .libPaths()))
    library(microdatasus)
    audit_assert(normalizePath(find.package("microdatasus")) == normalizePath(file.path(config$library, "microdatasus")), "Wrong package installation")
    capture.output(sessionInfo(), file = file.path(config$root, "session-info.txt"))
    if (args[[1L]] == "--replay") {
      original <- args[[3L]]
      case <- readRDS(file.path(config$root, "cases", original, "case.rds"))
      case$id <- paste0(original, "-replay-", format(Sys.time(), "%Y%m%dT%H%M%S"))
      config$deadline <- Sys.time() + 600
      return(audit_supervise(config, case))
    }
    if (args[[3L]] == "discover") return(audit_discover(config))
    return(audit_case(config, readRDS(file.path(config$root, "cases", args[[4L]], "case.rds"))))
  }
  resume <- length(args) && args[[1L]] == "--resume"
  root <- if (resume) args[[2L]] else Sys.getenv("MICRODATASUS_INTENSIVE_ROOT", file.path(".cache", "intensive", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")))
  if (resume) {
    config <- readRDS(file.path(root, "config.rds"))
    file.copy(config$runner, file.path(root, paste0("runner-before-resume-", format(Sys.time(), "%H%M%S"), ".R")))
    file.copy("tests/stress/live-intensive.R", config$runner, overwrite = TRUE)
  } else {
    audit_assert(!file.exists(file.path(root, "config.rds")), "Run directory already exists; use --resume or --replay")
    config <- audit_setup(root)
  }
  message("ARTIFACTS: ", config$root)
  .libPaths(c(config$library, .libPaths()))
  library(microdatasus)
  if (!resume) {
    audit_supervise(config, action = "discover", limit = 1200)
    audit_assert(file.exists(file.path(config$root, "catalog.rds")), "Discovery did not finish; inspect discovery logs")
  }
  plan <- if (resume) readRDS(file.path(root, "plan.rds")) else audit_select(config)
  on.exit(audit_report(config, plan), add = TRUE)
  pending <- function(case) {
    directory <- file.path(config$root, "cases", case$id)
    path <- file.path(directory, "state.rds")
    if (!file.exists(path)) return(TRUE)
    state <- readRDS(path)
    if (!identical(state$status, "running")) return(FALSE)
    destination <- file.path(config$root, "interrupted", paste0(case$id, "-", format(Sys.time(), "%H%M%S")))
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    audit_assert(file.rename(directory, destination), "Could not preserve interrupted case")
    TRUE
  }
  for (case in plan$files) {
    if (Sys.time() >= config$download_deadline || audit_size(config$root) >= config$disk_limit) break
    if (!pending(case)) next
    audit_supervise(config, case)
    audit_report(config, plan)
  }
  for (case in plan$groups) {
    if (Sys.time() >= config$deadline || audit_size(config$root) >= config$disk_limit) break
    if (!pending(case)) next
    audit_supervise(config, case)
    audit_report(config, plan)
  }
  audit_report(config, plan)
  message("FINISHED: ", file.path(config$root, "report.md"))
}

if (sys.nframe() == 0L) audit_main()
