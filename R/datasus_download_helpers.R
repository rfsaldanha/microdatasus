.datasus_ufs <- c(
  "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
  "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
  "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"
)

.datasus_repository <- function(url, release, priority, prefix) {
  list(
    url = url,
    release = release,
    priority = priority,
    prefix = prefix
  )
}

.datasus_registry <- function() {
  sim_final <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/"
  sim_prelim <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/PRELIM/"
  sih_current <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/"
  sih_old <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/199201_200712/Dados/"
  sinasc_old <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1994_1995/Dados/DNRES/"
  sinasc_current <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/Dados/DNRES/"
  sinasc_prelim <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/PRELIM/DNRES/"
  cnes <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/"
  sia_current <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/Dados/"
  sia_old <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/199407_200712/Dados/"
  sinan_final <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/FINAIS/"
  sinan_prelim <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS/PRELIM/"

  registry <- list()

  registry[["SIM-DO"]] <- list(
    granularity = "year",
    geography = "state",
    minimum = as.Date("1996-01-01"),
    repositories = list(
      .datasus_repository(paste0(sim_final, "DORES/"), "final", 1L, "DO"),
      .datasus_repository(paste0(sim_prelim, "DORES/"), "preliminary", 2L, "DO")
    )
  )

  for (prefix in c("DOFET", "DOEXT", "DOINF", "DOMAT")) {
    registry[[paste0("SIM-", prefix)]] <- list(
      granularity = "year",
      geography = "national",
      year_digits = 2L,
      minimum = as.Date("1996-01-01"),
      repositories = list(
        .datasus_repository(
          paste0(sim_final, "DOFET/"),
          "final",
          1L,
          prefix
        ),
        .datasus_repository(
          paste0(sim_prelim, "DOFET/"),
          "preliminary",
          2L,
          prefix
        )
      )
    )
  }

  for (prefix in c("RD", "RJ", "SP", "ER")) {
    registry[[paste0("SIH-", prefix)]] <- list(
      granularity = "month",
      geography = "state",
      minimum = as.Date("1992-01-01"),
      repositories = list(
        .datasus_repository(sih_current, "current", 1L, prefix),
        .datasus_repository(sih_old, "old", 3L, prefix)
      )
    )
  }

  registry[["SINASC"]] <- list(
    granularity = "year",
    geography = "state",
    minimum = as.Date("1994-01-01"),
    repositories = list(
      .datasus_repository(sinasc_current, "current", 1L, "DN"),
      .datasus_repository(sinasc_prelim, "preliminary", 2L, "DN"),
      .datasus_repository(sinasc_old, "old", 3L, "DNR")
    )
  )

  for (prefix in c(
    "LT", "ST", "DC", "EQ", "SR", "HB", "PF",
    "EP", "RC", "IN", "EE", "EF", "GM"
  )) {
    registry[[paste0("CNES-", prefix)]] <- list(
      granularity = "month",
      geography = "state",
      minimum = as.Date("2005-08-01"),
      repositories = list(
        .datasus_repository(
          paste0(cnes, prefix, "/"),
          "current",
          1L,
          prefix
        )
      )
    )
  }

  for (prefix in c(
    "AB", "ABO", "ACF", "AD", "AN", "AM",
    "AQ", "AR", "ATD", "PA", "PS", "SAD"
  )) {
    registry[[paste0("SIA-", prefix)]] <- list(
      granularity = "month",
      geography = "state",
      minimum = as.Date("1994-07-01"),
      repositories = list(
        .datasus_repository(sia_current, "current", 1L, prefix),
        .datasus_repository(sia_old, "old", 3L, prefix)
      )
    )
  }

  # The transfer portal currently exposes 58 SINAN file families. The
  # centralized table also feeds process_sinan() and the TabWin registry so
  # download identifiers, DBC prefixes, and definitions cannot drift apart.
  sinan_specs <- .sinan_system_specs()
  for (index in seq_len(nrow(sinan_specs))) {
    system <- sinan_specs$information_system[[index]]
    prefix <- sinan_specs$prefix[[index]]
    repositories <- list(
      .datasus_repository(sinan_final, "final", 1L, prefix),
      .datasus_repository(sinan_prelim, "preliminary", 2L, prefix)
    )
    if (identical(sinan_specs$acronym[[index]], "LERD")) {
      # Older final files use LERBR; current/future files use LERDBR. Directory
      # listings are cached by URL, so accepting both does not add a request.
      repositories <- list(
        .datasus_repository(sinan_final, "final", 1L, prefix),
        .datasus_repository(sinan_final, "final", 2L, "LERBR"),
        .datasus_repository(sinan_prelim, "preliminary", 3L, prefix)
      )
    }
    registry[[system]] <- list(
      granularity = "year",
      geography = "national",
      year_digits = 2L,
      minimum = as.Date("1996-01-01"),
      repositories = repositories
    )
  }

  registry
}

.datasus_assert_flag <- function(x, argument) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {argument}} must be `TRUE` or `FALSE`.")
  }
  invisible(x)
}

.datasus_cli_bullets <- function(x, type = "x") {
  x <- gsub("{", "{{", x, fixed = TRUE)
  x <- gsub("}", "}}", x, fixed = TRUE)
  structure(x, names = rep(type, length(x)))
}

.datasus_assert_number <- function(x, argument, integer = FALSE, lower = NULL) {
  valid <- is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x)
  if (valid && integer) {
    valid <- x == floor(x)
  }
  if (valid && !is.null(lower)) {
    valid <- x >= lower
  }
  if (!valid) {
    qualifier <- if (integer) "a single whole number" else "a single number"
    cli::cli_abort("{.arg {argument}} must be {qualifier}.")
  }
  invisible(x)
}

.datasus_validate_arguments <- function(
  year_start,
  month_start,
  year_end,
  month_end,
  uf,
  information_system,
  vars,
  stop_on_error,
  timeout,
  track_source,
  quiet
) {
  registry <- .datasus_registry()
  if (
    !is.character(information_system) ||
      length(information_system) != 1L ||
      is.na(information_system) ||
      !information_system %in% names(registry)
  ) {
    cli::cli_abort(
      "{.arg information_system} must be one of the supported systems."
    )
  }
  spec <- registry[[information_system]]

  .datasus_assert_number(year_start, "year_start", integer = TRUE)
  .datasus_assert_number(year_end, "year_end", integer = TRUE)
  .datasus_assert_number(timeout, "timeout", lower = .Machine$double.eps)
  .datasus_assert_flag(stop_on_error, "stop_on_error")
  .datasus_assert_flag(track_source, "track_source")
  .datasus_assert_flag(quiet, "quiet")

  validate_month <- function(x, argument, required) {
    if (is.null(x)) {
      if (required) {
        cli::cli_abort(
          "{.arg {argument}} is required for monthly information systems."
        )
      }
      return(invisible(NULL))
    }
    .datasus_assert_number(x, argument, integer = TRUE)
    if (x < 1L || x > 12L) {
      cli::cli_abort("{.arg {argument}} must be between 1 and 12.")
    }
    invisible(x)
  }

  monthly <- identical(spec$granularity, "month")
  validate_month(month_start, "month_start", monthly)
  validate_month(month_end, "month_end", monthly)
  if (!monthly && (!is.null(month_start) || !is.null(month_end))) {
    month_arguments <- c(
      if (!is.null(month_start)) "month_start",
      if (!is.null(month_end)) "month_end"
    )
    verb <- if (length(month_arguments) == 1L) "is" else "are"
    cli::cli_alert_warning(
      "{.arg {month_arguments}} {verb} ignored because {.val {information_system}} uses annual files."
    )
  }

  if (
    !is.character(uf) ||
      length(uf) < 1L ||
      anyNA(uf) ||
      any(!nzchar(uf)) ||
      !all(uf %in% c("all", .datasus_ufs))
  ) {
    cli::cli_abort(
      "{.arg uf} must contain `all` or valid two-letter state codes."
    )
  }
  if ("all" %in% uf && length(uf) != 1L) {
    cli::cli_abort("{.val all} cannot be combined with specific states.")
  }
  if (anyDuplicated(uf)) {
    cli::cli_abort("{.arg uf} must not contain duplicated states.")
  }

  if (!is.null(vars)) {
    if (
      !is.character(vars) ||
        length(vars) < 1L ||
        anyNA(vars) ||
        any(!nzchar(vars))
    ) {
      cli::cli_abort(
        "{.arg vars} must be `NULL` or a vector of non-empty names."
      )
    }
    if (anyDuplicated(vars)) {
      cli::cli_abort("{.arg vars} must not contain duplicated names.")
    }
  }

  if (monthly) {
    date_start <- as.Date(sprintf("%04d-%02d-01", year_start, month_start))
    date_end <- as.Date(sprintf("%04d-%02d-01", year_end, month_end))
  } else {
    date_start <- as.Date(sprintf("%04d-01-01", year_start))
    date_end <- as.Date(sprintf("%04d-01-01", year_end))
  }
  if (is.na(date_start) || is.na(date_end)) {
    cli::cli_abort("The supplied date range is invalid.")
  }
  if (date_start > date_end) {
    cli::cli_abort("Start date must not be later than end date.")
  }
  if (date_start < spec$minimum) {
    cli::cli_abort(c(
      "The requested start date predates this information system.",
      "i" = "The earliest supported date is {format(spec$minimum, '%Y-%m')}."
    ))
  }

  dates <- if (monthly) {
    seq(date_start, date_end, by = "month")
  } else {
    seq(date_start, date_end, by = "year")
  }
  periods <- if (monthly) {
    format(dates, "%y%m")
  } else {
    format(dates, "%Y")
  }

  list(
    spec = spec,
    periods = periods,
    ufs = if (identical(uf, "all")) .datasus_ufs else uf
  )
}

.datasus_retry <- function(
  operation,
  attempts = 3L,
  retry_if = function(error) TRUE
) {
  last_error <- NULL
  for (attempt in seq_len(attempts)) {
    result <- tryCatch(operation(), error = identity)
    if (!inherits(result, "error")) {
      return(result)
    }
    last_error <- result
    if (!retry_if(result)) {
      stop(result)
    }
    if (attempt < attempts) {
      .datasus_retry_wait(2 ^ (attempt - 1L))
    }
  }
  stop(last_error)
}

.datasus_retry_wait <- function(seconds) {
  Sys.sleep(seconds)
}

.datasus_timeout_ms <- function(timeout) {
  milliseconds <- min(ceiling(timeout * 1000), .Machine$integer.max)
  max(1L, as.integer(milliseconds))
}

.datasus_is_transient_curl_error <- function(error) {
  if (!inherits(error, "curl_error")) {
    return(FALSE)
  }
  permanent <- c(
    "curl_error_unsupported_protocol",
    "curl_error_url_malformat",
    "curl_error_ftp_access_denied",
    "curl_error_ftp_couldnt_retr_file",
    "curl_error_remote_file_not_found",
    "curl_error_login_denied"
  )
  !inherits(error, permanent)
}

.datasus_list_directory <- function(url, timeout) {
  .datasus_retry(function() {
    handle <- curl::new_handle()
    curl::handle_setopt(
      handle,
      dirlistonly = TRUE,
      ftp_use_epsv = TRUE,
      timeout_ms = .datasus_timeout_ms(timeout),
      connecttimeout_ms = .datasus_timeout_ms(min(timeout, 30))
    )
    response <- curl::curl_fetch_memory(url, handle = handle)
    rawToChar(response$content)
  }, retry_if = .datasus_is_transient_curl_error)
}

.datasus_parse_listing <- function(text, repository, spec) {
  files <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
  files <- sub("\r$", "", files)
  files <- files[nzchar(files)]
  if (!length(files)) {
    return(data.frame())
  }

  prefix <- repository$prefix
  if (identical(spec$geography, "state")) {
    digits <- if (identical(spec$granularity, "month")) 4L else 4L
    pattern <- paste0(
      "^", prefix, "([A-Z]{2})([0-9]{", digits, "})([^.]*)[.]DBC$"
    )
  } else {
    digits <- if (is.null(spec$year_digits)) 4L else spec$year_digits
    pattern <- paste0(
      "^", prefix, "([0-9]{", digits, "})([^.]*)[.]DBC$"
    )
  }

  matches <- regexec(pattern, toupper(files), perl = TRUE)
  captures <- regmatches(toupper(files), matches)
  keep <- lengths(captures) > 0L
  if (!any(keep)) {
    return(data.frame())
  }
  files <- files[keep]
  captures <- captures[keep]

  if (identical(spec$geography, "state")) {
    states <- vapply(captures, `[[`, character(1), 2L)
    periods <- vapply(captures, `[[`, character(1), 3L)
    fragments <- vapply(captures, `[[`, character(1), 4L)
  } else {
    states <- rep(NA_character_, length(captures))
    periods <- vapply(captures, `[[`, character(1), 2L)
    fragments <- vapply(captures, `[[`, character(1), 3L)
    if (digits == 2L) {
      periods <- ifelse(
        substr(periods, 1L, 1L) == "9",
        paste0("19", periods),
        paste0("20", periods)
      )
    }
  }

  data.frame(
    file = files,
    url = paste0(repository$url, files),
    period = periods,
    uf = states,
    fragment = fragments,
    release = repository$release,
    priority = repository$priority,
    stringsAsFactors = FALSE
  )
}

.datasus_build_manifest <- function(spec, periods, ufs, timeout) {
  manifests <- list()
  errors <- character()
  listings <- new.env(parent = emptyenv())

  for (index in seq_along(spec$repositories)) {
    repository <- spec$repositories[[index]]
    if (exists(repository$url, envir = listings, inherits = FALSE)) {
      listing <- get(repository$url, envir = listings, inherits = FALSE)
    } else {
      listing <- tryCatch(
        .datasus_list_directory(repository$url, timeout),
        error = identity
      )
      assign(repository$url, listing, envir = listings)
    }
    if (inherits(listing, "error")) {
      errors <- c(
        errors,
        paste0(repository$url, ": ", conditionMessage(listing))
      )
      next
    }
    parsed <- .datasus_parse_listing(listing, repository, spec)
    if (nrow(parsed)) {
      parsed$repository_order <- index
      manifests[[length(manifests) + 1L]] <- parsed
    }
  }

  if (!length(manifests)) {
    return(list(manifest = data.frame(), errors = errors))
  }
  manifest <- data.table::rbindlist(manifests, use.names = TRUE, fill = TRUE)
  manifest <- as.data.frame(manifest)
  manifest <- manifest[manifest$period %in% periods, , drop = FALSE]
  if (identical(spec$geography, "state")) {
    manifest <- manifest[manifest$uf %in% ufs, , drop = FALSE]
  }
  if (!nrow(manifest)) {
    return(list(manifest = manifest, errors = errors))
  }

  period_order <- match(manifest$period, periods)
  uf_order <- if (identical(spec$geography, "state")) {
    match(manifest$uf, ufs)
  } else {
    rep(1L, nrow(manifest))
  }
  precedence <- order(
    manifest$priority,
    manifest$repository_order,
    manifest$file
  )
  manifest <- manifest[precedence, , drop = FALSE]
  identity <- paste(
    manifest$period,
    manifest$uf,
    manifest$fragment,
    sep = "\r"
  )
  manifest <- manifest[!duplicated(identity), , drop = FALSE]

  period_order <- match(manifest$period, periods)
  uf_order <- if (identical(spec$geography, "state")) {
    match(manifest$uf, ufs)
  } else {
    rep(1L, nrow(manifest))
  }
  final_order <- order(period_order, uf_order, manifest$fragment, manifest$file)
  manifest <- manifest[final_order, , drop = FALSE]
  rownames(manifest) <- NULL

  list(manifest = manifest, errors = errors)
}

.datasus_transfer_file <- function(
  url,
  destination,
  timeout,
  quiet = FALSE
) {
  unlink(destination)
  handle <- curl::new_handle()
  curl::handle_setopt(
    handle,
    timeout_ms = .datasus_timeout_ms(timeout),
    connecttimeout_ms = .datasus_timeout_ms(min(timeout, 30)),
    ftp_use_epsv = TRUE
  )
  curl::curl_download(
    url,
    destfile = destination,
    quiet = quiet,
    mode = "wb",
    handle = handle
  )
  invisible(destination)
}

.datasus_download_file <- function(
  url,
  destination,
  timeout,
  quiet = FALSE
) {
  .datasus_retry(
    function() {
      .datasus_transfer_file(
        url,
        destination,
        timeout,
        quiet = quiet
      )
    },
    retry_if = .datasus_is_transient_curl_error
  )
  if (!file.exists(destination) || is.na(file.size(destination)) ||
      file.size(destination) == 0) {
    cli::cli_abort("The file downloaded from {.url {url}} is empty.")
  }
  invisible(destination)
}

.datasus_summarize_failures <- function(failures) {
  if (!length(failures)) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "{length(failures)} DataSUS file{?s} could not be processed.",
    .datasus_cli_bullets(failures)
  ))
}

.datasus_fetch_zip_dbf <- function(url, internal_file, timeout) {
  .datasus_assert_number(timeout, "timeout", lower = .Machine$double.eps)

  work_dir <- tempfile("microdatasus-auxiliary-")
  if (!dir.create(work_dir, recursive = TRUE)) {
    cli::cli_abort("Failed to create a temporary download directory.")
  }
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  archive <- file.path(work_dir, "download.zip")
  .datasus_download_file(url, archive, timeout)

  extracted <- tryCatch(
    zip::unzip(
      zipfile = archive,
      files = internal_file,
      exdir = work_dir,
      overwrite = TRUE
    ),
    error = identity
  )
  if (inherits(extracted, "error")) {
    cli::cli_abort(c(
      "Failed to extract the DataSUS auxiliary table.",
      "i" = conditionMessage(extracted)
    ))
  }

  dbf <- file.path(work_dir, internal_file)
  if (!file.exists(dbf) || is.na(file.size(dbf)) || file.size(dbf) == 0) {
    cli::cli_abort(
      "The downloaded archive does not contain a valid {.file {internal_file}} file."
    )
  }

  tryCatch(
    foreign::read.dbf(dbf, as.is = TRUE),
    error = function(error) {
      cli::cli_abort(c(
        "Failed to read the DataSUS auxiliary table.",
        "i" = conditionMessage(error)
      ))
    }
  )
}
