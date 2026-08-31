# Compare the direct reader with the legacy DBC -> DBF -> foreign path.
# Usage: Rscript tests/native/check-dbc-corpus.R FILE_OR_DIRECTORY [...]
library(microdatasus)

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) == 0L && requireNamespace("read.dbc", quietly = TRUE)) {
  arguments <- system.file("files", package = "read.dbc")
}
if (length(arguments) == 0L) {
  stop("Supply at least one DBC file or a directory containing DBC files.")
}

paths <- unlist(lapply(arguments, function(path) {
  if (dir.exists(path)) {
    list.files(path, pattern = "\\.dbc$", full.names = TRUE, recursive = TRUE)
  } else {
    path
  }
}), use.names = FALSE)
paths <- sort(unique(normalizePath(paths, mustWork = TRUE)))
if (length(paths) == 0L) stop("No DBC files found.")

for (path in paths) {
  direct_time <- system.time({
    direct <- read_dbc(path, as_character = FALSE)
  })[["elapsed"]]

  dbf <- tempfile(fileext = ".dbf")
  legacy_time <- system.time({
    microdatasus:::.dbc2dbf(path, dbf)
    legacy <- foreign::read.dbf(dbf, as.is = TRUE)
  })[["elapsed"]]
  unlink(dbf)

  source_encoding <- attr(direct, "dbc_encoding", exact = TRUE)
  character_columns <- which(vapply(legacy, is.character, logical(1)))
  for (index in character_columns) {
    legacy[[index]] <- iconv(
      legacy[[index]],
      from = source_encoding,
      to = "UTF-8",
      sub = NA
    )
  }

  plain_direct <- as.data.frame(direct)
  attributes(plain_direct) <- attributes(plain_direct)[
    c("names", "row.names", "class")
  ]
  comparison <- all.equal(plain_direct, legacy, check.attributes = FALSE)
  if (!isTRUE(comparison)) {
    stop(basename(path), ": ", paste(comparison, collapse = "; "))
  }

  selected_names <- head(names(direct), 5L)
  projected <- read_dbc(
    path,
    as_character = FALSE,
    vars = selected_names,
    encoding = source_encoding
  )
  if (!isTRUE(all.equal(
    as.data.frame(projected),
    plain_direct[, selected_names, drop = FALSE],
    check.attributes = FALSE
  ))) {
    stop(basename(path), ": projected read differs from full read")
  }

  message(sprintf(
    "%s: %d rows x %d columns; direct %.3fs; legacy %.3fs; encoding %s",
    basename(path),
    nrow(direct),
    ncol(direct),
    direct_time,
    legacy_time,
    source_encoding
  ))
}
