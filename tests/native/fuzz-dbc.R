# Deterministic mutation corpus for native DBC decompression and parsing.
library(microdatasus)

hex_to_raw <- function(hex) {
  starts <- seq.int(1L, nchar(hex), by = 2L)
  as.raw(strtoi(substring(hex, starts, starts + 1L), base = 16L))
}

seed <- hex_to_raw(paste0(
  "035f071a02000000810020000000000000000000000000000000000000000000",
  "434f444500000000000000430000000004000000000000000000000000000000",
  "56414c55450000000000004e00000000130f0000000000000000000000000000",
  "5748454e00000000000000440000000008000000000000000000000000000000",
  "0d00000000000640c080110364470c17355bc40c90f9c186b659ff6d80508934",
  "903f03fe01"
))

if (identical(Sys.getenv("MICRODATASUS_GCTORTURE"), "true")) {
  gctorture(TRUE)
  on.exit(gctorture(FALSE), add = TRUE)
}

exercise <- function(payload) {
  path <- tempfile(fileext = ".dbc")
  output <- tempfile(fileext = ".dbf")
  on.exit(unlink(c(path, output)), add = TRUE)
  writeBin(payload, path)
  suppressWarnings(suppressMessages(try(read_dbc(path), silent = TRUE)))
  suppressWarnings(suppressMessages(
    try(microdatasus:::.dbc2dbf(path, output), silent = TRUE)
  ))
  invisible(NULL)
}

set.seed(20260804)
for (index in seq_len(200L)) {
  size <- sample(c(1:128, 256, 512, 1024, 4096), 1L)
  payload <- as.raw(sample.int(256L, size, replace = TRUE) - 1L)
  # Include common truncated-header shapes among otherwise random streams.
  if (index %% 4L == 0L) payload[seq_len(min(4L, size))] <- as.raw(c(0, 0, 0, 0)[seq_len(min(4L, size))])
  exercise(payload)
}

# Mutations of a valid file reach header parsing, record assembly, and blast.
# Keep row-count and size words stable so fuzzing cannot request huge vectors.
protected <- c(5:12)
candidates <- setdiff(seq_along(seed), protected)
for (index in seq_len(500L)) {
  payload <- seed
  positions <- sample(candidates, sample.int(4L, 1L))
  payload[positions] <- as.raw(
    sample.int(256L, length(positions), replace = TRUE) - 1L
  )
  exercise(payload)
}

# Every truncation of the seed must become a regular R error, never a crash.
for (size in seq_len(length(seed) - 1L)) {
  exercise(seed[seq_len(size)])
}
