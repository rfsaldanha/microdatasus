# Deterministic mutation corpus for native DBC decompression safety checks.
library(microdatasus)
set.seed(20260804)
for (index in seq_len(200L)) {
  size <- sample(c(1:128, 256, 512, 1024, 4096), 1L)
  payload <- as.raw(sample.int(256L, size, replace = TRUE) - 1L)
  # Include common truncated-header shapes among otherwise random streams.
  if (index %% 4L == 0L) payload[seq_len(min(4L, size))] <- as.raw(c(0, 0, 0, 0)[seq_len(min(4L, size))])
  path <- tempfile(fileext = ".dbc")
  writeBin(payload, path)
  try(read_dbc(path), silent = TRUE)
  unlink(path)
}
