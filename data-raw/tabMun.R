# Rscript data-raw/tabMun.R [path/to/base_territorial_2023.zip]
# The pinned TXT members are also shipped, so the default rebuild is offline.
source("tests/support/territory-reference.R")
source_archive <- commandArgs(trailingOnly = TRUE)
portable <- "inst/extdata/territory/tabmun-source.zip"
if (length(source_archive)) {
  stopifnot(length(source_archive) == 1L)
  expected <- "798be2f62a53dd1af8e335a44a1916154f36ee3a3051a7375864a74ef47c3bc4"
  stopifnot(identical(digest::digest(source_archive, algo = "sha256", file = TRUE), expected))
  # Validate the exact official member bytes before packaging them.
  invisible(lapply(names(.territory_member_hashes), function(member) {
    .territory_read_member(source_archive, member)
  }))
  extract <- tempfile("tabmun-source-")
  dir.create(extract)
  utils::unzip(source_archive, files = names(.territory_member_hashes), exdir = extract)
  dir.create(dirname(portable), recursive = TRUE, showWarnings = FALSE)
  zip::zipr(file.path(normalizePath(dirname(portable)), basename(portable)), names(.territory_member_hashes),
             root = extract, include_directories = FALSE, mode = "mirror")
  unlink(extract, recursive = TRUE)
}
tabMun <- .rebuild_tabmun(portable)
save(tabMun, file = "data/tabMun.rda", compress = "xz", version = 2)
message("Rebuilt tabMun from pinned DataSUS TXT members: ", nrow(tabMun), " rows.")
