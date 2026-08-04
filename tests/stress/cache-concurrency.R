# Cross-process stress test for atomic cache payload and manifest commits.
library(microdatasus)
run_cache_stress <- function() {
  workers <- 4L
  cluster <- parallel::makePSOCKcluster(workers, outfile = "")
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  directory <- tempfile("microdatasus-cache-stress-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  destination <- file.path(directory, "shared.bin")
  manifest_path <- file.path(directory, "manifest.rds")
  payload <- as.raw(rep(0:255, 256L))
  manifest <- list(
    type = "stress", size = length(payload), checksum = NA_character_,
    downloaded_at = Sys.time()
  )
  parallel::clusterExport(
    cluster, c("destination", "manifest_path", "payload", "manifest"),
    envir = environment()
  )
  parallel::clusterEvalQ(cluster, library(microdatasus))
  result <- parallel::clusterCall(cluster, function() {
    temporary <- tempfile("payload-", tmpdir = dirname(destination))
    writeBin(payload, temporary)
    microdatasus:::.datasus_commit_file(temporary, destination)
    microdatasus:::.datasus_write_manifest(manifest, manifest_path)
    TRUE
  })
  stopifnot(all(unlist(result)))
  observed <- readBin(destination, what = "raw", n = length(payload))
  stopifnot(identical(observed, payload), identical(readRDS(manifest_path), manifest))
  stopifnot(!dir.exists(paste0(destination, ".lock")))
  stopifnot(!dir.exists(paste0(manifest_path, ".lock")))
  partials <- list.files(directory, pattern = "(payload|manifest).*-[0-9a-f]+$")
  stopifnot(!length(partials))

}

run_cache_stress()
