.legacy_name_archive <- function() {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "aXx"))
  write_tabwin_text(file.path(root, "aXx", "NASCIDO.def"), c(
    "Adn*.db?", "XSexo, SEXO, 1, SEXO.CNV"
  ))
  write_tabwin_text(file.path(root, "aXx", "SEXO.CNV"), c(
    "2 1 L", tabwin_cnv_line(1, "Masculino", "1"), tabwin_cnv_line(2, "Feminino", "2")
  ))
  writeLines("unused", file.path(root, "aXx", "CXpia de APGAR.CNV"))
  archive <- tempfile(fileext = ".zip")
  zip::zipr(archive, "aXx", root = root, include_directories = FALSE)
  bytes <- readBin(archive, "raw", n = file.size(archive))
  # Replace only equal-length member names in local and central headers.
  # No payload bytes or CRCs change. This deliberately does not normalize
  # ZIP names as the historical data fixtures do.
  for (name in c("aXx/", "CXpia de APGAR.CNV")) {
    pattern <- charToRaw(name)
    for (i in which(bytes == pattern[[1L]])) {
      indexes <- i + seq_along(pattern) - 1L
      if (max(indexes) <= length(bytes) && identical(bytes[indexes], pattern)) {
        bytes[[i + 1L]] <- as.raw(0xa2)
      }
    }
  }
  writeBin(bytes, archive)
  archive
}

test_that("legacy ZIP names are selective, atomic, and reusable across sessions", {
  archive <- .legacy_name_archive()
  withr::defer(unlink(archive))
  cache <- withr::local_tempdir()
  downloads <- 0L
  local_mocked_bindings(.datasus_download_file = function(url, destination, ...) {
    downloads <<- downloads + 1L
    file.copy(archive, destination)
  }, .package = "microdatasus")
  withr::defer(restore_empty_tabwin_cache())
  dictionary <- fetch_tabwin_dictionary("SINASC", cache_dir = cache, quiet = TRUE)
  expect_identical(dictionary$definitions$field, "SEXO")
  extracted <- list.files(file.path(dictionary$cache_dir, "members-v1"), recursive = TRUE)
  expect_length(extracted, 1L)
  expect_false(any(grepl("APGAR", extracted, ignore.case = TRUE)))
  cnv <- microdatasus:::.tabwin_extract_entry(dictionary, "SEXO.CNV")
  expect_identical(microdatasus:::.tabwin_parse_cnv(cnv)$map[c("1", "2")],
                   c(`1` = "Masculino", `2` = "Feminino"))
  original <- readBin(cnv, "raw", n = file.size(cnv))
  writeBin(as.raw(1L), cnv) # interrupted/truncated extracted member
  microdatasus:::.tabwin_clear_cache()
  dictionary <- fetch_tabwin_dictionary("SINASC", cache_dir = cache, quiet = TRUE)
  recovered <- microdatasus:::.tabwin_extract_entry(dictionary, "SEXO.CNV")
  expect_identical(readBin(recovered, "raw", n = file.size(recovered)), original)
  expect_identical(downloads, 1L)
  expect_error(microdatasus:::.tabwin_extract_entry(dictionary, "missing.cnv"),
               class = "microdatasus_dictionary_missing_error")
  expect_false(any(grepl("APGAR", list.files(dictionary$cache_dir, recursive = TRUE), ignore.case = TRUE)))
})

test_that("member extraction rejects empty members and disambiguates paths", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "a")); dir.create(file.path(root, "b"))
  writeLines("first", file.path(root, "a", "SAME.CNV"))
  writeLines("second", file.path(root, "b", "SAME.CNV"))
  file.create(file.path(root, "empty.cnv"))
  archive <- tempfile(fileext = ".zip")
  withr::defer(unlink(archive))
  zip::zipr(archive, c("a", "b", "empty.cnv"), root = root,
            include_directories = FALSE, mode = "mirror")
  listing <- utils::unzip(archive, list = TRUE)
  dictionary <- list(archive = archive, cache_dir = withr::local_tempdir(),
    entries = listing$Name, member_sizes = listing$Length, definition_dir = ".")
  expect_error(microdatasus:::.tabwin_extract_entry(dictionary, "SAME.CNV"),
               class = "microdatasus_dictionary_ambiguous_error")
  a <- microdatasus:::.tabwin_extract_entry(dictionary, "a/SAME.CNV")
  b <- microdatasus:::.tabwin_extract_entry(dictionary, "b/SAME.CNV")
  expect_false(identical(a, b))
  expect_identical(readLines(a), "first")
  expect_identical(readLines(b), "second")
  expect_error(microdatasus:::.tabwin_extract_entry(dictionary, "empty.cnv"), "empty|incomplete")
  # A different ZIP with an equal-length payload must not reuse old members.
  writeLines("other", file.path(root, "a", "SAME.CNV"))
  unlink(archive)
  zip::zipr(archive, c("a", "b", "empty.cnv"), root = root,
            include_directories = FALSE, mode = "mirror")
  changed <- microdatasus:::.tabwin_extract_entry(dictionary, "a/SAME.CNV")
  expect_identical(readLines(changed), "other")
  expect_identical(readLines(a), "first")
})
