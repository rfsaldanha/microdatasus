# Rebuild from checksum-pinned public files, using .cache/review-history.
# See tests/fixtures/historical/README.md. Missing originals are downloaded;
# a server-side revision is rejected, not adopted as a new expected result.
# Expected processed values are maintained separately and NEVER generated here.
devtools::load_all(".", quiet = TRUE)
cache <- ".cache/review-history"
destination <- "tests/fixtures/historical"
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
destination <- normalizePath(destination)
frozen <- if (file.exists(file.path(destination, "manifest.csv"))) {
  read.csv(file.path(destination, "manifest.csv"), colClasses = "character")
} else NULL
cases <- list(
  list("DNRAC1995.dbc", "SINASC-1994-1995", "SINASC", "contador;DATA_NASC;SEXO;PESO;GESTACAO", "SEXO.CNV;GESTACAO.CNV"),
  list("DNAC1996.DBC", "SINASC", "SINASC", "contador;DTNASC;SEXO;PESO;GESTACAO", "SEXO.CNV;SEMANAS.CNV"),
  list("RDAC9712.dbc", "SIH-RD-1992-1997", "SIH-RD", "ANO_CMPT;MES_CMPT;SEXO;VAL_TOT;DT_INTER;DIAS_PERM", "SEXO.CNV"),
  list("RDAC9801.dbc", "SIH-RD-1998-2003-07", "SIH-RD", "ANO_CMPT;MES_CMPT;SEXO;VAL_TOT;DT_INTER;DIAS_PERM", "SEXO.CNV"),
  list("PAAC9910.DBC", "SIA-PA-1994-07-1999-10", "SIA-PA", "PA_CONDIC;PA_DATREF;PA_QTDPRO;PA_QTDAPR;PA_VALPRO;PA_VALAPR", "TPGESTAO.CNV;TPGEST_C.CNV"),
  list("PAAC9911.dbc", "SIA-PA-1999-11-2003-07", "SIA-PA", "PA_CONDIC;PA_DATREF;PA_QTDPRO;PA_QTDAPR;PA_VALPRO;PA_VALAPR", "TPGESTAO.CNV;TPGEST_C.CNV"),
  list("SRAC0802.dbc", "CNES-SR-2005-08-2008-02", "CNES-SR", "SERV_ESP;CLASS_SR;COMPETEN;CONTSRVU", "S_CLASSEA.DBF"),
  list("SRAC0803.dbc", "CNES-SR", "CNES-SR", "SERV_ESP;CLASS_SR;COMPETEN;CONTSRVU", "S_CLASSEN.DBF")
)
little_endian <- function(value, size) {
  as.raw((value %/% (256^(seq_len(size) - 1L))) %% 256)
}
unsigned <- function(bytes) sum(as.integer(bytes) * 256^(seq_along(bytes) - 1L))
literal_stream <- function(value) {
  bits <- unlist(lapply(as.integer(value), function(byte) {
    c(0L, as.integer(intToBits(byte))[1:8])
  }), use.names = FALSE)
  bits <- c(bits, 1L, rep(0L, 7L), rep(1L, 8L))
  bits <- c(bits, rep(0L, (-length(bits)) %% 8L))
  packed <- vapply(seq.int(1L, length(bits), 8L), function(start) {
    sum(bits[start + 0:7] * 2^(0:7))
  }, numeric(1))
  c(as.raw(c(0L, 6L)), as.raw(packed))
}
checksum <- function(path) digest::digest(path, algo = "sha256", file = TRUE)
registry <- microdatasus:::.tabwin_registry()
if (!is.null(frozen)) {
  for (i in seq_len(nrow(frozen))) {
    pin <- frozen[i, ]
    paths <- c(
      file.path(cache, "outputs", "dbc", basename(pin$source_url)),
      file.path(cache, "tabwin", registry[[pin$dictionary]]$archive_key, "dictionary.zip")
    )
    urls <- c(pin$source_url, pin$dictionary_url)
    hashes <- c(pin$source_sha256, pin$dictionary_source_sha256)
    for (j in seq_along(paths)) {
      dir.create(dirname(paths[[j]]), recursive = TRUE, showWarnings = FALSE)
      if (!file.exists(paths[[j]])) {
        curl::curl_download(urls[[j]], paths[[j]], quiet = TRUE)
      }
      if (!identical(checksum(paths[[j]]), hashes[[j]])) {
        stop("Pinned source checksum differs: ", urls[[j]])
      }
    }
  }
}
manifests <- lapply(list.files(cache, "manifest[.]rds$", full.names = TRUE,
                               recursive = TRUE), readRDS)
all_dbc <- list.files(file.path(cache, "outputs"), "[.][dD][bB][cC]$",
                      recursive = TRUE, full.names = TRUE)
registry <- microdatasus:::.tabwin_registry()
provenance <- list()
members <- list()

for (case in cases) {
  id <- tools::file_path_sans_ext(case[[1L]])
  root <- file.path(destination, id)
  dir.create(root, showWarnings = FALSE)
  source <- all_dbc[basename(all_dbc) == case[[1L]]]
  stopifnot(length(source) == 1L)
  if (is.null(frozen)) {
    origin <- Filter(function(x) identical(basename(x$source), basename(source)), manifests)
    stopifnot(length(origin) == 1L)
    origin <- origin[[1L]]
  } else {
    pin <- frozen[frozen$case == id, ]
    stopifnot(nrow(pin) == 1L)
    origin <- list(source = pin$source_url, checksum = pin$source_sha256)
  }
  stopifnot(identical(checksum(source), origin$checksum))

  dbf <- tempfile(fileext = ".dbf")
  microdatasus:::.dbc2dbf(source, dbf)
  bytes <- readBin(dbf, "raw", n = file.size(dbf))
  old_header <- unsigned(bytes[9:10])
  old_record <- unsigned(bytes[11:12])
  count <- (old_header - 33L) %/% 32L
  descriptors <- lapply(seq_len(count), function(i) bytes[33L + (i - 1L) * 32L + 0:31])
  field_names <- vapply(descriptors, function(field) {
    rawToChar(field[seq_len(match(as.raw(0), field[1:11], nomatch = 12L) - 1L)])
  }, character(1))
  widths <- vapply(descriptors, function(field) as.integer(field[17L]), integer(1))
  requested <- strsplit(case[[4L]], ";", fixed = TRUE)[[1L]]
  stopifnot(all(requested %in% field_names))
  selected <- which(field_names %in% requested) # preserve physical column order
  offsets <- c(1L, 1L + cumsum(widths))
  source_rows <- 1:3
  records <- unlist(lapply(source_rows, function(row) {
    start <- old_header + (row - 1L) * old_record
    c(bytes[start + 1L], unlist(lapply(selected, function(i) {
      bytes[start + offsets[[i]] + seq_len(widths[[i]])]
    }), use.names = FALSE))
  }), use.names = FALSE)
  header <- c(bytes[1:32], unlist(descriptors[selected], use.names = FALSE), as.raw(13))
  header[5:8] <- little_endian(length(source_rows), 4L)
  header[9:10] <- little_endian(length(header), 2L)
  header[11:12] <- little_endian(1L + sum(widths[selected]), 2L)
  crc <- digest::digest(c(header, records), algo = "crc32", serialize = FALSE)
  crc <- rev(as.raw(strtoi(substring(crc, seq(1, 7, 2), seq(2, 8, 2)), 16L)))
  fixture <- file.path(root, "input.dbc")
  writeBin(c(header, crc, literal_stream(records)), fixture)
  # Independent legacy reader, not read_dbc() or process_*(), supplies raw expectations.
  raw_values <- foreign::read.dbf(dbf, as.is = TRUE)[source_rows, selected, drop = FALSE]
  raw_values[] <- lapply(raw_values, as.character)
  utils::write.csv(raw_values, file.path(root, "raw.csv"), row.names = FALSE, na = "NA")
  unlink(dbf)

  spec <- registry[[case[[2L]]]]
  archive <- file.path(cache, "tabwin", spec$archive_key, "dictionary.zip")
  listing <- utils::unzip(archive, list = TRUE)
  wanted <- c(basename(spec$definition), strsplit(case[[5L]], ";", fixed = TRUE)[[1L]])
  zip_names <- gsub("\\", "/", listing$Name, fixed = TRUE, useBytes = TRUE)
  keep <- Reduce(`|`, lapply(wanted, function(name) {
    grepl(paste0("(^|/)", gsub(".", "[.]", name, fixed = TRUE), "$"),
          zip_names, ignore.case = TRUE, useBytes = TRUE)
  }))
  chosen <- listing$Name[keep]
  stopifnot(length(chosen) == length(wanted))
  extract <- tempfile("historical-dictionary-")
  dir.create(extract)
  utils::unzip(archive, files = chosen, exdir = extract)
  # Keep member bytes exact, but normalize directory prefixes to ASCII so the
  # corpus itself is portable. Legacy ZIP-name decoding has separate unit tests.
  portable <- tempfile("portable-dictionary-")
  dir.create(portable)
  portable_names <- character()
  for (member in chosen) {
    base <- sub("^.*/", "", gsub("\\", "/", member, fixed = TRUE, useBytes = TRUE), useBytes = TRUE)
    target <- if (grepl("[.]dbf$", base, ignore.case = TRUE)) file.path("DBF", base) else base
    dir.create(dirname(file.path(portable, target)), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(extract, member), file.path(portable, target)))
    Sys.setFileTime(file.path(portable, target), as.POSIXct("2000-01-01 12:00:00"))
    portable_names <- c(portable_names, target)
    members[[length(members) + 1L]] <- data.frame(
      case = id, member = target,
      source_member_hex = paste(sprintf("%02x", as.integer(charToRaw(member))), collapse = ""),
      sha256 = checksum(file.path(extract, member))
    )
  }
  zip::zipr(file.path(root, "dictionary.zip"), files = portable_names, root = portable,
            include_directories = FALSE, mode = "mirror")
  unlink(extract, recursive = TRUE)
  unlink(portable, recursive = TRUE)
  provenance[[length(provenance) + 1L]] <- data.frame(
    case = id, information_system = case[[3L]], dictionary = case[[2L]],
    source_url = origin$source, source_sha256 = checksum(source),
    source_rows = paste(source_rows, collapse = ";"), fields = paste(field_names[selected], collapse = ";"),
    dictionary_url = spec$url, dictionary_source_sha256 = checksum(archive),
    input_sha256 = checksum(fixture), dictionary_fixture_sha256 = checksum(file.path(root, "dictionary.zip")),
    raw_sha256 = checksum(file.path(root, "raw.csv")), stringsAsFactors = FALSE
  )
}
utils::write.csv(do.call(rbind, provenance), file.path(destination, "manifest.csv"), row.names = FALSE)
utils::write.csv(do.call(rbind, members), file.path(destination, "dictionary-members.csv"), row.names = FALSE)
