create_sinan_tabwin_fixtures <- function() {
  specs <- microdatasus:::.sinan_system_specs()

  net_parent <- tempfile("sinan-net-fixture-")
  net_root <- file.path(net_parent, "TAB_SINANNET")
  dir.create(net_root, recursive = TRUE)
  online_root <- tempfile("sinan-online-fixture-")
  dir.create(online_root)

  write_definitions <- function(root, definitions) {
    for (definition in unique(definitions)) {
      write_tabwin_text(
        file.path(root, definition),
        c(
          "A*.dbc",
          "XFlag, FLAG, 1, FLAG.CNV",
          "XSexo, CS_SEXO, 1, SEX.CNV"
        )
      )
    }
    write_tabwin_text(
      file.path(root, "FLAG.CNV"),
      c("1 1", tabwin_cnv_line(1, "Rotulo SINAN", "1"))
    )
    write_tabwin_text(
      file.path(root, "SEX.CNV"),
      c("2 1", tabwin_cnv_line(1, "Masculino", "M"))
    )
  }
  write_definitions(
    net_root,
    specs$definition[specs$archive == "SINAN-NET"]
  )
  write_definitions(
    online_root,
    specs$definition[specs$archive == "SINAN-ONLINE"]
  )

  net_archive <- tempfile(fileext = ".zip")
  zip::zipr(
    net_archive,
    files = list.files(net_parent),
    root = net_parent
  )
  online_archive <- tempfile(fileext = ".zip")
  zip::zipr(
    online_archive,
    files = list.files(online_root),
    root = online_root
  )
  unlink(net_parent, recursive = TRUE)
  unlink(online_root, recursive = TRUE)
  list(net = net_archive, online = online_archive)
}

test_that("process_sinan has a stable unified signature", {
  expect_identical(
    formals(process_sinan),
    as.pairlist(alist(
      data = ,
      information_system = "SINAN-DENGUE",
      municipality_data = TRUE
    ))
  )
})

test_that("SINAN registry covers all transfer-page families and definitions", {
  specs <- microdatasus:::.sinan_system_specs()
  downloads <- microdatasus:::.datasus_registry()
  dictionaries <- microdatasus:::.tabwin_registry()
  expected_acronyms <- c(
    "ANIM", "ANTR", "AIDA", "AIDC", "BOTU", "COLE", "COQU", "DENG",
    "DIFT", "DCRJ", "CHAG", "EXAN", "ESQU", "ESPO", "CHIK", "FMAC",
    "FTIF", "HANS", "HANT", "HEPA", "HIVA", "HIVC", "HIVE", "HIVG",
    "INFL", "IEXO", "LEIV", "LTAN", "LEPT", "MALA", "MENI", "PFAN",
    "PEST", "RAIV", "ROTA", "SIFA", "SIFC", "SIFG", "SRC", "SDTA",
    "TETA", "TETN", "TOXC", "TOXG", "NTRA", "TRAC", "TUBE", "VARC",
    "VIOL", "ZIKA", "ACBI", "ACGR", "CANC", "DERM", "LERD", "PAIR",
    "PNEU", "MENT"
  )

  expect_equal(nrow(specs), 58L)
  expect_setequal(specs$acronym, expected_acronyms)
  expect_true(all(specs$information_system %in% names(downloads)))
  expect_true(all(specs$information_system %in% names(dictionaries)))
  expect_identical(
    downloads[["SINAN-DENGUE"]]$repositories[[1L]]$prefix,
    "DENGBR"
  )
  expect_identical(
    vapply(
      downloads[["SINAN-LERD"]]$repositories,
      `[[`,
      character(1),
      "prefix"
    ),
    c("LERDBR", "LERBR", "LERDBR")
  )
  expect_equal(sum(specs$archive == "SINAN-ONLINE"), 2L)
  expect_true(all(vapply(
    dictionaries[specs$information_system[specs$archive == "SINAN-ONLINE"]],
    function(spec) identical(basename(spec$url), "TAB_SINANONLINE.zip"),
    logical(1)
  )))
  expect_true(all(vapply(
    dictionaries[specs$information_system[specs$archive == "SINAN-NET"]],
    function(spec) identical(basename(spec$url), "TAB_SINANNET.zip"),
    logical(1)
  )))
  for (index in seq_len(nrow(specs))) {
    system <- specs$information_system[[index]]
    listing <- paste0(specs$prefix[[index]], "24.dbc\n")
    parsed <- microdatasus:::.datasus_parse_listing(
      listing,
      downloads[[system]]$repositories[[1L]],
      downloads[[system]]
    )
    expect_identical(parsed$period, "2024")
    expect_true(is.na(parsed$uf))
  }
})

test_that("all SINAN families use one of two shared archives", {
  archives <- create_sinan_tabwin_fixtures()
  on.exit(unlink(unlist(archives)), add = TRUE)
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      downloads <<- downloads + 1L
      source <- if (grepl("ONLINE", url, fixed = TRUE)) {
        archives$online
      } else {
        archives$net
      }
      file.copy(source, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  for (information_system in microdatasus:::.sinan_information_systems()) {
    result <- process_sinan(
      data.frame(FLAG = "1"),
      information_system = information_system,
      municipality_data = FALSE
    )
    expect_identical(as.character(result$FLAG), "Rotulo SINAN")
  }
  expect_equal(downloads, 2L)
})

test_that("process_sinan standardizes common types and message order", {
  archives <- create_sinan_tabwin_fixtures()
  on.exit(unlink(unlist(archives)), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      file.copy(archives$online, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  source <- data.frame(
    DT_NOTIFIC = c("2024-01-31", "20240201"),
    NU_ANO = c("2024", "2024"),
    NU_IDADE_N = c("4116", "3023"),
    NU_NOTIFIC = c("0000001", "0000002"),
    ID_MN_RESI = c("1200209", "1200308"),
    CS_SEXO = c("M", "X"),
    stringsAsFactors = FALSE
  )
  messages <- capture_messages({
    result <- process_sinan(
      source,
      information_system = "SINAN-DENGUE",
      municipality_data = FALSE
    )
  })

  expect_s3_class(result$DT_NOTIFIC, "Date")
  expect_type(result$NU_ANO, "integer")
  expect_identical(result$NU_IDADE_N, c("4116", "3023"))
  expect_identical(result$IDADEanos, c(116L, NA_integer_))
  expect_identical(result$IDADEmeses, c(NA_integer_, 23L))
  expect_identical(result$NU_NOTIFIC, c("0000001", "0000002"))
  expect_identical(result$ID_MN_RESI, c("120020", "120030"))
  expect_s3_class(result$CS_SEXO, "factor")
  expect_identical(as.character(result$CS_SEXO), c("Masculino", "X"))
  expect_lt(
    which(grepl("Cached.+SINAN-DENGUE", messages)),
    which(grepl("Starting SINAN-DENGUE", messages))
  )
  expect_lt(
    which(grepl("Starting SINAN-DENGUE", messages)),
    which(grepl("Finished SINAN-DENGUE", messages))
  )
})

test_that("legacy SINAN wrappers preserve signatures and emit deprecation", {
  wrappers <- c(
    process_sinan_chagas = "SINAN-CHAGAS",
    process_sinan_chikungunya = "SINAN-CHIKUNGUNYA",
    process_sinan_dengue = "SINAN-DENGUE",
    process_sinan_leishmaniose_tegumentar =
      "SINAN-LEISHMANIOSE-TEGUMENTAR",
    process_sinan_leishmaniose_visceral = "SINAN-LEISHMANIOSE-VISCERAL",
    process_sinan_malaria = "SINAN-MALARIA",
    process_sinan_zika = "SINAN-ZIKA"
  )
  expected_formals <- as.pairlist(alist(
    data = ,
    municipality_data = TRUE
  ))
  for (wrapper in names(wrappers)) {
    fun <- get(wrapper, envir = asNamespace("microdatasus"))
    expect_identical(formals(fun), expected_formals)
    expect_warning(
      result <- fun(
        data.frame(NU_IDADE_N = "4029"),
        municipality_data = FALSE
      ),
      class = "microdatasus_sinan_deprecated"
    )
    expect_s3_class(result, "tbl_df")
    expect_identical(result$IDADEanos, 29L)
  }
})

test_that("process_sinan validates all public arguments", {
  expect_error(process_sinan(NULL), "must be a data frame")
  expect_error(
    process_sinan(data.frame(), information_system = "SINAN-UNKNOWN"),
    "must be one of"
  )
  expect_error(
    process_sinan(data.frame(), municipality_data = NA),
    "TRUE.*FALSE"
  )
})
