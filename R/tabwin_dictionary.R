# The package namespace lives for the whole R session, so this private
# environment provides a session cache without writing permanent user files.
.tabwin_cache <- new.env(parent = emptyenv())

.tabwin_archive_cache <- new.env(parent = emptyenv())

# The five mortality products are subsets of the same death-certificate
# CID-10 database and use the same TabWin archive and DEF. The archive key
# allows all five dictionaries to share one download during the R session.
.tabwin_registry <- function() {
  sim_types <- c(
    "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT"
  )
  specs <- lapply(sim_types, function(information_system) {
    list(
      archive_key = "SIM-OBITOS-CID10",
      information_system = information_system,
      url = paste0(
        "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/TAB/",
        "OBITOS_CID10_TAB.zip"
      ),
      definition = "/tabdo/Obito_1996_CID10.def"
    )
  })
  names(specs) <- sim_types

  # The transfer portal publishes separate SINASC archives for the original
  # 1994-1995 layout and the layout used from 1996 onward.
  specs[["SINASC"]] <- list(
    archive_key = "SINASC-1996",
    information_system = "SINASC",
    url = paste0(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1996_/",
      "Auxiliar/Arq_Para_Tabulacao_A_Partir_1996.zip"
    ),
    definition = "/NASCIDO.def",
    extract_all = TRUE
  )
  specs[["SINASC-1994-1995"]] <- list(
    archive_key = "SINASC-1994-1995",
    information_system = "SINASC-1994-1995",
    url = paste0(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SINASC/1994_1995/",
      "Auxiliar/Arq_Para_Tabulacao_Ate_1995.zip"
    ),
    definition = "/NASC.DEF",
    extract_all = TRUE
  )

  # SIH publishes one current archive with separate definitions for each file
  # family and three historical archives for RD/RJ layout periods.
  sih_base <- paste0(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/",
    "Auxiliar/"
  )
  current_sih_definitions <- c(
    "SIH-RD" = "RD2008.DEF",
    "SIH-RJ" = "RJ2008.DEF",
    "SIH-SP" = "SP2008.DEF",
    "SIH-ER" = "Motivo_de_Erro.DEF"
  )
  for (information_system in names(current_sih_definitions)) {
    specs[[information_system]] <- list(
      archive_key = "SIH-2008",
      information_system = information_system,
      url = paste0(sih_base, "TAB_SIH.zip"),
      definition = unname(current_sih_definitions[[information_system]])
    )
  }
  historical_sih <- list(
    "1992-1997" = "TAB_SIH_199201-199712.zip",
    "1998-2003-07" = "TAB_SIH_199801-200307.zip",
    "2003-08-2007" = "TAB_SIH_200308-200712.zip"
  )
  for (period in names(historical_sih)) {
    for (file_type in c("RD", "RJ")) {
      information_system <- paste("SIH", file_type, period, sep = "-")
      specs[[information_system]] <- list(
        archive_key = paste0("SIH-", period),
        information_system = information_system,
        url = paste0(sih_base, historical_sih[[period]]),
        definition = paste0(file_type, ".DEF")
      )
    }
  }

  # SIA publishes one current archive for its twelve downloadable file
  # families. The older archives contain the PA production layouts that
  # preceded the 2008 table redesign.
  sia_base <- paste0(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SIASUS/200801_/",
    "Auxiliar/"
  )
  current_sia_definitions <- c(
    "SIA-AB" = "APAC_Cirurgia_Bariatica.DEF",
    "SIA-ABO" = "APAC_Pos_Cirurgia_Bariatica.def",
    "SIA-ACF" = "APAC_Confeccao_de_Fistula.DEF",
    "SIA-AD" = "APAC_Laudos_Diversos.DEF",
    "SIA-AN" = "APAC_Nefrologia.DEF",
    "SIA-AM" = "APAC_Medicamentos.DEF",
    "SIA-AQ" = "APAC_Quimioterapia.DEF",
    "SIA-AR" = "APAC_Radioterapia.DEF",
    "SIA-ATD" = "APAC_Tratamento_Dialitico.DEF",
    "SIA-PA" = "Producao_Ambulatorial.DEF",
    "SIA-PS" = "RAAS_Psicossocial.def",
    "SIA-SAD" = "Atencao_Domiciliar.def"
  )
  for (information_system in names(current_sia_definitions)) {
    specs[[information_system]] <- list(
      archive_key = "SIA-2008",
      information_system = information_system,
      url = paste0(sia_base, "TAB_SIA.zip"),
      definition = unname(current_sia_definitions[[information_system]])
    )
  }
  historical_sia <- list(
    "1994-07-1999-10" = c(
      archive = "TAB_SIA_199407-199910.zip",
      definition = "PRODUCAO.DEF"
    ),
    "1999-11-2003-07" = c(
      archive = "TAB_SIA_199911-200307.zip",
      definition = "PROD_SIA.DEF"
    ),
    "2003-08-2007" = c(
      archive = "TAB_SIA_200308-200712.zip",
      definition = "PRODCNES.DEF"
    )
  )
  for (period in names(historical_sia)) {
    specs[[paste0("SIA-PA-", period)]] <- list(
      archive_key = paste0("SIA-", period),
      information_system = paste0("SIA-PA-", period),
      url = paste0(sia_base, historical_sia[[period]][["archive"]]),
      definition = historical_sia[[period]][["definition"]]
    )
  }

  # CNES publishes all thirteen file families in one TabWin archive. Service
  # classifications changed in March 2008, and both definitions remain in the
  # current ZIP, which lets process_cnes() handle old and current rows without
  # a second download.
  cnes_url <- paste0(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/",
    "Auxiliar/TAB_CNES.zip"
  )
  cnes_definitions <- c(
    "CNES-LT" = "Leitos_Especialidade.def",
    "CNES-ST" = "Estabelecimento.def",
    "CNES-DC" = "DadosComplementares.def",
    "CNES-EQ" = "Equipamento.def",
    "CNES-SR" = "Servico_Especializado_200803_.def",
    "CNES-HB" = "Habilitacao.def",
    "CNES-PF" = "Profissional.def",
    "CNES-EP" = "Equipes.def",
    "CNES-RC" = "Regras_Contratuais.def",
    "CNES-IN" = "Incentivos.def",
    "CNES-EE" = "Estabel_Ensino.def",
    "CNES-EF" = "Estabel_Filantropico.def",
    "CNES-GM" = "Gestao_de_Metas.def"
  )
  for (information_system in names(cnes_definitions)) {
    specs[[information_system]] <- list(
      archive_key = "CNES-200508",
      information_system = information_system,
      url = cnes_url,
      definition = unname(cnes_definitions[[information_system]])
    )
  }
  specs[["CNES-SR-2005-08-2008-02"]] <- list(
    archive_key = "CNES-200508",
    information_system = "CNES-SR-2005-08-2008-02",
    url = cnes_url,
    definition = "Servico_Especializado_200508_200802.def"
  )

  # SINAN Net contains the dedicated definitions for most file families.
  # Dengue and chikungunya use the much smaller Online archive published next
  # to it. Families without a dedicated DEF use the official generic
  # NotIndiviNet definition for shared notification fields.
  sinan_specs <- .sinan_system_specs()
  sinan_net_url <- paste0(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/AUXILIAR/",
    "TAB_SINANNET.zip"
  )
  sinan_online_url <- paste0(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/AUXILIAR/",
    "TAB_SINANONLINE.zip"
  )
  for (index in seq_len(nrow(sinan_specs))) {
    information_system <- sinan_specs$information_system[[index]]
    net <- identical(sinan_specs$archive[[index]], "SINAN-NET")
    specs[[information_system]] <- list(
      archive_key = sinan_specs$archive[[index]],
      information_system = information_system,
      url = if (net) sinan_net_url else sinan_online_url,
      definition = if (net) {
        paste0("TAB_SINANNET/", sinan_specs$definition[[index]])
      } else {
        sinan_specs$definition[[index]]
      }
    )
  }
  specs
}

.tabwin_abort <- function(message, class, .envir = parent.frame()) {
  # Stable condition classes let audits classify failures without parsing text.
  cli::cli_abort(
    message, class = c(class, "microdatasus_dictionary_error"),
    .envir = .envir
  )
}

.tabwin_expand_tabs <- function(line, tab_width = 8L) {
  if (!grepl("\t", line, fixed = TRUE)) return(line)
  characters <- strsplit(line, "", fixed = TRUE)[[1L]]
  result <- character()
  column <- 1L
  for (character in characters) {
    if (identical(character, "\t")) {
      spaces <- tab_width - ((column - 1L) %% tab_width)
      result <- c(result, rep(" ", spaces))
      column <- column + spaces
    } else {
      result <- c(result, character)
      column <- column + 1L
    }
  }
  paste0(result, collapse = "")
}

.tabwin_read_text <- function(path) {
  # Official archives mix Windows-1252 and UTF-8. Detect valid UTF-8 first and
  # use the legacy encoding only as a fallback, independently of the R locale.
  size <- file.info(path)$size
  if (is.na(size) || size == 0) {
    .tabwin_abort(
      "TabWin file {.file {basename(path)}} is empty.",
      "microdatasus_dictionary_invalid_error"
    )
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, what = "raw", n = size)
  if (any(bytes == as.raw(0L))) {
    .tabwin_abort(
      "TabWin file {.file {basename(path)}} is binary, not text.",
      "microdatasus_dictionary_invalid_error"
    )
  }
  if (length(bytes) >= 3L &&
      identical(bytes[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    bytes <- bytes[-(1:3)]
  }
  encoded <- rawToChar(bytes)
  text <- iconv(encoded, from = "UTF-8", to = "UTF-8")
  encoding <- "UTF-8"
  if (is.na(text)) {
    text <- iconv(
      encoded, from = "windows-1252", to = "UTF-8", sub = "byte"
    )
    encoding <- "windows-1252"
  }
  if (is.na(text)) {
    .tabwin_abort(
      "Could not convert TabWin file {.file {basename(path)}} to UTF-8.",
      "microdatasus_dictionary_invalid_error"
    )
  }
  lines <- strsplit(text, "\r\n|\n|\r", perl = TRUE)[[1L]]
  tabs <- lengths(regmatches(lines, gregexpr("\t", lines, fixed = TRUE)))
  lines <- vapply(lines, .tabwin_expand_tabs, character(1), USE.NAMES = FALSE)
  attr(lines, "encoding") <- encoding
  attr(lines, "tabs_recovered") <- sum(tabs)
  lines
}

.tabwin_parse_def <- function(path) {
  lines <- .tabwin_read_text(path)
  records <- lapply(seq_along(lines), function(i) {
    line <- lines[[i]]
    # In DEF files, comments and inactive definitions start in column one.
    if (!nzchar(line) || substr(line, 1L, 1L) %in% c(";", " ")) {
      return(NULL)
    }
    # S/L/C/Q/D/T/X are TabWin categorical definitions. Increment and file
    # declarations do not point to label tables and are ignored here.
    command <- toupper(substr(line, 1L, 1L))
    if (!command %in% c("S", "L", "C", "Q", "D", "T", "X")) {
      return(NULL)
    }
    fields <- strsplit(line, ",", fixed = TRUE)[[1L]]
    if (length(fields) < 4L) {
      return(NULL)
    }
    file_name <- trimws(fields[[4L]])
    extension <- toupper(tools::file_ext(file_name))
    if (!extension %in% c("CNV", "DBF")) {
      return(NULL)
    }
    # For CNV, field three is the starting position in the source variable.
    # For DBF, the same field names the column that contains the description.
    argument <- trimws(fields[[3L]])
    data.frame(
      order = i,
      command = command,
      description = trimws(substring(fields[[1L]], 2L)),
      field = toupper(trimws(fields[[2L]])),
      argument = argument,
      position = if (extension == "CNV") {
        suppressWarnings(as.integer(argument))
      } else {
        NA_integer_
      },
      file = file_name,
      extension = extension,
      stringsAsFactors = FALSE
    )
  })
  records <- Filter(Negate(is.null), records)
  if (!length(records)) {
    .tabwin_abort(
      "TabWin definition {.file {basename(path)}} contains no usable conversions.",
      "microdatasus_dictionary_invalid_error"
    )
  }
  do.call(rbind, records)
}

.tabwin_parse_increment_fields <- function(path) {
  lines <- .tabwin_read_text(path)
  fields <- vapply(lines, function(line) {
    # TabWin's I command declares a numeric increment/frequency. It is the
    # closest type metadata available in DEF and avoids hand-maintained CNES
    # quantity lists. Headings and expressions are discarded below.
    if (!nzchar(line) || toupper(substr(line, 1L, 1L)) != "I") {
      return(NA_character_)
    }
    pieces <- strsplit(line, ",", fixed = TRUE)[[1L]]
    if (length(pieces) < 2L) {
      return(NA_character_)
    }
    field <- toupper(trimws(pieces[[2L]]))
    if (!grepl("^[A-Z][A-Z0-9_]*$", field)) {
      return(NA_character_)
    }
    field
  }, character(1))
  unique(fields[!is.na(fields)])
}

.tabwin_find_entry <- function(entries, suffix) {
  # ZIP members and DEF references vary in path separator and letter case.
  entries_normalized <- gsub("\\\\", "/", entries)
  suffix <- gsub("\\\\", "/", suffix)
  entries_lower <- tolower(entries_normalized)
  suffix_lower <- tolower(sub("^/+", "", sub("^\\./", "", suffix)))
  # Match complete path components. A plain UF.CNV reference must not also
  # match REGUF.CNV, nor may ANO.CNV match MESANO.CNV.
  matches <- which(
    entries_lower == suffix_lower |
      endsWith(entries_lower, paste0("/", suffix_lower))
  )
  if (!length(matches)) {
    .tabwin_abort(
      "The TabWin archive contains no file matching {.file {suffix}}.",
      "microdatasus_dictionary_missing_error"
    )
  }
  if (length(matches) > 1L) {
    .tabwin_abort(
      "The TabWin archive contains multiple files matching {.file {suffix}}.",
      "microdatasus_dictionary_ambiguous_error"
    )
  }
  entries[[matches]]
}

.tabwin_filename_key <- function(path) {
  # Preserve ASCII basenames while replacing undecodable bytes in unrelated
  # legacy filenames, which prevents locale-dependent matching failures.
  converted <- iconv(
    basename(path),
    from = "",
    to = "UTF-8",
    sub = "?"
  )
  tolower(converted)
}

.tabwin_extract_legacy_entry <- function(dictionary, file_name, destination) {
  # libzip can list a legacy-encoded member but fail to select it by its
  # converted UTF-8 name. Extracting to an isolated directory preserves the
  # original member bytes and lets us identify the basename afterwards.
  directory <- tempfile("tabwin-legacy-", tmpdir = dictionary$cache_dir)
  if (!dir.create(directory)) return(FALSE)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  extracted <- tryCatch(
    zip::unzip(dictionary$archive, exdir = directory),
    error = function(error) NULL
  )
  if (is.null(extracted)) return(FALSE)
  candidates <- list.files(directory, recursive = TRUE, full.names = TRUE)
  matches <- which(
    .tabwin_filename_key(candidates) == .tabwin_filename_key(file_name)
  )
  if (length(matches) != 1L || file.size(candidates[[matches]]) == 0) {
    return(FALSE)
  }
  isTRUE(file.copy(candidates[[matches]], destination, overwrite = TRUE))
}

.tabwin_extract_entry <- function(dictionary, file_name) {
  # Conversion files are extracted only when a processor actually needs them.
  # The extracted copy remains in the session cache directory.
  if (isTRUE(dictionary$extracted_all)) {
    # Some official archives have legacy-encoded directory names. They are
    # flattened once at download time, so locate files case-insensitively.
    candidates <- list.files(dictionary$cache_dir, full.names = TRUE)
    matches <- which(
      .tabwin_filename_key(candidates) == .tabwin_filename_key(file_name)
    )
    if (!length(matches)) {
      .tabwin_abort(
        "The extracted TabWin file {.file {file_name}} is missing.",
        "microdatasus_dictionary_missing_error"
      )
    }
    if (length(matches) > 1L) {
      .tabwin_abort(
        "The extracted TabWin file {.file {file_name}} is ambiguous.",
        "microdatasus_dictionary_ambiguous_error"
      )
    }
    if (file.size(candidates[[matches]]) == 0) {
      .tabwin_abort(
        "The extracted TabWin file {.file {file_name}} is empty.",
        "microdatasus_dictionary_invalid_error"
      )
    }
    return(candidates[[matches]])
  }
  definition_dir <- dictionary$definition_dir
  relative_file <- if (definition_dir %in% c("", ".", "/")) {
    file_name
  } else {
    paste0(definition_dir, "/", file_name)
  }
  entry <- .tabwin_find_entry(
    dictionary$entries,
    relative_file
  )
  destination <- file.path(dictionary$cache_dir, basename(entry))
  if (file.exists(destination) && file.size(destination) > 0) {
    return(destination)
  }
  extracted <- tryCatch(
    zip::unzip(
      zipfile = dictionary$archive,
      files = entry,
      exdir = dictionary$cache_dir,
      junkpaths = TRUE,
      overwrite = TRUE
    ),
    error = identity
  )
  if (inherits(extracted, "error")) {
    recovered <- .tabwin_extract_legacy_entry(
      dictionary, file_name, destination
    )
    if (!recovered) {
      .tabwin_abort(c(
        "Failed to extract TabWin file {.file {file_name}}.",
        "i" = conditionMessage(extracted)
      ), "microdatasus_dictionary_relation_error")
    }
  }
  if (!file.exists(destination)) {
    .tabwin_abort(
      "The extracted TabWin file {.file {file_name}} is missing.",
      "microdatasus_dictionary_missing_error"
    )
  }
  if (file.size(destination) == 0) {
    .tabwin_abort(
      "The extracted TabWin file {.file {file_name}} is empty.",
      "microdatasus_dictionary_invalid_error"
    )
  }
  destination
}

.tabwin_alpha_to_number <- function(value) {
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    return(NA_real_)
  }

  characters <- utf8ToInt(toupper(value))
  if (!length(characters) || any(characters < 65L | characters > 90L)) {
    return(NA_real_)
  }
  sum((characters - 65L) * 26^(rev(seq_along(characters)) - 1L))
}

.tabwin_number_to_alpha <- function(number, width) {
  digits <- integer(width)
  for (index in rev(seq_len(width))) {
    digits[[index]] <- number %% 26
    number <- number %/% 26
  }
  intToUtf8(digits + 65L)
}

.tabwin_expand_range <- function(token, width, max_codes = 100000L) {
  # Short CNV codes may use compact intervals such as 01-09, A-Z, or A01-A05.
  token <- trimws(token)
  bounds <- strsplit(token, "-", fixed = TRUE)[[1L]]
  if (length(bounds) != 2L || any(!nzchar(bounds))) {
    return(token)
  }
  if (all(grepl("^[[:alpha:]]+$", bounds)) &&
      length(unique(nchar(bounds))) == 1L) {
    limits <- vapply(bounds, .tabwin_alpha_to_number, numeric(1))
    size <- limits[[2L]] - limits[[1L]] + 1
    if (anyNA(limits) || size < 1) return(token)
    if (size > max_codes) {
      cli::cli_abort(
        "TabWin alphabetic range {.val {token}} is too large to expand as labels."
      )
    }
    return(vapply(
      seq.int(limits[[1L]], limits[[2L]]),
      .tabwin_number_to_alpha,
      character(1),
      width = nchar(bounds[[1L]])
    ))
  }
  if (all(grepl("^[0-9]+$", bounds))) {
    limits <- suppressWarnings(as.numeric(bounds))
    if (anyNA(limits) || limits[[1L]] > limits[[2L]]) {
      return(token)
    }
    # Some official CNVs describe analytical bands covering tens of millions
    # of identifiers. They are not finite code-label dictionaries and must not
    # be expanded into an object large enough to exhaust the R session.
    if (limits[[2L]] - limits[[1L]] + 1 > max_codes) {
      cli::cli_abort(
        "TabWin numeric range {.val {token}} is too large to expand as labels."
      )
    }
    return(sprintf(
      paste0("%0", width, ".0f"),
      seq.int(limits[[1L]], limits[[2L]])
    ))
  }
  parsed <- regexec("^([[:alpha:]]*)([0-9]+)$", bounds)
  pieces <- regmatches(bounds, parsed)
  if (
    length(pieces[[1L]]) == 3L &&
      length(pieces[[2L]]) == 3L &&
      identical(toupper(pieces[[1L]][[2L]]), toupper(pieces[[2L]][[2L]]))
  ) {
    limits <- as.numeric(c(pieces[[1L]][[3L]], pieces[[2L]][[3L]]))
    if (!anyNA(limits) && limits[[1L]] <= limits[[2L]]) {
      if (limits[[2L]] - limits[[1L]] + 1 > max_codes) {
        cli::cli_abort(
          "TabWin alphanumeric range {.val {token}} is too large to expand as labels."
        )
      }
      prefix <- toupper(pieces[[1L]][[2L]])
      digits <- width - nchar(prefix)
      return(paste0(
        prefix,
        sprintf(
          paste0("%0", digits, ".0f"),
          seq.int(limits[[1L]], limits[[2L]])
        )
      ))
    }
  }
  token
}

.tabwin_range_rule <- function(token, width, mode = "") {
  # Parse a compact interval without materialising its members. Keeping the
  # bounds lets every processor handle large analytical TabWin bands safely.
  token <- trimws(token)
  bounds <- strsplit(token, "-", fixed = TRUE)[[1L]]
  if (identical(toupper(mode), "L") &&
      (startsWith(token, "-") || endsWith(token, "-"))) {
    separators <- gregexpr("-", token, fixed = TRUE)[[1L]]
    if (length(separators) != 1L || separators[[1L]] < 1L) return(NULL)
    return(data.frame(
      token = token, kind = "literal", prefix = "",
      lower = NA_real_, upper = NA_real_, size = Inf, width = width,
      stringsAsFactors = FALSE
    ))
  }
  if (length(bounds) != 2L || any(!nzchar(bounds))) return(NULL)
  if (all(grepl("^[[:alpha:]]+$", bounds)) &&
      length(unique(nchar(bounds))) == 1L) {
    limits <- vapply(bounds, .tabwin_alpha_to_number, numeric(1))
    if (anyNA(limits) || limits[[1L]] > limits[[2L]]) return(NULL)
    return(data.frame(
      token = token, kind = "alphabetic", prefix = "",
      lower = limits[[1L]], upper = limits[[2L]],
      size = limits[[2L]] - limits[[1L]] + 1, width = width,
      stringsAsFactors = FALSE
    ))
  }
  if (all(grepl("^[0-9]+$", bounds))) {
    limits <- suppressWarnings(as.numeric(bounds))
    if (anyNA(limits) || limits[[1L]] > limits[[2L]]) return(NULL)
    return(data.frame(
      token = token, kind = "numeric", prefix = "",
      lower = limits[[1L]], upper = limits[[2L]],
      size = limits[[2L]] - limits[[1L]] + 1, width = width,
      stringsAsFactors = FALSE
    ))
  }
  parsed <- regexec("^([[:alpha:]]*)([0-9]+)$", bounds)
  pieces <- regmatches(bounds, parsed)
  if (length(pieces[[1L]]) != 3L || length(pieces[[2L]]) != 3L ||
      !identical(toupper(pieces[[1L]][[2L]]),
                 toupper(pieces[[2L]][[2L]]))) return(NULL)
  limits <- as.numeric(c(pieces[[1L]][[3L]], pieces[[2L]][[3L]]))
  if (anyNA(limits) || limits[[1L]] > limits[[2L]]) return(NULL)
  data.frame(
    token = token, kind = "alphanumeric",
    prefix = toupper(pieces[[1L]][[2L]]),
    lower = limits[[1L]], upper = limits[[2L]],
    size = limits[[2L]] - limits[[1L]] + 1, width = width,
    stringsAsFactors = FALSE
  )
}

.tabwin_empty_ranges <- function() {
  data.frame(
    token = character(), kind = character(), prefix = character(),
    lower = numeric(), upper = numeric(), size = numeric(), width = integer(),
    label = character(), priority = integer(), stringsAsFactors = FALSE
  )
}

.tabwin_normalize_code <- function(code, width, mode = "") {
  code <- as.character(code)
  literal <- identical(toupper(mode), "L")
  if (literal) {
    # Literal fields are fixed-width and right padded. This keeps "1  " distinct
    # from "001", as required by the L mode.
    code <- sub("^[[:space:]]+", "", code)
    short <- !is.na(code) & nchar(code, type = "chars") < width
    code[short] <- paste0(
      code[short],
      vapply(
        width - nchar(code[short], type = "chars"),
        strrep,
        character(1),
        x = " "
      )
    )
    return(substr(code, 1L, width))
  }

  # Discard physical line padding beyond the declared code width. Spaces
  # inside that width remain significant: "1  " still denotes 100 at width 3.
  overlong <- !is.na(code) & nchar(code, type = "chars") > width
  suffix <- substring(code, width + 1L)
  physical_padding <- overlong & grepl("^[[:space:]]+$", suffix)
  code[physical_padding] <- substr(code[physical_padding], 1L, width)

  # In numeric mode, right-padding represents zeroes: both "1  " and "10 "
  # denote 100 at width three. Unpadded official tokens remain tolerated by
  # left-padding numeric codes, which also repairs DBF readers that drop zeroes.
  right_padded <- !is.na(code) & grepl("^[0-9]+[[:space:]]+$", code)
  code[right_padded] <- gsub(
    "[[:space:]]", "0", code[right_padded]
  )
  code <- trimws(code)
  numeric <- !is.na(code) & grepl("^[0-9]+$", code) & nchar(code) < width
  code[numeric] <- vapply(
    code[numeric],
    function(value) paste0(strrep("0", width - nchar(value)), value),
    character(1)
  )
  code
}

.tabwin_dbf_field_text <- function(value, type, width) {
  value <- as.character(value)
  missing <- is.na(value)
  value[missing] <- ""
  padding <- vapply(
    pmax.int(0L, width - nchar(value, type = "chars")),
    strrep,
    character(1),
    x = " "
  )
  if (identical(type, "C")) {
    value <- paste0(substr(value, 1L, width), padding)
    value <- substr(value, 1L, width)
  } else {
    value <- paste0(padding, value)
    value <- substr(
      value,
      pmax.int(1L, nchar(value, type = "chars") - width + 1L),
      nchar(value, type = "chars")
    )
  }
  value[missing] <- strrep(" ", width)
  value
}

.tabwin_definition_coverage <- function(values, definition, conversion) {
  observed <- unique(as.character(values))
  observed <- observed[!is.na(observed) & nzchar(trimws(observed))]
  if (!length(observed)) return(0)
  lookup <- observed
  if (identical(conversion$type, "cnv")) {
    start <- definition$position[[1L]]
    lookup <- substring(
      lookup, start, start + conversion$code_width - 1L
    )
    lookup <- .tabwin_normalize_code(
      lookup, conversion$code_width, conversion$mode
    )
  }
  mean(!is.na(.tabwin_conversion_labels(lookup, conversion)))
}

.tabwin_definition_values <- function(
  data,
  field,
  definition,
  conversion,
  values
) {
  if (is.null(data) || !conversion$type %in% c("cnv", "dbf")) {
    return(values)
  }
  widths <- attr(data, "dbf_field_widths", exact = TRUE)
  types <- attr(data, "dbf_field_types", exact = TRUE)
  if (is.null(widths) || is.null(types) || is.null(names(widths)) ||
      is.null(names(types))) {
    return(values)
  }
  layout_names <- names(widths)
  start <- match(toupper(field), toupper(layout_names))
  if (is.na(start)) return(values)
  position <- if (identical(conversion$type, "cnv")) {
    definition$position[[1L]]
  } else {
    1L
  }
  required <- position + conversion$code_width - 1L
  if (widths[[start]] >= required) return(values)

  cumulative <- cumsum(widths[seq.int(start, length(widths))])
  finish_offsets <- which(cumulative >= required)
  if (!length(finish_offsets)) return(values)
  finish <- start + finish_offsets[[1L]] - 1L
  physical_names <- layout_names[seq.int(start, finish)]
  actual <- match(toupper(physical_names), toupper(names(data)))
  if (anyNA(actual)) return(values)

  pieces <- lapply(seq_along(actual), function(index) {
    .tabwin_dbf_field_text(
      data[[actual[[index]]]],
      types[[physical_names[[index]]]],
      widths[[physical_names[[index]]]]
    )
  })
  physical <- substr(do.call(paste0, pieces), 1L, required)
  candidates <- list(direct = values, physical = physical)

  mode <- if (is.null(conversion$mode)) "" else toupper(conversion$mode)
  prefix_fallback <- identical(conversion$type, "cnv") &&
    !identical(mode, "F") && position == 1L
  if (prefix_fallback) {
    for (endpoint in seq.int(start, max(start, finish - 1L))) {
      prefix_names <- layout_names[seq.int(start, endpoint)]
      prefix_actual <- match(toupper(prefix_names), toupper(names(data)))
      if (anyNA(prefix_actual)) next
      prefix <- lapply(prefix_actual, function(index) {
        value <- as.character(data[[index]])
        value[is.na(value)] <- ""
        trimws(value)
      })
      padded <- do.call(paste0, prefix)
      numeric <- grepl("^[0-9]+$", padded) &
        nchar(padded, type = "chars") < conversion$code_width
      padded[numeric] <- vapply(
        padded[numeric],
        function(value) {
          zeros <- strrep(
            "0", conversion$code_width - nchar(value, type = "chars")
          )
          if (identical(mode, "L")) {
            paste0(zeros, value)
          } else {
            paste0(value, zeros)
          }
        },
        character(1)
      )
      candidates[[paste0("padded_", endpoint)]] <- padded
    }
  }

  coverage <- vapply(
    candidates,
    .tabwin_definition_coverage,
    numeric(1),
    definition = definition,
    conversion = conversion
  )
  candidates[[which.max(coverage)]]
}

.tabwin_parse_cnv_header <- function(lines, path) {
  trimmed <- trimws(lines)
  useful <- which(
    nzchar(trimmed) &
      !startsWith(trimmed, ";") &
      !startsWith(trimmed, ":")
  )
  if (!length(useful)) {
    .tabwin_abort(
      "TabWin conversion {.file {basename(path)}} is empty.",
      "microdatasus_dictionary_invalid_error"
    )
  }
  header_index <- useful[[1L]]
  header_line <- lines[[header_index]]
  header <- toupper(trimmed[[header_index]])
  match <- regexec(
    "^(?:([NS])[[:space:]]+)?([0-9]+)[[:space:]]+([0-9]+)(?:[[:space:]]+([[:alpha:]]+))?[[:space:]]*$",
    header,
    perl = TRUE
  )
  parts <- regmatches(header, match)[[1L]]
  embedded_row <- NULL

  if (length(parts) != 5L) {
    # A published SINASC relation overwrites the first row prefix with its
    # header while leaving that row code in column 61. Accept only this exact
    # shape: a valid short header, an empty description area, and a code tail.
    embedded_header <- toupper(trimws(substr(header_line, 1L, 10L)))
    embedded_match <- regexec(
      "^([0-9]+)[[:space:]]+([0-9]+)(?:[[:space:]]+([[:alpha:]]+))?$",
      embedded_header,
      perl = TRUE
    )
    embedded_parts <- regmatches(embedded_header, embedded_match)[[1L]]
    middle <- substr(header_line, 11L, 60L)
    tail <- substring(header_line, 61L)
    if (length(embedded_parts) != 4L || nzchar(trimws(middle)) ||
        !nzchar(trimws(tail))) {
      .tabwin_abort(
        "TabWin conversion {.file {basename(path)}} has an invalid header.",
        "microdatasus_dictionary_invalid_error"
      )
    }
    parts <- c(
      embedded_parts[[1L]], "", embedded_parts[[2L]],
      embedded_parts[[3L]], embedded_parts[[4L]]
    )
    embedded_row <- paste0(strrep(" ", 60L), tail)
  }

  dialect <- parts[[2L]]
  category_count <- suppressWarnings(as.integer(parts[[3L]]))
  code_width <- suppressWarnings(as.integer(parts[[4L]]))
  mode <- parts[[5L]]
  if (identical(mode, "FAIXAS")) mode <- "F"
  if (is.na(category_count) || category_count < 1L ||
      is.na(code_width) || code_width < 1L ||
      !mode %in% c("", "L", "F")) {
    .tabwin_abort(
      "TabWin conversion {.file {basename(path)}} has unsupported header values.",
      "microdatasus_dictionary_invalid_error"
    )
  }
  list(
    index = header_index,
    dialect = dialect,
    category_count = category_count,
    code_width = code_width,
    mode = mode,
    embedded_row = embedded_row
  )
}

.tabwin_category_key <- function(sequence, subtotal) {
  key <- paste(subtotal, sequence, sep = "\034")
  key[!nzchar(sequence)] <- ""
  key
}

.tabwin_category_metadata <- function(sequence, subtotal, labels) {
  key <- .tabwin_category_key(sequence, subtotal)
  unique_key <- unique(key[nzchar(key)])
  if (!length(unique_key)) {
    return(data.frame(
      sequence = character(), subtotal = character(), label = character(),
      source_order = integer(), label_conflict = logical(),
      stringsAsFactors = FALSE
    ))
  }

  source_order <- match(unique_key, key)
  label_present <- nzchar(key) & nzchar(labels)
  label_key <- key[label_present]
  label_value <- labels[label_present]
  label_index <- match(unique_key, label_key)
  first_label <- label_value[label_index]

  distinct_pair <- !duplicated(paste(label_key, label_value, sep = "\035"))
  distinct_count <- tabulate(
    match(label_key[distinct_pair], unique_key),
    nbins = length(unique_key)
  )
  categories <- data.frame(
    sequence = sequence[source_order],
    subtotal = subtotal[source_order],
    label = first_label,
    source_order = source_order,
    label_conflict = distinct_count > 1L,
    stringsAsFactors = FALSE
  )
  numeric_sequence <- suppressWarnings(as.integer(categories$sequence))
  categories[order(numeric_sequence, categories$source_order), , drop = FALSE]
}

.tabwin_cnv_code_text <- function(rows, code_start, code_width, mode) {
  code_text <- sub(";.*$", "", substring(rows, code_start))
  line_width <- nchar(rows, type = "chars")
  candidate_start <- pmax.int(1L, line_width - code_width + 1L)
  candidate <- substring(rows, candidate_start)
  candidate_prefix <- substr(rows, 1L, pmax.int(0L, candidate_start - 1L))
  candidate_trimmed <- trimws(candidate)
  candidate_valid <- grepl(
    "^[[:alnum:]][[:alnum:]-]*(?:[[:space:]]*,[[:space:]]*[[:alnum:]][[:alnum:]-]*)*$",
    candidate_trimmed,
    perl = TRUE
  )
  separated <- grepl("[[:space:]]{2,}$", candidate_prefix)
  recover <- !identical(mode, "L") &
    line_width < code_start &
    !nzchar(trimws(code_text)) &
    candidate_start > 9L &
    candidate_valid &
    separated
  code_text[recover] <- candidate[recover]
  attr(code_text, "compact") <- recover
  attr(code_text, "candidate_start") <- candidate_start
  attr(code_text, "compact_rows") <- sum(recover)
  code_text
}

.tabwin_empty_thresholds <- function() {
  data.frame(
    upper = numeric(), label = character(), priority = integer(),
    stringsAsFactors = FALSE
  )
}

.tabwin_parse_cnv <- function(path) {
  lines <- .tabwin_read_text(path)
  source_encoding <- attr(lines, "encoding")
  tabs_recovered <- attr(lines, "tabs_recovered")
  header <- .tabwin_parse_cnv_header(lines, path)
  dialect <- header$dialect
  category_count <- header$category_count
  code_width <- header$code_width
  mode <- header$mode

  rows <- if (header$index < length(lines)) {
    lines[seq.int(header$index + 1L, length(lines))]
  } else {
    character()
  }
  if (!is.null(header$embedded_row)) {
    rows <- c(header$embedded_row, rows)
  }
  active <- nzchar(trimws(rows)) &
    !startsWith(trimws(rows), ";") &
    !startsWith(trimws(rows), ":")
  rows <- rows[active]
  extended <- identical(dialect, "N")
  subtotal_end <- if (extended) 4L else 3L
  sequence_start <- if (extended) 6L else 4L
  sequence_end <- if (extended) 9L else 7L
  label_start <- if (extended) 12L else 10L
  label_end <- if (extended) 111L else 59L
  code_start <- if (extended) 113L else 61L

  subtotal <- trimws(substr(rows, 1L, subtotal_end))
  sequence <- trimws(substr(rows, sequence_start, sequence_end))
  code_text <- .tabwin_cnv_code_text(rows, code_start, code_width, mode)
  compact <- attr(code_text, "compact")
  compact_start <- attr(code_text, "candidate_start")
  compact_code_rows <- attr(code_text, "compact_rows")
  row_labels <- trimws(substr(rows, label_start, label_end))
  if (any(compact)) {
    row_labels[compact] <- trimws(substr(
      rows[compact], label_start, compact_start[compact] - 1L
    ))
  }

  # Some published files omit a repeated sequence on a physical continuation
  # line. Recover only that local omission; descriptions are still resolved by
  # subtotal and sequence in a separate pass, preserving non-adjacent continuations.
  recovered_sequence <- 0L
  if (length(sequence)) {
    current <- ""
    for (index in seq_along(sequence)) {
      if (nzchar(sequence[[index]])) {
        current <- sequence[[index]]
      } else if (nzchar(current) && nzchar(trimws(code_text[[index]]))) {
        sequence[[index]] <- current
        recovered_sequence <- recovered_sequence + 1L
      }
    }
  }
  recovered_leading_sequence <- 0L
  first_sequence <- which(nzchar(sequence))
  if (length(first_sequence) && first_sequence[[1L]] > 1L) {
    first_sequence <- first_sequence[[1L]]
    leading <- seq_len(first_sequence - 1L)
    has_codes <- nzchar(trimws(code_text[leading]))
    recover <- leading[!nzchar(sequence[leading]) & has_codes]
    if (length(recover)) {
      sequence[recover] <- sequence[[first_sequence]]
      recovered_leading_sequence <- length(recover)
    }
  }
  invalid_sequence <- nzchar(sequence) & !grepl("^[0-9]+$", sequence)
  if (any(invalid_sequence)) {
    .tabwin_abort(
      "TabWin conversion {.file {basename(path)}} has an invalid category sequence.",
      "microdatasus_dictionary_invalid_error"
    )
  }

  categories <- .tabwin_category_metadata(sequence, subtotal, row_labels)
  category_keys <- .tabwin_category_key(
    categories$sequence, categories$subtotal
  )
  label_lookup <- stats::setNames(categories$label, category_keys)
  row_category_keys <- .tabwin_category_key(sequence, subtotal)
  resolved_labels <- unname(label_lookup[row_category_keys])
  tokens <- strsplit(code_text, ",", fixed = TRUE)
  tokens <- lapply(tokens, function(values) {
    if (identical(mode, "L")) {
      # Literal blanks are valid fixed-width codes in published CNVs.
      return(values[nzchar(values)])
    }
    # Leading whitespace aligns numeric code lists. Trailing whitespace is
    # significant and is converted to zeroes during numeric normalization.
    values <- sub("^[[:space:]]+", "", values)
    values[nzchar(trimws(values))]
  })
  unresolved <- lengths(tokens) > 0L &
    (is.na(resolved_labels) | !nzchar(resolved_labels))
  if (any(unresolved)) {
    .tabwin_abort(
      "TabWin conversion {.file {basename(path)}} has codes without a category description.",
      "microdatasus_dictionary_invalid_error"
    )
  }

  code_parts <- lapply(seq_along(tokens), function(index) tokens[[index]])
  label_parts <- lapply(seq_along(tokens), function(index) {
    rep(resolved_labels[[index]], length(tokens[[index]]))
  })
  raw_codes <- unlist(code_parts, use.names = FALSE)
  raw_labels <- unlist(label_parts, use.names = FALSE)
  if (is.null(raw_codes)) raw_codes <- character()
  if (is.null(raw_labels)) raw_labels <- character()

  levels <- unique(categories$label[!is.na(categories$label)])
  common <- list(
    type = "cnv",
    dialect = dialect,
    mode = mode,
    code_width = code_width,
    category_count = category_count,
    observed_category_count = nrow(categories),
    category_count_mismatch = category_count != nrow(categories),
    categories = categories,
    levels = levels,
    source_encoding = source_encoding,
    tabs_recovered = tabs_recovered,
    embedded_header = !is.null(header$embedded_row),
    compact_code_rows = compact_code_rows,
    recovered_sequence = recovered_sequence,
    recovered_leading_sequence = recovered_leading_sequence
  )

  if (identical(mode, "F")) {
    upper <- suppressWarnings(as.numeric(trimws(raw_codes)))
    if (!length(upper) || anyNA(upper)) {
      .tabwin_abort(
        "Numeric-range TabWin conversion {.file {basename(path)}} has invalid limits.",
        "microdatasus_dictionary_invalid_error"
      )
    }
    thresholds <- data.frame(
      upper = upper,
      label = raw_labels,
      priority = seq_along(upper),
      stringsAsFactors = FALSE
    )
    thresholds <- thresholds[order(thresholds$upper, thresholds$priority), ]
    if (anyDuplicated(thresholds$upper)) {
      .tabwin_abort(
        "Numeric-range TabWin conversion {.file {basename(path)}} has duplicate limits.",
        "microdatasus_dictionary_invalid_error"
      )
    }
    return(structure(
      c(common, list(
        map = stats::setNames(character(), character()),
        map_priority = stats::setNames(integer(), character()),
        ranges = .tabwin_empty_ranges(),
        thresholds = thresholds
      )),
      class = "microdatasus_tabwin_conversion"
    ))
  }

  expanded <- as.list(raw_codes)
  range_indexes <- which(grepl("-", trimws(raw_codes), fixed = TRUE))
  parsed_ranges <- lapply(
    raw_codes[range_indexes], .tabwin_range_rule, width = code_width,
    mode = mode
  )
  symbolic <- vapply(
    parsed_ranges,
    function(rule) !is.null(rule) && rule$size[[1L]] > 100000,
    logical(1)
  )
  symbolic_indexes <- range_indexes[symbolic]
  rules <- .tabwin_empty_ranges()
  if (length(symbolic_indexes)) {
    rules <- do.call(rbind, parsed_ranges[symbolic])
    rules$label <- raw_labels[symbolic_indexes]
    rules$priority <- symbolic_indexes
    expanded[symbolic_indexes] <- rep(
      list(character()), length(symbolic_indexes)
    )
  }
  materialized_indexes <- setdiff(range_indexes, symbolic_indexes)
  expanded[materialized_indexes] <- lapply(
    raw_codes[materialized_indexes],
    .tabwin_expand_range,
    width = code_width
  )
  map_codes <- .tabwin_normalize_code(
    unlist(expanded, use.names = FALSE), code_width, mode
  )
  map_labels <- rep(raw_labels, lengths(expanded))
  map_priorities <- rep(seq_along(raw_codes), lengths(expanded))
  normalized_collisions <- sum(duplicated(map_codes))
  keep <- !duplicated(map_codes, fromLast = TRUE)
  map <- map_labels[keep]
  names(map) <- map_codes[keep]
  map_priority <- map_priorities[keep]
  names(map_priority) <- map_codes[keep]
  if (!length(map) && !nrow(rules)) {
    .tabwin_abort(
      "TabWin conversion {.file {basename(path)}} contains no code labels.",
      "microdatasus_dictionary_invalid_error"
    )
  }
  structure(
    c(common, list(
      map = map,
      map_priority = map_priority,
      ranges = rules,
      thresholds = .tabwin_empty_thresholds(),
      normalized_collisions = normalized_collisions
    )),
    class = "microdatasus_tabwin_conversion"
  )
}

.tabwin_conversion_key <- function(definition) {
  file <- tolower(definition$file)
  if (identical(definition$extension, "CNV")) {
    # A CNV map depends only on the relation file; source field and substring
    # position affect application, not parsing, so all definitions can share it.
    return(paste(file, "CNV", sep = "::"))
  }
  paste(
    file,
    toupper(definition$field),
    toupper(definition$argument),
    sep = "::"
  )
}

.tabwin_parser_version <- 8L

.tabwin_conversion_cache_path <- function(dictionary, key) {
  if (!isTRUE(dictionary$persistent) || is.null(dictionary$archive_checksum)) {
    return(NULL)
  }
  component <- .datasus_cache_component(key)
  # Retain a deterministic suffix when long official names need truncation.
  hash <- 0
  for (byte in as.integer(charToRaw(enc2utf8(key)))) {
    hash <- (hash * 131 + byte) %% 2147483629
  }
  suffix <- sprintf("-%08x", as.integer(hash))
  if (nchar(component) > 170L) component <- substr(component, 1L, 170L)
  component <- paste0(component, suffix)
  file.path(
    dictionary$cache_dir, "parsed", dictionary$archive_checksum,
    paste0(component, ".rds")
  )
}

.tabwin_read_cached_conversion <- function(dictionary, key) {
  path <- .tabwin_conversion_cache_path(dictionary, key)
  if (is.null(path) || !file.exists(path)) return(NULL)
  payload <- tryCatch(readRDS(path), error = function(error) NULL)
  if (is.null(payload) ||
      !identical(payload$parser_version, .tabwin_parser_version) ||
      !identical(payload$archive_checksum, dictionary$archive_checksum) ||
      !inherits(payload$conversion, "microdatasus_tabwin_conversion")) {
    return(NULL)
  }
  payload$conversion
}

.tabwin_write_cached_conversion <- function(dictionary, key, conversion) {
  path <- .tabwin_conversion_cache_path(dictionary, key)
  if (is.null(path)) return(invisible(NULL))
  if (!dir.exists(dirname(path)) && !dir.create(dirname(path), recursive = TRUE)) {
    return(invisible(NULL))
  }
  temporary <- .datasus_temporary_path(path)
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(list(
    parser_version = .tabwin_parser_version,
    archive_checksum = dictionary$archive_checksum,
    conversion = conversion
  ), temporary, version = 2, compress = FALSE)
  tryCatch(.datasus_commit_file(temporary, path), error = function(error) NULL)
  invisible(path)
}

.tabwin_match_dbf_field <- function(field, table_names) {
  if (length(field) != 1L || is.na(field) || !nzchar(trimws(field))) {
    return(NA_integer_)
  }
  requested <- toupper(trimws(field))
  names_upper <- toupper(table_names)
  exact <- which(names_upper == requested)
  if (length(exact) == 1L) return(exact)
  if (length(exact) > 1L) return(NA_integer_)

  # dBase III field names occupy at most ten non-NUL bytes. Some official DEFs
  # retain the longer logical name, and a few append an ordinal such as " 1".
  # Resolve only those deterministic physical representations; never fuzzy
  # match a different semantic name.
  logical <- sub("[[:space:]]+[0-9]+$", "", requested)
  aliases <- logical
  if (nchar(logical, type = "bytes") > 10L &&
      identical(logical, iconv(logical, to = "ASCII"))) {
    aliases <- c(aliases, substr(logical, 1L, 10L))
  }
  matches <- which(names_upper %in% unique(aliases))
  if (length(matches) == 1L) matches else NA_integer_
}

.tabwin_dbf_encoding <- function(path) {
  header <- readBin(path, what = "raw", n = 32L)
  if (length(header) < 30L) {
    .tabwin_abort(
      "TabWin table {.file {basename(path)}} has a truncated DBF header.",
      "microdatasus_dictionary_invalid_error"
    )
  }
  language_driver <- as.integer(header[[30L]])
  list(
    language_driver = language_driver,
    encoding = .dbc_resolve_encoding("auto", language_driver)
  )
}

.tabwin_decode_dbf_values <- function(value, metadata, context, path) {
  decoded <- .dbc_decode_text_auto(
    as.character(value),
    metadata$encoding,
    metadata$language_driver,
    context,
    path
  )
  attr(decoded, "dbc_encoding_used") <- NULL
  decoded
}

.tabwin_read_conversion <- function(dictionary, definition) {
  key <- .tabwin_conversion_key(definition)
  # Parsed maps are memoised separately from extracted files. The nested
  # environment is shared even when the dictionary list is copied by R.
  if (exists(key, envir = dictionary$conversions, inherits = FALSE)) {
    return(get(key, envir = dictionary$conversions, inherits = FALSE))
  }
  cached <- .tabwin_read_cached_conversion(dictionary, key)
  if (!is.null(cached)) {
    assign(key, cached, envir = dictionary$conversions)
    return(cached)
  }
  path <- .tabwin_extract_entry(dictionary, definition$file)
  if (identical(definition$extension, "CNV")) {
    conversion <- .tabwin_parse_cnv(path)
  } else {
    table <- tryCatch(
      # Some official DBFs contain blank legacy fields. Their name-repair
      # messages are not relevant to the code and label columns selected here.
      suppressMessages(foreign::read.dbf(path, as.is = TRUE)),
      error = function(error) {
        .tabwin_abort(c(
          "Failed to read TabWin table {.file {definition$file}}.",
          "i" = conditionMessage(error)
        ), "microdatasus_dictionary_relation_error")
      }
    )
    dbf_encoding <- .tabwin_dbf_encoding(path)
    code_index <- .tabwin_match_dbf_field(definition$field, names(table))
    # The TabWin specification uses the first DBF field when the related table
    # does not repeat the source field name.
    if (is.na(code_index)) {
      code_index <- 1L
    }
    label_index <- .tabwin_match_dbf_field(
      definition$argument, names(table)
    )
    fallback_label <- FALSE
    if (is.na(label_index)) {
      # Some official DEF rows retain an old description-column name after a
      # two-column DBF was revised. The sole non-key column is unambiguous.
      candidates <- setdiff(seq_along(table), code_index)
      if (length(candidates) == 1L) {
        label_index <- candidates[[1L]]
        fallback_label <- TRUE
      } else {
        .tabwin_abort(
          "TabWin table {.file {definition$file}} has no field {.field {definition$argument}}.",
          "microdatasus_dictionary_invalid_error"
        )
      }
    }
    codes <- trimws(.tabwin_decode_dbf_values(
      table[[code_index]], dbf_encoding,
      sprintf("TabWin DBF key field %s", sQuote(names(table)[[code_index]])),
      path
    ))
    labels <- .tabwin_decode_dbf_values(
      table[[label_index]], dbf_encoding,
      sprintf("TabWin DBF label field %s", sQuote(names(table)[[label_index]])),
      path
    )
    keep <- !is.na(codes) & nzchar(codes) & !duplicated(codes)
    map <- labels[keep]
    names(map) <- codes[keep]
    conversion <- structure(
      list(
        type = "dbf",
        mode = "",
        code_width = max(nchar(codes[keep]), 0L),
        category_count = sum(keep),
        levels = unique(labels[keep]),
        map = map,
        map_priority = stats::setNames(rep(0L, length(map)), names(map)),
        ranges = .tabwin_empty_ranges(),
        thresholds = .tabwin_empty_thresholds(),
        fallback_label = fallback_label,
        requested_label_field = definition$argument,
        label_field = names(table)[[label_index]],
        source_encoding = dbf_encoding$encoding,
        language_driver = dbf_encoding$language_driver
      ),
      class = "microdatasus_tabwin_conversion"
    )
  }
  assign(key, conversion, envir = dictionary$conversions)
  .tabwin_write_cached_conversion(dictionary, key, conversion)
  conversion
}

.tabwin_range_matches <- function(values, rule, mode = "") {
  values <- .tabwin_normalize_code(values, rule$width[[1L]], mode)
  prefix <- rule$prefix[[1L]]
  kind <- rule$kind[[1L]]
  if (identical(kind, "literal")) {
    token <- rule$token[[1L]]
    separator <- regexpr("-", token, fixed = TRUE)[[1L]]
    lower <- substr(token, 1L, separator - 1L)
    upper <- substring(token, separator + 1L)
    matched <- !is.na(values)
    if (nzchar(lower)) {
      lower <- .tabwin_normalize_code(lower, rule$width[[1L]], "L")
      matched <- matched & values >= lower
    }
    if (nzchar(upper)) {
      upper <- .tabwin_normalize_code(upper, rule$width[[1L]], "L")
      matched <- matched & values <= upper
    }
    return(matched)
  }
  if (identical(kind, "numeric")) {
    comparable <- grepl("^[0-9]+$", values)
    number <- suppressWarnings(as.numeric(values))
  } else if (identical(kind, "alphabetic")) {
    comparable <- grepl("^[[:alpha:]]+$", values)
    number <- vapply(values, .tabwin_alpha_to_number, numeric(1))
  } else {
    comparable <- startsWith(toupper(values), prefix)
    suffix <- substring(values, nchar(prefix) + 1L)
    comparable <- comparable & grepl("^[0-9]+$", suffix)
    number <- suppressWarnings(as.numeric(suffix))
  }
  !is.na(values) & comparable & !is.na(number) &
    number >= rule$lower[[1L]] & number <= rule$upper[[1L]]
}

.tabwin_threshold_labels <- function(lookup, thresholds) {
  number <- suppressWarnings(as.numeric(trimws(lookup)))
  labels <- rep(NA_character_, length(number))
  valid <- which(!is.na(number))
  for (index in valid) {
    category <- which(number[[index]] <= thresholds$upper)[1L]
    if (!is.na(category)) labels[[index]] <- thresholds$label[[category]]
  }
  labels
}

.tabwin_conversion_labels <- function(lookup, conversion) {
  mode <- if (is.null(conversion$mode)) "" else conversion$mode
  if (identical(mode, "F")) {
    return(.tabwin_threshold_labels(lookup, conversion$thresholds))
  }
  labels <- unname(conversion$map[lookup])
  map_priority <- conversion$map_priority
  if (is.null(map_priority)) {
    map_priority <- stats::setNames(rep(0L, length(conversion$map)),
                                    names(conversion$map))
  }
  priorities <- unname(map_priority[lookup])
  priorities[is.na(priorities)] <- -Inf
  ranges <- conversion$ranges
  if (is.null(ranges) || !nrow(ranges)) return(labels)
  # Later source rules override earlier broad intervals, matching TabWin.
  for (index in seq_len(nrow(ranges))) {
    rule <- ranges[index, , drop = FALSE]
    matched <- .tabwin_range_matches(lookup, rule, mode) &
      rule$priority[[1L]] >= priorities
    labels[matched] <- rule$label[[1L]]
    priorities[matched] <- rule$priority[[1L]]
  }
  labels
}

.tabwin_range_is_catch_all <- function(rule) {
  # Catch-all bands are useful as a final display fallback, but they provide no
  # evidence that a relation is the right one for a field. In particular, an
  # unrelated 000000-999999 rule must not outrank a table containing the exact
  # observed codes.
  if (!identical(rule$kind[[1L]], "numeric") ||
      !identical(rule$prefix[[1L]], "")) {
    return(FALSE)
  }
  width <- rule$width[[1L]]
  if (is.na(width) || width < 1L || width > 15L) return(FALSE)
  rule$lower[[1L]] <= 0 && rule$upper[[1L]] >= (10^width - 1)
}

.tabwin_specific_conversion_labels <- function(lookup, conversion) {
  # Exclude only full-domain fallbacks. Literal codes and bounded analytical
  # ranges both remain positive evidence when alternative DEF rows compete.
  ranges <- conversion$ranges
  if (is.null(ranges) || !nrow(ranges)) {
    return(.tabwin_conversion_labels(lookup, conversion))
  }
  catch_all <- vapply(
    seq_len(nrow(ranges)),
    function(index) .tabwin_range_is_catch_all(ranges[index, , drop = FALSE]),
    logical(1)
  )
  if (!any(catch_all)) {
    return(.tabwin_conversion_labels(lookup, conversion))
  }
  specific <- conversion
  specific$ranges <- ranges[!catch_all, , drop = FALSE]
  .tabwin_conversion_labels(lookup, specific)
}

.tabwin_relation_revision <- function(path) {
  # Only a two- or four-digit suffix preceded by a non-digit is a plausible
  # year/revision (CNES26, CNES2026). Domain identifiers such as P040605 are
  # codes, not versions.
  stem <- tools::file_path_sans_ext(basename(path))
  match <- regexec("^.*[^0-9]([0-9]{2}|[0-9]{4})$", stem)
  parts <- regmatches(stem, match)[[1L]]
  if (length(parts) != 2L) return(-Inf)
  suppressWarnings(as.numeric(parts[[2L]]))
}

.tabwin_score_definition <- function(
  dictionary,
  definition,
  values,
  data = NULL,
  source_field = definition$field[[1L]]
) {
  # A source field can have direct labels and several analytical groupings.
  # Score each usable definition against the codes actually present in data.
  conversion <- tryCatch(
    .tabwin_read_conversion(dictionary, definition),
    error = identity
  )
  if (inherits(conversion, "error")) {
    return(NULL)
  }
  values <- .tabwin_definition_values(
    data, source_field, definition, conversion, values
  )
  observed <- unique(as.character(values))
  observed <- observed[!is.na(observed) & nzchar(trimws(observed))]
  if (identical(conversion$type, "cnv")) {
    start <- definition$position
    observed <- substring(observed, start, start + conversion$code_width - 1L)
    observed <- .tabwin_normalize_code(
      observed, conversion$code_width, conversion$mode
    )
  }
  coverage <- if (length(observed)) {
    mean(!is.na(.tabwin_conversion_labels(observed, conversion)))
  } else {
    0
  }
  specific_coverage <- if (length(observed)) {
    mean(!is.na(.tabwin_specific_conversion_labels(observed, conversion)))
  } else {
    0
  }
  stem <- toupper(tools::file_path_sans_ext(basename(definition$file)))
  exact_name <- identical(gsub("[^A-Z0-9]", "", stem), definition$field)
  list(
    definition = definition,
    conversion = conversion,
    exact_name = exact_name,
    # D controls simultaneous line/table display in TabWin; it does not mean
    # that the related table is more detailed than an L/S/C alternative.
    direct_command = definition$command == "X",
    national_relation = identical(conversion$type, "dbf") &&
      grepl("BR$", stem),
    revision = .tabwin_relation_revision(definition$file),
    specific_coverage = specific_coverage,
    coverage = coverage,
    codes = length(conversion$map),
    categories = conversion$category_count
  )
}

.tabwin_select_conversion <- function(
  dictionary,
  field,
  values,
  data = NULL,
  source_field = field
) {
  definitions <- dictionary$definitions
  candidates <- definitions[
    definitions$field == toupper(field) &
      (definitions$extension == "DBF" |
        (!is.na(definitions$position) & definitions$position >= 1L)),
    ,
    drop = FALSE
  ]
  if (!nrow(candidates)) {
    return(NULL)
  }
  # Score every usable relation independently. Official DEFs sometimes name a
  # missing DBF while also declaring working CNV alternatives, and no command
  # letter or file extension is sufficient evidence to discard those fallbacks.
  scores <- lapply(seq_len(nrow(candidates)), function(i) {
    .tabwin_score_definition(
      dictionary,
      candidates[i, , drop = FALSE],
      values,
      data,
      source_field
    )
  })
  scores <- Filter(Negate(is.null), scores)
  if (!length(scores)) {
    return(NULL)
  }
  ranking <- data.frame(
    exact_name = vapply(scores, `[[`, logical(1), "exact_name"),
    direct_command = vapply(scores, `[[`, logical(1), "direct_command"),
    national_relation = vapply(scores, `[[`, logical(1), "national_relation"),
    revision = vapply(scores, `[[`, numeric(1), "revision"),
    specific_coverage = vapply(scores, `[[`, numeric(1), "specific_coverage"),
    coverage = vapply(scores, `[[`, numeric(1), "coverage"),
    codes = vapply(scores, `[[`, integer(1), "codes"),
    categories = vapply(scores, `[[`, integer(1), "categories")
  )
  # Specific matches are the strongest evidence. Overall coverage follows so
  # bounded ranges remain useful, while homonymous/X/national/current relations
  # resolve only genuine ties. Source order is the final deterministic tie-break.
  best <- order(
    -ranking$specific_coverage,
    -ranking$coverage,
    -ranking$exact_name,
    -ranking$direct_command,
    -ranking$national_relation,
    -ranking$revision,
    -ranking$codes,
    -ranking$categories
  )[[1L]]
  selected <- scores[[best]]
  selected$source_values <- .tabwin_definition_values(
    data,
    source_field,
    selected$definition,
    selected$conversion,
    values
  )
  selected
}

# Return converted text without allocating factor levels. Historical batch
# processing combines every row period before factorizing the complete field.
.tabwin_apply_conversion_values <- function(values, selected) {
  definition <- selected$definition
  conversion <- selected$conversion
  raw_source <- as.character(values)
  source <- trimws(raw_source)
  lookup <- raw_source
  if (identical(conversion$type, "cnv")) {
    lookup <- substring(
      lookup,
      definition$position,
      definition$position + conversion$code_width - 1L
    )
    lookup <- .tabwin_normalize_code(
      lookup, conversion$code_width, conversion$mode
    )
  }
  # Replace known codes only. Unknown codes remain visible so a DataSUS
  # revision cannot silently turn valid information into missing data.
  labels <- .tabwin_conversion_labels(lookup, conversion)
  result <- source
  matched <- !is.na(labels)
  result[matched] <- labels[matched]
  result[is.na(values)] <- NA_character_
  result
}

.tabwin_factor <- function(values, conversion_levels = character()) {
  present <- unique(values[!is.na(values)])
  ordered <- conversion_levels[conversion_levels %in% present]
  unknown <- present[!present %in% ordered]
  factor(values, levels = unique(c(ordered, unknown)))
}

# Preserve the factor return used by the original single-conversion helper.
.tabwin_apply_conversion <- function(values, selected) {
  values <- .tabwin_apply_conversion_values(values, selected)
  levels <- selected$conversion$levels
  if (is.null(levels)) levels <- unique(unname(selected$conversion$map))
  .tabwin_factor(values, levels)
}
