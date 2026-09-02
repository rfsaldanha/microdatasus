# The package namespace lives for the whole R session, so this private
# environment provides a session cache without writing permanent user files.
.tabwin_cache <- new.env(parent = emptyenv())

.tabwin_archive_cache <- new.env(parent = emptyenv())

# The five mortality products are subsets of the same death-certificate
# CID-10 database and use the same TabWin archive and DEF. The archive key
# allows all five dictionaries to share one download during the R session.
.tabwin_registry <- function() {
  sim_types <- .sim_information_systems
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
  specs[["SIM-DO-CID9"]] <- list(
    archive_key = "SIM-OBITOS-CID9",
    information_system = "SIM-DO-CID9",
    url = paste0(
      "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID9/TAB/",
      "OBITOS_CID9_TAB.zip"
    ),
    definition = "/OBITO.DEF"
  )

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
  # Official archives mix UTF-8, CP1252, CP850, and CP860, and occasionally
  # multiple legacy code pages in one file. Split on ASCII line bytes before decoding so each
  # physical row can retain independent encoding evidence.
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
  Encoding(encoded) <- "bytes"
  lines <- strsplit(
    encoded, "\n", fixed = TRUE, useBytes = TRUE
  )[[1L]]
  lines <- unlist(lapply(lines, function(line) {
    strsplit(line, "\r", fixed = TRUE, useBytes = TRUE)[[1L]]
  }), use.names = FALSE)
  Encoding(lines) <- "unknown"

  utf8 <- stringi::stri_enc_isutf8(lines)
  if (all(utf8)) {
    lines <- iconv(lines, from = "UTF-8", to = "UTF-8")
    encoding <- "UTF-8"
  } else {
    lines <- suppressWarnings(.dbc_decode_text_auto(
      lines,
      "CP1252",
      0L,
      paste0("TabWin file ", basename(path)),
      path
    ))
    encoding <- attr(lines, "dbc_encoding_used", exact = TRUE)
    if (identical(encoding, "CP1252")) encoding <- "windows-1252"
    attr(lines, "dbc_encoding_used") <- NULL
  }
  unresolved <- is.na(lines) | Encoding(lines) == "bytes"
  if (any(unresolved)) {
    # DOS code pages preserve one source byte as one display character. Start
    # with CP850 only after conservative auto-decoding declines a row, then
    # promote strong CP860 evidence so corrupt labels cannot shift CNV columns.
    fallback <- suppressWarnings(iconv(
      lines[unresolved], from = "CP850", to = "UTF-8", sub = NA
    ))
    cp860 <- .dbc_recover_mixed_cp860(lines[unresolved], fallback)
    fallback[cp860$recover] <- cp860$value[cp860$recover]
    recovered <- !is.na(fallback)
    indices <- which(unresolved)
    lines[indices[recovered]] <- fallback[recovered]
    used <- strsplit(
      sub("^mixed:", "", encoding), "+", fixed = TRUE
    )[[1L]]
    fallback_used <- c("CP850", if (any(cp860$recover)) "CP860")
    used <- unique(c(setdiff(used, "bytes"), fallback_used))
    encoding <- if (length(used) == 1L) {
      used
    } else {
      paste0("mixed:", paste(used, collapse = "+"))
    }
  }
  if (anyNA(lines) || any(Encoding(lines) == "bytes")) {
    .tabwin_abort(
      "Could not convert TabWin file {.file {basename(path)}} to UTF-8.",
      "microdatasus_dictionary_invalid_error"
    )
  }
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
    syntax_recovered <- FALSE
    # A few official DEFs contain one of three unambiguous punctuation errors
    # in an otherwise complete CNV relation: the comma before the source field
    # or its position is missing. Three exact SINAN records instead omit
    # position 1 completely. Limit recovery to three-field rows ending in an
    # explicit CNV; incomplete DBF relations do not contain enough metadata to
    # infer their label field.
    if (length(fields) == 3L &&
        identical(toupper(tools::file_ext(trimws(fields[[3L]]))), "CNV") &&
        !grepl(
          "^[SLCQDTX][[:space:]]*[*+]", trimws(fields[[1L]]),
          ignore.case = TRUE
        )) {
      first <- trimws(fields[[1L]])
      second <- trimws(fields[[2L]])
      identifier <- "[A-Za-z_][A-Za-z0-9_]*"
      omitted_position <- switch(
        toupper(basename(path)),
        "AIDSNET.DEF" = identical(
          c(toupper(second), toupper(basename(trimws(fields[[3L]])))),
          c("ID_UNIDADE", "PENITENCIARIO.CNV")
        ),
        "HEPAVIRNET.DEF" =
          toupper(second) %in% c("RE_ANTIHBC", "RE_ANTIHCV") &&
          identical(
            toupper(basename(trimws(fields[[3L]]))), "HEPREAG.CNV"
          ),
        FALSE
      )
      missing_source_comma <- regexec(
        paste0("^(.*[^[:space:]])[[:space:]]+(", identifier, ")$"),
        first,
        perl = TRUE
      )
      missing_source_parts <- regmatches(first, missing_source_comma)[[1L]]
      missing_position_comma <- regexec(
        paste0("^(", identifier, ")[[:space:]]+([0-9]+)$"),
        second,
        perl = TRUE
      )
      missing_position_parts <- regmatches(
        second, missing_position_comma
      )[[1L]]

      if (grepl("^[0-9]+$", second) &&
          length(missing_source_parts) == 3L) {
        fields <- c(
          missing_source_parts[[2L]], missing_source_parts[[3L]],
          second, fields[[3L]]
        )
        syntax_recovered <- TRUE
      } else if (length(missing_position_parts) == 3L) {
        fields <- c(
          fields[[1L]], missing_position_parts[[2L]],
          missing_position_parts[[3L]], fields[[3L]]
        )
        syntax_recovered <- TRUE
      } else if (omitted_position) {
        fields <- c(fields[[1L]], second, "1", fields[[3L]])
        syntax_recovered <- TRUE
      }
    }
    if (length(fields) < 4L) {
      return(NULL)
    }
    # The fourth field is the relation by specification. One current CNES DEF
    # accidentally retains a stale value there and appends the real DBF as a
    # fifth field. Recover only a single, explicit CNV/DBF candidate; multiple
    # candidates retain the standard first relation instead of guessing.
    relation_fields <- seq.int(4L, length(fields))
    relation_extensions <- toupper(tools::file_ext(
      trimws(fields[relation_fields])
    ))
    relation_candidates <- relation_fields[
      relation_extensions %in% c("CNV", "DBF")
    ]
    relation_index <- if (toupper(tools::file_ext(
      trimws(fields[[4L]]))
    ) %in% c("CNV", "DBF")) {
      4L
    } else if (length(relation_candidates) == 1L) {
      relation_candidates[[1L]]
    } else {
      return(NULL)
    }
    file_name <- trimws(fields[[relation_index]])
    extension <- toupper(tools::file_ext(file_name))
    # For CNV, field three is the starting position in the source variable.
    # For DBF, the same field names the column that contains the description.
    argument <- trimws(fields[[3L]])
    position <- if (extension == "CNV") {
      suppressWarnings(as.integer(argument))
    } else {
      NA_integer_
    }
    position_recovered <- extension == "CNV" &&
      is.na(position) &&
      grepl("^[A-Za-z_][A-Za-z0-9_-]*$", argument)
    if (position_recovered) {
      # RD2008/RJ2008 contain twelve CNV relations whose third field was copied
      # from DBF syntax (DS_TPFIN or IP_DSCR). Both relations address the whole
      # source value, so TabWin's normal starting position of one is unambiguous.
      position <- 1L
    }
    data.frame(
      order = i,
      command = command,
      description = trimws(substring(fields[[1L]], 2L)),
      field = toupper(trimws(fields[[2L]])),
      argument = argument,
      position = position,
      position_recovered = position_recovered,
      syntax_recovered = syntax_recovered,
      file = file_name,
      file_recovered = relation_index != 4L,
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
    # One official tuberculosis DEF appends an explanatory parenthesis to the
    # physical DBF field. Recover only a complete identifier followed by that
    # comment shape; headings and arbitrary prose remain invalid.
    parenthetical <- regexec(
      "^([A-Z][A-Z0-9_]*)[[:space:]]+\\(.+\\)$", field, perl = TRUE
    )
    parenthetical <- regmatches(field, parenthetical)[[1L]]
    if (length(parenthetical) == 2L) {
      field <- parenthetical[[2L]]
    }
    if (!grepl("^[A-Z][A-Z0-9_]*$", field)) {
      return(NA_character_)
    }
    field
  }, character(1))
  fields <- unique(fields[!is.na(fields)])

  # Exact upstream typo: DifteriNET.def prefixes MED_QUAN_P with an extra L,
  # while every published DBC layout uses the unprefixed physical field.
  aliases <- switch(
    toupper(basename(path)),
    "DIFTERINET.DEF" = c("LMED_QUAN_P" = "MED_QUAN_P"),
    stats::setNames(character(), character())
  )
  replace <- match(fields, names(aliases), nomatch = 0L)
  fields[replace > 0L] <- unname(aliases[replace])
  unique(fields)
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
    # Some official ZIPs flatten relation directories without updating their
    # DEF, duplicate a directory component, or leave whitespace beside a path
    # separator. An exact, unique basename still identifies the relation.
    requested_basename <- trimws(basename(suffix_lower))
    entry_basenames <- trimws(basename(entries_lower))
    matches <- which(entry_basenames == requested_basename)
  }
  if (!length(matches)) {
    # Punctuation drift is accepted only for spaces and underscores. This
    # recovers TPAPAC.CNV -> TP_APAC.CNV without fuzzy-matching versioned names.
    filename_key <- function(value) {
      gsub("[[:space:]_]", "", trimws(basename(value)))
    }
    requested_key <- filename_key(suffix_lower)
    matches <- which(vapply(
      entries_lower,
      function(entry) identical(filename_key(entry), requested_key),
      logical(1)
    ))
  }
  if (!length(matches)) {
    # Evidence-backed typographical errors in official DEFs. Keep these exact
    # aliases explicit so unrelated one-edit filenames are never guessed.
    aliases <- c(
      "atjurc.cnv" = "natjurc.cnv",
      "cobrdets.cnv" = "cobrdet.cnv",
      "idx20b.cnv" = "cidx20b.cnv"
    )
    requested_basename <- trimws(basename(suffix_lower))
    alias <- unname(aliases[requested_basename])
    if (length(alias) == 1L && !is.na(alias)) {
      matches <- which(trimws(basename(entries_lower)) == alias)
    }
  }
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

.tabwin_expand_range <- function(
  token, width, mode = "", max_codes = 100000L
) {
  # Short CNV codes may use compact intervals such as 01-09, A-Z, or A01-A05.
  token <- trimws(token)
  bounds <- strsplit(token, "-", fixed = TRUE)[[1L]]
  if (length(bounds) != 2L || any(!nzchar(bounds))) {
    return(token)
  }
  bounds <- .tabwin_normalize_code(bounds, width, mode)
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
  open_range <- startsWith(token, "-") || endsWith(token, "-")
  alpha_bound <- grepl("[[:alpha:]]", token)
  if ((.tabwin_codes_are_literal(width, mode) || alpha_bound) &&
      open_range) {
    separators <- gregexpr("-", token, fixed = TRUE)[[1L]]
    if (length(separators) != 1L || separators[[1L]] < 1L) return(NULL)
    return(data.frame(
      token = token, kind = "literal", prefix = "",
      lower = NA_real_, upper = NA_real_, size = Inf, width = width,
      stringsAsFactors = FALSE
    ))
  }
  if (length(bounds) != 2L || any(!nzchar(bounds))) return(NULL)
  raw_bounds <- bounds
  control_bound <- any(grepl(intToUtf8(31L), raw_bounds, fixed = TRUE))
  bounds <- .tabwin_normalize_code(bounds, width, mode)
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
  same_prefix <- length(pieces[[1L]]) == 3L &&
    length(pieces[[2L]]) == 3L &&
    identical(
      toupper(pieces[[1L]][[2L]]), toupper(pieces[[2L]][[2L]])
    )
  if (same_prefix) {
    limits <- as.numeric(c(pieces[[1L]][[3L]], pieces[[2L]][[3L]]))
    if (!anyNA(limits) && limits[[1L]] <= limits[[2L]]) {
      return(data.frame(
        token = token, kind = "alphanumeric",
        prefix = toupper(pieces[[1L]][[2L]]),
        lower = limits[[1L]], upper = limits[[2L]],
        size = limits[[2L]] - limits[[1L]] + 1, width = width,
        stringsAsFactors = FALSE
      ))
    }
  }
  inferred_literal <- all(grepl("^[[:alnum:]]+$", bounds)) &&
    any(grepl("[[:alpha:]]", bounds))
  if (.tabwin_codes_are_literal(width, mode) || control_bound ||
      inferred_literal) {
    literal_bounds <- if (control_bound) {
      sentinel <- intToUtf8(31L)
      repaired <- gsub(sentinel, "0", raw_bounds, fixed = TRUE)
      .tabwin_normalize_code(repaired, width, "L")
    } else {
      bounds
    }
    literal_token <- if (control_bound) {
      paste(literal_bounds, collapse = "-")
    } else {
      token
    }
    fixed_width <- nchar(literal_bounds, type = "chars") == width
    if (all(fixed_width) && literal_bounds[[1L]] <= literal_bounds[[2L]]) {
      return(data.frame(
        token = literal_token, kind = "literal", prefix = "",
        lower = NA_real_, upper = NA_real_, size = Inf, width = width,
        stringsAsFactors = FALSE
      ))
    }
  }
  NULL
}

.tabwin_split_concatenated_ranges <- function(token, width, mode = "") {
  candidate <- trimws(token)
  parts <- strsplit(candidate, "-", fixed = TRUE)[[1L]]
  if (length(parts) != 4L ||
      any(!grepl("^[[:alnum:]]+$", parts)) ||
      any(nchar(parts, type = "chars") != width)) {
    return(token)
  }
  ranges <- c(
    paste(parts[1:2], collapse = "-"),
    paste(parts[3:4], collapse = "-")
  )
  valid <- vapply(
    ranges,
    function(value) !is.null(.tabwin_range_rule(value, width, mode)),
    logical(1)
  )
  if (!all(valid)) token else ranges
}

.tabwin_repair_range_token <- function(token, width, mode = "", path = NULL) {
  candidate <- trimws(token)

  # Three historical SIH archives use `=` once where every neighbouring
  # token uses `-`. Accept it only between two complete fixed-width bounds.
  equals <- strsplit(candidate, "=", fixed = TRUE)[[1L]]
  if (length(equals) == 2L &&
      all(grepl("^[[:alnum:]]+$", equals)) &&
      all(nchar(equals, type = "chars") == width)) {
    repaired <- paste(equals, collapse = "-")
    if (!is.null(.tabwin_range_rule(repaired, width, mode))) {
      return(repaired)
    }
  }

  if (!is.null(.tabwin_range_rule(candidate, width, mode))) {
    return(token)
  }

  # A few SINAN lists repeat a padded interval alongside its canonical form,
  # e.g. `2 -5,02-05`. If padding makes the bounds descend but removing only
  # whitespace adjacent to the separator yields a valid range, use that range.
  compact <- gsub(
    "[[:space:]]*-[[:space:]]*", "-", candidate, perl = TRUE
  )
  if (!identical(compact, candidate) &&
      !is.null(.tabwin_range_rule(compact, width, mode))) {
    return(compact)
  }

  # The same SIM archive contains the corrected L985-L989 interval in
  # MalDefCBPU.CNV, while improv.CNV and improv1.CNV contain this transposition.
  if (width == 4L && identical(toupper(candidate), "L985-L959")) {
    return("L985-L989")
  }

  # CID9BR.CNV in the same official mortality archive confirms that the
  # descending epilepsy interval in CID9BR2.CNV is a transposed upper bound.
  cid9br2 <- length(path) == 1L && !is.na(path) &&
    identical(toupper(basename(path)), "CID9BR2.CNV")
  if (cid9br2 && width == 4L && identical(candidate, "3450-2459")) {
    return("3450-3459")
  }

  token
}

.tabwin_infer_literal_width <- function(codes, labels, width, mode = "") {
  if (!identical(toupper(mode), "L") || !length(codes)) return(width)
  scalar <- trimws(codes)
  scalar_lengths <- nchar(scalar, type = "chars")
  eligible <- grepl("^[[:alnum:]]+$", scalar) &
    scalar_lengths <= width + 1L
  if (!any(eligible & scalar_lengths == width + 1L)) return(width)
  scalar <- scalar[eligible]
  scalar_labels <- labels[eligible]
  scalar_lengths <- scalar_lengths[eligible]

  conflicts <- function(candidate_width) {
    keys <- .tabwin_normalize_code(scalar, candidate_width, "L")
    groups <- split(seq_along(keys), keys)
    vapply(groups, function(index) {
      length(unique(scalar_labels[index])) > 1L &&
        any(scalar_lengths[index] > candidate_width)
    }, logical(1))
  }
  if (!any(conflicts(width))) return(width)

  maximum <- max(scalar_lengths)
  for (candidate_width in seq.int(width + 1L, maximum)) {
    if (!any(conflicts(candidate_width))) return(candidate_width)
  }
  width
}

.tabwin_sanitize_code_token <- function(
  token, width, mode = "", path = NULL
) {
  candidate <- trimws(token)
  if (!nzchar(candidate)) return(token)

  sentinel <- intToUtf8(31L)
  if (grepl(sentinel, candidate, fixed = TRUE) &&
      !grepl("-", candidate, fixed = TRUE)) {
    return(gsub(sentinel, "0", candidate, fixed = TRUE))
  }
  placeholder <- paste0("^-{", width, "}$")
  if (grepl(placeholder, candidate)) return(token)

  range_like <- grepl("-", candidate, fixed = TRUE) ||
    grepl("=", candidate, fixed = TRUE)
  if (range_like) {
    repaired <- .tabwin_repair_range_token(token, width, mode, path)
    if (!identical(repaired, token)) return(repaired)
    concatenated <- .tabwin_split_concatenated_ranges(token, width, mode)
    if (length(concatenated) > 1L) return(token)
    if (!is.null(.tabwin_range_rule(token, width, mode))) return(token)

    # Some literal SINAN tables repeat a numeric interval with unpadded bounds,
    # such as 7 -48 beside 07-48. Preserve the unpadded aliases explicitly;
    # lexical literal comparison cannot represent that mixed-width interval.
    compact_range <- gsub(
      "[[:space:]]*-[[:space:]]*", "-", candidate, perl = TRUE
    )
    numeric_bounds <- strsplit(compact_range, "-", fixed = TRUE)[[1L]]
    if (.tabwin_codes_are_literal(width, mode) &&
        length(numeric_bounds) == 2L &&
        all(grepl("^[0-9]+$", numeric_bounds))) {
      limits <- suppressWarnings(as.integer(numeric_bounds))
      size <- limits[[2L]] - limits[[1L]] + 1L
      if (!anyNA(limits) && size > 0L && size <= 100000L) {
        return(as.character(seq.int(limits[[1L]], limits[[2L]])))
      }
    }
  }

  # One official SINAN table spells numeric zero as `.0`. No other numeric
  # token in the corpus uses a leading dot; retain dotted literal versions.
  if (!.tabwin_codes_are_literal(width, mode) &&
      grepl("^\\.[0-9]+$", candidate) &&
      nchar(candidate, type = "chars") == width + 1L) {
    return(sub("^\\.", "", candidate))
  }

  # Preserve a complete code before prose that overflowed the description
  # field. This occurs in medico02.CNV (`XXXXXX ... vascular`).
  prefix_pattern <- paste0(
    "^([[:alnum:]]{", width, "})[[:space:]]{2,}.+$"
  )
  prefix_match <- regexec(prefix_pattern, candidate, perl = TRUE)
  prefix <- regmatches(candidate, prefix_match)[[1L]]
  if (length(prefix) == 2L) return(prefix[[2L]])

  # Literal tables occasionally use a meaningful internal blank (for example
  # `A O` in TP_DROGA.cnv). It is safe only inside the declared field width.
  if (.tabwin_codes_are_literal(width, mode) &&
      nchar(candidate, type = "chars") <= width &&
      grepl("^[[:alnum:].[:space:]]+$", candidate)) {
    return(token)
  }

  # Codes are fixed-width scalars or intervals. Text fragments overflowing
  # after a comma must not silently become synthetic keys such as `linf`.
  if (grepl("[[:space:]]", candidate) ||
      grepl("[^[:alnum:].]", candidate)) {
    return(character())
  }
  token
}

.tabwin_empty_ranges <- function() {
  data.frame(
    token = character(), kind = character(), prefix = character(),
    lower = numeric(), upper = numeric(), size = numeric(), width = integer(),
    label = character(), priority = integer(), stringsAsFactors = FALSE
  )
}

.tabwin_codes_are_literal <- function(width, mode = "") {
  mode <- if (length(mode) && !is.na(mode[[1L]])) {
    toupper(mode[[1L]])
  } else {
    ""
  }
  identical(mode, "L") || (identical(mode, "") && width >= 5L)
}

.tabwin_normalize_code <- function(code, width, mode = "") {
  code <- as.character(code)
  literal <- .tabwin_codes_are_literal(width, mode)
  if (literal) {
    # TabWin treats widths of five or more as alphanumeric automatically; L
    # forces the same treatment for shorter fields. Literal fields are
    # right-padded, keeping "1  " distinct from "001".
    code <- sub("^[[:space:]]+", "", code)
    # TabWin code fields have fixed width. Published tables sometimes append
    # an IBGE check digit, prose, or other bytes beyond that field; TabWin
    # compares only the declared prefix.
    code <- substr(code, 1L, width)
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
    return(code)
  }

  # Numeric fields are fixed-width too. Spaces inside the retained prefix
  # remain significant: "1  " still denotes 100 at width three.
  code <- substr(code, 1L, width)

  # In numeric mode, right-padding represents zeroes: both "1  " and "10 "
  # denote 100 at width three. Unpadded official tokens remain tolerated by
  # left-padding numeric codes, which also repairs DBF readers that drop zeroes.
  right_padded <- !is.na(code) &
    grepl("^[[:alnum:]]+[[:space:]]+$", code)
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

.tabwin_recover_numeric_collision_codes <- function(
  codes,
  labels,
  width,
  mode = ""
) {
  attr(codes, "recovered_numeric_collision_codes") <- 0L
  if (.tabwin_codes_are_literal(width, mode) || length(codes) < 2L) {
    return(codes)
  }

  padded <- grepl("^[0-9]+[[:space:]]+$", codes)
  candidates <- which(padded)
  if (!length(candidates)) return(codes)

  normalized <- .tabwin_normalize_code(codes, width, mode)
  trimmed <- trimws(codes)
  complete <- grepl(
    paste0("^[0-9]{", width, "}$"),
    trimmed,
    perl = TRUE
  )
  alternatives <- vapply(
    trimmed[candidates],
    function(value) paste0(strrep("0", width - nchar(value)), value),
    character(1)
  )

  recover <- vapply(seq_along(candidates), function(offset) {
    index <- candidates[[offset]]
    conflicts <- which(
      seq_along(codes) != index &
        complete &
        normalized == normalized[[index]] &
        labels != labels[[index]]
    )
    length(conflicts) > 0L &&
      identical(alternatives[[offset]], trimws(labels[[index]])) &&
      !alternatives[[offset]] %in% normalized[-index]
  }, logical(1))

  recovered <- candidates[recover]
  if (length(recovered)) {
    codes[recovered] <- alternatives[recover]
  }
  attr(codes, "recovered_numeric_collision_codes") <- as.integer(
    length(recovered)
  )
  codes
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
  if (identical(conversion$type, "dbf") && widths[[start]] >= required) {
    return(values)
  }
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
      implicit_literal <- .tabwin_codes_are_literal(
        conversion$code_width, mode
      ) && !identical(mode, "L")
      if (!implicit_literal) {
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
      }
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
  header_line <- sub(";.*$", "", lines[[header_index]])
  header <- toupper(trimws(header_line))
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
  # Repeated category rows update the category description in TabWin. Blank
  # continuations retain it, while the last non-blank description wins. This
  # is used by official tables that list members first and finish with the
  # aggregate label (for example, individual procedures then "Partos normais").
  reversed_index <- match(unique_key, rev(label_key))
  label_index <- length(label_key) - reversed_index + 1L
  last_label <- label_value[label_index]

  distinct_pair <- !duplicated(paste(label_key, label_value, sep = "\035"))
  distinct_count <- tabulate(
    match(label_key[distinct_pair], unique_key),
    nbins = length(unique_key)
  )
  categories <- data.frame(
    sequence = sequence[source_order],
    subtotal = subtotal[source_order],
    label = last_label,
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
  token_pattern <- paste0(
    "^[[:alnum:]][[:alnum:]-]*",
    "(?:[[:space:]]*,[[:space:]]*[[:alnum:]][[:alnum:]-]*)*$"
  )
  candidate_valid <- grepl(
    token_pattern, candidate_trimmed,
    perl = TRUE
  )
  compact_separated <- grepl("[[:space:]]{2,}$", candidate_prefix)
  compact <- !identical(mode, "L") &
    line_width < code_start &
    !nzchar(trimws(code_text)) &
    candidate_start > 9L &
    candidate_valid &
    compact_separated
  declared_trimmed <- trimws(code_text)
  declared_valid <- grepl(token_pattern, declared_trimmed, perl = TRUE)
  full_width <- nchar(candidate_trimmed, type = "chars") == code_width
  overflow_separated <- grepl("[[:space:]]$", candidate_prefix)
  overflow_unambiguous <- !grepl(",", declared_trimmed, fixed = TRUE)
  overflow_delimited <- candidate_start > code_start + 1L &
    endsWith(candidate_prefix, ")")
  overflow <- !identical(mode, "L") &
    line_width >= code_start &
    candidate_start > code_start &
    nzchar(declared_trimmed) &
    !declared_valid &
    candidate_valid &
    full_width &
    (overflow_separated | overflow_delimited) &
    overflow_unambiguous
  recover <- compact | overflow
  code_text[recover] <- candidate[recover]
  attr(code_text, "compact") <- compact
  attr(code_text, "overflow") <- overflow
  attr(code_text, "candidate_start") <- candidate_start
  attr(code_text, "compact_rows") <- sum(compact)
  attr(code_text, "overflow_rows") <- sum(overflow)
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
  # The CNV specification treats every semicolon and the remainder of its
  # physical line as documentation, independently of the semicolon's column.
  rows <- sub(";.*$", "", rows)
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
  overflow <- attr(code_text, "overflow")
  realigned <- compact | overflow
  realigned_start <- attr(code_text, "candidate_start")
  compact_code_rows <- attr(code_text, "compact_rows")
  overflow_code_rows <- attr(code_text, "overflow_rows")
  row_labels <- trimws(substr(rows, label_start, label_end))
  if (any(realigned)) {
    row_labels[realigned] <- trimws(substr(
      rows[realigned], label_start, realigned_start[realigned] - 1L
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
  # A comma followed only by physical line padding does not declare another
  # code. Literal CNVs also use genuinely blank fixed-width codes, but publish
  # those before a delimiter (`   ,`) or between delimiters (`,   ,`). Removing
  # only padding after the final comma preserves that distinction.
  trailing_delimiter_padding <- grepl(
    ",[[:space:]]+$", code_text, perl = TRUE
  )
  code_text[trailing_delimiter_padding] <- sub(
    ",[[:space:]]+$", ",", code_text[trailing_delimiter_padding],
    perl = TRUE
  )
  tokens <- strsplit(code_text, ",", fixed = TRUE)
  tokens <- lapply(tokens, function(values) {
    if (.tabwin_codes_are_literal(code_width, mode)) {
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

  declared_code_width <- code_width
  code_width <- .tabwin_infer_literal_width(
    raw_codes, raw_labels, code_width, mode
  )

  levels <- unique(categories$label[!is.na(categories$label)])
  common <- list(
    type = "cnv",
    dialect = dialect,
    mode = mode,
    code_width = code_width,
    declared_code_width = declared_code_width,
    recovered_code_width = code_width != declared_code_width,
    category_count = category_count,
    observed_category_count = nrow(categories),
    category_count_mismatch = category_count != nrow(categories),
    categories = categories,
    levels = levels,
    source_encoding = source_encoding,
    tabs_recovered = tabs_recovered,
    embedded_header = !is.null(header$embedded_row),
    compact_code_rows = compact_code_rows,
    overflow_code_rows = overflow_code_rows,
    recovered_concatenated_ranges = 0L,
    repaired_code_tokens = 0L,
    placeholder_code_tokens = 0L,
    discarded_code_tokens = 0L,
    truncated_code_tokens = 0L,
    recovered_numeric_collision_codes = 0L,
    recovered_sequence = recovered_sequence,
    recovered_leading_sequence = recovered_leading_sequence,
    trailing_delimiter_padding_rows = sum(trailing_delimiter_padding)
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

  trimmed_codes <- trimws(raw_codes)
  placeholder_pattern <- paste0("^-{", code_width, "}$")
  common$placeholder_code_tokens <- sum(grepl(
    placeholder_pattern, trimmed_codes
  ))
  suspicious_codes <- grepl("-", trimmed_codes, fixed = TRUE) |
    grepl("=", trimmed_codes, fixed = TRUE) |
    grepl("^\\.[0-9]+$", trimmed_codes) |
    grepl("[[:space:]]", trimmed_codes) |
    grepl("[^[:alnum:].]", trimmed_codes)
  suspicious_indexes <- which(suspicious_codes)
  sanitized_codes <- as.list(raw_codes)
  sanitized_codes[suspicious_indexes] <- lapply(
    raw_codes[suspicious_indexes],
    .tabwin_sanitize_code_token, width = code_width, mode = mode,
    path = path
  )
  sanitized_lengths <- lengths(sanitized_codes)
  common$discarded_code_tokens <- sum(sanitized_lengths == 0L)
  common$repaired_code_tokens <- sum(vapply(
    suspicious_indexes,
    function(index) {
      sanitized_lengths[[index]] > 0L && (
        sanitized_lengths[[index]] != 1L ||
          !identical(sanitized_codes[[index]], raw_codes[[index]])
      )
    },
    logical(1)
  ))
  kept_codes <- sanitized_lengths > 0L
  raw_labels <- rep(raw_labels[kept_codes], sanitized_lengths[kept_codes])
  raw_codes <- unlist(sanitized_codes[kept_codes], use.names = FALSE)

  split_candidates <- which(grepl("-", raw_codes, fixed = TRUE))
  split_codes <- as.list(raw_codes)
  split_codes[split_candidates] <- lapply(
    raw_codes[split_candidates],
    .tabwin_split_concatenated_ranges,
    width = code_width,
    mode = mode
  )
  split_lengths <- lengths(split_codes)
  common$recovered_concatenated_ranges <-
    sum(split_lengths) - length(raw_codes)
  raw_labels <- rep(raw_labels, split_lengths)
  raw_codes <- unlist(split_codes, use.names = FALSE)
  if (is.null(raw_codes)) raw_codes <- character()

  raw_codes <- .tabwin_recover_numeric_collision_codes(
    raw_codes, raw_labels, code_width, mode
  )
  common$recovered_numeric_collision_codes <- attr(
    raw_codes, "recovered_numeric_collision_codes"
  )

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
    width = code_width,
    mode = mode
  )
  map_input <- unlist(expanded, use.names = FALSE)
  measured_input <- map_input
  if (.tabwin_codes_are_literal(code_width, mode)) {
    measured_input <- sub("^[[:space:]]+", "", measured_input)
  }
  overlong_input <- !is.na(measured_input) &
    nchar(measured_input, type = "chars") > code_width
  suffix <- substring(measured_input, code_width + 1L)
  nonpadding_suffix <- overlong_input &
    !grepl("^[[:space:]]+$", suffix)
  common$truncated_code_tokens <- sum(nonpadding_suffix)
  map_codes <- .tabwin_normalize_code(map_input, code_width, mode)
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

.tabwin_recover_official_source_aliases <- function(conversion, path) {
  # Exact, evidence-backed aliases for published DBC values that drift from
  # their accompanying CNV. Keep this filename-scoped so unrelated domains
  # can never acquire a guessed equivalence.
  aliases <- switch(
    toupper(basename(path)),
    "GRAU_HIS.CNV" = c(
      "1 " = "G1", "01" = "G1",
      "2 " = "G2", "02" = "G2",
      "3 " = "G3", "03" = "G3",
      "4 " = "G4", "04" = "G4"
    ),
    "SIMNAO2.CNV" = c("S" = "1", "N" = "0"),
    stats::setNames(character(), character())
  )
  conversion$source_aliases <- stats::setNames(character(), character())
  conversion$recovered_source_aliases <- 0L
  if (!identical(conversion$type, "cnv") || !length(aliases)) {
    return(conversion)
  }

  recover <- !names(aliases) %in% names(conversion$map) &
    unname(aliases) %in% names(conversion$map)
  if (!any(recover)) return(conversion)
  recovered <- aliases[recover]
  alias_names <- names(recovered)
  canonical_names <- unname(recovered)
  conversion$map[alias_names] <- unname(conversion$map[canonical_names])
  conversion$map_priority[alias_names] <- unname(
    conversion$map_priority[canonical_names]
  )
  conversion$source_aliases <- recovered
  conversion$recovered_source_aliases <- as.integer(length(recovered))
  conversion
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

.tabwin_parser_version <- 24L

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

# The official EpizotNet DEF requests ID_RG_OCOR, while REGIONET omits it.
# Recover ID_REGIONA only when its notification and residence key copies agree.
.tabwin_recover_official_dbf_key <- function(field, table, path) {
  if (
    length(field) != 1L ||
      is.na(field) ||
      !nzchar(trimws(field)) ||
      !identical(toupper(basename(path)), "REGIONET.DBF") ||
      !identical(toupper(trimws(field)), "ID_RG_OCOR")
  ) {
    return(NA_integer_)
  }
  notification <- .tabwin_match_dbf_field("ID_REGIONA", names(table))
  residence <- .tabwin_match_dbf_field("ID_RG_RESI", names(table))
  if (
    is.na(notification) ||
      is.na(residence) ||
      !identical(
        as.character(table[[notification]]),
        as.character(table[[residence]])
      )
  ) {
    return(NA_integer_)
  }
  notification
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

.tabwin_repair_official_dbf_values <- function(value, path) {
  if (!identical(toupper(basename(path)), "INCENTIVOS.DBF")) {
    return(value)
  }

  sources <- list(
    c(
      charToRaw("8231-CEO-I-REDE DE CUIDADOS "),
      as.raw(145L),
      charToRaw(" PESSOA COM DEFICIENCIA")
    ),
    c(
      charToRaw("8232-CEO-II-REDE DE CUIDADOS "),
      as.raw(145L),
      charToRaw(" PESSOA COM DEFICIENCIA")
    ),
    c(
      charToRaw("8233-CEO-III-REDE DE CUIDADOS "),
      as.raw(145L),
      charToRaw(" PESSOA COM DEFICIENCIA")
    ),
    c(
      charToRaw(
        "8248-UNIDADE MOVEL DE ATENDIMENTO PRE-HOSPITALAR MOTOL"
      ),
      as.raw(143L),
      charToRaw("NCIA SAMU")
    )
  )
  targets <- c(
    "8231-CEO-I-REDE DE CUIDADOS \u00c0 PESSOA COM DEFICIENCIA",
    "8232-CEO-II-REDE DE CUIDADOS \u00c0 PESSOA COM DEFICIENCIA",
    "8233-CEO-III-REDE DE CUIDADOS \u00c0 PESSOA COM DEFICIENCIA",
    paste0(
      "8248-UNIDADE MOVEL DE ATENDIMENTO PRE-HOSPITALAR ",
      "MOTOL\u00c2NCIA SAMU"
    )
  )
  for (index in seq_along(sources)) {
    matched <- vapply(value, function(item) {
      !is.na(item) & identical(charToRaw(item), sources[[index]])
    }, logical(1))
    value[matched] <- targets[[index]]
  }
  value
}

.tabwin_dbf_cp850_rows <- function(table, metadata, path) {
  rows <- rep(FALSE, nrow(table))
  if (
    metadata$language_driver == 0L ||
      !identical(toupper(metadata$encoding), "CP1252")
  ) {
    return(rows)
  }

  for (index in which(vapply(table, is.character, logical(1)))) {
    value <- as.character(table[[index]])
    primary <- suppressWarnings(iconv(
      value,
      from = metadata$encoding,
      to = "UTF-8",
      sub = NA
    ))
    rows <- rows | .dbc_recover_mixed_cp850(value, primary)$recover
  }

  # CADGERBA contains two otherwise ambiguous CP850 rows. Restrict recovery to
  # the exact official filename and byte sequences evidenced in both copies.
  if (identical(toupper(basename(path)), "CADGERBA.DBF")) {
    fantasia <- .tabwin_match_dbf_field("FANTASIA", names(table))
    if (!is.na(fantasia)) {
      targets <- vapply(
        c(
          "CL\u00cdNICA DE OLHOS",
          "CL\u00cdNICA SANTO ANT\u00d4NIO"
        ),
        function(item) {
          rawToChar(charToRaw(iconv(
            item, from = "UTF-8", to = "CP850"
          )))
        },
        character(1)
      )
      value <- as.character(table[[fantasia]])
      rows <- rows | (!is.na(value) & value %in% targets)
    }
  }
  rows
}

.tabwin_decode_dbf_values <- function(
  value,
  metadata,
  context,
  path,
  cp850_rows = NULL
) {
  value <- .tabwin_repair_official_dbf_values(as.character(value), path)
  decoded <- .dbc_decode_text_auto(
    value,
    metadata$encoding,
    metadata$language_driver,
    context,
    path,
    cp850_rows
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
    conversion <- .tabwin_recover_official_source_aliases(
      .tabwin_parse_cnv(path), path
    )
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
    cp850_rows <- .tabwin_dbf_cp850_rows(table, dbf_encoding, path)
    code_index <- .tabwin_match_dbf_field(definition$field, names(table))
    recovered_key <- FALSE
    if (is.na(code_index)) {
      code_index <- .tabwin_recover_official_dbf_key(
        definition$field, table, path
      )
      recovered_key <- !is.na(code_index)
    }
    # The TabWin specification uses the first DBF field when the related table
    # does not repeat the source field name.
    fallback_key <- is.na(code_index)
    if (fallback_key) {
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
      path, cp850_rows
    ))
    labels <- .tabwin_decode_dbf_values(
      table[[label_index]], dbf_encoding,
      sprintf("TabWin DBF label field %s", sQuote(names(table)[[label_index]])),
      path, cp850_rows
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
        fallback_key = fallback_key,
        recovered_key = recovered_key,
        requested_key_field = definition$field,
        key_field = names(table)[[code_index]],
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
.tabwin_apply_conversion_values <- function(
  values,
  selected,
  fallback = values
) {
  definition <- selected$definition
  conversion <- selected$conversion
  raw_source <- as.character(values)
  source <- trimws(as.character(fallback))
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
  result[is.na(fallback)] <- NA_character_
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
