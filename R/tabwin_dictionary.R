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

.tabwin_read_text <- function(path) {
  # TabWin text files are published with a legacy Windows encoding. Read the
  # bytes explicitly so the result does not depend on the user's locale.
  size <- file.info(path)$size
  if (is.na(size) || size == 0) {
    cli::cli_abort("TabWin file {.file {basename(path)}} is empty.")
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  text <- rawToChar(readBin(con, what = "raw", n = size))
  text <- iconv(text, from = "windows-1252", to = "UTF-8", sub = "byte")
  if (is.na(text)) {
    cli::cli_abort(
      "Could not convert TabWin file {.file {basename(path)}} to UTF-8."
    )
  }
  strsplit(text, "\r\n|\n|\r", perl = TRUE)[[1L]]
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
    cli::cli_abort(
      "TabWin definition {.file {basename(path)}} contains no usable conversions."
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
  matches <- which(endsWith(tolower(entries_normalized), tolower(suffix)))
  if (length(matches) != 1L) {
    cli::cli_abort(
      "The TabWin archive must contain exactly one file matching {.file {suffix}}."
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
    if (length(matches) != 1L || file.size(candidates[[matches]]) == 0) {
      cli::cli_abort(
        "The extracted TabWin file {.file {file_name}} is missing or ambiguous."
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
    cli::cli_abort(c(
      "Failed to extract TabWin file {.file {file_name}}.",
      "i" = conditionMessage(extracted)
    ))
  }
  if (!file.exists(destination) || file.size(destination) == 0) {
    cli::cli_abort(
      "The extracted TabWin file {.file {file_name}} is missing or empty."
    )
  }
  destination
}

.tabwin_expand_range <- function(token, width) {
  # Short CNV codes may use compact intervals such as 01-09 or A01-A05.
  bounds <- strsplit(token, "-", fixed = TRUE)[[1L]]
  if (length(bounds) != 2L || any(!nzchar(bounds))) {
    return(token)
  }
  if (all(grepl("^[0-9]+$", bounds))) {
    limits <- suppressWarnings(as.integer(bounds))
    if (anyNA(limits) || limits[[1L]] > limits[[2L]]) {
      return(token)
    }
    return(sprintf(
      paste0("%0", width, "d"),
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
    limits <- as.integer(c(pieces[[1L]][[3L]], pieces[[2L]][[3L]]))
    if (!anyNA(limits) && limits[[1L]] <= limits[[2L]]) {
      prefix <- toupper(pieces[[1L]][[2L]])
      digits <- width - nchar(prefix)
      return(paste0(
        prefix,
        sprintf(
          paste0("%0", digits, "d"),
          seq.int(limits[[1L]], limits[[2L]])
        )
      ))
    }
  }
  token
}

.tabwin_normalize_code <- function(code, width) {
  # DBF readers can drop leading zeroes from numeric-looking codes.
  code <- trimws(as.character(code))
  numeric <- !is.na(code) & grepl("^[0-9]+$", code) & nchar(code) < width
  code[numeric] <- vapply(
    code[numeric],
    function(value) {
      paste0(strrep("0", width - nchar(value)), value)
    },
    character(1)
  )
  code
}

.tabwin_parse_cnv <- function(path) {
  lines <- .tabwin_read_text(path)
  # The first effective line declares the number of categories, comparison
  # width, and (optionally) the literal/range mode.
  useful <- which(nzchar(trimws(lines)) & !startsWith(trimws(lines), ";"))
  if (!length(useful)) {
    cli::cli_abort(
      "TabWin conversion {.file {basename(path)}} is empty."
    )
  }
  header_index <- useful[[1L]]
  header <- trimws(lines[[header_index]])
  match <- regexec(
    "^([0-9]+)\\s+([0-9]+)\\s*([[:alpha:]]*)",
    header
  )
  parts <- regmatches(header, match)[[1L]]
  if (length(parts) != 4L) {
    cli::cli_abort(
      "TabWin conversion {.file {basename(path)}} has an invalid header."
    )
  }
  category_count <- as.integer(parts[[2L]])
  code_width <- as.integer(parts[[3L]])
  mode <- toupper(parts[[4L]])
  if (mode %in% c("F", "FAIXAS")) {
    cli::cli_abort(
      "Numeric-range TabWin conversion {.file {basename(path)}} cannot be used as labels."
    )
  }

  rows <- lines[seq.int(header_index + 1L, length(lines))]
  categories <- list()
  labels <- character()
  for (line in rows) {
    if (!nzchar(trimws(line)) || startsWith(trimws(line), ";")) {
      next
    }
    # CNV is a fixed-width format: sequence in columns 4-7, description in
    # 10-59, and comma-separated source codes from column 61 onward.
    number <- suppressWarnings(as.integer(trimws(substr(line, 4L, 7L))))
    if (is.na(number)) {
      next
    }
    label <- trimws(substr(line, 10L, 59L))
    codes <- sub(";.*$", "", substring(line, 61L))
    codes <- trimws(strsplit(codes, ",", fixed = TRUE)[[1L]])
    codes <- codes[nzchar(codes)]
    if (!length(codes)) {
      next
    }
    # A category may continue on later lines when its code list exceeds the
    # TabWin line limit, hence codes are accumulated by sequence number.
    key <- as.character(number)
    categories[[key]] <- c(categories[[key]], codes)
    if (nzchar(label) && !key %in% names(labels)) {
      labels[[key]] <- label
    }
  }

  map_codes <- character()
  map_labels <- character()
  for (key in names(categories)) {
    if (!key %in% names(labels)) {
      next
    }
    codes <- unlist(
      lapply(categories[[key]], .tabwin_expand_range, width = code_width),
      use.names = FALSE
    )
    codes <- .tabwin_normalize_code(codes, code_width)
    map_codes <- c(map_codes, codes)
    map_labels <- c(map_labels, rep(labels[[key]], length(codes)))
  }
  # Later, more specific categories override broad catch-all ranges declared
  # earlier (for example 1 and 2 override the legacy SEXO range 0-9).
  keep <- !duplicated(map_codes, fromLast = TRUE)
  map <- map_labels[keep]
  names(map) <- map_codes[keep]
  if (!length(map)) {
    cli::cli_abort(
      "TabWin conversion {.file {basename(path)}} contains no code labels."
    )
  }
  structure(
    list(
      type = "cnv",
      code_width = code_width,
      category_count = category_count,
      map = map
    ),
    class = "microdatasus_tabwin_conversion"
  )
}

.tabwin_conversion_key <- function(definition) {
  paste(
    tolower(definition$file),
    toupper(definition$field),
    toupper(definition$argument),
    sep = "::"
  )
}

.tabwin_read_conversion <- function(dictionary, definition) {
  key <- .tabwin_conversion_key(definition)
  # Parsed maps are memoised separately from extracted files. The nested
  # environment is shared even when the dictionary list is copied by R.
  if (exists(key, envir = dictionary$conversions, inherits = FALSE)) {
    return(get(key, envir = dictionary$conversions, inherits = FALSE))
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
        cli::cli_abort(c(
          "Failed to read TabWin table {.file {definition$file}}.",
          "i" = conditionMessage(error)
        ))
      }
    )
    names_upper <- toupper(names(table))
    code_index <- match(toupper(definition$field), names_upper)
    # The TabWin specification uses the first DBF field when the related table
    # does not repeat the source field name.
    if (is.na(code_index)) {
      code_index <- 1L
    }
    label_index <- match(toupper(definition$argument), names_upper)
    if (is.na(label_index)) {
      cli::cli_abort(
        "TabWin table {.file {definition$file}} has no field {.field {definition$argument}}."
      )
    }
    codes <- trimws(as.character(table[[code_index]]))
    labels <- stringi::stri_enc_toutf8(as.character(table[[label_index]]))
    keep <- !is.na(codes) & nzchar(codes) & !duplicated(codes)
    map <- labels[keep]
    names(map) <- codes[keep]
    conversion <- structure(
      list(
        type = "dbf",
        code_width = max(nchar(codes[keep]), 0L),
        category_count = sum(keep),
        map = map
      ),
      class = "microdatasus_tabwin_conversion"
    )
  }
  assign(key, conversion, envir = dictionary$conversions)
  conversion
}

.tabwin_score_definition <- function(dictionary, definition, values) {
  # A source field can have direct labels and several analytical groupings.
  # Score each usable definition against the codes actually present in data.
  conversion <- tryCatch(
    .tabwin_read_conversion(dictionary, definition),
    error = identity
  )
  if (inherits(conversion, "error")) {
    return(NULL)
  }
  observed <- unique(trimws(as.character(values)))
  observed <- observed[!is.na(observed) & nzchar(observed)]
  if (identical(conversion$type, "cnv")) {
    start <- definition$position
    observed <- substring(observed, start, start + conversion$code_width - 1L)
    observed <- .tabwin_normalize_code(observed, conversion$code_width)
  }
  coverage <- if (length(observed)) {
    mean(observed %in% names(conversion$map))
  } else {
    0
  }
  stem <- toupper(tools::file_path_sans_ext(basename(definition$file)))
  exact_name <- identical(gsub("[^A-Z0-9]", "", stem), definition$field)
  list(
    definition = definition,
    conversion = conversion,
    exact_name = exact_name,
    direct_command = definition$command %in% c("X", "D"),
    coverage = coverage,
    codes = length(conversion$map),
    categories = conversion$category_count
  )
}

.tabwin_select_conversion <- function(dictionary, field, values) {
  definitions <- dictionary$definitions
  candidates <- definitions[
    definitions$field == toupper(field) &
      (definitions$extension == "DBF" |
        (!is.na(definitions$position) & definitions$position == 1L)),
    ,
    drop = FALSE
  ]
  if (!nrow(candidates)) {
    return(NULL)
  }
  # A DBF relationship provides the detailed entity description and therefore
  # takes precedence over CNV groupings for the same source field.
  if (any(candidates$extension == "DBF")) {
    candidates <- candidates[
      candidates$extension == "DBF",
      ,
      drop = FALSE
    ]
    # National entity tables (for example TCNESBR and TCHBR) contain every
    # state and avoid opening one DBF per UF merely to compare coverage.
    stems <- toupper(tools::file_path_sans_ext(basename(candidates$file)))
    national <- grepl("BR$", stems)
    if (any(national)) {
      candidates <- candidates[national, , drop = FALSE]
    }
    # TabWin's D command denotes the detailed relation. Prefer it to the
    # alternative line/column groupings of the same procedure or entity.
    if (any(candidates$command == "D")) {
      candidates <- candidates[candidates$command == "D", , drop = FALSE]
    }
    # Annual related tables are named with a numeric suffix (for example,
    # CNES24 and CNES26). Use the newest version declared by the current DEF.
    versions <- regmatches(
      tools::file_path_sans_ext(basename(candidates$file)),
      regexpr("[0-9]+$", tools::file_path_sans_ext(basename(candidates$file)))
    )
    versions <- suppressWarnings(as.integer(versions))
    if (any(!is.na(versions))) {
      candidates <- candidates[
        which.max(replace(versions, is.na(versions), -Inf)),
        ,
        drop = FALSE
      ]
    }
  }
  scores <- lapply(seq_len(nrow(candidates)), function(i) {
    .tabwin_score_definition(
      dictionary,
      candidates[i, , drop = FALSE],
      values
    )
  })
  scores <- Filter(Negate(is.null), scores)
  if (!length(scores)) {
    return(NULL)
  }
  ranking <- data.frame(
    exact_name = vapply(scores, `[[`, logical(1), "exact_name"),
    direct_command = vapply(scores, `[[`, logical(1), "direct_command"),
    coverage = vapply(scores, `[[`, numeric(1), "coverage"),
    codes = vapply(scores, `[[`, integer(1), "codes"),
    categories = vapply(scores, `[[`, integer(1), "categories")
  )
  # Homonymous files and X definitions normally represent direct labels.
  # Coverage then distinguishes current and legacy code systems.
  best <- order(
    -ranking$exact_name,
    -ranking$direct_command,
    -ranking$coverage,
    -ranking$codes,
    -ranking$categories
  )[[1L]]
  scores[[best]]
}

# Return converted text without allocating factor levels. Historical batch
# processing combines every row period before factorizing the complete field.
.tabwin_apply_conversion_values <- function(values, selected) {
  definition <- selected$definition
  conversion <- selected$conversion
  source <- trimws(as.character(values))
  lookup <- source
  if (identical(conversion$type, "cnv")) {
    lookup <- substring(
      lookup,
      definition$position,
      definition$position + conversion$code_width - 1L
    )
    lookup <- .tabwin_normalize_code(lookup, conversion$code_width)
  }
  # Replace known codes only. Unknown codes remain visible so a DataSUS
  # revision cannot silently turn valid information into missing data.
  labels <- unname(conversion$map[lookup])
  result <- source
  matched <- !is.na(labels)
  result[matched] <- labels[matched]
  result[is.na(values)] <- NA_character_
  result
}

# Preserve the factor return used by the original single-conversion helper.
.tabwin_apply_conversion <- function(values, selected) {
  factor(.tabwin_apply_conversion_values(values, selected))
}
