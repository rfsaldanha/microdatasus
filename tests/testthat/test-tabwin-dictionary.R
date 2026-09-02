create_tabwin_fixture <- function() {
  root <- tempfile("tabwin-fixture-")
  tabdo <- file.path(root, "OBITOS_CID10_TAB", "tabdo")
  dir.create(tabdo, recursive = TRUE)
  definition <- c(
    "; SIM-DO test fixture based on the official TabWin format",
    "Ado*.db?",
    "XTipo Obito, TIPOBITO, 1, TIPOBITO.CNV",
    "XSexo, SEXO, 1, SEXO.CNV",
    "XSemanas Gestacao, GESTACAO, 1, SEMANAS.CNV",
    ";XRaca antiga, RACACOR, 1, OLD.CNV",
    "XRaca Cor, RACACOR, 1, RACACOR.CNV",
    "XOcupacao antiga, OCUP, 1, OCUPGRP.CNV",
    "XEstabelecimento, CODESTAB, DESCESTAB, CNES26.DBF"
  )
  write_tabwin_text(
    file.path(tabdo, "Obito_1996_CID10.def"),
    definition
  )
  write_tabwin_text(
    file.path(tabdo, "TIPOBITO.CNV"),
    c(
      "003 1",
      tabwin_cnv_line(3, "Ignorado", "9"),
      tabwin_cnv_line(1, "Fetal", "1"),
      tabwin_cnv_line(2, "Não Fetal", "2")
    )
  )
  write_tabwin_text(
    file.path(tabdo, "SEXO.CNV"),
    c(
      "3 1",
      tabwin_cnv_line(3, "I", "I,0,9"),
      tabwin_cnv_line(1, "M", "M,1"),
      tabwin_cnv_line(2, "F", "F,2")
    )
  )
  write_tabwin_text(
    file.path(tabdo, "SEMANAS.CNV"),
    c(
      "008 1 L",
      tabwin_cnv_line(1, "Ignorado", "9"),
      tabwin_cnv_line(5, "32 a 36", "4")
    )
  )
  write_tabwin_text(
    file.path(tabdo, "RACACOR.CNV"),
    c(
      "6 1 L",
      tabwin_cnv_line(1, "N Inf", ""),
      tabwin_cnv_line(2, "Bra", "1"),
      tabwin_cnv_line(3, "Preta", "2"),
      tabwin_cnv_line(4, "Amar", "3"),
      tabwin_cnv_line(5, "Parda", "4"),
      tabwin_cnv_line(6, "Indig", "5")
    )
  )
  write_tabwin_text(
    file.path(tabdo, "OCUPGRP.CNV"),
    c(
      "2 3",
      tabwin_cnv_line(1, "Dona de casa", "008"),
      tabwin_cnv_line(2, "Trabalhador agropecuario", "621")
    )
  )
  foreign::write.dbf(
    data.frame(
      CD_CNES = "0000001",
      DESCESTAB = "0000001 POSTO DE SAUDE PARQUE AGUA LIMPA",
      stringsAsFactors = FALSE
    ),
    file.path(tabdo, "CNES26.DBF")
  )
  archive <- tempfile(fileext = ".zip")
  zip::zipr(
    archive,
    files = "OBITOS_CID10_TAB",
    root = root
  )
  unlink(root, recursive = TRUE)
  archive
}

create_sim_legacy_tabwin_fixture <- function() {
  root <- tempfile("sim-cid9-fixture-")
  directory <- file.path(root, "OBITOS_CID9_TAB")
  dir.create(directory, recursive = TRUE)
  definition <- c(
    "; SIM CID-9 fixture based on the official historical archive",
    "Ado*.db?",
    "XEstado Civil, ESTCIVIL, 1, ESTCIV.CNV",
    "XOcupacao Mae, OCUPMAE, 1, OCUPACAO.CNV",
    "XInstrucao Mae, INSTRMAE, 1, INSTRUC.CNV",
    "XSemanas Gestacao, SEMANGEST, 1, SEMANAS.CNV",
    "XGravidez, TIPOGRAV, 1, GRAVIDEZ.CNV",
    "XTipo Parto, TIPOPARTO, 1, PARTO.CNV",
    "XAtestante, ATESTANTE, 1, ATESTANT.CNV",
    "XTipo Violencia, TIPOVIOL, 1, TIPOVIOL.CNV",
    "XTipo Acidente, TIPOACID, 1, TIPOACID.CNV",
    "XLocal Acidente, LOCACID, 1, LOCACID.CNV"
  )
  write_tabwin_text(file.path(directory, "OBITO.DEF"), definition)
  conversions <- list(
    "ESTCIV.CNV" = c("005 1", tabwin_cnv_line(2, "Casado", "2")),
    "OCUPACAO.CNV" = c(
      "002 3",
      tabwin_cnv_line(8, "Dona-de-casa", "008"),
      tabwin_cnv_line(178, "Trab agropec poliv", "621")
    ),
    "INSTRUC.CNV" = c(
      "005 1",
      tabwin_cnv_line(1, "Ignorado", "0-9"),
      tabwin_cnv_line(2, "Nenhuma", "1"),
      tabwin_cnv_line(5, "Superior", "4")
    ),
    "SEMANAS.CNV" = c(
      "009 1",
      tabwin_cnv_line(1, "Ignorado", "0-9"),
      tabwin_cnv_line(5, "Menos 22", "4"),
      tabwin_cnv_line(8, "42 e mais", "8")
    ),
    "GRAVIDEZ.CNV" = c(
      "005 1",
      tabwin_cnv_line(1, "Ignorado", "0-9"),
      tabwin_cnv_line(5, "Mais de 3", "4")
    ),
    "PARTO.CNV" = c(
      "005 1",
      tabwin_cnv_line(1, "Ignorado", "0-9"),
      tabwin_cnv_line(4, "Fórceps", "3"),
      tabwin_cnv_line(5, "Outro", "4")
    ),
    "ATESTANT.CNV" = c(
      "006 1",
      tabwin_cnv_line(1, "Ignorado", "0-9"),
      tabwin_cnv_line(2, "Sim", "1")
    ),
    "TIPOVIOL.CNV" = c(
      "005 1",
      tabwin_cnv_line(1, "Ignorado", "0-9"),
      tabwin_cnv_line(2, "Homicídio", "1"),
      tabwin_cnv_line(4, "Acidente", "3")
    ),
    "TIPOACID.CNV" = c(
      "006 1",
      tabwin_cnv_line(1, "Ignorado", "0-9"),
      tabwin_cnv_line(2, "Atropelamento", "1")
    ),
    "LOCACID.CNV" = c(
      "005 1",
      tabwin_cnv_line(1, "Ignorado", "0-9"),
      tabwin_cnv_line(5, "Local de Trabalho", "4")
    )
  )
  for (file in names(conversions)) {
    write_tabwin_text(file.path(directory, file), conversions[[file]])
  }
  archive <- tempfile(fileext = ".zip")
  zip::zipr(archive, files = "OBITOS_CID9_TAB", root = root)
  unlink(root, recursive = TRUE)
  archive
}

test_that("process_sim appends its data type argument compatibly", {
  expect_identical(
    as.pairlist(formals(process_sim)[c("data", "municipality_data", "information_system")]),
    as.pairlist(alist(
      data = ,
      municipality_data = TRUE,
      information_system = "SIM-DO"
    ))
  )
})

test_that("TabWin registry covers current and historical SIM dictionaries", {
  expected <- c(
    "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT"
  )
  expect_setequal(microdatasus:::.sim_information_systems, expected)
  expect_true(
    "SIM-DO-CID9" %in% names(microdatasus:::.tabwin_registry())
  )
})

test_that("DEF parser reads active CNV metadata without commented entries", {
  archive <- create_tabwin_fixture()
  on.exit(unlink(archive), add = TRUE)
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      downloads <<- downloads + 1L
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  dictionary <- fetch_tabwin_dictionary(quiet = TRUE)

  expect_s3_class(dictionary, "microdatasus_tabwin_dictionary")
  expect_setequal(
    dictionary$definitions$field,
    c("TIPOBITO", "SEXO", "GESTACAO", "RACACOR", "OCUP", "CODESTAB")
  )
  expect_false("OLD.CNV" %in% dictionary$definitions$file)
  expect_equal(downloads, 1L)

  cached <- fetch_tabwin_dictionary(quiet = TRUE)
  expect_identical(cached, dictionary)
  expect_equal(downloads, 1L)

  refreshed <- fetch_tabwin_dictionary(refresh = TRUE, quiet = TRUE)
  expect_s3_class(refreshed, "microdatasus_tabwin_dictionary")
  expect_equal(downloads, 2L)
})

test_that("DEF increment parser recovers documented official field drift", {
  directory <- tempfile("tabwin-increments-")
  dir.create(directory)
  tuberculosis <- file.path(directory, "TuberculNET5_0.def")
  diphtheria <- file.path(directory, "DifteriNET.def")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  write_tabwin_text(tuberculosis, c(
    "IContatos identificados, nu_contato (observação oficial)",
    "IContatos examinados, NU_COMU_EX",
    "I* cabeçalho, comentário sem campo"
  ))
  write_tabwin_text(diphtheria, "IComunicantes portadores, lMED_QUAN_P")

  expect_identical(
    microdatasus:::.tabwin_parse_increment_fields(tuberculosis),
    c("NU_CONTATO", "NU_COMU_EX")
  )
  expect_identical(
    microdatasus:::.tabwin_parse_increment_fields(diphtheria),
    "MED_QUAN_P"
  )
})

test_that("DEF parser recovers documented official relation-field drift", {
  path <- tempfile(fileext = ".DEF")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "LSubtipo FAEC, FAEC_TP, DS_TPFIN, CNV/TP_FINAN.CNV",
    paste(
      "SNome Fantasia, CNES, NOMEFANT,",
      "DBF/HUF_FILIAL.DBFRA, DBF/HUF_FILIAL.DBF"
    ),
    paste(
      "LCID, PA_CIDPRI, CD_DESCR, DBF/S_CID.DBF,",
      "CD_DESCR, DBF/S_CID.DBF"
    ),
    "LInvalid position, CODE, not prose, MAP.CNV",
    "LZero position, CODE, 0, MAP.CNV"
  ))

  definitions <- microdatasus:::.tabwin_parse_def(path)

  expect_identical(
    definitions$position,
    c(1L, NA_integer_, NA_integer_, NA_integer_, 0L)
  )
  expect_identical(
    definitions$position_recovered,
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_identical(
    definitions$file,
    c(
      "CNV/TP_FINAN.CNV", "DBF/HUF_FILIAL.DBF",
      "DBF/S_CID.DBF", "MAP.CNV", "MAP.CNV"
    )
  )
  expect_identical(
    definitions$file_recovered,
    c(FALSE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("CNV parser reads labels, aliases, and numeric ranges", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(
    path,
    c(
      "2 2",
      tabwin_cnv_line(1, "Primeiro", "01-03"),
      tabwin_cnv_line(2, "Segundo", "09")
    )
  )

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(
    unname(conversion$map[c("01", "02", "03", "09")]),
    c("Primeiro", "Primeiro", "Primeiro", "Segundo")
  )
})

test_that("literal CNV codes can span adjacent physical DBF fields", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(
    path,
    c(
      "2 4 L",
      tabwin_cnv_line(1, "Primeiro", "1000"),
      tabwin_cnv_line(2, "Segundo", "0101")
    )
  )
  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  definition <- data.frame(
    field = "FLAG1", extension = "CNV", position = 1L,
    command = "L", file = path, argument = "1",
    stringsAsFactors = FALSE
  )
  dictionary <- list(
    definitions = definition,
    conversions = new.env(parent = emptyenv())
  )
  key <- microdatasus:::.tabwin_conversion_key(definition)
  assign(key, conversion, envir = dictionary$conversions)
  data <- data.frame(
    FLAG1 = c("1", "0"), FLAG2 = c("0", "1"),
    FLAG5 = c("0", "0"), FLAG6 = c("0", "1"),
    stringsAsFactors = FALSE
  )
  attr(data, "dbf_field_types") <- c(
    FLAG1 = "C", FLAG2 = "C", FLAG5 = "C", FLAG6 = "C"
  )
  attr(data, "dbf_field_widths") <- c(
    FLAG1 = 1L, FLAG2 = 1L, FLAG5 = 1L, FLAG6 = 1L
  )

  selected <- microdatasus:::.tabwin_select_conversion(
    dictionary, "FLAG1", data$FLAG1, data, "FLAG1"
  )

  expect_identical(selected$source_values, c("1000", "0101"))
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      selected$source_values, selected
    ),
    c("Primeiro", "Segundo")
  )
})

test_that("physical DEF reconstruction is chosen only when coverage improves", {
  definition <- data.frame(position = 1L)
  country_conversion <- list(
    type = "cnv", mode = "", code_width = 3L,
    map = c("010" = "Brasil")
  )
  country <- data.frame(
    NACION_PAC = c("01", "01"), SEXO = c("M", "F"),
    stringsAsFactors = FALSE
  )
  attr(country, "dbf_field_types") <- c(NACION_PAC = "C", SEXO = "C")
  attr(country, "dbf_field_widths") <- c(NACION_PAC = 2L, SEXO = 1L)

  country_values <- microdatasus:::.tabwin_definition_values(
    country, "NACION_PAC", definition, country_conversion,
    country$NACION_PAC
  )
  expect_identical(country_values, c("010", "010"))

  service_conversion <- list(
    type = "dbf", mode = "", code_width = 6L,
    map = c("115001" = "Servico")
  )
  service <- data.frame(
    PA_SRV = "115", PA_CLASS_S = "001", SIT_RUA = "",
    stringsAsFactors = FALSE
  )
  attr(service, "dbf_field_types") <- c(
    PA_SRV = "C", PA_CLASS_S = "C", SIT_RUA = "C"
  )
  attr(service, "dbf_field_widths") <- c(
    PA_SRV = 3L, PA_CLASS_S = 3L, SIT_RUA = 3L
  )

  service_values <- microdatasus:::.tabwin_definition_values(
    service, "PA_SRV", definition, service_conversion, service$PA_SRV
  )
  expect_identical(service_values, "115001")

  equipment_conversion <- list(
    type = "cnv", mode = "L", code_width = 4L,
    map = c("0221" = "Equipamento")
  )
  equipment <- data.frame(
    TIPEQUIP = "2", CODEQUIP = "21", QT_EXIST = "1",
    stringsAsFactors = FALSE
  )
  attr(equipment, "dbf_field_types") <- c(
    TIPEQUIP = "C", CODEQUIP = "C", QT_EXIST = "N"
  )
  attr(equipment, "dbf_field_widths") <- c(
    TIPEQUIP = 1L, CODEQUIP = 2L, QT_EXIST = 3L
  )

  equipment_values <- microdatasus:::.tabwin_definition_values(
    equipment, "TIPEQUIP", definition, equipment_conversion,
    equipment$TIPEQUIP
  )
  expect_identical(equipment_values, "0221")
})

test_that("physical DEF reconstruction preserves numeric right padding", {
  definition <- data.frame(position = 1L)
  conversion <- list(
    type = "cnv", mode = "", code_width = 3L,
    map = c("100" = "One hundred")
  )
  data <- data.frame(CODE = "1", stringsAsFactors = FALSE)
  attr(data, "dbf_field_types") <- c(CODE = "C")
  attr(data, "dbf_field_widths") <- c(CODE = 3L)

  values <- microdatasus:::.tabwin_definition_values(
    data, "CODE", definition, conversion, data$CODE
  )

  expect_identical(values, "1  ")
})

test_that("physical DEF reconstruction does not truncate complete DBF keys", {
  definition <- data.frame(position = NA_integer_)
  conversion <- list(
    type = "dbf", mode = "", code_width = 4L,
    map = c("3200" = "Regional")
  )
  data <- data.frame(ID_RG_RESI = "32002", stringsAsFactors = FALSE)
  attr(data, "dbf_field_types") <- c(ID_RG_RESI = "C")
  attr(data, "dbf_field_widths") <- c(ID_RG_RESI = 5L)

  values <- microdatasus:::.tabwin_definition_values(
    data, "ID_RG_RESI", definition, conversion, data$ID_RG_RESI
  )

  expect_identical(values, "32002")
})

test_that("specific CNV categories override earlier catch-all ranges", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(
    path,
    c(
      "3 1",
      tabwin_cnv_line(3, "Ignorado", "0-9"),
      tabwin_cnv_line(1, "Masculino", "1"),
      tabwin_cnv_line(2, "Feminino", "2")
    )
  )

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(
    unname(conversion$map[c("1", "2", "9")]),
    c("Masculino", "Feminino", "Ignorado")
  )
})

test_that("CNV parser preserves analytical ranges symbolically", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(
    path,
    c(
      "1 8",
      tabwin_cnv_line(1, "Faixa analitica", "00000000-89999999")
    )
  )

  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L), conversion = conversion
  )

  expect_length(conversion$map, 0L)
  expect_identical(conversion$ranges$token, "00000000-89999999")
  expect_identical(
    as.character(microdatasus:::.tabwin_apply_conversion(
      c("00000001", "90000000"), selected
    )),
    c("Faixa analitica", "90000000")
  )
})

test_that("symbolic ranges preserve later-category priority", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "2 8",
    tabwin_cnv_line(1, "Faixa ampla", "00000000-89999999"),
    tabwin_cnv_line(2, "Codigo especifico", "00000001")
  ))
  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(definition = data.frame(position = 1L), conversion = conversion)

  result <- microdatasus:::.tabwin_apply_conversion_values(
    c("00000001", "00000002", "90000000"), selected
  )

  expect_identical(result, c("Codigo especifico", "Faixa ampla", "90000000"))
})

test_that("literal CNV ranges support open bounds and later overrides", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "2 10 L",
    tabwin_cnv_line(2, "Inscrito", "-ZZZZZZZZZZ"),
    tabwin_cnv_line(1, "Não inscrito", "0000000000")
  ))
  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L), conversion = conversion
  )

  result <- microdatasus:::.tabwin_apply_conversion_values(
    c("1208100112", "0000000000", NA_character_), selected
  )

  expect_identical(conversion$ranges$kind, "literal")
  expect_identical(names(conversion$map), "0000000000")
  expect_identical(result, c("Inscrito", "Não inscrito", NA_character_))
})

test_that("symbolic alphanumeric CNV ranges retain their prefix", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "1 7",
    tabwin_cnv_line(1, "Faixa alfa", "A000000-A999999")
  ))
  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(definition = data.frame(position = 1L), conversion = conversion)

  result <- microdatasus:::.tabwin_apply_conversion_values(
    c("A123456", "B123456"), selected
  )

  expect_identical(conversion$ranges$kind, "alphanumeric")
  expect_identical(result, c("Faixa alfa", "B123456"))
})

test_that("DBF relationships use the description field declared by DEF", {
  archive <- create_tabwin_fixture()
  on.exit(unlink(archive), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)
  dictionary <- fetch_tabwin_dictionary(quiet = TRUE)

  selected <- microdatasus:::.tabwin_select_conversion(
    dictionary,
    "CODESTAB",
    "0000001"
  )
  result <- microdatasus:::.tabwin_apply_conversion("0000001", selected)

  expect_identical(
    as.character(result),
    "0000001 POSTO DE SAUDE PARQUE AGUA LIMPA"
  )
})

test_that("DBF selection prefers national and detailed official relations", {
  definitions <- data.frame(
    order = 1:4,
    command = c("L", "L", "D", "L"),
    description = c("CNES BR", "CNES AC", "Detalhado", "Grupo"),
    field = c("CNES", "CNES", "PROCED", "PROCED"),
    argument = "LABEL",
    position = NA_integer_,
    file = c(
      "DBF/TCNESBR.DBF", "DBF/TCNESAC.DBF",
      "DBF/TB_SIGTAW.DBF", "DBF/TB_GRUPO.DBF"
    ),
    extension = "DBF",
    stringsAsFactors = FALSE
  )
  dictionary <- list(
    definitions = definitions,
    conversions = new.env(parent = emptyenv())
  )
  for (i in seq_len(nrow(definitions))) {
    conversion <- structure(
      list(
        type = "dbf",
        code_width = 1L,
        category_count = 1L,
        map = stats::setNames(definitions$description[[i]], "1")
      ),
      class = "microdatasus_tabwin_conversion"
    )
    assign(
      microdatasus:::.tabwin_conversion_key(
        definitions[i, , drop = FALSE]
      ),
      conversion,
      envir = dictionary$conversions
    )
  }

  cnes <- microdatasus:::.tabwin_select_conversion(dictionary, "CNES", "1")
  procedure <- microdatasus:::.tabwin_select_conversion(
    dictionary,
    "PROCED",
    "1"
  )

  expect_identical(cnes$definition$file, "DBF/TCNESBR.DBF")
  expect_identical(procedure$definition$file, "DBF/TB_SIGTAW.DBF")
})

test_that("selection evaluates CNV fallbacks and substring positions", {
  definitions <- data.frame(
    order = 1:3,
    command = c("L", "D", "X"),
    description = c("Missing DBF", "Wrong D relation", "Position two"),
    field = "CODE",
    argument = c("LABEL", "1", "2"),
    position = c(NA_integer_, 1L, 2L),
    file = c("MISSING.DBF", "WRONG.CNV", "POSITION.CNV"),
    extension = c("DBF", "CNV", "CNV"),
    stringsAsFactors = FALSE
  )
  dictionary <- list(
    definitions = definitions,
    conversions = new.env(parent = emptyenv())
  )
  conversion <- function(map) {
    structure(
      list(
        type = "cnv", code_width = 1L, category_count = length(map),
        map = map, map_priority = stats::setNames(seq_along(map), names(map)),
        ranges = microdatasus:::.tabwin_empty_ranges()
      ),
      class = "microdatasus_tabwin_conversion"
    )
  }
  assign(
    microdatasus:::.tabwin_conversion_key(definitions[2, , drop = FALSE]),
    conversion(c("Z" = "Wrong")),
    envir = dictionary$conversions
  )
  assign(
    microdatasus:::.tabwin_conversion_key(definitions[3, , drop = FALSE]),
    conversion(c("2" = "Position matched")),
    envir = dictionary$conversions
  )

  selected <- microdatasus:::.tabwin_select_conversion(
    dictionary, "CODE", "A2"
  )

  expect_identical(selected$definition$file, "POSITION.CNV")
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values("A2", selected),
    "Position matched"
  )
})

test_that("specific matches outrank full-domain CNV fallbacks", {
  definitions <- data.frame(
    order = 1:2,
    command = "X",
    description = c("Generic financing type", "Financing"),
    field = "PA_TPFIN",
    argument = "1",
    position = 1L,
    file = c("TP_FINAN.CNV", "FINANC.CNV"),
    extension = "CNV",
    stringsAsFactors = FALSE
  )
  dictionary <- list(
    definitions = definitions,
    conversions = new.env(parent = emptyenv())
  )
  catch_all <- structure(
    list(
      type = "cnv", code_width = 2L, category_count = 1L,
      map = stats::setNames(character(), character()),
      map_priority = stats::setNames(integer(), character()),
      ranges = data.frame(
        token = "00-99", kind = "numeric", prefix = "",
        lower = 0, upper = 99, size = 100, width = 2L,
        label = "Nao discriminado", priority = 1L,
        stringsAsFactors = FALSE
      )
    ),
    class = "microdatasus_tabwin_conversion"
  )
  specific <- structure(
    list(
      type = "cnv", code_width = 2L, category_count = 1L,
      map = c("02" = "Assistencia Farmaceutica"),
      map_priority = c("02" = 1L),
      ranges = microdatasus:::.tabwin_empty_ranges()
    ),
    class = "microdatasus_tabwin_conversion"
  )
  assign(
    microdatasus:::.tabwin_conversion_key(definitions[1, , drop = FALSE]),
    catch_all,
    envir = dictionary$conversions
  )
  assign(
    microdatasus:::.tabwin_conversion_key(definitions[2, , drop = FALSE]),
    specific,
    envir = dictionary$conversions
  )

  selected <- microdatasus:::.tabwin_select_conversion(
    dictionary, "PA_TPFIN", rep("02", 10)
  )

  expect_identical(selected$definition$file, "FINANC.CNV")
  expect_equal(selected$specific_coverage, 1)
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values("02", selected),
    "Assistencia Farmaceutica"
  )
})

test_that("relation revisions do not confuse domain codes with years", {
  expect_equal(microdatasus:::.tabwin_relation_revision("CNES26.DBF"), 26)
  expect_equal(microdatasus:::.tabwin_relation_revision("CNES2026.DBF"), 2026)
  expect_equal(microdatasus:::.tabwin_relation_revision("P040605.DBF"), -Inf)
})

test_that("all SIM types share one TabWin archive download", {
  archive <- create_tabwin_fixture()
  on.exit(unlink(archive), add = TRUE)
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      downloads <<- downloads + 1L
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  sim_types <- microdatasus:::.sim_information_systems
  dictionaries <- lapply(
    sim_types,
    fetch_tabwin_dictionary,
    quiet = TRUE
  )

  expect_equal(downloads, 1L)
  expect_identical(
    vapply(dictionaries, `[[`, character(1), "information_system"),
    sim_types
  )
  expect_length(unique(vapply(
    dictionaries,
    `[[`,
    character(1),
    "archive"
  )), 1L)
})

test_that("process_sim applies official-style labels and stable types", {
  archive <- create_tabwin_fixture()
  on.exit(unlink(archive), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)
  fetch_tabwin_dictionary(quiet = TRUE)

  data <- data.frame(
    CONTADOR = c("1", "2"),
    TIPOBITO = c("1", "2"),
    DTOBITO = c("01012024", "02012024"),
    IDADE = c("405", "501"),
    SEXO = c("1", "2"),
    RACACOR = c("4", "8"),
    CODESTAB = c("0000001", "9999999"),
    CODMUNRES = c("120020", "120030"),
    stringsAsFactors = FALSE
  )
  result <- process_sim(data, municipality_data = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_s3_class(result$DTOBITO, "Date")
  expect_type(result$CONTADOR, "integer")
  expect_type(result$IDADEanos, "integer")
  expect_s3_class(result$TIPOBITO, "factor")
  expect_s3_class(result$SEXO, "factor")
  expect_s3_class(result$RACACOR, "factor")
  expect_s3_class(result$CODESTAB, "factor")
  expect_identical(as.character(result$TIPOBITO), c("Fetal", "Não Fetal"))
  expect_identical(as.character(result$SEXO), c("M", "F"))
  expect_identical(as.character(result$RACACOR), c("Parda", "8"))
  expect_identical(
    as.character(result$CODESTAB),
    c("0000001 POSTO DE SAUDE PARQUE AGUA LIMPA", "9999999")
  )
  expect_identical(result$IDADEanos, c(5L, 101L))
  expect_identical(result$CODMUNRES, c("120020", "120030"))
})

test_that("process_sim reports caching and pre-processing in order", {
  archive <- create_tabwin_fixture()
  on.exit(unlink(archive), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  messages <- capture_messages(
    process_sim(
      data.frame(TIPOBITO = "2"),
      municipality_data = FALSE
    )
  )
  patterns <- c(
    "Cached the DataSUS TabWin dictionary",
    "Starting SIM-DO data pre-processing",
    "Finished SIM-DO data pre-processing"
  )
  positions <- vapply(patterns, function(pattern) {
    which(grepl(pattern, messages, fixed = TRUE))[[1L]]
  }, integer(1))

  expect_true(all(diff(positions) > 0L))
})

test_that("process_sim handles every SIM type and legacy numeric names", {
  archive <- create_tabwin_fixture()
  on.exit(unlink(archive), add = TRUE)
  downloads <- 0L
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      downloads <<- downloads + 1L
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  sim_types <- microdatasus:::.sim_information_systems
  for (information_system in sim_types) {
    messages <- capture_messages({
      result <- process_sim(
        data.frame(
          contador = "1",
          TIPOBITO = "2",
          DTOBITO = "01012024",
          SEMAGESTAC = "38",
          stringsAsFactors = FALSE
        ),
        municipality_data = FALSE,
        information_system = information_system
      )
    })
    expect_type(result$contador, "integer")
    expect_type(result$SEMAGESTAC, "integer")
    expect_s3_class(result$DTOBITO, "Date")
    expect_identical(as.character(result$TIPOBITO), "Não Fetal")
    expect_true(any(grepl(
      paste("Starting", information_system, "data pre-processing"),
      messages,
      fixed = TRUE
    )))
    expect_true(any(grepl(
      paste("Finished", information_system, "data pre-processing"),
      messages,
      fixed = TRUE
    )))
  }
  expect_equal(downloads, 1L)
})

test_that("process_sim uses the official CID-9 domains for legacy rows", {
  current_archive <- create_tabwin_fixture()
  legacy_archive <- create_sim_legacy_tabwin_fixture()
  on.exit(unlink(c(current_archive, legacy_archive)), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      archive <- if (grepl("CID9", url, fixed = TRUE)) {
        legacy_archive
      } else {
        current_archive
      }
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  result <- process_sim(
    data.frame(
      UFINFORM = c("12", "12"),
      ESTCIV = c("2", "2"),
      OCUPMAE = c("008", "621"),
      INSTRMAE = c("0", "4"),
      GESTACAO = c("4", "8"),
      SEMANGEST = c("4", "8"),
      TIPOGRAV = c("4", "0"),
      TIPOPARTO = c("3", "4"),
      ATESTANTE = c("0", "1"),
      TIPOVIOL = c("1", "3"),
      TIPOACID = c("1", "0"),
      LOCACID = c("4", "0"),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE,
    information_system = "SIM-DOFET",
    labels = "character"
  )

  expect_identical(result$ESTCIV, rep("Casado", 2L))
  expect_identical(
    result$OCUPMAE,
    c("Dona-de-casa", "Trab agropec poliv")
  )
  expect_identical(result$INSTRMAE, c("Ignorado", "Superior"))
  expect_identical(result$GESTACAO, c("Menos 22", "42 e mais"))
  expect_identical(result$SEMANGEST, c("Menos 22", "42 e mais"))
  expect_identical(result$TIPOGRAV, c("Mais de 3", "Ignorado"))
  expect_identical(result$TIPOPARTO, c("Fórceps", "Outro"))
  expect_identical(result$ATESTANTE, c("Ignorado", "Sim"))
  expect_identical(result$TIPOVIOL, c("Homicídio", "Acidente"))
  expect_identical(result$TIPOACID, c("Atropelamento", "Ignorado"))
  expect_identical(result$LOCACID, c("Local de Trabalho", "Ignorado"))
})

test_that("process_sim separates current and historical domains by row", {
  current_archive <- create_tabwin_fixture()
  legacy_archive <- create_sim_legacy_tabwin_fixture()
  on.exit(unlink(c(current_archive, legacy_archive)), add = TRUE)
  local_mocked_bindings(
    .datasus_download_file = function(
      url,
      destination,
      timeout,
      quiet = FALSE
    ) {
      archive <- if (grepl("CID9", url, fixed = TRUE)) {
        legacy_archive
      } else {
        current_archive
      }
      file.copy(archive, destination)
      invisible(destination)
    },
    .package = "microdatasus"
  )
  microdatasus:::.tabwin_clear_cache()
  on.exit(restore_empty_tabwin_cache(), add = TRUE)

  result <- process_sim(
    data.frame(
      UFINFORM = c("12", NA),
      GESTACAO = c("4", "4"),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE,
    labels = "character"
  )

  expect_identical(result$GESTACAO, c("Menos 22", "32 a 36"))
})

test_that("process_sim enforces the official legacy child-count domain", {
  result <- process_sim(
    data.frame(
      UFINFORM = c(rep("12", 10L), NA),
      FILHVIVOS = c(
        "XX", "00", "01", "15", "16", "20", "21", "50", "51", "99", NA
      ),
      QTDFILMORT = c(
        "XX", "00", "01", "15", "16", "20", "21", "50", "51", "99", "00"
      ),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE,
    labels = "none",
    diagnostics = TRUE
  )

  legacy_expected <- c(
    0L, NA_integer_, 1L, 15L, 16L, 20L, 21L, 50L, NA_integer_,
    NA_integer_, NA_integer_
  )
  mixed_expected <- legacy_expected
  mixed_expected[[11L]] <- 0L
  expect_identical(result$FILHVIVOS, legacy_expected)
  expect_identical(result$QTDFILMORT, mixed_expected)
  report <- processing_diagnostics(result)
  expect_equal(nrow(report$coercion_failures), 0L)
})

test_that("process_sim enforces the official birth-weight domain", {
  result <- process_sim(
    data.frame(
      PESO = c("0000", "0001", "8000", "8001", "9999", "0 00", NA),
      PESONASC = c("0000", "0001", "8000", "8001", "9999", "0 00", NA),
      stringsAsFactors = FALSE
    ),
    municipality_data = FALSE,
    labels = "none",
    diagnostics = TRUE
  )

  expected <- c(NA_integer_, 1L, 8000L, NA_integer_, NA_integer_,
                NA_integer_, NA_integer_)
  expect_identical(result$PESO, expected)
  expect_identical(result$PESONASC, expected)
  report <- processing_diagnostics(result)
  expect_setequal(
    unique(report$coercion_failures$field),
    c("PESO", "PESONASC")
  )
  expect_setequal(
    unique(report$coercion_failures$value),
    c("8001", "0 00")
  )
  expect_equal(sum(report$coercion_failures$n), 4L)
})

test_that("process_sim rejects unsupported SIM data types", {
  expect_error(
    process_sim(
      data.frame(x = "1"),
      municipality_data = FALSE,
      information_system = "SIM-UNKNOWN"
    ),
    "information_system"
  )
})

test_that("CNV parser supports official long and compact dialect prefixes", {
  long <- tempfile(fileext = ".CNV")
  compact <- tempfile(fileext = ".CNV")
  on.exit(unlink(c(long, compact)), add = TRUE)
  long_row <- paste0(
    sprintf("%4s", ""), " ", sprintf("%4d", 1L), "  ",
    sprintf("%-100s", "Long description"), " ", "A001"
  )
  compact_row <- tabwin_cnv_line(1, "Compact description", "01")
  write_tabwin_text(long, c("N 1 4 L", long_row))
  write_tabwin_text(compact, c("s 1 2", compact_row))

  long_conversion <- microdatasus:::.tabwin_parse_cnv(long)
  compact_conversion <- microdatasus:::.tabwin_parse_cnv(compact)

  expect_identical(unname(long_conversion$map[["A001"]]), "Long description")
  expect_identical(unname(compact_conversion$map[["01"]]), "Compact description")
})

test_that("CNV continuations resolve labels by category sequence", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "3 1 L",
    tabwin_cnv_line(1, "Norte", "A"),
    tabwin_cnv_line(2, "Sul", "B"),
    tabwin_cnv_line(1, "", "C")
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(unname(conversion$map[c("A", "B", "C")]),
                   c("Norte", "Sul", "Norte"))
})

test_that("CNV parser distinguishes literal and numeric padding", {
  literal <- tempfile(fileext = ".CNV")
  numeric <- tempfile(fileext = ".CNV")
  on.exit(unlink(c(literal, numeric)), add = TRUE)
  write_tabwin_text(literal, c(
    "2 3 L",
    tabwin_cnv_line(1, "Right padded", "1  "),
    tabwin_cnv_line(2, "Leading zeroes", "001")
  ))
  write_tabwin_text(numeric, c(
    "1 3",
    tabwin_cnv_line(1, "One hundred", "1  ,10 ")
  ))

  literal_conversion <- microdatasus:::.tabwin_parse_cnv(literal)
  numeric_conversion <- microdatasus:::.tabwin_parse_cnv(numeric)
  literal_selected <- list(
    definition = data.frame(position = 1L),
    conversion = literal_conversion
  )
  numeric_selected <- list(
    definition = data.frame(position = 1L),
    conversion = numeric_conversion
  )

  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("1  ", "001", "1"), literal_selected
    ),
    c("Right padded", "Leading zeroes", "Right padded")
  )
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("100", "1  ", "10 "), numeric_selected
    ),
    rep("One hundred", 3)
  )
})

test_that("CNV parser recovers contradictory short numeric aliases", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "2 2",
    tabwin_cnv_line(1, "01", "1 ,"),
    tabwin_cnv_line(2, "10", "10")
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  expect_identical(
    unname(conversion[["map"]][c("01", "10")]),
    c("01", "10")
  )
  expect_identical(
    conversion[["recovered_numeric_collision_codes"]],
    1L
  )
  expect_identical(conversion[["normalized_collisions"]], 0L)
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(c("1", "10"), selected),
    c("01", "10")
  )
})

test_that("CNV widths of five or more are implicitly alphanumeric", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "3 5",
    tabwin_cnv_line(1, "Right padded", "1    "),
    tabwin_cnv_line(2, "Leading zeroes", "001  "),
    tabwin_cnv_line(3, "Blank literal", "     ,")
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  expect_identical(
    names(conversion$map),
    c("1    ", "001  ", "     ")
  )
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("1", "001", "", "1    ", "001  "), selected
    ),
    c("Right padded", "Leading zeroes", "Blank literal",
      "Right padded", "Leading zeroes")
  )
  expect_identical(
    microdatasus:::.tabwin_normalize_code("123456", 5L),
    "12345"
  )
})

test_that("implicit alphanumeric CNVs support open literal ranges", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "2 5",
    tabwin_cnv_line(2, "Through Z", "-ZZZZZ"),
    tabwin_cnv_line(1, "Exact", "00000")
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  expect_identical(conversion$ranges$kind, "literal")
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("00000", "ABCDE", "ZZZZZ", "zzzzz"), selected
    ),
    c("Exact", "Through Z", "Through Z", "zzzzz")
  )
})

test_that("short CNVs infer literal ranges from alphabetic bounds", {
  cross <- tempfile(fileext = ".CNV")
  open <- tempfile(fileext = ".CNV")
  on.exit(unlink(c(cross, open)), add = TRUE)
  write_tabwin_text(
    cross,
    c("1 3", tabwin_cnv_line(1, "Chapters A-B", "A00-B99"))
  )
  write_tabwin_text(
    open,
    c("1 4", tabwin_cnv_line(1, "Through z", "-zzzz"))
  )

  cross_conversion <- microdatasus:::.tabwin_parse_cnv(cross)
  open_conversion <- microdatasus:::.tabwin_parse_cnv(open)
  cross_selected <- list(
    definition = data.frame(position = 1L),
    conversion = cross_conversion
  )
  open_selected <- list(
    definition = data.frame(position = 1L),
    conversion = open_conversion
  )

  expect_identical(cross_conversion$ranges$kind, "literal")
  expect_identical(open_conversion$ranges$kind, "literal")
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("A00", "B99", "C00"), cross_selected
    ),
    c("Chapters A-B", "Chapters A-B", "C00")
  )
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("0000", "A000", "zzzz", "zzzzz"), open_selected
    ),
    c("Through z", "Through z", "Through z", "Through z")
  )
})

test_that("implicit alphanumeric CNVs support padded closed ranges", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "2 6",
    tabwin_cnv_line(1, "1981", "1981  -198112"),
    tabwin_cnv_line(2, "January 1981", "198101")
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  expect_identical(conversion$ranges$kind, "literal")
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("198100", "198101", "198112", "198113"), selected
    ),
    c("1981", "January 1981", "1981", "198113")
  )
})

test_that("numeric-mode CNVs normalize padded alphanumeric ranges", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(
    path,
    c("1 4", tabwin_cnv_line(1, "Pertussis", "A37 -A379"))
  )

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(
    unname(conversion$map[c("A370", "A375", "A379")]),
    rep("Pertussis", 3)
  )
  expect_identical(conversion$truncated_code_tokens, 0L)
  expect_identical(
    microdatasus:::.tabwin_normalize_code("A37 ", 4L), "A370"
  )
})

test_that("CNV parser repairs the official CID lower-bound sentinel", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  sentinel <- intToUtf8(31L)
  token <- paste0("A00", sentinel, "-A009")
  write_tabwin_text(
    path,
    c("1 4", tabwin_cnv_line(1, "Cholera", token))
  )

  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  expect_identical(conversion$ranges$token, "A000-A009")
  expect_identical(conversion$ranges$kind, "literal")
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("A000", "A005", "A009", "A010"), selected
    ),
    c("Cholera", "Cholera", "Cholera", "A010")
  )
  expect_identical(conversion$truncated_code_tokens, 0L)
})

test_that("CNV parser separates adjacent fixed-width ranges", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "2 6",
    tabwin_cnv_line(
      1, "AM interior", "130001-130259-130261-139000"
    ),
    tabwin_cnv_line(2, "Manaus", "130260")
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(conversion$recovered_concatenated_ranges, 1L)
  expect_identical(
    unname(conversion$map[c("130001", "130259", "130260", "139000")]),
    c("AM interior", "AM interior", "Manaus", "AM interior")
  )
})

test_that("CNV parser discards physical padding beyond code width", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "2 1",
    tabwin_cnv_line(1, "Com óbito", "1"),
    paste0(tabwin_cnv_line(2, "Sem óbito", "0"), "   ")
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_setequal(names(conversion$map), c("0", "1"))
  expect_identical(unname(conversion$map[["0"]]), "Sem óbito")
  expect_identical(
    microdatasus:::.tabwin_normalize_code("0   ", 1L),
    "0"
  )
  expect_identical(conversion$truncated_code_tokens, 0L)
})

test_that("CNV parser recovers narrow official layout inconformities", {
  blank <- tempfile(fileext = ".CNV")
  compact <- tempfile(fileext = ".CNV")
  embedded <- tempfile(fileext = ".CNV")
  on.exit(unlink(c(blank, compact, embedded)), add = TRUE)

  write_tabwin_text(blank, c(
    "1 3 L",
    tabwin_cnv_line(1, "Blank literal", "   ,")
  ))
  compact_rows <- paste0(
    sprintf("%7d", 1:2), "  ",
    sprintf("%-50s", c("Novo", "Antigo")), c("1", "0")
  )
  write_tabwin_text(compact, c("2 1", compact_rows))
  embedded_header <- paste0(
    sprintf("%10s", "1 2"), strrep(" ", 50), "01"
  )
  write_tabwin_text(embedded, c(
    embedded_header,
    tabwin_cnv_line(1, "First category", "02")
  ))

  blank_conversion <- microdatasus:::.tabwin_parse_cnv(blank)
  compact_conversion <- microdatasus:::.tabwin_parse_cnv(compact)
  embedded_conversion <- microdatasus:::.tabwin_parse_cnv(embedded)
  blank_selected <- list(
    definition = data.frame(position = 1L),
    conversion = blank_conversion
  )

  expect_identical(
    unname(blank_conversion$map[[strrep(" ", 3)]]),
    "Blank literal"
  )
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values("", blank_selected),
    "Blank literal"
  )
  expect_identical(unname(compact_conversion$map[c("1", "0")]),
                   c("Novo", "Antigo"))
  expect_identical(compact_conversion$compact_code_rows, 2L)
  expect_identical(unname(embedded_conversion$map[c("01", "02")]),
                   rep("First category", 2))
  expect_true(embedded_conversion$embedded_header)
  expect_identical(embedded_conversion$recovered_leading_sequence, 1L)
})

test_that("CNV parser recovers codes displaced by overflowing descriptions", {
  overflow <- tempfile(fileext = ".CNV")
  trailing_junk <- tempfile(fileext = ".CNV")
  overlong_code <- tempfile(fileext = ".CNV")
  on.exit(unlink(c(overflow, trailing_junk, overlong_code)), add = TRUE)

  long_label <- paste(
    rep("Description beyond the declared field", 2), collapse = " "
  )
  overflow_row <- paste0(
    sprintf("%7d", 1L), "  ", long_label, " ", "000061"
  )
  write_tabwin_text(overflow, c("1 6", overflow_row))
  write_tabwin_text(trailing_junk, c(
    "1 6",
    paste0(tabwin_cnv_line(1, "Valid", "120039"), strrep(" ", 33), "m")
  ))
  write_tabwin_text(overlong_code, c(
    "1 13",
    tabwin_cnv_line(1, "CNPJ", "12345678901234")
  ))

  overflow_conversion <- microdatasus:::.tabwin_parse_cnv(overflow)
  junk_conversion <- microdatasus:::.tabwin_parse_cnv(trailing_junk)
  overlong_conversion <- microdatasus:::.tabwin_parse_cnv(overlong_code)

  expect_identical(unname(overflow_conversion$map[["000061"]]), long_label)
  expect_identical(overflow_conversion$compact_code_rows, 0L)
  expect_identical(overflow_conversion$overflow_code_rows, 1L)
  expect_false("m" %in% names(junk_conversion$map))
  expect_identical(unname(junk_conversion$map[["120039"]]), "Valid")
  expect_identical(junk_conversion$overflow_code_rows, 0L)
  expect_identical(junk_conversion$truncated_code_tokens, 0L)
  expect_identical(junk_conversion$repaired_code_tokens, 1L)
  expect_identical(
    unname(overlong_conversion$map[["1234567890123"]]), "CNPJ"
  )
  expect_identical(overlong_conversion$truncated_code_tokens, 1L)
  expect_identical(overlong_conversion$overflow_code_rows, 0L)
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = overlong_conversion
  )
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("12345678901234", "1234567890123"), selected
    ),
    rep("CNPJ", 2)
  )
})

test_that("CNV overflow recovery does not reinterpret prose after a code", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "1 4",
    tabwin_cnv_line(
      1, "Long category", "O938,valid values from O930 to O935"
    )
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(conversion$overflow_code_rows, 0L)
  expect_identical(unname(conversion$map[["O938"]]), "Long category")
  expect_false("O935" %in% names(conversion$map))
  expect_identical(conversion$discarded_code_tokens, 1L)
})

test_that("later duplicate CNV codes take official source precedence", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "2 1 L",
    tabwin_cnv_line(1, "First", "A"),
    tabwin_cnv_line(2, "Second", "A")
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  expect_identical(conversion$normalized_collisions, 1L)
  expect_identical(unname(conversion$map[["A"]]), "Second")
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values("A", selected),
    "Second"
  )
})

test_that("CNV parser expands purely alphabetic ranges", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(
    path,
    c("1 1 L", tabwin_cnv_line(1, "Alphabet", "A-Z"))
  )

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(unname(conversion$map[c("A", "X", "Z")]),
                   rep("Alphabet", 3))
})

test_that("F mode categorizes numeric values by upper limits", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "3 5 F",
    tabwin_cnv_line(1, "Zero", "00000"),
    tabwin_cnv_line(2, "One to seven", "00007"),
    tabwin_cnv_line(3, "Eight or more", "09999")
  ))
  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  expect_identical(conversion$mode, "F")
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("0", "1", "7", "8", "9999", "10000"), selected
    ),
    c("Zero", "One to seven", "One to seven", "Eight or more",
      "Eight or more", "10000")
  )
})

test_that("CNV parser preserves subtotals and reports count mismatches", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  subtotal_row <- paste0(
    sprintf("%3s", "1"), sprintf("%4d", 2L), "  ",
    sprintf("%-50s", "Child"), " ", "2"
  )
  write_tabwin_text(path, c(
    "99 1",
    tabwin_cnv_line(1, "Parent", "1"),
    subtotal_row
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_true(conversion$category_count_mismatch)
  expect_identical(conversion$observed_category_count, 2L)
  expect_identical(
    conversion$categories[, c("sequence", "subtotal", "label")],
    data.frame(
      sequence = c("1", "2"),
      subtotal = c("", "1"),
      label = c("Parent", "Child"),
      stringsAsFactors = FALSE
    )
  )
})

test_that("subtotal and sequence jointly identify a CNV category", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  row <- function(subtotal, sequence, label, code) {
    paste0(
      sprintf("%3s", subtotal), sprintf("%4d", sequence), "  ",
      sprintf("%-50s", label), " ", code
    )
  }
  write_tabwin_text(path, c(
    "2 1 L",
    row("23", 24, "First subtotal", "A"),
    row("22", 24, "Second subtotal", "B")
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(conversion$observed_category_count, 2L)
  expect_false(conversion$category_count_mismatch)
  expect_identical(unname(conversion$map[c("A", "B")]),
                   c("First subtotal", "Second subtotal"))
  expect_false(any(conversion$categories$label_conflict))
})

test_that("CNV parser detects UTF-8 and recovers official tab alignment", {
  utf8 <- tempfile(fileext = ".CNV")
  legacy <- tempfile(fileext = ".CNV")
  tabbed <- tempfile(fileext = ".CNV")
  on.exit(unlink(c(utf8, legacy, tabbed)), add = TRUE)
  writeLines(
    c("1 1", tabwin_cnv_line(1, "Águas", "1")),
    utf8,
    useBytes = TRUE
  )
  write_tabwin_text(
    legacy,
    c("1 1", tabwin_cnv_line(1, "Atenção", "1"))
  )
  write_tabwin_text(tabbed, c(
    "1 4 L",
    "      1  Aparelho de Hemodialise - Hospitalar\t            1002"
  ))

  utf8_conversion <- microdatasus:::.tabwin_parse_cnv(utf8)
  legacy_conversion <- microdatasus:::.tabwin_parse_cnv(legacy)
  tabbed_conversion <- microdatasus:::.tabwin_parse_cnv(tabbed)

  expect_identical(unname(utf8_conversion$map[["1"]]), "Águas")
  expect_identical(utf8_conversion$source_encoding, "UTF-8")
  expect_identical(unname(legacy_conversion$map[["1"]]), "Atenção")
  expect_identical(legacy_conversion$source_encoding, "windows-1252")
  expect_identical(
    unname(tabbed_conversion$map[["1002"]]),
    "Aparelho de Hemodialise - Hospitalar"
  )
  expect_identical(tabbed_conversion$tabs_recovered, 1L)
})

test_that("DBF relation labels honor mixed legacy byte encodings", {
  directory <- tempfile("mixed-dbf-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "CADGERBA.dbf")
  cp1252_value <- rawToChar(as.raw(c(
    as.integer(charToRaw("ATEN")), 0xc7, 0xc3,
    as.integer(charToRaw("O"))
  )))
  cp850_value <- vapply(
    c(
      "CL\u00cdNICA DE OLHOS", "CL\u00cdNICA SANTO ANT\u00d4NIO",
      "Ind\u00edgena"
    ),
    function(item) {
      rawToChar(charToRaw(iconv(item, from = "UTF-8", to = "CP850")))
    },
    character(1)
  )
  value <- c(cp1252_value, cp850_value)
  Encoding(value) <- "unknown"
  foreign::write.dbf(
    data.frame(CODE = c("1", "2", "3", "4"), FANTASIA = value),
    path
  )
  table <- foreign::read.dbf(path, as.is = TRUE)
  metadata <- list(language_driver = 88L, encoding = "CP1252")
  cp850_rows <- microdatasus:::.tabwin_dbf_cp850_rows(
    table, metadata, path
  )
  decoded <- microdatasus:::.tabwin_decode_dbf_values(
    table$FANTASIA, metadata, "test label", path, cp850_rows
  )

  expect_identical(metadata$language_driver, 88L)
  expect_identical(metadata$encoding, "CP1252")
  expect_identical(cp850_rows, c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(
    decoded,
    c(
      "ATENÇÃO", "CLÍNICA DE OLHOS", "CLÍNICA SANTO ANTÔNIO",
      "Indígena"
    )
  )
})

test_that("DBF rows share only diagnostic CP850 evidence", {
  label <- c("ASCII", rawToChar(charToRaw(iconv(
    "CL\u00cdNICA", from = "UTF-8", to = "CP850"
  ))))
  evidence <- c("ASCII", rawToChar(charToRaw(iconv(
    "M\u00c9DICO", from = "UTF-8", to = "CP850"
  ))))
  table <- data.frame(LABEL = label, EVIDENCE = evidence)
  table[] <- lapply(table, function(value) {
    Encoding(value) <- "unknown"
    value
  })
  metadata <- list(language_driver = 88L, encoding = "CP1252")
  cp850_rows <- microdatasus:::.tabwin_dbf_cp850_rows(
    table, metadata, "OTHER.dbf"
  )

  expect_identical(cp850_rows, c(FALSE, TRUE))
  expect_identical(
    microdatasus:::.tabwin_decode_dbf_values(
      table$LABEL, metadata, "test label", "OTHER.dbf", cp850_rows
    ),
    c("ASCII", "CLÍNICA")
  )
})

test_that("mixed CP850 recovery leaves ambiguous source bytes lossless", {
  cp850_value <- rawToChar(charToRaw(iconv(
    "M\u00c9DICO", from = "UTF-8", to = "CP850"
  )))
  ambiguous <- rawToChar(as.raw(c(65L, 129L, 66L)))
  value <- c("ASCII", cp850_value, ambiguous)
  Encoding(value) <- "unknown"

  expect_warning(
    decoded <- microdatasus:::.dbc_decode_text_auto(
      value, "CP1252", 88L, "test field", "test.dbf"
    ),
    "1 value in test field could not be decoded safely",
    class = "microdatasus_dbc_encoding_warning"
  )

  expect_identical(decoded[1:2], c("ASCII", "MÉDICO"))
  expect_identical(charToRaw(decoded[[3L]]), as.raw(c(65L, 129L, 66L)))
  expect_identical(Encoding(decoded[[3L]]), "bytes")
  expect_identical(
    attr(decoded, "dbc_encoding_used"),
    "mixed:CP850+CP1252+bytes"
  )
})

test_that("CP1252 punctuation and names are not mistaken for CP850", {
  expected <- c(
    "M\u00e9dico", "FR\u00d6LICH", "25\u00a0MG",
    "AV\u00d2", "A \u00b7 B", "\u00a1Hola!"
  )
  value <- vapply(expected, function(item) {
    rawToChar(charToRaw(iconv(item, from = "UTF-8", to = "CP1252")))
  }, character(1))
  Encoding(value) <- "unknown"

  expect_no_warning(decoded <- microdatasus:::.dbc_decode_text_auto(
    value, "CP1252", 88L, "test field", "test.dbf"
  ))

  expect_identical(attr(decoded, "dbc_encoding_used"), "CP1252")
  attr(decoded, "dbc_encoding_used") <- NULL
  expect_identical(unname(decoded), expected)
})

test_that("official INCENTIVOS labels use exact byte-scoped repairs", {
  make_value <- function(prefix, byte, suffix) {
    rawToChar(c(charToRaw(prefix), as.raw(byte), charToRaw(suffix)))
  }
  value <- c(
    make_value(
      "8231-CEO-I-REDE DE CUIDADOS ", 145L,
      " PESSOA COM DEFICIENCIA"
    ),
    make_value(
      "8232-CEO-II-REDE DE CUIDADOS ", 145L,
      " PESSOA COM DEFICIENCIA"
    ),
    make_value(
      "8233-CEO-III-REDE DE CUIDADOS ", 145L,
      " PESSOA COM DEFICIENCIA"
    ),
    make_value(
      "8248-UNIDADE MOVEL DE ATENDIMENTO PRE-HOSPITALAR MOTOL",
      143L, "NCIA SAMU"
    )
  )
  Encoding(value) <- "unknown"
  metadata <- list(language_driver = 0L, encoding = "CP1252")

  expect_no_warning(decoded <- microdatasus:::.tabwin_decode_dbf_values(
    value, metadata, "test label", "INCENTIVOS.DBF"
  ))

  expect_identical(decoded, c(
    "8231-CEO-I-REDE DE CUIDADOS À PESSOA COM DEFICIENCIA",
    "8232-CEO-II-REDE DE CUIDADOS À PESSOA COM DEFICIENCIA",
    "8233-CEO-III-REDE DE CUIDADOS À PESSOA COM DEFICIENCIA",
    paste0(
      "8248-UNIDADE MOVEL DE ATENDIMENTO PRE-HOSPITALAR ",
      "MOTOLÂNCIA SAMU"
    )
  ))
  expect_identical(
    charToRaw(microdatasus:::.tabwin_repair_official_dbf_values(
      value, "OTHER.DBF"
    )[[1L]]),
    charToRaw(value[[1L]])
  )
})

test_that("CNV header tolerates colon comments but rejects trailing garbage", {
  valid <- tempfile(fileext = ".CNV")
  invalid <- tempfile(fileext = ".CNV")
  on.exit(unlink(c(valid, invalid)), add = TRUE)
  write_tabwin_text(valid, c(
    ": legacy pseudo-comment",
    "1 1",
    tabwin_cnv_line(1, "Valid", "1")
  ))
  write_tabwin_text(invalid, c(
    "1 1 trailing garbage",
    tabwin_cnv_line(1, "Invalid", "1")
  ))

  expect_identical(
    unname(microdatasus:::.tabwin_parse_cnv(valid)$map[["1"]]),
    "Valid"
  )
  expect_error(
    microdatasus:::.tabwin_parse_cnv(invalid),
    class = "microdatasus_dictionary_invalid_error"
  )
})

test_that("factor levels follow CNV category sequence", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "3 1 L",
    tabwin_cnv_line(3, "Ignorado", "I"),
    tabwin_cnv_line(1, "Masculino", "M"),
    tabwin_cnv_line(2, "Feminino", "F")
  ))
  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  result <- microdatasus:::.tabwin_apply_conversion(
    c("I", "F", "M"), selected
  )

  expect_identical(levels(result), c("Masculino", "Feminino", "Ignorado"))
})
test_that("ZIP entry matching observes path component boundaries", {
  expect_identical(
    microdatasus:::.tabwin_find_entry(c("REGUF.CNV", "UF.CNV"), "UF.CNV"),
    "UF.CNV"
  )
  expect_identical(
    microdatasus:::.tabwin_find_entry(c("MESANO.CNV", "ANO.CNV"), "ANO.CNV"),
    "ANO.CNV"
  )
  expect_identical(
    microdatasus:::.tabwin_find_entry(
      c("root/CNV/UF.CNV", "root/CNV/REGUF.CNV"), "CNV/UF.CNV"
    ),
    "root/CNV/UF.CNV"
  )
  expect_identical(
    microdatasus:::.tabwin_find_entry(
      "root/tabdo/Obito.def", "/tabdo/Obito.def"
    ),
    "root/tabdo/Obito.def"
  )
})

test_that("ZIP entry matching recovers only deterministic official path drift", {
  expect_identical(
    microdatasus:::.tabwin_find_entry(
      "root/TP_EQUIPAM.dbf", "root/TAB_DBF/TP_EQUIPAM.dbf"
    ),
    "root/TP_EQUIPAM.dbf"
  )
  expect_identical(
    microdatasus:::.tabwin_find_entry(
      "root/CNV/br_frontfaixa.cnv", "root/CNV/ br_frontfaixa.cnv"
    ),
    "root/CNV/br_frontfaixa.cnv"
  )
  expect_identical(
    microdatasus:::.tabwin_find_entry(
      "root/CNV/TP_APAC.CNV", "root/CNV/TPAPAC.CNV"
    ),
    "root/CNV/TP_APAC.CNV"
  )
  expect_identical(
    microdatasus:::.tabwin_find_entry(
      "root/CNV/NATJURC.CNV", "root/CNV/ATJURC.CNV"
    ),
    "root/CNV/NATJURC.CNV"
  )
  expect_identical(
    microdatasus:::.tabwin_find_entry(
      "root/CNV/COBRDET.CNV", "root/CNV/COBRDETS.CNV"
    ),
    "root/CNV/COBRDET.CNV"
  )
  expect_identical(
    microdatasus:::.tabwin_find_entry(
      "root/CNV/CIDX20B.CNV", "root/CNV/IDX20B.CNV"
    ),
    "root/CNV/CIDX20B.CNV"
  )
  expect_error(
    microdatasus:::.tabwin_find_entry(
      "root/CNV/Fxidad9NET.cnv", "root/CNV/FXIDAD5NET.CNV"
    ),
    class = "microdatasus_dictionary_missing_error"
  )
  expect_error(
    microdatasus:::.tabwin_find_entry(
      c("a/TP_APAC.CNV", "b/TP_APAC.CNV"), "root/TPAPAC.CNV"
    ),
    class = "microdatasus_dictionary_ambiguous_error"
  )
})

test_that("dictionary failures expose stable condition classes", {
  expect_error(
    microdatasus:::.tabwin_find_entry("path/one.cnv", "missing.cnv"),
    class = "microdatasus_dictionary_missing_error"
  )
  expect_error(
    microdatasus:::.tabwin_find_entry(
      c("a/same.cnv", "b/same.cnv"), "same.cnv"
    ),
    class = "microdatasus_dictionary_ambiguous_error"
  )
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, "not a header")
  expect_error(
    microdatasus:::.tabwin_parse_cnv(path),
    class = "microdatasus_dictionary_invalid_error"
  )
})

test_that("DBF fields use only deterministic dBase physical names", {
  expect_identical(
    microdatasus:::.tabwin_match_dbf_field("NM_DISTRITO", "NM_DISTRIT"),
    1L
  )
  expect_identical(
    microdatasus:::.tabwin_match_dbf_field("NM_DISTRITO 1", "NM_DISTRIT"),
    1L
  )
  expect_identical(
    microdatasus:::.tabwin_match_dbf_field("NM_BAIRRO 1", "NM_BAIRRO"),
    1L
  )
  expect_true(is.na(
    microdatasus:::.tabwin_match_dbf_field("NM_BA_OCOR", "NM_BAIRRO")
  ))

  directory <- tempfile("dbf-physical-name-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "district.dbf")
  foreign::write.dbf(
    data.frame(
      ID_DISTRIT = c("1", "2"), NM_DISTRIT = c("One", "Two"),
      ACTIVE = c("Y", "Y"), stringsAsFactors = FALSE
    ),
    path
  )
  dictionary <- list(
    extracted_all = TRUE, cache_dir = directory, persistent = FALSE,
    conversions = new.env(parent = emptyenv())
  )
  definition <- data.frame(
    file = "district.dbf", extension = "DBF", field = "ID_DISTRITO",
    argument = "NM_DISTRITO 1", stringsAsFactors = FALSE
  )

  result <- microdatasus:::.datasus_dictionary_conversion(
    dictionary, definition
  )

  expect_identical(result$status, "ok")
  expect_false(result$conversion$fallback_label)
  expect_identical(result$conversion$label_field, "NM_DISTRIT")
  expect_identical(unname(result$conversion$map), c("One", "Two"))
})

test_that("official REGIONET notification keys use the equivalent regional ID", {
  directory <- tempfile("dbf-regionet-key-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  table <- data.frame(
    SG_UF = c("SP", "SP"),
    ID_REGIONA = c("1340", "1341"),
    NM_REGIONA = c("Bauru", "Botucatu"),
    ID_RG_RESI = c("1340", "1341"),
    stringsAsFactors = FALSE
  )
  path <- file.path(directory, "REGIONET.DBF")
  foreign::write.dbf(table, path)
  dictionary <- list(
    extracted_all = TRUE, cache_dir = directory, persistent = FALSE,
    conversions = new.env(parent = emptyenv())
  )
  definition <- data.frame(
    file = "REGIONET.DBF", extension = "DBF", field = "ID_RG_OCOR",
    argument = "NM_REGIONA", stringsAsFactors = FALSE
  )

  result <- microdatasus:::.datasus_dictionary_conversion(
    dictionary, definition
  )

  expect_identical(result$status, "ok")
  expect_true(result$conversion$recovered_key)
  expect_false(result$conversion$fallback_key)
  expect_identical(result$conversion$requested_key_field, "ID_RG_OCOR")
  expect_identical(result$conversion$key_field, "ID_REGIONA")
  expect_identical(
    result$conversion$map,
    c(`1340` = "Bauru", `1341` = "Botucatu")
  )

  expect_true(is.na(microdatasus:::.tabwin_recover_official_dbf_key(
    "ID_RG_OCOR", table, file.path(directory, "OTHER.DBF")
  )))
  table$ID_RG_RESI[[2L]] <- "9999"
  expect_true(is.na(microdatasus:::.tabwin_recover_official_dbf_key(
    "ID_RG_OCOR", table, path
  )))
  expect_true(is.na(microdatasus:::.tabwin_recover_official_dbf_key(
    NA_character_, table, path
  )))
})

test_that("two-column DBFs use an explicit audited label fallback", {
  directory <- tempfile("dbf-fallback-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "labels.dbf")
  foreign::write.dbf(
    data.frame(CODE = c("1", "2"), CURRENT_LABEL = c("One", "Two")),
    path
  )
  dictionary <- list(
    extracted_all = TRUE, cache_dir = directory, persistent = FALSE,
    conversions = new.env(parent = emptyenv())
  )
  definition <- data.frame(
    file = "labels.dbf", extension = "DBF", field = "CODE",
    argument = "OLD_LABEL", stringsAsFactors = FALSE
  )

  result <- microdatasus:::.datasus_dictionary_conversion(
    dictionary, definition
  )

  expect_identical(result$status, "fallback")
  expect_true(result$conversion$fallback_label)
  expect_identical(unname(result$conversion$map), c("One", "Two"))
  expect_match(result$message, "CURRENT_LA")
})

test_that("dictionary inspection classifies structured relation failures", {
  dictionary <- list()
  definition <- data.frame(field = "X")
  cases <- list(
    missing = "microdatasus_dictionary_missing_error",
    invalid = "microdatasus_dictionary_invalid_error",
    ambiguous = "microdatasus_dictionary_ambiguous_error",
    error = "simpleError"
  )
  for (status in names(cases)) {
    class <- cases[[status]]
    local_mocked_bindings(
      .tabwin_read_conversion = function(...) {
        condition <- structure(
          list(message = paste(status, "relation"), call = NULL),
          class = c(class, "error", "condition")
        )
        stop(condition)
      },
      .package = "microdatasus"
    )
    result <- microdatasus:::.datasus_dictionary_conversion(
      dictionary, definition
    )
    expected <- if (status == "ambiguous") "invalid" else status
    expect_identical(result$status, expected)
    expect_match(result$message, status)
    expect_null(result$conversion)
  }
})

test_that("dictionary prefetch returns early when there is no work", {
  expect_null(microdatasus:::.datasus_prefetch_dictionary_relations(
    list(extracted_all = TRUE), data.frame()
  ))
  expect_null(microdatasus:::.datasus_prefetch_dictionary_relations(
    list(extracted_all = FALSE), data.frame()
  ))
})


test_that("CNV parser recovers one-position literal width conflicts", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  write_tabwin_text(path, c(
    "4 1 L",
    tabwin_cnv_line(1, "Unknown", "00-99"),
    tabwin_cnv_line(2, "Home", "1,01"),
    tabwin_cnv_line(3, "Transport", "10"),
    tabwin_cnv_line(4, "Abroad", "11")
  ))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  expect_identical(conversion$declared_code_width, 1L)
  expect_identical(conversion$code_width, 2L)
  expect_true(conversion$recovered_code_width)
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("1", "01", "10", "11", "99"), selected
    ),
    c("Home", "Home", "Transport", "Abroad", "Unknown")
  )
})

test_that("official source aliases are exact, auditable, and filename-scoped", {
  directory <- tempfile("source-aliases-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  lines <- c(
    "5 2 L",
    tabwin_cnv_line(1, "Not evaluable", "GX"),
    tabwin_cnv_line(2, "Grade one", "G1"),
    tabwin_cnv_line(3, "Grade two", "G2"),
    tabwin_cnv_line(4, "Grade three", "G3"),
    tabwin_cnv_line(5, "Grade four", "G4")
  )
  official <- file.path(directory, "GRAU_HIS.CNV")
  unrelated <- file.path(directory, "OTHER.CNV")
  write_tabwin_text(official, lines)
  write_tabwin_text(unrelated, lines)

  conversion <- microdatasus:::.tabwin_recover_official_source_aliases(
    microdatasus:::.tabwin_parse_cnv(official), official
  )
  other_conversion <- microdatasus:::.tabwin_recover_official_source_aliases(
    microdatasus:::.tabwin_parse_cnv(unrelated), unrelated
  )
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  expect_identical(conversion$recovered_source_aliases, 8L)
  expect_identical(
    unname(conversion$source_aliases),
    rep(paste0("G", 1:4), each = 2L)
  )
  expect_identical(other_conversion$recovered_source_aliases, 0L)
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("1", "01", "2", "02", "3", "03", "4", "04", "0", "00", "99"),
      selected
    ),
    c(
      "Grade one", "Grade one", "Grade two", "Grade two",
      "Grade three", "Grade three", "Grade four", "Grade four",
      "0", "00", "99"
    )
  )
})

test_that("official yes-no source aliases preserve both code dialects", {
  directory <- tempfile("yes-no-aliases-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "SIMNAO2.CNV")
  write_tabwin_text(path, c(
    "2 1",
    tabwin_cnv_line(1, "Yes", "1"),
    tabwin_cnv_line(2, "No", "0")
  ))

  conversion <- microdatasus:::.tabwin_recover_official_source_aliases(
    microdatasus:::.tabwin_parse_cnv(path), path
  )
  selected <- list(
    definition = data.frame(position = 1L),
    conversion = conversion
  )

  expect_identical(conversion$recovered_source_aliases, 2L)
  expect_identical(conversion$source_aliases, c("S" = "1", "N" = "0"))
  expect_identical(
    microdatasus:::.tabwin_apply_conversion_values(
      c("S", "N", "1", "0", "X"), selected
    ),
    c("Yes", "No", "Yes", "No", "X")
  )
})

test_that("CNV parser repairs evidenced malformed interval spellings", {
  equals <- tempfile(fileext = ".CNV")
  padded <- tempfile(fileext = ".CNV")
  transposed <- tempfile(fileext = ".CNV")
  on.exit(unlink(c(equals, padded, transposed)), add = TRUE)

  write_tabwin_text(
    equals,
    c("1 4", tabwin_cnv_line(1, "Processing", "1455=1456"))
  )
  write_tabwin_text(
    padded,
    c("1 2 L", tabwin_cnv_line(1, "Seven through 48", "7 -48"))
  )
  write_tabwin_text(
    transposed,
    c("1 4 L", tabwin_cnv_line(1, "Skin", "L985-L959"))
  )

  equals_conversion <- microdatasus:::.tabwin_parse_cnv(equals)
  padded_conversion <- microdatasus:::.tabwin_parse_cnv(padded)
  transposed_conversion <- microdatasus:::.tabwin_parse_cnv(transposed)

  expect_identical(
    unname(equals_conversion$map[c("1455", "1456")]),
    rep("Processing", 2)
  )
  expect_identical(
    unname(padded_conversion$map[c("7 ", "8 ", "10", "48")]),
    rep("Seven through 48", 4)
  )
  expect_identical(
    unname(transposed_conversion$map[c("L985", "L986", "L989")]),
    rep("Skin", 3)
  )
  expect_identical(equals_conversion$repaired_code_tokens, 1L)
  expect_identical(padded_conversion$repaired_code_tokens, 1L)
  expect_identical(transposed_conversion$repaired_code_tokens, 1L)
})

test_that("CNV parser repairs the official CID-9 epilepsy upper bound", {
  directory <- tempfile("cid9-range-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  cid9 <- file.path(directory, "CID9BR2.CNV")
  unrelated <- file.path(directory, "OTHER.CNV")
  write_tabwin_text(
    cid9,
    c("1 4", tabwin_cnv_line(1, "Epilepsy", "3450-2459"))
  )
  write_tabwin_text(
    unrelated,
    c("1 4", tabwin_cnv_line(1, "Invalid", "3450-2459"))
  )

  conversion <- microdatasus:::.tabwin_parse_cnv(cid9)

  expect_identical(
    unname(conversion$map[sprintf("%04d", 3450:3459)]),
    rep("Epilepsy", 10)
  )
  expect_identical(conversion$repaired_code_tokens, 1L)
  expect_error(
    microdatasus:::.tabwin_parse_cnv(unrelated),
    "contains no code labels"
  )
})

test_that("CNV parser discards prose without losing adjacent valid codes", {
  prose_path <- tempfile(fileext = ".CNV")
  prefix_path <- tempfile(fileext = ".CNV")
  dot_path <- tempfile(fileext = ".CNV")
  on.exit(unlink(c(prose_path, prefix_path, dot_path)), add = TRUE)

  write_tabwin_text(prose_path, c(
    "1 4 L",
    tabwin_cnv_line(1, "Lymphoma", "C884,linfoma MALT ou TLAB]")
  ))
  write_tabwin_text(prefix_path, c(
    "1 6 L",
    tabwin_cnv_line(
      1, "Family doctor",
      paste0("XXXXXX", strrep(" ", 40L), "vascular")
    )
  ))
  write_tabwin_text(dot_path, c(
    "1 1",
    tabwin_cnv_line(1, "Blank", ".0")
  ))

  prose <- microdatasus:::.tabwin_parse_cnv(prose_path)
  prefix <- microdatasus:::.tabwin_parse_cnv(prefix_path)
  dot <- microdatasus:::.tabwin_parse_cnv(dot_path)

  expect_identical(names(prose$map), "C884")
  expect_identical(prose$discarded_code_tokens, 1L)
  expect_identical(unname(prefix$map[["XXXXXX"]]), "Family doctor")
  expect_identical(prefix$repaired_code_tokens, 1L)
  expect_identical(unname(dot$map[["0"]]), "Blank")
  expect_identical(dot$repaired_code_tokens, 1L)
})

test_that("CNV parser recovers codes glued after a closing parenthesis", {
  path <- tempfile(fileext = ".CNV")
  on.exit(unlink(path), add = TRUE)
  label <- paste0(strrep("A", 52L), ")")
  row <- paste0(sprintf("%7d", 1L), "  ", label, "857")
  write_tabwin_text(path, c("1 3", row))

  conversion <- microdatasus:::.tabwin_parse_cnv(path)

  expect_identical(unname(conversion$map[["857"]]), label)
  expect_identical(conversion$overflow_code_rows, 1L)
})


test_that("CNV parser preserves official placeholders and internal literals", {
  literal_path <- tempfile(fileext = ".CNV")
  sentinel_path <- tempfile(fileext = ".CNV")
  on.exit(unlink(c(literal_path, sentinel_path)), add = TRUE)

  write_tabwin_text(literal_path, c(
    "2 3 L",
    tabwin_cnv_line(1, "Combined", "A O"),
    tabwin_cnv_line(2, "Subtotal placeholder", "---")
  ))
  sentinel <- intToUtf8(31L)
  write_tabwin_text(sentinel_path, c(
    "1 4 L",
    tabwin_cnv_line(1, "Category", paste0("C00", sentinel))
  ))

  literal <- microdatasus:::.tabwin_parse_cnv(literal_path)
  scalar <- microdatasus:::.tabwin_parse_cnv(sentinel_path)

  expect_identical(unname(literal$map[["A O"]]), "Combined")
  expect_identical(
    unname(literal$map[["---"]]), "Subtotal placeholder"
  )
  expect_identical(literal$placeholder_code_tokens, 1L)
  expect_identical(literal$discarded_code_tokens, 0L)
  expect_identical(unname(scalar$map[["C000"]]), "Category")
  expect_identical(scalar$repaired_code_tokens, 1L)
})
