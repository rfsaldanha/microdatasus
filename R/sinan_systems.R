# The SINAN transfer portal identifies DBC families with four-letter file
# acronyms. Public identifiers use readable names, while the acronyms remain
# available here for file discovery and as backward-compatible aliases.
.sinan_system_specs <- function() {
  acronyms <- c(
    "ANIM", "ANTR", "AIDA", "AIDC", "BOTU", "COLE", "COQU", "DENG",
    "DIFT", "DCRJ", "CHAG", "EXAN", "ESQU", "ESPO", "CHIK", "FMAC",
    "FTIF", "HANS", "HANT", "HEPA", "HIVA", "HIVC", "HIVE", "HIVG",
    "INFL", "IEXO", "LEIV", "LTAN", "LEPT", "MALA", "MENI", "PFAN",
    "PEST", "RAIV", "ROTA", "SIFA", "SIFC", "SIFG", "SRC", "SDTA",
    "TETA", "TETN", "TOXC", "TOXG", "NTRA", "TRAC", "TUBE", "VARC",
    "VIOL", "ZIKA", "ACBI", "ACGR", "CANC", "DERM", "LERD", "PAIR",
    "PNEU", "MENT"
  )

  # Names follow the descriptions published by the DataSUS transfer page.
  # Obvious spelling errors in the page are corrected without changing their
  # meaning, so this column is suitable for display to package users.
  names <- c(
    "Acidente por animais pe\u00e7onhentos",
    "Atendimento antirr\u00e1bico",
    "AIDS em adultos",
    "AIDS em crian\u00e7as",
    "Botulismo",
    "C\u00f3lera",
    "Coqueluche",
    "Dengue",
    "Difteria",
    "Doen\u00e7a de Creutzfeldt-Jakob (DCJ)",
    "Doen\u00e7a de Chagas aguda",
    "Doen\u00e7as exantem\u00e1ticas",
    "Esquistossomose",
    "Esporotricose (epizootia)",
    "Febre de chikungunya",
    "Febre maculosa",
    "Febre tifoide",
    "Hansen\u00edase",
    "Hantavirose",
    "Hepatites virais",
    "HIV em adultos",
    "HIV em crian\u00e7as",
    "HIV em crian\u00e7as expostas",
    "HIV em gestante",
    "Influenza pand\u00eamica",
    "Intoxica\u00e7\u00e3o ex\u00f3gena",
    "Leishmaniose visceral",
    "Leishmaniose tegumentar americana",
    "Leptospirose",
    "Mal\u00e1ria",
    "Meningite",
    "Paralisia fl\u00e1cida aguda",
    "Peste",
    "Raiva",
    "Rotav\u00edrus",
    "S\u00edfilis adquirida",
    "S\u00edfilis cong\u00eanita",
    "S\u00edfilis em gestante",
    "S\u00edndrome da rub\u00e9ola cong\u00eanita",
    "Surto de doen\u00e7as transmitidas por alimentos",
    "T\u00e9tano acidental",
    "T\u00e9tano neonatal",
    "Toxoplasmose cong\u00eanita",
    "Toxoplasmose gestacional",
    "Notifica\u00e7\u00e3o de tracoma",
    "Inqu\u00e9rito de tracoma",
    "Tuberculose",
    "Varicela",
    "Viol\u00eancia dom\u00e9stica, sexual e/ou outras viol\u00eancias",
    "Zika v\u00edrus",
    "Acidente de trabalho com material biol\u00f3gico",
    "Acidente de trabalho",
    "C\u00e2ncer relacionado ao trabalho",
    "Dermatoses ocupacionais",
    "LER/Dort",
    "Perda auditiva por ru\u00eddo relacionada ao trabalho",
    "Pneumoconioses relacionadas ao trabalho",
    "Transtornos mentais relacionados ao trabalho"
  )

  # These are the preferred values for the public information_system argument.
  # Existing descriptive values remain canonical when they are already clear;
  # the remaining values spell out the portal description in ASCII.
  information_system <- c(
    "SINAN-ACIDENTE-POR-ANIMAIS-PECONHENTOS",
    "SINAN-ATENDIMENTO-ANTIRRABICO",
    "SINAN-AIDS-EM-ADULTOS",
    "SINAN-AIDS-EM-CRIANCAS",
    "SINAN-BOTULISMO",
    "SINAN-COLERA",
    "SINAN-COQUELUCHE",
    "SINAN-DENGUE",
    "SINAN-DIFTERIA",
    "SINAN-DOENCA-DE-CREUTZFELDT-JAKOB",
    "SINAN-DOENCA-DE-CHAGAS-AGUDA",
    "SINAN-DOENCAS-EXANTEMATICAS",
    "SINAN-ESQUISTOSSOMOSE",
    "SINAN-ESPOROTRICOSE-EPIZOOTIA",
    "SINAN-FEBRE-DE-CHIKUNGUNYA",
    "SINAN-FEBRE-MACULOSA",
    "SINAN-FEBRE-TIFOIDE",
    "SINAN-HANSENIASE",
    "SINAN-HANTAVIROSE",
    "SINAN-HEPATITES-VIRAIS",
    "SINAN-HIV-EM-ADULTOS",
    "SINAN-HIV-EM-CRIANCAS",
    "SINAN-HIV-EM-CRIANCAS-EXPOSTAS",
    "SINAN-HIV-EM-GESTANTE",
    "SINAN-INFLUENZA-PANDEMICA",
    "SINAN-INTOXICACAO-EXOGENA",
    "SINAN-LEISHMANIOSE-VISCERAL",
    "SINAN-LEISHMANIOSE-TEGUMENTAR",
    "SINAN-LEPTOSPIROSE",
    "SINAN-MALARIA",
    "SINAN-MENINGITE",
    "SINAN-PARALISIA-FLACIDA-AGUDA",
    "SINAN-PESTE",
    "SINAN-RAIVA",
    "SINAN-ROTAVIRUS",
    "SINAN-SIFILIS-ADQUIRIDA",
    "SINAN-SIFILIS-CONGENITA",
    "SINAN-SIFILIS-EM-GESTANTE",
    "SINAN-SINDROME-DA-RUBEOLA-CONGENITA",
    "SINAN-SURTO-DE-DOENCAS-TRANSMITIDAS-POR-ALIMENTOS",
    "SINAN-TETANO-ACIDENTAL",
    "SINAN-TETANO-NEONATAL",
    "SINAN-TOXOPLASMOSE-CONGENITA",
    "SINAN-TOXOPLASMOSE-GESTACIONAL",
    "SINAN-NOTIFICACAO-DE-TRACOMA",
    "SINAN-INQUERITO-DE-TRACOMA",
    "SINAN-TUBERCULOSE",
    "SINAN-VARICELA",
    "SINAN-VIOLENCIA-DOMESTICA-SEXUAL-E-OU-OUTRAS-VIOLENCIAS",
    "SINAN-ZIKA-VIRUS",
    "SINAN-ACIDENTE-DE-TRABALHO-COM-MATERIAL-BIOLOGICO",
    "SINAN-ACIDENTE-DE-TRABALHO",
    "SINAN-CANCER-RELACIONADO-AO-TRABALHO",
    "SINAN-DERMATOSES-OCUPACIONAIS",
    "SINAN-LER-DORT",
    "SINAN-PERDA-AUDITIVA-POR-RUIDO-RELACIONADA-AO-TRABALHO",
    "SINAN-PNEUMOCONIOSES-RELACIONADAS-AO-TRABALHO",
    "SINAN-TRANSTORNOS-MENTAIS-RELACIONADOS-AO-TRABALHO"
  )

  # Values accepted before readable canonical names were introduced. They
  # remain valid aliases so existing scripts continue to run unchanged.
  legacy_information_system <- c(
    "SINAN-ANIM", "SINAN-ANTR", "SINAN-AIDA", "SINAN-AIDC",
    "SINAN-BOTU", "SINAN-COLE", "SINAN-COQU", "SINAN-DENGUE",
    "SINAN-DIFT", "SINAN-DCRJ", "SINAN-CHAGAS", "SINAN-EXAN",
    "SINAN-ESQU", "SINAN-ESPO", "SINAN-CHIKUNGUNYA", "SINAN-FMAC",
    "SINAN-FTIF", "SINAN-HANS", "SINAN-HANT", "SINAN-HEPA",
    "SINAN-HIVA", "SINAN-HIVC", "SINAN-HIVE", "SINAN-HIVG",
    "SINAN-INFL", "SINAN-IEXO", "SINAN-LEISHMANIOSE-VISCERAL",
    "SINAN-LEISHMANIOSE-TEGUMENTAR", "SINAN-LEPTOSPIROSE",
    "SINAN-MALARIA", "SINAN-MENI", "SINAN-PFAN", "SINAN-PEST",
    "SINAN-RAIV", "SINAN-ROTA", "SINAN-SIFA", "SINAN-SIFC",
    "SINAN-SIFG", "SINAN-SRC", "SINAN-SDTA", "SINAN-TETA",
    "SINAN-TETN", "SINAN-TOXC", "SINAN-TOXG", "SINAN-NTRA",
    "SINAN-TRAC", "SINAN-TUBE", "SINAN-VARC", "SINAN-VIOL",
    "SINAN-ZIKA", "SINAN-ACBI", "SINAN-ACGR", "SINAN-CANC",
    "SINAN-DERM", "SINAN-LERD", "SINAN-PAIR", "SINAN-PNEU",
    "SINAN-MENT"
  )

  definitions <- c(
    "AnimaispNET.DEF", "AntirabNET.def", "AidsNET.def",
    "AidsCriNET.def", "BotuNET.def", "ColeraNET.def", "CoqueNET.def",
    "DengueNETON3.0.def", "DifteriNET.def", "NotIndiviNet.def",
    "ChagasNET2.def", "ExantNET.def", "EsquisNET.def", "EpizotNet.def",
    "ChikNON.def", "FMacNet.def", "FTifoideNET.def", "HansNET.def",
    "HantaNET.def", "HepavirNET.def", "NotIndiviNet.def",
    "NotIndiviNet.def", "NotIndiviNet.def", "HivGestNET.def",
    "InfluenzaNET.def", "IntoxNET.def", "LeishvisNET.def",
    "LeishtegNET.def", "LeptoNET.def", "MalariaNET.def",
    "MeningeNET.def", "PfapolioNET.def", "PesteNET.def", "RaivaNET.def",
    "ROTANet.def", "NotIndiviNet.def", "SifilisNET.def",
    "GestSifNET.def", "SrcNET.def", "DTANet.def", "TetacidNET.def",
    "TetneoNET.def", "NotIndiviNet.def", "NotIndiviNet.def",
    "NotTracoNet.def", "TracoNet.def", "TuberculNET5_0.def",
    "NotIndiviNet.def", "ViolenciaNet.def", "NotIndiviNet.def",
    "AcidBioNET.def", "AcidGraveNET.def", "DRTCancerNET.def",
    "DRTDermatoseNET.def", "DRTLerDortNET.def", "DRTPairNET.def",
    "DRTPneumoconioseNET.def", "DRTTransMentalNET.def"
  )

  online <- acronyms %in% c("DENG", "CHIK")
  data.frame(
    information_system = information_system,
    name = names,
    acronym = acronyms,
    legacy_information_system = legacy_information_system,
    prefix = paste0(acronyms, "BR"),
    definition = definitions,
    archive = ifelse(online, "SINAN-ONLINE", "SINAN-NET"),
    stringsAsFactors = FALSE,
    row.names = information_system
  )
}

# Return every non-canonical name accepted by the public API. The acronym
# aliases are included systematically, even for the few families whose old
# identifier was already descriptive.
.sinan_alias_table <- function() {
  specs <- .sinan_system_specs()
  aliases <- data.frame(
    alias = c(
      specs$legacy_information_system,
      paste0("SINAN-", specs$acronym)
    ),
    information_system = rep(specs$information_system, 2L),
    stringsAsFactors = FALSE
  )
  aliases <- aliases[
    aliases$alias != aliases$information_system,
    ,
    drop = FALSE
  ]
  aliases <- aliases[!duplicated(aliases$alias), , drop = FALSE]
  rownames(aliases) <- NULL
  aliases
}

# Resolve aliases before looking up download or dictionary specifications.
# Unknown values are returned unchanged so the calling validator can provide
# the same public error used for all unsupported systems.
.sinan_resolve_information_system <- function(information_system) {
  if (
    !is.character(information_system) ||
      length(information_system) != 1L ||
      is.na(information_system)
  ) {
    return(information_system)
  }
  aliases <- .sinan_alias_table()
  match <- match(information_system, aliases$alias)
  if (!is.na(match)) {
    return(aliases$information_system[[match]])
  }
  information_system
}

.sinan_information_systems <- function() {
  .sinan_system_specs()$information_system
}

#' Consult supported SINAN information systems
#'
#' Returns the readable identifiers accepted by `information_system` in
#' [fetch_datasus()], [process_sinan()], and [fetch_tabwin_dictionary()]. The
#' table is generated from the same internal registry used for downloads and
#' processing, so identifiers, DBC acronyms, and aliases remain synchronized.
#'
#' @return A tibble with 58 rows and four columns:
#' \describe{
#'   \item{`information_system`}{Preferred readable identifier.}
#'   \item{`name`}{Full Portuguese name published for the file family.}
#'   \item{`file_acronym`}{DataSUS acronym used in DBC file names.}
#'   \item{`aliases`}{List-column containing accepted legacy identifiers.}
#' }
#'
#' @references
#' [DataSUS file transfer portal](https://datasus.saude.gov.br/transferencia-de-arquivos/)
#'
#' @examples
#' systems <- sinan_information_systems()
#' systems[, c("information_system", "name", "file_acronym")]
#' systems$aliases[[match("SINAN-TUBERCULOSE", systems$information_system)]]
#'
#' @seealso [fetch_datasus()], [process_sinan()],
#'   [fetch_tabwin_dictionary()]
#' @export
sinan_information_systems <- function() {
  specs <- .sinan_system_specs()
  aliases <- .sinan_alias_table()
  alias_list <- lapply(specs$information_system, function(system) {
    aliases$alias[aliases$information_system == system]
  })
  tibble::tibble(
    information_system = specs$information_system,
    name = specs$name,
    file_acronym = specs$acronym,
    aliases = alias_list
  )
}
