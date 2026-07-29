# Human-readable metadata for the 35 non-SINAN file families supported by
# fetch_datasus(). Names follow the DataSUS transfer page; date qualifiers live
# in the download registry and are exposed separately as minimum_date.
.datasus_non_sinan_system_metadata <- function() {
  information_system <- c(
    "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT",
    "SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER",
    "SINASC",
    paste0("CNES-", c(
      "LT", "ST", "DC", "EQ", "SR", "HB", "PF",
      "EP", "RC", "IN", "EE", "EF", "GM"
    )),
    paste0("SIA-", c(
      "AB", "ABO", "ACF", "AD", "AN", "AM",
      "AQ", "AR", "ATD", "PA", "PS", "SAD"
    ))
  )
  name <- c(
    "Declara\u00e7\u00f5es de \u00f3bito",
    "Declara\u00e7\u00f5es de \u00f3bitos fetais",
    "Declara\u00e7\u00f5es de \u00f3bitos por causas externas",
    "Declara\u00e7\u00f5es de \u00f3bitos infantis",
    "Declara\u00e7\u00f5es de \u00f3bitos maternos",
    "AIH reduzida",
    "AIH rejeitadas",
    "Servi\u00e7os profissionais",
    "AIH rejeitadas com c\u00f3digo de erro",
    "Declara\u00e7\u00f5es de nascidos vivos",
    "Leitos",
    "Estabelecimentos",
    "Dados complementares",
    "Equipamentos",
    "Servi\u00e7o especializado",
    "Habilita\u00e7\u00e3o",
    "Profissional",
    "Equipes",
    "Regra contratual",
    "Incentivos",
    "Estabelecimento de ensino",
    "Estabelecimento filantr\u00f3pico",
    "Gest\u00e3o e metas",
    "APAC de acompanhamento a cirurgia bari\u00e1trica",
    "APAC de acompanhamento p\u00f3s-cirurgia bari\u00e1trica",
    "APAC de confec\u00e7\u00e3o de f\u00edstula arteriovenosa",
    "APAC de laudos diversos",
    "APAC de nefrologia",
    "APAC de medicamentos",
    "APAC de quimioterapia",
    "APAC de radioterapia",
    "APAC de tratamento dial\u00edtico",
    "Produ\u00e7\u00e3o ambulatorial",
    "Psicossocial",
    "Aten\u00e7\u00e3o domiciliar"
  )
  file_acronym <- c(
    "DO", "DOFET", "DOEXT", "DOINF", "DOMAT",
    "RD", "RJ", "SP", "ER",
    "DN",
    "LT", "ST", "DC", "EQ", "SR", "HB", "PF",
    "EP", "RC", "IN", "EE", "EF", "GM",
    "AB", "ABO", "ACF", "AD", "AN", "AM",
    "AQ", "AR", "ATD", "PA", "PS", "SAD"
  )
  data.frame(
    information_system = information_system,
    system = sub("-.*$", "", information_system),
    name = name,
    file_acronym = file_acronym,
    stringsAsFactors = FALSE
  )
}

# Combine portal names with the operational registry rather than maintaining
# separate copies of periodicity, geography, or availability metadata.
.datasus_information_system_metadata <- function() {
  non_sinan <- .datasus_non_sinan_system_metadata()
  sinan <- .sinan_system_specs()
  sinan <- data.frame(
    information_system = sinan$information_system,
    system = "SINAN",
    name = sinan$name,
    file_acronym = sinan$acronym,
    stringsAsFactors = FALSE
  )
  rbind(non_sinan, sinan)
}

#' Consult supported DataSUS information systems
#'
#' Lists every canonical value accepted by the `information_system` argument
#' of [fetch_datasus()]. Names and file acronyms follow the DataSUS transfer
#' portal. Operational metadata comes directly from the same registry used for
#' file discovery, and SINAN aliases come from the registry shared with
#' [process_sinan()] and [fetch_tabwin_dictionary()].
#'
#' @return A tibble with 93 rows and eight columns:
#' \describe{
#'   \item{`information_system`}{Preferred identifier accepted by the API.}
#'   \item{`system`}{Source system: SIM, SIH, SINASC, CNES, SIA, or SINAN.}
#'   \item{`name`}{Human-readable Portuguese name.}
#'   \item{`file_acronym`}{Acronym used in DataSUS DBC file names.}
#'   \item{`periodicity`}{Publication interval, `"year"` or `"month"`.}
#'   \item{`geography`}{File coverage, `"state"` or `"national"`.}
#'   \item{`minimum_date`}{Earliest date supported by [fetch_datasus()].}
#'   \item{`aliases`}{List-column of accepted alternative identifiers.}
#' }
#'
#' @references
#' [DataSUS file transfer portal](https://datasus.saude.gov.br/transferencia-de-arquivos/)
#'
#' @examples
#' systems <- datasus_information_systems()
#' systems[, c("information_system", "system", "name")]
#' systems[systems$system == "SINAN", ]
#'
#' @seealso [fetch_datasus()], [process_sinan()],
#'   [fetch_tabwin_dictionary()]
#' @export
datasus_information_systems <- function() {
  registry <- .datasus_registry()
  metadata <- .datasus_information_system_metadata()
  information_system <- names(registry)
  metadata <- metadata[
    match(information_system, metadata$information_system),
    ,
    drop = FALSE
  ]
  if (anyNA(metadata$information_system)) {
    cli::cli_abort(
      "Internal information-system metadata is incomplete.",
      class = "microdatasus_internal_error"
    )
  }

  aliases <- .sinan_alias_table()
  alias_list <- lapply(information_system, function(system) {
    aliases$alias[aliases$information_system == system]
  })
  tibble::tibble(
    information_system = information_system,
    system = metadata$system,
    name = metadata$name,
    file_acronym = metadata$file_acronym,
    periodicity = vapply(
      registry,
      `[[`,
      character(1),
      "granularity"
    ),
    geography = vapply(registry, `[[`, character(1), "geography"),
    minimum_date = as.Date(vapply(
      registry,
      function(spec) as.character(spec$minimum),
      character(1)
    )),
    aliases = alias_list
  )
}
