.sinan_deprecate <- function(old, information_system) {
  cli::cli_warn(
    c(
      "{.fn {old}} is deprecated.",
      "i" = "Use {.code process_sinan(data, information_system = \"{information_system}\")}."
    ),
    class = "microdatasus_sinan_deprecated"
  )
}

#' Prepare SINAN Chagas disease microdata (deprecated)
#'
#' `process_sinan_chagas()` is retained for backward compatibility. New code
#' should call [process_sinan()] with
#' `information_system = "SINAN-DOENCA-DE-CHAGAS-AGUDA"`.
#'
#' @inheritParams process_sinan
#' @examplesIf interactive() && curl::has_internet()
#' process_sinan_chagas(sinan_chagas_sample)
#' @return The tibble returned by [process_sinan()].
#' @seealso [process_sinan()]
#' @export
process_sinan_chagas <- function(data, municipality_data = TRUE) {
  .sinan_deprecate(
    "process_sinan_chagas",
    "SINAN-DOENCA-DE-CHAGAS-AGUDA"
  )
  process_sinan(data, "SINAN-DOENCA-DE-CHAGAS-AGUDA", municipality_data)
}

#' Prepare SINAN chikungunya microdata (deprecated)
#'
#' `process_sinan_chikungunya()` is retained for backward compatibility. New
#' code should call [process_sinan()] with
#' `information_system = "SINAN-FEBRE-DE-CHIKUNGUNYA"`.
#'
#' @inheritParams process_sinan
#' @examplesIf interactive() && curl::has_internet()
#' process_sinan_chikungunya(sinan_chikungunya_sample)
#' @return The tibble returned by [process_sinan()].
#' @seealso [process_sinan()]
#' @export
process_sinan_chikungunya <- function(data, municipality_data = TRUE) {
  .sinan_deprecate(
    "process_sinan_chikungunya",
    "SINAN-FEBRE-DE-CHIKUNGUNYA"
  )
  process_sinan(data, "SINAN-FEBRE-DE-CHIKUNGUNYA", municipality_data)
}

#' Prepare SINAN dengue microdata (deprecated)
#'
#' `process_sinan_dengue()` is retained for backward compatibility. New code
#' should call [process_sinan()] with `information_system = "SINAN-DENGUE"`.
#'
#' @inheritParams process_sinan
#' @examplesIf interactive() && curl::has_internet()
#' process_sinan_dengue(sinan_dengue_sample)
#' @return The tibble returned by [process_sinan()].
#' @seealso [process_sinan()]
#' @export
process_sinan_dengue <- function(data, municipality_data = TRUE) {
  .sinan_deprecate("process_sinan_dengue", "SINAN-DENGUE")
  process_sinan(data, "SINAN-DENGUE", municipality_data)
}

#' Prepare SINAN tegumentary leishmaniasis microdata (deprecated)
#'
#' `process_sinan_leishmaniose_tegumentar()` is retained for backward
#' compatibility. New code should call [process_sinan()] with
#' `information_system = "SINAN-LEISHMANIOSE-TEGUMENTAR"`.
#'
#' @inheritParams process_sinan
#' @examplesIf interactive() && curl::has_internet()
#' process_sinan_leishmaniose_tegumentar(
#'   sinan_leishmaniose_tegumentar_sample
#' )
#' @return The tibble returned by [process_sinan()].
#' @seealso [process_sinan()]
#' @export
process_sinan_leishmaniose_tegumentar <- function(
  data,
  municipality_data = TRUE
) {
  .sinan_deprecate(
    "process_sinan_leishmaniose_tegumentar",
    "SINAN-LEISHMANIOSE-TEGUMENTAR"
  )
  process_sinan(
    data,
    "SINAN-LEISHMANIOSE-TEGUMENTAR",
    municipality_data
  )
}

#' Prepare SINAN visceral leishmaniasis microdata (deprecated)
#'
#' `process_sinan_leishmaniose_visceral()` is retained for backward
#' compatibility. New code should call [process_sinan()] with
#' `information_system = "SINAN-LEISHMANIOSE-VISCERAL"`.
#'
#' @inheritParams process_sinan
#' @examplesIf interactive() && curl::has_internet()
#' process_sinan_leishmaniose_visceral(sinan_leishmaniose_visceral_sample)
#' @return The tibble returned by [process_sinan()].
#' @seealso [process_sinan()]
#' @export
process_sinan_leishmaniose_visceral <- function(
  data,
  municipality_data = TRUE
) {
  .sinan_deprecate(
    "process_sinan_leishmaniose_visceral",
    "SINAN-LEISHMANIOSE-VISCERAL"
  )
  process_sinan(data, "SINAN-LEISHMANIOSE-VISCERAL", municipality_data)
}

#' Prepare SINAN malaria microdata (deprecated)
#'
#' `process_sinan_malaria()` is retained for backward compatibility. New code
#' should call [process_sinan()] with `information_system = "SINAN-MALARIA"`.
#'
#' @inheritParams process_sinan
#' @examplesIf interactive() && curl::has_internet()
#' process_sinan_malaria(sinan_malaria_sample)
#' @return The tibble returned by [process_sinan()].
#' @seealso [process_sinan()]
#' @export
process_sinan_malaria <- function(data, municipality_data = TRUE) {
  .sinan_deprecate("process_sinan_malaria", "SINAN-MALARIA")
  process_sinan(data, "SINAN-MALARIA", municipality_data)
}

#' Prepare SINAN Zika virus disease microdata (deprecated)
#'
#' `process_sinan_zika()` is retained for backward compatibility. New code
#' should call [process_sinan()] with
#' `information_system = "SINAN-ZIKA-VIRUS"`.
#'
#' @inheritParams process_sinan
#' @examplesIf interactive() && curl::has_internet()
#' process_sinan_zika(sinan_zika_sample)
#' @return The tibble returned by [process_sinan()].
#' @seealso [process_sinan()]
#' @export
process_sinan_zika <- function(data, municipality_data = TRUE) {
  .sinan_deprecate("process_sinan_zika", "SINAN-ZIKA-VIRUS")
  process_sinan(data, "SINAN-ZIKA-VIRUS", municipality_data)
}
