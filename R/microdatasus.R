#' microdatasus: Download and prepare DataSUS microdata
#'
#' Provides a reproducible workflow for DataSUS microdata: [fetch_datasus()]
#' discovers, downloads, and combines published DBC files; [read_dbc()] reads a
#' local DBC file; and the `process_*()` functions convert system-specific
#' fields into analysis-ready values and labels.
#'
#' Start with [fetch_datasus()] and then use the processor corresponding to the
#' selected system, such as [process_sim()], [process_sinasc()], [process_sih()],
#' [process_sia()], [process_cnes()], or one of the `process_sinan_*()`
#' functions. [fetch_cadger()] and [fetch_sigtab()] also retrieve current
#' auxiliary tables for standalone use.
#'
#' For concepts, coverage, data flows, and caveats of each Brazilian health
#' information system, see Saldanha (2026), [*Sistemas de Informação em Saúde
#' no Brasil*](https://rfsaldanha.github.io/sis/).
#'
#' @importFrom utils globalVariables
#' @importFrom data.table := setDT fcase
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables( c('a') )
