#' microdatasus: Download and prepare DataSUS microdata
#'
#' Provides a reproducible workflow for DataSUS microdata: [fetch_datasus()]
#' discovers, downloads, and combines published DBC files; [read_dbc()] reads a
#' local DBC file directly, with structural and checksum validation; and the
#' `process_*()` functions interpret official TabWin DEF/CNV/DBF dictionaries
#' to produce period-correct values, labels, and column types.
#'
#' Start with [fetch_datasus()] and then use the processor corresponding to the
#' selected system, such as [process_sim()], [process_sinasc()], [process_sih()],
#' [process_sia()], [process_cnes()], or [process_sinan()]. [fetch_cadger()]
#' and [fetch_sigtab()] also retrieve current
#' auxiliary tables for standalone use.
#'
#' Use [datasus_variables()] to inspect parsed dictionary definitions and
#' relations, [validate_datasus_schema()] to compare raw DBC fields with the
#' selected historical definitions and processed types, and
#' [processing_diagnostics()] to retrieve unknown codes, coercion failures, and
#' dictionary provenance from an individual processing call.
#'
#' For concepts, coverage, data flows, and caveats of each Brazilian health
#' information system, see Saldanha (2026), [*Sistemas de Informação em Saúde
#' no Brasil*](https://rfsaldanha.github.io/sis/).
#'
#' @importFrom utils globalVariables
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables( c('a') )
