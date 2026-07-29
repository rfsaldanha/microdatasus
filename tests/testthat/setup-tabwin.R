.empty_tabwin_definitions <- data.frame(
  order = integer(),
  command = character(),
  description = character(),
  field = character(),
  argument = character(),
  position = integer(),
  file = character(),
  extension = character(),
  stringsAsFactors = FALSE
)

.empty_tabwin_dictionary <- function(information_system = "SIM-DO") {
  structure(
    list(
      information_system = information_system,
      archive_key = "SIM-OBITOS-CID10",
      definitions = .empty_tabwin_definitions,
      conversions = new.env(parent = emptyenv())
    ),
    class = "microdatasus_tabwin_dictionary"
  )
}

for (information_system in c(
  "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT"
)) {
  assign(
    information_system,
    .empty_tabwin_dictionary(information_system),
    envir = microdatasus:::.tabwin_cache
  )
}
