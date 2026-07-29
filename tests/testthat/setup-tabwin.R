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

tabwin_cnv_line <- function(number, label, codes) {
  paste0(
    "   ",
    sprintf("%4d", number),
    "  ",
    label,
    strrep(" ", 50L - nchar(label, type = "chars")),
    " ",
    codes
  )
}

write_tabwin_text <- function(path, lines) {
  writeLines(
    iconv(lines, from = "UTF-8", to = "windows-1252"),
    path,
    useBytes = TRUE
  )
}

.empty_tabwin_dictionary <- function(information_system = "SIM-DO") {
  spec <- microdatasus:::.tabwin_registry()[[information_system]]
  structure(
    list(
      information_system = information_system,
      archive_key = spec$archive_key,
      definitions = .empty_tabwin_definitions,
      conversions = new.env(parent = emptyenv())
    ),
    class = "microdatasus_tabwin_dictionary"
  )
}

for (information_system in names(microdatasus:::.tabwin_registry())) {
  assign(
    information_system,
    .empty_tabwin_dictionary(information_system),
    envir = microdatasus:::.tabwin_cache
  )
}

restore_empty_tabwin_cache <- function() {
  microdatasus:::.tabwin_clear_cache()
  for (information_system in names(microdatasus:::.tabwin_registry())) {
    assign(
      information_system,
      .empty_tabwin_dictionary(information_system),
      envir = microdatasus:::.tabwin_cache
    )
  }
}
