# Flatten fields, labels, symbolic ranges, and relation states to stable keys.
.datasus_dictionary_long <- function(variables) {
  variable_rows <- data.frame(
    key = paste("field", variables$field, variables$file, variables$command,
                variables$position, sep = "::"),
    kind = "field", field = variables$field, code = NA_character_,
    description = variables$description, label = NA_character_,
    stringsAsFactors = FALSE
  )
  status <- if ("status" %in% names(variables)) variables$status else rep("ok", nrow(variables))
  message <- if ("message" %in% names(variables)) variables$message else rep(NA_character_, nrow(variables))
  status_rows <- data.frame(
    key = paste("status", variables$field, variables$file, variables$command,
                variables$position, sep = "::"),
    kind = "status", field = variables$field, code = NA_character_,
    description = paste(status, replace(message, is.na(message), ""), sep = "::"),
    label = NA_character_, stringsAsFactors = FALSE
  )
  nested <- lapply(seq_len(nrow(variables)), function(index) {
    result <- list()
    labels <- variables$labels[[index]]
    if (nrow(labels)) {
      result[["labels"]] <- data.frame(
        key = paste("label", variables$field[[index]], variables$file[[index]],
                    labels$code, sep = "::"),
        kind = "label", field = variables$field[[index]], code = labels$code,
        description = NA_character_, label = labels$label, stringsAsFactors = FALSE
      )
    }
    if ("ranges" %in% names(variables)) {
      ranges <- variables$ranges[[index]]
      if (nrow(ranges)) {
        result[["ranges"]] <- data.frame(
          key = paste("range", variables$field[[index]], variables$file[[index]],
                      ranges$token, sep = "::"),
          kind = "range", field = variables$field[[index]], code = ranges$token,
          description = NA_character_, label = ranges$label, stringsAsFactors = FALSE
        )
      }
    }
    if (length(result)) do.call(rbind, result) else NULL
  })
  nested <- Filter(Negate(is.null), nested)
  rows <- list(variable_rows, status_rows)
  if (length(nested)) rows[[3L]] <- do.call(rbind, nested)
  do.call(rbind, rows)
}

#' Compare cached and current DataSUS dictionaries
#'
#' @param information_system A value accepted by datasus_variables().
#' @param previous Optional table previously returned by datasus_variables().
#'   When NULL, the currently cached dictionary is used as the baseline.
#' @param refresh Logical scalar. If TRUE, download the current archive after
#'   capturing the baseline.
#' @inheritParams datasus_variables
#'
#' @return A tibble describing added, removed, and changed fields, labels,
#'   symbolic ranges, or relation states. Column `kind` identifies the item.
#'
#' @examplesIf interactive() && curl::has_internet()
#' changes <- compare_datasus_dictionary("SIM-DO")
#'
#' @export
compare_datasus_dictionary <- function(
  information_system,
  previous = NULL,
  refresh = TRUE,
  timeout = 240,
  quiet = FALSE,
  cache_dir = getOption("microdatasus.cache_dir", NULL)
) {
  .datasus_assert_flag(refresh, "refresh")
  before <- if (is.null(previous)) {
    datasus_variables(
      information_system,
      include_labels = TRUE,
      timeout = timeout,
      refresh = FALSE,
      quiet = quiet,
      cache_dir = cache_dir
    )
  } else {
    required <- c(
      "field", "file", "command", "position",
      "description", "type", "labels"
    )
    if (!is.data.frame(previous) || !all(required %in% names(previous))) {
      cli::cli_abort(
        "{.arg previous} must be NULL or a table returned by {.fn datasus_variables}."
      )
    }
    previous
  }
  after <- datasus_variables(
    information_system,
    include_labels = TRUE,
    timeout = timeout,
    refresh = refresh,
    quiet = quiet,
    cache_dir = cache_dir
  )
  old <- .datasus_dictionary_long(before)
  current <- .datasus_dictionary_long(after)
  keys <- union(old$key, current$key)
  old_index <- match(keys, old$key)
  new_index <- match(keys, current$key)
  old_value <- ifelse(
    is.na(old$code[old_index]),
    old$description[old_index],
    old$label[old_index]
  )
  new_value <- ifelse(
    is.na(current$code[new_index]),
    current$description[new_index],
    current$label[new_index]
  )
  value_changed <- is.na(old_value) != is.na(new_value) |
    (!is.na(old_value) & !is.na(new_value) & old_value != new_value)
  change <- ifelse(
    is.na(old_index),
    "added",
    ifelse(
      is.na(new_index),
      "removed",
      ifelse(value_changed, "changed", "unchanged")
    )
  )
  changed <- change != "unchanged"
  tibble::tibble(
    information_system = information_system,
    kind = ifelse(
      is.na(new_index[changed]), old$kind[old_index[changed]],
      current$kind[new_index[changed]]
    ),
    change = change[changed],
    field = ifelse(
      is.na(new_index[changed]),
      old$field[old_index[changed]],
      current$field[new_index[changed]]
    ),
    code = ifelse(
      is.na(new_index[changed]),
      old$code[old_index[changed]],
      current$code[new_index[changed]]
    ),
    before = old_value[changed],
    after = new_value[changed]
  )
}
