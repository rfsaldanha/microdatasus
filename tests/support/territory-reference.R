# Reproducible maintenance/test builder, deliberately outside the runtime API.
.territory_member_hashes <- c(
  "tb_municip.txt" = "33b995e2885f06ba0aa7cd44e619598572ca7fa4471ecc4179fb84a90fc4eb3b",
  "tb_municip_layout.txt" = "ccce3c86f18c1d8ed06b6543580a1a960d5a6547ded56b09c955585330129b47",
  "tb_uf.txt" = "b591221b9b53cf064729d7e1696cee6ae35d833234f1104cc66d64aa6c46c870",
  "tb_uf_layout.txt" = "01eda1dd2be900844a8b19d1920900aee92ff441393682fb202db53ce8ea46c0"
)

.territory_read_member <- function(archive, member) {
  connection <- unz(archive, member, open = "rb")
  on.exit(close(connection), add = TRUE)
  bytes <- readBin(connection, "raw", n = 2000000L)
  if (!identical(digest::digest(bytes, algo = "sha256", serialize = FALSE),
                 unname(.territory_member_hashes[member]))) {
    stop("Territorial source checksum differs: ", member)
  }
  bytes
}

.territory_read_fixed <- function(archive, table) {
  layout <- read.csv2(text = rawToChar(.territory_read_member(
    archive, paste0(table, "_layout.txt"))), stringsAsFactors = FALSE)
  text <- iconv(rawToChar(.territory_read_member(archive, paste0(table, ".txt"))),
                 from = "ISO-8859-1", to = "UTF-8", sub = NA)
  if (is.na(text)) stop("Invalid territorial text encoding.")
  lines <- strsplit(text, "\r\n|\n|\r")[[1L]]
  stopifnot(all(nchar(lines) == max(layout$Fim)))
  fields <- lapply(seq_len(nrow(layout)), function(i) {
    trimws(substring(lines, layout$Inicio[[i]], layout$Fim[[i]]))
  })
  names(fields) <- layout$Coluna
  as.data.frame(fields, stringsAsFactors = FALSE)
}

.rebuild_tabmun <- function(archive) {
  municipalities <- .territory_read_fixed(archive, "tb_municip")
  states <- .territory_read_fixed(archive, "tb_uf")
  stopifnot(nrow(municipalities) == 5659L,
            !anyDuplicated(municipalities$CO_MUNICIP), !anyDuplicated(states$CO_UF),
            all(grepl("^[0-9]{6}$", municipalities$CO_MUNICIP)),
            all(municipalities$CO_UF %in% states$CO_UF))
  result <- data.frame(
    munResCod = as.integer(municipalities$CO_MUNICIP),
    munResStatus = factor(municipalities$CO_STATUS, levels = c("ATIVO", "EXTINT", "IGNOR", "TRANSF")),
    munResTipo = factor(municipalities$CO_TIPO, levels = c("DISEST", "DISFED", "IGNOR", "MUNIC", "TERRIT")),
    munResNome = municipalities$DS_NOME,
    munResUf = states$DS_NOME[match(municipalities$CO_UF, states$CO_UF)],
    stringsAsFactors = FALSE
  )
  stopifnot(!anyNA(result$munResStatus), !anyNA(result$munResTipo))
  source_fields <- c("NU_LATITUD", "NU_LONGIT", "NU_ALTITUD", "NU_AREA")
  target_fields <- c("munResLat", "munResLon", "munResAlt", "munResArea")
  for (i in seq_along(source_fields)) {
    values <- municipalities[[source_fields[[i]]]]
    stopifnot(all(grepl("^-?[0-9]+(,[0-9]+)?$", values)))
    result[[target_fields[[i]]]] <- as.numeric(sub(",", ".", values, fixed = TRUE))
  }
  # Preserve the existing public table's documented compatibility policy.
  # Source zero placeholders are not real coordinates/altitude/area for these
  # 27 unknown units and the former federal territory (code 200010).
  missing <- result$munResStatus == "IGNOR" | result$munResCod == 200010L
  stopifnot(all(as.matrix(result[missing, target_fields]) == 0))
  result[missing, target_fields] <- NA_real_
  result$munResAlt <- as.integer(result$munResAlt)
  result$munResUf[result$munResCod == 0L] <- ""
  result$munResUf[result$munResCod == 200010L] <- "Pernambuco"
  result <- result[order(result$munResCod), ]
  rownames(result) <- NULL
  result
}
