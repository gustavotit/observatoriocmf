#!/usr/bin/env Rscript
# Sube la versión del paquete (DESCRIPTION, CITATION.cff y NEWS.md).
# Uso: Rscript pipeline/version.R patch|minor|major "nota de versión"
# Escribe la nueva versión en stdout (y en $GITHUB_OUTPUT si existe).

args <- commandArgs(trailingOnly = TRUE)
bump <- if (length(args) >= 1) args[1] else "patch"
nota <- if (length(args) >= 2) args[2] else ""

d <- read.dcf("DESCRIPTION", keep.white = TRUE)
v <- as.integer(strsplit(d[1, "Version"], ".", fixed = TRUE)[[1]])
v <- c(v, 0L, 0L)[1:3]
v <- switch(bump,
  major = c(v[1] + 1L, 0L, 0L),
  minor = c(v[1], v[2] + 1L, 0L),
  c(v[1], v[2], v[3] + 1L))
nueva <- paste(v, collapse = ".")
d[1, "Version"] <- nueva
write.dcf(d, "DESCRIPTION", keep.white = colnames(d))

hoy <- format(Sys.Date())
if (file.exists("CITATION.cff")) {
  cff <- readLines("CITATION.cff", encoding = "UTF-8")
  cff <- sub("^version: .*$", paste0("version: ", nueva), cff)
  cff <- sub("^date-released: .*$", paste0("date-released: ", hoy), cff)
  writeLines(cff, "CITATION.cff", useBytes = TRUE)
}
news <- if (file.exists("NEWS.md")) readLines("NEWS.md", encoding = "UTF-8") else character()
entrada <- c(paste0("# observatoriocmf ", nueva, " (", hoy, ")"), "",
             if (nzchar(nota)) paste0("- ", nota) else "- Actualización de datos.", "")
writeLines(c(entrada, news), "NEWS.md", useBytes = TRUE)

# Si ya hay datos publicados, el manifiesto refleja la nueva versión.
mf <- file.path("inst", "extdata", "manifest.json")
if (file.exists(mf)) {
  m <- jsonlite::read_json(mf)
  m$version <- nueva
  jsonlite::write_json(m, mf, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
}

cat(nueva, "\n")
out <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(out)) cat(paste0("version=", nueva, "\n"), file = out, append = TRUE)
