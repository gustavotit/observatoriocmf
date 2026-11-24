# Descarga y lectura de archivos fuente --------------------------------------

DATA_EXT <- c("csv", "txt", "tsv", "xlsx", "xls")
FILL_MAX <- if (utils::packageVersion("data.table") >= "1.15.0") Inf else TRUE

# Devuelve las rutas de los archivos de datos de una versión (descarga y descomprime si hace falta).
fetch_version <- function(id, v, root = ".", cache = "data-raw") {
  dest_dir <- file.path(root, cache, id, safe_name(v$version))
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(v$url) && nzchar(v$url)) {
    fname <- basename(sub("\\?.*$", "", v$url))
    if (!nzchar(tools::file_ext(fname))) fname <- paste0(fname, ".zip")
    dest <- file.path(dest_dir, fname)
    if (!file.exists(dest)) {
      log_msg("Descargando ", id, " ", v$version, " desde ", v$url)
      old <- options(timeout = max(3600, getOption("timeout")))
      on.exit(options(old), add = TRUE)
      utils::download.file(v$url, dest, mode = "wb", quiet = TRUE)
    }
  } else {
    if (is.null(v$archivo) || !nzchar(v$archivo)) {
      stop("La versión ", v$version, " de ", id, " no tiene url ni archivo")
    }
    # Ruta relativa al repositorio (data-small/...) o absoluta (corridas locales con archivos pesados).
    abs <- grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", v$archivo)
    dest <- if (abs) v$archivo else file.path(root, v$archivo)
    if (!file.exists(dest)) stop("No existe el archivo ", dest, " (", id, " ", v$version, ")")
  }

  if (!is.null(v$sha256) && nzchar(v$sha256 %||% "")) {
    h <- digest::digest(file = dest, algo = "sha256")
    if (!identical(tolower(h), tolower(v$sha256))) {
      stop("El archivo de ", id, " ", v$version, " no coincide con el sha256 registrado")
    }
  }

  if (tolower(tools::file_ext(dest)) == "zip") {
    out_dir <- file.path(dest_dir, "unzip")
    if (!dir.exists(out_dir)) utils::unzip(dest, exdir = out_dir)
    files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
    files <- files[tolower(tools::file_ext(files)) %in% DATA_EXT]
    if (!length(files)) stop("El zip de ", id, " ", v$version, " no contiene archivos de datos")
    return(files)
  }
  dest
}

# ¿La versión tiene una URL o un archivo presente en este equipo?
version_reachable <- function(v, root = ".") {
  if (!is.null(v$url) && nzchar(v$url)) return(TRUE)
  if (is.null(v$archivo) || !nzchar(v$archivo)) return(FALSE)
  abs <- grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", v$archivo)
  file.exists(if (abs) v$archivo else file.path(root, v$archivo))
}

safe_name <- function(x) gsub("[^A-Za-z0-9._-]", "_", as.character(x))

# Codificación: UTF-16 si trae BOM (GRD 2022 y 2023); UTF-8 si la muestra es válida; si no, Latin-1.
# Se revisan tres tramos (inicio, mitad y final, 1 MB cada uno): un archivo puede tener solo ASCII al comienzo y
# las Ñ y tildes más adelante, y una muestra única lo daría por UTF-8 por error.
detect_encoding <- function(path) {
  size <- file.size(path)
  con <- file(path, "rb")
  on.exit(close(con))
  head <- readBin(con, "raw", 1e6)
  if (length(head) >= 2 && head[1] == as.raw(0xFF) && head[2] == as.raw(0xFE)) return("UTF-16LE")
  if (length(head) >= 2 && head[1] == as.raw(0xFE) && head[2] == as.raw(0xFF)) return("UTF-16BE")
  chunks <- list(head)
  for (pos in unique(c(floor(size / 2), max(0, size - 1e6)))) {
    if (pos <= 1e6) next
    seek(con, pos)
    chunks[[length(chunks) + 1]] <- readBin(con, "raw", 1e6)
  }
  valido <- vapply(chunks, function(r) {
    nl <- which(r == as.raw(10))
    if (length(nl) >= 2) r <- r[(nl[1] + 1):nl[length(nl)]]   # solo líneas completas (no cortar una Ñ a la mitad)
    validUTF8(rawToChar(r[r != as.raw(0)]))
  }, TRUE)
  if (all(valido)) "UTF-8" else "Latin-1"
}

# fread no lee UTF-16: se convierte una vez a UTF-8 (por bloques) y se guarda junto al caché de descargas.
utf16_to_utf8 <- function(path, enc, cache_dir = file.path(dirname(path), ".utf8")) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(cache_dir, paste0(tools::file_path_sans_ext(basename(path)), "_utf8.", tools::file_ext(path)))
  if (file.exists(dest) && file.mtime(dest) >= file.mtime(path)) return(dest)
  log_msg("Convirtiendo ", basename(path), " de ", enc, " a UTF-8")
  inp <- file(path, open = "r", encoding = enc)
  out <- file(dest, open = "w", encoding = "UTF-8")
  on.exit({ close(inp); close(out) }, add = TRUE)
  primero <- TRUE
  repeat {
    x <- readLines(inp, n = 200000, warn = FALSE)
    if (!length(x)) break
    if (primero) { x[1] <- sub("^\ufeff", "", x[1]); primero <- FALSE }
    writeLines(x, out, useBytes = FALSE)
  }
  dest
}

# Columnas a leer según el mapa (nombres normalizados → originales). Así una base GRD de 100+ columnas
# se lee solo con las necesarias.
needed_columns <- function(header, map) {
  norm <- normalize_names(header)
  keep <- character()
  for (campo in setdiff(names(map), c("separador", "formato", "saltar_filas"))) {
    spec <- map[[campo]]
    if (is.list(spec) && !is.null(spec$patron)) {
      keep <- c(keep, header[grepl(spec$patron, norm)])
    } else {
      hit <- which(norm %in% normalize_names(unlist(spec)))
      if (length(hit)) keep <- c(keep, header[hit[1]])
    }
  }
  unique(keep)
}

# Lee csv/txt/xlsx como texto. Con `map`, solo las columnas necesarias.
read_any <- function(path, sheet = NULL, map = NULL, skip = 0) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Instala readxl para leer ", path)
    dt <- readxl::read_excel(path, sheet = sheet %||% 1, col_types = "text", skip = skip)
    return(data.table::as.data.table(dt))
  }
  enc <- detect_encoding(path)
  enc_original <- enc
  if (startsWith(enc, "UTF-16")) {
    path <- utf16_to_utf8(path, enc, file.path("data-cache", "utf8"))
    enc <- "UTF-8"
  }
  sep <- map$separador %||% "auto"
  hdr <- names(data.table::fread(path, nrows = 0, sep = sep, encoding = enc, skip = skip))
  # Si con el separador del mapa el encabezado queda en una sola columna (un año con otro formato), se detecta.
  if (length(hdr) <= 1 && !identical(sep, "auto")) {
    sep <- "auto"
    hdr <- names(data.table::fread(path, nrows = 0, sep = sep, encoding = enc, skip = skip))
    log_msg("Aviso: ", basename(path), " no usa el separador del mapa de columnas; se detectó automáticamente")
  }
  hdr_clean <- gsub("^﻿|^ï»¿", "", trimws(hdr))
  sel <- if (!is.null(map)) needed_columns(hdr_clean, map) else NULL
  sel_idx <- if (length(sel)) match(sel, hdr_clean) else NULL
  # fill = Inf: una fila con un separador de más (p. ej. GRD 2022, línea 120.127) no corta la lectura.
  dt <- data.table::fread(path, sep = sep, encoding = enc, colClasses = "character", skip = skip,
                          select = sel_idx, showProgress = FALSE, na.strings = c("", "NA", "NULL"),
                          fill = FILL_MAX)
  if (!length(sel_idx) && ncol(dt) > length(hdr_clean)) dt <- dt[, seq_along(hdr_clean), with = FALSE]
  data.table::setnames(dt, hdr_clean[if (length(sel_idx)) sel_idx else seq_along(hdr_clean)])
  data.table::setattr(dt, "codificacion", enc_original)
  dt
}

read_version <- function(id, v, cfg, map = NULL) {
  files <- fetch_version(id, v, cfg$root)
  skip <- if (is.null(map$saltar_filas)) 0 else as.integer(map$saltar_filas)
  dts <- lapply(files, read_any, sheet = v$hoja, map = map, skip = skip)
  enc <- unique(unlist(lapply(dts, attr, "codificacion")))
  out <- data.table::rbindlist(dts, use.names = TRUE, fill = TRUE)
  data.table::setattr(out, "codificacion", paste(enc, collapse = ", "))
  out
}
