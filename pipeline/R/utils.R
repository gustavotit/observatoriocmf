# Utilidades generales del pipeline ------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

log_msg <- function(...) {
  cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")
}

# Quita tildes y Ñ de forma explícita: en Windows, iconv(..., "ASCII//TRANSLIT") puede dejar "?" o borrar la
# letra, y "PEÑALOLÉN" no calzaría con el diccionario.
strip_accents <- function(x) {
  x <- enc2utf8(as.character(x))
  chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da\u00f1\u00d1\u00fc\u00dc\u00e0\u00e8\u00ec\u00f2\u00f9\u00c0\u00c8\u00cc\u00d2\u00d9\u00e2\u00ea\u00ee\u00f4\u00fb\u00e7\u00c7",
         "aeiouAEIOUnNuUaeiouAEIOUaeioucC", x)
}

# Mayúsculas, sin tildes, separadores como "_" (para nombres de columnas y valores).
normalize_names <- function(x) {
  x <- strip_accents(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- toupper(trimws(x))
  x <- gsub("[^A-Z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}
normalize_value <- normalize_names

# Repara texto mal codificado. "MojiBake" (UTF-8 leído como Latin-1/Windows-1252: "Ã‘UÃ‘OA", "PeÃ±alolÃ©n")
# se revierte; el carácter de reemplazo U+FFFD ("NU\uFFFDOA", típico del DEIS 2025) no tiene vuelta y se cuenta.
# Devuelve el texto y los conteos para el informe de calidad.
repair_text <- function(x) {
  x <- enc2utf8(as.character(x))
  moj <- !is.na(x) & grepl("\u00c3[\u0080-\u00bf\u2018\u2019\u201c\u201d\u2013\u2014\u0153\u0161]|\u00c2[\u00a0-\u00bf]", x)
  rep_ok <- 0L
  if (any(moj)) {
    for (enc in c("CP1252", "latin1")) {
      i <- which(moj)
      if (!length(i)) break
      y <- suppressWarnings(iconv(x[i], "UTF-8", enc))
      ok <- !is.na(y)
      if (any(ok)) {
        y <- y[ok]; Encoding(y) <- "UTF-8"
        val <- validUTF8(y)
        x[i[ok][val]] <- y[val]; moj[i[ok][val]] <- FALSE; rep_ok <- rep_ok + sum(val)
      }
    }
  }
  danado <- sum(!is.na(x) & grepl("\ufffd", x))
  attr(x, "calidad") <- c(reparados = rep_ok, danados = danado)
  x
}

# Códigos CIE sin puntos ni espacios: "S02.6" -> "S026"; "76.76" -> "7676".
normalize_cie <- function(x) {
  x <- toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
  x[is.na(x) | x == ""] <- NA_character_
  x
}

# Lectura/escritura de tablas de salida: parquet si arrow está disponible, si no csv.gz.
output_format <- function(params) {
  fmt <- params$salida$formato %||% "parquet"
  if (fmt == "parquet" && !requireNamespace("arrow", quietly = TRUE)) {
    log_msg("arrow no está instalado: se escribirá csv.gz")
    fmt <- "csv"
  }
  fmt
}

write_table <- function(dt, dir, name, fmt) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (fmt == "parquet") {
    path <- file.path(dir, paste0(name, ".parquet"))
    arrow::write_parquet(dt, path)
  } else {
    path <- file.path(dir, paste0(name, ".csv.gz"))
    data.table::fwrite(dt, path)
  }
  invisible(path)
}

# fread de .csv.gz sin depender de R.utils (que fread exige para descomprimir): si no está, se descomprime con
# una conexión gzfile de R base a un archivo temporal.
fread_gz <- function(path, ...) {
  if (!grepl("\\.gz$", path) || requireNamespace("R.utils", quietly = TRUE)) return(data.table::fread(path, ...))
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  inp <- gzfile(path, "rb"); out <- file(tmp, "wb")
  repeat {
    b <- readBin(inp, "raw", 1e7)
    if (!length(b)) break
    writeBin(b, out)
  }
  close(inp); close(out)
  data.table::fread(tmp, ...)
}

read_table <- function(dir, name) {
  pq <- file.path(dir, paste0(name, ".parquet"))
  cs <- file.path(dir, paste0(name, ".csv.gz"))
  if (file.exists(pq)) return(data.table::as.data.table(arrow::read_parquet(pq)))
  if (file.exists(cs)) return(fread_gz(cs, encoding = "UTF-8"))
  NULL
}

hash_object <- function(x) digest::digest(x, algo = "xxhash64")

hash_files <- function(paths) {
  paths <- sort(paths[file.exists(paths)])
  digest::digest(vapply(paths, digest::digest, character(1), file = TRUE, algo = "xxhash64"),
                 algo = "xxhash64")
}

as_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
