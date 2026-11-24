#!/usr/bin/env Rscript
# Importa las salidas AGREGADAS de un estudio económico (notebook de un artículo) al observatorio ---------------
#
#   Rscript pipeline/importar_estudio.R <id_estudio> ["<carpeta de salida del notebook>"] [<carpeta destino>]
#
# Lee config/estudios_economicos.yml, toma de cada CSV solo las columnas declaradas (renombradas), revisa que no
# haya identificadores, fechas ni filas individuales, suprime celdas con menos de n_min casos y escribe
#   <destino>/estudio_<id>_<tabla>.csv   (por defecto destino = inst/extdata)
# más una copia de estudios_economicos.yml. Se corre desde la carpeta del repositorio (o de la copia de trabajo).

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("R no está en una configuración UTF-8 (", Sys.getlocale("LC_CTYPE"), "). En Windows usa R >= 4.2.", call. = FALSE)
}
suppressPackageStartupMessages(library(data.table))
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Uso: Rscript pipeline/importar_estudio.R <id_estudio> [carpeta_origen] [carpeta_destino]")

cfg_all <- yaml::read_yaml("config/estudios_economicos.yml", fileEncoding = "UTF-8")$estudios
est <- Filter(function(e) identical(e$id, args[1]), cfg_all)
if (!length(est)) stop("Estudio desconocido en config/estudios_economicos.yml: ", args[1])
est <- est[[1]]
origen <- if (length(args) >= 2 && nzchar(args[2])) args[2] else est$carpeta_origen
destino <- if (length(args) >= 3) args[3] else file.path("inst", "extdata")
if (!is.null(origen) && !dir.exists(origen)) {
  en_descargas <- file.path(path.expand("~"), "Downloads", origen)
  if (.Platform$OS.type == "windows") en_descargas <- c(en_descargas, file.path(Sys.getenv("USERPROFILE"), "Downloads", origen))
  origen <- c(en_descargas[dir.exists(en_descargas)], origen)[1]
}
if (is.null(origen) || !dir.exists(origen)) stop("No encuentro la carpeta de salida del estudio: ", origen)
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
n_min <- tryCatch(yaml::read_yaml("config/pipeline.yml")$supresion$n_min, error = function(e) NULL) %||% 5

# Columnas que delatan registros individuales: no se publican nunca.
PROHIBIDAS <- "(^|_)(rut|run|episode|episodio|id_paciente|paciente|hospital_id|cod_hospital|fecha|nacimiento|nombre_paciente)($|_)"
MAX_FILAS <- 5000

cat("Estudio:", est$id, "\nOrigen: ", origen, "\nDestino:", destino, "\n\n")
salidas <- list()
for (tb in names(est$tablas)) {
  t <- est$tablas[[tb]]
  f <- file.path(origen, t$archivo)
  if (!file.exists(f)) { cat(sprintf("  %-20s no está (%s): se omite\n", tb, t$archivo)); next }
  d <- fread(f, encoding = "UTF-8", na.strings = c("", "NA", "nan", "NaN"))
  cols <- unlist(t$columnas)
  # YAML lee claves sin comillas como n, y, no, on u off como lógicos: deben ir entre comillas ("n": ...).
  if (any(names(cols) %in% c("TRUE", "FALSE"))) stop(t$archivo, ": escribe entre comillas las claves n, y, no, on u off en config/estudios_economicos.yml")
  opcional <- grepl("\\?$", names(cols))
  names(cols) <- sub("\\?$", "", names(cols))
  falta <- setdiff(cols[!opcional], names(d))
  if (length(falta)) stop(t$archivo, ": faltan las columnas ", paste(falta, collapse = ", "),
                          ". Revisa config/estudios_economicos.yml")
  cols <- cols[cols %in% names(d)]
  if (any(grepl(PROHIBIDAS, tolower(names(cols))) | grepl(PROHIBIDAS, tolower(cols)))) {
    stop(t$archivo, ": una columna parece identificador o fecha (", paste(cols, collapse = ", "), "). No se importa.")
  }
  if (nrow(d) > MAX_FILAS) stop(t$archivo, ": ", nrow(d), " filas; parece una tabla de registros, no agregada.")
  out <- d[, unname(cols), with = FALSE]
  setnames(out, names(cols))
  # Supresión de celdas pequeñas (tablas con conteos por celda).
  if ("n" %in% names(out)) {
    chica <- !is.na(out$n) & out$n > 0 & out$n < n_min
    if (any(chica)) {
      for (k in setdiff(names(out), c("anio", "servicio_salud", "escenario", "panel"))) set(out, which(chica), k, NA)
      cat(sprintf("  %-20s %d celdas con menos de %d casos suprimidas\n", tb, sum(chica), n_min))
    }
  }
  salidas[[tb]] <- out
  cat(sprintf("  %-20s %4d filas  <- %s\n", tb, nrow(out), t$archivo))
}
if (!length(salidas)) stop("No se importó ninguna tabla: revisa la carpeta de origen")
# Todo o nada: se escribe solo si todas las tablas pasaron los controles; se borran las del estudio anterior.
unlink(list.files(destino, pattern = paste0("^estudio_", est$id, "_.*\\.csv$"), full.names = TRUE))
for (tb in names(salidas)) fwrite(salidas[[tb]], file.path(destino, paste0("estudio_", est$id, "_", tb, ".csv")), bom = FALSE)
invisible(file.copy("config/estudios_economicos.yml", file.path(destino, "estudios_economicos.yml"), overwrite = TRUE))
cat("\nListo:", length(salidas), "tablas del estudio", est$id, "en", destino, "\n")
