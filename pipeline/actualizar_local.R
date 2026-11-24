#!/usr/bin/env Rscript
# Actualización anual en el computador local -------------------------------------------------------
#
# Registra los archivos DEIS, GRD, INE e IPC desde carpetas locales (no se suben a ningún lado), ejecuta el
# pipeline y deja las tablas agregadas en inst/extdata. Se usa desde notebooks/actualizacion_anual.ipynb o así:
#
#   Rscript pipeline/actualizar_local.R config_local.json
#
# config_local.json:
#   {"deis_dir": "C:/datos/DEIS", "deis_patron": "EGRE_DATOS_ABIERTOS_{anio}.csv",
#    "grd_dir": "C:/datos/GRD",   "grd_patron": "GRD_PUBLICO_{anio}.txt",
#    "ine": "C:/datos/estimaciones-y-proyecciones-2002-2035-comuna-y-area-urbana-y-rural.xlsx",
#    "ipc": "C:/datos/Indicador.xls",
#    "nacimientos_extra": ["C:/datos/nacidos_vivos_2026.csv"],
#    "anios_deis": [2001, 2025], "anios_grd": [2019, 2026]}
#
# Corre sobre una COPIA del repositorio: reescribe config/sources.yml de esa copia con rutas absolutas.

# En Windows, R >= 4.2 trabaja en UTF-8; con versiones anteriores (o forzando LANG=C.UTF-8, que Windows no
# reconoce) los acentos y guiones del código se leen mal y la corrida se corta.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("R no está en una configuración UTF-8 (", Sys.getlocale("LC_CTYPE"), "). En Windows usa R >= 4.2 ",
       "y no definas LANG/LC_ALL.", call. = FALSE)
}
# data.table < 1.15 no acepta fill = Inf y corta la lectura en filas con un separador de más (GRD 2022).
if (utils::packageVersion("data.table") < "1.15.0") {
  stop("Actualiza data.table (install.packages('data.table')): se requiere >= 1.15", call. = FALSE)
}
args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) stop("Uso: Rscript pipeline/actualizar_local.R config_local.json")
loc <- jsonlite::read_json(args[1])
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
P <- function(x) gsub("\\\\", "/", normalizePath(x, mustWork = FALSE))

por_anio <- function(dir, patron, rango) {
  if (is.null(dir) || !dir.exists(dir)) return(list())
  anios <- seq(rango[[1]] %||% 2001, rango[[2]] %||% as.integer(format(Sys.Date(), "%Y")))
  out <- list()
  for (a in anios) {
    f <- file.path(dir, sub("{anio}", a, patron, fixed = TRUE))
    if (file.exists(f)) out[[length(out) + 1]] <- list(version = as.character(a), anios = list(a), archivo = P(f))
  }
  out
}

src <- yaml::read_yaml("config/sources.yml", fileEncoding = "UTF-8")
src$fuentes$egresos_deis$versiones <- por_anio(loc$deis_dir, loc$deis_patron %||% "EGRE_DATOS_ABIERTOS_{anio}.csv",
                                               loc$anios_deis %||% list(2001, NULL))
src$fuentes$grd$versiones <- por_anio(loc$grd_dir, loc$grd_patron %||% "GRD_PUBLICO_{anio}.txt",
                                      loc$anios_grd %||% list(2019, NULL))
un <- function(f, v = "local") if (!is.null(f) && file.exists(f)) list(list(version = v, archivo = P(f))) else NULL
# Parámetros: por defecto se usan los registrados en el repositorio (data-small/parametros/, actualizables desde
# el panel). Las rutas locales solo se usan si el repositorio aún no tiene versión de esa fuente.
sin_version <- function(id) !length(src$fuentes[[id]]$versiones)
if (sin_version("poblacion_ine") && !is.null(un(loc$ine))) src$fuentes$poblacion_ine$versiones <- un(loc$ine)
if (sin_version("ipc") && !is.null(un(loc$ipc))) src$fuentes$ipc$versiones <- un(loc$ipc)
if (sin_version("productividad") && !is.null(un(loc$productividad))) src$fuentes$productividad$versiones <- un(loc$productividad)
for (f in unlist(loc$nacimientos_extra)) {
  if (file.exists(f)) src$fuentes$nacimientos_region$versiones <- c(src$fuentes$nacimientos_region$versiones,
                                                                   un(f, basename(f)))
}
n_deis <- length(src$fuentes$egresos_deis$versiones); n_grd <- length(src$fuentes$grd$versiones)
if (!n_deis && !n_grd) stop("No encontré archivos DEIS ni GRD con las rutas y patrones indicados")
message("Registrados: ", n_deis, " años DEIS, ", n_grd, " años GRD")
yaml::write_yaml(src, "config/sources.yml", fileEncoding = "UTF-8")

Sys.setenv(CMF_PERMITIR_SIN_EVENTOS = "1")
inicio <- Sys.time()
ok <- tryCatch({
  source("pipeline/run_pipeline.R", encoding = "UTF-8")
  TRUE
}, error = function(e) { message("ERROR: ", conditionMessage(e)); FALSE })
fresco <- file.exists("inst/extdata/manifest.json") && file.mtime("inst/extdata/manifest.json") >= inicio &&
  file.exists("inst/extdata/cubo.csv.gz")
if (!ok || !fresco) {
  message("La corrida no generó datos nuevos en inst/extdata: revisa los mensajes anteriores.")
  quit(status = 1)
}
