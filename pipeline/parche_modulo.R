#!/usr/bin/env Rscript
# Parche por módulo ---------------------------------------------------------------------------------
#
# Reclasifica UN módulo en data-casos/ sin rehacer la actualización anual completa y luego corre la fase 2.
# Sirve cuando cambia la definición de un módulo (diccionario, subgrupos, actividades) y los demás no.
#
#   Rscript pipeline/parche_modulo.R config_local.json patologia_maxilar
#
# Qué hace:
#   1. Registra en memoria los archivos DEIS y GRD de config_local.json (no reescribe config/sources.yml).
#   2. Por cada año lee solo las columnas necesarias y, ANTES de armonizar, se queda con los egresos cuyo
#      diagnóstico principal (o cualquier posición, si el módulo la usa) calza con un código del módulo. Esa es
#      la parte lenta de la fase 1, y aquí se hace sobre unos pocos miles de filas por año.
#   3. Clasifica con build_module_cases() (la misma función de la fase 1) y reemplaza las filas del módulo en
#      data-casos/casos_*.csv.gz. Los demás módulos no se tocan (si les falta la columna actividad, quedan como
#      "Hospitalización", que es lo que eran).
#   4. Si el módulo cuenta CMA en GRD, agrega los totales de egresos CMA a data-casos/totales_base.csv.gz.
#   5. Respaldo previo en data-casos_respaldo_<fecha>/ y luego fase 2 (run_pipeline.R con CMF_SOLO_FASE2=1).
#
# La caché de data-cache/ queda desactualizada a propósito: la próxima actualización anual completa lo
# reprocesa todo con las definiciones vigentes, que es lo que debe pasar.

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  stop("R no está en una configuración UTF-8 (", Sys.getlocale("LC_CTYPE"), "). En Windows usa R >= 4.2.", call. = FALSE)
}
if (utils::packageVersion("data.table") < "1.15.0" && !nzchar(Sys.getenv("CMF_PERMITIR_DT_ANTIGUO"))) {
  stop("Actualiza data.table (install.packages('data.table')): se requiere >= 1.15", call. = FALSE)
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Uso: Rscript pipeline/parche_modulo.R config_local.json <id_modulo>")
suppressPackageStartupMessages(library(data.table))
root <- normalizePath(".")
for (f in sort(list.files(file.path(root, "pipeline", "R"), pattern = "\\.R$", full.names = TRUE))) source(f, encoding = "UTF-8")
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
P <- function(x) gsub("\\\\", "/", normalizePath(x, mustWork = FALSE))
loc <- jsonlite::read_json(args[1]); MID <- args[2]
inicio <- Sys.time()

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

log_msg("Parche del módulo ", MID)
cfg <- load_config(root)
cfg$sources$egresos_deis$versiones <- por_anio(loc$deis_dir, loc$deis_patron %||% "EGRE_DATOS_ABIERTOS_{anio}.csv",
                                               loc$anios_deis %||% list(2001, NULL))
cfg$sources$grd$versiones <- por_anio(loc$grd_dir, loc$grd_patron %||% "GRD_PUBLICO_{anio}.txt",
                                      loc$anios_grd %||% list(2019, NULL))
dicts <- load_dictionaries(cfg)
dicts$dx <- apply_official_catalog(dicts$dx, cfg, "catalogo_dx")
dicts$proc <- apply_official_catalog(dicts$proc, cfg, "catalogo_proc")
dicts$icdpic <- load_icdpic(cfg)
mod <- Filter(function(m) identical(m$id, MID), event_modules(cfg))
if (!length(mod)) stop("Módulo desconocido en config/modules.yml: ", MID)
mod <- mod[[1]]
cod_mod <- dicts$dx[modulo == MID & incluir == TRUE & !is.na(codigo_norm)]$codigo_norm
if (!length(cod_mod)) stop("El módulo ", MID, " no tiene códigos incluidos en el diccionario")

# Columna original del diagnóstico principal (o todas, según la posición del módulo) y de la actividad.
cols_dx <- function(raw, map, fuente, posicion) {
  nm <- normalize_names(names(raw))
  if (fuente == "grd") {
    pat <- map$diagnosticos$patron
    c_ <- names(raw)[grepl(pat, nm)]
    c_ <- c_[order(suppressWarnings(as.integer(gsub("\\D", "", c_))))]
    if (posicion == "cualquiera") c_ else c_[1]
  } else {
    hit <- match(normalize_names(unlist(map$diag1)), nm)
    names(raw)[hit[!is.na(hit)][1]]
  }
}
col_act <- function(raw, map) {
  nm <- normalize_names(names(raw)); hit <- match(normalize_names(unlist(map$tipo_actividad)), nm)
  if (any(!is.na(hit))) names(raw)[hit[!is.na(hit)][1]] else NA_character_
}

nuevos <- list(); tot_cma <- list(); qc_n <- list(); procesados <- character()
for (id in intersect(unlist(mod$fuentes), c("egresos_deis", "grd"))) {
  src <- cfg$sources[[id]]
  map <- cfg$colmaps[[src$mapa_columnas]]
  fuente <- if (identical(src$mapa_columnas, "grd")) "grd" else "deis"
  posicion <- mod$posicion[[id]] %||% "dx1"
  suma_cma <- "CMA" %in% unlist(mod_get(mod, "actividades", id))
  for (v in source_versions(cfg, id)) {
    raw <- read_version(id, v, cfg, map)
    n_leidos <- nrow(raw)
    dxc <- cols_dx(raw, map, fuente, posicion)
    keep <- rep(FALSE, nrow(raw))
    for (cc in dxc) keep <- keep | !is.na(match_prefix(normalize_cie(raw[[cc]]), cod_mod))
    es_cma <- rep(FALSE, nrow(raw))
    if (fuente == "grd" && suma_cma) {
      ca <- col_act(raw, map)
      if (!is.na(ca)) {
        a <- repair_text(raw[[ca]]); attr(a, "calidad") <- NULL
        es_cma <- !(normalize_value(a) %in% normalize_value(unlist(cfg$recode$actividad_hospitalizacion))) &
          contains_any(a, unlist(cfg$recode$actividad_cma))
      }
    }
    sub_ <- raw[keep | es_cma]
    rm(raw); invisible(gc(verbose = FALSE))
    anio_archivo <- if (length(v$anios) == 1) as.integer(v$anios[[1]]) else NA_integer_
    h <- harmonize_events(sub_, map, cfg, dicts, fuente, anio_archivo)
    mc <- mechanism_cfg(cfg$params, id)
    h$ev[, mecanismo := classify_mechanism(h$dx, dicts$ce, mc$columna %||% "mecanismo",
                                           solo_etiologicos = !isFALSE(mc$solo_etiologicos),
                                           sin_dato = mc$sin_dato %||% "Sin causa externa registrada")]
    if (fuente == "grd" && suma_cma) {
      tc <- h$ev[actividad %in% "CMA", .(n_total = .N), by = .(anio, region, sexo, grupo_edad_5, actividad)]
      if (nrow(tc)) tot_cma[[paste(id, v$version)]] <- tc[, fuente := id]
    }
    r <- build_module_cases(h, mod, id, dicts, cfg)
    if (!is.null(r$casos)) {
      r$casos[, `:=`(fuente = id, version = v$version)]
      nuevos[[paste(id, v$version)]] <- r$casos
    }
    qc_n[[paste(id, v$version)]] <- r$n
    procesados <- c(procesados, paste(id, unique(stats::na.omit(h$ev$anio))))
    log_msg(id, " ", v$version, ": ", n_leidos, " leídos, ", sum(keep), " con código del módulo, ", r$n, " casos",
            if (fuente == "grd" && suma_cma) paste0(" (", sum(es_cma), " egresos CMA para totales)"))
  }
}
if (!length(procesados)) stop("No se leyó ninguna base: revisa las rutas de config_local.json")
nuevos <- if (length(nuevos)) rbindlist(nuevos, use.names = TRUE, fill = TRUE) else data.table(fuente = character(), anio = integer())

# Respaldo y reemplazo en data-casos/ --------------------------------------------------------------------
dir_c <- file.path(root, CASOS_DIR)
if (!has_cases(root)) stop("No hay data-casos/: corre primero la actualización anual completa")
resp <- file.path(root, paste0("data-casos_respaldo_", format(Sys.time(), "%Y%m%d_%H%M")))
dir.create(resp, showWarnings = FALSE)
invisible(file.copy(list.files(dir_c, full.names = TRUE), resp, overwrite = TRUE))
log_msg("Respaldo: ", basename(resp))

leer <- function(f) {
  cols <- names(fread_gz(f, nrows = 0))
  fread_gz(f, encoding = "UTF-8", na.strings = c("", "NA"), colClasses = list(character = intersect(CASOS_TEXTO, cols)))
}
claves <- unique(nuevos[, .(fuente, anio)])
archivos <- list.files(dir_c, "^casos_.*\\.csv(\\.gz)?$", full.names = TRUE)
hechos <- character()
for (f in archivos) {
  # Solo los años efectivamente reprocesados: una base no registrada en config_local.json conserva sus casos.
  fu <- sub("^casos_(.*)_([0-9]{4})\\.csv(\\.gz)?$", "\\1", basename(f))
  an <- as.integer(sub("^casos_(.*)_([0-9]{4})\\.csv(\\.gz)?$", "\\2", basename(f)))
  if (!paste(fu, an) %in% procesados) next
  x <- leer(f)
  if (!"actividad" %in% names(x)) x[, actividad := "Hospitalización"]
  x[is.na(actividad), actividad := "Hospitalización"]
  quitados <- sum(x$modulo == MID)
  x <- x[modulo != MID]
  nv <- nuevos[fuente == fu & anio == an]
  nv <- nv[, setdiff(names(nv), CASOS_DESCARTAR), with = FALSE]
  x <- rbindlist(list(x, nv), use.names = TRUE, fill = TRUE)
  fwrite(x, f)
  hechos <- c(hechos, paste(fu, an))
  log_msg(basename(f), ": ", quitados, " filas antiguas del módulo reemplazadas por ", nrow(nv))
}
# Años con casos nuevos y sin archivo previo
for (k in seq_len(nrow(claves))) {
  if (paste(claves$fuente[k], claves$anio[k]) %in% hechos) next
  nv <- nuevos[fuente == claves$fuente[k] & anio == claves$anio[k]]
  fwrite(nv[, setdiff(names(nv), CASOS_DESCARTAR), with = FALSE],
         file.path(dir_c, sprintf("casos_%s_%s.csv.gz", claves$fuente[k], claves$anio[k])))
}

ft <- file.path(dir_c, "totales_base.csv.gz")
if (file.exists(ft)) {
  tt <- fread_gz(ft, encoding = "UTF-8", colClasses = list(character = c("sexo", "grupo_edad_5", "fuente")))
  if (!"actividad" %in% names(tt)) tt[, actividad := "Hospitalización"]
  tt[is.na(actividad), actividad := "Hospitalización"]
  if (length(tot_cma)) {
    tc <- rbindlist(tot_cma, use.names = TRUE)
    tt <- rbindlist(list(tt[!(actividad == "CMA" & paste(fuente, anio) %in% unique(tc[, paste(fuente, anio)]))], tc),
                    use.names = TRUE, fill = TRUE)
  }
  fwrite(tt, ft)
}
qf <- file.path(dir_c, "qc.json")
if (file.exists(qf)) {
  qc <- jsonlite::read_json(qf)
  for (i in seq_along(qc)) {
    k <- paste(qc[[i]]$fuente, qc[[i]]$version)
    if (!is.null(qc_n[[k]])) qc[[i]]$casos_por_modulo[[MID]] <- qc_n[[k]]
  }
  jsonlite::write_json(qc, qf, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
}
log_msg("data-casos/ actualizado: ", nrow(nuevos), " casos de ", MID, " en ", round(as.numeric(difftime(Sys.time(), inicio, units = "mins")), 1), " min")

# Fase 2 ------------------------------------------------------------------------------------------------------
# Mismo registro de parámetros que actualizar_local.R (INE, IPC, productividad, nacidos vivos extra) para que la
# fase 2 publique las mismas tasas y valorizaciones; DEIS y GRD quedan registrados para el manifiesto.
src <- yaml::read_yaml("config/sources.yml", fileEncoding = "UTF-8")
src$fuentes$egresos_deis$versiones <- cfg$sources$egresos_deis$versiones
src$fuentes$grd$versiones <- cfg$sources$grd$versiones
un <- function(f, v = "local") if (!is.null(f) && file.exists(f)) list(list(version = v, archivo = P(f))) else NULL
sin_version <- function(id) !length(src$fuentes[[id]]$versiones)
if (sin_version("poblacion_ine") && !is.null(un(loc$ine))) src$fuentes$poblacion_ine$versiones <- un(loc$ine)
if (sin_version("ipc") && !is.null(un(loc$ipc))) src$fuentes$ipc$versiones <- un(loc$ipc)
if (sin_version("productividad") && !is.null(un(loc$productividad))) src$fuentes$productividad$versiones <- un(loc$productividad)
for (f in unlist(loc$nacimientos_extra)) {
  if (file.exists(f)) src$fuentes$nacimientos_region$versiones <- c(src$fuentes$nacimientos_region$versiones, un(f, basename(f)))
}
if (sin_version("poblacion_ine")) stop("Sin población INE registrada (\"ine\" en config_local.json): la fase 2 publicaría solo conteos. ",
                                       "data-casos/ ya quedó parchado; agrega la ruta y vuelve a correr.")
yaml::write_yaml(src, "config/sources.yml", fileEncoding = "UTF-8")
Sys.setenv(CMF_SOLO_FASE2 = "1", CMF_PERMITIR_SIN_EVENTOS = "1")
source("pipeline/run_pipeline.R", encoding = "UTF-8")
log_msg("Parche terminado en ", round(as.numeric(difftime(Sys.time(), inicio, units = "mins")), 1), " min")
