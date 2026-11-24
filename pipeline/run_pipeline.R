#!/usr/bin/env Rscript
# Pipeline del Observatorio CMF Chile.
# Uso (desde la raíz del repositorio): Rscript pipeline/run_pipeline.R
#
# 1. Lee la configuración (config/) y los diccionarios (dictionaries/).
# 2. Descarga o lee cada versión registrada en config/sources.yml (caché en data-raw/ y data-cache/).
# 3. Armoniza (edad, comuna → región y Servicio de Salud, desenlaces), clasifica por módulo, subgrupo y
#    mecanismo, y valoriza (valor GRD por clúster de precios, IPC, MAI y productividad opcionales).
# 4. Construye tablas agregadas, valida y exporta a inst/extdata/ con un manifiesto de versión.

suppressPackageStartupMessages(library(data.table))

root <- normalizePath(".")
for (f in sort(list.files(file.path(root, "pipeline", "R"), pattern = "\\.R$", full.names = TRUE))) {
  source(f, encoding = "UTF-8")
}

log_msg("Observatorio CMF Chile: pipeline")
cfg <- load_config(root)
dicts <- load_dictionaries(cfg)
dicts$dx <- apply_official_catalog(dicts$dx, cfg, "catalogo_dx")
dicts$proc <- apply_official_catalog(dicts$proc, cfg, "catalogo_proc")
dicts$icdpic <- load_icdpic(cfg)
code_hash <- hash_files(list.files(file.path(root, "pipeline", "R"), full.names = TRUE))

# Bases de eventos -----------------------------------------------------------------
ev_ids <- c(sources_by_role(cfg, "egresos"), sources_by_role(cfg, "defunciones"))
# Fase 1 (bases DEIS/GRD accesibles en este equipo): clasificar y guardar los casos en data-casos/.
# Fase 2 (sin bases, p. ej. GitHub Actions): usar los casos ya clasificados. Si no hay ni bases ni casos y ya
# existen datos publicados, se detiene para no reemplazarlos por tablas vacías.
accesibles <- sum(vapply(ev_ids, function(id) sum(vapply(cfg$sources[[id]]$versiones %||% list(),
                                                          version_reachable, TRUE, root = root)), 0))
# CMF_SOLO_FASE2=1 (p. ej. tras pipeline/parche_modulo.R): usa data-casos/ aunque las bases estén accesibles.
if (accesibles > 0 && !nzchar(Sys.getenv("CMF_SOLO_FASE2"))) {
  results <- Filter(Negate(is.null), setNames(lapply(ev_ids, process_event_source, cfg = cfg, dicts = dicts,
                                                     code_hash = code_hash), ev_ids))
  bind <- function(k) rbindlist(lapply(results, `[[`, k), use.names = TRUE, fill = TRUE)
  eventos <- bind("eventos"); universo <- bind("universo"); totales <- bind("totales")
  qc <- unlist(lapply(results, `[[`, "qc"), recursive = FALSE)
  save_cases(eventos, universo, totales, qc, root)
} else if (has_cases(root)) {
  lc <- load_cases(root)
  eventos <- lc$eventos; universo <- lc$universo; totales <- lc$totales; qc <- lc$qc
} else if (file.exists(file.path(root, "inst", "extdata", "cubo.csv.gz")) &&
           !nzchar(Sys.getenv("CMF_PERMITIR_SIN_EVENTOS"))) {
  stop("No hay bases DEIS/GRD ni casos clasificados (data-casos/) en este equipo: los datos publicados se ",
       "mantienen. Corre la actualización anual local y sube el zip desde el panel.")
} else {
  log_msg("Aún no hay versiones de egresos registradas en config/sources.yml")
  eventos <- data.table(); universo <- data.table(); totales <- data.table(); qc <- list()
}

# Referencias y ajustes ------------------------------------------------------------
pobl <- process_population(cfg, dicts)
pob <- pobl$poblacion
edad0 <- process_births(cfg, dicts, pobl$edad0)
ipc <- process_ipc(cfg)
mai <- process_mai(cfg)
prod <- process_productividad(cfg, dicts)
territorial <- process_territorial(cfg, dicts)

if (nrow(eventos)) eventos <- add_costs(eventos, dicts, ipc, mai, prod, cfg$params)

cubo <- build_cube(eventos, event_modules(cfg))
universo <- build_universe(universo)
estada <- build_los_summary(eventos)
severidad <- build_severity_summary(eventos)
comunas_n <- build_commune_counts(eventos)
if (nrow(totales)) totales <- totales[, .(n_total = sum(n_total)), by = .(fuente, anio, region, sexo, grupo_edad_5, actividad)]

# Validación y exportación -----------------------------------------------------------
validacion <- validate_outputs(qc, cubo, pob, dicts, cfg)

tablas <- list(
  cubo = cubo,
  universo = universo,
  totales = totales,
  estada = estada,
  severidad = severidad,
  conteos_comuna = comunas_n,
  poblacion = pob,
  poblacion_edad0 = edad0,
  ipc = ipc,
  aranceles_mai = mai,
  oferta_servicios = territorial,
  dic_diagnosticos = dicts$dx[, .(codigo, descripcion, modulo, subgrupo, incluir, requiere_proc, excluir_si_proc, nota)],
  dic_procedimientos = dicts$proc[, .(codigo, descripcion, modulo, incluir, nota)],
  dic_causas_externas = dicts$ce,
  dic_regiones = dicts$regiones,
  dic_servicios_salud = dicts$servicios,
  dic_comunas = dicts$comunas[, .(codigo_comuna, nombre_comuna, region, codigo_servicio_salud)],
  poblacion_estandar = dicts$estandar,
  mapa_regiones = read_dict(root, "mapa_regiones.csv"),
  mapa_servicios_salud = read_dict(root, "mapa_servicios_salud.csv")
)
export_outputs(tablas, cfg, validacion)
controles <- run_controls(cfg)

if (length(validacion$errores)) {
  stop(length(validacion$errores), " error(es) crítico(s): revisa inst/extdata/validacion.json")
}
log_msg("Listo.")
