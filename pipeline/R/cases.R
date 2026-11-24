# Casos clasificados (capa intermedia) --------------------------------------------------------------
#
# Fase 1 (local, una vez al año): lee DEIS y GRD, armoniza y clasifica. Guarda en data-casos/ solo los egresos
# que caen en algún módulo, sin identificadores ni fechas, más los universos y totales ya agregados.
# Fase 2 (local o GitHub Actions): valorización, población, nacidos vivos, cubo, tasas y controles. Por eso los
# parámetros (IPC, productividad, precios GRD, INE, nacidos vivos, oferta) se actualizan desde el panel y se
# reprocesan en GitHub sin volver a tocar las bases pesadas.

CASOS_DIR <- "data-casos"
CASOS_DESCARTAR <- c("comuna_fuente", "region_fuente", "hospitalizacion", "version")
CASOS_TEXTO <- c("subgrupo", "codigo_dict", "sexo", "grupo_edad_5", "comuna", "sector", "prevision", "diag1",
                 "mecanismo", "modulo", "grupo_edad", "procedimientos_txt", "fuente", "actividad")
CASOS_LOGICO <- c("aislado", "fallecido", "pabellon")

save_cases <- function(eventos, universo, totales, qc, root = ".") {
  dir <- file.path(root, CASOS_DIR)
  unlink(list.files(dir, full.names = TRUE))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(eventos) && nrow(eventos)) {
    ev <- eventos[, setdiff(names(eventos), CASOS_DESCARTAR), with = FALSE]
    for (k in split(seq_len(nrow(ev)), paste(ev$fuente, ev$anio, sep = "_"))) {
      x <- ev[k]
      data.table::fwrite(x, file.path(dir, sprintf("casos_%s_%s.csv.gz", x$fuente[1], x$anio[1])))
    }
  }
  if (!is.null(universo) && nrow(universo)) data.table::fwrite(universo, file.path(dir, "universo_base.csv.gz"))
  if (!is.null(totales) && nrow(totales)) data.table::fwrite(totales, file.path(dir, "totales_base.csv.gz"))
  jsonlite::write_json(qc, file.path(dir, "qc.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  log_msg("Casos clasificados guardados en ", CASOS_DIR, "/ (", length(list.files(dir, "^casos_")), " archivos)")
}

has_cases <- function(root = ".") length(list.files(file.path(root, CASOS_DIR), "^casos_.*\\.csv(\\.gz)?$")) > 0

load_cases <- function(root = ".") {
  dir <- file.path(root, CASOS_DIR)
  rd <- function(f) {
    if (!file.exists(f)) return(data.table::data.table())
    fread_gz(f, encoding = "UTF-8", na.strings = c("", "NA"),
             colClasses = list(character = intersect(CASOS_TEXTO, names(fread_gz(f, nrows = 0)))))
  }
  ev <- data.table::rbindlist(lapply(list.files(dir, "^casos_.*\\.csv(\\.gz)?$", full.names = TRUE), rd),
                              use.names = TRUE, fill = TRUE)
  # Casos clasificados antes de incorporar la actividad: todos eran hospitalización.
  # (al mezclar archivos antiguos y nuevos, las filas antiguas quedan con NA: también son hospitalización;
  # los casos nuevos nunca tienen actividad NA porque el filtro del módulo los excluye).
  if (nrow(ev) && !"actividad" %in% names(ev)) ev[, actividad := "Hospitalización"]
  if (nrow(ev)) ev[is.na(actividad), actividad := "Hospitalización"]
  for (k in intersect(CASOS_LOGICO, names(ev))) data.table::set(ev, j = k, value = as.logical(ev[[k]]))
  for (k in intersect(c("anio", "region", "servicio_salud", "cod_hospital", "grd_severidad", "n_proc"), names(ev))) {
    data.table::set(ev, j = k, value = as.integer(ev[[k]]))
  }
  for (k in intersect(c("edad_min", "edad_max", "dias_estada", "grd_peso", "riss", "maxais", "riss_excl"), names(ev))) {
    data.table::set(ev, j = k, value = as.numeric(ev[[k]]))
  }
  u <- rd(file.path(dir, "universo_base.csv.gz")); t <- rd(file.path(dir, "totales_base.csv.gz"))
  for (d in list(u, t)) for (k in intersect(c("sexo", "grupo_edad", "grupo_edad_5", "prevision", "sector", "fuente", "modulo"), names(d))) {
    data.table::set(d, j = k, value = as.character(d[[k]]))
  }
  if (nrow(t) && !"actividad" %in% names(t)) t[, actividad := "Hospitalización"]
  if (nrow(t)) t[is.na(actividad), actividad := "Hospitalización"]
  qf <- file.path(dir, "qc.json")
  qc <- if (file.exists(qf)) jsonlite::read_json(qf) else list()
  log_msg("Fase 2: ", nrow(ev), " casos clasificados leídos de ", CASOS_DIR, "/ (sin bases DEIS/GRD en este equipo)")
  list(eventos = ev, universo = u, totales = t, qc = qc)
}
