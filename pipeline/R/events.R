# Procesamiento de bases de eventos (egresos DEIS y GRD) ------------------------
#
# Cada versión se procesa una vez y queda en data-cache/. Solo se reprocesa si cambia la versión
# registrada, el mapa de columnas, los diccionarios, los módulos, los parámetros o el código.

DIMENSIONES <- c("fuente", "modulo", "subgrupo", "anio", "region", "servicio_salud", "sexo", "grupo_edad",
                 "grupo_edad_5", "prevision", "sector", "mecanismo", "actividad")
DIM_UNIVERSO <- c("fuente", "modulo", "anio", "region", "servicio_salud", "sexo", "grupo_edad",
                  "grupo_edad_5", "prevision", "sector")

version_cache_key <- function(id, v, cfg, code_hash) {
  src <- cfg$sources[[id]]
  # Firma del archivo (tamaño y fecha): si se reemplaza por una versión corregida, se vuelve a procesar.
  firma <- if (!is.null(v$archivo) && nzchar(v$archivo)) {
    f <- if (grepl("^(/|[A-Za-z]:)", v$archivo)) v$archivo else file.path(cfg$root, v$archivo)
    if (file.exists(f)) c(file.size(f), as.numeric(file.mtime(f))) else NA
  } else NA
  hash_object(list(id, v, firma, cfg$colmaps[[src$mapa_columnas]], cfg$recode, cfg$params$anios, cfg$params$edad,
                   cfg$params$geografia, cfg$params$clasificacion$mecanismo[[id]], cfg$params$severidad,
                   modules_for_key(cfg, id), dictionaries_hash(cfg),
                   code_hash))
}

# Configuración de módulos resuelta para una fuente: un cambio solo del GRD no reprocesa el DEIS.
modules_for_key <- function(cfg, id) {
  lapply(modules_for_source(cfg, id), function(m) {
    m[c("nombre", "descripcion", "publicaciones", "estado", "metricas", "por", "agrupaciones")] <- NULL
    lapply(m, function(v) if (is.list(v) && !is.null(names(v)) && any(names(v) %in% c("egresos_deis", "grd"))) v[[id]] else v)
  })
}

modules_for_source <- function(cfg, id) {
  Filter(function(m) id %in% unlist(m$fuentes), event_modules(cfg))
}

# Procesa una versión. Devuelve casos clasificados (una fila por egreso y módulo), totales del universo
# de referencia, totales de egresos y control de calidad.
process_event_version <- function(id, v, cfg, dicts, code_hash, cache = "data-cache") {
  src <- cfg$sources[[id]]
  key <- version_cache_key(id, v, cfg, code_hash)
  cache_file <- file.path(cfg$root, cache, id, paste0(safe_name(v$version), "_", key, ".rds"))
  if (file.exists(cache_file)) {
    log_msg("Caché: ", id, " ", v$version)
    return(readRDS(cache_file))
  }
  log_msg("Procesando ", id, " ", v$version)
  map <- cfg$colmaps[[src$mapa_columnas]]
  raw <- read_version(id, v, cfg, map)
  n_leidos <- nrow(raw)
  raw_enc <- structure(list(), enc = attr(raw, "codificacion"))
  anio_archivo <- if (length(v$anios) == 1) as.integer(v$anios[[1]]) else NA_integer_
  fuente <- if (identical(src$mapa_columnas, "grd")) "grd" else "deis"
  h <- harmonize_events(raw, map, cfg, dicts, fuente, anio_archivo)
  rm(raw); invisible(gc(verbose = FALSE))
  ev <- h$ev

  mc <- mechanism_cfg(cfg$params, id)
  ev[, mecanismo := classify_mechanism(h$dx, dicts$ce, mc$columna %||% "mecanismo",
                                       solo_etiologicos = !isFALSE(mc$solo_etiologicos),
                                       sin_dato = mc$sin_dato %||% "Sin causa externa registrada")]

  # Totales por actividad: los módulos de solo hospitalización usan "Hospitalización"; los que suman CMA, ambas.
  totales <- ev[!is.na(actividad), .(n_total = .N), by = .(anio, region, sexo, grupo_edad_5, actividad)]
  totales[, fuente := id]

  casos_l <- list(); univ_l <- list(); qc_mod <- list()
  for (mod in modules_for_source(cfg, id)) {
    r <- build_module_cases(h, mod, id, dicts, cfg)
    if (!is.null(r$casos)) casos_l[[mod$id]] <- r$casos
    if (!is.null(r$universo)) univ_l[[mod$id]] <- r$universo
    qc_mod[[mod$id]] <- r$n
  }
  casos <- data.table::rbindlist(casos_l, use.names = TRUE, fill = TRUE)
  if (nrow(casos)) casos[, `:=`(fuente = id, version = v$version)]

  res <- list(
    eventos = casos,
    universo = data.table::rbindlist(univ_l, use.names = TRUE, fill = TRUE),
    totales = totales,
    qc = list(
      fuente = id, version = v$version, n_leidos = n_leidos, n_hospitalizacion = sum(ev$hospitalizacion %in% TRUE),
      casos_por_modulo = qc_mod,
      anios = sort(unique(stats::na.omit(ev$anio))),
      campos_faltantes = h$faltantes,
      valores_no_reconocidos = unrecognized_values(ev),
      pct_sin_region = round(100 * mean(is.na(ev$region)), 2),
      pct_region_por_comuna = round(100 * mean(ev$region_fuente == "comuna"), 2),
      region_fuente = as.list(table(ev$region_fuente)),
      comuna_fuente = as.list(table(ev$comuna_fuente)),
      pct_sin_edad = round(100 * mean(is.na(ev$edad_min)), 2),
      pct_sin_grupo_quinquenal = round(100 * mean(is.na(ev$grupo_edad_5)), 2),
      etiquetas_edad_sin_grupo = if (any(is.na(ev$grupo_edad_5) & !is.na(ev$edad_min)))
        utils::head(unique(paste0(ev$edad_min, "-", ev$edad_max)[is.na(ev$grupo_edad_5) & !is.na(ev$edad_min)]), 10)
        else list(),
      pct_fallecido_na = round(100 * mean(is.na(ev$fallecido)), 2),
      pct_sin_sexo = round(100 * mean(!ev$sexo %in% c("Hombre", "Mujer")), 2),
      pct_sin_estada = round(100 * mean(is.na(ev$dias_estada)), 2),
      textos_codificacion = h$calidad_texto,
      codificacion_archivo = attr(raw_enc, "enc") %||% NA_character_
    )
  )
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(dirname(cache_file), pattern = paste0("^", safe_name(v$version), "_.*\\.rds$"), full.names = TRUE))
  saveRDS(res, cache_file)
  res
}

# Casos de un módulo (una fila por egreso) y su universo de referencia. La usan la fase 1 completa y el
# parche por módulo (pipeline/parche_modulo.R), para que ambos clasifiquen igual.
build_module_cases <- function(h, mod, id, dicts, cfg) {
  ev <- h$ev
  cl <- classify_module(h, mod, id, dicts, cfg)
  grupos <- module_age_groups_cfg(mod, id, cfg$params)
  cs <- NULL; u <- NULL
  if (nrow(cl$casos)) {
    cs <- cbind(cl$casos[, .(subgrupo, codigo_dict, n_proc, aislado)], ev[cl$casos$idx])
    cs[, `:=`(modulo = mod$id, grupo_edad = age_group_lower(edad_min, grupos, edad_max))]
    if (identical(mod_get(mod, "severidad", id), "icdpic") && !is.null(dicts$icdpic)) {
      dxc <- lapply(h$dx, function(col) col[cl$casos$idx])
      princ <- unlist(mod_get(mod, "subgrupos_principales", id))
      cod_mod <- dicts$dx[modulo == mod$id & incluir == TRUE & (subgrupo %in% princ | !length(princ))]$codigo_norm
      s_tot <- icdpic_riss(dxc, dicts$icdpic)
      s_exc <- icdpic_riss(dxc, dicts$icdpic, excluir = cod_mod)
      cs[, `:=`(riss = s_tot$riss, maxais = s_tot$maxais, riss_excl = s_exc$riss)]
    }
    if (!is.null(h$proc)) {
      pr <- lapply(h$proc, function(col) data.table::fifelse(is.na(col[cl$casos$idx]), "", col[cl$casos$idx]))
      cs[, procedimientos_txt := do.call(paste, c(pr, sep = "|"))]
    }
  }
  if (!is.null(cl$universo)) {
    eu <- ev[cl$universo]
    eu[, grupo_edad := age_group_lower(edad_min, grupos, edad_max)]
    u <- eu[, .(n_universo = .N), by = setdiff(DIM_UNIVERSO, c("fuente", "modulo"))]
    u[, `:=`(fuente = id, modulo = mod$id)]
  }
  list(casos = cs, universo = u, n = nrow(cl$casos))
}

unrecognized_values <- function(ev) {
  out <- list()
  for (k in c("sexo", "sector", "prevision")) {
    v <- ev[[k]]
    bad <- unique(v[!is.na(v) & startsWith(v, "?")])
    if (length(bad)) out[[k]] <- utils::head(bad, 20)
  }
  out
}

process_event_source <- function(id, cfg, dicts, code_hash) {
  vers <- source_versions(cfg, id)
  if (!length(vers)) return(NULL)
  res <- lapply(vers, function(v) process_event_version(id, v, cfg, dicts, code_hash))
  bind <- function(k) data.table::rbindlist(lapply(res, `[[`, k), use.names = TRUE, fill = TRUE)
  list(eventos = bind("eventos"), universo = bind("universo"), totales = bind("totales"),
       qc = lapply(res, `[[`, "qc"))
}

# Suma que devuelve NA (no 0) cuando no hay ningún dato: distingue "sin información" de "cero".
sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# Cubo agregado: una fila por combinación de dimensiones con conteos y sumas.
build_cube <- function(eventos, modules_cfg) {
  if (is.null(eventos) || !nrow(eventos)) return(data.table::data.table())
  mec_mod <- vapply(modules_cfg, function(m) isTRUE(m$desagregar_mecanismo), logical(1))
  names(mec_mod) <- vapply(modules_cfg, `[[`, character(1), "id")
  ev <- data.table::copy(eventos)
  ev[!(modulo %in% names(mec_mod)[mec_mod]), mecanismo := NA_character_]
  for (k in c("valor_grd", "valor_grd_nominal_real", "costo_mai", "costo_indirecto", "riss", "riss_excl")) {
    if (!k %in% names(ev)) ev[, (k) := NA_real_]
  }
  ev[, .(
    n               = .N,
    n_con_estada    = sum(!is.na(dias_estada)),
    dias_estada     = sum(dias_estada, na.rm = TRUE),
    n_estada_5      = sum(dias_estada >= 5, na.rm = TRUE),
    n_estada_7      = sum(dias_estada >= 7, na.rm = TRUE),
    n_con_condicion = sum(!is.na(fallecido)),
    fallecidos      = sum(fallecido, na.rm = TRUE),
    n_con_pabellon  = sum(!is.na(pabellon)),
    n_pabellon      = sum(pabellon, na.rm = TRUE),
    n_con_aislado   = sum(!is.na(aislado)),
    n_aislado       = sum(aislado, na.rm = TRUE),
    n_con_peso      = sum(!is.na(grd_peso)),
    peso_grd        = sum_or_na(grd_peso),
    n_con_valor     = sum(!is.na(valor_grd) & valor_grd > 0),
    valor_grd       = sum_or_na(valor_grd[!is.na(valor_grd) & valor_grd > 0]),
    valor_grd_nominal_real = sum_or_na(valor_grd_nominal_real),
    costo_mai       = sum_or_na(costo_mai),
    costo_indirecto = sum_or_na(costo_indirecto),
    # Severidad ICDPIC-R (RISS derivado de CIE-10): suma, bandas del notebook y trauma mayor (RISS >= 16).
    n_con_riss      = sum(!is.na(riss)),
    riss            = sum_or_na(riss),
    n_riss_0_3      = sum(riss <= 3, na.rm = TRUE),
    n_riss_4_8      = sum(riss >= 4 & riss <= 8, na.rm = TRUE),
    n_riss_9_15     = sum(riss >= 9 & riss <= 15, na.rm = TRUE),
    n_riss_16_24    = sum(riss >= 16 & riss <= 24, na.rm = TRUE),
    n_riss_25       = sum(riss >= 25, na.rm = TRUE),
    n_trauma_mayor  = sum(riss >= 16, na.rm = TRUE),
    riss_excl       = sum_or_na(riss_excl),
    n_trauma_mayor_excl = sum(riss_excl >= 16, na.rm = TRUE)
  ), by = DIMENSIONES]
}

build_universe <- function(universo) {
  if (is.null(universo) || !nrow(universo)) return(data.table::data.table())
  universo[, .(n_universo = sum(n_universo)), by = DIM_UNIVERSO]
}

# Medianas de estada por módulo, subgrupo, fuente y año (no se recomponen desde el cubo).
build_los_summary <- function(eventos) {
  if (is.null(eventos) || !nrow(eventos)) return(data.table::data.table())
  eventos[!is.na(dias_estada), .(
    mediana = stats::median(dias_estada),
    p25 = stats::quantile(dias_estada, 0.25, names = FALSE),
    p75 = stats::quantile(dias_estada, 0.75, names = FALSE),
    n = .N
  ), by = .(fuente, modulo, subgrupo, anio)]
}

# Conteos por comuna de residencia (para mapas futuros), sin otras dimensiones.
build_commune_counts <- function(eventos) {
  if (is.null(eventos) || !nrow(eventos)) return(data.table::data.table())
  eventos[!is.na(comuna), .(n = .N), by = .(fuente, modulo, subgrupo, anio, comuna)]
}

# RISS: mediana e IQR por módulo, subgrupo, fuente y año (no se recomponen desde el cubo).
build_severity_summary <- function(eventos) {
  if (is.null(eventos) || !nrow(eventos) || !"riss" %in% names(eventos)) return(data.table::data.table())
  eventos[!is.na(riss), .(
    mediana = stats::median(riss),
    p25 = stats::quantile(riss, 0.25, names = FALSE),
    p75 = stats::quantile(riss, 0.75, names = FALSE),
    mediana_excl = stats::median(riss_excl, na.rm = TRUE),
    n = .N
  ), by = .(fuente, modulo, subgrupo, anio)]
}
