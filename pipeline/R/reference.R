# Tablas de referencia: población INE, nacidos vivos, IPC, aranceles, productividad, catálogos ----

ref_versions <- function(cfg, role) {
  ids <- sources_by_role(cfg, role)
  out <- list()
  for (id in ids) for (v in source_versions(cfg, id)) out[[length(out) + 1]] <- list(id = id, v = v)
  out
}

first_multi <- function(cm, k) if (!is.null(cm$multi[[k]])) as.character(cm$multi[[k]][[1]]) else NULL

# Población INE por comuna (base Censo 2017, formato ancho "Poblacion AAAA"), agregada a año, región,
# Servicio de Salud, sexo y grupos de edad. Suma áreas urbana y rural salvo que el archivo traiga el total.
process_population <- function(cfg, dicts) {
  vs <- ref_versions(cfg, "poblacion")
  if (!length(vs)) return(NULL)
  map <- cfg$colmaps$poblacion_ine
  parts <- lapply(seq_along(vs), function(i) {
    x <- vs[[i]]
    raw <- read_version(x$id, x$v, cfg)
    cm <- apply_column_map(raw, map)
    std <- cm$std
    wide <- cm$multi$ancho
    if (is.null(wide)) stop("El archivo de población no trae columnas 'Poblacion AAAA'")
    sexo <- first_multi(cm, "sexo") %||% std$sexo
    area <- first_multi(cm, "area")
    edad_raw <- as.character(std$edad)
    lo <- floor(as_num(edad_raw)); hi <- lo
    pa <- parse_age_label(edad_raw)
    fill <- is.na(lo); lo[fill] <- pa$lo[fill]; hi[fill] <- pa$hi[fill]
    com <- harmonize_commune(std$comuna, dicts)
    base <- data.table::data.table(comuna = com$codigo, sexo = recode_values(sexo, cfg$recode$sexo),
                                   edad_min = lo, edad_max = hi)
    if (!is.null(area)) {
      an <- normalize_value(area)
      tot <- grepl("TOTAL|AMBAS", an)
      if (any(tot)) { keep <- tot; base <- base[keep]; wide <- wide[keep] }
    }
    long <- cbind(base, wide)
    long <- data.table::melt(long, id.vars = names(base), variable.name = "col", value.name = "poblacion")
    long[, anio := as.integer(sub(".*((19|20)[0-9]{2})$", "\\1", col))]
    long[, `:=`(col = NULL, poblacion = as_num(poblacion))]
    long[!is.na(comuna) & sexo %in% c("Hombre", "Mujer") & !is.na(poblacion)][, .v := i]
  })
  pop <- data.table::rbindlist(parts)
  # Una nueva versión de proyecciones (p. ej. con base Censo 2024) reemplaza a la anterior en los años que cubre.
  pop <- pop[pop[, .I[.v == max(.v)], by = anio]$V1][, .v := NULL]
  pop <- pop[, .(poblacion = sum(poblacion)), by = .(anio, comuna, sexo, edad_min, edad_max)]
  pop[, `:=`(region = dicts$comunas$region[match(comuna, dicts$comunas$codigo_comuna)],
             servicio_salud = dicts$comunas$codigo_servicio_salud[match(comuna, dicts$comunas$codigo_comuna)])]
  ab <- cfg$params$edad$grupo_abierto %||% 80
  # Se conservan los tramos INE (edad_min, edad_max): la consulta los agrupa según los grupos de cada módulo.
  pop[, grupo_edad_5 := age_group_5(edad_min, edad_max, ab)]
  totals <- pop[, .(total = sum(poblacion)), by = anio]
  if (any(totals$total < 1e7 | totals$total > 3e7)) {
    log_msg("Aviso: totales INE fuera de 10-30 millones; revisar doble conteo de áreas: ",
            paste(totals$anio, round(totals$total), collapse = "; "))
  }
  edad0 <- pop[edad_min == 0 & edad_max == 0, .(poblacion0 = sum(poblacion)), by = .(anio, region, servicio_salud)]
  pob <- pop[, .(poblacion = sum(poblacion)), by = .(anio, region, servicio_salud, sexo, edad_min, edad_max, grupo_edad_5)]
  list(poblacion = pob, edad0 = edad0)
}

# Nacidos vivos regionales; calibra la población INE de 0 años de cada Servicio/región al total regional
# de nacidos vivos del mismo año (notebook de fisuras: DENOM_SOURCE = "ine_age0_calibrado").
process_births <- function(cfg, dicts, edad0) {
  vs <- ref_versions(cfg, "nacimientos")
  if (!length(vs) || is.null(edad0)) return(edad0)
  map <- cfg$colmaps$nacimientos
  # Archivos regionales (anio, region, nacidos_vivos) y nacionales (sin región, o región "0"/"Chile"): los años
  # con solo cifra nacional (p. ej. INE provisional 2024-2025) calibran todas las regiones con el factor nacional.
  nv <- data.table::rbindlist(lapply(seq_along(vs), function(i) {
    x <- vs[[i]]
    cm <- apply_column_map(read_version(x$id, x$v, cfg), map)
    reg_raw <- cm$std$region %||% rep(NA_character_, cm$n)
    nacional <- is.na(reg_raw) | normalize_value(reg_raw) %in% c("0", "00", "CHILE", "NACIONAL", "TOTAL", "PAIS")
    region <- region_from_recorded(reg_raw, dicts)
    region[nacional] <- NA
    data.table::data.table(anio = as.integer(cm$std$anio), nacional = nacional, region = region,
                           nacidos_vivos = as_num(cm$std$nacidos_vivos), .v = i)
  }))
  # Versiones que repiten un año (p. ej. cifra provisional reemplazada por la definitiva): manda la más reciente.
  nv <- nv[nv[, .I[.v == max(.v)], by = .(anio, nacional)]$V1]
  nac <- nv[nacional == TRUE, .(nv_nacional = sum(nacidos_vivos, na.rm = TRUE)), by = anio]
  nv <- nv[nacional == FALSE & !is.na(region), .(nacidos_vivos = sum(nacidos_vivos, na.rm = TRUE)), by = .(anio, region)]
  out <- data.table::copy(edad0)
  if (isTRUE(cfg$params$tasas$calibrar_edad0_a_nacidos_vivos)) {
    reg <- out[, .(ine_region = sum(poblacion0)), by = .(anio, region)]
    reg <- merge(reg, nv, by = c("anio", "region"), all.x = TRUE)
    reg[, factor_nv := nacidos_vivos / ine_region]
    if (nrow(nac)) {
      fn <- merge(reg[, .(ine_pais = sum(ine_region)), by = anio], nac, by = "anio")
      fn[, factor_pais := nv_nacional / ine_pais]
      reg <- merge(reg, fn[, .(anio, factor_pais)], by = "anio", all.x = TRUE)
      sin_reg <- reg[, .(sin = all(is.na(nacidos_vivos))), by = anio][sin == TRUE]$anio
      reg[anio %in% sin_reg & !is.na(factor_pais), factor_nv := factor_pais]
      reg[, factor_pais := NULL]
    }
    out <- merge(out, reg[, .(anio, region, factor_nv)], by = c("anio", "region"), all.x = TRUE)
    out[, nacidos_vivos := poblacion0 * factor_nv]
  } else {
    out[, `:=`(factor_nv = NA_real_, nacidos_vivos = NA_real_)]
  }
  out[]
}

excel_or_text_date <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  d <- as.Date(rep(NA_character_, length(x)))
  serial <- !is.na(n) & n > 20000 & n < 80000
  d[serial] <- as.Date(n[serial], origin = "1899-12-30")
  d[!serial] <- parse_date_any(x[!serial])
  d
}

# IPC: índice encadenado desde variaciones mensuales del BCCh (o índice en niveles) y promedio anual.
# Con costos$referencia_precios ("AAAA-MM"), deflactor[y] = índice[mes de referencia] / índice_promedio[y]: los
# precios de cada año (referidos al promedio de su año) quedan en pesos de ese mes, como en los artículos.
# Sin referencia, se usa el año base anterior: deflactor[y] = índice_promedio[base] / índice_promedio[y].
process_ipc <- function(cfg) {
  vs <- ref_versions(cfg, "ipc")
  if (!length(vs)) return(NULL)
  map <- cfg$colmaps$ipc
  x <- vs[[length(vs)]]
  files <- fetch_version(x$id, x$v, cfg$root)
  f <- files[1]
  ext <- tolower(tools::file_ext(f))
  if (ext %in% c("xls", "xlsx")) {
    raw <- suppressMessages(readxl::read_excel(f, sheet = x$v$hoja %||% 1, col_names = FALSE, col_types = "text",
                                               skip = as.integer(map$saltar_filas %||% 0)))
    d <- data.table::data.table(fecha = excel_or_text_date(raw[[1]]), valor = as_num(raw[[2]]))
  } else {
    cm <- apply_column_map(read_any(f), map)
    s <- cm$std
    fecha <- if (!is.null(s$anio) && !is.null(s$mes)) as.Date(sprintf("%s-%02d-01", s$anio, as.integer(as_num(s$mes))))
             else excel_or_text_date(s$mes)
    d <- data.table::data.table(fecha = fecha, valor = as_num(s$valor))
  }
  desde <- as.Date(cfg$params$costos$ipc_desde %||% "2018-01-01")
  d <- d[!is.na(fecha) & !is.na(valor) & fecha >= desde][order(fecha)]
  if (identical(map$formato, "variacion_mensual")) d[, indice := cumprod(1 + valor / 100)] else d[, indice := valor]
  anual <- d[, .(indice_prom = mean(indice), meses = .N), by = .(anio = as.integer(format(fecha, "%Y")))]
  ref <- cfg$params$costos$referencia_precios
  if (!is.null(ref) && nzchar(ref)) {
    mes_ref <- as.Date(paste0(substr(ref, 1, 7), "-01"))
    if (is.na(mes_ref)) stop("costos$referencia_precios debe tener la forma AAAA-MM (p. ej. 2025-10).")
    i_ref <- d[fecha == mes_ref]$indice
    if (!length(i_ref)) stop("El IPC cargado no llega a ", format(mes_ref, "%m-%Y"), ": carga una versión más reciente del BCCh.")
    anual[, deflactor := i_ref[1] / indice_prom]
    anual[, anio_base := format(mes_ref, "%Y-%m")]
    log_msg("IPC: precios llevados a pesos de ", format(mes_ref, "%Y-%m"), " (factor ",
            paste0(anual$anio, " = ", round(anual$deflactor, 4), collapse = "; "), ")")
  } else {
    base <- cfg$params$costos$anio_base_ipc %||% max(anual$anio)
    if (!base %in% anual$anio) stop("El IPC no tiene datos para el año base ", base)
    anual[, deflactor := anual[anio == base]$indice_prom / indice_prom]
    anual[, anio_base := base]
  }
  incompletos <- anual[meses < 12]$anio
  if (length(incompletos)) log_msg("Aviso: IPC con menos de 12 meses en ", paste(incompletos, collapse = ", "))
  anual[]
}

process_mai <- function(cfg) {
  vs <- ref_versions(cfg, "aranceles")
  if (!length(vs)) return(NULL)
  map <- cfg$colmaps$aranceles_mai
  data.table::rbindlist(lapply(vs, function(x) {
    cm <- apply_column_map(read_version(x$id, x$v, cfg), map)
    s <- cm$std
    codigo <- if (!is.null(s$grupo) && !is.null(s$subgrupo) && !is.null(s$prestacion)) {
      sprintf("%02d-%02d-%03d", as.integer(as_num(s$grupo)), as.integer(as_num(s$subgrupo)),
              as.integer(as_num(s$prestacion)))
    } else gsub("[^0-9A-Za-z-]", "", s$codigo)
    anio <- if (!is.null(s$anio)) as.integer(s$anio) else as.integer(x$v$anios[[1]] %||% NA)
    out <- data.table::data.table(anio = anio, codigo_mai = codigo, glosa = s$glosa %||% NA_character_,
                                  valor = as_num(gsub("[$ .]", "", s$valor)))
    unique(out[!is.na(valor) & valor > 0], by = c("anio", "codigo_mai"))
  }), fill = TRUE)
}

# Parámetros de productividad (ESI): ingreso diario × tasa de ocupación por año, región, sexo y tramo.
process_productividad <- function(cfg, dicts) {
  vs <- ref_versions(cfg, "productividad")
  if (!length(vs)) return(NULL)
  map <- cfg$colmaps$productividad
  out <- data.table::rbindlist(lapply(seq_along(vs), function(i) {
    x <- vs[[i]]
    s <- apply_column_map(read_version(x$id, x$v, cfg), map)$std
    tramo <- normalize_value(s$tramo)
    nums <- regmatches(tramo, gregexpr("[0-9]+", tramo))
    lo <- as.numeric(vapply(nums, function(z) z[1] %||% NA_character_, ""))
    hi <- as.numeric(vapply(nums, function(z) if (length(z) > 1) z[2] else NA_character_, ""))
    hi[grepl("GE|MAS|\\+", tramo)] <- 150
    to <- as_num(s$tasa_ocupacion); to[!is.na(to) & to > 1] <- to[!is.na(to) & to > 1] / 100
    data.table::data.table(anio = as.integer(s$anio), region = region_from_recorded(s$region, dicts),
                           sexo = recode_values(s$sexo, cfg$recode$sexo), edad_min = lo, edad_max = hi,
                           ingreso_diario = as_num(s$ingreso_diario), tasa_ocupacion = to, .v = i)
  }))
  if (!nrow(out) || all(is.na(out$ingreso_diario))) {
    log_msg("Aviso: productividad sin columnas reconocidas (anio, region, sexo, tramo_esi, ingreso_diario_medio, ",
            "tasa_ocupacion): no se calculan costos indirectos")
    return(NULL)
  }
  # Si dos versiones cubren el mismo año (p. ej. 2019-2024 y luego 2019-2025), manda la más reciente.
  out <- out[out[, .I[.v == max(.v)], by = anio]$V1]
  out[, .v := NULL]
  # Respaldo nacional (mediana por año, sexo y tramo) para regiones sin dato, como en el notebook GRD.
  nac <- out[!is.na(ingreso_diario), .(ingreso_diario = stats::median(ingreso_diario),
                                       tasa_ocupacion = stats::median(tasa_ocupacion, na.rm = TRUE)),
             by = .(anio, sexo, edad_min, edad_max)]
  falta <- data.table::CJ(anio = unique(out$anio), region = 1:16, sexo = unique(out$sexo),
                          tramo = unique(paste(out$edad_min, out$edad_max)))
  falta[, `:=`(edad_min = as.numeric(sub(" .*", "", tramo)), edad_max = as.numeric(sub(".* ", "", tramo)), tramo = NULL)]
  falta <- falta[!out, on = .(anio, region, sexo, edad_min, edad_max)]
  if (nrow(falta)) out <- rbind(out, merge(falta, nac, by = c("anio", "sexo", "edad_min", "edad_max")), use.names = TRUE)
  out[]
}

process_territorial <- function(cfg, dicts) {
  vs <- ref_versions(cfg, "territorial")
  if (!length(vs)) return(NULL)
  x <- vs[[length(vs)]]
  raw <- read_version(x$id, x$v, cfg)
  ss_col <- intersect(tolower(normalize_names(unlist(cfg$colmaps$territorial$servicio_salud))),
                      tolower(normalize_names(names(raw))))
  data.table::setnames(raw, tolower(normalize_names(names(raw))))
  if (!length(ss_col)) stop("La tabla territorial no trae el código de Servicio de Salud")
  raw[, servicio_salud := as.integer(as_num(get(ss_col[1])))]
  for (k in setdiff(names(raw), c("servicio_salud", ss_col[1]))) {
    v <- as_num(raw[[k]])
    if (mean(!is.na(v[!is.na(raw[[k]])])) > 0.9) data.table::set(raw, j = k, value = v)
  }
  raw[, `:=`(version_fuente = x$v$version)]
  merge(raw, dicts$servicios[, .(servicio_salud = codigo_servicio_salud, nombre_servicio_salud = nombre)],
        by = "servicio_salud", all.x = TRUE)
}

apply_official_catalog <- function(dict, cfg, role) {
  vs <- ref_versions(cfg, role)
  if (!length(vs)) return(dict)
  cat <- data.table::rbindlist(lapply(vs, function(x) {
    s <- apply_column_map(read_version(x$id, x$v, cfg), cfg$colmaps$catalogo)$std
    data.table::data.table(codigo_norm = normalize_cie(s$codigo), desc_oficial = s$descripcion)
  }))
  cat <- unique(cat[!is.na(codigo_norm)], by = "codigo_norm")
  out <- merge(dict, cat, by = "codigo_norm", all.x = TRUE, sort = FALSE)
  out[!is.na(desc_oficial), descripcion := desc_oficial]
  out[, desc_oficial := NULL]
  out[]
}
