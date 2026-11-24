# Armonización: columnas, edad, geografía, desenlaces ---------------------------

# Selecciona y renombra columnas según el mapa. Campos con "patron" devuelven todas las columnas
# coincidentes, en orden numérico, como lista. Devuelve list(std = data.table, multi = list, faltantes).
apply_column_map <- function(dt, map) {
  orig <- names(dt)
  data.table::setnames(dt, normalize_names(orig))
  out <- list(); multi <- list(); missing <- character()
  for (campo in setdiff(names(map), c("separador", "formato", "saltar_filas"))) {
    spec <- map[[campo]]
    if (is.list(spec) && !is.null(spec$patron)) {
      cols <- grep(spec$patron, names(dt), value = TRUE)
      if (!length(cols)) { missing <- c(missing, campo); next }
      ord <- suppressWarnings(as.integer(gsub("\\D", "", cols)))
      multi[[campo]] <- dt[, cols[order(ord)], with = FALSE]
      next
    }
    hit <- intersect(normalize_names(unlist(spec)), names(dt))
    if (length(hit)) out[[campo]] <- dt[[hit[1]]] else missing <- c(missing, campo)
  }
  n <- nrow(dt)
  std <- if (length(out)) data.table::as.data.table(out) else data.table::data.table(.n = seq_len(n))
  list(std = std, multi = multi, faltantes = missing, n = n)
}

# Valores exactos; los que empiezan con "~" se buscan como texto contenido (p. ej. "~FONASA" reconoce
# "FONASA INSTITUCIONAL - (MAI) B" del GRD). Las coincidencias exactas tienen prioridad.
recode_values <- function(x, table) {
  if (is.null(table)) return(x)
  xn <- normalize_value(x)
  out <- rep(NA_character_, length(x))
  for (cat in names(table)) {
    v <- unlist(table[[cat]])
    sub_ <- startsWith(v, "~")
    if (any(sub_)) {
      pat <- paste(normalize_value(substring(v[sub_], 2)), collapse = "|")
      out[is.na(out) & !is.na(xn) & grepl(pat, xn)] <- cat
    }
  }
  for (cat in names(table)) out[xn %in% normalize_value(unlist(table[[cat]]))] <- cat
  bad <- is.na(out) & !is.na(x) & nzchar(xn)
  out[bad] <- paste0("?", xn[bad])
  out
}

derive_year <- function(anio, fecha, fallback = NA_integer_) {
  y <- suppressWarnings(as.integer(substr(gsub("[^0-9]", "", anio), 1, 4)))
  y[!is.na(y) & (y < 1900 | y > 2100)] <- NA_integer_
  ok <- !is.na(fecha) & grepl("(19|20)[0-9]{2}", fecha)
  from_date <- rep(NA_integer_, length(fecha))
  from_date[ok] <- suppressWarnings(as.integer(sub(".*((19|20)[0-9]{2}).*", "\\1", fecha[ok])))
  y[is.na(y)] <- from_date[is.na(y)]
  y[is.na(y)] <- fallback
  y
}

# Fechas con varios formatos: elige el que resuelve más valores (como parse_fecha de los notebooks).
parse_date_any <- function(x) {
  s <- substr(trimws(as.character(x)), 1, 10)
  best <- NULL; best_ok <- -1
  for (fmt in c("%Y-%m-%d", "%d-%m-%Y", "%d/%m/%Y", "%Y/%m/%d", "%m/%d/%Y", "%Y%m%d")) {
    d <- as.Date(s, format = fmt)
    # as.Date ignora caracteres sobrantes ("15-06-1960" se leería como año 15): se exige un año plausible.
    y <- as.integer(format(d, "%Y"))
    d[!is.na(y) & (y < 1900 | y > 2100)] <- NA
    ok <- mean(!is.na(d[!is.na(s)]))
    if (is.nan(ok)) ok <- 0
    if (ok > best_ok) { best <- d; best_ok <- ok }
    if (ok > 0.99) break
  }
  best
}

# Edad exacta en años cumplidos entre nacimiento e ingreso.
age_from_dates <- function(birth, ref) {
  b <- parse_date_any(birth); r <- parse_date_any(ref)
  bl <- as.POSIXlt(b); rl <- as.POSIXlt(r)
  age <- rl$year - bl$year - as.integer(rl$mon < bl$mon | (rl$mon == bl$mon & rl$mday < bl$mday))
  age[is.na(b) | is.na(r)] <- NA
  as.numeric(age)
}

# Etiqueta de edad agrupada (GRUPO_EDAD del DEIS o TRAMO_EDAD del GRD) → límites en años cumplidos.
# Equivalente a parse_age_label / parse_tramo de los notebooks.
parse_age_label <- function(label) {
  # DEIS 2025 trae caracteres dañados ("menor de un a�o", "90 y m�s"), o "ï¿½" si se leyó como Latin-1.
  # Se quita el carácter dañado y se reconstruyen "AÑO"/"MÁS" (también si la transliteración dejó "?").
  label <- gsub("�|ï¿½|\\?", "", as.character(label))
  s <- normalize_value(label)
  s <- gsub("_", " ", s)
  s <- gsub("(^| )AO(S?)( |$)", "\\1ANO\\2\\3", s)
  s <- gsub("(^| )MS$", "\\1MAS", s)
  lo <- rep(NA_real_, length(s)); hi <- rep(NA_real_, length(s))
  u <- unique(s[!is.na(s) & nzchar(s)])
  res <- lapply(u, function(z) {
    if (z %in% c("NULL", "NA", "IGNORADO", "DESCONOCIDO")) return(c(NA, NA))
    nums <- as.numeric(regmatches(z, gregexpr("[0-9]+", z))[[1]])
    meses_dias <- grepl("MES|DIA", z)
    if (grepl("MENOS DE (1|UN) ANO|MENOR(ES)? (DE|A) (1|UN) ANO", z)) return(c(0, 0))
    if (meses_dias && !grepl("ANO", z)) return(c(0, 0))
    if (grepl("MENOR|MENOS", z) && length(nums) == 1) return(c(0, max(nums - 1, 0)))
    if (grepl("Y MAS|MAS$|MAYOR|\\+", z) && length(nums) >= 1) return(c(nums[1], 150))
    if (length(nums) >= 2) return(c(nums[1], nums[2]))
    if (length(nums) == 1) return(c(nums[1], nums[1]))
    c(NA, NA)
  })
  names(res) <- u
  m <- match(s, u)
  ok <- !is.na(m)
  lo[ok] <- vapply(res[m[ok]], `[`, numeric(1), 1)
  hi[ok] <- vapply(res[m[ok]], `[`, numeric(1), 2)
  list(lo = lo, hi = hi)
}

# Grupo quinquenal de estandarización. Con edad agrupada se usa el límite inferior, siempre que el
# tramo no cruce dos grupos (p. ej. "1 a 4" → 0-4; "5 a 14" queda sin grupo).
age_group_5 <- function(lo, hi = lo, abierto = 80) {
  g <- pmin(floor(lo / 5) * 5, abierto)
  g_hi <- pmin(floor(pmin(hi, 149) / 5) * 5, abierto)
  out <- ifelse(is.na(g), NA_character_, ifelse(g >= abierto, paste0(abierto, "+"), paste0(g, "-", g + 4)))
  out[!is.na(g_hi) & g_hi != g] <- NA_character_
  out
}

age_group_app <- function(lo, hi, grupos) {
  out <- rep(NA_character_, length(lo))
  for (g in grupos) out[!is.na(lo) & lo >= g$min & !is.na(hi) & pmin(hi, 150) <= g$max] <- g$etiqueta
  out
}

# Grupo de edad por límite inferior del tramo (age_min de los scripts de los artículos): "10 a 19" del DEIS
# cae en el grupo que contiene 10. Con edad exacta (GRD) equivale a la edad. Salvaguarda: si menos de la mitad
# de los años del tramo cae en ese grupo (p. ej. GRD 2025 "0 - 4 años" frente al grupo "<1"), queda sin grupo.
age_group_lower <- function(lo, grupos, hi = lo) {
  out <- rep(NA_character_, length(lo))
  hi <- pmin(ifelse(is.na(hi), lo, hi), 150)
  for (g in grupos) {
    en <- !is.na(lo) & lo >= g$min & lo <= g$max
    dentro <- pmin(hi, g$max) - lo + 1
    total <- hi - lo + 1
    # Tramos abiertos (80+, 90 y más) se tratan como dentro del grupo que contiene su límite inferior.
    abierto <- hi >= 150
    out[en & (abierto | dentro >= total / 2)] <- g$etiqueta
  }
  out
}

# Comuna de residencia → código CUT actual. Acepta códigos (incluidos los anteriores a Arica y Parinacota,
# Los Ríos y Ñuble) y nombres (con alias). Devuelve list(codigo, fuente).
harmonize_commune <- function(x, dicts) {
  com <- dicts$comunas
  z <- trimws(as.character(x))
  z <- sub("\\.0+$", "", z)
  out <- rep(NA_character_, length(z)); src <- rep("sin dato", length(z))
  vacio <- is.na(z) | z %in% c("", "*", "NULL", "NA", "99", "99999", "88888", "0", "00000")
  num <- !vacio & grepl("^[0-9]+$", z)
  cod <- ifelse(num, formatC(suppressWarnings(as.integer(z)), width = 5, flag = "0"), NA_character_)
  leg <- match(cod, dicts$comunas_legacy$codigo_antiguo)
  out[!is.na(leg)] <- dicts$comunas_legacy$codigo_actual[leg[!is.na(leg)]]; src[!is.na(leg)] <- "código antiguo"
  cur <- is.na(out) & cod %in% com$codigo_comuna
  out[cur] <- cod[cur]; src[cur] <- "código"
  src[num & is.na(out)] <- "código no reconocido"
  txt <- !vacio & !num
  key <- gsub(" ", "", normalize_value(z[txt]))
  ali <- match(key, dicts$comunas_alias$alias)
  key[!is.na(ali)] <- dicts$comunas_alias$nombre_normalizado[ali[!is.na(ali)]]
  hit <- match(key, com$clave_nombre)
  idx <- which(txt)
  out[idx[!is.na(hit)]] <- com$codigo_comuna[hit[!is.na(hit)]]
  src[idx[!is.na(hit)]] <- "nombre"
  src[idx[is.na(hit)]] <- ifelse(grepl("EXTRANJ|IGNORAD|DESCONOC", key[is.na(hit)]), "extranjero/ignorado",
                                 "nombre no reconocido")
  # Nombres con carácter dañado (U+FFFD o "?", p. ej. "PE\uFFFDALOL\uFFFDN"): cada carácter dañado vale por una
  # letra; se acepta solo si calza con una única comuna.
  dan <- idx[is.na(hit) & grepl("\ufffd|\\?", z[txt])]
  if (length(dan)) {
    u <- unique(z[dan])
    pat <- gsub(" ", "", normalize_value(gsub("\ufffd|\\?", "QZQ", u)))
    pat <- paste0("^", gsub("QZQ", ".", pat, fixed = TRUE), "$")
    cod_u <- vapply(pat, function(pp) { m <- which(grepl(pp, com$clave_nombre)); if (length(m) == 1) com$codigo_comuna[m] else NA_character_ }, "")
    cod <- cod_u[match(z[dan], u)]
    out[dan[!is.na(cod)]] <- cod[!is.na(cod)]; src[dan[!is.na(cod)]] <- "nombre con carácter dañado"
  }
  list(codigo = out, fuente = src)
}

region_from_recorded <- function(x, dicts) {
  num <- suppressWarnings(as.integer(sub("\\.0+$", "", trimws(as.character(x)))))
  out <- ifelse(!is.na(num) & num %in% 1:16, num, NA_integer_)
  txt <- is.na(out) & !is.na(x)
  key <- normalize_value(x[txt])
  r <- dicts$regiones
  for (i in seq_len(nrow(r))) {
    hit <- is.na(out[txt]) & grepl(r$claves[i], key)
    out[which(txt)[hit]] <- r$codigo_region[i]
  }
  as.integer(out)
}

# Región actual (16 regiones): desde la comuna; si no se resuelve, la región registrada, salvo en
# regiones madre antes de su división (regla del notebook de infecciones).
harmonize_region <- function(comuna_cod, region_raw, anio, dicts, params) {
  reg_com <- dicts$comunas$region[match(comuna_cod, dicts$comunas$codigo_comuna)]
  rec <- region_from_recorded(region_raw, dicts)
  ambig <- rep(FALSE, length(rec))
  for (r in params$geografia$region_respaldo_ambigua %||% list()) {
    ambig <- ambig | (!is.na(anio) & anio <= r$hasta_anio & rec %in% unlist(r$regiones))
  }
  fb <- ifelse(is.na(reg_com) & !ambig, rec, NA_integer_)
  list(region = as.integer(ifelse(is.na(reg_com), fb, reg_com)),
       fuente = ifelse(!is.na(reg_com), "comuna", ifelse(!is.na(fb), "región registrada", "sin resolver")))
}

contains_any <- function(x, tokens) {
  if (!length(tokens)) return(rep(FALSE, length(x)))
  pat <- paste(normalize_value(tokens), collapse = "|")
  !is.na(x) & grepl(pat, normalize_value(x))
}

# GRD: TIPOALTA en texto → TRUE (fallecido), FALSE (vivo), NA (no clasificable).
death_from_tipo_alta <- function(x, rc) {
  xn <- normalize_value(x)
  muerto <- contains_any(x, rc$tipo_alta_fallecido) | xn %in% unlist(rc$tipo_alta_fallecido_codigos)
  vivo <- contains_any(x, rc$tipo_alta_vivo) & !muerto
  out <- rep(NA, length(x)); out[muerto] <- TRUE; out[vivo] <- FALSE
  out
}

# Pasa una tabla cruda de egresos (DEIS o GRD) al formato estándar común.
harmonize_events <- function(raw, map, cfg, dicts, fuente, anio_archivo = NA_integer_) {
  cm <- apply_column_map(raw, map)
  std <- cm$std; n <- cm$n
  # Textos con tildes o Ñ mal codificados: se reparan antes de recodificar y se cuentan para el informe.
  calidad_texto <- list()
  for (k in intersect(c("sexo", "grupo_edad", "tramo_edad", "comuna", "provincia", "region", "sector", "prevision",
                        "condicion_egreso", "tipo_alta", "tipo_actividad", "intervencion"), names(std))) {
    if (!is.character(std[[k]])) next
    r <- repair_text(std[[k]])
    q <- attr(r, "calidad"); attr(r, "calidad") <- NULL
    data.table::set(std, j = k, value = r)
    if (sum(q) > 0) calidad_texto[[k]] <- as.list(q)
  }
  has <- function(k) k %in% names(std)
  g <- function(k) if (has(k)) as.character(std[[k]]) else rep(NA_character_, n)
  rc <- cfg$recode; p <- cfg$params

  fecha_ref <- if (has("fecha_ingreso")) g("fecha_ingreso") else rep(NA_character_, n)
  # GRD no trae año: se usa el año del archivo (GRD_PUBLICO_{año}); sin él, el año de la fecha de alta.
  anio <- if (has("anio")) derive_year(g("anio"), rep(NA_character_, n), anio_archivo)
          else if (!is.na(anio_archivo)) rep(as.integer(anio_archivo), n)
          else derive_year(rep(NA_character_, n), g("fecha_alta"), anio_archivo)

  # Edad: exacta (GRD con fecha de nacimiento) > edad numérica > etiqueta agrupada (DEIS, TRAMO_EDAD GRD).
  lo <- hi <- rep(NA_real_, n)
  if (has("fecha_nacimiento")) { lo <- hi <- age_from_dates(g("fecha_nacimiento"), fecha_ref) }
  if (has("edad")) {
    e <- as_num(g("edad")); fill <- is.na(lo) & !is.na(e) & e >= 0 & e <= 120
    lo[fill] <- hi[fill] <- floor(e[fill])
  }
  for (k in c("grupo_edad", "tramo_edad")) if (has(k)) {
    pa <- parse_age_label(g(k)); fill <- is.na(lo) & !is.na(pa$lo)
    lo[fill] <- pa$lo[fill]; hi[fill] <- pa$hi[fill]
  }
  lo[!is.na(lo) & (lo < 0 | lo > 120)] <- NA; hi[is.na(lo)] <- NA

  com <- harmonize_commune(g("comuna"), dicts)
  reg <- harmonize_region(com$codigo, g("region"), anio, dicts, p)
  ss <- dicts$comunas$codigo_servicio_salud[match(com$codigo, dicts$comunas$codigo_comuna)]
  # GRD 2025 ya no trae COMUNA (ni fecha de nacimiento) pero sí PROVINCIA de residencia: región desde la provincia
  # y Servicio de Salud cuando la provincia pertenece a uno solo.
  if (has("provincia") && !is.null(dicts$provincias) && nrow(dicts$provincias)) {
    pv <- g("provincia")
    k <- gsub(" ", "", normalize_value(pv))
    i <- match(k, dicts$provincias$clave)
    i[is.na(i)] <- match(sprintf("%03d", suppressWarnings(as.integer(pv[is.na(i)]))), dicts$provincias$codigo_provincia)
    usar <- is.na(reg$region) & !is.na(i)
    reg$region[usar] <- dicts$provincias$region[i[usar]]
    reg$fuente[usar] <- "provincia"
    usar_ss <- is.na(ss) & !is.na(i)
    ss[usar_ss] <- dicts$provincias$codigo_servicio_salud[i[usar_ss]]
  }

  out <- data.table::data.table(
    anio = anio,
    sexo = recode_values(g("sexo"), rc$sexo),
    edad_min = lo, edad_max = hi,
    grupo_edad_5 = age_group_5(lo, hi, p$edad$grupo_abierto %||% 80),
    comuna = com$codigo, comuna_fuente = com$fuente,
    region = reg$region, region_fuente = reg$fuente,
    servicio_salud = ss,
    sector = if (fuente == "grd") "Público" else recode_values(g("sector"), rc$sector),
    prevision = recode_values(g("prevision"), rc$prevision)
  )

  if (fuente == "grd") {
    dx <- cm$multi$diagnosticos
    out[, diag1 := if (!is.null(dx)) normalize_cie(dx[[1]]) else NA_character_]
    fi <- parse_date_any(fecha_ref); fa <- parse_date_any(g("fecha_alta"))
    out[, dias_estada := pmax(as.numeric(fa - fi), 0)]
    out[, fallecido := death_from_tipo_alta(g("tipo_alta"), rc)]
    act <- normalize_value(g("tipo_actividad"))
    out[, hospitalizacion := act %in% normalize_value(unlist(rc$actividad_hospitalizacion))]
    # Actividad: Hospitalización | CMA | NA (categorías extra de 2019 y valores no reconocidos).
    es_cma <- !out$hospitalizacion & contains_any(g("tipo_actividad"), unlist(rc$actividad_cma %||% c("CIRUGIA MAYOR AMBULATORIA", "CMA")))
    out[, actividad := data.table::fifelse(hospitalizacion, "Hospitalización", data.table::fifelse(es_cma, "CMA", NA_character_))]
    pab_num <- as_num(g("uso_pabellon")); pab_txt <- normalize_value(g("uso_pabellon"))
    pab <- (!is.na(pab_num) & pab_num > 0) | pab_txt %in% c("SI", "S", "YES", "TRUE")
    # USOSPABELLON vino vacío en 2020 y 2025: se usa la fecha de intervención como respaldo.
    if (!any(pab) && has("fecha_intervencion")) pab <- !is.na(g("fecha_intervencion")) & nzchar(g("fecha_intervencion"))
    out[, pabellon := pab]
    out[, cod_hospital := suppressWarnings(as.integer(g("cod_hospital")))]
    out[, grd_peso := as_num(g("grd_peso"))]
    out[, grd_severidad := suppressWarnings(as.integer(g("grd_severidad")))]
  } else {
    out[, diag1 := normalize_cie(g("diag1"))]
    out[, dias_estada := as_num(g("dias_estada"))]
    out[dias_estada < 0, dias_estada := NA]
    ce <- recode_values(g("condicion_egreso"), rc$condicion_egreso)
    out[, fallecido := ifelse(ce %in% "Fallecido", TRUE, ifelse(ce %in% "Vivo", FALSE, NA))]
    out[, hospitalizacion := TRUE]
    # El DEIS no registra la CMA de la red pública (GRD 2022-2023: el DEIS público ≈ solo la hospitalización GRD).
    out[, actividad := "Hospitalización"]
    iv <- recode_values(g("intervencion"), rc$intervencion)
    out[, pabellon := ifelse(iv %in% "Sí", TRUE, ifelse(iv %in% "No", FALSE, NA))]
    out[, `:=`(cod_hospital = NA_integer_, grd_peso = NA_real_, grd_severidad = NA_integer_)]
  }
  # Estadas > 365 días son inválidas para los análisis de estada (artículo de infecciones); el egreso se
  # mantiene en todo lo demás.
  out[!is.na(dias_estada) & dias_estada > (p$estada$max_valida %||% 365), dias_estada := NA]

  desde <- p$anios$desde %||% 1900; hasta <- p$anios$hasta %||% 2100
  keep <- is.na(out$anio) | (out$anio >= desde & out$anio <= hasta)
  codes <- if (fuente == "grd") {
    lapply(cm$multi$diagnosticos %||% list(), function(col) normalize_cie(col)[keep])
  } else {
    list(normalize_cie(g("diag1"))[keep], normalize_cie(g("diag2"))[keep])
  }
  procs <- if (!is.null(cm$multi$procedimientos)) lapply(cm$multi$procedimientos, function(col) normalize_cie(col)[keep]) else NULL
  # Campos alternativos: si uno del grupo está, no se informa la falta de los otros.
  alternativas <- list(c("comuna", "provincia", "region"), c("fecha_nacimiento", "tramo_edad", "edad", "grupo_edad"),
                       c("uso_pabellon", "fecha_intervencion"))
  falt <- cm$faltantes
  for (a in alternativas) if (any(vapply(a, has, TRUE))) falt <- setdiff(falt, a)
  list(ev = out[keep], dx = codes, proc = procs, faltantes = falt, calidad_texto = calidad_texto)
}
