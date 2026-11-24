# Clasificación de egresos en módulos, subgrupos y mecanismos -------------------

# Coincidencia por prefijo con el diccionario: para cada código, la fila más específica del diccionario.
# Devuelve el índice de fila de `dict` (NA si no coincide).
match_prefix <- function(codes, dict_codes) {
  out <- rep(NA_integer_, length(codes))
  if (!length(dict_codes) || !length(codes)) return(out)
  lens <- sort(unique(nchar(dict_codes)), decreasing = TRUE)
  for (L in lens) {
    idx <- which(nchar(dict_codes) == L)
    pend <- is.na(out) & !is.na(codes) & nchar(codes) >= L
    if (!any(pend)) next
    m <- match(substr(codes[pend], 1, L), dict_codes[idx])
    out[which(pend)[!is.na(m)]] <- idx[m[!is.na(m)]]
  }
  out
}

# "S00-S99" o "T00-T14": compara los tres primeros caracteres.
in_ranges <- function(codes, ranges) {
  c3 <- substr(codes, 1, 3)
  out <- rep(FALSE, length(codes))
  for (r in ranges) {
    b <- strsplit(r, "-", fixed = TRUE)[[1]]
    hi <- if (length(b) > 1) b[2] else b[1]
    out <- out | (!is.na(c3) & c3 >= b[1] & c3 <= hi)
  }
  out
}

# Mecanismo: primer código V-Y en las posiciones secundarias (DEIS: DIAG2; GRD: todas).
# - solo_etiologicos = TRUE (notebook GRD 2019-2025): solo rangos etiológicos; se sigue buscando en las
#   posiciones siguientes si el código es una complicación de la atención o un factor suplementario.
# - solo_etiologicos = FALSE (final_final.R, artículo DEIS): todos los rangos, incluidas Y40-Y84 e Y90-Y99.
# Sin código V-Y clasificable → `sin_dato`.
classify_mechanism <- function(dx_list, ce, columna = "mecanismo", solo_etiologicos = TRUE,
                               sin_dato = "Sin causa externa registrada") {
  n <- length(dx_list[[1]])
  out <- rep(NA_character_, n)
  if (length(dx_list) < 2) return(rep(sin_dato, n))
  ce_e <- if (solo_etiologicos) ce[etiologico == TRUE] else ce
  ce_e <- ce_e[!is.na(ce_e[[columna]]) & nzchar(ce_e[[columna]])]
  for (pos in seq(2, length(dx_list))) {
    code <- dx_list[[pos]]
    pend <- is.na(out) & !is.na(code) & grepl("^[V-Y][0-9]{2}", code)
    if (!any(pend)) next
    c3 <- substr(code[pend], 1, 3)
    val <- rep(NA_character_, length(c3))
    for (i in seq_len(nrow(ce_e))) {
      hit <- is.na(val) & c3 >= ce_e$desde[i] & c3 <= ce_e$hasta[i]
      val[hit] <- ce_e[[columna]][i]
    }
    # Con todos los rangos, el primer V-Y decide aunque no calce con ninguno (queda sin dato).
    if (!solo_etiologicos) val[is.na(val)] <- sin_dato
    out[which(pend)] <- val
  }
  out[is.na(out)] <- sin_dato
  out
}

# Configuración del mecanismo por fuente (config/pipeline.yml → clasificacion: mecanismo).
mechanism_cfg <- function(params, id) {
  m <- params$clasificacion$mecanismo
  if (is.character(m)) return(list(columna = m))
  m[[id]] %||% list(columna = "mecanismo")
}

# Clasifica los egresos de una fuente en un módulo. Devuelve data.table(idx, subgrupo, codigo_dict,
# en_universo) para los casos, y un vector lógico con el universo de referencia.
SUFIJO_SECUNDARIO <- " (secundario con cirugía)"

classify_module <- function(h, mod, fuente, dicts, cfg) {
  ev <- h$ev
  n <- nrow(ev)
  d <- dicts$dx[modulo == mod$id & incluir == TRUE & !is.na(codigo_norm)]
  principales <- unlist(mod_get(mod, "subgrupos_principales", fuente)) %||% unique(d$subgrupo)
  d[, prioridad := ifelse(subgrupo %in% principales, 1L, 2L)]

  # Filtros comunes: actividad (GRD; por defecto solo hospitalización) y rango de edad del módulo.
  actividades <- unlist(mod_get(mod, "actividades", fuente)) %||% "Hospitalización"
  base <- if ("actividad" %in% names(ev)) ev$actividad %in% actividades else ev$hospitalizacion %in% TRUE
  # El rango de edad puede ser común o por fuente (p. ej. fisuras: DEIS < 1 año, GRD 0-4 años por el TRAMO_EDAD 2025).
  edad <- mod_get(mod, "edad", fuente)
  if (!is.null(edad)) {
    base <- base & !is.na(ev$edad_min) & ev$edad_min >= (edad$min %||% 0) & ev$edad_max <= (edad$max %||% 150)
  }
  universo_def <- mod$universo[[fuente]]
  universo <- if (!is.null(universo_def)) base & in_ranges(ev$diag1, unlist(universo_def)) else NULL

  # Procedimientos del módulo (para códigos que los exigen).
  tiene_proc <- rep(FALSE, n); n_proc <- rep(0L, n)
  pdict <- dicts$proc[modulo == mod$id & incluir == TRUE]
  if (!is.null(h$proc) && nrow(pdict)) {
    for (col in h$proc) {
      hit <- !is.na(match_prefix(col, pdict$codigo_norm))
      n_proc <- n_proc + hit
    }
    tiene_proc <- n_proc > 0
  }

  # Exclusión por procedimiento (excluir_si_proc): para cada fila del diccionario que la define, marca los
  # egresos con algún procedimiento que empiece por esos prefijos.
  excluye <- NULL
  if (!is.null(h$proc) && any(!is.na(d$excluir_si_proc) & nzchar(d$excluir_si_proc))) {
    filas <- which(!is.na(d$excluir_si_proc) & nzchar(d$excluir_si_proc))
    excluye <- matrix(FALSE, n, length(filas))
    for (j in seq_along(filas)) {
      pref <- normalize_cie(trimws(strsplit(d$excluir_si_proc[filas[j]], ";", fixed = TRUE)[[1]]))
      pat <- paste0("^(", paste(pref, collapse = "|"), ")")
      for (col in h$proc) excluye[, j] <- excluye[, j] | (!is.na(col) & grepl(pat, col))
    }
    colnames(excluye) <- filas
  }
  excluido_por_proc <- function(r, ok) {
    if (is.null(excluye)) return(rep(FALSE, length(r)))
    out <- rep(FALSE, length(r))
    for (f in colnames(excluye)) {
      i <- which(ok & r == as.integer(f))
      out[i] <- excluye[i, f]
    }
    out
  }

  posicion <- mod$posicion[[fuente]] %||% "dx1"
  cols <- if (posicion == "cualquiera") h$dx else h$dx[1]
  best_row <- rep(NA_integer_, n); best_pri <- rep(9L, n)
  for (col in cols) {
    r <- match_prefix(col, d$codigo_norm)
    ok <- !is.na(r)
    ok[ok] <- !d$requiere_proc[r[ok]] | tiene_proc[ok]
    ok[ok] <- !excluido_por_proc(r, ok)[ok]
    pri <- ifelse(ok, d$prioridad[r], 9L)
    better <- ok & pri < best_pri          # gana la prioridad; a igual prioridad, la primera posición
    best_row[better] <- r[better]; best_pri[better] <- pri[better]
  }
  # Código del módulo en posición secundaria, aceptado solo con procedimiento del módulo (notebook de
  # craneosinostosis GRD: "Q75.0 secundario con cirugía"). Queda como subgrupo propio.
  secundario <- rep(FALSE, n)
  sec <- mod_get(mod, "secundario_con_proc", fuente)
  if ((isTRUE(sec) || is.list(sec)) && posicion == "dx1" && length(h$dx) > 1) {
    # Opcional: solo ciertos subgrupos en posición secundaria y solo con DX1 de ciertas categorías
    # (notebook v35: Q75.0 secundario con DX1 craneofacial Q67/Q75/Q87).
    filas_ok <- if (is.list(sec) && length(sec$subgrupos)) d$subgrupo %in% unlist(sec$subgrupos) else rep(TRUE, nrow(d))
    dx1_ok <- if (is.list(sec) && length(sec$dx1_permitido)) {
      !is.na(h$dx[[1]]) & substr(h$dx[[1]], 1, 3) %in% normalize_cie(unlist(sec$dx1_permitido))
    } else rep(TRUE, n)
    for (col in h$dx[-1]) {
      r <- match_prefix(col, d$codigo_norm)
      r[!is.na(r) & !filas_ok[r]] <- NA
      ok <- is.na(best_row) & !is.na(r) & tiene_proc & dx1_ok
      best_row[ok] <- r[ok]; secundario[ok] <- TRUE
    }
  }
  es_caso <- !is.na(best_row) & base
  if (!is.null(universo)) es_caso <- es_caso & universo

  idx <- which(es_caso)
  sg <- d$subgrupo[best_row[idx]]
  sg[secundario[idx]] <- paste0(sg[secundario[idx]], SUFIJO_SECUNDARIO)
  casos <- data.table::data.table(idx = idx, subgrupo = sg,
                                  codigo_dict = d$codigo[best_row[idx]], n_proc = n_proc[idx])

  # Aislado: sin otras lesiones S00-T79 fuera de los códigos principales del módulo (como facial_aislado;
  # solo cuando se leen todas las posiciones diagnósticas).
  if (isTRUE(mod$aislado) && posicion == "cualquiera" && length(idx)) {
    otras <- rep(FALSE, length(idx))
    for (col in h$dx) {
      cc <- col[idx]
      lesion <- !is.na(cc) & grepl("^(S[0-9]{2}|T[0-7][0-9])", cc)
      otras <- otras | (lesion & is.na(match_prefix(cc, d[prioridad == 1L]$codigo_norm)))
    }
    casos[, aislado := !otras]
  } else {
    casos[, aislado := NA]
  }
  list(casos = casos, universo = universo)
}
