# Validación: detiene el pipeline ante errores críticos y reporta advertencias ----

validate_outputs <- function(qc, cubo, pob, dicts, cfg) {
  errores <- character(); advert <- character()

  for (q in qc) {
    tag <- paste0(q$fuente, " ", q$version)
    sin_dx <- if (q$fuente == "grd") "diagnosticos" else "diag1"
    if (sin_dx %in% q$campos_faltantes) {
      errores <- c(errores, paste0(tag, ": no se encontró la columna de diagnóstico. Agrega su nombre en config/column_maps.yml"))
    }
    if (!length(q$anios)) errores <- c(errores, paste0(tag, ": no se pudo determinar el año"))
    opcionales <- c("anio", "edad", "grupo_edad", "tramo_edad", "fecha_nacimiento", "fecha_intervencion",
                    "servicio_salud_estab", "intervencion", "procedimientos", "prevision", "sector")
    otros <- setdiff(q$campos_faltantes, c(sin_dx, opcionales))
    if (length(otros)) advert <- c(advert, paste0(tag, ": columnas no encontradas: ", paste(otros, collapse = ", ")))
    for (k in names(q$valores_no_reconocidos)) {
      advert <- c(advert, paste0(tag, ": valores de ", k, " sin recodificar: ",
                                 paste(q$valores_no_reconocidos[[k]], collapse = ", ")))
    }
    if (isTRUE(q$pct_sin_region > 5)) advert <- c(advert, paste0(tag, ": ", q$pct_sin_region, "% sin región resuelta"))
    ajusta <- unlist(cfg$params$tasas$ajuste_edad_fuentes %||% "grd")
    if (q$fuente %in% ajusta && isTRUE(q$pct_sin_grupo_quinquenal > 5)) {
      advert <- c(advert, paste0(tag, ": ", q$pct_sin_grupo_quinquenal, "% sin grupo quinquenal (no entran en la tasa ajustada). ",
                                 "Tramos sin grupo: ", paste(unlist(q$etiquetas_edad_sin_grupo), collapse = ", ")))
    }
    vacios <- names(Filter(function(x) x == 0, q$casos_por_modulo))
    if (length(vacios)) advert <- c(advert, paste0(tag, ": sin casos en ", paste(vacios, collapse = ", ")))
  }

  dup <- dicts$dx[incluir == TRUE, .N, by = .(modulo, codigo_norm)][N > 1]
  if (nrow(dup)) advert <- c(advert, paste0("Códigos duplicados en el diccionario CIE-10: ", paste(dup$codigo_norm, collapse = ", ")))
  sin_desc <- dicts$dx[is.na(descripcion) | descripcion == ""]
  if (nrow(sin_desc)) advert <- c(advert, paste0("Códigos sin descripción: ", paste(sin_desc$codigo, collapse = ", ")))
  for (m in event_modules(cfg)) {
    faltan <- setdiff(sub(" \\(secundario con cirugía\\)$", "", unlist(m$subgrupos_principales)),
                      dicts$dx[modulo == m$id]$subgrupo)
    ag <- unlist(m$agrupaciones)
    if (length(ag)) faltan <- c(faltan, setdiff(ag, dicts$dx[modulo == m$id]$subgrupo))
    if (length(faltan)) errores <- c(errores, paste0("Módulo ", m$id, ": subgrupos principales sin códigos: ",
                                                    paste(faltan, collapse = ", ")))
  }

  if (nrow(cubo)) {
    if (is.null(pob)) {
      advert <- c(advert, "No hay población INE cargada: la app mostrará solo conteos")
    } else {
      faltan <- setdiff(unique(cubo$anio), unique(pob$anio))
      if (length(faltan)) advert <- c(advert, paste0("Años sin población INE (sin tasas): ", paste(sort(faltan), collapse = ", ")))
    }
  }

  rep <- list(fecha = format(Sys.time(), tz = "UTC", usetz = TRUE),
              errores = as.list(errores), advertencias = as.list(advert), fuentes = qc)
  for (e in errores) log_msg("ERROR: ", e)
  for (a in advert) log_msg("Aviso: ", a)
  rep
}
