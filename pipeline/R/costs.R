# Valorización de egresos -------------------------------------------------------
#
# Valor GRD (principal, perspectiva FONASA): peso GRD × precio base del hospital según el clúster de la
# resolución de precios, con una estructura de precios constante (por defecto 2024, como en los notebooks),
# llevado con el IPC a pesos del mes de referencia (costos$referencia_precios; octubre de 2025 en los artículos).
# Hospitales fuera del listado reciben el precio único (U) en estructuras desde 2023.
# Valor nominal deflactado (sensibilidad): peso × precio del propio año presupuestario × deflactor IPC.
# No existe para 2019 (el pago GRD por Programa 05 parte con el presupuesto 2020).
# Costeo alternativo por arancel MAI (opcional): crosswalk CIE-9-MC → código MAI.
# Pérdida de productividad (opcional): días de estada × ingreso diario × tasa de ocupación, entre la edad
# laboral mínima y la de retiro (capital humano, solo la estada).

price_lookup <- function(hosp, estructura, dicts) {
  cl <- dicts$clusters[dicts$clusters$anio_precio == estructura]
  pr <- dicts$precios[dicts$precios$anio_precio == estructura]
  if (!nrow(pr)) return(rep(NA_real_, length(hosp)))
  grp <- cl$grupo[match(hosp, cl$cod_hospital)]
  if (estructura >= 2023) grp[is.na(grp) & !is.na(hosp)] <- "U"
  pr$precio_base[match(grp, pr$grupo)]
}

# Años de eventos posteriores al último parámetro cargado (p. ej. GRD 2025 con productividad hasta 2024) usan el
# último año disponible: el de los artículos, hasta que se cargue el nuevo. Igual hacia atrás.
anio_parametro <- function(anio, disponibles, que) {
  disponibles <- sort(unique(stats::na.omit(disponibles)))
  if (!length(disponibles)) return(anio)
  out <- pmin(pmax(anio, min(disponibles)), max(disponibles))
  arr <- sort(unique(anio[!is.na(anio) & !anio %in% disponibles]))
  if (length(arr)) log_msg("Aviso: ", que, " sin datos para ", paste(arr, collapse = ", "),
                           "; se usa el año disponible más cercano (", paste(range(disponibles), collapse = "-"), ")")
  out
}

add_costs <- function(eventos, dicts, ipc, mai, prod, params) {
  if (is.null(eventos) || !nrow(eventos)) return(eventos)
  eventos[, `:=`(valor_grd = NA_real_, valor_grd_nominal_real = NA_real_, costo_mai = NA_real_,
                 costo_indirecto = NA_real_)]
  grd <- which(!is.na(eventos$grd_peso))
  est <- params$costos$estructura_precios_grd %||% 2024
  if (length(grd) && nrow(dicts$precios)) {
    # Factor IPC de la estructura de precios al mes de referencia (1 si no hay IPC o referencia).
    f_est <- 1
    if (!is.null(ipc) && !is.null(params$costos$referencia_precios)) {
      f_est <- ipc$deflactor[match(est, ipc$anio)]
      if (is.na(f_est)) stop("El IPC no tiene el año de la estructura de precios GRD (", est, ").")
    } else log_msg("Aviso: valor GRD sin actualizar por IPC (falta el IPC o costos$referencia_precios).")
    eventos[grd, valor_grd := grd_peso * price_lookup(cod_hospital, est, dicts) * f_est]
    if (!is.null(ipc)) {
      for (y in intersect(unique(eventos$anio[grd]), unique(dicts$precios$anio_precio))) {
        idx <- grd[eventos$anio[grd] == y]
        defl <- ipc$deflactor[match(anio_parametro(y, ipc$anio, "IPC"), ipc$anio)]
        eventos[idx, valor_grd_nominal_real := grd_peso * price_lookup(cod_hospital, y, dicts) * defl]
      }
    }
  }

  if (!is.null(mai) && nrow(dicts$crosswalk) && "procedimientos_txt" %in% names(eventos)) {
    eventos[, fila_id := .I]
    parts <- strsplit(eventos$procedimientos_txt, "|", fixed = TRUE)
    long <- data.table::data.table(fila_id = rep(eventos$fila_id, lengths(parts)),
                                   cie9mc_norm = normalize_cie(unlist(parts)))[!is.na(cie9mc_norm)]
    long <- merge(long, dicts$crosswalk[, .(cie9mc_norm, codigo_mai, cantidad)], by = "cie9mc_norm")
    if (nrow(long)) {
      long[, anio := eventos$anio[fila_id]]
      ult <- mai[, .SD[which.max(anio)], by = codigo_mai]
      long <- merge(long, ult[, .(codigo_mai, valor, anio_mai = anio)], by = "codigo_mai", all.x = TRUE)
      long[, anio_ipc := anio_parametro(anio_mai, ipc$anio, "IPC")]
      long <- merge(long, ipc[, .(anio_ipc = anio, defl = deflactor)], by = "anio_ipc", all.x = TRUE)
      tot <- long[, .(costo = sum(valor * cantidad * data.table::fifelse(is.na(defl), 1, defl), na.rm = TRUE)),
                  by = fila_id]
      eventos[tot, costo_mai := i.costo, on = "fila_id"]
    }
    eventos[, fila_id := NULL]
  }

  if (isTRUE(params$costos$incluir_costos_indirectos) && !is.null(prod)) {
    ret <- params$costos$edad_retiro
    eventos[, fila_id := .I]
    e <- eventos[!is.na(edad_min) & edad_min == edad_max & !is.na(dias_estada) & sexo %in% c("Hombre", "Mujer"),
                 .(fila_id, anio, region, sexo, edad = edad_min, dias_estada)]
    e <- e[edad >= (params$costos$edad_laboral_min %||% 15) &
             edad < ifelse(sexo == "Hombre", ret$Hombre %||% 65, ret$Mujer %||% 60)]
    e[, anio_evento := anio]
    e[, anio := anio_parametro(anio, prod$anio, "Productividad (ingresos y ocupación)")]
    m <- prod[e, on = .(anio, region, sexo, edad_min <= edad, edad_max >= edad),
              .(fila_id = i.fila_id, anio_par = i.anio, dias = i.dias_estada, v = x.ingreso_diario * x.tasa_ocupacion),
              nomatch = 0L]
    if (nrow(m)) {
      if (!is.null(ipc)) {
        # El ingreso está en pesos del año del parámetro: se deflacta con el IPC de ese año.
        m[, anio_ipc := anio_parametro(anio_par, ipc$anio, "IPC")]
        m <- merge(m, ipc[, .(anio_ipc = anio, defl = deflactor)], by = "anio_ipc", all.x = TRUE)
      } else m[, defl := 1]
      eventos[m, costo_indirecto := i.dias * i.v * data.table::fifelse(is.na(i.defl), 1, i.defl), on = "fila_id"]
    }
    eventos[, fila_id := NULL]
  }
  eventos[]
}
