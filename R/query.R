# Consultas: filtros, agregación, tasas y supresión -------------------------------

DIM_POBLACION <- c("anio", "region", "servicio_salud", "sexo", "grupo_edad", "grupo_edad_5")
DIM_EDAD0 <- c("anio", "region", "servicio_salud")
DIM_UNIVERSO <- c("anio", "region", "servicio_salud", "sexo", "grupo_edad", "grupo_edad_5", "prevision", "sector")
DIM_TOTALES <- c("anio", "region", "sexo")
MEDIDAS <- c("n", "n_con_estada", "dias_estada", "n_estada_5", "n_estada_7", "n_con_condicion", "fallecidos", "n_con_pabellon",
             "n_pabellon", "n_con_aislado", "n_aislado", "n_con_peso", "peso_grd", "n_con_valor", "valor_grd",
             "valor_grd_nominal_real", "costo_mai", "costo_indirecto", "n_con_riss", "riss", "n_riss_0_3", "n_riss_4_8",
             "n_riss_9_15", "n_riss_16_24", "n_riss_25", "n_trauma_mayor", "riss_excl", "n_trauma_mayor_excl")

#' Consulta el observatorio
#'
#' @param modulo Id del módulo (ver [cmf_modules()]).
#' @param fuente "egresos_deis" o "grd".
#' @param years Vector de años.
#' @param subgrupo Subgrupos del módulo. NULL = definición principal del módulo para esa fuente; "todos" = todos.
#' @param agrupacion Series publicadas que suman subgrupos (p. ej. "Tejidos duros"); ver [cmf_module()].
#'   También puede usarse en `by`.
#' @param region Códigos de región (1-16). NULL = todas.
#' @param servicio_salud Códigos de Servicio de Salud de residencia.
#' @param sexo "Hombre" y/o "Mujer". NULL = ambos.
#' @param grupo_edad Grupos etarios del módulo para esa fuente (ver [cmf_age_groups()]). NULL = todos.
#' @param prevision,sector,mecanismo Filtros adicionales (impiden las tasas poblacionales).
#' @param actividad "Hospitalización" y/o "CMA" (cirugía mayor ambulatoria; solo GRD y solo en los módulos que la
#'   incluyen, ver `actividades` en [cmf_module()]). NULL = las actividades del módulo. También puede usarse en `by`.
#' @param by Dimensiones de salida, p. ej. "anio", c("anio", "sexo"), "region", "subgrupo", "mecanismo".
#' @param metric "conteo"; "cruda" o "ajustada" (por 100.000 habitantes, ajustada por edad con OMS e IC gamma
#'   de Fay-Feuer; solo fuentes con edad quinquenal, por defecto GRD); "nacidos_vivos" (por 1.000 nacidos vivos,
#'   módulos de menores de 1 año); "universo" (por 1.000 egresos del universo de referencia del módulo, p. ej.
#'   egresos por trauma); "egresos" (por 100.000 egresos totales de la fuente); "ajustada_chile" (por 100.000,
#'   estandarizada a la distribución etaria chilena 2002-2025 con los grupos del módulo; sirve para el DEIS).
#' @param suppress Ocultar celdas con menos de `n_min` casos.
#' @param n_min Umbral de supresión (por defecto, el del pipeline).
#' @return data.table con conteos, tasas, IC 95% e indicadores clínicos y de valor GRD.
#' @export
cmf_query <- function(modulo, fuente = "egresos_deis", years = NULL, subgrupo = NULL, agrupacion = NULL,
                      region = NULL, servicio_salud = NULL, sexo = NULL, grupo_edad = NULL, prevision = NULL,
                      sector = NULL, mecanismo = NULL, actividad = NULL, by = "anio",
                      metric = c("ajustada", "cruda", "conteo", "nacidos_vivos", "universo", "egresos", "ajustada_chile"),
                      suppress = TRUE, n_min = NULL) {
  metric <- match.arg(metric)
  cubo <- cmf_load("cubo")
  if (is.null(cubo) || !nrow(cubo)) stop("Aún no hay datos procesados en ", cmf_data_dir())
  mod <- cmf_module(modulo)
  params <- cmf_manifest()$parametros
  n_min <- n_min %||% params$supresion$n_min %||% 5
  if (is.null(subgrupo)) subgrupo <- unlist(mod_get(mod, "subgrupos_principales", fuente))
  if (identical(subgrupo, "todos")) subgrupo <- NULL

  filtros <- list(modulo = modulo, fuente = fuente, anio = years, subgrupo = subgrupo, region = region,
                  servicio_salud = servicio_salud, sexo = sexo, grupo_edad = grupo_edad,
                  prevision = prevision, sector = sector, mecanismo = mecanismo, agrupacion = agrupacion,
                  actividad = actividad)
  # Cubos publicados antes de la dimensión actividad: todo era hospitalización.
  if (!"actividad" %in% names(cubo) || anyNA(cubo$actividad)) {
    cubo <- data.table::copy(cubo)
    if (!"actividad" %in% names(cubo)) cubo[, actividad := "Hospitalización"] else cubo[is.na(actividad), actividad := "Hospitalización"]
  }
  d <- filter_table(cubo, filtros)
  if ("agrupacion" %in% by || length(agrupacion)) {
    ag <- mod_get(mod, "agrupaciones", fuente)
    if (!length(ag)) stop("El módulo ", modulo, " no define agrupaciones para ", fuente)
    mapa <- data.table::data.table(subgrupo = unlist(ag, use.names = FALSE),
                                   agrupacion = rep(names(ag), lengths(ag)))
    d <- merge(d, mapa, by = "subgrupo")
    if (length(agrupacion)) d <- d[d$agrupacion %in% agrupacion]
  }

  medidas <- intersect(MEDIDAS, names(d))
  sum_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  res <- d[, lapply(.SD, sum_na), by = by, .SDcols = medidas]
  if (!nrow(res) && !length(by)) res <- data.table::as.data.table(stats::setNames(as.list(rep(0, length(medidas))), medidas))

  # Indicadores derivados
  res[, `:=`(estada_media = dias_estada / n_con_estada, pct_estada_5 = 100 * n_estada_5 / n_con_estada,
             pct_estada_7 = 100 * n_estada_7 / n_con_estada,
             letalidad_pct = 100 * fallecidos / n_con_condicion, pct_pabellon = 100 * n_pabellon / n_con_pabellon,
             pct_aislado = 100 * n_aislado / n_con_aislado, peso_grd_medio = peso_grd / n_con_peso,
             valor_grd_medio = valor_grd / n_con_valor)]
  if ("n_con_riss" %in% names(res)) {
    res[, `:=`(riss_medio = riss / n_con_riss, pct_trauma_mayor = 100 * n_trauma_mayor / n_con_riss,
               riss_excl_medio = riss_excl / n_con_riss, pct_trauma_mayor_excl = 100 * n_trauma_mayor_excl / n_con_riss,
               pct_riss_0_3 = 100 * n_riss_0_3 / n_con_riss, pct_riss_4_8 = 100 * n_riss_4_8 / n_con_riss,
               pct_riss_9_15 = 100 * n_riss_9_15 / n_con_riss, pct_riss_16_24 = 100 * n_riss_16_24 / n_con_riss,
               pct_riss_25 = 100 * n_riss_25 / n_con_riss)]
  }

  res[, `:=`(denominador = NA_real_, tasa = NA_real_, tasa_li = NA_real_, tasa_ls = NA_real_)]
  motivo <- NULL
  if (metric != "conteo") {
    den <- tryCatch(rate_denominators(res, d, filtros, by, metric, mod, params),
                    error = function(e) { motivo <<- conditionMessage(e); NULL })
    if (!is.null(den)) {
      res[, c("denominador", "tasa", "tasa_li", "tasa_ls") := NULL]
      res <- if (length(by)) merge(res, den, by = by, all.x = TRUE) else cbind(res, den)
    }
  }

  # Supresión de celdas pequeñas
  res[, suprimido := suppress & !is.na(n) & n < n_min]
  ocultar <- setdiff(names(res), c(by, "suprimido", "denominador"))
  for (k in ocultar) data.table::set(res, which(res$suprimido), k, NA)

  if (length(by)) data.table::setorderv(res, by)
  data.table::setattr(res, "cmf_query", list(filtros = filtros, by = by, metric = metric, suppress = suppress,
                                             n_min = n_min, version = cmf_version(), motivo_sin_tasa = motivo,
                                             por = rate_scale(metric, params, mod)))
  res[]
}

filter_table <- function(dt, filtros) {
  for (k in intersect(names(filtros), names(dt))) {
    v <- filtros[[k]]
    if (!is.null(v) && length(v)) dt <- dt[dt[[k]] %in% v]
  }
  dt
}

rate_scale <- function(metric, params, mod = NULL) {
  if (!is.null(mod$por[[metric]])) return(as.numeric(mod$por[[metric]]))
  switch(metric,
         nacidos_vivos = params$tasas$por_nacidos_vivos %||% 1000,
         universo = params$tasas$por_universo %||% 1000,
         egresos = params$tasas$por_egresos %||% 1e5,
         params$tasas$por %||% 1e5)
}

# Ajuste común o por fuente ({egresos_deis: ..., grd: ...}) de un módulo.
mod_get <- function(mod, key, fuente) {
  v <- mod[[key]]
  if (is.list(v) && !is.null(names(v)) && any(names(v) %in% c("egresos_deis", "grd"))) v <- v[[fuente]]
  v
}

#' Grupos etarios de un módulo para una fuente
#'
#' Cada egreso entra al grupo que contiene el límite inferior de su tramo de edad (como `age_min` en los
#' scripts de los artículos).
#' @export
cmf_age_groups <- function(modulo, fuente = "egresos_deis") {
  g <- mod_get(cmf_module(modulo), "grupos_edad", fuente) %||% cmf_manifest()$parametros$edad$grupos_app
  data.frame(etiqueta = vapply(g, function(x) as.character(x$etiqueta), ""),
             min = vapply(g, function(x) as.numeric(x$min), 0),
             max = vapply(g, function(x) as.numeric(x$max), 0), stringsAsFactors = FALSE)
}

# Población con tramos INE (edad_min, edad_max). Compatible con salidas antiguas que solo traen grupo_edad_5.
population_bands <- function(pob) {
  pob <- data.table::copy(pob)
  if (!all(c("edad_min", "edad_max") %in% names(pob))) {
    lo <- suppressWarnings(as.numeric(sub("[-+].*$", "", pob$grupo_edad_5)))
    hi <- suppressWarnings(as.numeric(sub("^.*-", "", pob$grupo_edad_5)))
    hi[grepl("\\+", pob$grupo_edad_5)] <- 150
    pob[, `:=`(edad_min = lo, edad_max = hi)]
  }
  if ("grupo_edad" %in% names(pob)) pob[, grupo_edad := NULL]
  pob
}

# Asigna cada tramo INE a un grupo del módulo. Detiene si un tramo necesario cruza dos grupos
# (p. ej. INE 15-19 frente a los grupos 10-18 y 19-44 del artículo DEIS de trauma).
bands_to_groups <- function(pf, grupos, necesarios = NULL) {
  pf[, grupo_edad := NA_character_]
  necesarios <- necesarios %||% vapply(grupos, function(g) as.character(g$etiqueta), "")
  for (g in grupos) {
    dentro <- pf$edad_min >= g$min & pmin(pf$edad_max, 150) <= g$max
    pf[dentro, grupo_edad := as.character(g$etiqueta)]
    cruza <- !dentro & pf$edad_min <= g$max & pmin(pf$edad_max, 150) >= g$min
    if (as.character(g$etiqueta) %in% necesarios && any(cruza)) {
      stop("El grupo etario ", g$etiqueta, " no calza con los tramos de la población INE (",
           paste(unique(paste0(pf$edad_min[cruza], "-", pf$edad_max[cruza])), collapse = ", "),
           "): hay tasas por universo o conteos, pero no tasa poblacional por ese grupo")
    }
  }
  pf
}

poisson_ci <- function(n, den, por) {
  list(tasa = n / den * por,
       li = ifelse(n > 0, stats::qchisq(0.025, 2 * n) / 2, 0) / den * por,
       ls = stats::qchisq(0.975, 2 * (n + 1)) / 2 / den * por)
}

# Grupos quinquenales dentro del rango de edad del módulo.
module_bands5 <- function(mod, abierto = 80, fuente = "grd") {
  edad <- mod_get(mod, "edad", fuente)
  if (is.null(edad)) return(NULL)
  lo <- seq(0, abierto, 5)
  lab <- ifelse(lo >= abierto, paste0(abierto, "+"), paste0(lo, "-", lo + 4))
  hi <- ifelse(lo >= abierto, 150, lo + 4)
  lab[lo >= (edad$min %||% 0) & hi <= (edad$max %||% 150)]
}

rate_denominators <- function(res, d, filtros, by, metric, mod, params) {
  por <- rate_scale(metric, params, mod)
  dims_extra <- c("subgrupo", "agrupacion", "actividad")
  edad <- mod_get(mod, "edad", filtros$fuente)
  if (metric %in% c("cruda", "ajustada", "ajustada_chile")) {
    if (length(filtros$prevision) || length(filtros$sector)) {
      stop("La población INE no tiene previsión ni sector: con esos filtros solo hay conteos")
    }
    if (length(setdiff(by, c(DIM_POBLACION, dims_extra, "mecanismo")))) {
      stop("La población INE no tiene esa dimensión: solo hay conteos")
    }
    if (isTRUE(as.numeric(edad$max) == 0)) {
      stop("Módulo de menores de 1 año: use la tasa por nacidos vivos")
    }
    ajusta <- unlist(params$tasas$ajuste_edad_fuentes %||% "grd")
    if (metric == "ajustada" && !(filtros$fuente %in% ajusta)) {
      stop("El DEIS publica la edad en tramos decenales en la mayoría de los años y no permite estandarizar ",
           "con quinquenios OMS: use la tasa cruda o la tasa por universo")
    }
    pob <- cmf_load("poblacion")
    if (is.null(pob)) stop("No hay población INE cargada")
    pf <- population_bands(filter_table(pob, filtros[c("anio", "region", "servicio_salud", "sexo")]))
    if (!is.null(edad)) {
      pf <- pf[edad_min >= (edad$min %||% 0) & pmin(edad_max, 150) <= (edad$max %||% 150)]
    }
    if ("grupo_edad" %in% by || length(filtros$grupo_edad)) {
      grupos <- mod_get(mod, "grupos_edad", filtros$fuente) %||% params$edad$grupos_app
      pf <- bands_to_groups(pf, grupos, if ("grupo_edad" %in% by) NULL else filtros$grupo_edad)
      if (length(filtros$grupo_edad)) pf <- pf[grupo_edad %in% filtros$grupo_edad]
    }
    by_pob <- intersect(by, DIM_POBLACION)
    if (metric == "cruda") {
      den <- pf[, .(denominador = sum(poblacion)), by = by_pob]
      x <- if (length(by_pob)) merge(res[, c(by, "n"), with = FALSE], den, by = by_pob, all.x = TRUE)
           else cbind(res[, c(by, "n"), with = FALSE], den)
      ci <- poisson_ci(x$n, x$denominador, por)
      x[, `:=`(tasa = ci$tasa, tasa_li = ci$li, tasa_ls = ci$ls)]
      return(x[, c(by, "denominador", "tasa", "tasa_li", "tasa_ls"), with = FALSE])
    }
    if (metric == "ajustada_chile") return(age_adjusted_chile(d, pf, by, por, mod, filtros$fuente, params))
    return(age_adjusted(d, pf, by, por, module_bands5(mod, params$edad$grupo_abierto %||% 80, filtros$fuente)))
  }
  if (metric == "egresos") {
    if (length(filtros$grupo_edad) || length(filtros$servicio_salud) || length(filtros$prevision) ||
        length(filtros$sector) || length(filtros$mecanismo)) {
      stop("Los egresos totales solo se desagregan por año, región y sexo")
    }
    if (length(setdiff(by, c(DIM_TOTALES, dims_extra)))) stop("Los egresos totales solo se desagregan por año, región y sexo")
    tt <- cmf_load("totales")
    if (is.null(tt)) stop("No hay totales de egresos publicados")
    if (!"actividad" %in% names(tt)) tt <- data.table::copy(tt)[, actividad := "Hospitalización"]
    if (anyNA(tt$actividad)) tt <- data.table::copy(tt)[is.na(actividad), actividad := "Hospitalización"]
    # Denominador de la misma actividad que el numerador: egresos hospitalarios para los módulos de solo
    # hospitalización; hospitalización + CMA para los que suman CMA (o la actividad filtrada).
    act <- filtros$actividad %||% unlist(mod_get(mod, "actividades", filtros$fuente)) %||% "Hospitalización"
    tf <- filter_table(tt, c(filtros[c("fuente", "anio", "region", "sexo")], list(actividad = act)))
    by_t <- intersect(by, c(DIM_TOTALES, "actividad"))
    den <- tf[, .(denominador = sum(n_total)), by = by_t]
    x <- if (length(by_t)) merge(res[, c(by, "n"), with = FALSE], den, by = by_t, all.x = TRUE)
         else cbind(res[, c(by, "n"), with = FALSE], den)
    ci <- poisson_ci(x$n, x$denominador, por)
    x[, `:=`(tasa = ci$tasa, tasa_li = ci$li, tasa_ls = ci$ls)]
    return(x[, c(by, "denominador", "tasa", "tasa_li", "tasa_ls"), with = FALSE])
  }
  if (metric == "nacidos_vivos") {
    if (length(filtros$sexo) || length(filtros$grupo_edad) || length(filtros$prevision) || length(filtros$sector)) {
      stop("Los nacidos vivos no se desagregan por sexo, edad, previsión ni sector")
    }
    if (length(setdiff(by, c(DIM_EDAD0, "subgrupo", "agrupacion")))) stop("Los nacidos vivos solo se desagregan por año, región y Servicio de Salud")
    e0 <- cmf_load("poblacion_edad0")
    if (is.null(e0) || all(is.na(e0$nacidos_vivos))) stop("No hay nacidos vivos cargados")
    ef <- filter_table(e0, filtros[c("anio", "region", "servicio_salud")])[!is.na(nacidos_vivos)]
    by_e <- intersect(by, DIM_EDAD0)
    den <- ef[, .(denominador = sum(nacidos_vivos)), by = by_e]
    x <- if (length(by_e)) merge(res[, c(by, "n"), with = FALSE], den, by = by_e, all.x = TRUE)
         else cbind(res[, c(by, "n"), with = FALSE], den)
    ci <- poisson_ci(x$n, x$denominador, por)
    x[, `:=`(tasa = ci$tasa, tasa_li = ci$li, tasa_ls = ci$ls)]
    return(x[, c(by, "denominador", "tasa", "tasa_li", "tasa_ls"), with = FALSE])
  }
  if (metric == "universo") {
    if (length(filtros$mecanismo)) stop("El universo de referencia no se desagrega por mecanismo")
    if (length(setdiff(by, c(DIM_UNIVERSO, "subgrupo", "agrupacion")))) stop("El universo de referencia no tiene esa dimensión")
    u <- cmf_load("universo")
    if (is.null(u)) stop("Este módulo no tiene universo de referencia")
    uf <- filter_table(u, filtros[c("modulo", "fuente", "anio", "region", "servicio_salud", "sexo", "grupo_edad",
                                    "prevision", "sector")])
    if (!nrow(uf)) stop("Este módulo no tiene universo de referencia para esa fuente")
    by_u <- intersect(by, DIM_UNIVERSO)
    den <- uf[, .(denominador = sum(n_universo)), by = by_u]
    x <- if (length(by_u)) merge(res[, c(by, "n"), with = FALSE], den, by = by_u, all.x = TRUE)
         else cbind(res[, c(by, "n"), with = FALSE], den)
    ci <- poisson_ci(x$n, x$denominador, por)
    x[, `:=`(tasa = ci$tasa, tasa_li = ci$li, tasa_ls = ci$ls)]
    return(x[, c(by, "denominador", "tasa", "tasa_li", "tasa_ls"), with = FALSE])
  }
}

#' IC gamma de Fay-Feuer para una tasa directamente estandarizada
#'
#' Réplica de `_fay_feuer_gamma_ci` del notebook de trauma (Fay & Feuer, Stat Med 1997).
#' @param n Casos por estrato. @param p Población por estrato. @param w Pesos estándar (se normalizan).
#' @return c(tasa, li, ls) por unidad de población.
#' @export
fay_feuer <- function(n, p, w, alpha = 0.05) {
  w <- w / sum(w)
  ew <- w / p
  y <- sum(ew * n); v <- sum(ew^2 * n); wm <- max(ew)
  lo <- if (y == 0) 0 else stats::qgamma(alpha / 2, shape = y^2 / v, scale = v / y)
  yu <- y + wm; vu <- v + wm^2
  hi <- stats::qgamma(1 - alpha / 2, shape = yu^2 / vu, scale = vu / yu)
  c(y, lo, hi)
}

# Tasas ajustadas por edad (método directo, OMS 2000-2025 con 80+ agregado), IC de Fay-Feuer.
age_adjusted <- function(d, pf, by, por, grupos = NULL, col = "grupo_edad_5", w = NULL) {
  if (is.null(w)) {
    std <- data.table::copy(cmf_load("poblacion_estandar"))
    abierto <- max(as.integer(sub("\\+", "", grep("\\+", unique(pf$grupo_edad_5), value = TRUE))), 80, na.rm = TRUE)
    std[, grupo_edad_5 := ifelse(as.numeric(edad_min) >= abierto, paste0(abierto, "+"),
                                 paste0(as.numeric(edad_min), "-", as.numeric(edad_min) + 4))]
    w <- std[, .(peso = sum(as.numeric(peso))), by = grupo_edad_5]
    if (!is.null(grupos)) w <- w[grupo_edad_5 %in% grupos]
  }
  data.table::setnames(w, 1, ".g")
  by_pob <- intersect(by, DIM_POBLACION)
  num <- d[!is.na(get(col)), .(n = sum(n)), by = c(by, col)]
  den <- pf[!is.na(get(col)), .(poblacion = sum(poblacion)), by = c(by_pob, col)]
  data.table::setnames(num, col, ".g"); data.table::setnames(den, col, ".g")
  claves <- if (length(by)) unique(d[, by, with = FALSE]) else data.table::data.table(.k = 1L)
  full <- merge(claves[, .k := 1L], w[, .(.g, peso, .k = 1L)], by = ".k", allow.cartesian = TRUE)
  full[, .k := NULL]
  full <- merge(full, den, by = c(by_pob, ".g"), all.x = TRUE)
  full <- merge(full, num, by = c(by, ".g"), all.x = TRUE)
  full[is.na(n), n := 0]
  out <- full[, {
    ok <- !is.na(poblacion) & poblacion > 0
    if (!any(ok)) list(denominador = NA_real_, tasa = NA_real_, tasa_li = NA_real_, tasa_ls = NA_real_)
    else {
      r <- fay_feuer(n[ok], poblacion[ok], peso[ok])
      list(denominador = as.numeric(sum(poblacion[ok])), tasa = r[1] * por, tasa_li = r[2] * por, tasa_ls = r[3] * por)
    }
  }, by = by]
  extra <- setdiff(names(out), c(by, "denominador", "tasa", "tasa_li", "tasa_ls"))
  if (length(extra)) out[, (extra) := NULL]
  # Sin edad suficiente (p. ej. un año con tramos que no calzan) la tasa ajustada no es comparable:
  # se omite si menos del 95% de los egresos tiene grupo de edad.
  cob <- d[, .(n_todos = sum(n), n_q = sum(n[!is.na(get(col))])), by = by]
  out <- if (length(by)) merge(out, cob, by = by, all.x = TRUE) else cbind(out, cob)
  out[!is.na(n_todos) & n_todos > 0 & n_q / n_todos < 0.95,
      `:=`(tasa = NA_real_, tasa_li = NA_real_, tasa_ls = NA_real_)]
  out[, c("n_todos", "n_q") := NULL]
  out
}

# Estandarización directa a la distribución etaria chilena agrupada (población INE nacional de los años
# indicados, por defecto 2002-2025) con los grupos del módulo: sensibilidad del artículo de infecciones,
# aplicable al DEIS porque no requiere quinquenios.
age_adjusted_chile <- function(d, pf, by, por, mod, fuente, params) {
  if ("grupo_edad" %in% by) stop("La tasa ajustada no se desagrega por grupo etario: use la tasa cruda")
  grupos <- mod_get(mod, "grupos_edad", fuente) %||% params$edad$grupos_app
  anios <- unlist(params$tasas$estandar_chile_anios %||% c(2002, 2025))
  pob <- population_bands(cmf_load("poblacion"))
  pob <- pob[anio >= min(anios) & anio <= max(anios)]
  edad <- mod_get(mod, "edad", fuente)
  if (!is.null(edad)) pob <- pob[edad_min >= (edad$min %||% 0) & pmin(edad_max, 150) <= (edad$max %||% 150)]
  pob <- bands_to_groups(pob, grupos)
  w <- pob[!is.na(grupo_edad), .(peso = as.numeric(sum(poblacion))), by = grupo_edad]
  pf <- bands_to_groups(pf, grupos)
  age_adjusted(d, pf, by, por, col = "grupo_edad", w = w)
}
