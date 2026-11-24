# Dimensión económica: valor GRD y días cama desde el cubo -------------------------------------------
#
# El valor GRD de cada egreso es su peso GRD por el precio base del hospital según el clúster de la resolución
# de precios, con una estructura de precios constante (config/pipeline.yml, costos$estructura_precios_grd). Es el
# valor de los recursos desde la perspectiva del financiador público, no un costo hospitalario observado. Solo
# existe en GRD: el DEIS no trae peso ni precio.

ECO_INDICADORES <- c("valor_total", "valor_medio", "valor_100k", "dias_cama", "estada_media")

estructura_precios <- function() cmf_manifest()$parametros$costos$estructura_precios_grd %||% 2024

#' Etiqueta de un indicador económico
#' @param indicador Uno de "valor_total", "valor_medio", "valor_100k", "dias_cama", "estada_media".
#' @export
cmf_eco_label <- function(indicador) {
  est <- estructura_precios()
  switch(indicador,
         valor_total = paste0("Valor GRD, millones de CLP (precios ", est, ")"),
         valor_medio = paste0("Valor GRD medio por egreso, miles de CLP (precios ", est, ")"),
         valor_100k = paste0("Valor GRD por 100.000 habitantes, millones de CLP (precios ", est, ")"),
         dias_cama = "Días cama",
         estada_media = "Estada media, días",
         indicador)
}

#' ¿El módulo tiene dimensión económica?
#'
#' Sí cuando el módulo usa GRD (valor GRD por egreso), salvo que config/modules.yml diga `economia: false`.
#' @export
cmf_economia_disponible <- function(modulo) {
  m <- tryCatch(cmf_module(modulo), error = function(e) NULL)
  if (is.null(m) || isFALSE(m$economia) || !identical(m$tipo %||% "eventos", "eventos")) return(FALSE)
  "grd" %in% unlist(m$fuentes)
}

#' Consulta económica: valor GRD y días cama
#'
#' Igual que [cmf_query()] (mismos filtros y dimensiones), siempre sobre GRD, y devuelve la columna `valor` con
#' el indicador pedido. Las celdas con menos de `n_min` egresos se suprimen como en el resto del observatorio.
#' @param indicador "valor_total" (millones de CLP), "valor_medio" (miles de CLP por egreso con valor),
#'   "valor_100k" (millones de CLP por 100.000 habitantes, denominador INE), "dias_cama" (suma de la estada) o
#'   "estada_media" (días).
#' @inheritParams cmf_query
#' @return data.table con `valor`, los egresos (`n`, `n_con_valor`) y las sumas de origen.
#' @export
cmf_query_eco <- function(modulo, indicador = ECO_INDICADORES, years = NULL, subgrupo = NULL, agrupacion = NULL,
                          region = NULL, servicio_salud = NULL, sexo = NULL, grupo_edad = NULL, prevision = NULL,
                          sector = NULL, mecanismo = NULL, actividad = NULL, by = "anio", suppress = TRUE,
                          n_min = NULL) {
  indicador <- match.arg(indicador)
  if (!cmf_economia_disponible(modulo)) stop("El módulo ", modulo, " no tiene valor GRD (solo DEIS)")
  metric <- if (indicador == "valor_100k") "cruda" else "conteo"
  q <- cmf_query(modulo, fuente = "grd", years = years, subgrupo = subgrupo, agrupacion = agrupacion,
                 region = region, servicio_salud = servicio_salud, sexo = sexo, grupo_edad = grupo_edad,
                 prevision = prevision, sector = sector, mecanismo = mecanismo, actividad = actividad, by = by,
                 metric = metric, suppress = suppress, n_min = n_min)
  meta <- attr(q, "cmf_query")
  if (indicador == "valor_100k" && !is.null(meta$motivo_sin_tasa)) stop(meta$motivo_sin_tasa)
  cols <- intersect(c(by, "n", "n_con_valor", "valor_grd", "valor_grd_medio", "n_con_estada", "dias_estada",
                      "estada_media", "denominador", "suprimido"), names(q))
  out <- q[, cols, with = FALSE]
  out[, valor := switch(indicador,
                        valor_total = q$valor_grd / 1e6,
                        valor_medio = q$valor_grd_medio / 1e3,
                        valor_100k = q$valor_grd / q$denominador * 1e5 / 1e6,
                        dias_cama = q$dias_estada,
                        estada_media = q$estada_media)]
  meta$metric <- indicador
  meta$indicador <- indicador
  meta$etiqueta <- cmf_eco_label(indicador)
  data.table::setattr(out, "cmf_query", meta)
  out[]
}

# Medida que reparte una composición económica (porcentaje del valor o de los días cama).
eco_medida <- function(indicador) {
  if (indicador %in% c("dias_cama", "estada_media")) "dias_estada" else "valor_grd"
}
