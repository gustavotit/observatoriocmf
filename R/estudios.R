# Estudios económicos importados -------------------------------------------------------------------
#
# Resultados agregados de los notebooks de los artículos (p. ej. carga económica del trauma facial, GRD
# 2019-2025): estimandos principales, escenarios de pérdida de productividad y carga por Servicio de Salud. Los
# importa pipeline/importar_estudio.R desde la carpeta de salida del notebook, según config/estudios_economicos.yml,
# y quedan como estudio_<id>_<tabla>.csv junto a los demás datos publicados. No contienen registros individuales.

#' Estudios económicos publicados
#' @param modulo Filtra por módulo (opcional).
#' @return Lista de estudios (configuración de config/estudios_economicos.yml) que tienen al menos una tabla.
#' @export
cmf_estudios <- function(modulo = NULL) {
  f <- file.path(cmf_data_dir(), "estudios_economicos.yml")
  if (!file.exists(f)) return(list())
  e <- tryCatch(yaml::read_yaml(f, fileEncoding = "UTF-8")$estudios, error = function(e) list())
  e <- Filter(function(x) {
    any(file.exists(file.path(cmf_data_dir(), paste0("estudio_", x$id, "_", names(x$tablas), ".csv"))))
  }, e %||% list())
  if (!is.null(modulo)) e <- Filter(function(x) identical(x$modulo, modulo), e)
  e
}

#' Una tabla de un estudio económico
#' @param id Id del estudio (ver [cmf_estudios()]).
#' @param tabla Nombre de la tabla, p. ej. "estimandos", "escenarios", "productividad_anual", "territorio".
#' @return data.table o NULL si el estudio no la trae.
#' @export
cmf_estudio_tabla <- function(id, tabla) {
  key <- paste(cmf_data_dir(), "estudio", id, tabla)
  if (!is.null(.cmf_env[[key]])) return(.cmf_env[[key]])
  f <- file.path(cmf_data_dir(), paste0("estudio_", id, "_", tabla, ".csv"))
  if (!file.exists(f)) return(NULL)
  dt <- data.table::fread(f, encoding = "UTF-8", na.strings = c("", "NA"))
  .cmf_env[[key]] <- dt
  dt
}

estudio_cfg <- function(id) {
  e <- Filter(function(x) identical(x$id, id), cmf_estudios())
  if (!length(e)) stop("Estudio desconocido o sin tablas: ", id)
  e[[1]]
}

estudio_fuente <- function(cfg) {
  paste0(cfg$titulo_corto %||% cfg$titulo, if (!is.null(cfg$periodo)) paste0(", ", cfg$periodo) else "")
}

# Escenario sin el sufijo de ingreso, para rotular.
escenario_base <- function(x) sub(" \\| ingreso mediano$", "", x)

#' Escenarios de pérdida de productividad con IC 95%
#' @param id Id del estudio.
#' @param ingreso "medio" o "mediano" (ingreso del trabajo usado para valorar los días de ausencia).
#' @param color Color de los puntos (por defecto el del módulo).
#' @export
cmf_plot_escenarios <- function(id, ingreso = c("medio", "mediano"), color = NULL) {
  ingreso <- match.arg(ingreso)
  cfg <- estudio_cfg(id)
  d <- data.table::copy(cmf_estudio_tabla(id, "escenarios"))
  if (is.null(d)) stop("El estudio no trae escenarios")
  med <- grepl("\\| ingreso mediano$", d$escenario)
  d <- d[if (ingreso == "mediano") med else !med]
  etiquetas <- unlist(cfg$etiquetas_escenarios %||% list())
  d[, rotulo := escenario_base(escenario)]
  if (length(etiquetas)) d[rotulo %in% names(etiquetas), rotulo := etiquetas[rotulo]]
  principal <- escenario_base(cfg$escenario_principal %||% d$escenario[1])
  d[, es_base := escenario_base(escenario) == principal]
  d[, rotulo := factor(rotulo, levels = rev(unique(rotulo)))]
  col <- color %||% tryCatch(cmf_module(cfg$modulo)$color, error = function(e) NULL) %||% CMF_COLORES[1]
  ggplot2::ggplot(d, ggplot2::aes(total, rotulo)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = li, xmax = ls), height = 0, linewidth = 0.5, colour = "grey25") +
    ggplot2::geom_point(ggplot2::aes(fill = es_base), shape = 22, size = 2.6, colour = "grey15", stroke = 0.3) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = col, `FALSE` = "white"), guide = "none") +
    ggplot2::scale_x_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE),
                                expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
    ggplot2::expand_limits(x = 0) +
    ggplot2::labs(x = paste0("Millones de ", cfg$moneda %||% "CLP"), y = NULL,
                  caption = paste0("Fuente: ", estudio_fuente(cfg), "; Observatorio CMF Chile, versión ", cmf_version(), ".")) +
    cmf_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = "grey91", linewidth = 0.3),
                   axis.ticks.y = ggplot2::element_blank())
}

#' Serie anual de pérdida de productividad (escenario principal) con IC 95%
#' @export
cmf_plot_estudio_anual <- function(id, escenario = NULL, color = NULL) {
  cfg <- estudio_cfg(id)
  d <- cmf_estudio_tabla(id, "productividad_anual")
  if (is.null(d)) stop("El estudio no trae la serie anual")
  esc <- escenario %||% cfg$escenario_principal %||% d$escenario[1]
  d <- data.table::copy(d[escenario == esc])
  col <- color %||% tryCatch(cmf_module(cfg$modulo)$color, error = function(e) NULL) %||% CMF_COLORES[1]
  ggplot2::ggplot(d, ggplot2::aes(anio, total)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = li, ymax = ls), fill = col, alpha = 0.16) +
    ggplot2::geom_line(colour = col, linewidth = 0.7) +
    ggplot2::geom_point(colour = col, size = 1.6) +
    ggplot2::scale_x_continuous(breaks = unique(d$anio)) +
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = NULL, y = wrap_lbl(paste0("Pérdida de productividad, millones de ", cfg$moneda %||% "CLP"), 34),
                  caption = paste0("Fuente: ", estudio_fuente(cfg), "; Observatorio CMF Chile, versión ", cmf_version(), ".")) +
    cmf_theme()
}

ESTUDIO_TERRITORIO <- c(productividad_100k = "Pérdida de productividad por 100.000 personas-año",
                        productividad = "Pérdida de productividad",
                        valor_grd = "Valor GRD observado",
                        dias_cama = "Días cama")

#' Carga por Servicio de Salud de residencia en un estudio económico
#' @param indicador "productividad_100k", "productividad", "valor_grd" o "dias_cama".
#' @return data.table con `servicio_salud` y `valor`, con el atributo cmf_query que usan
#'   [cmf_plot_map()] y [cmf_plot_bars()].
#' @export
cmf_estudio_territorio <- function(id, indicador = names(ESTUDIO_TERRITORIO)) {
  indicador <- match.arg(indicador)
  cfg <- estudio_cfg(id)
  d <- cmf_estudio_tabla(id, "territorio")
  if (is.null(d) || !indicador %in% names(d)) stop("El estudio no trae ese indicador territorial")
  out <- d[!is.na(servicio_salud), .(servicio_salud = as.integer(servicio_salud), n = n, valor = get(indicador))]
  # Montos en millones; días cama tal cual.
  if (indicador %in% c("productividad", "productividad_100k", "valor_grd")) out[, valor := valor / 1e6]
  etiqueta <- switch(indicador,
                     productividad_100k = paste0("Pérdida de productividad por 100.000 personas-año, millones de ", cfg$moneda %||% "CLP"),
                     productividad = paste0("Pérdida de productividad, millones de ", cfg$moneda %||% "CLP"),
                     valor_grd = paste0("Valor GRD observado, millones de ", cfg$moneda %||% "CLP"),
                     dias_cama = "Días cama")
  data.table::setattr(out, "cmf_query", list(by = "servicio_salud", metric = indicador, indicador = indicador,
                                             etiqueta = etiqueta, version = cmf_version(), por = 1e5,
                                             fuente_txt = estudio_fuente(cfg)))
  out[]
}

#' Cita de un estudio económico del observatorio
#'
#' Si el artículo del estudio está publicado (`referencia` en config/estudios_economicos.yml), va primero.
#' @export
cmf_cite_estudio <- function(id, fecha = Sys.Date()) {
  cfg <- estudio_cfg(id)
  obs <- cmf_cite(NULL, fecha = fecha, incluir_articulo = FALSE)
  obs <- sub("\\]\\.", paste0("]. Estudio: ", cfg$titulo_corto %||% cfg$titulo,
                               if (!is.null(cfg$periodo)) paste0(" (", cfg$periodo, ")"), "."), obs)
  ref <- cfg$referencia
  if (!is.null(ref) && nzchar(ref)) paste0(ref, "\n", obs) else obs
}
