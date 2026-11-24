# Figuras listas para publicar ---------------------------------------------------
#
# Estética de revista médica (JAMA): fondo blanco, sans serif, ejes negros finos, sin grilla salvo guías
# horizontales tenues, paleta JAMA (ggsci::pal_jama) para series y una rampa secuencial sobria para mapas.

CMF_COLORES <- c("#374E55", "#DF8F44", "#00A1D5", "#B24745", "#79AF97", "#6A6599", "#80796B",
                 "#1B2B30", "#A0602A", "#006A8E", "#7A2F2D", "#4E7D69")
CMF_GRISES <- c("#1A1A1A", "#5E5E5E", "#8F8F8F", "#B8B8B8", "#3B3B3B", "#A3A3A3")
CMF_SECUENCIAL <- c("#EEF1F2", "#C9D6DA", "#93AEB6", "#5E8591", "#374E55", "#1B2B30")

cmf_pal <- function(estilo, n) {
  v <- if (estilo == "grises") CMF_GRISES else CMF_COLORES
  rep(v, length.out = n)
}

#' Tema gráfico del observatorio (estilo JAMA)
#' @param base_size Tamaño base de letra (pt).
#' @param estilo "jama" (paleta JAMA) o "grises".
#' @export
cmf_theme <- function(base_size = 9, estilo = c("jama", "grises", "revista", "color")) {
  estilo <- match.arg(estilo)
  ggplot2::theme_classic(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "black", linewidth = 0.35),
      axis.ticks = ggplot2::element_line(colour = "black", linewidth = 0.35),
      axis.ticks.length = grid::unit(2.5, "pt"),
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(colour = "black"),
      panel.grid.major.y = ggplot2::element_line(colour = "grey91", linewidth = 0.3),
      legend.position = "top", legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.key.height = grid::unit(9, "pt"), legend.key.width = grid::unit(14, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(colour = "grey35", hjust = 0, size = base_size - 1.5),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}

norm_estilo <- function(estilo) {
  estilo <- estilo[1] %||% "jama"
  if (estilo %in% c("revista", "grises")) "grises" else "jama"
}

# Etiqueta del eje: la del indicador (económico o de un estudio) o la de la métrica epidemiológica.
eje_label <- function(meta) meta$etiqueta %||% metric_label(meta$metric, meta$por %||% 1e5)

metric_label <- function(metric, por = 1e5) {
  p <- format(por, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  switch(metric,
         conteo = "Egresos, n",
         cruda = paste0("Egresos por ", p, " habitantes"),
         ajustada = paste0("Egresos por ", p, " habitantes, ajustada por edad"),
         ajustada_chile = paste0("Egresos por ", p, " habitantes, ajustada por edad"),
         nacidos_vivos = paste0("Egresos por ", p, " nacidos vivos"),
         universo = paste0("Egresos por ", p, " egresos del universo"),
         egresos = paste0("Egresos por ", p, " egresos totales"))
}

# Pie de figura breve: la descripción completa de los filtros va en la cita y en el párrafo de métodos.
cmf_fuente <- function(q) {
  fq <- attr(q, "cmf_query")
  paste0("Fuente: ", if (!is.null(fq$fuente_txt)) paste0(fq$fuente_txt, "; ") else "", "Observatorio CMF Chile, versión ", fq$version, ".")
}

wrap_lbl <- function(x, width = 34) paste(strwrap(x, width = width), collapse = "\n")

# Categorías sin dato o no especificadas van al final y en gris.
es_sin_dato <- function(x) grepl("^(Desconocid|No especificad|Sin |Otro|Otra)", x, ignore.case = TRUE)
ordenar_cats <- function(lv) c(lv[!es_sin_dato(lv)], lv[es_sin_dato(lv)])
pal_cats <- function(lv, estilo) {
  pal <- stats::setNames(cmf_pal(estilo, length(lv)), lv)
  pal[es_sin_dato(lv)] <- if (estilo == "grises") "#D0D0D0" else "#B8B3AC"
  pal
}

value_col <- function(q) {
  meta <- attr(q, "cmf_query")
  if (!is.null(meta$indicador)) return("valor")
  if (meta$metric == "conteo") "n" else "tasa"
}

#' Tendencia anual con IC 95% y línea de tendencia
#'
#' Dibuja la serie observada (línea y puntos) con su IC 95% (banda) y una tendencia lineal (discontinua).
#' Con `its_break`, la tendencia es segmentada (nivel y pendiente antes y después) con el contrafactual.
#' Es descriptiva: para inferencia usa modelos con autocorrelación (Poisson/BN con Fourier o ARIMA).
#' @param q Resultado de [cmf_query()] con "anio" en `by`.
#' @param its_break Año del quiebre (el primero del período posterior).
#' @param estilo "jama" o "grises".
#' @param tendencia Dibuja la tendencia lineal (o segmentada con `its_break`).
#' @export
cmf_plot_trend <- function(q, its_break = NULL, estilo = "jama", tendencia = TRUE, color = NULL) {
  estilo <- norm_estilo(estilo)
  meta <- attr(q, "cmf_query")
  y <- value_col(q)
  grp <- setdiff(meta$by, "anio")
  d <- data.table::copy(q)
  d[, y := get(y)]
  if (length(grp)) d <- d[!is.na(get(grp[1]))]
  d[, .g := if (length(grp)) as.character(get(grp[1])) else "serie"]
  ci <- meta$metric != "conteo" && all(c("tasa_li", "tasa_ls") %in% names(d))
  lv <- ordenar_cats(unique(d$.g))
  d[, .g := factor(.g, levels = lv)]
  pal <- pal_cats(lv, estilo)
  if (length(lv) == 1 && !is.null(color) && estilo != "grises") pal[] <- color

  # Tendencia: lineal por serie; con quiebre, segmentada (nivel + pendiente) y contrafactual.
  tr <- NULL; cf <- NULL
  if (tendencia) {
    segs <- lapply(split(d[!is.na(y)], by = ".g"), function(s) {
      if (nrow(s) < 3) return(NULL)
      s <- data.table::copy(s)
      s[, t := anio - min(anio)]
      if (!is.null(its_break) && sum(s$anio < its_break) >= 3 && sum(s$anio >= its_break) >= 2) {
        s[, `:=`(post = as.integer(anio >= its_break), t_post = pmax(0, anio - its_break + 1))]
        fit <- stats::lm(y ~ t + post + t_post, data = s)
        s[, `:=`(ajuste = stats::predict(fit, s), contrafactual = stats::coef(fit)[1] + stats::coef(fit)[2] * t)]
        s[, tramo := ifelse(post == 1, "post", "pre")]
      } else {
        fit <- stats::lm(y ~ t, data = s)
        s[, `:=`(ajuste = stats::predict(fit, s), contrafactual = NA_real_, tramo = "todo", post = 0L)]
      }
      s
    })
    tr <- data.table::rbindlist(segs, fill = TRUE)
    if (nrow(tr) && any(!is.na(tr$contrafactual))) cf <- tr[post == 1]
  }

  p <- ggplot2::ggplot(d, ggplot2::aes(anio, y, colour = .g, fill = .g, group = .g))
  if (!is.null(its_break)) {
    p <- p + ggplot2::geom_vline(xintercept = its_break - 0.5, colour = "grey55", linewidth = 0.35, linetype = "dotted")
  }
  if (ci) p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = tasa_li, ymax = tasa_ls), alpha = 0.16, colour = NA)
  if (!is.null(tr) && nrow(tr)) {
    p <- p + ggplot2::geom_line(data = tr, ggplot2::aes(anio, ajuste, group = interaction(.g, tramo)),
                                linetype = "dashed", linewidth = 0.45)
    if (!is.null(cf) && nrow(cf)) {
      p <- p + ggplot2::geom_line(data = cf, ggplot2::aes(anio, contrafactual, group = .g),
                                  linetype = "dotted", linewidth = 0.45, alpha = 0.8)
    }
  }
  p <- p + ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 1.5, shape = 21, stroke = 0.5, fill = "white")
  p <- p + ggplot2::scale_colour_manual(values = pal, aesthetics = c("colour", "fill"),
                                        guide = if (length(grp)) "legend" else "none")
  p + ggplot2::scale_x_continuous(breaks = function(l) {
        r <- seq(ceiling(l[1]), floor(l[2])); r[(r - r[1]) %% max(1, round(length(r) / 8)) == 0]
      }, expand = ggplot2::expansion(add = 0.4)) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.06)),
                                labels = function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)) +
    ggplot2::labs(x = NULL, y = wrap_lbl(eje_label(meta)), caption = cmf_fuente(q)) +
    cmf_theme(estilo = estilo)
}

territorial_names <- function(d, var) {
  if (var == "region") {
    reg <- cmf_region_names()
    d <- merge(d, reg, by.x = "region", by.y = "codigo_region", all.x = TRUE)
    d[, etiqueta := factor(nombre, levels = rev(reg[order(orden_norte_sur)]$nombre))]
  } else if (var == "servicio_salud") {
    ss <- cmf_service_names()
    reg <- cmf_region_names()
    ss <- merge(ss, reg[, .(region = codigo_region, orden_norte_sur)], by = "region")
    d <- merge(d, ss, by.x = "servicio_salud", by.y = "codigo_servicio_salud", all.x = TRUE)
    lv <- ss[order(orden_norte_sur, codigo_servicio_salud)]$nombre
    d[, etiqueta := factor(nombre, levels = rev(lv))]
  } else {
    d[, etiqueta := factor(get(var), levels = rev(unique(get(var)[order(-y)])))]
  }
  d
}

#' Barras por región o Servicio de Salud (norte a sur) con IC 95%
#' @export
cmf_plot_bars <- function(q, estilo = "jama", color = NULL) {
  estilo <- norm_estilo(estilo)
  meta <- attr(q, "cmf_query")
  var <- meta$by[1]
  y <- value_col(q)
  d <- data.table::copy(q)[!is.na(get(var))]
  d[, y := get(y)]
  d <- territorial_names(d, var)
  fill <- if (estilo == "grises") "grey40" else color %||% CMF_COLORES[1]
  p <- ggplot2::ggplot(d, ggplot2::aes(y, etiqueta)) +
    ggplot2::geom_col(fill = fill, width = 0.68)
  if (meta$metric != "conteo" && all(c("tasa_li", "tasa_ls") %in% names(d))) {
    p <- p + ggplot2::geom_errorbarh(ggplot2::aes(xmin = tasa_li, xmax = tasa_ls), height = 0.28,
                                     linewidth = 0.3, colour = "black")
  }
  p + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)),
                                  labels = function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)) +
    ggplot2::labs(x = eje_label(meta), y = NULL, caption = cmf_fuente(q)) +
    cmf_theme(estilo = estilo) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = "grey91", linewidth = 0.3),
                   axis.ticks.y = ggplot2::element_blank())
}

#' Mapa coroplético por región o Servicio de Salud de residencia
#'
#' Chile continental (sin territorios insulares). Por Servicio de Salud agrega un recuadro de la Región
#' Metropolitana. Escala por quintiles.
#' @export
cmf_plot_map <- function(q, estilo = "jama", etiqueta = NULL) {
  estilo <- norm_estilo(estilo)
  meta <- attr(q, "cmf_query")
  var <- meta$by[1]
  if (!var %in% c("region", "servicio_salud")) stop("El mapa es por región o Servicio de Salud")
  geo <- cmf_load(if (var == "region") "mapa_regiones" else "mapa_servicios_salud")
  if (is.null(geo) || !nrow(geo)) stop("No hay geometría publicada para el mapa")
  geo <- data.table::copy(geo)
  for (k in c("id", "parte", "orden")) data.table::set(geo, j = k, value = as.integer(geo[[k]]))
  for (k in c("lon", "lat")) data.table::set(geo, j = k, value = as.numeric(geo[[k]]))
  y <- value_col(q)
  d <- data.table::copy(q)[!is.na(get(var)), .(id = as.integer(get(var)), y = get(y))]
  g <- merge(geo, d, by = "id", all.x = TRUE)
  data.table::setorder(g, id, parte, orden)
  rampa <- if (estilo == "grises") grDevices::grey.colors(6, start = 0.92, end = 0.15) else CMF_SECUENCIAL
  br <- unique(stats::quantile(d$y, probs = seq(0, 1, 0.2), na.rm = TRUE, names = FALSE))
  # Decimales suficientes para que los cortes de las clases no se repitan al redondear.
  dig <- 0
  while (dig < 3 && length(br) > 1 && anyDuplicated(round(br, dig))) dig <- dig + 1
  fmt <- function(x) formatC(round(x, dig), format = "f", digits = dig, big.mark = ".", decimal.mark = ",")
  g[, clase := if (length(br) > 2) cut(y, br, include.lowest = TRUE, labels = paste0(fmt(utils::head(br, -1)), "-", fmt(br[-1])))
               else factor(fmt(y))]
  pal <- stats::setNames(rampa[round(seq(1, length(rampa), length.out = max(1, nlevels(g$clase))))], levels(g$clase))
  base <- function(gg, xlim = NULL, ylim = NULL) {
    ggplot2::ggplot(gg, ggplot2::aes(lon, lat, group = interaction(id, parte), fill = clase)) +
      ggplot2::geom_polygon(colour = "white", linewidth = 0.15) +
      ggplot2::scale_fill_manual(values = pal, na.value = "grey85", drop = FALSE,
                                 name = wrap_lbl(etiqueta %||% eje_label(meta), 24),
                                 guide = ggplot2::guide_legend(ncol = 1, title.position = "top")) +
      ggplot2::coord_quickmap(xlim = xlim, ylim = ylim, expand = FALSE) +
      ggplot2::theme_void(base_size = 9, base_family = "sans") +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 8), legend.text = ggplot2::element_text(size = 7.5),
                     plot.caption = ggplot2::element_text(colour = "grey35", hjust = 0, size = 7.5),
                     plot.caption.position = "plot")
  }
  p <- base(g) + ggplot2::theme(legend.position = "right", legend.justification = c(0, 0.5)) +
    ggplot2::labs(caption = cmf_fuente(q))
  if (var == "servicio_salud" && requireNamespace("patchwork", quietly = TRUE)) {
    # Recuadro con los seis Servicios Metropolitanos completos (sin recortar sus polígonos).
    ids_rm <- intersect(9:14, unique(g$id))
    grm <- g[id %in% ids_rm]
    pad <- 0.04
    rm <- base(grm, xlim = range(grm$lon) + c(-pad, pad), ylim = range(grm$lat) + c(-pad, pad)) +
      ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(size = 8, face = "bold")) +
      ggplot2::labs(title = "Región Metropolitana")
    p <- base(g) +
      (rm / patchwork::guide_area() + patchwork::plot_layout(heights = c(1, 1))) +
      patchwork::plot_layout(widths = c(1, 1.3), guides = "collect") +
      patchwork::plot_annotation(caption = cmf_fuente(q),
                                 theme = ggplot2::theme(plot.caption = ggplot2::element_text(colour = "grey35", hjust = 0, size = 7.5)))
  }
  p
}

#' Composición porcentual por una dimensión a lo largo del tiempo
#' @param q Resultado de [cmf_query()] con by = c("anio", <dimensión>).
#' @export
cmf_plot_composition <- function(q, estilo = "jama") {
  estilo <- norm_estilo(estilo)
  meta <- attr(q, "cmf_query")
  var <- setdiff(meta$by, "anio")[1]
  # En la dimensión económica se reparte el valor GRD (o los días cama), no los egresos.
  medida <- if (!is.null(meta$indicador)) eco_medida(meta$indicador) else "n"
  ylab <- c(n = "Egresos, %", valor_grd = "Valor GRD, %", dias_estada = "Días cama, %")[[medida]]
  d <- data.table::copy(q)[!is.na(get(medida)) & !is.na(get(var))]
  if (var == "sexo") d <- d[get(var) %in% c("Hombre", "Mujer")]
  d[, m := get(medida)]
  d[, pct := 100 * m / sum(m), by = anio]
  lv <- ordenar_cats(as.character(d[, .(m = sum(m)), by = var][order(-m)][[var]]))
  d[, .k := factor(get(var), levels = lv)]
  ggplot2::ggplot(d, ggplot2::aes(factor(anio), pct, fill = .k)) +
    ggplot2::geom_col(width = 0.78, colour = "white", linewidth = 0.15) +
    ggplot2::scale_fill_manual(values = pal_cats(lv, estilo)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x, "%")) +
    ggplot2::scale_x_discrete(breaks = function(l) l[seq(1, length(l), by = max(1, round(length(l) / 8)))]) +
    ggplot2::labs(x = NULL, y = ylab, caption = cmf_fuente(q)) +
    cmf_theme(estilo = estilo) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Guarda una figura con dimensiones de revista
#' @param plot Objeto ggplot.
#' @param file Ruta; la extensión define el formato (tiff, png, pdf, svg).
#' @param width "1 col" (85 mm) o "2 col" (174 mm), o un número en mm.
#' @param height Alto en mm (por defecto, 0,7 × ancho; mapas, 1,6 × ancho).
#' @param dpi Resolución para formatos de mapa de bits.
#' @export
cmf_save <- function(plot, file, width = "1 col", height = NULL, dpi = 600) {
  w <- if (is.numeric(width)) width else c("1 col" = 85, "2 col" = 174)[[width]]
  es_mapa <- inherits(plot, "patchwork") || isTRUE(attr(plot, "cmf_mapa")) ||
    (inherits(plot, "ggplot") && inherits(plot$coordinates, "CoordQuickmap"))
  h <- height %||% round(w * if (es_mapa) 1.6 else 0.7)
  ext <- tolower(tools::file_ext(file))
  args <- list(filename = file, plot = plot, width = w, height = h, units = "mm", dpi = dpi, bg = "white")
  if (ext %in% c("tif", "tiff")) args$compression <- "lzw"
  if (ext == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
    stop("Instala svglite para exportar SVG")
  }
  do.call(ggplot2::ggsave, args)
  invisible(file)
}
