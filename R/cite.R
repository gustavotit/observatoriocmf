# Cita, párrafo de métodos y código reproducible ----------------------------------

pub_meta <- function() cmf_manifest()$parametros$publicacion %||% list()

FUENTE_CORTA <- c(egresos_deis = "DEIS", grd = "GRD", defunciones_deis = "Defunciones DEIS")

describe_filters <- function(q) {
  meta <- attr(q, "cmf_query")
  f <- meta$filtros
  anios <- if (length(f$anio)) paste0(min(f$anio), "-", max(f$anio)) else "todos los años"
  region <- if (length(f$region)) {
    reg <- cmf_region_names(); paste(reg[codigo_region %in% f$region]$nombre, collapse = ", ")
  } else "todas las regiones"
  ss <- if (length(f$servicio_salud)) {
    s <- cmf_service_names(); paste("SS", paste(s[codigo_servicio_salud %in% f$servicio_salud]$nombre, collapse = ", "))
  }
  sexo <- if (length(f$sexo) == 1) tolower(paste0(f$sexo, "s")) else "ambos sexos"
  edad <- if (length(f$grupo_edad)) paste(f$grupo_edad, collapse = ", ") else NULL
  sg <- if (length(f$agrupacion)) paste(f$agrupacion, collapse = " + ")
        else if (length(f$subgrupo)) paste(f$subgrupo, collapse = " + ")
  extra <- c(
    if (length(f$prevision)) paste("previsión", paste(f$prevision, collapse = "/")),
    if (length(f$sector)) paste("sector", paste(tolower(f$sector), collapse = "/")),
    if (length(f$mecanismo)) paste("mecanismo", paste(tolower(f$mecanismo), collapse = "/")),
    if (length(f$actividad)) paste(f$actividad, collapse = "/")
  )
  paste(c(FUENTE_CORTA[[f$fuente]], sg, anios, region, ss, sexo, edad, extra), collapse = "; ")
}

cmf_caption <- function(q, width = 70) {
  txt <- paste0("Fuente: Observatorio CMF Chile v", attr(q, "cmf_query")$version, " (",
                describe_filters(q), ").")
  paste(strwrap(txt, width = width), collapse = "\n")
}

#' Cita de una consulta o figura (formato AMA)
#'
#' Usa un único DOI, el de concepto de Zenodo, para que todas las citas se acumulen en el mismo registro
#' aunque los datos se actualicen cada año. La versión y la fecha de consulta identifican los datos exactos.
#' Si existe el artículo de referencia (letter), se antepone como cita principal.
#' @param incluir_articulo Antepone el artículo de referencia cuando está configurado.
#' @export
cmf_cite <- function(q = NULL, fecha = Sys.Date(), incluir_articulo = TRUE) {
  pm <- pub_meta()
  doi <- pm$doi %||% "10.5281/zenodo.[pendiente]"
  base <- paste0(pm$autores %||% "Sáenz-Ravello G", ". ", pm$titulo %||% "Observatorio CMF Chile",
                 " [versión de datos ", cmf_version(), "].")
  modulo <- if (!is.null(q)) {
    mods <- cmf_modules()
    id <- attr(q, "cmf_query")$filtros$modulo
    paste0(" Módulo ", paste(mods$nombre[mods$id %in% id], collapse = ", "), ": ", describe_filters(q), ".")
  } else ""
  url <- if (!is.null(pm$url_app)) paste0(" ", pm$url_app, ".") else ""
  obs <- paste0(base, modulo, url, " doi:", doi, ". Consultado el ", format(fecha, "%d-%m-%Y"), ".")
  art <- pm$articulo_referencia
  if (isTRUE(incluir_articulo) && !is.null(art) && nzchar(art)) paste0(art, "\n", obs) else obs
}

#' Párrafo de métodos para la consulta
#' @export
cmf_methods <- function(q) {
  meta <- attr(q, "cmf_query")
  f <- meta$filtros
  mod <- cmf_module(f$modulo)
  dx <- cmf_dictionary(f$modulo, "diagnosticos", solo_incluidos = TRUE)
  if (length(f$subgrupo)) dx <- dx[subgrupo %in% f$subgrupo]
  codigos <- if (!is.null(dx) && nrow(dx)) {
    paste0(paste(dx$codigo, collapse = ", "),
           if (any(as.logical(dx$requiere_proc))) " (los marcados como tales, solo con un procedimiento del módulo)" else "")
  } else "[códigos]"
  pos <- mod$posicion[[f$fuente]] %||% "dx1"
  donde <- if (pos == "cualquiera") "en cualquier posición diagnóstica" else "como diagnóstico principal"
  fuente <- c(egresos_deis = "egresos hospitalarios de los datos abiertos del Departamento de Estadísticas e Información de Salud (DEIS)",
              grd = if ("CMA" %in% (f$actividad %||% unlist(mod_get(mod, "actividades", "grd"))))
                      "egresos de hospitalización y de cirugía mayor ambulatoria (CMA) de las bases GRD públicas (FONASA/MINSAL)"
                    else "egresos de hospitalización de las bases GRD públicas (FONASA/MINSAL)")[[f$fuente]]
  anios <- if (length(f$anio)) paste0(" entre ", min(f$anio), " y ", max(f$anio)) else ""
  universo <- if (!is.null(mod$universo[[f$fuente]])) {
    paste0(" dentro del universo de egresos con diagnóstico principal ", paste(unlist(mod$universo[[f$fuente]]), collapse = ", "))
  } else ""
  ed <- mod_get(mod, "edad", f$fuente)
  edad <- if (!is.null(ed)) {
    if ((ed$max %||% 150) == 0) " en menores de 1 año" else paste0(" de ", ed$min %||% 0, " a ", ed$max, " años")
  } else ""
  geo <- "La región y el Servicio de Salud corresponden a la residencia, reconstruida desde la comuna (incluidos los códigos anteriores a la creación de Arica y Parinacota, Los Ríos y Ñuble)."
  tasa <- switch(meta$metric,
    conteo = "Se reportan conteos absolutos.",
    cruda = "Las tasas crudas usan como denominador las estimaciones y proyecciones de población del INE por comuna (base Censo 2017), con intervalos de confianza exactos de Poisson.",
    ajustada_chile = "Las tasas se estandarizaron por edad por el método directo con la distribución etaria chilena agrupada 2002-2025 (población INE) en los grupos etarios del módulo, con intervalos de confianza gamma de Fay-Feuer; los egresos sin edad conocida se excluyeron del numerador.",
    ajustada = "Las tasas se estandarizaron por edad por el método directo con la población estándar mundial de la OMS (2000-2025; grupos desde 80 años agregados), con denominadores INE por comuna e intervalos de confianza gamma de Fay-Feuer.",
    nacidos_vivos = paste0("El denominador es la población INE de 0 años calibrada a los nacidos vivos regionales del mismo año (DEIS); intervalos de confianza exactos de Poisson. La tasa expresa egresos por ", format(meta$por %||% 1000, big.mark = ".", decimal.mark = ","), " nacidos vivos, no prevalencia al nacimiento."),
    universo = "Las tasas se expresan por 1.000 egresos del universo de referencia del módulo, con intervalos de confianza exactos de Poisson.",
    egresos = "Las tasas se expresan por 100.000 egresos hospitalarios totales de la fuente (análisis de sensibilidad del denominador), con intervalos de confianza exactos de Poisson.")
  grupos <- mod_get(mod, "grupos_edad", f$fuente)
  edad_txt <- if (length(grupos) > 1) {
    paste0(" Los grupos etarios (", paste(vapply(grupos, function(g) as.character(g$etiqueta), ""), collapse = ", "),
           ") se asignaron por el límite inferior del tramo de edad registrado",
           if (f$fuente == "egresos_deis") " (el DEIS publica GRUPO_EDAD en tramos, decenales en la mayoría de los años)." else ".")
  } else ""
  mec <- if (isTRUE(mod$desagregar_mecanismo)) {
    if (f$fuente == "egresos_deis") " El mecanismo se clasificó desde el diagnóstico secundario (DIAG2) en seis grupos de causa externa; los egresos sin código V-Y clasificable quedaron como no especificados."
    else " El mecanismo corresponde al primer código etiológico V-Y en las posiciones diagnósticas secundarias."
  } else ""
  sev <- if (identical(mod_get(mod, "severidad", f$fuente), "icdpic")) {
    " La severidad anatómica se estimó desde todas las posiciones diagnósticas con ICDPIC-R 1.0.1 (tabla base, método NIS, calc_method = 1), como RISS derivado de CIE-10 (no un ISS clínico); trauma mayor = RISS ≥ 16."
  } else ""
  valor <- if (f$fuente == "grd") {
    est <- cmf_manifest()$parametros$costos$estructura_precios_grd %||% 2024
    ref <- referencia_precios()
    paste0(" El valor GRD corresponde al peso GRD por el precio base del hospital según su clúster de precios, con la estructura de precios ", est,
           " y desde la perspectiva del financiador público",
           if (!is.null(ref)) paste0(", expresado en ", moneda_lbl(TRUE), " con el IPC del Banco Central (índice del mes sobre el índice promedio de ", est, ")") else "",
           ". No es un costo hospitalario observado.")
  } else ""
  eco <- if (!is.null(meta$indicador) && meta$indicador %in% ECO_INDICADORES) {
    switch(meta$indicador,
      valor_total = " Se reporta el valor GRD total en millones de pesos chilenos.",
      valor_medio = " Se reporta el valor GRD medio por egreso con valor (miles de pesos chilenos).",
      valor_100k = " Se reporta el valor GRD por 100.000 habitantes (millones de pesos chilenos), con las estimaciones y proyecciones de población del INE por comuna como denominador.",
      dias_cama = " Se reportan los días cama (suma de la estada de los egresos con estada conocida).",
      estada_media = " Se reporta la estada media en días entre los egresos con estada conocida.")
  } else ""
  if (nzchar(eco)) tasa <- NULL
  paste0("Se analizaron ", fuente, anios, edad, " con códigos CIE-10 ", codigos, " ", donde, universo,
         ", obtenidos mediante el Observatorio CMF Chile (versión ", meta$version, "). ", geo, edad_txt, mec, if (length(tasa)) paste0(" ", tasa), sev, valor, eco,
         perdida_datos(q),
         " La unidad de análisis es el egreso, no la persona. Las celdas con menos de ", meta$n_min,
         " casos fueron suprimidas.")
}

# Pérdida de datos de la selección, en una frase: egresos sin región o sin edad, que se excluyen solo de esas
# desagregaciones (los totales y tendencias los incluyen).
perdida_datos <- function(q) {
  f <- attr(q, "cmf_query")$filtros
  cubo <- cmf_load("cubo")
  if (is.null(cubo) || !nrow(cubo)) return("")
  d <- filter_table(cubo, f[c("modulo", "fuente", "anio", "subgrupo", "sexo", "prevision", "sector", "mecanismo")])
  if (!nrow(d) || !sum(d$n)) return("")
  pc <- function(x) formatC(100 * sum(d$n[x]) / sum(d$n), format = "f", digits = 1, decimal.mark = ",")
  sin_reg <- is.na(d$region); sin_edad <- is.na(d$grupo_edad)
  partes <- c(if (any(sin_reg)) paste0("el ", pc(sin_reg), "% sin región de residencia"),
              if (any(sin_edad)) paste0("el ", pc(sin_edad), "% sin edad registrada"))
  if (!length(partes)) return("")
  paste0(" Del total de egresos, ", paste(partes, collapse = " y "),
         " se excluyó solo de las desagregaciones por esas variables.")
}

#' Código R que reproduce la consulta (y la figura)
#' @param plot "trend", "bars", "composition" o NULL.
#' @export
cmf_code <- function(q, plot = NULL, its_break = NULL, file = NULL, width = "1 col") {
  meta <- attr(q, "cmf_query")
  f <- meta$filtros
  dep <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = "")
  eco <- !is.null(meta$indicador) && meta$indicador %in% ECO_INDICADORES
  args <- c(paste0('"', f$modulo, '"'),
            if (eco) paste0('indicador = "', meta$indicador, '"') else paste0('fuente = "', f$fuente, '"'))
  if (length(f$anio)) args <- c(args, paste0("years = ", min(f$anio), ":", max(f$anio)))
  principales <- unlist(mod_get(cmf_module(f$modulo), "subgrupos_principales", f$fuente))
  if (length(f$subgrupo) && !setequal(f$subgrupo, principales)) args <- c(args, paste0("subgrupo = ", dep(f$subgrupo)))
  if (!length(f$subgrupo)) args <- c(args, 'subgrupo = "todos"')
  for (k in c("agrupacion", "region", "servicio_salud", "sexo", "grupo_edad", "prevision", "sector", "mecanismo",
              "actividad")) {
    if (length(f[[k]])) args <- c(args, paste0(k, " = ", dep(f[[k]])))
  }
  args <- c(args, paste0("by = ", dep(meta$by)), if (!eco) paste0('metric = "', meta$metric, '"'))
  fn_q <- if (eco) "cmf_query_eco(" else "cmf_query("
  code <- paste0("library(observatoriocmf)  # datos v", meta$version, "\n\n",
                 "q <- ", fn_q, paste(args, collapse = paste0(",\n", strrep(" ", 5 + nchar(fn_q)))), ")")
  if (!is.null(plot)) {
    fn <- switch(plot,
                 trend = paste0("cmf_plot_trend(q", if (!is.null(its_break)) paste0(", its_break = ", its_break), ")"),
                 bars = "cmf_plot_bars(q)",
                 map = "cmf_plot_map(q)",
                 composition = "cmf_plot_composition(q)")
    code <- paste0(code, "\np <- ", fn)
    if (!is.null(file)) code <- paste0(code, '\ncmf_save(p, "', file, '", width = "', width, '")')
  }
  code
}
