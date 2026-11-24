# Acceso a los datos publicados --------------------------------------------------

.cmf_env <- new.env(parent = emptyenv())

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Carpeta con los datos agregados
#'
#' Por defecto, los datos que vienen con el paquete. Se puede cambiar con
#' `options(cmf.data_dir = "...")` o la variable de entorno `CMF_DATA_DIR`.
#' @export
cmf_data_dir <- function() {
  d <- getOption("cmf.data_dir", Sys.getenv("CMF_DATA_DIR", ""))
  if (nzchar(d)) return(d)
  system.file("extdata", package = "observatoriocmf")
}

#' Carga una tabla publicada (cubo, poblacion, universo, diccionarios, etc.)
#' @param name Nombre de la tabla, p. ej. "cubo" o "dic_diagnosticos".
#' @export
cmf_load <- function(name) {
  dir <- cmf_data_dir()
  key <- paste(dir, name)
  if (!is.null(.cmf_env[[key]])) return(.cmf_env[[key]])
  pq <- file.path(dir, paste0(name, ".parquet"))
  cs <- file.path(dir, paste0(name, ".csv.gz"))
  dt <- if (file.exists(pq)) {
    # arrow es opcional (Suggests): se resuelve en tiempo de ejecución.
    read_parquet <- get("read_parquet", envir = asNamespace("arrow"))
    data.table::as.data.table(read_parquet(pq))
  } else if (file.exists(cs)) {
    cmf_fread(cs, encoding = "UTF-8", na.strings = c("", "NA"),
              colClasses = list(character = intersect(c("codigo_comuna", "comuna", "codigo", "subgrupo"),
                                                      names(cmf_fread(cs, nrows = 0)))))
  } else {
    NULL
  }
  .cmf_env[[key]] <- dt
  dt
}

#' Manifiesto de la versión de datos (fecha, fuentes, parámetros)
#' @export
cmf_manifest <- function() {
  f <- file.path(cmf_data_dir(), "manifest.json")
  if (!file.exists(f)) return(list(version = "sin datos"))
  jsonlite::read_json(f)
}

#' Versión de los datos
#' @export
cmf_version <- function() cmf_manifest()$version %||% "sin datos"

modules_raw <- function() {
  f <- file.path(cmf_data_dir(), "modules.yml")
  if (!file.exists(f)) return(list())
  yaml::read_yaml(f, fileEncoding = "UTF-8")$modulos
}

#' Configuración completa de un módulo (subgrupos principales, edad, métricas, universo)
#' @export
cmf_module <- function(id) {
  m <- Filter(function(x) identical(x$id, id), modules_raw())
  if (!length(m)) stop("Módulo desconocido: ", id)
  m[[1]]
}

#' Módulos temáticos disponibles
#' @export
cmf_modules <- function() {
  m <- modules_raw()
  if (!length(m)) return(data.frame())
  data.frame(
    id = vapply(m, `[[`, "", "id"),
    tipo = vapply(m, function(x) x$tipo %||% "eventos", ""),
    nombre = vapply(m, `[[`, "", "nombre"),
    descripcion = vapply(m, function(x) x$descripcion %||% "", ""),
    estado = vapply(m, function(x) x$estado %||% "en_construccion", ""),
    fuentes = vapply(m, function(x) paste(unlist(x$fuentes), collapse = ", "), ""),
    mecanismo = vapply(m, function(x) isTRUE(x$desagregar_mecanismo), TRUE),
    color = vapply(m, function(x) x$color %||% "#80796B", ""),
    stringsAsFactors = FALSE
  )
}

#' Subgrupos de un módulo (y cuáles forman la definición principal)
#' @export
cmf_subgroups <- function(modulo, fuente = "egresos_deis") {
  d <- cmf_dictionary(modulo, "diagnosticos", solo_incluidos = TRUE)
  m <- cmf_module(modulo)
  sg <- unique(d$subgrupo)
  cubo <- cmf_load("cubo")
  if (!is.null(cubo) && nrow(cubo)) {
    sel <- cubo$modulo == modulo & cubo$fuente == fuente
    sg <- unique(c(sg, unique(cubo$subgrupo[sel])))
  }
  data.frame(subgrupo = sg, principal = sg %in% (unlist(mod_get(m, "subgrupos_principales", fuente)) %||% sg),
             stringsAsFactors = FALSE)
}

#' Diccionario de códigos con descripciones
#' @param modulo Id del módulo (opcional).
#' @param tipo "diagnosticos" (CIE-10) o "procedimientos" (CIE-9-MC, GRD).
#' @param solo_incluidos Mostrar solo los códigos que entran en la definición de caso.
#' @export
cmf_dictionary <- function(modulo = NULL, tipo = c("diagnosticos", "procedimientos"),
                           solo_incluidos = FALSE) {
  tipo <- match.arg(tipo)
  d <- cmf_load(paste0("dic_", tipo))
  if (is.null(d)) return(NULL)
  d <- data.table::copy(d)
  mod_sel <- modulo
  if (!is.null(mod_sel)) d <- d[d$modulo %in% mod_sel]
  if (solo_incluidos) d <- d[as.logical(d$incluir)]
  d[]
}

cmf_region_names <- function() {
  r <- cmf_load("dic_regiones")
  if (is.null(r)) return(data.table::data.table(codigo_region = integer(), nombre = character(),
                                                orden_norte_sur = integer()))
  r[, .(codigo_region = as.integer(codigo_region), nombre, orden_norte_sur = as.integer(orden_norte_sur))]
}

cmf_service_names <- function() {
  s <- cmf_load("dic_servicios_salud")
  if (is.null(s)) return(data.table::data.table(codigo_servicio_salud = integer(), nombre = character(),
                                                region = integer()))
  s[, .(codigo_servicio_salud = as.integer(codigo_servicio_salud), nombre, region = as.integer(region))]
}


# fread de .csv.gz sin depender de R.utils (que fread exige para descomprimir): si no está, se descomprime con
# una conexión gzfile de R base a un archivo temporal.
cmf_fread <- function(path, ...) {
  if (!grepl("\\.gz$", path) || requireNamespace("R.utils", quietly = TRUE)) return(data.table::fread(path, ...))
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  inp <- gzfile(path, "rb"); out <- file(tmp, "wb")
  repeat {
    b <- readBin(inp, "raw", 1e7)
    if (!length(b)) break
    writeBin(b, out)
  }
  close(inp); close(out)
  data.table::fread(tmp, ...)
}
