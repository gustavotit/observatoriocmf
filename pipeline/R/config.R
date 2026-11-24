# Carga de configuración y diccionarios ---------------------------------------

load_config <- function(root = ".") {
  rd <- function(f) yaml::read_yaml(file.path(root, "config", f), fileEncoding = "UTF-8")
  cfg <- list(
    root    = root,
    sources = rd("sources.yml")$fuentes,
    modules = rd("modules.yml")$modulos,
    colmaps = rd("column_maps.yml"),
    params  = rd("pipeline.yml")
  )
  cfg$recode <- cfg$colmaps$recodificar
  cfg
}

source_versions <- function(cfg, id) cfg$sources[[id]]$versiones %||% list()

sources_by_role <- function(cfg, role) {
  names(Filter(function(s) identical(s$rol, role), cfg$sources))
}

event_modules <- function(cfg) {
  Filter(function(m) !identical(m$tipo, "territorial"), cfg$modules)
}

# Ajuste de un módulo que puede ser común o por fuente ({egresos_deis: ..., grd: ...}).
mod_get <- function(mod, key, fuente) {
  v <- mod[[key]]
  if (is.list(v) && !is.null(names(v)) && any(names(v) %in% c("egresos_deis", "grd"))) v <- v[[fuente]]
  v
}

# Grupos de edad del módulo para esa fuente; si no define, los grupos generales de la app.
module_age_groups_cfg <- function(mod, fuente, params) {
  mod_get(mod, "grupos_edad", fuente) %||% params$edad$grupos_app
}

read_dict <- function(root, file) {
  path <- file.path(root, "dictionaries", file)
  if (!file.exists(path)) return(data.table::data.table())
  data.table::fread(path, encoding = "UTF-8", colClasses = "character", na.strings = c("", "NA"))
}

as_flag <- function(x) toupper(trimws(x %||% "")) %in% c("TRUE", "VERDADERO", "SI", "SÍ", "1")

load_dictionaries <- function(cfg) {
  root <- cfg$root
  dx <- read_dict(root, "cie10_diagnosticos.csv")
  dx[, incluir := as_flag(incluir)]
  if (!"requiere_proc" %in% names(dx)) dx[, requiere_proc := "FALSE"]
  dx[, requiere_proc := as_flag(requiere_proc)]
  if (!"subgrupo" %in% names(dx)) dx[, subgrupo := modulo]
  dx[is.na(subgrupo) | subgrupo == "", subgrupo := modulo]
  dx[, codigo_norm := normalize_cie(codigo)]
  # excluir_si_proc: prefijos CIE-9-MC separados por ";" que descartan el caso si aparecen en cualquier
  # procedimiento (solo GRD; el DEIS no trae procedimientos). P. ej. D16.4 con cirugía craneal "01;02".
  if (!"excluir_si_proc" %in% names(dx)) dx[, excluir_si_proc := NA_character_]

  proc <- read_dict(root, "cie9mc_procedimientos.csv")
  proc[, incluir := as_flag(incluir)]
  proc[, codigo_norm := normalize_cie(codigo)]

  ce <- read_dict(root, "causas_externas.csv")
  ce[, etiologico := as_flag(etiologico)]

  reg <- read_dict(root, "regiones.csv")
  reg[, `:=`(codigo_region = as.integer(codigo_region), orden_norte_sur = as.integer(orden_norte_sur))]

  com <- read_dict(root, "comunas.csv")
  com[, `:=`(region = as.integer(region), codigo_servicio_salud = as.integer(codigo_servicio_salud),
             clave_nombre = gsub(" ", "", normalize_value(nombre_comuna)))]
  legacy <- read_dict(root, "comunas_legacy.csv")
  alias <- read_dict(root, "comunas_alias.csv")
  prov <- read_dict(root, "provincias.csv")
  if (nrow(prov)) prov[, `:=`(region = as.integer(region), codigo_servicio_salud = as.integer(codigo_servicio_salud),
                              clave = gsub(" ", "", normalize_value(nombre_provincia)))]
  ss <- read_dict(root, "servicios_salud.csv")
  ss[, `:=`(codigo_servicio_salud = as.integer(codigo_servicio_salud), region = as.integer(region))]

  std_file <- paste0(cfg$params$tasas$poblacion_estandar %||% "poblacion_estandar_oms", ".csv")
  std <- read_dict(root, std_file)
  std[, `:=`(edad_min = as.integer(edad_min), edad_max = as.integer(edad_max), peso = as_num(peso))]

  precios <- read_dict(root, "grd_precios_base.csv")
  if (nrow(precios)) precios[, `:=`(anio_precio = as.integer(anio_precio), precio_base = as_num(precio_base))]
  clusters <- read_dict(root, "grd_hospital_grupo.csv")
  if (nrow(clusters)) clusters[, `:=`(cod_hospital = as.integer(cod_hospital), anio_precio = as.integer(anio_precio))]

  cw <- read_dict(root, "crosswalk_cie9mc_mai.csv")
  if (nrow(cw)) {
    cw[, cie9mc_norm := normalize_cie(cie9mc)]
    cw[, cantidad := as_num(cantidad)]
    cw[is.na(cantidad), cantidad := 1]
  }

  list(dx = dx, proc = proc, ce = ce, regiones = reg, comunas = com, comunas_legacy = legacy,
       comunas_alias = alias, provincias = prov, servicios = ss, estandar = std, precios = precios, clusters = clusters,
       crosswalk = cw)
}

dictionaries_hash <- function(cfg) {
  hash_files(list.files(file.path(cfg$root, "dictionaries"), full.names = TRUE))
}
