`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

SOURCES_HEADER <- paste(
  "# Registro de fuentes del Observatorio CMF Chile (editado desde el panel de administración).",
  "# Cada versión trae 'url' (la descarga GitHub Actions) o 'archivo' (en data-small/).",
  "# Ver README para el detalle de cada campo.", "", sep = "\n")

DICCIONARIOS <- c(
  "Diagnósticos CIE-10 por módulo y subgrupo (DEIS y GRD)" = "dictionaries/cie10_diagnosticos.csv",
  "Procedimientos CIE-9-MC (GRD)" = "dictionaries/cie9mc_procedimientos.csv",
  "Causas externas (mecanismo)" = "dictionaries/causas_externas.csv",
  "GRD: precios base por año y grupo" = "dictionaries/grd_precios_base.csv",
  "GRD: clúster de precio de cada hospital" = "dictionaries/grd_hospital_grupo.csv",
  "Crosswalk CIE-9-MC → arancel MAI" = "dictionaries/crosswalk_cie9mc_mai.csv",
  "Comunas (región y Servicio de Salud)" = "dictionaries/comunas.csv",
  "Códigos comunales antiguos" = "dictionaries/comunas_legacy.csv",
  "Alias de nombres de comuna" = "dictionaries/comunas_alias.csv",
  "Servicios de Salud" = "dictionaries/servicios_salud.csv",
  "Regiones" = "dictionaries/regiones.csv",
  "Población estándar (OMS)" = "dictionaries/poblacion_estandar_oms.csv"
)

# "2019-2024" -> 2019:2024 ; "2019, 2021" -> c(2019, 2021)
parse_years <- function(x) {
  x <- gsub("\\s", "", x %||% "")
  if (!nzchar(x)) return(list())
  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  yrs <- unlist(lapply(parts, function(p) {
    if (grepl("^[0-9]{4}-[0-9]{4}$", p)) {
      r <- as.integer(strsplit(p, "-")[[1]]); seq(r[1], r[2])
    } else suppressWarnings(as.integer(p))
  }))
  as.list(sort(unique(stats::na.omit(yrs))))
}

safe_version <- function(x) gsub("[^A-Za-z0-9._-]", "_", trimws(x))

csv_text <- function(df) {
  con <- textConnection("out", "w", local = TRUE)
  utils::write.csv(df, con, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  close(con)
  paste(out, collapse = "\n")
}

read_csv_text <- function(text) {
  utils::read.csv(text = text, colClasses = "character", na.strings = character(0),
                  check.names = FALSE, encoding = "UTF-8")
}

versions_table <- function(src) {
  v <- src$versiones %||% list()
  if (!length(v)) return(data.frame(version = character(), anios = character(), origen = character(),
                                    fecha_carga = character(), nota = character()))
  data.frame(
    version = vapply(v, function(x) as.character(x$version %||% ""), ""),
    anios = vapply(v, function(x) paste(range(unlist(x$anios %||% NA)), collapse = "-"), ""),
    origen = vapply(v, function(x) x$url %||% x$archivo %||% "", ""),
    fecha_carga = vapply(v, function(x) as.character(x$fecha_carga %||% ""), ""),
    nota = vapply(v, function(x) as.character(x$nota %||% ""), ""),
    stringsAsFactors = FALSE
  )
}
