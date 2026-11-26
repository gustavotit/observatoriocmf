#!/usr/bin/env Rscript
# Prepara la carpeta inst/app para desplegar: copia las funciones del paquete (R/) y los datos
# publicados (inst/extdata/). Shiny carga automáticamente los archivos de app/R/.

app <- file.path("inst", "app")
unlink(file.path(app, c("R", "data")), recursive = TRUE)
dir.create(file.path(app, "R"), recursive = TRUE)
dir.create(file.path(app, "data"), recursive = TRUE)

ok_r <- file.copy(list.files("R", pattern = "\\.R$", full.names = TRUE), file.path(app, "R"))
datos <- list.files(file.path("inst", "extdata"), full.names = TRUE)
ok_d <- file.copy(datos, file.path(app, "data"))
# Artículos: siempre desde config/, para que una edición en el panel llegue con "Solo redesplegar".
for (yml in c("publicaciones.yml", "estudios_economicos.yml")) {
  f <- file.path("config", yml)
  if (file.exists(f)) file.copy(f, file.path(app, "data"), overwrite = TRUE)
}
cat("Funciones copiadas:", sum(ok_r), "; archivos de datos:", sum(ok_d), "\n")
if (!length(datos)) cat("Aviso: no hay datos en inst/extdata; la app mostrará 'sin datos'\n")
# rsconnect descarta del despliegue todo archivo llamado "manifest.json" (aunque esté en data/): en la app va como
# manifiesto.json, que cmf_manifest() lee primero.
man0 <- file.path(app, "data", "manifest.json")
man <- file.path(app, "data", "manifiesto.json")
if (file.exists(man0)) {
  m <- jsonlite::read_json(man0)
  # Con "Solo redesplegar" + versión nueva no se recalcula: la versión publicada sigue a DESCRIPTION.
  v <- unname(read.dcf("DESCRIPTION", fields = "Version")[1, 1])
  if (!identical(m$version, v)) cat("Versión de datos publicada:", v, "\n")
  m$version <- v
  # La cita (DOI de concepto, artículo de referencia, URL) se edita en el panel y llega con "Solo redesplegar".
  pub <- tryCatch(yaml::read_yaml(file.path("config", "pipeline.yml"))$publicacion, error = function(e) NULL)
  if (!is.null(pub)) m$parametros$publicacion <- pub
  jsonlite::write_json(m, man, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  unlink(man0)
}
