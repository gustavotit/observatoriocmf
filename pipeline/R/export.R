# Exportación de tablas agregadas y manifiesto de versión ----------------------

package_version_str <- function(root) {
  d <- file.path(root, "DESCRIPTION")
  if (!file.exists(d)) return("0.0.0")
  unname(read.dcf(d, fields = "Version")[1, 1])
}

git_sha <- function() {
  sha <- Sys.getenv("GITHUB_SHA")
  if (nzchar(sha)) return(sha)
  out <- tryCatch(system("git rev-parse HEAD", intern = TRUE, ignore.stderr = TRUE),
                  error = function(e) NA_character_, warning = function(w) NA_character_)
  if (length(out)) out[1] else NA_character_
}

export_outputs <- function(tables, cfg, validacion, out_dir = "inst/extdata") {
  out <- file.path(cfg$root, out_dir)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  old <- list.files(out, pattern = "\\.(parquet|csv\\.gz)$", full.names = TRUE)
  unlink(old)
  fmt <- output_format(cfg$params)

  filas <- list()
  for (nm in names(tables)) {
    dt <- tables[[nm]]
    if (is.null(dt) || !nrow(dt)) next
    write_table(dt, out, nm, fmt)
    filas[[nm]] <- nrow(dt)
  }

  fuentes <- lapply(cfg$sources, function(s) {
    list(nombre = s$nombre,
         versiones = lapply(s$versiones %||% list(), function(v)
           # Rutas locales (corrida anual en el computador): solo el nombre del archivo, no la carpeta.
           list(version = v$version, anios = v$anios, url = v$url,
                archivo = if (is.null(v$archivo)) NULL else if (grepl("^(/|[A-Za-z]:)", v$archivo)) basename(v$archivo) else v$archivo,
                sha256 = v$sha256, fecha_carga = v$fecha_carga)))
  })

  manifest <- list(
    version = package_version_str(cfg$root),
    construido = format(Sys.time(), tz = "UTC", usetz = TRUE),
    commit = git_sha(),
    formato = fmt,
    tablas = filas,
    anios = if (!is.null(tables$cubo) && nrow(tables$cubo))
      lapply(split(tables$cubo$anio, tables$cubo$fuente), function(a) range(a, na.rm = TRUE)) else NULL,
    parametros = cfg$params,
    fuentes = fuentes,
    n_errores = length(validacion$errores),
    n_advertencias = length(validacion$advertencias)
  )
  jsonlite::write_json(manifest, file.path(out, "manifest.json"), auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  jsonlite::write_json(validacion, file.path(out, "validacion.json"), auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  file.copy(file.path(cfg$root, "config", "modules.yml"), file.path(out, "modules.yml"), overwrite = TRUE)
  for (yml in c("publicaciones.yml", "estudios_economicos.yml")) {
    f <- file.path(cfg$root, "config", yml)
    if (file.exists(f)) file.copy(f, file.path(out, yml), overwrite = TRUE)
  }
  log_msg("Exportado a ", out_dir, " (", fmt, "): ", paste(names(filas), collapse = ", "))
  invisible(manifest)
}
