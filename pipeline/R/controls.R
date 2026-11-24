# Controles contra las publicaciones -------------------------------------------------------------
#
# Tras exportar, reconsulta las salidas con el mismo código del paquete (R/) y compara con las cifras
# publicadas de dictionaries/controles_publicaciones.csv. El resultado queda en inst/extdata/controles.json
# y en la app (pestaña Métodos): así cada versión documenta que reproduce los artículos.

run_controls <- function(cfg, out_dir = "inst/extdata") {
  f <- file.path(cfg$root, "dictionaries", "controles_publicaciones.csv")
  if (!file.exists(f)) return(invisible(NULL))
  ctl <- data.table::fread(f, encoding = "UTF-8")
  env <- new.env()
  for (r in list.files(file.path(cfg$root, "R"), pattern = "\\.R$", full.names = TRUE)) {
    eval(parse(file = r, encoding = "UTF-8", keep.source = FALSE), envir = env)
  }
  old <- options(cmf.data_dir = file.path(cfg$root, out_dir))
  on.exit(options(old), add = TRUE)
  res <- lapply(seq_len(nrow(ctl)), function(i) {
    x <- ctl[i]
    sg <- if (identical(x$subgrupo, "principales") || !nzchar(x$subgrupo)) NULL else x$subgrupo
    obtenido <- tryCatch({
      q <- env$cmf_query(x$modulo, fuente = x$fuente, years = x$anio_desde:x$anio_hasta, subgrupo = sg,
                         by = character(0), metric = x$metrica, suppress = FALSE)
      v <- if (x$metrica == "conteo") q$n else q$tasa
      if (!length(v)) NA_real_ else as.numeric(v[1])
    }, error = function(e) NA_real_)
    anios_ok <- all(x$anio_desde:x$anio_hasta %in% unique(env$cmf_load("cubo")[fuente == x$fuente]$anio))
    estado <- if (!anios_ok || is.na(obtenido)) "sin datos" else
      if (abs(obtenido - x$valor) <= x$tolerancia + 1e-9) "coincide" else "difiere"
    c(as.list(x), list(obtenido = obtenido, estado = estado))
  })
  out <- data.table::rbindlist(res, fill = TRUE)
  jsonlite::write_json(out, file.path(cfg$root, out_dir, "controles.json"), auto_unbox = TRUE, pretty = TRUE,
                       digits = NA)
  tab <- table(factor(out$estado, levels = c("coincide", "difiere", "sin datos")))
  log_msg("Controles contra publicaciones: ", paste(names(tab), tab, sep = " ", collapse = ", "))
  for (i in which(out$estado == "difiere")) {
    log_msg("  Difiere: ", out$referencia[i], ": publicado ", out$valor[i], ", observatorio ", round(out$obtenido[i], 3))
  }
  invisible(out)
}
