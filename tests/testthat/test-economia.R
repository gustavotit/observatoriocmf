# Dimensión económica y estudios importados (usa los datos publicados si están).

hay_cubo <- function() {
  d <- tryCatch(cmf_load("cubo"), error = function(e) NULL)
  !is.null(d) && nrow(d) && "valor_grd" %in% names(d) && any(d$fuente == "grd")
}

test_that("cmf_query_eco suma el mismo valor GRD que cmf_query", {
  skip_if_not(hay_cubo())
  m <- cmf_modules()$id[vapply(cmf_modules()$id, cmf_economia_disponible, TRUE)][1]
  skip_if(is.na(m))
  q <- cmf_query(m, fuente = "grd", by = "anio", metric = "conteo")
  e <- cmf_query_eco(m, "valor_total", by = "anio")
  expect_equal(e$valor, q$valor_grd / 1e6)
  expect_equal(attr(e, "cmf_query")$indicador, "valor_total")
  expect_match(cmf_code(e), "cmf_query_eco")
  expect_false(cmf_economia_disponible("oferta"))
})

test_that("el importador de estudios rechaza identificadores y suprime celdas pequeñas", {
  raiz <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
  script <- file.path(raiz, "pipeline", "importar_estudio.R")
  skip_if_not(file.exists(script))
  tmp <- tempfile("estudio_"); dir.create(file.path(tmp, "config"), recursive = TRUE); dir.create(file.path(tmp, "out"))
  writeLines(c("estudios:", "  - id: prueba", "    modulo: trauma", "    tablas:",
               "      territorio:", "        archivo: t.csv",
               "        columnas: {servicio_salud: ss, \"n\": casos, valor_grd: v}",
               "  - id: mala", "    modulo: trauma", "    tablas:",
               "      malo:", "        archivo: m.csv", "        columnas: {servicio_salud: ss, hospital_id: hospital_id}"),
             file.path(tmp, "config", "estudios_economicos.yml"))
  data.table::fwrite(data.table::data.table(ss = 1:3, casos = c(100, 3, 50), v = c(1e6, 2e4, 5e5)), file.path(tmp, "out", "t.csv"))
  data.table::fwrite(data.table::data.table(ss = 1, hospital_id = 112100), file.path(tmp, "out", "m.csv"))
  rs <- file.path(R.home("bin"), "Rscript")
  owd <- setwd(tmp); on.exit(setwd(owd))
  ok <- suppressWarnings(system2(rs, c(shQuote(script), "prueba", "out", "dest"), stdout = TRUE, stderr = TRUE))
  expect_true(any(grepl("Listo", ok)))
  malo <- suppressWarnings(system2(rs, c(shQuote(script), "mala", "out", "dest"), stdout = TRUE, stderr = TRUE))
  expect_true(any(grepl("identificador", malo)))
  expect_false(file.exists(file.path(tmp, "dest", "estudio_mala_malo.csv")))
  t <- data.table::fread(file.path(tmp, "dest", "estudio_prueba_territorio.csv"))
  expect_true(is.na(t$valor_grd[2]))
  expect_equal(t$n[1], 100)
})
