# Pruebas de las funciones del pipeline (se ejecutan desde el repositorio, no del paquete instalado).

pipeline_dir <- file.path(testthat::test_path(), "..", "..", "pipeline", "R")
skip_if_not(dir.exists(pipeline_dir), "Pruebas del pipeline: solo desde el repositorio")
for (f in list.files(pipeline_dir, pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
library(data.table)
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
cfg <- load_config(root)
dicts <- load_dictionaries(cfg)

test_that("normaliza códigos CIE", {
  expect_equal(normalize_cie(c("S02.6", " s026 ", "76.76", "", NA)), c("S026", "S026", "7676", NA, NA))
})

test_that("coincidencia por prefijo elige el código más específico", {
  dict <- normalize_cie(c("S02.6", "S02", "J36"))
  expect_equal(match_prefix(c("S0260", "S029", "J369", "K122", NA), dict), c(1L, 2L, 3L, NA, NA))
})

test_that("rangos del universo de referencia", {
  expect_equal(in_ranges(c("S026", "T140", "T150", "T250", "V01", NA), c("S00-S99", "T00-T14")),
               c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
})

test_that("mecanismo: primer código etiológico en posiciones secundarias", {
  dx <- list(c("S026", "S026", "S022", "S026"), c("Y04", "Y45", "S061", NA), c(NA, "W01", "V43", NA))
  expect_equal(classify_mechanism(dx, dicts$ce),
               c("Agresión", "Caída", "Transporte", "Sin causa externa registrada"))
})

test_that("etiquetas de edad del DEIS y tramos del GRD", {
  e <- parse_age_label(c("Menor de 1 año", "1 a 4 años", "5 a 9 años", "80 y más años", "5 a 14 años", "28 días a 11 meses"))
  expect_equal(e$lo, c(0, 1, 5, 80, 5, 0)); expect_equal(e$hi, c(0, 4, 9, 150, 14, 0))
  expect_equal(age_group_5(e$lo, e$hi), c("0-4", "0-4", "5-9", "80+", NA, "0-4"))
})

test_that("edad exacta desde fechas (sin confundir formatos)", {
  expect_equal(age_from_dates(c("15-06-1960", "01-01-2020"), c("14-06-2020", "01-06-2020")), c(59, 0))
})

test_that("comunas: códigos actuales, antiguos y nombres", {
  h <- harmonize_commune(c("1402", "08401", "Coyhaique", "VIÑA DEL MAR", "99", "xx"), dicts)
  expect_equal(h$codigo, c("01402", "16101", "11101", "05109", NA, NA))
  expect_equal(h$fuente[1:3], c("código", "código antiguo", "nombre"))
})

test_that("región desde la comuna con respaldo no ambiguo", {
  r <- harmonize_region(c("01402", NA, NA, NA), c("1", "8", "8", "13"), c(2010, 2015, 2020, 2015), dicts, cfg$params)
  expect_equal(r$region, c(1L, NA, 8L, 13L))
})

test_that("TIPOALTA del GRD", {
  expect_equal(death_from_tipo_alta(c("FALLECIDO", "DOMICILIO", "DERIVACIÓN A OTRA INSTITUCIÓN", "DESCONOCIDO", "4"), cfg$recode),
               c(TRUE, FALSE, FALSE, NA, TRUE))
})

test_that("recodificación conserva valores desconocidos marcados", {
  tab <- list(Hombre = c("1", "HOMBRE"), Mujer = c("2", "MUJER"))
  expect_equal(recode_values(c("1", "mujer", "9", NA), tab), c("Hombre", "Mujer", "?9", NA))
})

test_that("mapa de columnas: candidatos, patrones ordenados y faltantes", {
  raw <- data.table(`Año egreso` = "2020", DIAGNOSTICO10 = "x", DIAGNOSTICO2 = "b", DIAGNOSTICO1 = "a")
  cm <- apply_column_map(raw, list(anio = list("ANO_EGRESO"), sexo = list("SEXO"),
                                   diagnosticos = list(patron = "^DIAGNOSTICO_?[0-9]+$")))
  expect_equal(cm$std$anio, "2020")
  expect_equal(names(cm$multi$diagnosticos), c("DIAGNOSTICO1", "DIAGNOSTICO2", "DIAGNOSTICO10"))
  expect_equal(cm$faltantes, "sexo")
})

test_that("trauma GRD: cualquier posición, prioridad de subgrupos principales, universo y lesión aislada", {
  mod <- Filter(function(m) m$id == "trauma", cfg$modules)[[1]]
  h <- list(
    ev = data.table(hospitalizacion = c(TRUE, TRUE, TRUE, TRUE, FALSE), edad_min = 30, edad_max = 30,
                    diag1 = c("S061", "S020", "S026", "K122", "S026")),
    dx = list(c("S061", "S020", "S026", "K122", "S026"), c("S013", "S026", NA, "S026", NA), c(NA, NA, "S720", NA, NA)),
    proc = NULL)
  cl <- classify_module(h, mod, "grd", dicts, cfg)
  expect_equal(cl$casos$idx, c(1L, 2L, 3L))                  # K122 fuera del universo; CMA excluida
  expect_equal(cl$casos$subgrupo, c("Tejidos blandos faciales", "Fracturas faciales", "Fracturas faciales"))
  expect_equal(cl$casos$aislado, c(FALSE, FALSE, FALSE))     # S061, S020 y S720 son otras lesiones
  expect_equal(sum(cl$universo), 3L)
})

test_that("craneosinostosis: Q87.0 solo con procedimiento craneal y edad 0-9", {
  mod <- Filter(function(m) m$id == "craneosinostosis", cfg$modules)[[1]]
  h <- list(ev = data.table(hospitalizacion = TRUE, edad_min = c(1, 1, 1, 12), edad_max = c(1, 1, 1, 12),
                            diag1 = c("Q870", "Q870", "Q750", "Q750")),
            dx = list(c("Q870", "Q870", "Q750", "Q750")), proc = list(c("0203", NA, NA, NA)))
  cl <- classify_module(h, mod, "grd", dicts, cfg)
  expect_equal(cl$casos$idx, c(1L, 3L))
})

test_that("valor GRD por clúster de precios, con precio único fuera del listado", {
  expect_equal(price_lookup(c(120101, 111195, 125100, 999999), 2024, dicts), c(2821990, 3261000, 3851000, 2821990))
})

test_that("mecanismo DEIS (final_final.R): DIAG2, seis grupos, todos los rangos", {
  dx <- list(rep("S026", 6), c("Y04", "Y45", "W01", "S061", NA, "Y92"))
  expect_equal(classify_mechanism(dx, dicts$ce, "mecanismo_6grupos", solo_etiologicos = FALSE,
                                  sin_dato = "No especificada/otras"),
               c("Violencia interpersonal", "Eventos no accidentales / alta complejidad",
                 "Accidentes no intencionales de baja energía", "No especificada/otras",
                 "No especificada/otras", "No especificada/otras"))
})

test_that("etiquetas reales del DEIS: decenales, neonatales y dañadas (2025)", {
  e <- parse_age_label(c("menor de un año", "1 a 9", "10 a 19", "90 y más", "menor a 7 días", "7 A 27 DIAS",
                         "28 DIAS A 2 MES", "2 MESES A MENOS DE 1 AÑO", "85 A MAS", "menor de un a�o",
                         "90 y m�s", "*"))
  expect_equal(e$lo, c(0, 1, 10, 90, 0, 0, 0, 0, 85, 0, 90, NA))
  expect_equal(e$hi, c(0, 9, 19, 150, 0, 0, 0, 0, 150, 0, 150, NA))
})

test_that("grupos del artículo por límite inferior del tramo (age_min)", {
  g <- cfg$modules[[which(vapply(cfg$modules, `[[`, "", "id") == "trauma")]]
  grupos <- module_age_groups_cfg(g, "egresos_deis", cfg$params)
  expect_equal(age_group_lower(c(0, 1, 10, 19, 20, 45, 60, 90, NA), grupos),
               c("0-9", "0-9", "10-18", "19-44", "19-44", "45-59", "60+", "60+", NA))
})

test_that("definición DEIS de trauma incluye S02.0; la de GRD no", {
  g <- cfg$modules[[which(vapply(cfg$modules, `[[`, "", "id") == "trauma")]]
  expect_true("Craneal: bóveda" %in% mod_get(g, "subgrupos_principales", "egresos_deis"))
  expect_false("Craneal: bóveda" %in% mod_get(g, "subgrupos_principales", "grd"))
})

test_that("recodificación de previsión y pertenencia del DEIS", {
  expect_equal(recode_values(c("1", "3", "4", "5", "96", "99"), cfg$recode$prevision),
               c("FONASA", "FF.AA. y de Orden", "FF.AA. y de Orden", "FF.AA. y de Orden", "Ninguna", "Desconocida"))
  expect_equal(recode_values(c("FONASA INSTITUCIONAL - (MAI) B", "FONASA LIBRE ELECCIÓN (FMLE_D)", "CAJA DE PREVISIÓN FFAA (SISA)",
                               "PARTICULAR", "NO IDENTIFICADA", "ISAPRE"), cfg$recode$prevision),
               c("FONASA", "FONASA", "FF.AA. y de Orden", "Ninguna", "Desconocida", "ISAPRE"))
  expect_equal(recode_values(c("Pertenecientes al Sistema Nacional de Servicios de Salud, SNSS",
                               "No Pertenecientes al Sistema Nacional de Servicios de Salud, SNSS"), cfg$recode$sector),
               c("Público", "Privado"))
})

test_that("grupo por límite inferior con salvaguarda de mayoría", {
  g <- list(list(etiqueta = "<1", min = 0, max = 0), list(etiqueta = "1-9", min = 1, max = 9))
  expect_equal(age_group_lower(c(0, 0, 1, 5), g, c(0, 4, 9, 9)), c("<1", NA, "1-9", "1-9"))
  gi <- list(list(etiqueta = "0-14", min = 0, max = 14), list(etiqueta = "15-64", min = 15, max = 64),
             list(etiqueta = "65+", min = 65, max = 150))
  expect_equal(age_group_lower(c(10, 60, 90), gi, c(19, 69, 150)), c("0-14", "15-64", "65+"))
})

test_that("RISS ICDPIC-R: regiones, top 3 y AIS 6", {
  tab <- load_icdpic(cfg)
  skip_if(is.null(tab))
  sev <- function(k) tab[code4 == k]$sev
  dx <- list(c("S026", "J36", NA), c("S720", NA, NA), c("S020", NA, NA))
  r <- icdpic_riss(dx, tab)
  esperado1 <- sum(sort(c(max(sev("S026"), if (tab[code4 == "S020"]$issbr == tab[code4 == "S026"]$issbr) sev("S020") else 0),
                          sev("S720"), if (tab[code4 == "S020"]$issbr != tab[code4 == "S026"]$issbr) sev("S020") else 0),
                        decreasing = TRUE)[1:3]^2)
  expect_equal(r$riss[1], esperado1)
  expect_equal(r$riss[2:3], c(0, 0))
  r2 <- icdpic_riss(dx, tab, excluir = c("S026", "S020"))
  expect_equal(r2$riss[1], sev("S720")^2)
})

test_that("craneosinostosis GRD: Q75.0 secundario solo con cirugía craneal (notebook v35)", {
  mod <- Filter(function(m) m$id == "craneosinostosis", cfg$modules)[[1]]
  h <- list(ev = data.table(hospitalizacion = TRUE, edad_min = 1, edad_max = 1, diag1 = c("Q673", "Q673", "Q751", "Q040")),
            dx = list(c("Q673", "Q673", "Q751", "Q040"), c("Q750", "Q750", "Q750", "Q750")), proc = list(c("0201", NA, NA, "0201")))
  cl <- classify_module(h, mod, "grd", dicts, cfg)
  # Q04.0 (no craneofacial) con Q75.0 secundario y cirugía no cuenta.
  expect_equal(cl$casos$idx, c(1L, 3L))
  expect_equal(cl$casos$subgrupo, c("Q75.0 Craneosinostosis (secundario con cirugía)", "Q75.1 Sindrómica (Crouzon)"))
  expect_equal(nrow(classify_module(h, mod, "egresos_deis", dicts, cfg)$casos), 1L)
})

test_that("parámetros anuales: años sin dato usan el más cercano (el de los artículos)", {
  expect_equal(anio_parametro(c(2019, 2024, 2025, 2026), 2019:2024, "prueba"), c(2019, 2024, 2024, 2024))
  ev <- data.table(anio = c(2024L, 2025L), region = 1L, sexo = "Hombre", edad_min = 30, edad_max = 30,
                   dias_estada = 10, grd_peso = NA_real_)
  prod <- data.table(anio = 2024L, region = 1L, sexo = "Hombre", edad_min = 25, edad_max = 34,
                     ingreso_diario = 1000, tasa_ocupacion = 0.5)
  ipc <- data.table(anio = c(2024L, 2025L), deflactor = c(1, 0.95))
  p <- list(costos = list(incluir_costos_indirectos = TRUE, edad_laboral_min = 15,
                          edad_retiro = list(Hombre = 65, Mujer = 60)))
  d <- list(precios = data.table(), crosswalk = data.table())
  out <- add_costs(ev, d, ipc, NULL, prod, p)
  # 2025 usa el ingreso 2024 (en pesos 2024): mismo costo que 2024.
  expect_equal(out$costo_indirecto, c(5000, 5000))
})

test_that("tildes y Ñ: mojibake reparado, carácter dañado recuperado si calza con una sola comuna", {
  r <- repair_text(c("PEÃ‘ALOLÃ‰N", "NU�OA"))
  expect_equal(as.vector(r)[1], "PEÑALOLÉN")
  expect_equal(unname(attr(r, "calidad")), c(1, 1))
  h <- harmonize_commune(c("PEÑALOLÉN", "NU�OA", "LOS �NGELES", "ZZ�ZZ"), dicts)
  expect_equal(h$codigo, c("13122", "13120", "08301", NA))
  expect_equal(normalize_value("Aysén del General Carlos Ibáñez"), "AYSEN_DEL_GENERAL_CARLOS_IBANEZ")
})

test_that("patología maxilar GRD: suma CMA, excluye D16.4 con cirugía craneal y deja K10.8 fuera", {
  mod <- Filter(function(m) m$id == "patologia_maxilar", cfg$modules)[[1]]
  dx1 <- c("K010", "K010", "D164", "D164", "K108", "K092", "K010")
  h <- list(ev = data.table(hospitalizacion = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE),
                            actividad = c("Hospitalización", "CMA", "Hospitalización", "Hospitalización", "Hospitalización", "CMA", NA),
                            edad_min = 20, edad_max = 20, diag1 = dx1),
            dx = list(dx1), proc = list(c("2319", "2319", "016", "762", "765", "244", "2319"), c(NA, NA, NA, "0225", NA, NA, NA)))
  cl <- classify_module(h, mod, "grd", dicts, cfg)
  # 1 y 2: hospitalización y CMA; 3: D16.4 con 01.6 y 4: D16.4 con 02.25 excluidos; 5: K10.8 fuera; 6: CMA; 7: actividad NA.
  expect_equal(cl$casos$idx, c(1L, 2L, 6L))
  expect_equal(cl$casos$subgrupo, c("Dientes incluidos e impactados", "Dientes incluidos e impactados", "Quistes de los maxilares"))
  # Los demás módulos siguen siendo solo hospitalización.
  modt <- Filter(function(m) m$id == "infecciones", cfg$modules)[[1]]
  h2 <- list(ev = data.table(hospitalizacion = c(TRUE, FALSE), actividad = c("Hospitalización", "CMA"), edad_min = 30, edad_max = 30,
                             diag1 = c("K122", "K122")), dx = list(c("K122", "K122")), proc = NULL)
  expect_equal(classify_module(h2, modt, "grd", dicts, cfg)$casos$idx, 1L)
  # En DEIS no hay procedimientos: D16.4 cuenta (limitación declarada; sensibilidad sin el subgrupo).
  h3 <- list(ev = data.table(hospitalizacion = TRUE, actividad = "Hospitalización", edad_min = 40, edad_max = 40, diag1 = "D164"),
             dx = list("D164", NA_character_), proc = NULL)
  expect_equal(classify_module(h3, mod, "egresos_deis", dicts, cfg)$casos$subgrupo, "Tumor benigno óseo craneofacial (D16.4)")
})

test_that("actividad GRD desde TIPO_ACTIVIDAD", {
  rc <- cfg$recode
  act <- c("HOSPITALIZACIÓN", "CIRUGÍA MAYOR AMBULATORIA (CMA)", "HOSPITALIZACION DOMICILIARIA", NA)
  hosp <- normalize_value(act) %in% normalize_value(unlist(rc$actividad_hospitalizacion))
  cma <- !hosp & contains_any(act, unlist(rc$actividad_cma))
  expect_equal(hosp, c(TRUE, FALSE, FALSE, FALSE)); expect_equal(cma, c(FALSE, TRUE, FALSE, FALSE))
})
