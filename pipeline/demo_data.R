#!/usr/bin/env Rscript
# Genera datos SINTÉTICOS con la estructura de DEIS (EGRE_DATOS_ABIERTOS), GRD_PUBLICO, INE comunal,
# nacidos vivos, IPC del BCCh, productividad y oferta por Servicio de Salud, para probar el pipeline y la
# app sin datos reales. No usar para análisis.
#
# Uso: Rscript pipeline/demo_data.R            -> escribe archivos en data-small/demo/
#      Rscript pipeline/demo_data.R registrar  -> además los registra en config/sources.yml
#                                                (hazlo en una copia del repo, no en main)

suppressPackageStartupMessages(library(data.table))
set.seed(2026)
out <- file.path("data-small", "demo")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
com <- fread("dictionaries/comunas.csv", colClasses = c(codigo_comuna = "character"))
anios_deis <- 2015:2025
anios_grd <- 2019:2025

# Etiquetas reales de GRUPO_EDAD: decenales en la mayoría de los años; quinquenales con tramos neonatales en
# 2012, 2021 y 2024; en 2025 con caracteres dañados ("a\uFFFDo", "m\uFFFDs").
grupos_dec <- c("menor de un año", "1 a 9", sprintf("%d a %d", seq(10, 80, 10), seq(19, 89, 10)), "90 y más")
grupos_quin <- c("menor a 7 días", "7 A 27 DIAS", "28 DIAS A 2 MES", "2 MESES A MENOS DE 1 AÑO", "1 A 4 AÑOS",
                 sprintf("%d A %d AÑOS", seq(5, 80, 5), seq(9, 84, 5)), "85 A MAS")
grupos_anio <- function(anio) {
  if (anio %in% c(2012, 2021, 2024)) return(list(g = grupos_quin, infante = grupos_quin[1:4], uno = grupos_quin[5:6]))
  g <- grupos_dec
  if (anio >= 2025) g <- gsub("ñ", "\uFFFD", gsub("á", "\uFFFD", g))
  list(g = g, infante = g[1], uno = g[2])
}
dx_pool <- c("S026", "S022", "S024", "S023", "S029", "S013", "S015", "S030", "S020", "S021", "S061", "S72",
             "K122", "L032", "J36", "J390", "K102", "Q37", "Q36", "Q35", "Q750", "Q751", "K010", "J189", "I219")
dx_p <- c(8, 6, 5, 2, 2, 3, 3, 1, 2, 1, 15, 10, 4, 3, 3, 1, 1, 0.6, 0.4, 0.3, 0.3, 0.05, 4, 30, 20)
ce_pool <- c("Y04", "X99", "W01", "W10", "V43", "V29", "W21", "W50", "X59", NA)
legacy <- c("08401", "10501", "01201")

make_deis <- function(n, anio) {
  dx <- sample(dx_pool, n, TRUE, prob = dx_p)
  ga <- grupos_anio(anio)
  grupo <- sample(ga$g, n, TRUE)
  fis <- dx %in% c("Q35", "Q36", "Q37"); cs <- dx %in% c("Q750", "Q751")
  grupo[fis] <- ifelse(runif(sum(fis)) < 0.85, sample(ga$infante, sum(fis), TRUE), ga$uno[1])
  grupo[cs] <- ifelse(runif(sum(cs)) < 0.4, sample(ga$infante, sum(cs), TRUE), sample(ga$uno, sum(cs), TRUE))
  grupo[sample(n, max(1, round(n * 0.002)))] <- "*"
  comuna <- sample(com$codigo_comuna, n, TRUE)
  if (anio <= 2017) comuna[sample(n, round(n * 0.01))] <- sample(legacy, round(n * 0.01), TRUE)
  data.table(
    ANO_EGRESO = anio, SEXO = sample(if (anio >= 2020) c("HOMBRE", "MUJER") else c("1", "2"), n, TRUE, prob = c(0.58, 0.42)),
    GRUPO_EDAD = grupo,
    COMUNA_RESIDENCIA = sub("^0", "", comuna), REGION_RESIDENCIA = as.integer(substr(comuna, 1, 2)),
    PERTENENCIA_ESTABLECIMIENTO_SALUD = sample(c("Pertenecientes al Sistema Nacional de Servicios de Salud, SNSS",
                                                 "No Pertenecientes al Sistema Nacional de Servicios de Salud, SNSS"), n, TRUE, prob = c(0.75, 0.25)),
    PREVISION = sample(c("1", "2", "3", "4", "5", "96", "99"), n, TRUE, prob = c(0.76, 0.16, 0.01, 0.01, 0.005, 0.04, 0.015)),
    DIAS_ESTADA = rpois(n, 3) + 1, CONDICION_EGRESO = sample(c("1", "2"), n, TRUE, prob = c(0.993, 0.007)),
    INTERV_Q = sample(c("1", "2"), n, TRUE), DIAG1 = dx, DIAG2 = sample(ce_pool, n, TRUE)
  )
}

deis_versions <- list()
for (a in anios_deis) {
  f <- file.path(out, sprintf("EGRE_DATOS_ABIERTOS_%d.csv", a))
  fwrite(make_deis(round(5000 * ifelse(a %in% 2020:2021, 0.8, 1)), a), f, sep = ";")
  deis_versions[[length(deis_versions) + 1]] <- list(version = as.character(a), anios = list(a), archivo = f,
                                                     fecha_carga = as.character(Sys.Date()), nota = "Sintético")
}

hosp <- fread("dictionaries/grd_hospital_grupo.csv")[anio_precio == 2024]$cod_hospital
grd_versions <- list()
for (a in anios_grd) {
  n <- 2500
  e <- make_deis(n, a)
  nac <- as.Date(sprintf("%d-06-15", a)) - round(runif(n, 0, 85) * 365.25)
  nac[e$DIAG1 %in% c("Q750", "Q751")] <- as.Date(sprintf("%d-06-15", a)) - round(runif(sum(e$DIAG1 %in% c("Q750", "Q751")), 60, 900))
  ing <- as.Date(sprintf("%d-01-01", a)) + sample(0:360, n, TRUE)
  dx2 <- ifelse(e$DIAG1 %in% c("S061", "S72"), sample(c("S026", "S022", NA, NA, NA), n, TRUE), sample(c("S061", NA, NA, NA), n, TRUE))
  g <- data.table(
    COD_HOSPITAL = sample(c(hosp, 999999), n, TRUE),
    TIPO_ACTIVIDAD = sample(c("HOSPITALIZACIÓN", "CIRUGÍA MAYOR AMBULATORIA (CMA)"), n, TRUE, prob = c(0.85, 0.15)),
    SEXO = ifelse(e$SEXO %in% c("1", "HOMBRE"), "HOMBRE", "MUJER"),
    FECHA_NACIMIENTO = format(nac, "%d-%m-%Y"),
    FECHA_INGRESO = format(ing, "%d-%m-%Y"), FECHAALTA = format(ing + e$DIAS_ESTADA, "%d-%m-%Y"),
    COMUNA = toupper(com$nombre_comuna[match(sprintf("%05d", as.integer(e$COMUNA_RESIDENCIA)), com$codigo_comuna)]),
    PREVISION = sample(c("FONASA", "ISAPRE", "CAPREDENA", "NINGUNA"), n, TRUE, prob = c(0.9, 0.05, 0.02, 0.03)),
    TIPOALTA = sample(c("DOMICILIO", "FALLECIDO", "DERIVACIÓN A OTRA INSTITUCIÓN", "DESCONOCIDO"), n, TRUE, prob = c(0.93, 0.01, 0.05, 0.01)),
    USOSPABELLON = if (a %in% c(2020, 2025)) "0" else sample(c("0", "1"), n, TRUE),
    FECHAINTERV1 = ifelse(runif(n) < 0.5, format(ing, "%d-%m-%Y"), ""),
    IR_29301_COD_GRD = sample(sprintf("%06d", 1:300), n, TRUE),
    IR_29301_PESO = sub(".", ",", sprintf("%.4f", runif(n, 0.3, 3)), fixed = TRUE),
    IR_29301_SEVERIDAD = sample(1:3, n, TRUE),
    DIAGNOSTICO1 = e$DIAG1, DIAGNOSTICO2 = dx2, DIAGNOSTICO3 = e$DIAG2,
    PROCEDIMIENTO1 = ifelse(e$DIAG1 == "S026", "76.76", ifelse(e$DIAG1 %in% c("Q750", "Q751"), "0203", "")),
    PROCEDIMIENTO2 = ifelse(e$DIAG1 == "K122", "27.0", "")
  )
  if (a == 2025) { g[, FECHA_NACIMIENTO := NULL]; g[, TRAMO_EDAD := { ed <- pmin(floor(as.numeric(as.Date(sprintf("%d-06-15", a)) - nac) / 365.25), 109); lo <- floor(ed / 5) * 5; sprintf("[%d - %d años]", lo, lo + 4) }] }
  f <- file.path(out, sprintf("GRD_PUBLICO_%d.txt", a))
  fwrite(g, f, sep = "|")
  grd_versions[[length(grd_versions) + 1]] <- list(version = as.character(a), anios = list(a), archivo = f, nota = "Sintético")
}

pop <- CJ(Comuna = as.integer(com$codigo_comuna), sexo = 1:2, area = 1:2, Edad = 0:80)
pop[, Region := Comuna %/% 1000]
pop[, `Nombre Comuna` := com$nombre_comuna[match(sprintf("%05d", Comuna), com$codigo_comuna)]]
base <- runif(nrow(pop), 5, 40) * ifelse(pop$Edad == 80, 6, 1)
for (a in 2002:2025) pop[[paste("Poblacion", a)]] <- round(base * (1 + (a - 2002) * 0.01))
setnames(pop, c("sexo", "area"), c("Sexo (1=Hombre 2=Mujer)", "Area (1=Urbano 2=Rural)"))
fwrite(pop, file.path(out, "ine_poblacion_comunal_demo.csv"))

nv <- CJ(anio = 2002:2024, region = 1:16)[, nacidos_vivos := round(runif(.N, 2000, 9000) * (1 - (anio - 2002) * 0.012))]
fwrite(nv, file.path(out, "nacimientos_region_demo.csv"))
fwrite(CJ(ANIO = 2018:2025, MES = 1:12)[, VAR_MENSUAL := round(rnorm(.N, 0.35, 0.2), 1)], file.path(out, "ipc_demo.csv"))
fwrite(data.table(servicio_salud_id = c(1:26, 28, 29, 33), especialistas = rpois(29, 10),
                  FTE_por_100000_residentes = round(runif(29, 0.3, 2.5), 2),
                  cne_cmf_bucal_personas_unicas = rpois(29, 900), mediana_espera_dias = round(runif(29, 80, 700))),
       file.path(out, "oferta_servicios_demo.csv"))

cat("Archivos sintéticos escritos en", out, "\n")

if ("registrar" %in% commandArgs(trailingOnly = TRUE)) {
  src <- yaml::read_yaml("config/sources.yml", fileEncoding = "UTF-8")
  src$fuentes$egresos_deis$versiones <- deis_versions
  src$fuentes$grd$versiones <- grd_versions
  reg <- function(f) list(list(version = "demo", archivo = file.path(out, f)))
  src$fuentes$poblacion_ine$versiones <- reg("ine_poblacion_comunal_demo.csv")
  src$fuentes$nacimientos_region$versiones <- reg("nacimientos_region_demo.csv")
  src$fuentes$ipc$versiones <- reg("ipc_demo.csv")
  src$fuentes$oferta_servicios$versiones <- reg("oferta_servicios_demo.csv")
  yaml::write_yaml(src, "config/sources.yml")
  cat("Versiones de prueba registradas en config/sources.yml\n")
}
