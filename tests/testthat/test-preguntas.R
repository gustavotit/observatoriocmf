# Intérprete de preguntas: la normalización y el reconocimiento no requieren datos; las respuestas sí.

test_that("la normalización ignora tildes, mayúsculas y signos", {
  expect_equal(pq_norm("¿Cuántos EGRESOS hubo en Biobío?"), " cuantos egresos hubo en biobio ")
  expect_true(pq_has(pq_norm("Fracturas faciales"), "fractur"))
})

test_that("años: rango, desde, últimos años y valor por defecto", {
  r <- c(2001, 2025)
  expect_equal(pq_anios(pq_norm("entre 2019 y 2024"), r)$anios, 2019:2024)
  expect_equal(pq_anios(pq_norm("desde 2020"), r)$anios, 2020:2025)
  expect_equal(pq_anios(pq_norm("en los últimos 5 años"), r)$anios, 2021:2025)
  expect_true(pq_anios(pq_norm("trauma en Chile"), r)$todos)
})

test_that("medidas y desagregaciones", {
  expect_equal(pq_medida(pq_norm("¿cuántos egresos?"))$metric, "conteo")
  expect_equal(pq_medida(pq_norm("valor GRD medio por egreso"))$ind, "valor_medio")
  expect_equal(pq_medida(pq_norm("tasa ajustada"))$metric, "ajustada")
  expect_equal(pq_by(pq_norm("¿cómo ha evolucionado por región?")), c("anio", "region"))
})

test_that("las respuestas usan las mismas cifras que cmf_query()", {
  skip_if(is.null(cmf_load("cubo")) || !nrow(cmf_load("cubo")), "sin datos publicados")
  r <- cmf_preguntar("¿Cuántos egresos por trauma craneomaxilofacial hubo en Tarapacá entre 2019 y 2024?")
  expect_true(r$ok)
  q <- cmf_query("trauma", "egresos_deis", years = 2019:2024, region = 1, by = character(), metric = "conteo")
  expect_match(r$parrafos[1], formatC(q$n, format = "d", big.mark = "."), fixed = TRUE)
  expect_false(cmf_preguntar("hola")$ok)
})
