# Tramos INE frente a los grupos de cada módulo (no requiere datos publicados).

test_that("tramos INE que cruzan un grupo del artículo detienen la tasa poblacional", {
  pf <- data.table::data.table(edad_min = c(0, 1, 5, 10, 15, 20), edad_max = c(0, 4, 9, 14, 19, 24), poblacion = 1)
  g_deis <- list(list(etiqueta = "0-9", min = 0, max = 9), list(etiqueta = "10-18", min = 10, max = 18),
                 list(etiqueta = "19-44", min = 19, max = 44))
  expect_error(bands_to_groups(data.table::copy(pf), g_deis), "no calza")
  ok <- bands_to_groups(data.table::copy(pf), g_deis, necesarios = "0-9")
  expect_equal(ok$grupo_edad[1:3], rep("0-9", 3))
  g_inf <- list(list(etiqueta = "0-14", min = 0, max = 14), list(etiqueta = "15-64", min = 15, max = 64))
  expect_equal(bands_to_groups(data.table::copy(pf), g_inf)$grupo_edad, c(rep("0-14", 4), rep("15-64", 2)))
})

test_that("mod_get resuelve ajustes comunes o por fuente", {
  m <- list(a = c("x", "y"), b = list(egresos_deis = "d", grd = "g"))
  expect_equal(mod_get(m, "a", "grd"), c("x", "y"))
  expect_equal(mod_get(m, "b", "grd"), "g")
})

test_that("estandarización a población chilena con grupos del módulo", {
  d <- data.table::data.table(anio = 2020L, grupo_edad = c("0-14", "15-64", "65+"), n = c(10, 20, 5))
  pf <- data.table::data.table(anio = 2020L, edad_min = c(0, 15, 65), edad_max = c(14, 64, 150), poblacion = c(1e5, 2e5, 5e4))
  pf[, grupo_edad := c("0-14", "15-64", "65+")]
  w <- data.table::data.table(grupo_edad = c("0-14", "15-64", "65+"), peso = c(1e5, 2e5, 5e4))
  r <- age_adjusted(d, pf, "anio", 1e5, col = "grupo_edad", w = w)
  # Con estándar igual a la población observada, la ajustada iguala a la cruda.
  expect_equal(r$tasa, 35 / 3.5e5 * 1e5, tolerance = 1e-9)
})
