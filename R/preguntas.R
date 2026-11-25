# Preguntas en lenguaje natural ---------------------------------------------------------------------
#
# Intérprete determinista, sin modelo de lenguaje: reconoce en la pregunta el módulo, el subgrupo, la fuente,
# el territorio, los años, el sexo, el grupo etario, la medida y la desagregación, llama a cmf_query() o
# cmf_query_eco() y redacta la respuesta con plantillas. Todas las cifras salen del cubo publicado (con la
# misma supresión de celdas pequeñas que el resto del observatorio); si la pregunta no calza, lo dice y
# sugiere preguntas que sí puede responder. No inventa ni completa datos.

# Texto en minúsculas, sin tildes ni signos; con espacios en los bordes para buscar palabras completas.
pq_norm <- function(x) {
  x <- tolower(enc2utf8(as.character(x)))
  x <- chartr("áéíóúüñàèìòùâêîôûç", "aeiouunaeiouaeiouc", x)
  x <- gsub("[^a-z0-9+]+", " ", x)
  paste0(" ", trimws(gsub(" +", " ", x)), " ")
}
pq_has <- function(txt, pats) any(vapply(pats, function(p) grepl(p, txt, perl = TRUE), TRUE))
pq_fmt <- function(x, d = 0) {
  vapply(x, function(v) if (is.na(v)) "sin dato" else
    formatC(v, format = "f", digits = d, big.mark = ".", decimal.mark = ","), "")
}
pq_dec <- function(v) if (is.na(v)) 1 else if (abs(v) >= 100) 0 else if (abs(v) >= 10) 1 else 2

# Sinónimos ----------------------------------------------------------------------------------------
PQ_MODULOS <- list(
  trauma = c(" trauma", "traumatismo", "fractur", "lesion(es)? (facial|de la cara)", " cmft ", "contusion",
             "herida(s)? (facial|de la cara)", "trauma (facial|maxilofacial|craneomaxilofacial)"),
  infecciones = c("infecc", "absces", "celulitis", "flegmon", "ludwig", "periamigdal", "parotiditis", "sialoadenitis"),
  fisuras = c("fisura", "leporino", "paladar hendido", "labio hendido", "labiopalat", " q3[567] "),
  craneosinostosis = c("craneosinost", "sinostosis", "craneoestenosis", "crouzon", " apert "),
  patologia_maxilar = c("patologia (quirurgica )?maxilar", "incluid", "impactad", "terceros? molar", "cordal",
                        "muelas? del juicio", "quiste", "tumor(es)? benign", "queratoquist", "ameloblastom",
                        "odontoma", "supernumerari", "lesiones de los maxilares"),
  oferta = c("especialistas?", "cirujanos? maxilofacial", " fte ", "dotacion", "listas? de espera",
             " espera ", "capacidad instalada", "esperando")
)

# Subgrupos (o agrupaciones publicadas) que se nombran explícitamente en la pregunta.
PQ_SUBGRUPOS <- list(
  trauma = list(
    "Fracturas faciales" = "fractur",
    "Tejidos blandos faciales" = c("tejidos? blandos?", "herida", "contusion"),
    "Craneal: bóveda" = "boveda",
    "Craneal: base" = "base (de|del) craneo"),
  infecciones = list(
    "Oral y maxilares" = c("odontogen", " oral(es)? ", " maxilares? "),
    "Glándulas salivales" = c("salival", "parotid", "submandibular", "sialoadenitis"),
    "Orbitaria" = "orbit",
    "Periamigdalina" = "amigdal",
    "Faríngea/cervical profunda" = c("faring", "(cuello|cervical)(es)? profund", "ludwig"),
    "Piel y tejidos blandos facial/cervical" = c(" piel ", "tejidos? blandos?")),
  fisuras = list(
    "Q37 Labio y paladar" = c("labio y paladar"),
    "Q36 Labio" = c("solo (de )?labio", "fisura (de )?labio (aislad|sin)"),
    "Q35 Paladar" = c("solo (de )?paladar", "fisura palatina aislada", "paladar aislad")),
  craneosinostosis = list(
    "Q75.1 Sindrómica (Crouzon)" = "crouzon"),
  patologia_maxilar = list(
    "Dientes incluidos e impactados" = c("incluid", "impactad", "terceros? molar", "cordal", "muelas? del juicio"),
    "Quistes de los maxilares" = c("quiste", "queratoquist"),
    "Tumores benignos de los maxilares" = c("tumor", "ameloblastom", "odontoma"),
    "Dientes supernumerarios" = "supernumerari")
)
PQ_AGRUPACIONES <- list(
  trauma = list("Tejidos duros" = "tejidos? duros?"),
  patologia_maxilar = list("Lesiones de los maxilares" = "lesiones de los maxilares")
)

PQ_REGIONES_EXTRA <- list(`13` = c(" rm ", "santiago", "region metropolitana"), `6` = c("o ?higgins", "libertador"),
                          `9` = "araucania", `8` = "bio ?bio", `15` = "arica", `11` = c("aysen", "aisen"),
                          `12` = c("magallanes", "punta arenas"), `14` = "valdivia", `10` = "puerto montt",
                          `16` = "nuble")

PQ_MEDIDAS_LBL <- c(conteo = "egresos", cruda = "tasa cruda", ajustada = "tasa ajustada (OMS)",
                    ajustada_chile = "tasa ajustada a la población chilena", nacidos_vivos = "tasa",
                    universo = "tasa", egresos = "tasa")

# Ejemplos que la portada ofrece como botones (todos tienen respuesta con los datos publicados).
#' Preguntas de ejemplo para el intérprete
#' @export
cmf_preguntas_ejemplo <- function() {
  c("¿Cuántos egresos por trauma craneomaxilofacial hubo en Tarapacá entre 2019 y 2024?",
    "¿Cómo han evolucionado las infecciones maxilofaciales en Chile?",
    "¿Qué región tiene la tasa de fisuras labiopalatinas más alta?",
    "¿Cuál es el valor GRD medio por egreso de dientes incluidos?",
    "¿Cuántos especialistas CMF hay en el Servicio de Salud Biobío?",
    "¿Qué códigos CIE-10 incluye craneosinostosis?")
}

#' Preguntas para el autocompletado del buscador
#'
#' Genera preguntas completas que el intérprete sabe responder, combinando los módulos publicados, sus
#' subgrupos más consultados, las regiones, los Servicios de Salud y los indicadores de capacidad y espera.
#' La app las filtra en el navegador mientras se escribe.
#' @return Vector de texto sin duplicados.
#' @export
cmf_preguntas_sugeridas <- function() {
  mods <- cmf_modules()
  cubo <- cmf_load("cubo")
  disp <- mods[mods$estado == "disponible" & mods$tipo == "eventos" & mods$id %in% unique(cubo$modulo), ]
  reg <- cmf_region_names()$nombre
  ss <- cmf_service_names()$nombre
  ult <- max(cubo$anio, na.rm = TRUE)
  temas <- c(stats::setNames(tolower(disp$nombre), disp$id),
             trauma = "fracturas faciales", patologia_maxilar = "dientes incluidos",
             patologia_maxilar = "quistes de los maxilares", infecciones = "infecciones de cuello profundo",
             infecciones = "infecciones odontogénicas")
  out <- character()
  for (i in seq_along(temas)) {
    t <- temas[[i]]; id <- names(temas)[i]
    m <- cmf_module(id)
    grd <- "grd" %in% unlist(m$fuentes)
    out <- c(out,
      paste0("¿Cuántos egresos por ", t, " hubo en ", ult, "?"),
      paste0("¿Cómo han evolucionado los egresos por ", t, " en Chile?"),
      paste0("¿Qué región tiene la tasa de ", t, " más alta?"),
      paste0("¿Qué Servicio de Salud tiene la tasa de ", t, " más alta?"),
      paste0("Egresos por ", t, " por sexo"),
      paste0("Egresos por ", t, " por grupo etario"),
      paste0("Estada media de ", t),
      paste0("¿Qué códigos CIE-10 incluye ", t, "?"),
      if (grd) c(paste0("¿Cuál es el valor GRD total de ", t, " en ", ult, "?"),
                 paste0("¿Cuál es el valor GRD medio por egreso de ", t, "?"),
                 paste0("Valor GRD de ", t, " por año")),
      paste0("Tasa de ", t, " en ", reg),
      paste0("¿Cuántos egresos por ", t, " hubo en ", reg, " en los últimos 5 años?"),
      paste0("Egresos por ", t, " en el Servicio de Salud ", ss))
  }
  out <- c(out,
    "¿Cuántos egresos por trauma craneomaxilofacial por agresión hubo en hombres?",
    "Fracturas faciales por mecanismo en GRD",
    "¿Ha aumentado la patología quirúrgica maxilar ambulatoria desde 2019?",
    "Letalidad de las infecciones de cuello profundo",
    "Tasa de fisuras labiopalatinas por año",
    "Trauma mayor (RISS 16 o más) en GRD por año",
    "¿Cuántos especialistas CMF hay en Chile?",
    "¿Qué Servicio de Salud tiene más especialistas CMF por 100.000 habitantes?",
    "¿Qué Servicio de Salud tiene la mayor lista de espera de cirugías por 100.000?",
    paste0("¿Cuántos especialistas CMF hay en el Servicio de Salud ", ss, "?"),
    paste0("Mediana de días de espera de consultas nuevas en el Servicio de Salud ", ss),
    paste0("¿Cuántas cirugías CMF en espera hay en el Servicio de Salud ", ss, "?"),
    "¿Cuál es la diferencia entre el DEIS y el GRD?",
    "¿Cómo cito el observatorio?")
  unique(out)
}

# Interpretación -----------------------------------------------------------------------------------

pq_modulo <- function(txt, disponibles) {
  sc <- vapply(names(PQ_MODULOS), function(m) sum(vapply(PQ_MODULOS[[m]], grepl, TRUE, x = txt, perl = TRUE)), 0)
  sc <- sc[names(sc) %in% c(disponibles, "oferta") & sc > 0]
  # "Infecciones odontogénicas" o "quistes infectados": manda la palabra del módulo, no el sitio anatómico.
  if (all(c("infecciones", "patologia_maxilar") %in% names(sc)) && grepl("infecc|absces|celulitis", txt)) {
    sc <- sc[names(sc) != "patologia_maxilar"]
  }
  if (!length(sc)) return(NULL)
  top <- names(sc)[sc == max(sc)]
  if (length(top) > 1) return(top)
  top
}

pq_subgrupo <- function(txt, modulo, fuente) {
  disp <- cmf_subgroups(modulo, fuente)$subgrupo
  ag <- PQ_AGRUPACIONES[[modulo]]
  agr <- names(ag)[vapply(ag, function(p) pq_has(txt, p), TRUE)]
  agr <- intersect(agr, names(mod_get(cmf_module(modulo), "agrupaciones", fuente)))
  if (length(agr)) return(list(agrupacion = agr))
  sg <- PQ_SUBGRUPOS[[modulo]]
  hit <- names(sg)[vapply(sg, function(p) pq_has(txt, p), TRUE)]
  # La palabra que nombra el módulo no filtra: "infecciones orales" sí, "infecciones maxilofaciales" no.
  hit <- intersect(hit, disp)
  if (modulo == "craneosinostosis" && grepl("sindromic", txt)) {
    hit <- intersect(c("Q75.1 Sindrómica (Crouzon)", "Q87.0 Sindrómica con cirugía craneal"), disp)
  }
  if (length(hit)) list(subgrupo = hit) else list()
}

pq_fuente <- function(txt, modulo) {
  fu <- unlist(cmf_module(modulo)$fuentes)
  if (pq_has(txt, c(" grd ", "red publica", "hospitales? publicos?", " fonasa ")) && "grd" %in% fu) return(list(f = "grd", explicita = TRUE))
  if (pq_has(txt, c(" deis ", "todos los prestadores", "publicos? y privados?")) && "egresos_deis" %in% fu) {
    return(list(f = "egresos_deis", explicita = TRUE))
  }
  list(f = fu[1], explicita = FALSE)
}

pq_territorio <- function(txt) {
  reg <- cmf_region_names(); ss <- cmf_service_names()
  out <- list(region = NULL, servicio_salud = NULL)
  quiere_ss <- grepl(" servicios? de salud | ss | servicio ", txt)
  # Servicios de Salud: los nombres propios (Concepción, Arauco, Metropolitano Sur...) se reconocen solos; los que
  # coinciden con una región (Tarapacá, Biobío...) solo si la pregunta dice "Servicio de Salud".
  ss_n <- vapply(ss$nombre, pq_norm, "")
  reg_n <- vapply(reg$nombre, pq_norm, "")
  ord <- order(-nchar(ss_n))
  resto <- txt
  for (i in ord) {
    nom <- ss_n[i]
    propio <- !(trimws(nom) %in% trimws(reg_n))
    if ((propio || quiere_ss) && grepl(nom, resto, fixed = TRUE)) {
      out$servicio_salud <- c(out$servicio_salud, ss$codigo_servicio_salud[i])
      resto <- sub(nom, " ", resto, fixed = TRUE)
    }
  }
  if (length(out$servicio_salud)) return(out)
  for (i in seq_len(nrow(reg))) {
    pats <- c(gsub(" ", " ?", trimws(reg_n[i])), PQ_REGIONES_EXTRA[[as.character(reg$codigo_region[i])]])
    pats <- vapply(pats, function(p) paste0("\\b", trimws(p), "\\b"), "")
    if (pq_has(resto, pats)) out$region <- c(out$region, reg$codigo_region[i])
  }
  # "Los Ríos" y "Los Lagos" contienen palabras sueltas que no deben activar otras regiones.
  out$region <- unique(out$region)
  out
}

pq_anios <- function(txt, rango) {
  ys <- as.integer(regmatches(txt, gregexpr("\\b(19[89][0-9]|20[0-4][0-9])\\b", txt))[[1]])
  todos <- rango[1]:rango[2]
  m <- regmatches(txt, regexec("ultimos? ([0-9]+|dos|tres|cuatro|cinco|seis|siete|ocho|nueve|diez) anos", txt))[[1]]
  if (length(m)) {
    k <- suppressWarnings(as.integer(m[2]))
    if (is.na(k)) k <- match(m[2], c("uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez"))
    return(list(anios = max(rango[1], rango[2] - k + 1):rango[2], todos = FALSE))
  }
  if (!length(ys)) {
    if (pq_has(txt, c("ultimo ano", "ano mas reciente", "ultimo dato", "actualmente", " hoy "))) return(list(anios = rango[2], todos = FALSE))
    return(list(anios = todos, todos = TRUE))
  }
  if (length(ys) >= 2) {
    a <- seq(min(ys), max(ys))
  } else if (pq_has(txt, c("desde", "a partir", "despues de"))) {
    a <- ys:max(ys, rango[2])
  } else if (pq_has(txt, c("hasta", "antes de"))) {
    a <- min(ys, rango[1]):ys
  } else {
    a <- ys
  }
  list(anios = a, pedidos = range(ys), todos = FALSE)
}

pq_by <- function(txt) {
  by <- character()
  if (pq_has(txt, c(" por anos? ", " y anos? ", " anual", "tendencia", "evolucion", "evolucionad", " serie ", "cada ano", "ano a ano",
                    "en el tiempo", "a lo largo", "ha cambiado", "han cambiado", "aument", "disminu", " creci", " cayo "))) by <- "anio"
  if (pq_has(txt, c("por servicio", "cada servicio", "que servicio", "servicios de salud", "entre servicios"))) {
    by <- c(by, "servicio_salud")
  } else if (pq_has(txt, c("por region", "cada region", "que region", " regiones ", "regional", "entre regiones", "region con", "region tiene"))) {
    by <- c(by, "region")
  }
  if (pq_has(txt, c("por sexo", "segun sexo", "hombres y mujeres", "mujeres y hombres", "entre sexos"))) by <- c(by, "sexo")
  if (pq_has(txt, c("por edad", "grupos? etario", "segun edad", "por grupos? de edad", "por tramo"))) by <- c(by, "grupo_edad")
  if (pq_has(txt, c("mecanismo", " causas? ", "por que se produc"))) by <- c(by, "mecanismo")
  if (pq_has(txt, c("por tipo", "por subgrupo", "tipos de", "por dominio", "por diagnostico", "por lesion"))) by <- c(by, "subgrupo")
  if (pq_has(txt, c("por actividad", "hospitalizacion y cma", "ambulatoria y hosp"))) by <- c(by, "actividad")
  unique(by)
}

pq_medida <- function(txt) {
  if (pq_has(txt, c("dias cama"))) return(list(tipo = "eco", ind = "dias_cama"))
  if (pq_has(txt, c(" costo", "cuesta", " valor", " gasto", "dinero", " plata", " monto", "millones", " pesos"))) {
    medio <- pq_has(txt, c(" medio", "promedio", "por egreso", "por paciente", "por caso", "unitari", " cada "))
    por_hab <- pq_has(txt, c("por habitante", "por 100 ?000", "per capita"))
    return(list(tipo = "eco", ind = if (por_hab) "valor_100k" else if (medio) "valor_medio" else "valor_total"))
  }
  if (pq_has(txt, c("estada", "estadia", "dias de hospitalizacion", "dias hospitalizad", "cuantos dias", "dias internad"))) {
    return(list(tipo = "clinico", col = "estada_media"))
  }
  if (pq_has(txt, c("letalidad", "mortalidad", "fallec", "muert", "murieron"))) return(list(tipo = "clinico", col = "letalidad_pct"))
  if (pq_has(txt, c("pabellon", "operad", "quirofano"))) return(list(tipo = "clinico", col = "pct_pabellon"))
  if (pq_has(txt, c(" riss ", "severidad", "gravedad", "trauma mayor"))) return(list(tipo = "clinico", col = "pct_trauma_mayor"))
  if (pq_has(txt, c("nacidos vivos"))) return(list(tipo = "epi", metric = "nacidos_vivos"))
  if (pq_has(txt, c("ajustad", "estandarizad"))) return(list(tipo = "epi", metric = "ajustada"))
  if (pq_has(txt, c("tasa cruda", " cruda"))) return(list(tipo = "epi", metric = "cruda"))
  if (pq_has(txt, c("egresos totales", "por 100 ?000 egresos"))) return(list(tipo = "epi", metric = "egresos"))
  if (pq_has(txt, c("por (cada )?1 ?000 egresos", "universo"))) return(list(tipo = "epi", metric = "universo"))
  if (pq_has(txt, c("cuant", "numero de", "cantidad")) && !pq_has(txt, c(" tasa", "incidencia"))) return(list(tipo = "epi", metric = "conteo"))
  if (pq_has(txt, c(" tasa", "incidencia", "frecuencia", " riesgo", "por 100", "por habitante", "mas alta", "mas baja",
                    "mayor tasa", "menor tasa"))) return(list(tipo = "epi", metric = "tasa"))
  if (pq_has(txt, c("cuant", "numero", "cantidad", " egresos ", " casos ", "hospitalizaciones", " total"))) {
    return(list(tipo = "epi", metric = "conteo"))
  }
  list(tipo = "epi", metric = "ambas")
}

# Respuesta ----------------------------------------------------------------------------------------

pq_respuesta <- function(ok, parrafos, entendido = NULL, tabla = NULL, grafico = NULL, cita = NULL, modulo = NULL,
                         sugerencias = NULL) {
  list(ok = ok, parrafos = parrafos, entendido = entendido, tabla = tabla, grafico = grafico, cita = cita,
       modulo = modulo, sugerencias = sugerencias)
}

pq_no_entiendo <- function(motivo = NULL, modulos = NULL) {
  mods <- cmf_modules()
  disp <- if (!is.null(modulos)) mods[mods$id %in% modulos, ] else mods[mods$estado == "disponible", ]
  pq_respuesta(FALSE,
    c(motivo %||% "No reconocí en la pregunta un tema del observatorio.",
      paste0("Puedo responder sobre ", paste(tolower(disp$nombre), collapse = ", "),
             ": egresos, tasas, tendencias, comparación entre regiones o Servicios de Salud, sexo y edad, ",
             "valor GRD, estada y los códigos de cada definición de caso.")),
    sugerencias = cmf_preguntas_ejemplo()[1:3])
}

pq_periodo <- function(a) if (length(a) == 1) paste("En", a) else paste0("Entre ", min(a), " y ", max(a))
pq_lugar <- function(region, ss) {
  if (length(ss)) {
    s <- cmf_service_names()
    return(paste0("con residencia en el Servicio de Salud ", paste(s$nombre[match(ss, s$codigo_servicio_salud)], collapse = " y ")))
  }
  if (length(region)) {
    r <- cmf_region_names()
    return(paste0("con residencia en ", paste(r$nombre[match(region, r$codigo_region)], collapse = " y ")))
  }
  "en todo Chile"
}
pq_fuente_lbl <- function(f, actividad = NULL) {
  if (f == "grd") paste0("GRD, red pública", if (length(actividad) && "CMA" %in% actividad) ", incluida la cirugía mayor ambulatoria" else "")
  else "DEIS, todos los prestadores"
}
pq_nombre <- function(modulo, filt) {
  nom <- tolower(cmf_modules()$nombre[cmf_modules()$id == modulo])
  det <- filt$agrupacion %||% filt$subgrupo
  if (length(det)) paste0(nom, " (", paste(tolower(det), collapse = ", "), ")") else nom
}
pq_unidad <- function(metric, por, modulo = "") {
  paste0("por ", pq_fmt(por), " ", switch(metric,
    nacidos_vivos = "nacidos vivos",
    universo = if (identical(modulo, "trauma")) "egresos por trauma" else "egresos del universo de referencia",
    egresos = "egresos totales", "habitantes"),
    if (metric %in% c("ajustada", "ajustada_chile")) ", ajustada por edad" else "")
}
pq_ic <- function(r, d) if (is.na(r$tasa_li)) "" else paste0(pq_fmt(r$tasa_li, d), " a ", pq_fmt(r$tasa_ls, d))
pq_tasa_txt <- function(r, metric, por, modulo = "", ic = TRUE) {
  if (is.na(r$tasa)) return(NULL)
  d <- pq_dec(r$tasa)
  paste0(pq_fmt(r$tasa, d), " ", pq_unidad(metric, por, modulo),
         if (ic && !is.na(r$tasa_li)) paste0(" (IC 95% ", pq_ic(r, d), ")") else "")
}

pq_etiquetas <- function(d, var) {
  if (var == "region") {
    r <- cmf_region_names(); return(r$nombre[match(d$region, r$codigo_region)])
  }
  if (var == "servicio_salud") {
    s <- cmf_service_names(); return(s$nombre[match(d$servicio_salud, s$codigo_servicio_salud)])
  }
  as.character(d[[var]])
}

pq_definicion <- function(modulo, fuente) {
  m <- cmf_module(modulo)
  d <- cmf_dictionary(modulo, "diagnosticos", solo_incluidos = TRUE)
  pr <- unlist(mod_get(m, "subgrupos_principales", fuente)) %||% unique(d$subgrupo)
  d <- d[subgrupo %in% pr]
  pos <- (m$posicion[[fuente]] %||% "dx1")
  tabla <- data.frame(`Código` = d$codigo, `Descripción` = d$descripcion, Subgrupo = d$subgrupo, check.names = FALSE)
  pq_respuesta(TRUE,
    c(paste0("La definición de caso por defecto de ", tolower(m$nombre), " en ", pq_fuente_lbl(fuente), " incluye ",
             nrow(d), if (nrow(d) == 1) " código CIE-10 " else " códigos CIE-10 ", if (pos == "cualquiera") "en cualquier posición diagnóstica" else "como diagnóstico principal", "."),
      "Los subgrupos de sensibilidad se pueden activar en el explorador."),
    entendido = c(`Módulo` = m$nombre, Fuente = pq_fuente_lbl(fuente), Consulta = "Definición de caso"),
    tabla = tabla, modulo = modulo)
}

pq_oferta <- function(txt, terr) {
  o <- cmf_load("oferta_servicios")
  if (is.null(o) || !nrow(o)) return(pq_no_entiendo("Aún no hay datos de capacidad instalada y lista de espera."))
  o <- data.table::copy(o)
  s <- cmf_service_names()
  o[, nombre := s$nombre[match(servicio_salud, s$codigo_servicio_salud)]]
  o[, region := s$region[match(servicio_salud, s$codigo_servicio_salud)]]
  ind <- if (pq_has(txt, c("cirugia", "operacion", "quirurgic"))) {
    if (pq_has(txt, c("dias", "mediana", "cuanto (tiempo )?espera", "demora"))) "cirugias_mediana_dias" else "cirugias_pendientes"
  } else if (pq_has(txt, c("consulta", "espera", "esperando"))) {
    if (pq_has(txt, c("dias", "mediana", "cuanto (tiempo )?espera", "demora"))) "consultas_nuevas_mediana_dias" else "consultas_nuevas_pendientes"
  } else if (pq_has(txt, " fte ")) "fte_cmf" else "especialistas_cmf"
  por100 <- pq_has(txt, c("por 100", "por habitante", "tasa", "mas alta", "mas baja", "mayor", "menor")) &&
            !grepl("mediana", ind)
  col <- if (por100) c(especialistas_cmf = "especialistas_por_100000", fte_cmf = "fte_por_100000",
                       consultas_nuevas_pendientes = "consultas_nuevas_por_100000",
                       cirugias_pendientes = "cirugias_por_100000")[[ind]] else ind
  lbl <- c(especialistas_cmf = "especialistas CMF", fte_cmf = "FTE CMF de 44 horas",
           consultas_nuevas_pendientes = "consultas nuevas en espera", cirugias_pendientes = "cirugías en espera",
           consultas_nuevas_mediana_dias = "días de mediana de espera para una consulta nueva",
           cirugias_mediana_dias = "días de mediana de espera para una cirugía",
           especialistas_por_100000 = "especialistas CMF por 100.000 habitantes", fte_por_100000 = "FTE CMF por 100.000 habitantes",
           consultas_nuevas_por_100000 = "consultas nuevas en espera por 100.000 habitantes",
           cirugias_por_100000 = "cirugías en espera por 100.000 habitantes")
  fecha <- unique(o$version_fuente)[1]
  pie <- paste0("Fuente: RNPI (Transparencia) y Visor Ciudadano LE; listas de espera al ", fecha, ".")
  sel <- if (length(terr$servicio_salud)) o[servicio_salud %in% terr$servicio_salud]
         else if (length(terr$region)) o[region %in% terr$region] else o[0]
  d <- if (col %in% c("especialistas_por_100000", "fte_por_100000", "consultas_nuevas_por_100000", "cirugias_por_100000",
                      "fte_cmf")) 1 else 0
  if (nrow(sel)) {
    txt_v <- paste0(sel$nombre, ": ", pq_fmt(sel[[col]], d), " ", lbl[[col]])
    tabla <- if (nrow(sel) > 1) data.frame(`Servicio de Salud` = sel$nombre, Valor = pq_fmt(sel[[col]], d), check.names = FALSE)
    return(pq_respuesta(TRUE, c(paste0(paste(txt_v, collapse = "; "), "."), pie),
      entendido = c(`Módulo` = "Capacidad instalada y lista de espera", Indicador = lbl[[col]],
                    Territorio = paste(sel$nombre, collapse = ", ")),
      tabla = tabla, modulo = "oferta"))
  }
  orden <- o[order(-get(col))]
  total <- if (!grepl("por_100000|mediana", col)) paste0("En los 29 Servicios de Salud suman ", pq_fmt(sum(o[[col]], na.rm = TRUE), d), " ", lbl[[col]], ". ") else ""
  top <- head(orden[!is.na(get(col))], 3)
  pq_respuesta(TRUE,
    c(paste0(total, "Los valores más altos: ", paste0(top$nombre, " (", pq_fmt(top[[col]], d), ")", collapse = ", "), "."), pie),
    entendido = c(`Módulo` = "Capacidad instalada y lista de espera", Indicador = lbl[[col]], Territorio = "29 Servicios de Salud"),
    tabla = data.frame(`Servicio de Salud` = orden$nombre, Valor = pq_fmt(orden[[col]], d), check.names = FALSE),
    modulo = "oferta")
}

#' Responde una pregunta en lenguaje natural con los datos del observatorio
#'
#' Intérprete determinista (sin modelo de lenguaje). Reconoce el módulo, el subgrupo, la fuente, el territorio,
#' los años, el sexo, el grupo etario, la medida y la desagregación; consulta [cmf_query()] o [cmf_query_eco()]
#' y redacta la respuesta. Las cifras son las mismas del explorador, con la misma supresión de celdas pequeñas.
#' @param pregunta Texto libre en español.
#' @return Lista con `ok`, `parrafos` (texto), `entendido` (lo que se reconoció), `tabla`, `grafico` (ggplot o
#'   NULL), `cita`, `modulo` y `sugerencias`.
#' @examples
#' \dontrun{
#' r <- cmf_preguntar("¿Cuántos egresos por trauma hubo en Tarapacá entre 2019 y 2024?")
#' cat(r$parrafos, sep = "\n")
#' }
#' @export
cmf_preguntar <- function(pregunta) {
  txt <- pq_norm(substr(pregunta %||% "", 1, 400))
  if (nchar(trimws(txt)) < 3) return(pq_no_entiendo("Escribe una pregunta sobre los datos del observatorio."))
  cubo <- cmf_load("cubo")
  mods <- cmf_modules()
  disponibles <- mods$id[mods$estado == "disponible" & mods$tipo == "eventos" & mods$id %in% unique(cubo$modulo)]

  if (pq_has(txt, c("como (lo |la )?cit", "citar", "como se cita", "referencia del observatorio"))) {
    return(pq_respuesta(TRUE, c("Cita del observatorio (formato AMA). Agrega la versión de datos y la fecha de consulta:", cmf_cite()),
                        entendido = c(Consulta = "Cita")))
  }
  if (pq_has(txt, c("diferencia entre (el )?deis y (el )?grd", "que es el grd", "que es el deis", "deis o grd", "grd o deis"))) {
    return(pq_respuesta(TRUE, c(
      "El DEIS publica los egresos hospitalarios de todos los prestadores del país, públicos y privados, desde 2001. Registra la edad en tramos y no trae valor GRD.",
      "Las bases GRD (FONASA) cubren los hospitales públicos con GRD desde 2019, con edad exacta, procedimientos, cirugía mayor ambulatoria y peso GRD, que permite estimar el valor de los recursos.",
      "Sus cifras no son intercambiables: cada módulo usa en cada fuente la definición de su artículo."),
      entendido = c(Consulta = "Fuentes de datos")))
  }

  modulo <- pq_modulo(txt, disponibles)
  if (is.null(modulo)) return(pq_no_entiendo())
  if (length(modulo) > 1) {
    return(pq_no_entiendo(paste0("La pregunta calza con más de un tema (", paste(tolower(mods$nombre[match(modulo, mods$id)]), collapse = " y "),
                                 "). Precisa cuál te interesa."), modulos = modulo))
  }
  terr <- pq_territorio(txt)
  if (modulo == "oferta") return(pq_oferta(txt, terr))

  m <- cmf_module(modulo)
  fu <- pq_fuente(txt, modulo)
  medida <- pq_medida(txt)
  notas <- character()
  # Valor GRD, pabellón, severidad y cirugía ambulatoria solo existen en GRD.
  necesita_grd <- medida$tipo == "eco" || identical(medida$col, "pct_pabellon") || identical(medida$col, "pct_trauma_mayor") ||
                  pq_has(txt, c("ambulatori", " cma "))
  if (necesita_grd && fu$f != "grd") {
    if ("grd" %in% unlist(m$fuentes)) {
      if (fu$explicita) notas <- c(notas, "Esa medida solo existe en las bases GRD; la respuesta usa GRD.")
      fu$f <- "grd"
    } else return(pq_no_entiendo("Esa medida solo existe en GRD y este módulo no tiene GRD."))
  }
  fuente <- fu$f
  if (pq_has(txt, c("codigos?", " cie ", "cie 10", "definicion", "que incluye", "que diagnosticos", "como se define"))) {
    return(pq_definicion(modulo, fuente))
  }

  rango <- range(cubo$anio[cubo$modulo == modulo & cubo$fuente == fuente], na.rm = TRUE)
  an <- pq_anios(txt, rango)
  anios <- an$anios[an$anios >= rango[1] & an$anios <= rango[2]]
  if (!length(anios)) {
    otra <- setdiff(unlist(m$fuentes), fuente)
    if (!fu$explicita && length(otra) && !necesita_grd) {
      r2 <- range(cubo$anio[cubo$modulo == modulo & cubo$fuente == otra[1]], na.rm = TRUE)
      a2 <- an$anios[an$anios >= r2[1] & an$anios <= r2[2]]
      if (length(a2)) { fuente <- otra[1]; anios <- a2; rango <- r2 }
    }
    if (!length(anios)) {
      return(pq_no_entiendo(paste0("No hay datos de ", tolower(m$nombre), " para esos años en ", pq_fuente_lbl(fuente),
                                   ": la serie publicada va de ", rango[1], " a ", rango[2], "."), modulos = modulo))
    }
  }
  if (!an$todos && length(an$anios) > length(anios)) {
    notas <- c(notas, paste0(pq_fuente_lbl(fuente), " tiene datos de ", rango[1], " a ", rango[2], "; la respuesta usa ese tramo."))
  }

  filt <- pq_subgrupo(txt, modulo, fuente)
  sexo <- if (pq_has(txt, c(" hombres? ", "masculin", " varones? ")) && !pq_has(txt, c(" mujer", "femenin"))) "Hombre"
          else if (pq_has(txt, c(" mujer", "femenin")) && !pq_has(txt, c(" hombres? ", "masculin"))) "Mujer" else NULL
  if (pq_has(txt, c("hombres y mujeres", "mujeres y hombres"))) sexo <- NULL
  grupos <- cmf_age_groups(modulo, fuente)
  grupo_edad <- NULL
  for (g in grupos$etiqueta) if (grepl(paste0(" ", pq_norm(g) |> trimws(), " "), txt, fixed = TRUE) && nchar(g) > 2) grupo_edad <- c(grupo_edad, g)
  if (is.null(grupo_edad) && nrow(grupos) > 1) {
    if (pq_has(txt, c(" ninos? ", " ninas? ", "infantil", "pediatric"))) grupo_edad <- grupos$etiqueta[1]
    if (pq_has(txt, c("adultos? mayores?", "personas mayores", "tercera edad", "ancian"))) grupo_edad <- grupos$etiqueta[nrow(grupos)]
  }
  mecanismo <- NULL
  if (isTRUE(m$desagregar_mecanismo)) {
    mecs <- unique(cubo$mecanismo[cubo$modulo == modulo & cubo$fuente == fuente & !is.na(cubo$mecanismo)])
    claves <- c(agresion = " agresi| violencia", violencia = " violencia| agresi", transporte = "transporte",
                caida = " caida|baja energ", autoagresion = "autoagresi")
    for (k in names(claves)) if (grepl(paste0(" ", k), txt)) mecanismo <- c(mecanismo, mecs[grepl(claves[[k]], pq_norm(mecs))])
    mecanismo <- unique(mecanismo)
    if (!length(mecanismo)) mecanismo <- NULL
  }
  actividad <- if (pq_has(txt, c("ambulatori", " cma ")) && fuente == "grd" &&
                   "CMA" %in% unlist(mod_get(m, "actividades", fuente))) "CMA" else NULL

  by <- pq_by(txt)
  if ("mecanismo" %in% by && !isTRUE(m$desagregar_mecanismo)) by <- setdiff(by, "mecanismo")
  if ("actividad" %in% by && length(unlist(mod_get(m, "actividades", fuente))) < 2) by <- setdiff(by, "actividad")
  ranking <- pq_has(txt, c("mas alt", "(la|el) mayor", "mayor (tasa|numero|cantidad|valor|carga)", "mas baj", "(la|el) menor",
                           "menor (tasa|numero|cantidad|valor|carga)", "ranking", "lidera", "que region", "que servicio"))
  if (ranking && !any(c("region", "servicio_salud", "sexo", "grupo_edad", "mecanismo", "subgrupo") %in% by) &&
      !length(terr$region) && !length(terr$servicio_salud)) by <- c(by, "region")
  if (length(setdiff(by, "anio")) > 1) by <- c(intersect(by, "anio"), setdiff(by, "anio")[1])
  if ("anio" %in% by && length(anios) < 3) by <- setdiff(by, "anio")

  # Medida epidemiológica permitida por el módulo en esa fuente.
  mets <- unlist(mod_get(m, "metricas", fuente)) %||% c("cruda", "conteo")
  tasa_def <- setdiff(mets, "conteo")[1]
  metric <- medida$metric %||% "conteo"
  tasa_pedida <- medida$tipo == "epi" && !metric %in% c("conteo", "ambas")
  if (medida$tipo == "epi") {
    if (metric %in% c("tasa", "ambas")) metric <- if (!is.na(tasa_def)) tasa_def else "conteo"
    if (metric == "ajustada" && !"ajustada" %in% mets) {
      alt <- if ("ajustada_chile" %in% mets) "ajustada_chile" else tasa_def
      notas <- c(notas, if (alt == "ajustada_chile") "En el DEIS la edad viene en tramos y no permite el ajuste con la población OMS; uso el ajuste a la población chilena."
                        else "Esta fuente no tiene tasa ajustada por edad en este módulo; uso la tasa por defecto.")
      metric <- alt
    }
    if (!metric %in% mets) metric <- if (!is.na(tasa_def)) tasa_def else "conteo"
    if (identical(medida$metric, "conteo")) metric <- "conteo"
  }
  # Las tasas por habitantes no se desagregan por mecanismo ni por subgrupo con universo; cmf_query lo explica.
  args <- list(modulo = modulo, fuente = fuente, years = anios, subgrupo = filt$subgrupo, agrupacion = filt$agrupacion,
               region = terr$region, servicio_salud = terr$servicio_salud, sexo = sexo, grupo_edad = grupo_edad,
               mecanismo = mecanismo, actividad = actividad, by = by)
  q <- tryCatch(if (medida$tipo == "eco") do.call(cmf_query_eco, c(args[names(args) != "fuente"], list(indicador = medida$ind)))
                else do.call(cmf_query, c(args, list(metric = if (medida$tipo == "clinico") "conteo" else metric))),
                error = function(e) e)
  if (inherits(q, "error")) return(pq_no_entiendo(paste0("No pude calcular esa combinación: ", conditionMessage(q), ".")))
  meta <- attr(q, "cmf_query")
  if (!is.null(meta$motivo_sin_tasa) && medida$tipo == "epi" && metric != "conteo") {
    if (tasa_pedida) notas <- c(notas, paste0("No hay tasa para esta combinación: ", sub("^(.)", "\\L\\1", meta$motivo_sin_tasa, perl = TRUE), ". Muestro los egresos."))
    metric <- "conteo"
  }
  por <- meta$por %||% 1e5

  entendido <- c(`Módulo` = m$nombre,
                 Subgrupo = if (length(filt$agrupacion %||% filt$subgrupo)) paste(filt$agrupacion %||% filt$subgrupo, collapse = ", ") else NA,
                 Fuente = pq_fuente_lbl(fuente, actividad %||% unlist(mod_get(m, "actividades", fuente))),
                 `Años` = if (length(anios) == 1) anios else paste0(min(anios), "-", max(anios)),
                 Territorio = sub("^(con residencia en (el )?|en todo )", "", pq_lugar(terr$region, terr$servicio_salud)),
                 Sexo = sexo %||% NA, Edad = if (length(grupo_edad)) paste(grupo_edad, collapse = ", ") else NA,
                 Mecanismo = if (length(mecanismo)) paste(mecanismo, collapse = ", ") else NA,
                 Medida = if (medida$tipo == "eco") cmf_eco_label(medida$ind)
                          else if (medida$tipo == "clinico") c(estada_media = "Estada media", letalidad_pct = "Letalidad hospitalaria",
                                                              pct_pabellon = "Uso de pabellón", pct_trauma_mayor = "Trauma mayor (RISS 16 o más)")[[medida$col]]
                          else if (metric == "conteo") "Egresos" else paste(PQ_MEDIDAS_LBL[[metric]], pq_unidad(metric, por, modulo)),
                 `Desagregación` = if (length(by)) paste(c(anio = "año", region = "región", servicio_salud = "Servicio de Salud", sexo = "sexo",
                                                          grupo_edad = "grupo etario", mecanismo = "mecanismo", subgrupo = "subgrupo",
                                                          actividad = "actividad")[by], collapse = " y ") else NA)
  entendido <- entendido[!is.na(entendido)]

  sujeto <- paste0("egresos por ", pq_nombre(modulo, filt),
                   if (!is.null(sexo)) c(Hombre = " en hombres", Mujer = " en mujeres")[[sexo]] else "",
                   if (length(grupo_edad)) paste0(" de ", paste(grupo_edad, collapse = " y "), " años") else "",
                   if (length(mecanismo)) paste0(" por ", tolower(paste(mecanismo, collapse = " o "))) else "",
                   " ", pq_lugar(terr$region, terr$servicio_salud))
  fuente_txt <- paste0(" (", pq_fuente_lbl(fuente, actividad %||% unlist(mod_get(m, "actividades", fuente))), ")")
  supr_txt <- paste0("Hay menos de ", meta$n_min, " egresos en esa combinación: la cifra se omite para proteger la confidencialidad.")

  valor_de <- function(r, ic = TRUE) {
    v <- valor_de0(r, ic)
    if (is.null(v) || !length(v) || is.na(v)) NA_character_ else v
  }
  valor_de0 <- function(r, ic = TRUE) {
    if (medida$tipo == "eco") {
      v <- r$valor
      return(switch(medida$ind,
        valor_total = paste0("$", pq_fmt(v, 1), " millones"),
        valor_medio = paste0("$", pq_fmt(v * 1e3, 0), " por egreso"),
        valor_100k = paste0("$", pq_fmt(v, 2), " millones por 100.000 habitantes"),
        dias_cama = paste0(pq_fmt(v, 0), " días cama"),
        estada_media = paste0(pq_fmt(v, 1), " días de estada media")))
    }
    if (medida$tipo == "clinico") {
      v <- r[[medida$col]]
      if (is.na(v)) return(NA_character_)
      return(switch(medida$col, estada_media = paste0(pq_fmt(v, 1), " días de estada media"),
                    letalidad_pct = paste0(pq_fmt(v, 2), " % de letalidad hospitalaria"),
                    pct_pabellon = paste0(pq_fmt(v, 1), " % con uso de pabellón"),
                    pct_trauma_mayor = paste0(pq_fmt(v, 1), " % de trauma mayor (RISS 16 o más)")))
    }
    if (metric == "conteo") return(paste(pq_fmt(r$n), "egresos"))
    pq_tasa_txt(r, metric, por, modulo, ic = ic)
  }
  num_de <- function(r) {
    v <- if (medida$tipo == "eco") r$valor else if (medida$tipo == "clinico") r[[medida$col]] else if (metric == "conteo") r$n else r$tasa
    as.numeric(v %||% NA)[1]
  }

  # Tabla: egresos y la medida en columnas separadas (con IC 95% si es tasa).
  tabla_de <- function(d, primera, nombre) {
    supr <- d$suprimido %in% TRUE
    out <- data.frame(x = primera, Egresos = ifelse(supr, paste("menos de", meta$n_min), pq_fmt(d$n)), check.names = FALSE)
    names(out)[1] <- nombre
    if (medida$tipo == "epi" && metric != "conteo") {
      dd <- vapply(d$tasa, pq_dec, 0)
      out[[paste0("Tasa ", pq_unidad(metric, por, modulo))]] <- ifelse(supr, "", pq_fmt(d$tasa, max(dd, na.rm = TRUE)))
      out[["IC 95%"]] <- ifelse(supr | is.na(d$tasa_li), "", vapply(seq_len(nrow(d)), function(i) pq_ic(d[i], max(dd, na.rm = TRUE)), ""))
    } else if (medida$tipo != "epi") {
      dig <- if (medida$tipo == "eco" && medida$ind %in% c("valor_medio", "dias_cama")) 0 else 1
      out[[entendido[["Medida"]]]] <- ifelse(supr, "", vapply(seq_len(nrow(d)), function(i) pq_fmt(num_de(d[i]), dig), ""))
    }
    out
  }
  parrafos <- character(); tabla <- NULL; grafico <- NULL
  if (!length(by)) {
    r <- q[1]
    if (!nrow(q) || (is.na(r$n) && !isTRUE(r$suprimido))) {
      parrafos <- paste0(pq_periodo(anios), " no hay egresos registrados por ", pq_nombre(modulo, filt), " ",
                         pq_lugar(terr$region, terr$servicio_salud), fuente_txt, " con esos filtros.")
    } else if (isTRUE(r$suprimido)) {
      parrafos <- supr_txt
    } else {
      base <- paste0(pq_periodo(anios), " hubo ", pq_fmt(r$n), " ", sujeto, fuente_txt, ".")
      extra <- if (medida$tipo == "epi" && metric != "conteo") paste0("Tasa: ", valor_de(r), ".")
               else if (medida$tipo != "epi") { v <- valor_de(r); if (is.na(v)) "Ese indicador no está disponible en esta fuente." else paste0("Indicador: ", v, ".") }
      parrafos <- c(base, extra)
    }
  } else {
    var <- setdiff(by, "anio")[1]
    tot_args <- args; tot_args$by <- character()
    tot <- tryCatch(do.call(cmf_query, c(tot_args, list(metric = "conteo"))), error = function(e) NULL)
    if (!is.null(tot) && nrow(tot) && !isTRUE(tot$suprimido[1])) {
      parrafos <- paste0(pq_periodo(anios), " hubo ", pq_fmt(tot$n[1]), " ", sujeto, fuente_txt, ".")
    }
    d <- data.table::copy(q)
    d[, .v := NA_real_]
    for (i in seq_len(nrow(d))) data.table::set(d, i, ".v", num_de(d[i]))
    if (identical(by, "anio")) {
      s <- d[!is.na(.v)]
      if (nrow(s) >= 6) {
        # Promedios de los tres primeros y los tres últimos años con dato: más estables que dos años sueltos.
        i0 <- 1:3; i1 <- (nrow(s) - 2):nrow(s)
        m0 <- mean(s$.v[i0]); m1 <- mean(s$.v[i1]); cambio <- (m1 - m0) / m0 * 100
        u <- if (medida$tipo == "epi" && metric != "conteo") paste0(" ", pq_unidad(metric, por, modulo))
             else if (medida$tipo == "epi") " egresos al año"
             else if (medida$tipo == "eco") c(valor_total = paste(" millones de", moneda_lbl(TRUE)), valor_medio = paste(" miles de", moneda_lbl(TRUE), "por egreso"),
                                             valor_100k = paste(" millones de", moneda_lbl(TRUE), "por 100.000 habitantes"), dias_cama = " días cama",
                                             estada_media = " días")[[medida$ind]]
             else c(estada_media = " días", letalidad_pct = " %", pct_pabellon = " %", pct_trauma_mayor = " %")[[medida$col]]
        parrafos <- c(parrafos, paste0("El promedio de ", s$anio[1], "-", s$anio[3], " fue ", pq_fmt(m0, pq_dec(m0)), u,
                                       " y el de ", s$anio[i1[1]], "-", s$anio[nrow(s)], ", ", pq_fmt(m1, pq_dec(m1)),
                                       if (medida$tipo == "clinico" && grepl("^(pct|letalidad)", medida$col)) {
                                         dif <- m1 - m0; paste0(" (", if (dif >= 0) "+" else "", pq_fmt(dif, 1), " puntos porcentuales)")
                                       } else if (is.finite(cambio)) paste0(" (", if (cambio >= 0) "+" else "", pq_fmt(cambio, 1), " %)") else "", "."),
                       paste0("El valor más alto de la serie fue en ", s$anio[which.max(s$.v)], " y el más bajo, en ", s$anio[which.min(s$.v)], "."))
      } else if (nrow(s) >= 2) {
        parrafos <- c(parrafos, paste0("La serie va de ", valor_de(s[1], FALSE), " en ", s$anio[1], " a ", valor_de(s[.N], FALSE),
                                       " en ", s$anio[nrow(s)], "."))
      }
      if (nrow(s) >= 3 && medida$tipo == "epi") {
        grafico <- tryCatch(cmf_plot_trend(q, color = m$color %||% NULL), error = function(e) NULL)
      }
      tabla <- tabla_de(d, d$anio, "Año")
    } else {
      d[, .etq := pq_etiquetas(d, var)]
      if ("anio" %in% by) {
        # Con año y otra dimensión: total por categoría para el período y la serie en el gráfico.
        grafico <- if (medida$tipo == "epi") tryCatch(cmf_plot_trend(q), error = function(e) NULL)
        res <- d[!is.na(.etq) & !es_sin_dato(.etq), .(n = sum(n, na.rm = TRUE)), by = .etq][order(-n)]
        tabla <- data.frame(x = res$.etq, `Egresos en el período` = pq_fmt(res$n), check.names = FALSE)
        names(tabla)[1] <- c(region = "Región", servicio_salud = "Servicio de Salud", sexo = "Sexo", grupo_edad = "Grupo etario",
                             mecanismo = "Mecanismo", subgrupo = "Subgrupo", actividad = "Actividad")[[var]]
      } else {
        dd <- d[!is.na(.etq) & !es_sin_dato(.etq)]
        asc <- pq_has(txt, c("mas baj", "menor", "minim", "mejor"))
        dd <- dd[order(if (asc) .v else -.v, na.last = TRUE)]
        val <- dd[!is.na(.v)]
        if (nrow(val)) {
          k <- min(3, nrow(val))
          parrafos <- c(parrafos, paste0(if (asc) "Los valores más bajos: " else "Los valores más altos: ",
                                         paste0(val$.etq[1:k], " (", vapply(1:k, function(i) valor_de(val[i], FALSE), ""), ")", collapse = "; "), "."))
        }
        if (any(dd$suprimido %in% TRUE)) parrafos <- c(parrafos, paste0("Las categorías con menos de ", meta$n_min, " egresos se omiten."))
        tabla <- tabla_de(dd, dd$.etq, c(region = "Región", servicio_salud = "Servicio de Salud", sexo = "Sexo", grupo_edad = "Grupo etario",
                                          mecanismo = "Mecanismo", subgrupo = "Subgrupo", actividad = "Actividad")[[var]])
      }
    }
  }
  if (medida$tipo == "eco" && grepl("^valor", medida$ind)) notas <- c(notas, paste0("Valor GRD en ", moneda_lbl(), ": valor de los recursos para el financiador público, no costo observado."))
  if (!length(parrafos)) parrafos <- supr_txt
  pq_respuesta(TRUE, c(parrafos, notas), entendido = entendido, tabla = tabla, grafico = grafico,
               cita = tryCatch(cmf_cite(q), error = function(e) cmf_cite()), modulo = modulo)
}
