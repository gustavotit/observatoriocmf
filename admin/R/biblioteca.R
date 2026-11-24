# Biblioteca (config/publicaciones.yml): agregar y quitar referencias desde el panel --------------------------
# Los tipos y campos coinciden con R/biblioteca.R del paquete (la app pública).

BIBLIO_TIPOS <- c(observatorio = "Reproducido por el observatorio", serie_hospitalaria = "Serie hospitalaria",
                  nacional = "Estudio nacional o regional", revision = "Revisión")
BIBLIO_CAMPOS <- c("tipo", "modulo", "autores", "anio", "titulo", "revista", "referencia", "doi", "pmid", "estado",
                   "institucion", "region", "periodo", "casos", "diseno", "datos")
REGIONES_CHILE <- c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
                    "Metropolitana", "O'Higgins", "Maule", "Ñuble", "Biobío", "La Araucanía", "Los Ríos",
                    "Los Lagos", "Aysén", "Magallanes", "Nacional", "Varias regiones")


# Bloque YAML de una referencia nueva, entre comillas dobles; se agrega al final sin tocar los comentarios.
biblio_yaml_entrada <- function(a) {
  q <- function(v) paste0('"', gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", as.character(v))), '"')
  a <- a[intersect(BIBLIO_CAMPOS, names(a))]
  a <- a[vapply(a, function(v) !is.null(v) && length(v) && !is.na(v[1]) && nzchar(trimws(as.character(v[1]))), TRUE)]
  ks <- names(a)
  lin <- vapply(seq_along(ks), function(i) {
    v <- trimws(as.character(a[[i]]))
    val <- if (ks[i] %in% c("anio", "casos") && grepl("^[0-9]+$", v)) v else q(v)
    paste0(if (i == 1) "  - " else "    ", ks[i], ": ", val)
  }, "")
  paste(c("", lin), collapse = "\n")
}

biblio_agregar_texto <- function(texto, a) {
  texto <- sub("(?m)^articulos:\\s*\\[\\s*\\]\\s*$", "articulos:", texto, perl = TRUE)
  if (!grepl("(?m)^articulos:", texto, perl = TRUE)) texto <- paste0(sub("\\s*$", "", texto), "\n\narticulos:")
  paste0(sub("\\s*$", "", texto), "\n", biblio_yaml_entrada(a), "\n")
}

# Quita la referencia i (en el orden del archivo) sin tocar comentarios ni las demás.
biblio_quitar_texto <- function(texto, i) {
  l <- strsplit(texto, "\n", fixed = TRUE)[[1]]
  ini <- grep("^  - ", l)
  if (i > length(ini)) stop("No existe esa referencia")
  fin <- ini[i]
  while (fin < length(l) && grepl("^    \\S", l[fin + 1])) fin <- fin + 1
  paste(l[-(ini[i]:fin)], collapse = "\n")
}

# Metadatos desde Crossref para un DOI: autores en formato Vancouver (6 y "et al"), título, revista abreviada,
# año y volumen(número):páginas.
crossref_meta <- function(m) {
  inic <- function(g) {
    p <- strsplit(g %||% "", "[ .]+")[[1]]; p <- p[nzchar(p)]
    paste(vapply(p, function(x) paste(toupper(substr(strsplit(x, "-")[[1]], 1, 1)), collapse = ""), ""), collapse = "")
  }
  au <- vapply(m$author %||% list(), function(a) trimws(paste(a$family %||% a$name %||% "", inic(a$given))), "")
  au <- au[nzchar(au)]
  autores <- if (length(au) > 6) paste(c(au[1:6], "et al"), collapse = ", ") else paste(au, collapse = ", ")
  fecha <- m$`published-print`$`date-parts` %||% m$issued$`date-parts` %||% m$`published-online`$`date-parts`
  anio <- tryCatch(as.character(fecha[[1]][[1]]), error = function(e) "")
  pag <- m$page %||% m$`article-number` %||% ""
  ref <- paste0(m$volume %||% "", if (!is.null(m$issue)) paste0("(", m$issue, ")") else "",
                if (nzchar(pag)) paste0(":", pag) else "")
  limpiar <- function(x) trimws(gsub("<[^>]+>", "", x %||% ""))
  list(autores = autores, anio = anio, titulo = limpiar(unlist(m$title)[1]),
       revista = limpiar(unlist(m$`short-container-title`)[1] %||% unlist(m$`container-title`)[1]),
       referencia = ref, doi = m$DOI %||% "")
}

crossref_doi <- function(doi) {
  doi <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", trimws(doi))
  if (!nzchar(doi)) stop("Escribe un DOI")
  resp <- httr2::request("https://api.crossref.org/works") |>
    httr2::req_url_path_append(doi) |>
    httr2::req_user_agent("observatoriocmf-admin") |>
    httr2::req_timeout(20) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) != 200) stop("Crossref no encontró el DOI ", doi)
  crossref_meta(httr2::resp_body_json(resp)$message)
}
