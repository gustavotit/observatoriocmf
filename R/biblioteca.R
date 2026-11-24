# Biblioteca de epidemiología CMF de Chile ---------------------------------------------------------------
#
# config/publicaciones.yml (lista `articulos`) reúne tres tipos de referencias:
#   observatorio        artículos cuyos números reproduce un módulo del observatorio
#   serie_hospitalaria  series de casos de un hospital o centro chileno
#   nacional            estudios nacionales o regionales (bases administrativas, encuestas, registros)
#   revision            revisiones con datos chilenos
# El panel de administración agrega y quita referencias (admin/R/biblioteca.R, con autocompletado por DOI) y la
# app las muestra con filtros. Los tipos y campos deben coincidir en ambos archivos.

BIBLIO_TIPOS <- c(observatorio = "Reproducido por el observatorio", serie_hospitalaria = "Serie hospitalaria",
                  nacional = "Estudio nacional o regional", revision = "Revisión")
BIBLIO_CAMPOS <- c("tipo", "modulo", "autores", "anio", "titulo", "revista", "referencia", "doi", "pmid", "estado",
                   "institucion", "region", "periodo", "casos", "diseno", "datos")

#' Biblioteca de publicaciones CMF de Chile
#' @param archivo Ruta de publicaciones.yml (por defecto, la de los datos publicados).
#' @return data.frame con una fila por referencia (campos de `BIBLIO_CAMPOS`).
#' @export
cmf_biblioteca <- function(archivo = NULL) {
  f <- archivo %||% file.path(cmf_data_dir(), "publicaciones.yml")
  if (!file.exists(f)) return(biblio_df(list()))
  x <- tryCatch(yaml::read_yaml(f, fileEncoding = "UTF-8")$articulos, error = function(e) list())
  biblio_df(Filter(function(a) nzchar(a$titulo %||% ""), x %||% list()))
}

biblio_df <- function(x) {
  col <- function(k) vapply(x, function(a) {
    v <- a[[k]]
    if (is.null(v) || !length(v) || all(is.na(v))) "" else paste(as.character(v), collapse = ", ")
  }, "")
  d <- as.data.frame(stats::setNames(lapply(BIBLIO_CAMPOS, col), BIBLIO_CAMPOS), stringsAsFactors = FALSE)
  if (nrow(d)) d$tipo[!nzchar(d$tipo)] <- "observatorio"
  d
}

# Cita AMA/Vancouver en texto plano.
biblio_cita <- function(r) {
  paste0(r$autores, ". ", r$titulo, ". ", if (nzchar(r$revista)) paste0(r$revista, ". ") else "", r$anio,
         if (nzchar(r$referencia)) paste0(";", r$referencia) else "", ".",
         if (nzchar(r$doi)) paste0(" doi:", r$doi) else "")
}

#' Exporta la biblioteca a RIS (Zotero, Mendeley, EndNote)
#' @export
cmf_biblioteca_ris <- function(d = cmf_biblioteca()) {
  unlist(lapply(seq_len(nrow(d)), function(i) {
    r <- d[i, ]
    au <- trimws(strsplit(r$autores, ",")[[1]])
    vol <- sub("\\(.*$|:.*$", "", r$referencia); iss <- sub("^.*\\((.*)\\).*$", "\\1", r$referencia)
    pag <- sub("^[^:]*:?", "", r$referencia)
    c("TY  - JOUR", paste0("AU  - ", au[nzchar(au)]), paste0("PY  - ", r$anio), paste0("TI  - ", r$titulo),
      if (nzchar(r$revista)) paste0("JO  - ", r$revista),
      if (nzchar(vol) && vol != r$referencia) paste0("VL  - ", vol),
      if (grepl("\\(", r$referencia)) paste0("IS  - ", iss),
      if (nzchar(pag)) paste0("SP  - ", pag),
      if (nzchar(r$doi)) paste0("DO  - ", r$doi),
      paste0("KW  - ", BIBLIO_TIPOS[r$tipo] %||% r$tipo),
      if (nzchar(r$institucion)) paste0("N1  - ", r$institucion, if (nzchar(r$periodo)) paste0(", ", r$periodo)),
      "ER  - ", "")
  }))
}
