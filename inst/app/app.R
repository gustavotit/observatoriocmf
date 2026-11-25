# Observatorio CMF Chile: aplicación pública
#
# En el despliegue, pipeline/prepare_app.R copia aquí las funciones del paquete (R/) y los datos (data/).
# En desarrollo local, la app las toma directamente del repositorio.

library(shiny)
library(bslib)
library(data.table)

if (!exists("cmf_query", mode = "function")) {
  for (f in list.files("../../R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
}
options(cmf.data_dir = if (dir.exists("data")) "data" else "../extdata")

cubo0 <- cmf_load("cubo")
hay_datos <- !is.null(cubo0) && nrow(cubo0) > 0
modulos <- cmf_modules()
mod_ev <- if (nrow(modulos)) modulos[modulos$tipo == "eventos", ] else modulos
# El explorador muestra solo los módulos publicados (estado "disponible") que tienen datos.
mod_ev_datos <- if (hay_datos) mod_ev[mod_ev$id %in% unique(cubo0$modulo) & mod_ev$estado == "disponible", ] else mod_ev[0, ]
regiones <- cmf_region_names()[order(orden_norte_sur)]
servicios <- cmf_service_names()
manifest <- cmf_manifest()
pub <- manifest$parametros$publicacion %||% list()
# Biblioteca de epidemiología CMF de Chile (config/publicaciones.yml; en el despliegue, data/publicaciones.yml).
biblio <- local({
  f <- c("../../config/publicaciones.yml", file.path(cmf_data_dir(), "publicaciones.yml"))
  f <- f[file.exists(f)][1]
  if (is.na(f)) cmf_biblioteca("") else cmf_biblioteca(f)
})
anios_disp <- if (hay_datos) range(cubo0$anio, na.rm = TRUE) else c(2001, 2025)
prevision_disp <- if (hay_datos) sort(setdiff(unique(cubo0$prevision), c(NA, "Desconocida"))) else character()
oferta <- cmf_load("oferta_servicios")
validacion <- tryCatch(jsonlite::read_json(file.path(cmf_data_dir(), "validacion.json")), error = function(e) NULL)
# Dimensión económica: módulos con valor GRD (GRD entre sus fuentes) y estudios económicos importados.
eco_ok <- if (nrow(modulos)) stats::setNames(vapply(modulos$id, function(id) cmf_economia_disponible(id) && hay_datos &&
                                                       any(cubo0$modulo == id & cubo0$fuente == "grd"), TRUE), modulos$id) else logical()
estudios <- cmf_estudios()

FUENTES_LBL <- c(egresos_deis = "DEIS, egresos de todos los prestadores", grd = "GRD, hospitales públicos")
FUENTES_CORTO <- c(egresos_deis = "DEIS", grd = "GRD", oferta_servicios = "RNPI (Transparencia) y Visor Ciudadano LE")
METRICAS_LBL <- c(ajustada = "Tasa ajustada por edad, por 100.000",
                  ajustada_chile = "Tasa ajustada por edad (población chilena), por 100.000",
                  cruda = "Tasa cruda, por 100.000", nacidos_vivos = "Por 1.000 nacidos vivos",
                  universo = "Por 1.000 egresos del universo", egresos = "Por 100.000 egresos totales",
                  conteo = "Egresos, n")
ECO_LBL <- c(valor_total = "Valor GRD total, millones de CLP", valor_medio = "Valor GRD medio por egreso, miles de CLP",
             valor_100k = "Valor GRD por 100.000 habitantes", dias_cama = "Días cama", estada_media = "Estada media, días")
DIM_LBL <- c(epi = "Epidemiología", eco = "Economía")
fmt_num <- function(x, d = 0) {
  d <- rep_len(d, length(x))
  vapply(seq_along(x), function(i) if (is.na(x[i])) "" else
    formatC(x[i], format = "f", digits = d[i], big.mark = ".", decimal.mark = ","), "")
}

# Sistema visual (inspirado en Continuum de Pentagram y en "1,374 Days"):
# - Titulares y controles en una grotesca (Hanken Grotesk); texto corrido en serif (Source Serif 4).
# - Naranja dorado como color de firma; cada módulo tiene su color (paleta JAMA) y se marca con una pestaña
#   semicircular en el borde, como las pestañas de sección de la revista.
# - Mucho blanco, una columna principal y una columna de margen para las cifras clave.
# - Esquinas rectas en todo lo interactivo; reglas finas negras como estructura.
FIRMA <- "#D98A2B"
tema <- bs_theme(
  version = 5, bg = "#FFFFFF", fg = "#141414", primary = "#141414", secondary = "#5C5C5C",
  success = "#79AF97", info = "#00A1D5", warning = FIRMA, danger = "#B24745",
  base_font = font_google("Source Serif 4", local = FALSE),
  heading_font = font_google("Hanken Grotesk", local = FALSE),
  code_font = font_google("IBM Plex Mono", local = FALSE),
  "border-radius" = "0", "border-radius-sm" = "0", "border-radius-lg" = "0",
  "card-border-color" = "#141414", "input-border-color" = "#141414", "font-size-base" = "1.02rem"
)

css <- "
:root { --mod:#D98A2B; --mod-tinte:#FAF8F3; --tinta:#141414; --firma:#D98A2B; --gris:#5C5C5C; --regla:#141414; --tenue:#E4E2DC; --papel:#FAF8F3;
        --sans:'Hanken Grotesk', 'Helvetica Neue', Arial, sans-serif; --serif:'Source Serif 4', Georgia, serif; --mono:'IBM Plex Mono', monospace; }
*, .btn, .form-control, .form-select, .card, .nav-link, .selectize-input, .selectize-dropdown, .dropdown-menu,
.irs--shiny .irs-bar, .irs--shiny .irs-line, .irs--shiny .irs-handle, .irs--shiny .irs-from, .irs--shiny .irs-to,
.irs--shiny .irs-single, .badge, .nav-tabs .nav-link { border-radius:0 !important; }
body { background:#fff; color:var(--tinta); }
h1, h2, h3, h4, h5, .navbar, .btn, label, .control-label, .form-control, .form-select, .selectize-input, .selectize-dropdown,
.nav-link, .irs, table, .kpi, .sans { font-family:var(--sans) !important; }
h1, h2, h3 { font-weight:700; letter-spacing:-.025em; }
.navbar { background:#fff !important; border-bottom:1px solid var(--regla); }
.navbar .navbar-brand { font-weight:800; letter-spacing:-.02em; color:var(--tinta) !important; display:flex; align-items:center; gap:10px; }
.navbar .navbar-brand .sello { width:14px; height:28px; background:var(--mod); transition:background .3s; border-radius:28px 0 0 28px !important; display:inline-block; }
.navbar .nav-link { font-size:.86rem; font-weight:600; color:var(--tinta) !important; }
.navbar .nav-link.active { box-shadow: inset 0 -3px 0 var(--mod); transition:box-shadow .3s; }
.rotulo { font-family:var(--serif) !important; font-size:.8rem; letter-spacing:.08em; text-transform:uppercase; color:var(--gris); }
.pagina { max-width:1240px; margin:0 auto; padding:36px 28px 80px; }
/* Pestaña semicircular de módulo (borde de página) */
.pestana { position:absolute; right:0; top:22px; width:18px; height:36px; border-radius:36px 0 0 36px !important; }
/* Portada */
/* Portada sobria: título corto, un párrafo y las cifras; la lámina hace el trabajo visual. */
.portada { display:grid; grid-template-columns: minmax(0, 6fr) minmax(0, 5fr); gap:56px; padding:28px 0 8px; align-items:stretch; }
@media (max-width: 900px) { .portada { grid-template-columns: 1fr; } }
.portada .texto { display:flex; flex-direction:column; justify-content:space-between; gap:36px; }
.portada h1 { font-size:2.1rem; line-height:1.12; font-weight:700; letter-spacing:-.02em; margin:0; max-width:22ch; }
.portada .bajada { font-size:1.06rem; line-height:1.6; max-width:50ch; margin:16px 0 0; color:#2a2a2a; }
.portada .cifras { grid-template-columns:repeat(2, minmax(0, 1fr)); margin-top:0; }
.portada .cifra { font-size:1.75rem; }
.lamina { margin:0; }
.lamina img { display:block; width:100%; max-height:560px; object-fit:contain; object-position:left top; background:#fff; }
.lamina figcaption { font-size:.86rem; line-height:1.45; color:var(--gris); margin-top:10px; max-width:46ch; }
.cifras { display:grid; grid-template-columns:repeat(auto-fit, minmax(160px, 1fr)); margin-top:28px; }
.cifras > div { padding:12px 18px 4px 0; border-top:1px solid var(--regla); margin-right:18px; }
.cifra { font-family:var(--sans); font-size:2.3rem; font-weight:700; letter-spacing:-.03em; line-height:1.05; }
/* Índice de módulos, como el índice de una revista */
.indice { margin-top:52px; }
.indice-fila { position:relative; display:grid; grid-template-columns: 96px minmax(0, 3fr) minmax(0, 4fr) minmax(0, 3fr) 176px;
               gap:22px; align-items:start; padding:22px 40px 22px 0; border-top:1px solid var(--regla); color:inherit; text-decoration:none; }
.indice-fila:last-child { border-bottom:1px solid var(--regla); }
@media (max-width: 900px) { .indice-fila { grid-template-columns: 64px 1fr; } .indice-fila .num { font-size:2.6rem; } .indice-fila > .ocultable { display:none; }
                            .indice-fila .dims { grid-column:2; flex-direction:row; flex-wrap:wrap; } }
.indice-fila.activo:hover { background:var(--papel); }
.indice-fila.activo .num { color:var(--c); }
/* Toda la fila abre el explorador del módulo (enlace estirado); los rectángulos abren cada dimensión. */
.indice-fila .fila-enlace { position:absolute; inset:0; z-index:1; }
.indice-fila .dims { position:relative; z-index:2; display:flex; flex-direction:column; gap:8px; align-self:center; }
.indice-fila .dim { display:block; border:1.5px solid var(--c); color:var(--c); background:transparent; padding:7px 12px;
                    font-family:var(--sans); font-size:.8rem; font-weight:700; letter-spacing:.01em; text-decoration:none;
                    white-space:nowrap; transition:box-shadow .15s; }
.indice-fila .dim:hover { box-shadow:inset 0 0 0 1.5px var(--c); color:var(--c); }
.indice-fila .dim .flecha { float:right; margin-left:10px; transition:transform .15s; }
.indice-fila .dim:hover .flecha { transform:translateX(3px); }
.indice-fila .num { font-family:var(--sans); font-weight:800; font-size:3.6rem; line-height:1; letter-spacing:-.04em;
                    font-variant-numeric:lining-nums tabular-nums; align-self:center; }
.indice-fila h3 { font-size:1.5rem; margin:0; line-height:1.1; }
.indice-fila .estado { font-family:var(--serif); font-size:.76rem; letter-spacing:.08em; text-transform:uppercase; color:var(--gris); margin-top:8px; }
.indice-fila p { margin:0; font-size:1rem; line-height:1.5; }
.indice-fila .codigos { font-family:var(--serif); font-size:.9rem; line-height:1.6; color:#2a2a2a; }
.indice-fila.inactivo { color:#8a8a8a; } .indice-fila.inactivo .codigos { color:#9a9a9a; }
.articulo { position:relative; display:grid; grid-template-columns:96px minmax(0, 1fr); gap:22px; align-items:center;
            padding:24px 40px 24px 0; border-top:1px solid var(--regla); }
.articulo:last-child { border-bottom:1px solid var(--regla); } .pagina h2 + .articulo { border-top:0; }
.articulo .anio { font-family:var(--sans); font-weight:800; font-size:2.4rem; line-height:1; letter-spacing:-.03em; }
.articulo .ref { margin:6px 0 0; font-size:1.06rem; line-height:1.55; max-width:78ch; }
.articulo .ref a { color:inherit; text-decoration-thickness:1px; text-underline-offset:3px; }
.articulo .anio { color:var(--c, var(--tinta)); }
.filtros-biblio { gap:14px; border-bottom:1px solid var(--regla); padding-bottom:10px; }
.filtros-biblio .shiny-input-container { margin-bottom:0; }
/* Explorador */
.bslib-sidebar-layout > .sidebar { background:var(--mod-tinte) !important; transition:background .3s; border-right:1px solid var(--regla) !important; }
.sidebar .control-label { font-size:.76rem; text-transform:uppercase; letter-spacing:.08em; font-weight:700; }
.sidebar .checkbox label, .sidebar .radio label, .sidebar .form-check-label { font-family:var(--sans); font-size:.92rem; font-weight:400; }
.sidebar .shiny-input-container { border-top:1px solid #d6d3cc; padding-top:10px; }
.form-control, .form-select, .selectize-input { border:1px solid var(--tinta) !important; box-shadow:none !important; }
.btn { font-weight:700; font-size:.82rem; padding:.5rem 1rem; }
.btn-primary { background:var(--tinta); border-color:var(--tinta); }
.btn-primary:hover { background:var(--firma); border-color:var(--firma); color:var(--tinta); }
.btn-outline-secondary { color:var(--tinta); border-color:var(--tinta); }
.btn-outline-secondary:hover { background:var(--tinta); color:#fff; }
.irs--shiny .irs-bar { background:var(--tinta); border-color:var(--tinta); }
.irs--shiny .irs-handle { border:2px solid var(--tinta); }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:var(--tinta); }
.form-check-input:checked { background-color:var(--tinta); border-color:var(--tinta); }
.encabezado-modulo { position:relative; padding:4px 0 16px 0; border-bottom:3px solid var(--regla); margin-bottom:18px;
                     display:flex; justify-content:space-between; align-items:flex-end; gap:24px; flex-wrap:wrap; }
.dimensiones { display:flex; }
.dimensiones a { border:1.5px solid var(--mod); color:var(--mod); padding:7px 18px; font-family:var(--sans); font-size:.84rem;
                 font-weight:700; text-decoration:none; margin-left:-1.5px; transition:background .2s, color .2s; }
.dimensiones a.activa { background:var(--mod); color:#fff; }
.dimensiones a.deshabilitada { border-color:#D6D3CC; color:#B8B3AC; pointer-events:none; }
.estudio-cab { padding:18px 0 6px; }
.estudio-cab h3 { font-size:1.45rem; margin:6px 0 4px; line-height:1.15; max-width:60ch; }
.estudio-cab .meta { font-family:var(--serif); color:var(--gris); font-size:.95rem; }
.estudio .cifras { margin-top:14px; }
.estudio .cifras .s { font-family:var(--serif); font-size:.86rem; color:var(--gris); margin-top:4px; }
.nota { font-size:.9rem; color:#2a2a2a; max-width:90ch; margin-top:12px; }
.encabezado-modulo h2, .pagina .encabezado-modulo h2 { font-size:2.2rem; margin:6px 0 0; border-bottom:0; padding-bottom:0; }
.encabezado-modulo .pestana { top:10px; width:22px; height:44px; border-radius:44px 0 0 44px !important; }
.margen { border-left:1px solid var(--regla); padding-left:22px; }
.margen .kpi { padding:10px 0 12px; border-bottom:1px solid var(--tenue); }
.margen .kpi .v { font-size:1.8rem; font-weight:700; letter-spacing:-.02em; line-height:1.1; }
.margen .kpi .s { font-family:var(--sans); font-size:.8rem; color:var(--gris); }
.nav-underline { border-bottom:1px solid var(--regla); gap:0; }
.nav-underline .nav-link { font-family:var(--sans); color:var(--gris) !important; font-weight:600; font-size:.88rem; padding:.5rem 1rem .5rem 0; margin-right:1rem; }
.nav-underline .nav-link.active { color:var(--tinta) !important; border-bottom:3px solid var(--tinta) !important; font-weight:700; }
.vista-opciones .shiny-input-container { margin-bottom:0; }
.vista-opciones { display:flex; gap:36px; flex-wrap:wrap; padding:12px 0 2px; font-family:var(--sans); }
.descargas { display:grid; gap:10px; margin-top:18px; }
.descargas .shiny-input-container { width:100% !important; margin-bottom:0; }
.bloque-cita { border-left:3px solid var(--mod); transition:border-color .3s; padding:4px 0 4px 16px; font-size:.98rem; white-space:pre-line; }
pre.codigo { background:var(--mod-tinte); border:1px solid var(--mod); transition:background .3s, border-color .3s; padding:14px; font-size:.78rem; white-space:pre-wrap; font-family:var(--mono); }
.aviso { border-left:3px solid #B24745; padding:6px 12px; font-size:.92rem; margin-bottom:14px; }
/* Tablas estilo revista: solo reglas horizontales */
table.dataTable { border-collapse:collapse !important; font-size:.88rem; }
table.dataTable thead th { border-top:1.5px solid var(--tinta) !important; border-bottom:1px solid var(--tinta) !important; font-weight:700; background:#fff; }
table.dataTable tbody td { border-top:none !important; padding:5px 10px !important; }
table.dataTable tbody tr:last-child td { border-bottom:1.5px solid var(--tinta) !important; }
table.dataTable tbody tr.odd, table.dataTable tbody tr.even { background:#fff !important; }
table.dataTable tbody tr:hover { background:var(--papel) !important; }
.tabla-simple { width:100%; border-collapse:collapse; font-size:.92rem; }
.tabla-simple th { border-top:1.5px solid var(--tinta); border-bottom:1px solid var(--tinta); text-align:left; padding:6px 10px 6px 0; }
.tabla-simple td { padding:5px 10px 5px 0; }
.tabla-simple tr:last-child td { border-bottom:1.5px solid var(--tinta); }
.pagina h2 { font-size:2rem; border-bottom:3px solid var(--regla); padding-bottom:10px; margin:44px 0 16px; }
.pagina h2:first-child { margin-top:0; }
.pagina ol li, .pagina p { max-width:78ch; line-height:1.6; }
.pie { border-top:1px solid var(--regla); margin-top:56px; padding:16px 0; font-size:.82rem; color:var(--gris); font-family:var(--sans); }
/* Pregunta al observatorio: intérprete determinista, sin modelo de lenguaje */
.pregunta { margin-top:52px; border-top:3px solid var(--regla); padding-top:14px; }
.pregunta h2 { font-size:1.6rem; margin:4px 0 0; border:0; padding:0; }
.pregunta .caja { display:flex; margin-top:18px; position:relative; }
.sugerencias { position:absolute; left:0; right:0; top:100%; z-index:20; background:#fff; border:1.5px solid var(--tinta); border-top:0;
               box-shadow:0 10px 24px rgba(20,20,20,.08); max-height:340px; overflow-y:auto; }
.sugerencias .op { padding:10px 16px; font-family:var(--sans); font-size:.95rem; cursor:pointer; border-top:1px solid var(--tenue); color:#3a3a3a; }
.sugerencias .op b { color:var(--tinta); font-weight:700; }
.sugerencias .op:hover, .sugerencias .op.activa { background:var(--papel); box-shadow:inset 3px 0 0 var(--firma); }
.pregunta .caja input { flex:1; border:1.5px solid var(--tinta); border-right:0; padding:14px 16px; font-family:var(--sans); font-size:1.05rem;
                        outline:none; background:#fff; }
.pregunta .caja input:focus { box-shadow:inset 0 -3px 0 var(--firma); }
.pregunta .caja .btn { border:1.5px solid var(--tinta); padding:0 26px; font-size:.9rem; }
.pregunta .ejemplos { display:flex; flex-wrap:wrap; gap:8px; margin-top:12px; }
.pregunta .ejemplos a { border:1px solid #C9C5BD; color:var(--tinta); padding:6px 11px; font-family:var(--sans); font-size:.8rem;
                        text-decoration:none; transition:border-color .15s, background .15s; }
.pregunta .ejemplos a:hover { border-color:var(--tinta); background:var(--papel); }
.respuesta { border-top:1px solid var(--regla); margin-top:26px; padding-top:16px; display:grid;
             grid-template-columns:minmax(0, 7fr) minmax(0, 5fr); gap:36px; }
@media (max-width: 900px) { .respuesta { grid-template-columns:1fr; gap:16px; } }
.respuesta .q { font-family:var(--sans); font-weight:700; font-size:1.15rem; letter-spacing:-.01em; margin:0 0 10px; }
.respuesta .entendido { display:flex; flex-wrap:wrap; gap:4px 14px; margin-bottom:12px; }
.respuesta .entendido span { font-family:var(--sans); font-size:.76rem; color:var(--gris); }
.respuesta .entendido b { color:var(--tinta); font-weight:600; }
.respuesta p { font-size:1.02rem; line-height:1.6; margin:0 0 8px; max-width:68ch; }
.respuesta .no-ok p:first-child { border-left:3px solid var(--firma); padding-left:12px; }
.respuesta .bloque-cita { font-size:.84rem; margin-top:14px; border-left-color:var(--firma); }
.respuesta .ir { font-family:var(--sans); font-size:.84rem; font-weight:700; color:var(--tinta); text-decoration:none;
                 border-bottom:1.5px solid var(--firma); display:inline-block; margin-top:12px; }
.respuesta .tabla-simple { font-size:.84rem; }
.respuesta .tabla-simple td, .respuesta .tabla-simple th { padding-right:14px; }
.respuesta .mas { font-family:var(--sans); font-size:.78rem; color:var(--gris); margin-top:6px; }
"

codes_for <- function(id) {
  d <- cmf_dictionary(id, "diagnosticos", solo_incluidos = TRUE)
  if (is.null(d) || !nrow(d)) return("")
  m <- tryCatch(cmf_module(id), error = function(e) list())
  pr <- unlist(mod_get(m, "subgrupos_principales", m$fuentes[[1]])) %||% unique(d$subgrupo)
  paste(d[subgrupo %in% pr]$codigo, collapse = "  ")
}
# Tablas sin rayas ni puntos medios (textos de datos publicados con versiones anteriores).
sin_rayas <- function(x) gsub("\u2013", "-", gsub("\\s*[\u2014\u00b7]\\s*", ", ", x))
dt_pub <- function(x, ...) {
  x <- data.table::copy(data.table::as.data.table(x))
  for (j in names(x)) if (is.character(x[[j]])) data.table::set(x, j = j, value = sin_rayas(x[[j]]))
  DT::datatable(x, ...)
}
# Estimandos de un estudio económico en cifras grandes (como la portada), con su IC 95%.
ESTIMANDO_LBL <- c(LOS = "Exceso de estada por egreso", `DRG resource value` = "Exceso de valor GRD por egreso",
                   Productivity = "Pérdida temporal de productividad")
estimando_fmt <- function(r) {
  u <- tolower(r$unidad %||% "")
  tit <- if (!is.na(ESTIMANDO_LBL[r$resultado])) ESTIMANDO_LBL[[r$resultado]] else r$estimando
  if (grepl("day|día", u)) {
    v <- paste(fmt_num(r$estimacion, 2), "días"); ic <- paste0(fmt_num(r$li, 2), "-", fmt_num(r$ls, 2), " días")
  } else if (grepl("million|millon", u)) {
    v <- paste0("$", fmt_num(r$estimacion), " M"); ic <- paste0("$", fmt_num(r$li), "-", fmt_num(r$ls), " M")
  } else if (grepl("clp|\\$", u)) {
    v <- paste0("$", fmt_num(r$estimacion)); ic <- paste0("$", fmt_num(r$li), "-", fmt_num(r$ls))
  } else {
    v <- fmt_num(r$estimacion, 2); ic <- paste0(fmt_num(r$li, 2), "-", fmt_num(r$ls, 2))
  }
  tipo <- if (grepl("matched", tolower(r$estimando))) " Diferencia emparejada (CEM)."
          else if (grepl("model", tolower(r$estimando))) " Modelada, caso base." else ""
  list(titulo = tit, valor = v, sub = paste0("IC 95% ", ic, ".", tipo))
}
# Color mezclado con blanco (alfa sobre fondo blanco), para fondos muy sutiles.
tinte <- function(col, alfa = 0.10) {
  v <- grDevices::col2rgb(col)[, 1]
  grDevices::rgb(t(round(255 - alfa * (255 - v))), maxColorValue = 255)
}
color_mod <- function(id) { c <- modulos$color[modulos$id == id]; if (length(c) && nzchar(c)) c else "#80796B" }

n_total <- if (hay_datos) sum(cubo0$n, na.rm = TRUE) else NA
# Buscador con autocompletado (sin dependencias): cada palabra escrita debe ser el comienzo de una palabra de la
# sugerencia, sin importar tildes ni mayúsculas. Flechas para moverse, Enter para preguntar, Esc para cerrar.
js_autocompletar <- "
(function () {
  var datos = document.getElementById('preg_sug'), inp = document.getElementById('preg_texto'), caja = document.getElementById('preg_lista');
  if (!datos || !inp || !caja) return;
  var SUG = JSON.parse(datos.textContent);
  var norm = function (s) { return s.toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g, '').replace(/[^a-z0-9+]+/g, ' ').trim(); };
  var PAL = SUG.map(function (s) { return norm(s).split(' '); });
  var esc = function (s) { return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;'); };
  var act = -1, cur = [], toks = [];
  function enviar(txt) { cerrar(); inp.value = txt; Shiny.setInputValue('preg_enviar', txt, {priority: 'event'}); }
  function cerrar() { caja.hidden = true; caja.innerHTML = ''; cur = []; act = -1; inp.setAttribute('aria-expanded', 'false'); }
  function marcar(s) {
    return s.split(' ').map(function (w) {
      var n = norm(w);
      return toks.some(function (t) { return n.indexOf(t) === 0; }) ? '<b>' + esc(w) + '</b>' : esc(w);
    }).join(' ');
  }
  function pintar() {
    caja.innerHTML = '';
    if (!cur.length) { cerrar(); return; }
    cur.forEach(function (i, k) {
      var d = document.createElement('div');
      d.className = 'op' + (k === act ? ' activa' : ''); d.setAttribute('role', 'option');
      d.innerHTML = marcar(SUG[i]);
      d.addEventListener('mousedown', function (e) { e.preventDefault(); enviar(SUG[i]); });
      caja.appendChild(d);
    });
    caja.hidden = false; inp.setAttribute('aria-expanded', 'true');
  }
  function buscar() {
    toks = norm(inp.value).split(' ').filter(function (t) { return t.length > 0; });
    act = -1;
    if (!toks.length || inp.value.trim().length < 2) { cerrar(); return; }
    var res = [];
    for (var i = 0; i < SUG.length; i++) {
      var w = PAL[i], ok = true, pos = 0;
      for (var j = 0; j < toks.length && ok; j++) {
        var k = w.findIndex(function (x) { return x.indexOf(toks[j]) === 0; });
        if (k < 0) ok = false; else pos += k;
      }
      if (ok) res.push([i, pos + w.length / 4]);
    }
    res.sort(function (a, b) { return a[1] - b[1]; });
    cur = res.slice(0, 7).map(function (r) { return r[0]; });
    pintar();
  }
  inp.addEventListener('input', buscar);
  inp.addEventListener('focus', buscar);
  inp.addEventListener('blur', function () { setTimeout(cerrar, 120); });
  inp.addEventListener('keydown', function (e) {
    if (e.key === 'ArrowDown' && cur.length) { e.preventDefault(); act = (act + 1) % cur.length; pintar(); }
    else if (e.key === 'ArrowUp' && cur.length) { e.preventDefault(); act = (act - 1 + cur.length) % cur.length; pintar(); }
    else if (e.key === 'Tab' && act >= 0) { e.preventDefault(); inp.value = SUG[cur[act]]; cerrar(); }
    else if (e.key === 'Escape') { cerrar(); }
    else if (e.key === 'Enter') { e.preventDefault(); enviar(act >= 0 ? SUG[cur[act]] : inp.value); }
  });
})();
"
home_ui <- div(class = "pagina",
  div(class = "portada",
    div(class = "texto",
      div(
        h1("Epidemiología craneomaxilofacial de Chile"),
        p(class = "bajada", "Egresos hospitalarios del DEIS y episodios GRD de la red pública, con las definiciones de caso, denominadores y métodos de los artículos publicados. Cada figura y tabla trae su cita y el código R que la reproduce.")),
      div(class = "cifras",
        div(div(class = "rotulo", "Egresos clasificados"), div(class = "cifra", fmt_num(n_total))),
        div(div(class = "rotulo", "Período"), div(class = "cifra", paste(anios_disp, collapse = "-"))),
        div(div(class = "rotulo", "Módulos disponibles"), div(class = "cifra", nrow(mod_ev_datos))),
        div(div(class = "rotulo", "Versión de datos"), div(class = "cifra", cmf_version())))),
    tags$figure(class = "lamina",
      tags$img(src = "gautier_dagoty_1745.jpg", alt = "Músculos del lado derecho de la cabeza y el cuello, grabado anatómico de 1745"),
      tags$figcaption(
        "Músculos del lado derecho de la cabeza y el cuello. Mezzotinta en color de J. F. Gautier d'Agoty, según su propio dibujo, 1745-1746. Wellcome Collection, 33339i."))
  ),
  if (hay_datos) div(class = "pregunta",
    h2("Pregunta al observatorio"),
    div(class = "caja",
      # Sin <form> y sin atributo type: Shiny no lo enlaza como input propio; la pregunta viaja en preg_enviar.
      tags$input(id = "preg_texto", maxlength = "300", autocomplete = "off", enterkeyhint = "send",
                 placeholder = "Por ejemplo: ¿cuántos egresos por fracturas faciales hubo en Biobío en 2023?",
                 `aria-label` = "Pregunta al observatorio", role = "combobox", `aria-autocomplete` = "list",
                 `aria-controls` = "preg_lista", `aria-expanded` = "false"),
      div(id = "preg_lista", class = "sugerencias", role = "listbox", hidden = NA),
      tags$button(type = "button", class = "btn btn-primary",
                  onclick = "Shiny.setInputValue('preg_enviar', document.getElementById('preg_texto').value, {priority: 'event'});", "Preguntar")),
    div(class = "ejemplos",
      lapply(cmf_preguntas_ejemplo(), function(x) tags$a(href = "#", x,
        onclick = sprintf("document.getElementById('preg_texto').value = %s; Shiny.setInputValue('preg_enviar', %s, {priority: 'event'}); return false;",
                          jsonlite::toJSON(x, auto_unbox = TRUE), jsonlite::toJSON(x, auto_unbox = TRUE))))),
    uiOutput("respuestas"),
    # Autocompletado: preguntas que el intérprete sabe responder, filtradas en el navegador mientras se escribe.
    tags$script(id = "preg_sug", type = "application/json", HTML(jsonlite::toJSON(cmf_preguntas_sugeridas()))),
    tags$script(HTML(js_autocompletar))),
  div(class = "indice",
    div(class = "rotulo mb-2", "Índice"),
    lapply(seq_len(nrow(modulos)), function(i) {
      m <- modulos[i, ]
      activo <- m$id %in% mod_ev_datos$id || (m$tipo == "territorial" && !is.null(oferta) && nrow(oferta) > 0)
      fuentes_txt <- paste(FUENTES_CORTO[unlist(strsplit(m$fuentes, ", "))] %||% m$fuentes, collapse = " y ")
      # Rectángulos por dimensión (abren el explorador directamente en ella); la fila completa abre el explorador
      # del módulo tal como esté. Capacidad y espera tiene una sola vista.
      ir_js <- function(dim) sprintf("Shiny.setInputValue('ir', {modulo: '%s', dim: '%s'}, {priority: 'event'}); return false;", m$id, dim)
      dims <- if (!activo) NULL else if (m$tipo == "territorial") {
        list(tags$a(class = "dim", href = "#", onclick = ir_js("general"), "Capacidad y espera", span(class = "flecha", "→")))
      } else {
        c(list(tags$a(class = "dim", href = "#", onclick = ir_js("epi"), "Epidemiología", span(class = "flecha", "→"))),
          if (isTRUE(eco_ok[m$id])) list(tags$a(class = "dim", href = "#", onclick = ir_js("eco"), "Economía", span(class = "flecha", "→"))))
      }
      div(class = paste("indice-fila", if (activo) "activo" else "inactivo"), style = paste0("--c:", m$color),
        if (activo) tags$a(class = "fila-enlace", href = "#", onclick = ir_js("general"), `aria-label` = paste("Explorar", m$nombre)),
        div(class = "num", i),
        div(h3(m$nombre), div(class = "estado", if (activo) fuentes_txt else paste0("En construcción, ", fuentes_txt))),
        p(class = "ocultable", m$descripcion),
        div(class = "codigos ocultable", if (m$tipo == "eventos") codes_for(m$id) else "29 Servicios de Salud"),
        div(class = "dims", dims),
        span(class = "pestana", style = paste0("background:", if (activo) m$color else "#D6D3CC")))
    })),
  div(class = "pie", paste0("Observatorio CMF Chile. Versión de datos ", cmf_version(), ", construida el ",
                            manifest$construido %||% "", ". Código MIT, datos CC BY 4.0."))
)

explorer_ui <- layout_sidebar(
  sidebar = sidebar(width = 290, open = "desktop",
    selectInput("modulo", "Módulo", choices = stats::setNames(mod_ev_datos$id, mod_ev_datos$nombre)),
    conditionalPanel("input.dimension != 'eco'",
      selectInput("fuente", "Fuente", choices = NULL),
      radioButtons("metrica", "Medida", choices = METRICAS_LBL[1])),
    conditionalPanel("input.dimension == 'eco'",
      radioButtons("indicador", "Indicador (GRD)", choices = stats::setNames(names(ECO_LBL), ECO_LBL))),
    checkboxGroupInput("subgrupo", "Subgrupos", choices = NULL),
    sliderInput("anios", "Años", min = anios_disp[1], max = anios_disp[2], value = anios_disp, step = 1, sep = "", ticks = FALSE),
    selectizeInput("region", "Región de residencia", multiple = TRUE,
                   choices = stats::setNames(regiones$codigo_region, regiones$nombre), options = list(placeholder = "Todas")),
    selectizeInput("servicio", "Servicio de Salud", multiple = TRUE, choices = NULL, options = list(placeholder = "Todos")),
    checkboxGroupInput("sexo", "Sexo", choices = c("Hombre", "Mujer"), selected = c("Hombre", "Mujer"), inline = TRUE),
    selectizeInput("edad", "Grupo etario", choices = NULL, multiple = TRUE, options = list(placeholder = "Todas las edades")),
    selectizeInput("prevision", "Previsión", choices = prevision_disp, multiple = TRUE, options = list(placeholder = "Todas")),
    selectizeInput("sector", "Sector", choices = c("Público", "Privado"), multiple = TRUE, options = list(placeholder = "Ambos")),
    uiOutput("ui_actividad"),
    textInput("its", "Quiebre de la serie (año)", placeholder = "p. ej. 2020")
  ),
  div(class = "pagina", style = "padding:8px 8px 40px; max-width:none",
    uiOutput("encabezado"),
    uiOutput("aviso_tasa"),
    layout_columns(col_widths = c(9, 3),
      div(
        navset_underline(id = "vista",
          nav_panel("Tendencia", value = "trend", plotOutput("p_trend", height = "460px")),
          nav_panel("Territorio", value = "bars",
            div(class = "vista-opciones",
              radioButtons("territorio_tipo", NULL, inline = TRUE, choices = c("Mapa" = "map", "Barras" = "bars")),
              radioButtons("nivel", NULL, inline = TRUE, choices = c("Región" = "region", "Servicio de Salud" = "servicio_salud"))),
            plotOutput("p_bars", height = "660px")),
          nav_panel("Composición", value = "composition",
            div(class = "vista-opciones", radioButtons("comp_var", NULL, inline = TRUE, choices = c("Subgrupo" = "subgrupo"))),
            plotOutput("p_comp", height = "460px")),
          nav_panel("Tabla", value = "table", div(class = "pt-3", DT::DTOutput("t_tabla"))),
          nav_panel("Estudio", value = "estudio",
            div(class = "estudio",
              uiOutput("estudio_cab"),
              uiOutput("estudio_cifras"),
              div(class = "vista-opciones",
                radioButtons("est_fig", NULL, inline = TRUE, choices = c("Escenarios de productividad" = "escenarios")),
                conditionalPanel("input.est_fig == 'escenarios'",
                  radioButtons("est_ingreso", NULL, inline = TRUE, choices = c("Ingreso medio" = "medio", "Ingreso mediano" = "mediano"))),
                conditionalPanel("input.est_fig == 'territorio'",
                  selectInput("est_terr", NULL, choices = NULL, width = "340px"),
                  radioButtons("est_terr_tipo", NULL, inline = TRUE, choices = c("Mapa" = "map", "Barras" = "bars")))),
              plotOutput("p_estudio", height = "600px"),
              uiOutput("estudio_nota"))),
          nav_panel("Códigos", value = "codes", div(class = "pt-3", DT::DTOutput("t_dx"), uiOutput("proc_ui")))
        )),
      div(class = "margen",
        div(class = "rotulo", "Cifras del período"),
        uiOutput("kpis"),
        div(class = "descargas",
          div(class = "rotulo mt-3", "Exportar"),
          selectInput("fmt", NULL, c("TIFF 600 dpi" = "tiff", "PNG" = "png", "PDF" = "pdf", "SVG" = "svg")),
          selectInput("ancho", NULL, c("1 columna (85 mm)" = "1 col", "2 columnas (174 mm)" = "2 col")),
          selectInput("estilo", NULL, c("Paleta JAMA" = "jama", "Escala de grises" = "grises")),
          downloadButton("dl_fig", "Descargar figura", class = "btn-primary w-100"),
          downloadButton("dl_tab", "Descargar tabla (CSV)", class = "btn-outline-secondary w-100")))
    ),
    layout_columns(col_widths = c(6, 6), class = "mt-5 pt-3 regla",
      div(div(class = "rotulo mb-2", "Cita"), div(class = "bloque-cita", textOutput("cita")),
          div(class = "rotulo mt-4 mb-2", "Párrafo de métodos"), p(class = "small", textOutput("metodos", inline = TRUE))),
      div(div(class = "rotulo mb-2", "Código R"), tags$pre(class = "codigo", textOutput("codigo"))))
  )
)


OFERTA_LBL <- c(
  residentes_2026 = "Residentes (INE 2026)", especialistas_cmf = "Especialistas CMF",
  fte_cmf = "FTE CMF (44 h)", especialistas_por_100000 = "Especialistas por 100.000 habitantes",
  fte_por_100000 = "FTE por 100.000 habitantes", consultas_nuevas_pendientes = "Consultas nuevas en espera",
  consultas_nuevas_por_100000 = "Consultas nuevas en espera por 100.000", consultas_nuevas_mediana_dias = "Consultas nuevas: mediana de espera, días",
  cirugias_pendientes = "Cirugías en espera", cirugias_por_100000 = "Cirugías en espera por 100.000",
  cirugias_mediana_dias = "Cirugías: mediana de espera, días")
oferta_lbl <- function(x) ifelse(x %in% names(OFERTA_LBL), OFERTA_LBL[x], x)

# Biblioteca ----
cita_articulo <- function(a) {
  e <- htmltools::htmlEscape
  vol <- if (nzchar(a$referencia)) paste0(";", e(a$referencia)) else ""
  rev <- if (nzchar(a$revista)) paste0("<em>", e(a$revista), "</em>. ") else ""
  doi <- if (nzchar(a$doi)) sprintf(' <a href="https://doi.org/%s" target="_blank">doi:%s</a>', e(a$doi), e(a$doi)) else ""
  pm <- if (nzchar(a$pmid)) sprintf(' <a href="https://pubmed.ncbi.nlm.nih.gov/%s/" target="_blank">PMID %s</a>', e(a$pmid), e(a$pmid)) else ""
  HTML(paste0(e(a$autores), ". ", e(a$titulo), ". ", rev, e(a$anio), vol, ".", doi, pm))
}
biblio_regiones <- sort(unique(biblio$region[nzchar(biblio$region)]))
biblio_mods <- intersect(modulos$id, unique(biblio$modulo))
biblioteca_ui <- div(class = "pagina",
  h2("Biblioteca"),
  p("Publicaciones sobre epidemiología craneomaxilofacial en Chile: los artículos que el observatorio reproduce, series hospitalarias y estudios nacionales o regionales."),
  div(class = "vista-opciones filtros-biblio",
    selectInput("bib_ambito", NULL, width = "260px",
                choices = c("Todos los ámbitos" = "", stats::setNames(biblio_mods, modulos$nombre[match(biblio_mods, modulos$id)]))),
    selectInput("bib_tipo", NULL, width = "260px",
                choices = c("Todos los tipos" = "", stats::setNames(names(BIBLIO_TIPOS), BIBLIO_TIPOS))),
    if (length(biblio_regiones)) selectInput("bib_region", NULL, width = "220px", choices = c("Todas las regiones" = "", biblio_regiones)),
    textInput("bib_buscar", NULL, width = "260px", placeholder = "Autor, título u hospital")),
  div(class = "d-flex justify-content-between align-items-end mt-3 mb-1",
    div(class = "rotulo", textOutput("bib_n", inline = TRUE)),
    div(class = "d-flex gap-2",
      downloadButton("dl_bib_ris", "RIS (Zotero, EndNote)", class = "btn-outline-secondary"),
      downloadButton("dl_bib_csv", "CSV", class = "btn-outline-secondary"))),
  uiOutput("bib_lista"))

oferta_ui <- if (!is.null(oferta) && nrow(oferta)) {
  ind <- intersect(names(OFERTA_LBL), names(oferta))   # indicadores conocidos, en el orden de la tabla
  if (!length(ind)) ind <- grep("(^servicio_salud|_id$|^codigo)", names(oferta)[vapply(oferta, is.numeric, TRUE)],
                                value = TRUE, invert = TRUE)
  ind <- stats::setNames(ind, oferta_lbl(ind))
  layout_sidebar(
    sidebar = sidebar(width = 300,
      selectInput("of_ind", "Indicador", choices = ind),
      radioButtons("of_tipo", "Vista", choices = c("Mapa" = "map", "Barras" = "bars"), inline = TRUE),
      p(class = "small", paste0("29 Servicios de Salud. Dotación del último mes validado; listas de espera al ",
        unique(oferta$version_fuente)[1], ". Fuente: RNPI (Transparencia) y Visor Ciudadano LE.")),
      downloadButton("dl_oferta", "Tabla CSV", class = "btn-outline-secondary")),
    div(class = "encabezado-modulo", div(class = "rotulo", "Capacidad instalada y lista de espera"), h2(textOutput("of_titulo", inline = TRUE))),
    plotOutput("p_oferta", height = "680px"),
    div(class = "mt-4", DT::DTOutput("t_oferta")))
} else div(class = "pagina", h2("Aún no hay datos de capacidad instalada y lista de espera cargados"))

perdida_ui <- function() {
  if (is.null(validacion) || !length(validacion$fuentes)) return(NULL)
  q <- validacion$fuentes
  num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  d <- data.table::rbindlist(lapply(q, function(x) list(fuente = x$fuente %||% NA_character_, n = num(x$n_leidos),
                                                       reg = num(x$pct_sin_region), edad = num(x$pct_sin_edad),
                                                       sexo = num(x$pct_sin_sexo))), fill = TRUE)
  d <- d[!is.na(fuente), .(anios = .N, rango_reg = paste0(fmt_num(min(reg, na.rm = TRUE), 1), "-", fmt_num(max(reg, na.rm = TRUE), 1)),
                           rango_edad = paste0(fmt_num(min(edad, na.rm = TRUE), 1), "-", fmt_num(max(edad, na.rm = TRUE), 1)),
                           rango_sexo = paste0(fmt_num(min(sexo, na.rm = TRUE), 1), "-", fmt_num(max(sexo, na.rm = TRUE), 1))), by = fuente]
  tags$table(class = "tabla-simple",
    tags$thead(tags$tr(tags$th("Fuente"), tags$th("Años"), tags$th("Sin región, %"), tags$th("Sin edad, %"), tags$th("Sin sexo, %"))),
    tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(
      tags$td(FUENTES_CORTO[d$fuente[i]] %||% d$fuente[i]), tags$td(d$anios[i]), tags$td(d$rango_reg[i]),
      tags$td(d$rango_edad[i]), tags$td(d$rango_sexo[i])))))
}

ui <- page_navbar(
  id = "nav",
  title = tags$span(tags$span(class = "sello"), "Observatorio CMF Chile"),
  theme = tema, fillable = FALSE,
  header = tags$head(tags$style(HTML(css)), tags$script(HTML(
    "Shiny.addCustomMessageHandler('color_mod', function(x) {
       var r = document.documentElement.style; r.setProperty('--mod', x.color); r.setProperty('--mod-tinte', x.tinte); });
     Shiny.addCustomMessageHandler('set_dimension', function(x) { Shiny.setInputValue('dimension', x); });
     Shiny.addCustomMessageHandler('preg_limpiar', function(x) { var e = document.getElementById('preg_texto'); if (e) e.value = ''; });"))),
  nav_panel("Inicio", value = "inicio", home_ui),
  nav_panel("Explorar", value = "explorar", if (hay_datos) explorer_ui else
    div(class = "pagina", h2("Aún no hay datos procesados"),
        p("Carga versiones de datos desde el panel de administración y ejecuta la construcción."))),
  nav_panel("Capacidad y espera", value = "oferta", oferta_ui),
  nav_panel("Métodos", value = "metodos",
    div(class = "pagina",
      h2("Métodos"),
      tags$ol(
        tags$li("Unidad de análisis: el egreso hospitalario (reingresos y traslados cuentan por separado)."),
        tags$li("DEIS cubre todos los prestadores; GRD, los hospitales públicos con GRD. Sus cifras no son intercambiables."),
        tags$li("Región y Servicio de Salud de residencia, reconstruidos desde la comuna (códigos previos a Arica y Parinacota, Los Ríos y Ñuble); en GRD 2025, desde la provincia."),
        tags$li("Grupos etarios de cada artículo, asignados por el límite inferior del tramo registrado."),
        tags$li("GRD: tasas ajustadas por edad por método directo (población OMS 2000-2025) con IC gamma de Fay-Feuer. DEIS: tasas crudas, por egresos o ajustadas a la población chilena 2002-2025. IC exactos de Poisson para tasas crudas."),
        tags$li("Trauma GRD: RISS derivado de CIE-10 (ICDPIC-R 1.0.1, método NIS); trauma mayor, RISS ≥ 16."),
        tags$li("Fisuras, por 1.000 nacidos vivos; craneosinostosis, por 10.000 nacidos vivos."),
        tags$li(paste0("Economía: valor GRD = peso GRD × precio base del clúster del hospital, con la estructura de precios ",
                       estructura_precios(), " constante",
                       if (!is.null(referencia_precios())) paste0(", expresado en ", moneda_lbl(TRUE), " con el IPC del Banco Central") else "",
                       " (perspectiva del financiador público; no es costo observado). Días cama = suma de la estada. Solo GRD.")),
        tags$li("Estudios económicos: resultados agregados de los notebooks de cada artículo (estimandos, escenarios y carga por Servicio de Salud), importados sin registros individuales y con su propia moneda y métodos."),
        tags$li("Se suprimen las celdas con menos casos que el umbral configurado.")),
      h2("Datos faltantes"),
      p("Los egresos sin región, edad o sexo se mantienen en totales y tendencias y se excluyen solo de la desagregación correspondiente. Rango anual por fuente:"),
      perdida_ui(),
      h2("Reproducción de las publicaciones"),
      p("Cada versión recalcula cifras publicadas, en los años de cada artículo, con el mismo código de la app."),
      DT::DTOutput("t_controles"),
      h2("Fuentes"),
      tableOutput("t_fuentes"),
      h2("Diccionario de códigos"),
      DT::DTOutput("t_dic_all"))),
  nav_panel("Cómo citar", value = "citar",
    div(class = "pagina",
      h2("Cómo citar"),
      div(class = "bloque-cita", cmf_cite()),
      if (!is.null(pub$repositorio)) p(tags$a(href = pub$repositorio, "Repositorio y paquete R")))),
  nav_panel("Biblioteca", value = "biblioteca", biblioteca_ui),
  nav_spacer(),
  nav_item(tags$span(class = "rotulo", paste0("v", cmf_version())))
)

server <- function(input, output, session) {
  # Portada: la fila abre el explorador con ese módulo; cada rectángulo, directamente en su dimensión.
  observeEvent(input$ir, {
    x <- input$ir
    if (identical(x$modulo, "oferta")) return(nav_select("nav", "oferta"))
    req(x$modulo %in% mod_ev_datos$id)
    if (x$dim %in% c("epi", "eco")) session$sendCustomMessage("set_dimension", if (x$dim == "eco" && isTRUE(eco_ok[x$modulo])) "eco" else "epi")
    updateSelectInput(session, "modulo", selected = x$modulo)
    nav_select("nav", "explorar")
  })
  # Pregunta al observatorio: cmf_preguntar() interpreta la pregunta y consulta el cubo publicado.
  # Solo se muestra la última respuesta: cada pregunta nueva reemplaza a la anterior.
  historial <- reactiveVal(list())
  n_preguntas <- 0L
  observeEvent(input$preg_enviar, {
    txt <- trimws(substr(input$preg_enviar %||% "", 1, 300))
    if (!nzchar(txt)) return()
    n_preguntas <<- n_preguntas + 1L
    r <- if (n_preguntas > 60) {
      list(ok = FALSE, parrafos = "Se alcanzó el máximo de preguntas de esta sesión. Recarga la página para seguir.")
    } else tryCatch(cmf_preguntar(txt), error = function(e) list(ok = FALSE, parrafos = c(
      "No pude responder esa pregunta con los datos publicados.", "Prueba con otra forma de plantearla o usa el explorador.")))
    id <- paste0("preg_", n_preguntas)
    if (!is.null(r$grafico)) local({
      g <- r$grafico
      output[[paste0(id, "_g")]] <- renderPlot(g, res = 110)
    })
    historial(list(list(id = id, pregunta = txt, r = r)))
    session$sendCustomMessage("preg_limpiar", list())
  })
  output$respuestas <- renderUI({
    h <- historial()
    if (!length(h)) return(NULL)
    lapply(h, function(x) {
      r <- x$r
      tabla <- if (!is.null(r$tabla) && nrow(r$tabla)) {
        t <- r$tabla; extra <- nrow(t) - 12
        tagList(
          tags$table(class = "tabla-simple",
            tags$thead(tags$tr(lapply(names(t), tags$th))),
            tags$tbody(lapply(seq_len(min(12, nrow(t))), function(i) tags$tr(lapply(t[i, ], function(v) tags$td(as.character(v))))))),
          if (extra > 0) div(class = "mas", paste0("Y ", extra, if (extra == 1) " fila más" else " filas más", " en el explorador.")))
      }
      ir <- if (!is.null(r$modulo) && (r$modulo %in% mod_ev_datos$id || identical(r$modulo, "oferta"))) {
        tags$a(class = "ir", href = "#", onclick = sprintf("Shiny.setInputValue('ir', {modulo: '%s', dim: 'general'}, {priority: 'event'}); return false;", r$modulo),
               "Abrir en el explorador →")
      }
      div(class = "respuesta",
        div(class = if (isTRUE(r$ok)) "ok" else "no-ok",
          div(class = "q", x$pregunta),
          if (length(r$entendido)) div(class = "entendido", lapply(names(r$entendido), function(k) tags$span(paste0(k, ": "), tags$b(r$entendido[[k]])))),
          lapply(r$parrafos, tags$p),
          if (length(r$sugerencias)) div(class = "ejemplos", lapply(r$sugerencias, function(s) tags$a(href = "#", s,
            onclick = sprintf("document.getElementById('preg_texto').value = %s; Shiny.setInputValue('preg_enviar', %s, {priority: 'event'}); return false;",
                              jsonlite::toJSON(s, auto_unbox = TRUE), jsonlite::toJSON(s, auto_unbox = TRUE))))),
          if (!is.null(r$cita)) div(class = "bloque-cita", r$cita),
          ir),
        div(if (!is.null(r$grafico)) plotOutput(paste0(x$id, "_g"), height = "300px"), tabla))
    })
  })

  # Dimensión del explorador: Epidemiología (por defecto) o Economía (solo módulos con GRD).
  es_eco <- reactive(identical(input$dimension, "eco") && isTRUE(eco_ok[input$modulo %||% ""]))
  observe({
    if (identical(input$dimension, "eco") && !isTRUE(eco_ok[input$modulo %||% ""])) session$sendCustomMessage("set_dimension", "epi")
  })

  output$t_controles <- DT::renderDT({
    f <- file.path(cmf_data_dir(), "controles.json")
    if (!file.exists(f)) return(NULL)
    x <- data.table::as.data.table(jsonlite::read_json(f, simplifyVector = TRUE))
    x <- x[, .(Referencia = referencia, Fuente = FUENTES_CORTO[fuente],
               Años = ifelse(anio_desde == anio_hasta, anio_desde, paste0(anio_desde, "-", anio_hasta)),
               Publicado = fmt_num(valor, ifelse(valor %% 1 == 0, 0, 2)),
               Observatorio = fmt_num(obtenido, ifelse(valor %% 1 == 0, 0, 2)),
               Estado = ifelse(estado == "coincide", "Coincide", ifelse(estado == "difiere", "Difiere", "Sin datos")))]
    dt_pub(x, rownames = FALSE, class = "compact", options = list(pageLength = 50, dom = "t", ordering = FALSE))
  })
  modulo_cfg <- reactive({ req(input$modulo); cmf_module(input$modulo) })

  # Color del módulo en el sello del título, la pestaña activa del menú y el fondo del panel lateral (tinte al 10%).
  # Fuera del explorador se usa el color de firma; en Capacidad y espera, el color de ese módulo.
  observe({
    pag <- input$nav %||% "inicio"
    col <- switch(pag, explorar = color_mod(input$modulo %||% ""), oferta = color_mod("oferta"), FIRMA)
    session$sendCustomMessage("color_mod", list(color = col, tinte = tinte(col, 0.10)))
  })
  output$encabezado <- renderUI({
    m <- modulo_cfg()
    eco <- es_eco()
    sw <- function(dim, ok = TRUE) tags$a(
      class = paste(c(if ((dim == "eco") == eco) "activa", if (!ok) "deshabilitada"), collapse = " "), href = "#",
      onclick = sprintf("Shiny.setInputValue('dimension', '%s'); return false;", dim), DIM_LBL[[dim]])
    div(class = "encabezado-modulo",
      div(div(class = "rotulo", if (eco) paste0("GRD, hospitales públicos. Valor GRD en ", moneda_lbl())
                               else FUENTES_LBL[input$fuente] %||% ""),
          h2(m$nombre)),
      div(class = "dimensiones", sw("epi"), sw("eco", isTRUE(eco_ok[m$id]))))
  })

  # Controles que dependen del módulo y de la fuente (cada fuente reproduce su artículo).
  # En Economía la fuente es siempre GRD (el DEIS no trae valor); al volver a Epidemiología se conserva.
  observeEvent(list(input$modulo, es_eco()), {
    m <- modulo_cfg()
    fu <- intersect(unlist(m$fuentes), unique(cubo0[modulo == input$modulo]$fuente))
    actual <- isolate(input$fuente)
    sel <- if (es_eco() && "grd" %in% fu) "grd" else if (length(actual) && actual %in% fu) actual else fu[1]
    updateSelectInput(session, "fuente", choices = stats::setNames(fu, FUENTES_LBL[fu]), selected = sel)
  })
  observeEvent(list(input$modulo, input$fuente), {
    req(input$fuente)
    m <- modulo_cfg()
    sg <- cmf_subgroups(input$modulo, input$fuente)
    updateCheckboxGroupInput(session, "subgrupo", choices = sg$subgrupo, selected = sg$subgrupo[sg$principal])
    met <- unlist(mod_get(m, "metricas", input$fuente)) %||% c("cruda", "conteo")
    lbl <- METRICAS_LBL[met]
    if ("nacidos_vivos" %in% met && !is.null(m$por$nacidos_vivos)) {
      lbl[met == "nacidos_vivos"] <- paste0("Por ", fmt_num(m$por$nacidos_vivos), " nacidos vivos")
    }
    if ("universo" %in% met && identical(m$id, "trauma")) lbl[met == "universo"] <- "Por 1.000 egresos por trauma"
    updateRadioButtons(session, "metrica", choices = stats::setNames(met, lbl), selected = met[1])
    ge <- cmf_age_groups(input$modulo, input$fuente)$etiqueta
    updateSelectizeInput(session, "edad", choices = ge, selected = character(0))
    ag <- mod_get(m, "agrupaciones", input$fuente)
    comp <- c(if (length(ag)) stats::setNames("agrupacion", if (identical(m$id, "trauma")) "Tejido" else "Agrupación"),
              "Subgrupo" = "subgrupo",
              if (isTRUE(m$desagregar_mecanismo)) c("Mecanismo" = "mecanismo"),
              if (length(unlist(mod_get(m, "actividades", input$fuente))) > 1) c("Actividad" = "actividad"),
              if (length(ge) > 1) c("Grupo etario" = "grupo_edad"), "Sexo" = "sexo")
    updateRadioButtons(session, "comp_var", choices = comp, selected = comp[1])
  })
  # Años disponibles del módulo en la fuente elegida (GRD parte en 2019): al cambiar, el período vuelve a ese rango.
  observeEvent(list(input$modulo, input$fuente), {
    req(input$modulo, input$fuente)
    r <- range(cubo0[modulo == input$modulo & fuente == input$fuente]$anio, na.rm = TRUE)
    if (!all(is.finite(r))) return()
    updateSliderInput(session, "anios", min = r[1], max = r[2], value = r)
  })
  observe({
    s <- if (length(input$region)) servicios[region %in% as.integer(input$region)] else servicios
    updateSelectizeInput(session, "servicio", choices = stats::setNames(s$codigo_servicio_salud, s$nombre),
                         selected = intersect(isolate(input$servicio), s$codigo_servicio_salud))
  })

  # Actividad (hospitalización / CMA): solo en módulos que suman CMA en esa fuente.
  actividades_mod <- reactive({
    req(input$fuente)
    unlist(mod_get(modulo_cfg(), "actividades", input$fuente)) %||% "Hospitalización"
  })
  output$ui_actividad <- renderUI({
    act <- actividades_mod()
    if (length(act) < 2) return(NULL)
    checkboxGroupInput("actividad", "Actividad", choices = stats::setNames(act, ifelse(act == "CMA", "Cirugía mayor ambulatoria", act)),
                       selected = act, inline = FALSE)
  })

  filtros <- reactive({
    req(input$modulo, input$fuente, input$anios, length(input$subgrupo) > 0)
    list(modulo = input$modulo, fuente = input$fuente, years = seq(input$anios[1], input$anios[2]),
         subgrupo = input$subgrupo, region = as.integer(input$region), servicio_salud = as.integer(input$servicio),
         sexo = if (length(input$sexo) == 1) input$sexo else NULL,
         grupo_edad = input$edad, prevision = input$prevision, sector = input$sector,
         actividad = if (length(actividades_mod()) > 1 && length(input$actividad)) input$actividad else NULL)
  })
  # Consulta según la dimensión: cmf_query (epidemiología) o cmf_query_eco (valor GRD y días cama).
  consulta <- function(by, metric = input$metrica, indicador = input$indicador) {
    if (es_eco()) {
      req(indicador, input$fuente == "grd")
      f <- filtros(); f$fuente <- NULL
      return(tryCatch(do.call(cmf_query_eco, c(f, list(indicador = indicador, by = by))),
                      error = function(e) validate(need(FALSE, conditionMessage(e)))))
    }
    req(metric)
    tryCatch(do.call(cmf_query, c(filtros(), list(by = by, metric = metric))),
             error = function(e) validate(need(FALSE, conditionMessage(e))))
  }
  ind_suma <- reactive(if (input$indicador %in% c("dias_cama", "estada_media")) "dias_cama" else "valor_total")

  q_trend <- reactive(consulta("anio"))
  q_bars <- reactive(consulta(input$nivel %||% "region"))
  q_comp <- reactive({ req(input$comp_var); consulta(c("anio", input$comp_var), metric = "conteo", indicador = ind_suma()) })
  q_total <- reactive(consulta(character(0)))
  its <- reactive({ y <- suppressWarnings(as.integer(input$its)); if (length(y) && !is.na(y)) y else NULL })
  es_mapa <- reactive(identical(input$territorio_tipo, "map"))

  col_mod <- reactive(color_mod(input$modulo))
  plot_territorio <- function(q) if (es_mapa()) cmf_plot_map(q, estilo = input$estilo) else
    cmf_plot_bars(q, estilo = input$estilo, color = col_mod())
  plot_actual <- reactive({
    switch(input$vista,
           estudio = plot_estudio(),
           bars = plot_territorio(q_bars()),
           composition = cmf_plot_composition(q_comp(), estilo = input$estilo),
           cmf_plot_trend(q_trend(), its_break = its(), estilo = input$estilo, color = col_mod()))
  })
  q_actual <- reactive(switch(input$vista, bars = q_bars(), composition = q_comp(), q_trend()))
  en_estudio <- reactive(identical(input$vista, "estudio") && es_eco() && !is.null(estudio_mod()))

  output$aviso_tasa <- renderUI({
    if (es_eco()) return(NULL)
    m <- attr(q_trend(), "cmf_query")$motivo_sin_tasa
    if (!is.null(m)) div(class = "aviso", paste0("Sin tasa para esta selección: ", m, ". Se muestran conteos."))
  })
  pantalla <- function(p, size = 14) {
    if (inherits(p, "patchwork")) return(p & ggplot2::theme(text = ggplot2::element_text(size = size - 2)))
    if (inherits(p$coordinates, "CoordQuickmap")) return(p + ggplot2::theme(text = ggplot2::element_text(size = size - 2)))
    p + cmf_theme(base_size = size, estilo = input$estilo)
  }
  output$p_trend <- renderPlot(pantalla(cmf_plot_trend(q_trend(), its_break = its(), estilo = input$estilo, color = col_mod())), res = 96)
  output$p_bars <- renderPlot(pantalla(plot_territorio(q_bars()), 13), res = 96)
  output$p_comp <- renderPlot(pantalla(cmf_plot_composition(q_comp(), estilo = input$estilo)), res = 96)

  output$kpis <- renderUI({
    if (es_eco()) return(kpis_eco())
    q <- q_total()
    con_tasa <- input$metrica != "conteo" && !is.na(q$tasa)
    grd <- input$fuente == "grd"
    k <- function(t, v, s) div(class = "kpi", div(class = "rotulo", t), div(class = "v", v), div(class = "s", s))
    boxes <- list(
      k("Egresos", fmt_num(q$n), paste(input$anios, collapse = "-")),
      if (con_tasa) k("Tasa del período", fmt_num(q$tasa, 1),
                      paste0("IC 95% ", fmt_num(q$tasa_li, 1), "-", fmt_num(q$tasa_ls, 1)))
      else k("Promedio anual", fmt_num(q$n / (diff(input$anios) + 1), 0), "egresos por año"),
      k("Estada media", paste(fmt_num(q$estada_media, 1), "d"), paste0(fmt_num(q$pct_estada_5, 1), "% ≥ 5 días")),
      if (!is.na(q$letalidad_pct)) k("Letalidad", paste0(fmt_num(q$letalidad_pct, 2), "%"), "intrahospitalaria"),
      if (grd && "pct_trauma_mayor" %in% names(q) && !is.na(q$pct_trauma_mayor))
        k("Trauma mayor", paste0(fmt_num(q$pct_trauma_mayor, 1), "%"), paste0("RISS ≥ 16; media ", fmt_num(q$riss_medio, 1))),
      if (!is.na(q$pct_pabellon)) k(if (grd) "Pabellón" else "Cirugía", paste0(fmt_num(q$pct_pabellon, 1), "%"), "de los egresos")
    )
    tagList(Filter(Negate(is.null), boxes))
  })
  kpis_eco <- function() {
    q <- q_total()
    k <- function(t, v, s) div(class = "kpi", div(class = "rotulo", t), div(class = "v", v), div(class = "s", s))
    f <- filtros(); f$fuente <- NULL
    eco_q <- function(ind, by = character(0)) tryCatch(do.call(cmf_query_eco, c(f, list(indicador = ind, by = by))), error = function(e) NULL)
    per <- eco_q("valor_100k")
    act <- if (length(actividades_mod()) > 1) eco_q("valor_total", "actividad")
    pct_cma <- if (!is.null(act) && "CMA" %in% act$actividad) 100 * sum(act[actividad == "CMA"]$valor_grd, na.rm = TRUE) / sum(act$valor_grd, na.rm = TRUE)
    boxes <- list(
      k("Valor GRD total", paste0("$", fmt_num(q$valor_grd / 1e6), " M"),
        paste0("millones de ", moneda_lbl(TRUE), ", ", paste(input$anios, collapse = "-"))),
      k("Valor medio por egreso", paste0("$", fmt_num(q$valor_grd_medio / 1e3), " mil"), paste0(fmt_num(q$n_con_valor), " egresos con valor")),
      if (!is.null(per) && !is.na(per$valor)) k("Por 100.000 habitantes", paste0("$", fmt_num(per$valor, 1), " M"), paste0("millones de ", moneda_lbl(TRUE), " por 100.000 personas-año")),
      k("Días cama", fmt_num(q$dias_estada), paste0("estada media ", fmt_num(q$estada_media, 1), " días")),
      if (length(pct_cma) && !is.na(pct_cma)) k("Valor en CMA", paste0(fmt_num(pct_cma, 1), "%"), "cirugía mayor ambulatoria")
    )
    tagList(Filter(Negate(is.null), boxes))
  }

  # Tabla de publicación: encabezados en castellano, tasa con IC en una columna.
  tabla_pub <- function(q) {
    meta <- attr(q, "cmf_query")
    d <- data.table::copy(q)
    if ("anio" %in% names(d)) d <- d[order(anio)]
    if (!is.null(meta$indicador)) {
      return(data.table::data.table(Año = d$anio, `Egresos, n` = fmt_num(d$n),
                                    `Valor GRD, millones de CLP` = fmt_num(d$valor_grd / 1e6, 1),
                                    `Valor medio por egreso, miles de CLP` = fmt_num(d$valor_grd_medio / 1e3, 0),
                                    `Días cama` = fmt_num(d$dias_estada), `Estada media, d` = fmt_num(d$estada_media, 1)))
    }
    out <- data.table::data.table(Año = d$anio, `Egresos, n` = fmt_num(d$n))
    if (meta$metric != "conteo" && "tasa" %in% names(d)) {
      out[, (METRICAS_LBL[[meta$metric]] %||% "Tasa") := paste0(fmt_num(d$tasa, 1), " (", fmt_num(d$tasa_li, 1), "-", fmt_num(d$tasa_ls, 1), ")")]
    }
    if ("estada_media" %in% names(d)) out[, `Estada media, d` := fmt_num(d$estada_media, 1)]
    if ("pct_estada_5" %in% names(d)) out[, `Estada ≥ 5 d, %` := fmt_num(d$pct_estada_5, 1)]
    if ("letalidad_pct" %in% names(d) && input$fuente != "grd") out[, `Letalidad, %` := fmt_num(d$letalidad_pct, 2)]
    if ("pct_pabellon" %in% names(d) && any(!is.na(d$pct_pabellon))) out[, `Pabellón, %` := fmt_num(d$pct_pabellon, 1)]
    if ("pct_trauma_mayor" %in% names(d) && any(!is.na(d$pct_trauma_mayor))) out[, `RISS ≥ 16, %` := fmt_num(d$pct_trauma_mayor, 1)]
    out
  }
  output$t_tabla <- DT::renderDT({
    dt_pub(tabla_pub(q_trend()), rownames = FALSE, class = "compact",
                  options = list(dom = "t", pageLength = 100, ordering = FALSE,
                                 columnDefs = list(list(className = "dt-right", targets = "_all"))))
  })
  dic_pub <- function(d, tipo = "dx") {
    if (is.null(d) || !nrow(d)) return(NULL)
    d <- d[as.logical(incluir) %in% TRUE]
    m <- modulo_cfg()
    pr <- unlist(mod_get(m, "subgrupos_principales", input$fuente %||% m$fuentes[[1]]))
    out <- data.table::data.table(Código = d$codigo, Descripción = d$descripcion)
    if (tipo == "dx") {
      out[, Subgrupo := d$subgrupo]
      out[, `Definición principal` := ifelse(d$subgrupo %in% pr, "Sí", "Sensibilidad")]
      if (any(as.logical(d$requiere_proc) %in% TRUE)) out[, `Con cirugía` := ifelse(as.logical(d$requiere_proc) %in% TRUE, "Sí", "")]
    }
    dt_pub(out, rownames = FALSE, class = "compact", options = list(dom = "t", pageLength = 200, ordering = FALSE))
  }
  output$t_dx <- DT::renderDT(dic_pub(cmf_dictionary(input$modulo, "diagnosticos")))
  output$proc_ui <- renderUI({
    d <- cmf_dictionary(input$modulo, "procedimientos")
    if (is.null(d) || !nrow(d)) return(NULL)
    tagList(div(class = "rotulo mt-4 mb-2", "Procedimientos CIE-9-MC"), DT::DTOutput("t_proc"))
  })
  output$t_proc <- DT::renderDT(dic_pub(cmf_dictionary(input$modulo, "procedimientos"), "proc"))
  output$t_dic_all <- DT::renderDT({
    d <- cmf_dictionary(NULL, "diagnosticos")
    d <- d[as.logical(incluir) %in% TRUE]
    nm <- stats::setNames(modulos$nombre, modulos$id)
    dt_pub(data.table::data.table(Módulo = nm[d$modulo], Subgrupo = d$subgrupo, Código = d$codigo, Descripción = d$descripcion),
                  rownames = FALSE, class = "compact", filter = "top", options = list(pageLength = 25, dom = "tip"))
  })
  output$t_fuentes <- renderTable({
    f <- manifest$fuentes %||% list()
    data.frame(Fuente = vapply(f, function(x) x$nombre %||% "", ""),
               Versiones = vapply(f, function(x) paste(vapply(x$versiones %||% list(), function(v) as.character(v$version), ""), collapse = ", "), ""))
  }, class = "tabla-simple", striped = FALSE, bordered = FALSE)

  output$cita <- renderText(if (en_estudio()) cmf_cite_estudio(estudio_mod()$id) else cmf_cite(q_actual()))
  output$metodos <- renderText(if (en_estudio()) estudio_mod()$metodos %||% "" else cmf_methods(q_actual()))
  output$codigo <- renderText({
    if (en_estudio()) return(codigo_estudio())
    tipo <- switch(input$vista, bars = if (es_mapa()) "map" else "bars", composition = "composition", "trend")
    cmf_code(q_actual(), plot = tipo, its_break = if (tipo == "trend") its() else NULL,
             file = paste0("figura.", input$fmt), width = input$ancho)
  })

  output$dl_fig <- downloadHandler(
    filename = function() sprintf("cmf_%s_%s_v%s.%s", input$modulo, input$vista, cmf_version(), input$fmt),
    content = function(file) cmf_save(plot_actual(), file, width = input$ancho, dpi = if (input$fmt == "tiff") 600 else 300)
  )
  output$dl_tab <- downloadHandler(
    filename = function() sprintf("cmf_%s%s_v%s.csv", input$modulo, if (es_eco()) "_economia" else "", cmf_version()),
    content = function(file) {
      if (en_estudio()) {
        e <- estudio_mod()
        tb <- switch(input$est_fig, anual = "productividad_anual", territorio = "territorio", "escenarios")
        writeLines(paste0("# ", gsub("\n", " ", cmf_cite_estudio(e$id))), file)
        return(data.table::fwrite(cmf_estudio_tabla(e$id, tb), file, append = TRUE, col.names = TRUE))
      }
      q <- q_actual()
      writeLines(paste0("# ", cmf_cite(q)), file)
      data.table::fwrite(q, file, append = TRUE, col.names = TRUE)
    }
  )

  # Estudio económico del módulo (pestaña Estudio, solo en Economía) ---------------------------------------
  estudio_mod <- reactive({ e <- cmf_estudios(input$modulo %||% ""); if (length(e)) e[[1]] else NULL })
  observe({
    if (es_eco() && !is.null(estudio_mod())) nav_show("vista", "estudio")
    else { nav_hide("vista", "estudio"); if (identical(isolate(input$vista), "estudio")) nav_select("vista", "trend") }
  })
  observeEvent(estudio_mod(), {
    e <- estudio_mod()
    figs <- c(if (!is.null(cmf_estudio_tabla(e$id, "escenarios"))) c("Escenarios de productividad" = "escenarios"),
              if (!is.null(cmf_estudio_tabla(e$id, "productividad_anual"))) c("Serie anual" = "anual"),
              if (!is.null(cmf_estudio_tabla(e$id, "territorio"))) c("Servicio de Salud" = "territorio"))
    updateRadioButtons(session, "est_fig", choices = figs, selected = figs[1], inline = TRUE)
    tt <- cmf_estudio_tabla(e$id, "territorio")
    ind <- names(ESTUDIO_TERRITORIO)[names(ESTUDIO_TERRITORIO) %in% names(tt)]
    ind <- ind[vapply(ind, function(k) any(!is.na(tt[[k]])), TRUE)]
    updateSelectInput(session, "est_terr", choices = stats::setNames(ind, ESTUDIO_TERRITORIO[ind]))
  })
  output$estudio_cab <- renderUI({
    e <- estudio_mod(); req(e)
    div(class = "estudio-cab",
      div(class = "rotulo", "Estudio de carga económica"),
      h3(e$titulo),
      div(class = "meta", paste0(paste(c(e$periodo, paste0("Montos en ", e$moneda),
                                           if (!is.null(e$notebook)) paste("Notebook", e$notebook)), collapse = ". "), ".")))
  })
  output$estudio_cifras <- renderUI({
    e <- estudio_mod(); req(e)
    d <- cmf_estudio_tabla(e$id, "estimandos")
    if (is.null(d) || !nrow(d)) return(NULL)
    div(class = "cifras", lapply(seq_len(nrow(d)), function(i) {
      x <- estimando_fmt(d[i])
      div(div(class = "rotulo", x$titulo), div(class = "cifra", x$valor), div(class = "s", x$sub))
    }))
  })
  plot_estudio <- reactive({
    e <- estudio_mod(); req(e, input$est_fig)
    col <- col_mod()
    switch(input$est_fig,
      anual = cmf_plot_estudio_anual(e$id, color = col),
      territorio = {
        req(input$est_terr)
        q <- cmf_estudio_territorio(e$id, input$est_terr)
        if (identical(input$est_terr_tipo, "bars")) cmf_plot_bars(q, estilo = input$estilo, color = col)
        else cmf_plot_map(q, estilo = input$estilo)
      },
      cmf_plot_escenarios(e$id, ingreso = input$est_ingreso %||% "medio", color = col))
  })
  output$p_estudio <- renderPlot(pantalla(plot_estudio(), 13), res = 96)
  output$estudio_nota <- renderUI({
    e <- estudio_mod(); req(e)
    mo <- cmf_estudio_tabla(e$id, "moran")
    txt <- switch(input$est_fig %||% "escenarios",
      escenarios = "Cada punto es la pérdida total del escenario entre los episodios valorables; la línea, su IC 95% (bootstrap agrupado por hospital, parámetros de retorno al trabajo fijos). El relleno marca el caso base. Los escenarios expresan la incertidumbre estructural y no se promedian.",
      anual = "Escenario principal, millones de pesos del mismo período monetario en todos los años; banda, IC 95% por bootstrap agrupado por hospital.",
      territorio = {
        m <- if (!is.null(mo)) mo[grepl("productiv", tolower(panel))] else NULL
        paste0("Servicio de Salud de residencia (29 territorios; los episodios sin residencia quedan fuera).",
               if (!is.null(m) && nrow(m)) paste0(" Autocorrelación espacial de la pérdida por 100.000 personas-año, I de Moran: ",
                 paste(sprintf("%s %s (p = %s)", m$pesos, fmt_num(m$moran_i, 3), fmt_num(m$p, 3)), collapse = "; "), "."))
      })
    p(class = "nota", txt)
  })
  codigo_estudio <- reactive({
    e <- estudio_mod()
    fig <- switch(input$est_fig %||% "escenarios",
      anual = sprintf('cmf_plot_estudio_anual("%s")', e$id),
      territorio = sprintf('cmf_plot_%s(cmf_estudio_territorio("%s", "%s"))', if (identical(input$est_terr_tipo, "bars")) "bars" else "map",
                           e$id, input$est_terr),
      sprintf('cmf_plot_escenarios("%s", ingreso = "%s")', e$id, input$est_ingreso %||% "medio"))
    paste0("library(observatoriocmf)  # datos v", cmf_version(), "\n\n",
           sprintf('cmf_estudio_tabla("%s", "%s")\n', e$id, switch(input$est_fig %||% "escenarios", anual = "productividad_anual",
                                                                   territorio = "territorio", "escenarios")),
           "p <- ", fig, "\n", sprintf('cmf_save(p, "figura.%s", width = "%s")', input$fmt, input$ancho))
  })

  # Biblioteca -----------------------------------------------------------------------------------------
  bib_filtrada <- reactive({
    d <- biblio
    if (nzchar(input$bib_ambito %||% "")) d <- d[d$modulo == input$bib_ambito, ]
    if (nzchar(input$bib_tipo %||% "")) d <- d[d$tipo == input$bib_tipo, ]
    if (nzchar(input$bib_region %||% "")) d <- d[d$region == input$bib_region, ]
    b <- trimws(input$bib_buscar %||% "")
    if (nzchar(b)) {
      txt <- tolower(paste(d$autores, d$titulo, d$institucion, d$revista, d$diseno))
      d <- d[grepl(tolower(b), txt, fixed = TRUE), ]
    }
    d[order(-suppressWarnings(as.numeric(d$anio)), d$autores), ]
  })
  output$bib_n <- renderText({
    d <- bib_filtrada()
    n_tipo <- table(factor(d$tipo, levels = names(BIBLIO_TIPOS)))
    partes <- paste0(n_tipo[n_tipo > 0], " ", tolower(BIBLIO_TIPOS[names(n_tipo)[n_tipo > 0]]))
    paste0(nrow(d), if (nrow(d) == 1) " referencia" else " referencias", if (length(partes)) paste0(": ", paste(partes, collapse = ", ")) else "")
  })
  output$bib_lista <- renderUI({
    d <- bib_filtrada()
    if (!nrow(d)) return(p(class = "mt-3", "No hay referencias con esos filtros."))
    tagList(lapply(seq_len(nrow(d)), function(i) {
      a <- d[i, ]
      col <- if (a$modulo %in% modulos$id) color_mod(a$modulo) else "#141414"
      meta <- c(if (a$modulo %in% modulos$id) modulos$nombre[modulos$id == a$modulo],
                BIBLIO_TIPOS[[a$tipo]] %||% a$tipo,
                if (nzchar(a$institucion)) a$institucion, if (nzchar(a$region)) a$region,
                if (nzchar(a$periodo)) a$periodo, if (nzchar(a$casos)) paste0(fmt_num(as.numeric(a$casos)), " casos"),
                if (nzchar(a$datos)) a$datos, if (nzchar(a$estado) && a$estado != "publicado") a$estado)
      div(class = "articulo", style = paste0("--c:", col),
        div(class = "anio", a$anio),
        div(div(class = "rotulo", paste(meta, collapse = ", ")),
            p(class = "ref", cita_articulo(a)),
            if (nzchar(a$diseno)) p(class = "small mb-0", style = "color:var(--gris)", a$diseno)),
        span(class = "pestana", style = paste0("background:", if (a$modulo %in% modulos$id) col else "#D6D3CC")))
    }))
  })
  output$dl_bib_ris <- downloadHandler(
    filename = function() sprintf("biblioteca_cmf_chile_v%s.ris", cmf_version()),
    content = function(file) writeLines(cmf_biblioteca_ris(bib_filtrada()), file, useBytes = TRUE))
  output$dl_bib_csv <- downloadHandler(
    filename = function() sprintf("biblioteca_cmf_chile_v%s.csv", cmf_version()),
    content = function(file) data.table::fwrite(bib_filtrada(), file, bom = TRUE))

  # Capacidad instalada y lista de espera -----------------------------------------------------------------
  if (!is.null(oferta) && nrow(oferta)) {
    output$of_titulo <- renderText(oferta_lbl(input$of_ind))
    output$p_oferta <- renderPlot({
      req(input$of_ind)
      d <- data.table::copy(oferta)[!is.na(get(input$of_ind))]
      q <- data.table::data.table(servicio_salud = as.integer(d$servicio_salud), n = d[[input$of_ind]])
      data.table::setattr(q, "cmf_query", list(metric = "conteo", by = "servicio_salud", version = cmf_version(),
                                               filtros = list(), fuente_txt = "RNPI (Transparencia) y Visor Ciudadano LE"))
      p <- if (identical(input$of_tipo, "bars")) cmf_plot_bars(q, color = color_mod("oferta")) + ggplot2::labs(x = oferta_lbl(input$of_ind))
           else cmf_plot_map(q, etiqueta = oferta_lbl(input$of_ind))
      if (inherits(p, "patchwork")) p & ggplot2::theme(text = ggplot2::element_text(size = 12))
      else p + cmf_theme(base_size = 13)
    }, res = 96)
    output$t_oferta <- DT::renderDT({
      cols <- intersect(c("nombre_servicio_salud", names(OFERTA_LBL)), names(oferta))
      d <- oferta[, cols, with = FALSE]
      data.table::setnames(d, cols, c("Servicio de Salud", OFERTA_LBL[setdiff(cols, "nombre_servicio_salud")]))
      dt_pub(d, rownames = FALSE, class = "compact", options = list(pageLength = 29, dom = "t", scrollX = TRUE))
    })
    output$dl_oferta <- downloadHandler(
      filename = function() sprintf("cmf_oferta_servicios_v%s.csv", cmf_version()),
      content = function(file) { writeLines(paste0("# ", cmf_cite()), file); data.table::fwrite(oferta, file, append = TRUE, col.names = TRUE) })
  }
}

shinyApp(ui, server)
