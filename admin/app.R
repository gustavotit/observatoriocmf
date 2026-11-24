# Panel de administración: Observatorio CMF Chile
#
# Permite cargar nuevas versiones de datos, editar diccionarios, parámetros y mapeos de columnas,
# y lanzar la reconstrucción y el redespliegue, sin tocar el código fuente.
# Nada se guarda en el servidor: todo se escribe en el repositorio de GitHub (versionado y auditable).
#
# Variables de entorno: GH_TOKEN, GH_REPO, ADMIN_USER, ADMIN_PASSWORD
#   (opcionales: GH_BRANCH = main, GH_WORKFLOW = build-deploy.yml, ADMIN_LOCAL = true para uso local sin login)

library(shiny)
library(bslib)

options(shiny.maxRequestSize = 300 * 1024^2)

tema <- bs_theme(
  version = 5, bg = "#F3F5F8", fg = "#0B1B2E", primary = "#0E6E6A", secondary = "#4A5668",
  base_font = font_google("Inter", local = FALSE), heading_font = font_google("Space Grotesk", local = FALSE),
  code_font = font_google("JetBrains Mono", local = FALSE), "border-radius" = "10px"
)

ui_inner <- page_navbar(
  title = tags$span(tags$strong("Observatorio CMF"), tags$span(", administración", style = "opacity:.7")),
  theme = tema,
  bg = "#0B1B2E",
  fillable = FALSE,

  nav_panel("Estado",
    uiOutput("aviso_config"),
    layout_columns(col_widths = c(4, 4, 4),
      value_box("Versión publicada", textOutput("v_version"), theme = "primary"),
      value_box("Última construcción", textOutput("v_fecha")),
      value_box("Avisos de validación", textOutput("v_avisos"))
    ),
    card(card_header("Fuentes registradas"), tableOutput("tabla_fuentes")),
    card(card_header("Validación de la última construcción"), uiOutput("validacion")),
    actionButton("refrescar", "Actualizar", icon = icon("rotate"), class = "btn-outline-secondary")
  ),

  nav_panel("Cargar datos",
    layout_columns(col_widths = c(5, 7),
      card(card_header("Registrar una nueva versión"),
        selectInput("fuente", "Fuente", choices = NULL),
        textInput("version", "Etiqueta de versión", placeholder = "p. ej. 2023 o 2019-2024"),
        textInput("anios", "Años que cubre", placeholder = "2023; 2019-2024; 2019, 2021"),
        radioButtons("modo", "Cómo cargar", inline = TRUE,
                     choices = c("URL oficial" = "url", "Subir archivo" = "archivo")),
        conditionalPanel("input.modo == 'url'",
          textInput("url", "URL del archivo (zip, csv o xlsx)"),
          helpText("Recomendado para DEIS y GRD: GitHub Actions descarga el archivo al construir.")),
        conditionalPanel("input.modo == 'archivo'",
          fileInput("archivo", "Archivo (hasta 50 MB)", accept = c(".csv", ".txt", ".xlsx", ".xls", ".zip")),
          helpText("Para tablas pequeñas: población INE, IPC, aranceles MAI, empleo, tipo de cambio.")),
        textInput("hoja", "Hoja (solo xlsx, opcional)"),
        textInput("sha", "sha256 (opcional, para verificar la descarga)"),
        textAreaInput("nota", "Nota", rows = 2),
        actionButton("registrar", "Registrar versión", class = "btn-primary", icon = icon("upload"))
      ),
      card(card_header("Versiones de esta fuente"),
        DT::DTOutput("tabla_versiones"),
        actionButton("borrar_version", "Quitar versión seleccionada", class = "btn-outline-danger",
                     icon = icon("trash"))
      )
    )
  ),

  nav_panel("Diccionarios",
    card(
      card_header(class = "d-flex flex-wrap gap-2 align-items-center",
        selectInput("dic", NULL, choices = DICCIONARIOS, width = "360px"),
        actionButton("dic_agregar", "Agregar fila", icon = icon("plus"), class = "btn-outline-secondary btn-sm"),
        actionButton("dic_quitar", "Quitar filas seleccionadas", icon = icon("minus"), class = "btn-outline-secondary btn-sm"),
        actionButton("dic_guardar", "Guardar en el repositorio", icon = icon("floppy-disk"), class = "btn-primary btn-sm")
      ),
      helpText("Doble clic en una celda para editarla. 'incluir' = TRUE/FALSE define si el código entra en la definición de caso.",
               "Si cargas el catálogo oficial CIE-10, sus descripciones reemplazan a las de esta tabla al construir."),
      DT::DTOutput("tabla_dic")
    )
  ),

  nav_panel("Parámetros",
    layout_columns(col_widths = c(6, 6),
      card(card_header("Análisis"),
        numericInput("p_desde", "Año desde", value = NA),
        numericInput("p_hasta", "Año hasta (vacío = último disponible)", value = NA),
        numericInput("p_nmin", "Supresión: mínimo de casos por celda", value = 5, min = 1),
        numericInput("p_abierto", "Grupo de edad abierto (años)", value = 80),
        numericInput("p_ipc", "Año base IPC (CLP de ese año)", value = NA),
        numericInput("p_estructura", "Estructura de precios GRD (año de la resolución)", value = 2024),
        textInput("p_diacama", "Código MAI del día cama (opcional)"),
        checkboxInput("p_indirectos", "Incluir costos indirectos (capital humano)")
      ),
      card(card_header("Publicación"),
        textInput("p_titulo", "Título para la cita", width = "100%"),
        textInput("p_autores", "Autores (formato AMA)"),
        textInput("p_doi", "DOI de concepto (Zenodo)"),
        textAreaInput("p_articulo", "Artículo de referencia (cita completa del letter, cuando esté publicado)", rows = 2),
        textInput("p_urlapp", "URL de la app pública", width = "100%"),
        textInput("p_repo", "URL del repositorio", width = "100%")
      )
    ),
    actionButton("p_guardar", "Guardar parámetros", class = "btn-primary", icon = icon("floppy-disk"))
  ),

  nav_panel("Biblioteca",
    layout_columns(col_widths = c(5, 7),
      card(card_header("Agregar una referencia"),
        div(class = "d-flex gap-2 align-items-end",
          textInput("bib_doi", "DOI", placeholder = "10.xxxx/xxxxx", width = "100%"),
          actionButton("bib_crossref", "Completar desde el DOI", class = "btn-outline-secondary mb-3")),
        selectInput("bib_tipo", "Tipo", width = "100%", choices = stats::setNames(names(BIBLIO_TIPOS), BIBLIO_TIPOS), selected = "serie_hospitalaria"),
        selectizeInput("bib_modulo", "Ámbito", choices = NULL, width = "100%", options = list(placeholder = "Sin ámbito del observatorio")),
        textAreaInput("bib_autores", "Autores (Vancouver: Apellido AB, Apellido C)", rows = 2, width = "100%"),
        textAreaInput("bib_titulo", "Título", rows = 2, width = "100%"),
        layout_columns(col_widths = c(8, 4),
          textInput("bib_revista", "Revista (abreviatura NLM)"), textInput("bib_anio", "Año")),
        layout_columns(col_widths = c(6, 6),
          textInput("bib_referencia", "Volumen(número):páginas"), textInput("bib_pmid", "PMID (opcional)")),
        conditionalPanel("input.bib_tipo == 'serie_hospitalaria'",
          textInput("bib_institucion", "Hospital o centro", width = "100%")),
        layout_columns(col_widths = c(6, 6),
          selectizeInput("bib_region", "Región", choices = c("", REGIONES_CHILE)),
          textInput("bib_periodo", "Período de los datos", placeholder = "2010-2019")),
        layout_columns(col_widths = c(6, 6),
          textInput("bib_casos", "Casos o pacientes, n"),
          selectInput("bib_estado", "Estado", c("publicado", "aceptado", "en prensa"))),
        textInput("bib_diseno", "Diseño y fuente", width = "100%", placeholder = "p. ej. retrospectivo, fichas clínicas; DEIS 2001-2024"),
        actionButton("bib_agregar", "Agregar a la biblioteca", class = "btn-primary", icon = icon("plus")),
        helpText("Se guarda en config/publicaciones.yml. Para verla en la app basta \"Solo redesplegar\".")),
      card(card_header("Referencias"),
        DT::DTOutput("bib_tabla"),
        actionButton("bib_quitar", "Quitar la seleccionada", class = "btn-outline-danger", icon = icon("trash")),
        helpText("Para corregir una referencia: Configuración avanzada, archivo de artículos."))
    )
  ),

  nav_panel("Configuración avanzada",
    card(card_header(class = "d-flex flex-wrap gap-2 align-items-center",
        selectInput("cfg_file", NULL, width = "420px", choices = c(
          "Mapeo de columnas (config/column_maps.yml)" = "config/column_maps.yml",
          "Módulos: subgrupos principales, edad, universo, métricas (config/modules.yml)" = "config/modules.yml",
          "Biblioteca (config/publicaciones.yml)" = "config/publicaciones.yml"))),
      helpText("Mapeo: cuando DEIS o GRD cambian el nombre de una columna, agrega el nuevo nombre a la lista del campo.",
               "Módulos: define subgrupos principales, rango de edad, universo de referencia y métricas de cada módulo.",
               "Biblioteca: corrige referencias (para agregar usa la pestaña Biblioteca); basta \"Solo redesplegar\".",
               "El archivo se valida antes de guardarse."),
      textAreaInput("colmap", NULL, rows = 30, width = "100%"),
      actionButton("colmap_guardar", "Validar y guardar", class = "btn-primary", icon = icon("check"))
    )
  ),

  nav_panel("Publicar",
    layout_columns(col_widths = c(5, 7),
      card(card_header("Actualización anual: subir datos procesados"),
        p(class = "small text-secondary",
          "Las bases DEIS y GRD se procesan una vez al año en tu computador con el notebook de actualización. ",
          "Sube aquí el zip que genera (casos clasificados y tablas): queda en el repositorio en un solo commit. ",
          "Los parámetros (IPC, productividad, INE, nacidos vivos, MAI, oferta) se actualizan en 'Cargar datos' y ",
          "los precios GRD en 'Diccionarios'; luego 'Recalcular'."),
        fileInput("zip_datos", "Zip de datos procesados", accept = ".zip"),
        uiOutput("zip_resumen"),
        actionButton("zip_guardar", "Guardar en el repositorio", class = "btn-primary", icon = icon("upload"))
      ),
      card(card_header("Controles contra las publicaciones (del zip)"), tableOutput("zip_controles"))
    ),
    layout_columns(col_widths = c(5, 7),
      card(card_header("Desplegar y versionar"),
        radioButtons("pub_modo", NULL, choices = c(
          "Recalcular con los parámetros vigentes y redesplegar" = "completo",
          "Solo redesplegar (los datos ya vienen calculados, p. ej. tras subir el zip anual)" = "solo_deploy")),
        helpText("Recalcular usa los casos clasificados guardados (data-casos/) con el IPC, la productividad, los precios GRD, ",
                 "la población INE, los nacidos vivos y la oferta vigentes. No necesita las bases DEIS/GRD."),
        checkboxInput("pub_release", "Crear versión (release en GitHub → Zenodo la agrega bajo el mismo DOI de concepto)"),
        conditionalPanel("input.pub_release",
          selectInput("pub_bump", "Tipo de versión", c("patch (correcciones)" = "patch",
                                                       "minor (actualización anual de datos o módulo nuevo)" = "minor",
                                                       "major (cambios de definición)" = "major")),
          textAreaInput("pub_nota", "Nota de versión", rows = 3)),
        actionButton("publicar", "Ejecutar", class = "btn-primary", icon = icon("rocket"))
      ),
      card(card_header("Ejecuciones recientes"), tableOutput("tabla_runs"),
           helpText("Se actualiza cada 30 segundos."))
    )
  )
)

ui <- if (identical(Sys.getenv("ADMIN_LOCAL"), "true")) {
  ui_inner
} else {
  if (!requireNamespace("shinymanager", quietly = TRUE)) {
    stop("El panel publicado requiere shinymanager para el inicio de sesión")
  }
  shinymanager::secure_app(ui_inner, language = "es", enable_admin = FALSE)
}

server <- function(input, output, session) {
  if (!identical(Sys.getenv("ADMIN_LOCAL"), "true")) {
    creds <- data.frame(user = Sys.getenv("ADMIN_USER"), password = Sys.getenv("ADMIN_PASSWORD"),
                        stringsAsFactors = FALSE)
    if (!nzchar(creds$user) || !nzchar(creds$password)) stop("Faltan ADMIN_USER y ADMIN_PASSWORD")
    shinymanager::secure_server(check_credentials = shinymanager::check_credentials(creds))
  }

  notify_error <- function(expr) {
    tryCatch(expr, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 10)
      NULL
    })
  }

  output$aviso_config <- renderUI({
    if (gh_configured()) return(NULL)
    div(class = "alert alert-danger", "Faltan GH_TOKEN y GH_REPO: el panel no puede leer ni escribir el repositorio.")
  })

  # ---- Estado ------------------------------------------------------------------
  refresh <- reactiveVal(0)
  observeEvent(input$refrescar, refresh(refresh() + 1))

  sources <- reactive({
    refresh()
    req(gh_configured())
    f <- notify_error(gh_get_file("config/sources.yml"))
    req(f)
    yaml::yaml.load(f$text)
  })

  manifest <- reactive({
    refresh()
    req(gh_configured())
    f <- notify_error(gh_get_file("inst/extdata/manifest.json"))
    if (is.null(f)) return(list())
    jsonlite::fromJSON(f$text, simplifyVector = FALSE)
  })

  validacion <- reactive({
    refresh()
    req(gh_configured())
    f <- notify_error(gh_get_file("inst/extdata/validacion.json"))
    if (is.null(f)) return(list())
    jsonlite::fromJSON(f$text, simplifyVector = FALSE)
  })

  output$v_version <- renderText(manifest()$version %||% "")
  output$v_fecha <- renderText(manifest()$construido %||% "")
  output$v_avisos <- renderText({
    m <- manifest()
    if (!length(m)) return("")
    paste0(m$n_errores %||% 0, " errores, ", m$n_advertencias %||% 0, " avisos")
  })

  output$tabla_fuentes <- renderTable({
    s <- sources()$fuentes
    data.frame(
      Fuente = vapply(s, `[[`, "", "nombre"),
      Versiones = vapply(s, function(x) length(x$versiones %||% list()), 0L),
      `Última versión` = vapply(s, function(x) {
        v <- x$versiones %||% list()
        if (length(v)) as.character(v[[length(v)]]$version) else ""
      }, ""),
      check.names = FALSE
    )
  })

  output$validacion <- renderUI({
    v <- validacion()
    if (!length(v)) return(helpText("Aún no hay construcciones."))
    tagList(
      if (length(v$errores)) div(class = "alert alert-danger", tags$ul(lapply(v$errores, tags$li))),
      if (length(v$advertencias)) div(class = "alert alert-warning", tags$ul(lapply(v$advertencias, tags$li))),
      if (!length(v$errores) && !length(v$advertencias)) div(class = "alert alert-success", "Sin observaciones.")
    )
  })

  # ---- Cargar datos ------------------------------------------------------------
  observe({
    s <- sources()$fuentes
    ch <- stats::setNames(names(s), vapply(s, `[[`, "", "nombre"))
    updateSelectInput(session, "fuente", choices = ch, selected = isolate(input$fuente) %||% ch[1])
  })

  observeEvent(input$fuente, {
    src <- sources()$fuentes[[input$fuente]]
    updateRadioButtons(session, "modo", selected = if (identical(src$tipo_carga, "url")) "url" else "archivo")
  })

  output$tabla_versiones <- DT::renderDT({
    req(input$fuente)
    DT::datatable(versions_table(sources()$fuentes[[input$fuente]]), selection = "single",
                  rownames = FALSE, options = list(dom = "tp", pageLength = 10))
  })

  write_sources <- function(s, msg) {
    txt <- paste0(SOURCES_HEADER, yaml::as.yaml(s, indent.mapping.sequence = TRUE))
    gh_put_file("config/sources.yml", txt, msg)
    refresh(refresh() + 1)
  }

  observeEvent(input$registrar, {
    req(input$fuente)
    ver <- safe_version(input$version)
    if (!nzchar(ver)) return(showNotification("Falta la etiqueta de versión", type = "warning"))
    s <- sources()
    entry <- list(version = ver, anios = parse_years(input$anios),
                  fecha_carga = as.character(Sys.Date()), nota = input$nota)
    if (nzchar(input$hoja)) entry$hoja <- input$hoja
    if (nzchar(input$sha)) entry$sha256 <- trimws(input$sha)

    ok <- notify_error({
      if (input$modo == "url") {
        if (!grepl("^https?://", input$url)) stop("La URL debe empezar con http:// o https://")
        entry$url <- trimws(input$url)
      } else {
        f <- input$archivo
        if (is.null(f)) stop("Selecciona un archivo")
        ext <- tolower(tools::file_ext(f$name))
        path <- sprintf("data-small/%s/%s.%s", input$fuente, ver, ext)
        withProgress(message = "Subiendo archivo al repositorio…", {
          bytes <- readBin(f$datapath, "raw", file.info(f$datapath)$size)
          entry$sha256 <- digest::digest(file = f$datapath, algo = "sha256")
          gh_put_file(path, bytes, sprintf("Datos: %s %s", input$fuente, ver))
        })
        entry$archivo <- path
      }
      vers <- s$fuentes[[input$fuente]]$versiones %||% list()
      vers <- Filter(function(x) !identical(as.character(x$version), ver), vers)
      s$fuentes[[input$fuente]]$versiones <- c(vers, list(entry))
      write_sources(s, sprintf("Registrar %s %s", input$fuente, ver))
      TRUE
    })
    if (isTRUE(ok)) showNotification("Versión registrada. Ve a 'Publicar' para reconstruir.", type = "message")
  })

  observeEvent(input$borrar_version, {
    sel <- input$tabla_versiones_rows_selected
    req(sel)
    s <- sources()
    vers <- s$fuentes[[input$fuente]]$versiones
    quitada <- vers[[sel]]$version
    s$fuentes[[input$fuente]]$versiones <- vers[-sel]
    notify_error(write_sources(s, sprintf("Quitar %s %s", input$fuente, quitada)))
    showNotification(paste("Versión", quitada, "quitada (el archivo queda en el historial del repositorio)."))
  })

  # ---- Diccionarios ------------------------------------------------------------
  dic_data <- reactiveVal(NULL)
  observeEvent(list(input$dic, refresh()), {
    req(gh_configured(), input$dic)
    f <- notify_error(gh_get_file(input$dic))
    dic_data(if (is.null(f)) NULL else read_csv_text(f$text))
  })

  output$tabla_dic <- DT::renderDT({
    req(dic_data())
    DT::datatable(dic_data(), editable = "cell", rownames = FALSE, filter = "top",
                  options = list(pageLength = 25, scrollX = TRUE))
  })

  observeEvent(input$tabla_dic_cell_edit, {
    info <- input$tabla_dic_cell_edit
    d <- dic_data()
    d[info$row, info$col + 1] <- as.character(info$value)
    dic_data(d)
  })

  observeEvent(input$dic_agregar, {
    d <- dic_data()
    req(d)
    nueva <- as.data.frame(as.list(stats::setNames(rep("", ncol(d)), names(d))), check.names = FALSE)
    if ("incluir" %in% names(d)) nueva$incluir <- "TRUE"
    dic_data(rbind(nueva, d))
  })

  observeEvent(input$dic_quitar, {
    sel <- input$tabla_dic_rows_selected
    req(sel)
    dic_data(dic_data()[-sel, , drop = FALSE])
  })

  observeEvent(input$dic_guardar, {
    d <- dic_data()
    req(d)
    ok <- notify_error({
      if ("codigo" %in% names(d) && any(!nzchar(d$codigo))) stop("Hay filas sin código")
      for (k in intersect(c("incluir", "requiere_proc", "etiologico"), names(d))) {
        if (any(!toupper(d[[k]]) %in% c("TRUE", "FALSE"))) stop("La columna '", k, "' solo admite TRUE o FALSE")
      }
      gh_put_file(input$dic, csv_text(d), paste("Diccionario:", basename(input$dic)))
      TRUE
    })
    if (isTRUE(ok)) showNotification("Diccionario guardado.", type = "message")
  })

  # ---- Parámetros ----------------------------------------------------------------
  params <- reactive({
    refresh()
    req(gh_configured())
    f <- notify_error(gh_get_file("config/pipeline.yml"))
    req(f)
    yaml::yaml.load(f$text)
  })

  observe({
    p <- params()
    updateNumericInput(session, "p_desde", value = p$anios$desde %||% NA)
    updateNumericInput(session, "p_hasta", value = p$anios$hasta %||% NA)
    updateNumericInput(session, "p_nmin", value = p$supresion$n_min %||% 5)
    updateNumericInput(session, "p_abierto", value = p$edad$grupo_abierto %||% 80)
    updateNumericInput(session, "p_ipc", value = p$costos$anio_base_ipc %||% NA)
    updateNumericInput(session, "p_estructura", value = p$costos$estructura_precios_grd %||% 2024)
    updateTextInput(session, "p_diacama", value = p$costos$codigo_mai_dia_cama %||% "")
    updateCheckboxInput(session, "p_indirectos", value = isTRUE(p$costos$incluir_costos_indirectos))
    updateTextInput(session, "p_titulo", value = p$publicacion$titulo %||% "")
    updateTextInput(session, "p_autores", value = p$publicacion$autores %||% "")
    updateTextInput(session, "p_doi", value = p$publicacion$doi %||% "")
    updateTextAreaInput(session, "p_articulo", value = p$publicacion$articulo_referencia %||% "")
    updateTextInput(session, "p_urlapp", value = p$publicacion$url_app %||% "")
    updateTextInput(session, "p_repo", value = p$publicacion$repositorio %||% "")
  })

  observeEvent(input$p_guardar, {
    p <- params()
    na_null <- function(x) if (is.null(x) || is.na(x) || identical(x, "")) NULL else x
    p$anios$desde <- na_null(input$p_desde)
    p$anios["hasta"] <- list(na_null(input$p_hasta))
    p$supresion$n_min <- input$p_nmin
    p$edad$grupo_abierto <- input$p_abierto
    p$costos$anio_base_ipc <- na_null(input$p_ipc)
    p$costos$estructura_precios_grd <- na_null(input$p_estructura)
    p$costos["codigo_mai_dia_cama"] <- list(na_null(input$p_diacama))
    p$costos$incluir_costos_indirectos <- isTRUE(input$p_indirectos)
    p$publicacion$titulo <- input$p_titulo
    p$publicacion$autores <- input$p_autores
    p$publicacion["doi"] <- list(na_null(input$p_doi))
    p$publicacion["articulo_referencia"] <- list(na_null(input$p_articulo))
    p$publicacion["url_app"] <- list(na_null(input$p_urlapp))
    p$publicacion["repositorio"] <- list(na_null(input$p_repo))
    txt <- paste0("# Parámetros del pipeline (editado desde el panel de administración).\n\n", yaml::as.yaml(p))
    ok <- notify_error({ gh_put_file("config/pipeline.yml", txt, "Parámetros del pipeline"); TRUE })
    if (isTRUE(ok)) showNotification("Parámetros guardados.", type = "message")
  })

  # ---- Biblioteca ----------------------------------------------------------------------
  bib_rev <- reactiveVal(0)
  bib_archivo <- reactive({
    refresh(); bib_rev()
    req(gh_configured())
    notify_error(gh_get_file("config/publicaciones.yml")) %||% list(text = "articulos:\n", sha = NULL)
  })
  bib_lista <- reactive(yaml::yaml.load(bib_archivo()$text)$articulos %||% list())
  observe({
    req(gh_configured())
    m <- tryCatch(yaml::yaml.load(gh_get_file("config/modules.yml")$text)$modulos, error = function(e) list())
    ids <- vapply(m, function(x) x$id %||% "", ""); nm <- vapply(m, function(x) x$nombre %||% "", "")
    updateSelectizeInput(session, "bib_modulo", choices = c("Sin ámbito" = "", stats::setNames(ids, nm)))
  })
  observeEvent(input$bib_crossref, {
    x <- notify_error(crossref_doi(input$bib_doi))
    req(x)
    for (k in c("autores", "titulo", "revista", "referencia")) updateTextAreaInput(session, paste0("bib_", k), value = x[[k]])
    updateTextInput(session, "bib_anio", value = x$anio)
    updateTextInput(session, "bib_doi", value = x$doi)
    showNotification("Datos completados desde Crossref: revísalos antes de agregar.", type = "message")
  })
  output$bib_tabla <- DT::renderDT({
    a <- bib_lista()
    d <- data.frame(Año = vapply(a, function(x) as.character(x$anio %||% ""), ""),
                    Tipo = vapply(a, function(x) BIBLIO_TIPOS[x$tipo %||% "observatorio"] %||% "", ""),
                    Autores = vapply(a, function(x) substr(x$autores %||% "", 1, 40), ""),
                    Título = vapply(a, function(x) x$titulo %||% "", ""), stringsAsFactors = FALSE)
    DT::datatable(d, selection = "single", rownames = FALSE, options = list(pageLength = 15, dom = "ftp"))
  })
  observeEvent(input$bib_agregar, {
    ok <- notify_error({
      if (!nzchar(trimws(input$bib_autores)) || !nzchar(trimws(input$bib_titulo))) stop("Faltan autores o título")
      if (!grepl("^[0-9]{4}$", trimws(input$bib_anio))) stop("El año debe tener 4 dígitos")
      doi <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", trimws(input$bib_doi))
      dup <- nzchar(doi) && any(vapply(bib_lista(), function(x) identical(tolower(x$doi %||% ""), tolower(doi)), TRUE))
      if (dup) stop("Ese DOI ya está en la biblioteca")
      a <- list(tipo = input$bib_tipo, modulo = input$bib_modulo, autores = input$bib_autores, anio = input$bib_anio,
                titulo = input$bib_titulo, revista = input$bib_revista, referencia = input$bib_referencia, doi = doi,
                pmid = input$bib_pmid, estado = input$bib_estado,
                institucion = if (input$bib_tipo == "serie_hospitalaria") input$bib_institucion else "",
                region = input$bib_region, periodo = input$bib_periodo, casos = input$bib_casos, diseno = input$bib_diseno)
      nuevo <- biblio_agregar_texto(bib_archivo()$text, a)
      if (length(yaml::yaml.load(nuevo)$articulos) != length(bib_lista()) + 1) stop("No se pudo agregar (YAML inválido)")
      gh_put_file("config/publicaciones.yml", nuevo, paste("Biblioteca: agrega", substr(a$titulo, 1, 60)))
      TRUE
    })
    if (isTRUE(ok)) {
      bib_rev(bib_rev() + 1)
      for (k in c("doi", "autores", "titulo", "revista", "anio", "referencia", "pmid", "institucion", "periodo", "casos", "diseno"))
        updateTextInput(session, paste0("bib_", k), value = "")
      showNotification("Referencia agregada. Usa \"Solo redesplegar\" para verla en la app.", type = "message")
    }
  })
  observeEvent(input$bib_quitar, {
    i <- input$bib_tabla_rows_selected
    if (!length(i)) return(showNotification("Selecciona una referencia en la tabla", type = "warning"))
    ok <- notify_error({
      nuevo <- biblio_quitar_texto(bib_archivo()$text, i)
      if (length(yaml::yaml.load(nuevo)$articulos %||% list()) != length(bib_lista()) - 1) stop("No se pudo quitar")
      gh_put_file("config/publicaciones.yml", nuevo, paste("Biblioteca: quita", substr(bib_lista()[[i]]$titulo %||% "", 1, 60)))
      TRUE
    })
    if (isTRUE(ok)) { bib_rev(bib_rev() + 1); showNotification("Referencia quitada.", type = "message") }
  })

  # ---- Configuración avanzada (YAML) ---------------------------------------------------
  observeEvent(list(refresh(), input$cfg_file), {
    req(gh_configured(), input$cfg_file)
    f <- notify_error(gh_get_file(input$cfg_file))
    if (!is.null(f)) updateTextAreaInput(session, "colmap", value = f$text)
  })

  observeEvent(input$colmap_guardar, {
    ok <- notify_error({
      parsed <- yaml::yaml.load(input$colmap)
      if (input$cfg_file == "config/column_maps.yml" && is.null(parsed$egresos_deis$diag1)) {
        stop("El mapa debe definir egresos_deis: diag1")
      }
      if (input$cfg_file == "config/modules.yml") {
        ids <- vapply(parsed$modulos %||% list(), function(m) m$id %||% "", "")
        if (!length(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) stop("Cada módulo necesita un id único")
      }
      if (input$cfg_file == "config/publicaciones.yml") {
        arts <- parsed$articulos %||% list()
        falta <- vapply(arts, function(a) !nzchar(a$titulo %||% "") || !nzchar(a$autores %||% ""), TRUE)
        if (any(falta)) stop("Cada artículo necesita autores y título")
      }
      gh_put_file(input$cfg_file, input$colmap, paste("Configuración:", basename(input$cfg_file)))
      TRUE
    })
    if (isTRUE(ok)) showNotification("Configuración guardada.", type = "message")
  })

  # ---- Actualización anual: zip de datos procesados ---------------------------------
  zip_datos <- reactive({
    req(input$zip_datos)
    dir <- tempfile("datos_"); dir.create(dir)
    utils::unzip(input$zip_datos$datapath, exdir = dir)
    files <- list.files(dir, recursive = TRUE, full.names = TRUE)
    rel <- substring(files, nchar(dir) + 2)
    # Destino en el repositorio: data-casos/ (casos clasificados) o inst/extdata/ (tablas publicadas).
    destino <- ifelse(grepl("(^|/)data-casos/", rel), sub("^.*?data-casos/", "data-casos/", rel),
                      paste0("inst/extdata/", basename(rel)))
    permitidos <- grepl("\\.(csv\\.gz|csv|json|parquet|yml)$", rel)
    files <- files[permitidos]; names(files) <- destino[permitidos]
    validate(need(all(c("inst/extdata/manifest.json", "inst/extdata/cubo.csv.gz") %in% names(files)),
                  "El zip debe traer manifest.json y cubo.csv.gz (salida del notebook de actualización)."))
    ctl <- files[["inst/extdata/controles.json"]]
    list(files = files, manifest = jsonlite::read_json(files[["inst/extdata/manifest.json"]]),
         n_casos = sum(grepl("^data-casos/casos_", names(files))),
         controles = if (!is.null(ctl)) jsonlite::read_json(ctl, simplifyVector = TRUE))
  })
  output$zip_resumen <- renderUI({
    z <- zip_datos(); m <- z$manifest
    tags$ul(class = "small",
      tags$li(paste0("Construido: ", m$construido %||% "")),
      tags$li(paste0("Archivos: ", length(z$files), " (", round(sum(file.size(z$files)) / 1e6, 1), " MB), ",
                     z$n_casos, " de casos clasificados por fuente y año")),
      tags$li(paste0("Años: ", if (is.null(m$anios)) "" else
        paste(vapply(names(m$anios), function(k) paste0(k, " ", paste(unlist(m$anios[[k]]), collapse = "-")), ""), collapse = "; "))))
  })
  output$zip_controles <- renderTable({
    z <- zip_datos(); ct <- z$controles
    if (is.null(ct)) return(data.frame(Aviso = "El zip no trae controles.json"))
    data.frame(Referencia = ct$referencia, Publicado = ct$valor, Observatorio = round(ct$obtenido, 3), Estado = ct$estado)
  })
  observeEvent(input$zip_guardar, {
    ok <- notify_error({
      z <- zip_datos()
      contenido <- lapply(z$files, function(f) readBin(f, "raw", file.size(f)))
      names(contenido) <- names(z$files)
      gh_commit_files(contenido, paste("Datos procesados (actualización local)", z$manifest$construido %||% ""))
      TRUE
    })
    if (isTRUE(ok)) showNotification("Datos guardados. Ahora usa 'Desplegar y versionar'.", type = "message")
  })

  # ---- Publicar --------------------------------------------------------------------
  observeEvent(input$publicar, {
    ok <- notify_error({
      gh_dispatch(list(
        solo_deploy = input$pub_modo == "solo_deploy",
        release = isTRUE(input$pub_release),
        bump = input$pub_bump %||% "patch",
        nota = input$pub_nota %||% ""
      ))
      TRUE
    })
    if (isTRUE(ok)) showNotification("Construcción lanzada. Sigue el avance abajo.", type = "message")
  })

  output$tabla_runs <- renderTable({
    invalidateLater(30000)
    input$publicar
    req(gh_configured())
    gh_runs()
  }, sanitize.text.function = identity)
}

shinyApp(ui, server)
