#' Abre la aplicación pública del observatorio
#' @export
run_app <- function(...) {
  dir <- system.file("app", package = "observatoriocmf")
  shiny::runApp(dir, ...)
}

#' Abre el panel de administración (requiere GH_TOKEN y GH_REPO en el entorno)
#' @export
run_admin <- function(dir = "admin", ...) {
  shiny::runApp(dir, ...)
}
