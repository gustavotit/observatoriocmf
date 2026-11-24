# Cliente mínimo de la API de GitHub para el panel de administración -------------
# Requiere en el entorno: GH_TOKEN (token fine-grained con permisos Contents y Actions de
# lectura/escritura, solo para este repositorio) y GH_REPO ("usuario/observatoriocmf").

gh_repo <- function() Sys.getenv("GH_REPO")
gh_branch <- function() Sys.getenv("GH_BRANCH", "main")
gh_workflow <- function() Sys.getenv("GH_WORKFLOW", "build-deploy.yml")

gh_configured <- function() nzchar(Sys.getenv("GH_TOKEN")) && nzchar(gh_repo())

gh_req <- function(path, accept = "application/vnd.github+json") {
  httr2::request("https://api.github.com") |>
    httr2::req_url_path_append(path) |>
    httr2::req_headers(Accept = accept, `X-GitHub-Api-Version` = "2022-11-28") |>
    httr2::req_auth_bearer_token(Sys.getenv("GH_TOKEN")) |>
    httr2::req_user_agent("observatoriocmf-admin") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_timeout(120)
}

gh_check <- function(resp, what) {
  st <- httr2::resp_status(resp)
  if (st >= 300) {
    msg <- tryCatch(httr2::resp_body_json(resp)$message, error = function(e) "")
    stop(what, " falló (HTTP ", st, "): ", msg, call. = FALSE)
  }
  resp
}

# Lee un archivo del repositorio. Devuelve list(sha, text) o NULL si no existe.
gh_get_file <- function(path) {
  resp <- gh_req(paste0("repos/", gh_repo(), "/contents/", path)) |>
    httr2::req_url_query(ref = gh_branch()) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) == 404) return(NULL)
  gh_check(resp, paste("Leer", path))
  body <- httr2::resp_body_json(resp)
  text <- if (identical(body$encoding, "base64") && nzchar(body$content %||% "")) {
    rawToChar(jsonlite::base64_dec(gsub("\n", "", body$content, fixed = TRUE)))
  } else {
    raw <- gh_req(paste0("repos/", gh_repo(), "/contents/", path),
                  accept = "application/vnd.github.raw+json") |>
      httr2::req_url_query(ref = gh_branch()) |>
      httr2::req_perform()
    httr2::resp_body_string(gh_check(raw, paste("Leer", path)))
  }
  Encoding(text) <- "UTF-8"
  list(sha = body$sha, text = text)
}

# Crea o actualiza un archivo (un commit por llamada). `content` puede ser texto o raw.
gh_put_file <- function(path, content, message) {
  if (is.character(content)) content <- charToRaw(enc2utf8(paste(content, collapse = "\n")))
  current <- tryCatch(gh_get_file(path), error = function(e) NULL)
  body <- list(message = paste(message, "[skip ci]"), content = jsonlite::base64_enc(content),
               branch = gh_branch())
  if (!is.null(current)) body$sha <- current$sha
  resp <- gh_req(paste0("repos/", gh_repo(), "/contents/", path)) |>
    httr2::req_method("PUT") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()
  gh_check(resp, paste("Guardar", path))
  invisible(TRUE)
}

gh_dispatch <- function(inputs) {
  inputs <- lapply(inputs, function(x) if (is.logical(x)) tolower(as.character(x)) else as.character(x))
  resp <- gh_req(paste0("repos/", gh_repo(), "/actions/workflows/", gh_workflow(), "/dispatches")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(ref = gh_branch(), inputs = inputs)) |>
    httr2::req_perform()
  gh_check(resp, "Lanzar la construcción")
  invisible(TRUE)
}

gh_runs <- function(n = 8) {
  resp <- gh_req(paste0("repos/", gh_repo(), "/actions/workflows/", gh_workflow(), "/runs")) |>
    httr2::req_url_query(per_page = n) |>
    httr2::req_perform()
  if (httr2::resp_status(resp) >= 300) return(data.frame())
  runs <- httr2::resp_body_json(resp)$workflow_runs
  if (!length(runs)) return(data.frame())
  data.frame(
    inicio = vapply(runs, function(r) sub("T", " ", sub("Z$", "", r$created_at)), ""),
    estado = vapply(runs, function(r) r$status %||% "", ""),
    resultado = vapply(runs, function(r) r$conclusion %||% "en curso", ""),
    detalle = vapply(runs, function(r) sprintf('<a href="%s" target="_blank">Ver registro</a>', r$html_url), ""),
    stringsAsFactors = FALSE
  )
}

# Sube varios archivos en un solo commit (API Git Data): blobs → árbol → commit → mueve la rama.
# `files`: lista con nombre (ruta en el repositorio) → contenido raw.
gh_commit_files <- function(files, message) {
  repo <- gh_repo(); br <- gh_branch()
  json <- function(resp, what) httr2::resp_body_json(gh_check(resp, what))
  ref <- json(httr2::req_perform(gh_req(paste0("repos/", repo, "/git/ref/heads/", br))), "Leer la rama")
  base_commit <- ref$object$sha
  base_tree <- json(httr2::req_perform(gh_req(paste0("repos/", repo, "/git/commits/", base_commit))),
                    "Leer el commit")$tree$sha
  entries <- lapply(names(files), function(path) {
    blob <- gh_req(paste0("repos/", repo, "/git/blobs")) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(list(content = jsonlite::base64_enc(files[[path]]), encoding = "base64")) |>
      httr2::req_perform() |>
      json(paste("Subir", path))
    list(path = path, mode = "100644", type = "blob", sha = blob$sha)
  })
  tree <- gh_req(paste0("repos/", repo, "/git/trees")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(base_tree = base_tree, tree = entries)) |>
    httr2::req_perform() |> json("Crear el árbol")
  commit <- gh_req(paste0("repos/", repo, "/git/commits")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(message = paste(message, "[skip ci]"), tree = tree$sha, parents = list(base_commit))) |>
    httr2::req_perform() |> json("Crear el commit")
  gh_req(paste0("repos/", repo, "/git/refs/heads/", br)) |>
    httr2::req_method("PATCH") |>
    httr2::req_body_json(list(sha = commit$sha, force = FALSE)) |>
    httr2::req_perform() |> gh_check("Actualizar la rama")
  invisible(commit$sha)
}
