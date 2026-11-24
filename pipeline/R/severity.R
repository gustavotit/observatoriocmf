# Severidad anatómica derivada de CIE-10: ICDPIC-R 1.0.1 (icd10 = "base", calc_method = 1) ------------
#
# Réplica de icdpic_scores() del notebook de trauma GRD 2019-2025, que a su vez replica cat_trauma() de
# icdpicr. Tabla i10base_map_roc de icdpicr 1.0.1 (MIT, D. Clark y A. Black), sin modificar.
# riss es una estimación derivada de CIE, no un ISS clínico. El RISS total incluye la lesión del módulo;
# riss_excl recalcula sin los códigos principales del módulo (severidad de las lesiones asociadas).

ISS_REGIONES <- c("Head/Neck", "Face", "Chest", "Abdomen", "Extremities", "General")

load_icdpic <- function(cfg) {
  sv <- cfg$params$severidad %||% list()
  f <- file.path(cfg$root, "dictionaries", sv$tabla %||% "icdpic_i10base_map_roc.csv")
  if (!file.exists(f)) return(NULL)
  if (!is.null(sv$sha256) && nzchar(sv$sha256)) {
    h <- digest::digest(file = f, algo = "sha256")
    if (!identical(tolower(h), tolower(sv$sha256))) stop("La tabla ICDPIC no coincide con el sha256 registrado")
  }
  t <- data.table::fread(f, colClasses = list(character = "dx"))
  m <- sv$metodo %||% "NIS"
  tab <- data.table::data.table(code4 = t$dx, sev = as.numeric(t[[paste0(m, "_severity")]]),
                                issbr = t[[paste0(m, "_issbr")]])
  data.table::setattr(tab, "metodo", paste0("roc_max_", m))
  tab
}

# dx_list: lista de vectores de códigos normalizados (una posición por elemento), ya subconjuntos de los casos.
# excluir: códigos (prefijos) que no se puntúan. Devuelve data.table(riss, maxais) en el orden de los casos.
icdpic_riss <- function(dx_list, tab, excluir = NULL) {
  n <- length(dx_list[[1]])
  if (!n) return(data.table::data.table(riss = numeric(), maxais = numeric()))
  long <- data.table::rbindlist(lapply(seq_along(dx_list), function(p) {
    x <- dx_list[[p]]
    ok <- which(!is.na(x))
    data.table::data.table(row = ok, code = x[ok])
  }))
  if (length(excluir)) long <- long[is.na(match_prefix(code, excluir))]
  long[, code4 := substr(code, 1, 4)]
  inj <- tab[long, on = "code4", nomatch = NULL]
  inj <- inj[!is.na(sev) & sev != 9]
  reg <- inj[, .(mx = max(sev)), by = .(row, issbr)]
  data.table::setorder(reg, row, -mx)
  por_caso <- reg[, .(maxais = max(mx), riss = sum(utils::head(mx, 3)^2)), by = row]
  out <- data.table::data.table(row = seq_len(n), riss = 0, maxais = 0)
  out[por_caso, on = "row", `:=`(riss = i.riss, maxais = i.maxais)]
  out[maxais == 6, riss := 75]
  out[, row := NULL]
  out[]
}

# Bandas del notebook (RISS_CUTS / RISS_LABELS) y umbral de trauma mayor (RISS ≥ 16).
RISS_BANDAS <- list(c(0, 3), c(4, 8), c(9, 15), c(16, 24), c(25, Inf))
