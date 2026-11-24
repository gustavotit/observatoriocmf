"""Utilidades de los notebooks locales del observatorio (solo biblioteca estándar de Python).

Las usan notebooks/parche_modulo.ipynb, notebooks/importar_estudio_economico.ipynb y el aplicar_parche.py de
cada entrega. Rutas por defecto: las de este computador; cámbialas en el notebook si hace falta.
"""
import datetime, json, os, shutil, subprocess, time, webbrowser, zipfile
from pathlib import Path

DESCARGAS = Path.home() / "Downloads"
CANDIDATOS_REPO = [DESCARGAS / "observatorio CMF" / "observatoriocmf", DESCARGAS / "observatoriocmf" / "observatoriocmf",
                   DESCARGAS / "observatoriocmf"]
# Lo que nunca va al repositorio publicado: bases crudas, caché, rutas locales, respaldos y logs.
EXCLUIR_GITHUB = {"data-raw", "data-cache", "config_local.json", "inst/app/R", "inst/app/data", "rsconnect", ".Rhistory",
                  ".RData", ".Rproj.user", ".Renviron", "admin/.Renviron", ".env", "__pycache__", ".ipynb_checkpoints"}


def es_repo(p):
    return (Path(p) / "pipeline" / "actualizar_local.R").exists() and (Path(p) / "DESCRIPTION").exists()


def encontrar_repo(desde=None):
    cands = ([Path(desde)] if desde else []) + [Path.cwd().parent, Path.cwd()] + CANDIDATOS_REPO
    for c in cands:
        if es_repo(c) and c.name != "observatorio_corrida_anual":
            return c.resolve()
    raise FileNotFoundError("No encontré el repositorio observatoriocmf: define REPO_DIR")


def rscript(ruta=None):
    if ruta and Path(ruta).exists():
        return str(ruta)
    w = shutil.which("Rscript")
    if w:
        return w
    for base in [Path(r"C:/Program Files/R"), Path.home() / "AppData/Local/Programs/R"]:
        c = sorted(base.glob("R-*/bin/Rscript.exe")) if base.exists() else []
        if c:
            return str(c[-1])
    raise FileNotFoundError("No encontré Rscript: define RSCRIPT con la ruta a Rscript.exe")


def entorno():
    # Windows no reconoce LANG=C.UTF-8 (R leería mal los acentos): se deja el de R, UTF-8 desde R 4.2.
    env = dict(os.environ)
    if os.name == "nt":
        for k in ("LANG", "LC_ALL", "LC_CTYPE"):
            env.pop(k, None)
    else:
        env.update(LANG="C.UTF-8", LC_ALL="C.UTF-8")
    return env


def correr_r(args, cwd, log=None, r=None, mostrar=60):
    """Corre Rscript con args en cwd; guarda el log y muestra las últimas líneas. Falla si R falla."""
    p = subprocess.run([rscript(r)] + list(args), cwd=cwd, capture_output=True, text=True, encoding="utf-8",
                       errors="replace", env=entorno())
    salida = (p.stdout or "") + "\n" + (p.stderr or "")
    if log:
        Path(cwd, log).write_text(salida, encoding="utf-8")
    lineas = [l for l in salida.splitlines() if l.strip() and "Procesando" not in l]
    print("\n".join(lineas[-mostrar:]))
    if p.returncode != 0:
        raise RuntimeError(f"R terminó con error (revisa {log or 'la salida de arriba'})")
    return salida


def copiar(origen, destino):
    origen, destino = Path(origen), Path(destino)
    if origen.is_dir():
        shutil.copytree(origen, destino, dirs_exist_ok=True)
    elif origen.exists():
        destino.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(origen, destino)


def sincronizar_codigo(repo, trabajo):
    """Lleva el código y la configuración del repositorio a la copia de trabajo, sin tocar sus datos
    (inst/extdata, data-casos, data-cache) ni config_local.json."""
    repo, trabajo = Path(repo), Path(trabajo)
    for sub in ["R", "pipeline", "dictionaries", "data-small", "notebooks", "parches", "admin", "tests", "inst/app/www"]:
        copiar(repo / sub, trabajo / sub)
    copiar(repo / "inst" / "app" / "app.R", trabajo / "inst" / "app" / "app.R")
    # inst/app/R y inst/app/data son del despliegue: si quedaran, la app local leería datos viejos.
    for viejo in ["inst/app/R", "inst/app/data"]:
        if (trabajo / viejo).exists():
            shutil.rmtree(trabajo / viejo)
    for f in (repo / "config").glob("*"):
        copiar(f, trabajo / "config" / f.name)
    for f in ["DESCRIPTION", "NAMESPACE", "CITATION.cff", "NEWS.md", "README.md", "inst/CITATION"]:
        copiar(repo / f, trabajo / f)
    for yml in ["modules.yml", "publicaciones.yml", "estudios_economicos.yml"]:
        if (trabajo / "config" / yml).exists() and (trabajo / "inst" / "extdata").exists():
            shutil.copy2(trabajo / "config" / yml, trabajo / "inst" / "extdata" / yml)


def publicar_en_repo(trabajo, repo):
    """Copia al repositorio las tablas publicadas y los casos clasificados de la copia de trabajo."""
    for carpeta in ["inst/extdata", "data-casos"]:
        o, d = Path(trabajo) / carpeta, Path(repo) / carpeta
        if o.exists():
            if d.exists():
                shutil.rmtree(d)
            shutil.copytree(o, d)
    print("Repositorio actualizado con inst/extdata y data-casos de la copia de trabajo")


def zip_panel(trabajo, salida=None, nombre="datos_procesados_observatorio"):
    trabajo = Path(trabajo)
    salida = Path(salida or trabajo.parent)
    zp = salida / f"{nombre}_{datetime.date.today():%Y%m%d}.zip"
    with zipfile.ZipFile(zp, "w", zipfile.ZIP_DEFLATED) as z:
        for carpeta in ["inst/extdata", "data-casos"]:
            for f in sorted((trabajo / carpeta).rglob("*")):
                if f.is_file():
                    z.write(f, f.relative_to(trabajo).as_posix())
    print(f"Zip para el panel: {zp} ({zp.stat().st_size / 1e6:.1f} MB)")
    return zp


def abrir_app(carpeta, puerto=8765, r=None):
    app = subprocess.Popen([rscript(r), "-e", f"shiny::runApp('inst/app', port = {puerto}, launch.browser = FALSE)"],
                           cwd=carpeta, env=entorno())
    time.sleep(8)
    webbrowser.open(f"http://127.0.0.1:{puerto}")
    print(f"App en http://127.0.0.1:{puerto} ; para cerrarla: app.terminate()")
    return app


def _excluido(rel):
    rel = rel.replace("\\", "/")
    if rel.startswith("data-casos_respaldo") or rel.startswith("log_") or rel.endswith((".bak", ".pyc")):
        return True
    return any(rel == e or rel.startswith(e + "/") for e in EXCLUIR_GITHUB) or \
        any(parte in EXCLUIR_GITHUB for parte in rel.split("/"))


def carpeta_github(repo, destino=None, con_casos=False):
    """Arma una carpeta limpia para subir a GitHub (arrastrándola en github.com o con GitHub Desktop).

    Incluye código, configuración, diccionarios, data-small y las tablas publicadas (inst/extdata).
    Excluye bases crudas, caché, config_local.json, respaldos, logs y credenciales. Los casos clasificados
    (data-casos: un registro por egreso CMF, sin identificadores ni fechas, pero con edad, comuna y hospital)
    solo van con con_casos=True; sin ellos GitHub puede redesplegar, pero no "Recalcular".
    """
    repo = Path(repo)
    destino = Path(destino or repo.parent / "observatoriocmf_github")
    if destino.exists():
        shutil.rmtree(destino)
    n = 0
    for f in sorted(repo.rglob("*")):
        rel = f.relative_to(repo).as_posix()
        if f.is_dir() or _excluido(rel) or rel.startswith(".git/"):
            continue
        if rel.startswith("data-casos/") and not con_casos:
            continue
        copiar(f, destino / rel)
        n += 1
    # Respaldo: aunque alguien copie la carpeta encima de otra, el .gitignore evita subir lo local.
    gi = destino / ".gitignore"
    extra = "\n# Local (agregado por carpeta_github)\nconfig_local.json\ndata-casos_respaldo*/\nlog_*.txt\n*.bak\n"
    gi.write_text((gi.read_text(encoding="utf-8") if gi.exists() else "") + extra, encoding="utf-8")
    tam = sum(p.stat().st_size for p in destino.rglob("*") if p.is_file()) / 1e6
    print(f"Carpeta para GitHub: {destino} ({n} archivos, {tam:.1f} MB; data-casos {'incluido' if con_casos else 'excluido'})")
    return destino
