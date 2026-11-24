"""Aplica la definición de un módulo desde parches/<modulo>/ al repositorio (o a la copia de trabajo).

    python pipeline/definicion_modulo.py parches/cancer_oral [carpeta_destino]

La carpeta del parche trae:
  modulo.yml        el bloque del módulo tal como va en config/modules.yml, empezando en "- id: <modulo>"
  diagnosticos.csv  códigos CIE-10 del módulo (mismas columnas que dictionaries/cie10_diagnosticos.csv;
                    la columna modulo puede faltar)
  procedimientos.csv  (opcional) CIE-9-MC, columnas de dictionaries/cie9mc_procedimientos.csv
  controles.csv       (opcional) cifras publicadas para validar, columnas de dictionaries/controles_publicaciones.csv

Reemplaza SOLO el bloque y las filas de ese módulo; el resto de los módulos no cambia. Deja un respaldo
config/modules.yml.bak y dictionaries/*.bak. Solo usa la biblioteca estándar de Python.
"""
import csv, io, re, shutil, sys
from pathlib import Path

TABLAS = {"diagnosticos.csv": "cie10_diagnosticos.csv", "procedimientos.csv": "cie9mc_procedimientos.csv",
          "controles.csv": "controles_publicaciones.csv"}


def leer_csv(p):
    with open(p, encoding="utf-8-sig", newline="") as f:
        r = csv.DictReader(f)
        return r.fieldnames, list(r)


def bloque_modulo(texto):
    m = re.search(r"(?m)^-\s*id:\s*([A-Za-z0-9_]+)\s*$", texto)
    if not m:
        raise ValueError("modulo.yml debe empezar con una línea '- id: <modulo>'")
    return m.group(1)


def aplicar(parche, destino):
    parche, destino = Path(parche), Path(destino)
    texto = (parche / "modulo.yml").read_text(encoding="utf-8").replace("\t", "  ").rstrip() + "\n"
    texto = "\n".join(l for l in texto.splitlines() if not l.lstrip().startswith("#") or l.startswith("  "))
    mid = bloque_modulo(texto)
    nuevo = "".join("  " + l + "\n" if l.strip() else "\n" for l in texto.splitlines())

    yml = destino / "config" / "modules.yml"
    s = yml.read_text(encoding="utf-8")
    shutil.copy(yml, yml.with_suffix(".yml.bak"))
    patron = re.compile(rf"(?ms)^  - id: {re.escape(mid)}\s*\n.*?(?=^  - id: |\Z)")
    if patron.search(s):
        s = patron.sub(lambda _: nuevo.rstrip("\n") + "\n\n", s, count=1)
        accion = "reemplazado"
    else:
        s = s.rstrip("\n") + "\n\n" + nuevo
        accion = "agregado"
    yml.write_text(s, encoding="utf-8")
    print(f"  config/modules.yml: módulo {mid} {accion}")

    for origen, dic in TABLAS.items():
        po = parche / origen
        if not po.exists():
            continue
        pd_ = destino / "dictionaries" / dic
        campos, filas = leer_csv(pd_)
        _, nuevas = leer_csv(po)
        for r in nuevas:
            r["modulo"] = r.get("modulo") or mid
            if r["modulo"] != mid:
                raise ValueError(f"{origen}: hay filas de otro módulo ({r['modulo']})")
        faltan = [c for c in campos if c not in (nuevas[0].keys() if nuevas else campos)]
        if faltan and nuevas:
            raise ValueError(f"{origen}: faltan las columnas {', '.join(faltan)}")
        shutil.copy(pd_, pd_.with_suffix(".csv.bak"))
        resto = [r for r in filas if r.get("modulo") != mid]
        buf = io.StringIO()
        w = csv.DictWriter(buf, fieldnames=campos, extrasaction="ignore", lineterminator="\n")
        w.writeheader(); w.writerows(resto + nuevas)
        pd_.write_text(buf.getvalue(), encoding="utf-8")
        print(f"  dictionaries/{dic}: {len(nuevas)} filas de {mid} (antes {len(filas) - len(resto)})")
    # La app lee la copia publicada de modules.yml.
    ext = destino / "inst" / "extdata"
    if ext.exists():
        shutil.copy(yml, ext / "modules.yml")
    return mid


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    aplicar(sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else ".")
