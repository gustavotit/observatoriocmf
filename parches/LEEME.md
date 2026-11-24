# Parches de módulos

Cada carpeta `parches/<id_modulo>/` define un módulo completo y se aplica con
`notebooks/parche_modulo.ipynb` (o `python pipeline/definicion_modulo.py parches/<id_modulo>`):

| Archivo | Obligatorio | Contenido |
|---|---|---|
| `modulo.yml` | sí | bloque del módulo, igual que en `config/modules.yml`, empezando en `- id: <id_modulo>` |
| `diagnosticos.csv` | sí | códigos CIE-10: `codigo, descripcion, subgrupo, incluir, requiere_proc, excluir_si_proc, nota` |
| `procedimientos.csv` | no | CIE-9-MC: `codigo, descripcion, grupo, incluir, nota` |
| `controles.csv` | no | cifras publicadas: `fuente, metrica, subgrupo, anio_desde, anio_hasta, valor, tolerancia, referencia` |

El notebook reemplaza solo ese módulo (definición, diccionarios y controles), reclasifica sus casos con
`pipeline/parche_modulo.R` en unos minutos, muestra los controles y abre la app. `_plantilla/` es un ejemplo.
Un estudio económico se agrega aparte, en `config/estudios_economicos.yml` (ver `notebooks/importar_estudio_economico.ipynb`).
