# Observatorio CMF Chile

Plataforma abierta de epidemiología craneomaxilofacial basada en los egresos hospitalarios del DEIS y en las bases GRD públicas. Reproduce las definiciones de caso, la armonización geográfica, los denominadores y la valorización de los artículos del proyecto. Incluye:

- **Pipeline reproducible:** descarga, armoniza, clasifica por módulo, subgrupo y mecanismo, valoriza, valida y publica tablas agregadas.
- **Diccionarios editables:** códigos CIE-10 por módulo y subgrupo con descripción y criterio de inclusión, procedimientos CIE-9-MC, causas externas, comunas, Servicios de Salud y precios GRD.
- **Paquete R `observatoriocmf`:** consultas, tasas (crudas, ajustadas por edad con IC de Fay-Feuer, por nacidos vivos y por universo de referencia), figuras de revista, citas, párrafos de métodos y código reproducible.
- **App pública (Shiny)** y **panel de administración** para cargar datos y publicar sin tocar el código.
- **GitHub Actions:** reconstruye, redespliega y crea releases citables (DOI en Zenodo).

## Cómo fluye una actualización

El pipeline tiene dos fases:

| Fase | Qué hace | Dónde corre | Cuándo |
|---|---|---|---|
| 1. Clasificación | Lee DEIS y GRD, armoniza y clasifica por módulo, subgrupo, mecanismo, edad y RISS. Guarda en `data-casos/` solo los egresos de algún módulo (sin identificadores ni fechas) y los universos y totales ya agregados | Tu computador (`notebooks/actualizacion_anual.ipynb`) | Una vez al año, o si cambia una definición de caso |
| 2. Cálculo | Valor GRD, IPC, productividad, población INE, nacidos vivos, cubo, tasas, controles | Tu computador o GitHub Actions | Cada vez que cambia un parámetro |

```
Una vez al año (tu computador)
  notebook ──▶ fase 1 + fase 2 ──▶ zip (data-casos/ + inst/extdata/) ──▶ panel: "Actualización anual"
                                                                        ──▶ "Solo redesplegar" + versión minor
Cuando cambia un parámetro (solo el panel)
  "Cargar datos" (IPC, productividad, INE, nacidos vivos, MAI, oferta) o "Diccionarios" (precios GRD)
  ──▶ "Recalcular con los parámetros vigentes" ──▶ GitHub Actions corre la fase 2 sobre data-casos/ ──▶ app
```

- Los parámetros de los artículos (población INE base 2017, IPC del BCCh con base 2024, productividad 2019-2024,
  estructura de precios GRD 2024) quedan en `data-small/` y se reemplazan desde el panel. Si dos versiones cubren
  el mismo año, manda la más reciente. Los años de egresos sin parámetro usan el último disponible.
- `data-casos/` contiene registros individuales derivados de bases públicas y anonimizadas (DEIS y GRD), con
  menos variables que el original: sin CIP, sin fechas y sin fecha de nacimiento. La app solo publica agregados
  con supresión de celdas pequeñas.
- Si no hay bases ni casos clasificados, el pipeline se detiene y conserva los datos publicados.
- Versión: *minor* para la actualización anual o un módulo nuevo, *major* si cambia una definición de caso,
  *patch* para correcciones o parámetros.

## Cómo se cita

- **Un solo DOI:** el de concepto de Zenodo. Resuelve siempre a la última versión y suma las citas de todas; la
  app y `cmf_cite()` nunca muestran los DOI por versión. La versión de datos y la fecha de consulta identifican
  los números exactos.
- **Artículo de referencia:** cuando el letter esté publicado se registra en el panel (*Parámetros*) y pasa a
  ser la cita principal: artículo + DOI de concepto.
- **Controles:** solo cubren los años de cada artículo, así que sumar años no los altera. Si el DEIS revisa un
  archivo antiguo o el INE cambia las proyecciones (p. ej. con la base del Censo 2024), el control lo muestra y
  se explica en la nota de versión.

## Fuentes que espera el pipeline

| Fuente (`config/sources.yml`) | Archivo | Cómo se carga |
|---|---|---|
| `egresos_deis` | `EGRE_DATOS_ABIERTOS_{año}.csv` (separador `;`) | URL oficial del DEIS, una versión por año |
| `grd` | `GRD_PUBLICO_{año}.txt` (separador `\|`) | URL oficial, una versión por año |
| `poblacion_ine` | `estimaciones-y-proyecciones-2002-2035-comuna-y-área-urbana-y-rural.xlsx` | Subir archivo |
| `nacimientos_region` | CSV `anio, region, nacidos_vivos` (DEIS; INE provisional en años recientes; sin región = cifra nacional) | Incluido: DEIS 2001-2023 por región e INE provisional 2024-2025 |
| `ipc` | `Indicador.xls` del Banco Central (variación % mensual) | Subir archivo |
| `aranceles_mai` | Planilla MAI (`grupo, subgrupo, presta, glosa, precio`) | Subir archivo (opcional) |
| `productividad` | `parametros_productividad_*.csv` (ESI: ingreso diario y tasa de ocupación) | Subir archivo (opcional) |
| `oferta_servicios` | CSV por Servicio de Salud (`servicio_salud_id` + indicadores) | Subir archivo |
| `catalogo_cie10`, `catalogo_cie9mc` | Catálogos oficiales con `codigo, descripcion` | Opcional; sus descripciones reemplazan a las del diccionario |

Los nombres de columnas aceptados están en `config/column_maps.yml`. Si una base cambia un nombre, se agrega ahí (o desde el panel → *Configuración avanzada*); la validación avisa qué columna faltó.

## Módulos y definiciones (`config/modules.yml` + `dictionaries/cie10_diagnosticos.csv`)

| Módulo | Definición por defecto | Fuentes | Denominadores |
|---|---|---|---|
| Trauma CMF | Por egresos y por habitantes en ambas fuentes. **DEIS (artículo publicado, CMTR 2026;19(3):32):** tejidos blandos (S00.1, S00.3-S00.5, S01.2-S01.5) + tejidos duros (S02.2-S02.4, S02.6-S02.9 y S02.0) como diagnóstico principal. **GRD (2019-2025):** blandos + fracturas faciales + otras lesiones faciales (S03.0, S03.1, S03.4, S05.4, S07.0) en cualquier posición, solo hospitalización. | DEIS, GRD | Por 1.000 egresos por trauma (S00-S99, T00-T14; GRD además T20-T32), por 100.000 egresos totales y por 100.000 habitantes (DEIS cruda; GRD cruda y ajustada por edad) |
| Infecciones | 11 códigos en 6 dominios: K10.2, K12.2, L02.0, L02.1, L03.2, K11.2, K11.3, H05.0, J36, J39.0, J39.1 (A46 excluido); grupos 0-14, 15-64 y 65+ | DEIS, GRD | Por 100.000 habitantes (cruda; DEIS también ajustada a la población chilena 2002-2025 en los 3 grupos, como la sensibilidad del artículo; GRD ajustada OMS) y por 100.000 egresos totales |
| Fisuras | Q36-Q37 como diagnóstico principal (Q35 como sensibilidad). DEIS (artículo): menores de 1 año. GRD (sin publicar): 0-4 años, porque el GRD 2025 solo trae tramos quinquenales y la palatoplastía suele ser posterior al primer año | DEIS, GRD | DEIS: por 1.000 nacidos vivos (INE 0 años calibrado a nacidos vivos regionales). GRD: por 100.000 habitantes de 0-4 años, conteos y valor GRD |
| Craneosinostosis | DEIS: Q75.0 principal en 0-9 años. GRD (notebook v35): Q75.0 o Q75.1 principal, Q87.0 principal con cirugía craneal y Q75.0 secundario con cirugía craneal | DEIS, GRD | DEIS: por 10.000 nacidos vivos (serie `craneosinostosis_series`) y población de 0-9 años; GRD: conteos y valor GRD por episodio |
| Capacidad instalada y lista de espera | Tabla S1 del manuscrito de dotación y listas de espera: residentes INE 2026, especialistas y FTE CMF (44 h), consultas nuevas y cirugías en espera (84 códigos) al 30-09-2026, por 100.000 y mediana de días | RNPI (Transparencia) y Visor Ciudadano LE | Ya agregada |
| Patología quirúrgica maxilar (disponible; 25 de 25 controles coinciden) | Diagnóstico principal. Dientes incluidos e impactados (K01.0-K01.1), quistes de los maxilares (K04.8, K09.0, K09.1, K09.2, K09.9), tumores benignos (D16.5, K10.1) y D16.4 como subgrupo propio. **GRD: hospitalización + CMA** (la CMA concentra ~80% en 2022-2023) y D16.4 excluido con cirugía craneal (01.x, 02.x; columna `excluir_si_proc`). Sensibilidades: K00.1, K09.8, K10.0/K10.9. K10.8 excluido (en GRD se usa con cirugía de ATM y osteoplastías). Agrupación "Lesiones de los maxilares" (quistes + tumores) por la reclasificación OMS del queratoquiste (2005/2017). Grupos 0-9/10-19/20-29/30-49/50+ | DEIS, GRD | Por 100.000 habitantes (DEIS cruda y ajustada a Chile; GRD cruda y ajustada OMS) y por 100.000 egresos de la misma actividad |
| Cáncer oral, anomalías dentofaciales, ATM | Semilla para revisar | DEIS, GRD | Población INE |

Los subgrupos que no forman la definición por defecto quedan seleccionables en la app y en `cmf_query(subgrupo = ...)`. Así se reproduce cualquier versión de las definiciones de los artículos. Por ejemplo, el CMFT amplio con S02.0/S02.1 o el grupo "duro" de `final_final.R`, que incluía S02.0.

## Decisiones heredadas de los artículos

- **Geografía:** la región y el Servicio de Salud son de residencia y se reconstruyen desde `COMUNA_RESIDENCIA`, que acepta código o nombre.
  - Considera los códigos anteriores a la creación de Arica y Parinacota (2007), Los Ríos (2007) y Ñuble (2018).
  - Si la comuna no se resuelve, se usa la región registrada, salvo en regiones madre antes de su división (01/10 hasta 2007 y 08 hasta 2018).
  - La correspondencia comuna-Servicio de Salud proviene de `chilemapas` (Apache 2.0).
- **Edad:**
  - DEIS usa `GRUPO_EDAD`, que viene en tramos decenales en la mayoría de los años ("menor de un año", "1 a 9", "10 a 19"…), quinquenales con tramos neonatales en 2012, 2021 y 2024, y con caracteres dañados en 2025. Como en los scripts de los artículos, cada egreso entra al grupo que contiene el límite inferior del tramo (`age_min`). Los grupos son los de cada artículo: trauma 0-9/10-18/19-44/45-59/60+, infecciones 0-14/15-64/65+.
  - Por eso el DEIS **no** se estandariza por edad con quinquenios OMS (`tasas: ajuste_edad_fuentes: [grd]`). Una tasa poblacional por grupo solo se calcula si el grupo calza con los tramos INE; si no (p. ej. 10-18), la app lo explica y ofrece la tasa por universo o conteos.
  - GRD usa la edad exacta entre la fecha de nacimiento y la de ingreso. GRD 2025 trae `TRAMO_EDAD` en vez de fecha de nacimiento: sirve para los grupos amplios, pero la tasa ajustada de ese año se omite (menos del 95% de los egresos con grupo quinquenal).
- **Mecanismo:**
  - DEIS (artículo publicado, `final_final.R`): `DIAG2`, seis grupos, todos los rangos V01-Y99 (incluidas complicaciones Y40-Y84 en "eventos no accidentales"); sin código V-Y clasificable → "No especificada/otras".
  - GRD (notebook 2019-2025): primer código etiológico V-Y en cualquier posición secundaria, siete grupos; sin código → "Desconocido/otro".
- **Agrupaciones:** las series publicadas que suman subgrupos (DEIS trauma: tejidos duros = fracturas faciales + S02.0; tejidos blandos) se consultan con `cmf_query(by = "agrupacion")`.
- **Estada:** media, mediana (IQR), % con 5 días o más (artículo DEIS de trauma) y % con 7 o más. Las estadas mayores a 365 días quedan sin dato solo para la estada (artículo de infecciones).
- **Controles contra las publicaciones:** `dictionaries/controles_publicaciones.csv` guarda cifras publicadas (totales y tasas de infecciones, fisuras, craneosinostosis y trauma DEIS). Cada corrida las recalcula con el código del paquete y deja el resultado en `controles.json`, visible en la app (*Datos y métodos*). Los conteos del diagnóstico local cuadran exactos: infecciones 153.408 y cada dominio, fisuras 2002-2025 y craneosinostosis año a año.
- **Mortalidad:**
  - DEIS: `CONDICION_EGRESO`.
  - GRD: `TIPOALTA` por raíces de texto. Lo no clasificable queda como NA, no como vivo.
- **Actividad (GRD):** `TIPO_ACTIVIDAD` → Hospitalización | CMA (dimensión `actividad` del cubo y de `cmf_query()`). Cada módulo declara en `actividades` cuáles cuenta; por defecto solo hospitalización, como en los artículos de trauma, infecciones y craneosinostosis, cuyos resultados no cambian. El DEIS no registra la CMA de la red pública (2023: el DEIS público equivale al 22% de hospitalización + CMA GRD de estos códigos), así que su serie es de hospitalizados. Los totales de egresos se guardan por actividad y el denominador "por 100.000 egresos" usa la misma actividad que el numerador.
- **Exclusión por procedimiento:** `excluir_si_proc` en `cie10_diagnosticos.csv` (prefijos CIE-9-MC separados por `;`) descarta el egreso si algún procedimiento empieza así. Solo GRD; en DEIS se declara como limitación.
- **Pabellón (GRD):** `USOSPABELLON`. Si viene vacío en todo el año (2020, 2025), se usa `FECHAINTERV1` como respaldo.
- **Valor GRD:** peso × precio base del hospital según su clúster, con las resoluciones 2020-2025 (`grd_precios_base.csv`, `grd_hospital_grupo.csv`).
  - Por defecto se usa la estructura de precios 2024, expresada en pesos de octubre de 2025 (`costos$referencia_precios`):
    factor IPC = índice de octubre de 2025 / índice promedio de 2024 = 1,0502, igual que el notebook de carga económica del
    trauma v2.20. Así los montos del explorador y de los estudios importados quedan en la misma moneda.
  - Los hospitales fuera del listado reciben el precio único desde 2023.
  - El escenario nominal (precio del propio año) se lleva a la misma referencia con el IPC del BCCh: variaciones mensuales
    encadenadas desde 2018, índice del mes de referencia sobre el promedio de cada año.
- **Tasas:** GRD, directamente estandarizadas con la población OMS 2000-2025 (80+ agregado) e IC gamma de Fay-Feuer. Las tasas crudas, por nacidos vivos, por universo y por egresos totales llevan IC exactos de Poisson.
- **Severidad (trauma GRD):** RISS derivado de CIE-10 con ICDPIC-R 1.0.1 (tabla base, método NIS, `calc_method = 1`), igual que el notebook 2019-2025: claves de 4 caracteres, todas las posiciones diagnósticas, bandas 0-3/4-8/9-15/16-24/25+ y trauma mayor = RISS ≥ 16, más la versión que excluye las lesiones del módulo. La réplica coincide al 100% con `icdpicr::cat_trauma()` en una prueba de 3.000 egresos.
- **Unidad de análisis:** el egreso. La identificación de pacientes únicos y las primeras cirugías quedan en los notebooks de cada artículo.
- **Codificación de archivos:** UTF-8, Latin-1 (GRD 2024) o UTF-16 (GRD 2022 y 2023, que se convierten a UTF-8 una vez).

## Estructura

| Carpeta | Contenido |
|---|---|
| `config/` | Fuentes, mapeo de columnas, módulos y parámetros. |
| `dictionaries/` | Códigos CIE-10/CIE-9-MC, causas externas, comunas (actuales, antiguas, alias), Servicios de Salud, regiones, población estándar, precios y clústeres GRD, crosswalk MAI. |
| `data-small/` | Tablas pequeñas subidas desde el panel. |
| `pipeline/` | `run_pipeline.R`, preparación de la app, versionado y datos sintéticos de prueba (`demo_data.R`). |
| `R/` | Paquete: consultas, tasas, figuras, citas. |
| `inst/extdata/` | Salidas publicadas: `cubo`, `universo`, `poblacion`, `poblacion_edad0`, `estada`, `conteos_comuna`, `oferta_servicios`, diccionarios, `manifest.json`, `validacion.json`. |
| `inst/app/`, `admin/` | App pública y panel de administración. |
| `notebooks/` | Actualización anual, parche de módulo e importación de estudios económicos (corren en tu computador). |
| `parches/` | Definiciones de módulos para aplicar con el notebook de parche (`_plantilla/` de ejemplo). |

## Puesta en marcha (una vez)

1. **Repositorio:** sube esta carpeta a un repositorio público de GitHub en tu cuenta.
2. **Zenodo:** inicia sesión con GitHub y activa el repositorio. Tras el primer release, copia el DOI de concepto en el panel → *Parámetros*.
3. **Hosting:** shinyapps.io (secrets `SHINYAPPS_NAME`, `SHINYAPPS_TOKEN`, `SHINYAPPS_SECRET`) o un servidor propio con dominio `.cl`.
4. **Token para el panel:** crea un token fine-grained solo para este repositorio, con permisos *Contents* y *Actions* de lectura y escritura. Guárdalo como secret `ADMIN_GH_TOKEN`, junto con `ADMIN_USER` y `ADMIN_PASSWORD`.
5. **Primera ejecución:** *Actions* → *Construir datos y desplegar* → *Run workflow*.

## Uso local

```r
# Probar con datos sintéticos (en una copia del repositorio):
#   Rscript pipeline/demo_data.R registrar
#   Rscript pipeline/run_pipeline.R
shiny::runApp("inst/app")
```

```r
library(observatoriocmf)
cmf_subgroups("trauma")
q <- cmf_query("trauma", years = 2001:2024, by = "anio", metric = "universo")      # DEIS, como el artículo
q_tejido <- cmf_query("trauma", years = 2001:2024, by = c("anio", "agrupacion"), metric = "universo")
q_grd <- cmf_query("trauma", fuente = "grd", by = "anio", metric = "ajustada")    # GRD, ajustada por edad
q_amplio <- cmf_query("trauma", subgrupo = "todos", by = c("anio", "subgrupo"), metric = "conteo")
q_fisuras <- cmf_query("fisuras", by = "region", metric = "nacidos_vivos")
p <- cmf_plot_trend(q, its_break = 2020); cmf_save(p, "figura1.tiff", width = "1 col")
cmf_age_groups("trauma", "egresos_deis")
cmf_cite(q); cmf_methods(q); cat(cmf_code(q, "trend", its_break = 2020))
```

## Agregar o corregir un módulo (parches)

1. Copia `parches/_plantilla/` como `parches/<id_modulo>/` y edita `modulo.yml` (el bloque del módulo) y
   `diagnosticos.csv`; opcionalmente `procedimientos.csv` y `controles.csv` con las cifras del artículo.
2. Corre `notebooks/parche_modulo.ipynb`: aplica la definición, reclasifica solo ese módulo
   (`pipeline/parche_modulo.R`, unos minutos), muestra los controles, abre la app y arma el zip del panel y la
   carpeta para GitHub.
3. Cuando los controles coincidan, `estado: disponible` y publica.

## Biblioteca

La pestaña Biblioteca reúne la epidemiología CMF de Chile: artículos que reproduce el observatorio, series
hospitalarias y estudios nacionales o regionales, con filtros y descarga RIS/CSV. Se alimenta de
`config/publicaciones.yml`; desde el panel (pestaña Biblioteca) se agregan referencias con autocompletado por DOI y se
quitan, y basta "Solo redesplegar". En R: `cmf_biblioteca()`.

## Pregunta al observatorio

Bajo la presentación de la portada hay un buscador que responde preguntas en español ("¿cuántos egresos por fracturas
faciales hubo en Biobío en 2023?", "¿qué región tiene la tasa de fisuras más alta?"). No usa un modelo de lenguaje: un
intérprete determinista (`R/preguntas.R`) reconoce el módulo, el subgrupo, la fuente, el territorio, los años, el sexo,
la edad, la medida y la desagregación; llama a `cmf_query()` o `cmf_query_eco()` y redacta la respuesta con
plantillas, con la cita de la consulta. Las cifras son las del explorador, con la misma supresión de celdas pequeñas;
si la pregunta no calza con los datos, lo dice y sugiere otras. Mientras se escribe, el buscador autocompleta con
`cmf_preguntas_sugeridas()` (filtrado en el navegador). En R: `cmf_preguntar("...")`.

## Dimensión económica y estudios

Cada módulo con GRD tiene dos dimensiones en el explorador: **Epidemiología** (tasas, territorio, composición) y
**Economía** (valor GRD total, medio y por 100.000 habitantes, días cama y estada; `cmf_query_eco()`). Los estudios
económicos de los artículos (p. ej. carga del trauma facial 2019-2025: estimandos CEM, escenarios de pérdida de
productividad, carga por Servicio de Salud) se importan agregados con `notebooks/importar_estudio_economico.ipynb`
según `config/estudios_economicos.yml`, y se consultan con `cmf_estudios()`, `cmf_estudio_tabla()`,
`cmf_plot_escenarios()` y `cmf_estudio_territorio()`.

## Licencias y atribución

Código MIT; datos agregados y diccionarios CC BY 4.0 (`LICENSE.md`). La correspondencia comuna-Servicio de Salud deriva de `chilemapas` (Apache 2.0). La tabla `icdpic_i10base_map_roc.csv` proviene de `icdpicr` 1.0.1 (D. Clark y A. Black, licencia MIT), sin modificar. Los datos de origen pertenecen a DEIS-MINSAL, FONASA, INE y Banco Central; el observatorio no constituye estadística oficial.
