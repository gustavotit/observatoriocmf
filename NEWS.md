# observatoriocmf (desarrollo)

## Consolidación: epidemiología y economía por ámbito

- Índice de la portada: cada ámbito tiene rectángulos de su color, sin relleno, que abren el explorador directo en
  **Epidemiología** o **Economía**; la fila completa sigue abriendo el explorador del módulo.
- Explorador con dos dimensiones. Economía (solo GRD): valor GRD total, medio por egreso y por 100.000 habitantes,
  días cama y estada media, con tendencia, territorio, composición del valor, tabla, cita, métodos y código
  (`cmf_query_eco()`); cifras al margen con el porcentaje del valor en CMA cuando el módulo la incluye. Epidemiología
  ya no muestra el valor GRD.
- Estudios económicos importados (`config/estudios_economicos.yml`, `pipeline/importar_estudio.R`): carga económica
  del trauma facial GRD 2019-2025 (notebook v2.20), con estimandos CEM, escenarios de productividad con IC 95%,
  serie anual y mapa por Servicio de Salud. Pestaña "Estudio" en Economía.
- Parches simples por módulo: `parches/<modulo>/` + `notebooks/parche_modulo.ipynb` (definición, diccionarios y
  controles de un módulo, reclasificación en minutos, zip del panel y carpeta para GitHub).
- `pipeline/cmf_local.py`: utilidades de los notebooks y `carpeta_github()` (carpeta limpia para subir a GitHub,
  sin bases crudas, caché, rutas locales ni respaldos; `data-casos` opcional).
- Patología quirúrgica maxilar pasa a **disponible** (25 de 25 controles coinciden tras el parche del 14-11-2026).
- El período del explorador se ajusta a los años de la fuente (GRD desde 2019).
- **Biblioteca** (antes "Artículos"): epidemiología CMF de Chile con tres tipos de referencia (reproducidas por el
  observatorio, series hospitalarias, estudios nacionales o regionales), filtros por ámbito, tipo, región y texto,
  y descarga RIS/CSV (`cmf_biblioteca()`, `cmf_biblioteca_ris()`). El panel tiene una pestaña Biblioteca para
  agregar referencias con autocompletado por DOI (Crossref) y quitarlas, sin tocar el YAML.
- **Fisuras con GRD 2019-2025** (sin publicar): 0-4 años, porque el GRD 2025 solo trae tramos quinquenales; tasa
  por 100.000 habitantes de 0-4 años y dimensión Economía. El rango de edad de un módulo puede ser por fuente.
- Portada más sobria (título corto, un párrafo, cifras junto a la lámina); números del índice en el color del ámbito.

- Módulo **Patología quirúrgica maxilar** (en construcción): definición revisada con GRD 2022-2023 reales. El
  análisis del artículo vive en un kit aparte que lee las salidas del observatorio.
- `pipeline/parche_modulo.R`: reclasifica un solo módulo en data-casos/ (filtra por código antes de armonizar)
  y corre la fase 2, sin la actualización anual completa. `CMF_SOLO_FASE2=1` fuerza la fase 2 en run_pipeline.R.
- Nueva dimensión `actividad` (Hospitalización | CMA) en casos, cubo, totales y `cmf_query(actividad = ...)`;
  selector en la app para los módulos que suman CMA. `actividades` por módulo en `modules.yml`; los demás
  módulos siguen siendo solo hospitalización (trauma e infecciones dan las mismas cifras).
- `excluir_si_proc` en el diccionario CIE-10 (p. ej. D16.4 con cirugía craneal). Columna `grupo` en
  `cie9mc_procedimientos.csv` para describir procedimientos.
- Compatibilidad: casos, cubos y totales clasificados antes de esta versión se leen como hospitalización.
  **Requiere rehacer la fase 1** para que el GRD incluya la CMA y el módulo use los subgrupos nuevos.

- Interfaz rediseñada a partir de "Continuum" y "1,374 Days" (Pentagram): titulares sans (Hanken Grotesk) sobre
  texto serif (Source Serif 4), también en rótulos y códigos CIE; firma ocre, esquinas rectas. Cada módulo tiene su
  color JAMA y una pestaña semicircular. Portada con la lámina de Gautier d'Agoty (1745-1746, Wellcome Collection),
  cifras e índice de módulos con enlaces que abren el explorador; cifras del período al margen. Sin rayas ni puntos
  medios en los textos.
- Pestaña "Artículos" con las publicaciones que el observatorio reproduce (config/publicaciones.yml), editable
  desde el panel de administración sin recalcular datos.
- Figuras estilo JAMA: tendencia con línea, IC 95% y tendencia lineal (segmentada con quiebre); barras con IC;
  mapa coroplético por región o Servicio de Salud (recuadro completo de la Región Metropolitana, SS 9-14); "sin dato" al final en gris.
  Pie de figura breve.
- Tablas de publicación (encabezados en castellano, tasa con IC en una columna, solo reglas horizontales); el
  diccionario muestra solo código, descripción, subgrupo y definición.
- Las vistas ya no muestran NA; el párrafo de métodos y la página de Métodos resumen la pérdida de datos.

- Primera corrida real completa: 22 de 25 controles coinciden. Craneosinostosis GRD: Q75.0 secundario con cirugía
  solo cuenta con DX1 craneofacial (Q67/Q75/Q87), como `SECONDARY_DX1_ALLOWED` del notebook v35.
- La caché de cada fuente usa la configuración de módulos resuelta para esa fuente: un cambio solo del GRD ya no
  reprocesa el DEIS.

- Tildes y Ñ: quitado de tildes explícito (no depende del iconv de Windows), reparación de mojibake ("PeÃ±alolÃ©n"),
  recuperación de comunas con carácter dañado ("NU\uFFFDOA") cuando calzan con un solo nombre, y detección de
  codificación en tres tramos del archivo.
- Calidad de datos: pestaña en la app con filas leídas, codificación, % sin región/edad/sexo/estada/condición,
  columnas ausentes y textos reparados o dañados por base y año; cada vista avisa cuántos egresos sin el dato no
  aparecen. Las exclusiones de los artículos se mantienen.
- La caché de cada base considera tamaño y fecha del archivo: si se reemplaza, se vuelve a procesar.

- Primera corrida real: GRD 2022 se cortaba en la línea 120.127 (una fila con un separador de más); ahora se lee
  completo (`fill = Inf`, requiere data.table >= 1.15). GRD 2025 ya no trae COMUNA: región y Servicio de Salud
  desde PROVINCIA (`dictionaries/provincias.csv`, de chilemapas). Productividad con las columnas de los artículos
  (`ingreso_diario_medio`, respaldo nacional por tramo). Lectura de `.csv.gz` sin R.utils.

- Pipeline en dos fases: la clasificación (DEIS/GRD, local y anual) guarda `data-casos/`; el cálculo (valor GRD,
  IPC, productividad, población, nacidos vivos, tasas y controles) corre también en GitHub Actions, así los
  parámetros se actualizan desde el panel y se recalculan sin las bases pesadas. Verificado: la fase 2 sobre
  `data-casos/` reproduce exactamente el cubo de la corrida completa.
- Población INE, productividad y nacidos vivos: si dos versiones cubren el mismo año, manda la más reciente.
- El notebook anual registra en el repositorio los parámetros de los artículos (una vez) y deja el repositorio
  listo para la primera publicación.

- Parámetros anuales (IPC, productividad): los años de egresos sin parámetro usan el último disponible (el de los
  artículos) y queda registrado en el log; la productividad se deflacta con el IPC de su propio año.
- Windows: la corrida local ya no fuerza `LANG=C.UTF-8` (Windows no lo reconoce y R leía mal los acentos); exige
  R >= 4.2 en UTF-8 y falla con mensaje claro si la corrida no genera datos nuevos.

- Flujo anual local: `notebooks/actualizacion_anual.ipynb` + `pipeline/actualizar_local.R` generan un zip de
  agregados que el panel sube en un solo commit; GitHub Actions solo despliega y versiona por defecto, y el
  pipeline se niega a reemplazar datos publicados si no hay bases accesibles.
- Cita con un único DOI (concepto de Zenodo) y, cuando exista, el artículo de referencia.
- El manifiesto ya no publica rutas locales (solo el nombre del archivo).

- Craneosinostosis GRD con la definición "principal" del notebook v35 (Q75.0/Q75.1 principal, Q87.0 con cirugía
  craneal y Q75.0 secundario con cirugía craneal); Q75.0 principal cuadra exacto 2019-2024.
- Nacidos vivos incluidos: DEIS por región 2001-2023 e INE provisional nacional 2024-2025 (los años solo
  nacionales calibran todas las regiones con el factor nacional).
- Controles GRD de trauma y craneosinostosis agregados (21 + 7 cifras).

- Controles automáticos contra las publicaciones (`controles_publicaciones.csv` → `controles.json`, en la app).
- Capacidad instalada y lista de espera (antes "Oferta y acceso") cargado con la Tabla S1 del manuscrito de dotación (29 Servicios, corte 30-09-2026).
- Infecciones DEIS: tasa ajustada a la población chilena 2002-2025 en 3 grupos (`ajustada_chile`); estadas
  > 365 días sin dato para la estada. Servicios de Salud con nombres vigentes (Tarapacá, Arica y Parinacota, Los Ríos).

- Trauma e infecciones: tasas por egresos y por habitantes en ambas fuentes. Craneosinostosis DEIS por 10.000
  nacidos vivos (cuadra exacto con la serie 2002-2025). Fisuras cuadran con la tabla 1 (8.238 egresos, 2002-2025).
- RISS de trauma GRD con ICDPIC-R 1.0.1 (réplica del notebook, validada contra icdpicr): bandas, trauma mayor
  (RISS >= 16) y RISS excluyendo las lesiones del módulo; tabla `severidad` con medianas.
- GRD 2022-2023 en UTF-16 y GRD 2024 en Latin-1; GRD 2025 con TRAMO_EDAD quinquenal (permite tasa ajustada).
- Previsión GRD en texto ("FONASA INSTITUCIONAL - (MAI) B"…) por coincidencia parcial; SISA con FF.AA. y de Orden.

- Métodos alineados con las publicaciones: definición DEIS de trauma con S02.0 y tasa por 1.000 egresos por trauma
  (CMTR 2026), mecanismo DEIS en 6 grupos desde DIAG2, grupos etarios de cada artículo por límite inferior del tramo,
  sin estandarización OMS en el DEIS, estada de 5 días o más, agrupaciones (tejidos duros/blandos) y tasa por
  egresos totales (sensibilidad de infecciones).
- Recodificación con los valores reales del DEIS (previsión 1-5/96/99, pertenencia en plural, sexo 3/9, etiquetas de
  edad neonatales y dañadas de 2025) y año del archivo en GRD.

# observatoriocmf 0.1.0

- Primera versión: pipeline de datos DEIS/GRD, diccionarios con descripciones, app pública y panel de administración.
