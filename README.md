# Los dos extremos

Boric y Kast en la Cámara de Diputados de Chile, y lo que su paso por ella dice sobre la polarización. Usa las estimaciones de B-Call por período del análisis [Siete Cámaras en dos dimensiones](https://alcatruz.github.io/bcall-camara-2002-2026/).

El post está en `index.html` y se publica con GitHub Pages.

## Contenido

| Ruta | Qué es |
|---|---|
| `index.html` | El post, con gráficos interactivos (Plotly) |
| `R/01_base.R` | Lealtad al bloque y al partido, votaciones de bloque contra bloque, Rice, carreras y votos de Boric y Kast |
| `R/02_materias.R` | Materia de cada votación por el título del proyecto, validación contra los descriptores de la BCN y votos contra el bloque por materia |
| `R/03_indicadores.R` | Polarización, Rice, modelo logit de carreras y exportación de `datos/` |
| `datos/trayectorias_boric_kast.csv` | d1, d2 y lealtad de Boric y Kast por período |
| `datos/contra_bloque_materia.csv` | Votos contra el propio bloque por materia, con la mediana de la Cámara |
| `datos/polarizacion.csv` | Votaciones de bloque contra bloque y lealtad mediana por período |
| `datos/rice_partidos.csv` | Índice de Rice por partido y período |
| `datos/carreras.csv` | Diputados que dejaron la Cámara entre 2006 y 2026 y su destino posterior |
| `datos/modelo_carreras.csv` | Coeficientes del modelo logit |
| `datos/validacion_materias.csv` | Acuerdo de la clasificación por título con los descriptores de la BCN |
| `datos/datos.js` | Los datos que lee la página |

## Datos y decisiones

- Votos nominales de opendata.camara.cl, 2002-2026, y estimaciones de B-Call del análisis anterior.
- Cargos posteriores (Senado, Presidencia) desde los datos abiertos de la BCN.
- Las materias se asignan por el título del proyecto. Los diccionarios se armaron mirando una mitad de los proyectos con descriptores de la BCN y se midieron una vez en la otra mitad.

Los scripts se corren desde la raíz de un proyecto local que guarda los datos crudos ya bajados. Esos archivos no están en este repositorio.

## Referencia

Toro-Maureira, S., Reutter, J., Valenzuela, L., Alcatruz, D. y Valenzuela, M. (2025). B-Call: integrating ideological position and voting cohesion in legislative behavior. *Frontiers in Political Science, 7*, 1670089. https://doi.org/10.3389/fpos.2025.1670089
