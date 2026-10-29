# Los dos extremos

Boric y Kast en la Cámara de Diputados de Chile, y lo que su paso por ella dice sobre la polarización. Usa las estimaciones de B-Call por período del análisis [Siete Cámaras en dos dimensiones](https://alcatruz.github.io/bcall-camara-2002-2026/).

**📖 [Leer el análisis →](https://alcatruz.github.io/boric-kast-camara/)**

El post está en `index.html` y se publica con GitHub Pages.

## Contenido

| Ruta | Qué es |
|---|---|
| `index.html` | El post, con gráficos interactivos (Plotly) |
| `R/01_base.R` | Lealtad al bloque y al partido, votaciones de bloque contra bloque, Rice, carreras y votos de Boric y Kast |
| `R/02_materias.R` | Materia de cada votación por el título del proyecto, validación contra los descriptores de la BCN y votos contra el bloque por materia |
| `R/03_indicadores.R` | Polarización, Rice, modelo logit de carreras y `datos/datos.js` |
| `datos/datos.js` | Los datos que lee la página |

## Datos y decisiones

- Votos nominales de opendata.camara.cl, 2002-2026, y estimaciones de B-Call del análisis anterior.
- Cargos posteriores (Senado, Presidencia) desde los datos abiertos de la BCN.
- Las materias se asignan por el título del proyecto. Los diccionarios se armaron mirando una mitad de los proyectos con descriptores de la BCN y se midieron una vez en la otra mitad.

Los scripts se corren desde la raíz de un proyecto local que guarda los datos crudos ya bajados. Esos archivos no están en este repositorio.

## Referencia

Toro-Maureira, S., Reutter, J., Valenzuela, L., Alcatruz, D. y Valenzuela, M. (2025). B-Call: integrating ideological position and voting cohesion in legislative behavior. *Frontiers in Political Science, 7*, 1670089. https://doi.org/10.3389/fpos.2025.1670089
