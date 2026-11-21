# =============================================================================
# 04_textos.R — lo que dicen Boric y Kast en la Sala. Volumen por año, palabras
# que distinguen a uno del otro y a cada uno de su bloque, y por materia.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 01_base.R y 02_materias.R:
#   Rscript post_boric_kast/R/04_textos.R
#
# FUENTES
#   intervenciones_pleno/datos/base_pleno.rds   intervenciones en Sala desde
#                 1990, con orador revisado, id de la Cámara y boletín
#   post_bcall_periodos/datos/bcall_periodos.csv  bloque B-Call por período
#   outputs/data/post_boric_kast/materias_boletin.rds  materia de cada boletín
#
# QUÉ DISCURSOS
#   - Cámara, tipo "Intervención" (920), del 11-03-2002 al 10-03-2022, de
#     diputados en ejercicio con id de la Cámara y bloque B-Call en el período.
#   - Fuera los informes de comisión. Quien informa habla por la comisión, no
#     por sí mismo. Se reconocen porque la Mesa da la palabra al "diputado
#     informante" o anuncia que alguien "rinde el informe", al comienzo.
#   - Fuera las peticiones de oficio, los homenajes y las indicaciones, que
#     son otro género.
#
# LIMPIEZA
#   - Se corta lo que dice la Mesa antes del turno del orador. El turno
#     empieza en el encabezado con su apellido en mayúsculas ("El señor
#     BORIC.-", "El señor KAST (don José Antonio).-").
#   - Se quitan los nombres de personas que siguen a un tratamiento
#     ("el diputado Jorge González", "la ministra Bachelet"): queda el
#     tratamiento y sale el nombre. Los nombres de partidos se dejan, porque
#     a quién se nombra es parte de lo que se mide.
#   - Palabras vacías del castellano (stopwords-iso) y fórmulas de la Sala
#     (señor, presidente, diputado, honorable, proyecto, ley, artículo...).
#   - Unigramas y bigramas. Un bigrama cuenta si sus dos palabras no son vacías.
#
# PALABRAS DISTINTIVAS
#   Log-odds con prior de Dirichlet informativo. El prior es la frecuencia de
#   cada término en toda la Cámara del mismo período, escalada a 10.000
#   palabras. Se informa z = log-odds / error estándar. Solo términos que el
#   par usa al menos 8 veces, en al menos 4 discursos.
#
# ESCRIBE (outputs/data/post_boric_kast/)
#   textos_discursos.rds    una fila por discurso, con texto limpio
#   textos_distintivas.rds  términos distintivos por comparación
#   textos_resumen.md       tablas para revisar
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tidytext)
})

DIR <- "outputs/data/post_boric_kast"
BK  <- c("José Antonio Kast Rist" = "Kast", "Gabriel Boric Font" = "Boric")

PERIODOS <- tibble(periodo = c("2002-2006", "2006-2010", "2010-2014", "2014-2018", "2018-2022"),
                   inicio = as.Date(c("2002-03-11", "2006-03-11", "2010-03-11", "2014-03-11", "2018-03-11")),
                   fin    = as.Date(c("2006-03-10", "2010-03-10", "2014-03-10", "2018-03-10", "2022-03-10")))

# --- Discursos ---------------------------------------------------------------
b <- readRDS("intervenciones_pleno/datos/base_pleno.rds") |>
  filter(camara == "Cámara de Diputados", tipo == "Intervención", en_ejercicio %in% TRUE,
         !is.na(id_camara), fecha >= as.Date("2002-03-11"), fecha <= as.Date("2022-03-10")) |>
  select(id, fecha, id_camara, orador, boletin, debate, texto)

cabeza <- substr(b$texto, 1, 500)
es_informe <- grepl("(?i)(palabra,? (el|la) diputad[oa] informante|diputad[oa] informante de la comisi|rinde el informe)",
                    cabeza, perl = TRUE)
cat("discursos 920 en el rango:", nrow(b), "; informes de comisión fuera:", sum(es_informe), "\n")
b <- b[!es_informe, ]

bloques <- readr::read_csv("post_bcall_periodos/datos/bcall_periodos.csv", show_col_types = FALSE,
                           col_types = readr::cols(id_parlamentario = "c")) |>
  select(periodo, id_camara = id_parlamentario, bloque)

b <- b |>
  left_join(PERIODOS, by = join_by(between(fecha, inicio, fin))) |>
  select(-inicio, -fin) |>
  inner_join(bloques, by = c("periodo", "id_camara")) |>
  mutate(quien = unname(BK[orador]))

# --- Limpieza ----------------------------------------------------------------
apellido <- str_to_upper(str_extract(b$orador, "(?<= )\\S+(?= \\S+$)|^\\S+ \\S+$"))
cortar_mesa <- function(t, ap) {
  # el último encabezado de turno con el apellido del orador en los primeros 600 caracteres
  m <- gregexpr(paste0("(El señor|La señora|La señorita) ", ap, "[^.]{0,60}\\.-"), substr(t, 1, 600), perl = TRUE)[[1]]
  if (m[1] > 0) substr(t, max(m) + attr(m, "match.length")[length(m)], nchar(t)) else t
}
b$limpio <- mapply(cortar_mesa, b$texto, apellido, USE.NAMES = FALSE)

TRATOS <- "(diputad[oa]s?|señor(a|ita)?|(ex)?ministr[oa]|colega|senador(a)?|presidente|presidenta|don|doña|subsecretari[oa]|general|almirante|alcalde(sa)?|intendente|ex[- ]presidente|ex[- ]ministr[oa])"
b$limpio <- str_replace_all(b$limpio, paste0("(?<=\\b", TRATOS, ")\\s+(\\p{Lu}[\\p{L}'-]+)(\\s+(de |del )?\\p{Lu}[\\p{L}'-]+){0,3}"), "")
b$palabras <- str_count(b$limpio, "\\S+")

SALA <- c("señor", "señora", "señores", "señorita", "presidente", "presidenta", "diputado", "diputada", "diputados",
          "diputadas", "honorable", "honorables", "colega", "colegas", "sala", "proyecto", "proyectos", "ley", "leyes",
          "artículo", "artículos", "inciso", "número", "boletín", "cámara", "corporación", "sesión", "palabra",
          "intervención", "indicación", "indicaciones", "votación", "votar", "voto", "votos", "comisión", "comisiones",
          "gracias", "muchas", "presidencia", "mesa", "minutos", "tiempo", "intermedio", "su", "don", "doña",
          "particular", "general", "señoría", "señorías")
vacias <- unique(c(stopwords::stopwords("es", source = "stopwords-iso"), SALA))

tokens <- function(d) {
  uni <- d |> select(id, quien, periodo, bloque, limpio) |>
    unnest_tokens(termino, limpio, token = "words") |>
    filter(!termino %in% vacias, !str_detect(termino, "^[0-9]+$"), nchar(termino) > 2)
  bi <- d |> select(id, quien, periodo, bloque, limpio) |>
    unnest_tokens(termino, limpio, token = "ngrams", n = 2) |>
    filter(!is.na(termino)) |>
    separate(termino, c("w1", "w2"), sep = " ", remove = FALSE) |>
    filter(!w1 %in% vacias, !w2 %in% vacias, !str_detect(w1, "^[0-9]+$"), !str_detect(w2, "^[0-9]+$"),
           nchar(w1) > 2, nchar(w2) > 2) |>
    select(-w1, -w2)
  bind_rows(uni, bi)
}
tk <- tokens(b)

# --- Log-odds con prior informativo ------------------------------------------
log_odds <- function(tk_a, tk_b, fondo, nombre_a, nombre_b, alfa0 = 10000, min_n = 8, min_doc = 4) {
  ca <- count(tk_a, termino, name = "ya"); cb <- count(tk_b, termino, name = "yb")
  docs <- bind_rows(tk_a, tk_b) |> distinct(termino, id) |> count(termino, name = "discursos")
  pf <- count(fondo, termino, name = "f") |> mutate(alfa = alfa0 * f / sum(f))
  na <- nrow(tk_a); nb <- nrow(tk_b)
  full_join(ca, cb, by = "termino") |>
    mutate(ya = coalesce(ya, 0L), yb = coalesce(yb, 0L)) |>
    inner_join(pf |> select(termino, alfa), by = "termino") |>
    left_join(docs, by = "termino") |>
    filter(ya + yb >= min_n, discursos >= min_doc) |>
    mutate(delta = log((ya + alfa) / (na + alfa0 - ya - alfa)) - log((yb + alfa) / (nb + alfa0 - yb - alfa)),
           z = delta / sqrt(1 / (ya + alfa) + 1 / (yb + alfa)),
           lado = if_else(z > 0, nombre_a, nombre_b)) |>
    arrange(desc(abs(z)))
}

comparar <- function(etiqueta, tk_a, tk_b, fondo, a, bb) {
  log_odds(tk_a, tk_b, fondo, a, bb) |> mutate(comparacion = etiqueta, .before = 1)
}

dist <- bind_rows(
  comparar("Boric y Kast, 2014-2018",
           tk |> filter(quien == "Boric", periodo == "2014-2018"), tk |> filter(quien == "Kast", periodo == "2014-2018"),
           tk |> filter(periodo == "2014-2018"), "Boric", "Kast"),
  comparar("Boric y Kast, toda su carrera",
           tk |> filter(quien == "Boric"), tk |> filter(quien == "Kast"),
           tk |> filter(periodo %in% c("2002-2006", "2006-2010", "2010-2014", "2014-2018", "2018-2022")), "Boric", "Kast"),
  comparar("Kast y el resto de la derecha, 2002-2018",
           tk |> filter(quien == "Kast"), tk |> filter(is.na(quien), bloque == "right", periodo != "2018-2022"),
           tk |> filter(periodo != "2018-2022"), "Kast", "resto de la derecha"),
  comparar("Boric y el resto de la izquierda, 2014-2022",
           tk |> filter(quien == "Boric"), tk |> filter(is.na(quien), bloque == "left", periodo %in% c("2014-2018", "2018-2022")),
           tk |> filter(periodo %in% c("2014-2018", "2018-2022")), "Boric", "resto de la izquierda")
)

# --- Volumen y materias ------------------------------------------------------
mat <- readRDS(file.path(DIR, "materias_boletin.rds")) |> select(boletin, seguridad, economia, valorico, politico)
bk <- b |> filter(!is.na(quien)) |>
  mutate(boletin1 = str_extract(boletin, "^[0-9]+-[0-9]+")) |>
  left_join(mat, by = c("boletin1" = "boletin"))

volumen <- bk |> mutate(anno = as.integer(format(fecha, "%Y"))) |>
  group_by(quien, anno) |> summarise(discursos = n(), palabras = sum(palabras), .groups = "drop")
por_periodo <- bk |> group_by(quien, periodo) |>
  summarise(discursos = n(), mediana_palabras = median(palabras), palabras = sum(palabras),
            con_boletin = round(100 * mean(!is.na(boletin1))), .groups = "drop")
materias <- bk |> filter(!is.na(boletin1)) |>
  group_by(quien) |>
  summarise(con_boletin = n(), seguridad = sum(seguridad %in% TRUE), economia = sum(economia %in% TRUE),
            valorico = sum(valorico %in% TRUE), politico = sum(politico %in% TRUE),
            otras = sum(!(seguridad %in% TRUE | economia %in% TRUE | valorico %in% TRUE | politico %in% TRUE)),
            .groups = "drop")

# --- Guardar y mostrar -------------------------------------------------------
saveRDS(b |> select(id, fecha, periodo, id_camara, orador, quien, bloque, boletin, debate, palabras, limpio),
        file.path(DIR, "textos_discursos.rds"))
saveRDS(dist, file.path(DIR, "textos_distintivas.rds"))

top <- dist |> group_by(comparacion, lado) |> slice_max(abs(z), n = 25, with_ties = FALSE) |>
  ungroup() |> transmute(comparacion, lado, termino, ya, yb, z = round(z, 1))
print.data.frame(por_periodo); print.data.frame(materias)
for (cmp in unique(top$comparacion)) { cat("\n==", cmp, "==\n"); print.data.frame(top |> filter(comparacion == cmp)) }

md <- c("# Lo que dicen en la Sala", "", "Generado por `R/04_textos.R`. Solo para revisar; no va al repositorio.", "",
        "## Discursos por período", "", knitr::kable(por_periodo, format = "pipe"), "",
        "## Discursos con boletín, por materia", "", knitr::kable(materias, format = "pipe"), "")
for (cmp in unique(top$comparacion)) {
  md <- c(md, paste("##", cmp), "", knitr::kable(top |> filter(comparacion == cmp) |> select(-comparacion), format = "pipe"), "")
}
writeLines(md, file.path(DIR, "textos_resumen.md"), useBytes = TRUE)
