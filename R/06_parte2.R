# =============================================================================
# 06_parte2.R — los datos de la Parte 2 del post (parte2.html), en
# datos/datos2.js, con lo mínimo que necesitan los gráficos.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 04_textos.R y 05_biografia.R:
#   Rscript post_boric_kast/R/06_parte2.R
#
# Además de lo que dejan 04 y 05, pide a Labor Parlamentaria las facetas de
# cada persona por año (una consulta por año en que fue diputado), para
# separar las peticiones de oficio del resto. Las respuestas quedan en la
# caché de congresochile.
#
# ESCRIBE  post_boric_kast/datos/datos2.js
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(congresochile)
})

DIR <- "outputs/data/post_boric_kast"
dist <- readRDS(file.path(DIR, "textos_distintivas.rds"))
disc <- readRDS(file.path(DIR, "textos_discursos.rds")) |> filter(!is.na(quien))
bio  <- readRDS(file.path(DIR, "biografia.rds"))

r2 <- function(x) round(x, 2)

# --- Palabras distintivas ------------------------------------------------------
# Una palabra suelta no se muestra si al menos la mitad de sus usos, del lado
# que la distingue, está dentro de un bigrama que también está entre los 40
# primeros de ese lado ("derechos humanos" deja fuera a "humanos" y a
# "derechos"; "educación superior", que es el 16% de "educación", no).
tope <- function(cmp, clave, n, lados = NULL) {
  d <- dist |> filter(comparacion == cmp)
  if (!is.null(lados)) d <- d |> filter(lado %in% lados)
  d |> group_by(lado) |> slice_max(abs(z), n = 40, with_ties = FALSE) |>
    mutate(usos = if_else(z > 0, ya, yb)) |>
    group_modify(function(g, k) {
      bi <- g |> filter(grepl(" ", termino))
      dentro <- vapply(seq_len(nrow(g)), function(i) {
        if (grepl(" ", g$termino[i])) return(FALSE)
        b <- bi |> filter(vapply(strsplit(termino, " "), function(w) g$termino[i] %in% w, logical(1)))
        nrow(b) > 0 && max(b$usos) / g$usos[i] >= 0.5
      }, logical(1))
      g[!dentro, ]
    }) |>
    slice_max(abs(z), n = n, with_ties = FALSE) |> ungroup() |>
    transmute(c = clave, lado, t = termino, z = r2(z), a = ya, b = yb)
}
palabras <- bind_rows(
  tope("Boric y Kast, toda su carrera", "carrera", 15),
  tope("Boric y Kast, 2014-2018", "p1418", 15),
  tope("Kast y el resto de la derecha, 2002-2018", "kast_bloque", 14, "Kast"),
  tope("Boric y el resto de la izquierda, 2014-2022", "boric_bloque", 14, "Boric")
)

# --- Discursos por año -----------------------------------------------------------
volumen <- disc |> mutate(anno = as.integer(format(fecha, "%Y"))) |>
  count(q = quien, anno, name = "n")

# --- Trayectoria ---------------------------------------------------------------
bonito <- function(p) {
  p <- recode(p, "partido-union-democrata-independiente" = "UDI", "Unión Demócrata Independiente" = "UDI",
              "independiente" = "Independiente", "Independientes" = "Independiente", "IND" = "Independiente",
              "partido-humanista" = "Partido Humanista", "partido-convergencia-social" = "Convergencia Social",
              "Partido Convergencia Social" = "Convergencia Social", "PCS" = "Convergencia Social")
  p
}
periodos <- bio$periodos |>
  transmute(q = quien, periodo, distrito, partido = bonito(partido), bloque,
            pos = r2(d1_pct), d2 = r2(d2_pct), leal = r2(leal_bloque), discursos)
militancias <- bio$militancias |>
  transmute(q = quien, fuente, partido = bonito(partido), inicio = format(inicio, "%d-%m-%Y"),
            fin = ifelse(is.na(fin), "", format(fin, "%d-%m-%Y")))

# --- Labor por año, oficios y el resto ----------------------------------------------
URL <- "https://www.bcn.cl/laborparlamentaria/facetas-buscador-avanzado"
ANNOS <- list(Kast = c("2231", 2002:2018), Boric = c("4536", 2014:2022))
labor_anno <- bind_rows(lapply(names(ANNOS), function(q) {
  id <- ANNOS[[q]][1]
  bind_rows(lapply(ANNOS[[q]][-1], function(a) {
    j <- jsonlite::fromJSON(congresochile:::cc_pedir(paste0(URL, "?sort=desc&start=0&rows=0&personas=", id, "&anio=", a),
                                                     "bcn_www", paste0("labor_facetas_", id, "_", a, ".json"),
                                                     navegador = TRUE, pausa = 1))
    t <- as.data.frame(j$id_tipo_participacion)
    oficios <- if (nrow(t)) sum(as.integer(t$total[t$id == "917"])) else 0L
    tibble(q = q, anno = as.integer(a), total = as.integer(j$numFound), oficios = oficios, otros = total - oficios)
  }))
}))

# Peticiones de oficio de toda la Cámara por año. Labor las registra desde 2010.
jc <- jsonlite::fromJSON(congresochile:::cc_pedir(paste0(URL, "?sort=desc&start=0&rows=0&id_tipo_participacion=917&id_camara=288"),
                                                  "bcn_www", "labor_facetas_tipo917_camara288.json", navegador = TRUE, pausa = 1))
oficios_camara <- as.data.frame(jc$anio) |>
  transmute(anno = as.integer(anio), oficios = as.integer(total)) |>
  filter(anno >= 2010, anno <= 2022) |> arrange(anno)

ANNOS_DIPUTADO <- c(Kast = 16, Boric = 8)
TIPOS <- c("917" = "Peticiones de oficio", "915" = "Mociones", "920" = "Intervenciones en Sala",
           "939" = "Proyectos de resolución", "909" = "Proyectos de acuerdo", "949" = "Intervenciones en comisión",
           "914" = "Permisos", "935" = "Licencias médicas")
labor_tipos <- bio$labor_tipos |>
  filter(as.character(id_tipo) %in% names(TIPOS)) |>
  transmute(q = quien, tipo = unname(TIPOS[as.character(id_tipo)]), total,
            por_anno = r2(total / ANNOS_DIPUTADO[quien]))

print.data.frame(labor_anno)

datos <- list(palabras = palabras, volumen = volumen, periodos = periodos, militancias = militancias,
              labor_anno = labor_anno, labor_tipos = labor_tipos, oficios_camara = oficios_camara)
js <- paste0("window.BK2 = ", jsonlite::toJSON(datos, dataframe = "rows", auto_unbox = TRUE, na = "null"), ";\n")
writeLines(js, "post_boric_kast/datos/datos2.js", useBytes = TRUE)
cat("escrito post_boric_kast/datos/datos2.js,", nchar(js), "caracteres\n")
