# =============================================================================
# 06_parte2.R — los datos de la Parte 2 del post (parte2.html), en
# datos/datos2.js, con lo mínimo que necesitan los gráficos.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 04_textos.R, 05_biografia.R, 07_copatrocinio.R y 08_texto_medidas.R:
#   Rscript post_boric_kast/R/06_parte2.R
#
# Además de lo que dejan esos scripts, pide a Labor Parlamentaria las facetas de
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
disc <- readRDS(file.path(DIR, "textos_discursos.rds")) |> filter(!is.na(quien))
bio  <- readRDS(file.path(DIR, "biografia.rds"))

r2 <- function(x) round(x, 2)

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

# --- Copatrocinio (07_copatrocinio.R) ------------------------------------------------
cop <- readRDS(file.path(DIR, "copatrocinio.rds"))
nombre_corto <- function(x) sub("^(\\S+)(?: \\S+)?, (.*)$", "\\2 \\1", x, perl = TRUE)
copatrocinio <- cop$bk |>
  transmute(q = quien, periodo, mociones, transversales = r2(transversales), otro_bloque = r2(otro_bloque),
            distintos, transversales_pct = r2(transversales_pct), distintos_pct = r2(distintos_pct))
cop_medianas <- cop$medianas |>
  transmute(periodo, diputados, transversales = r2(transversales), distintos)
socios <- cop$socios |>
  group_by(quien, periodo) |> slice_max(mociones_juntos, n = 4, with_ties = FALSE) |> ungroup() |>
  transmute(q = quien, periodo, nombre = nombre_corto(nombre_completo), partido, n = mociones_juntos)

# --- Medidas de texto (08_texto_medidas.R) --------------------------------------------
med <- readRDS(file.path(DIR, "textos_medidas.rds"))
limpio <- function(x) gsub("_", " ", x)
keyness <- med$keyness |>
  group_by(comparacion, lado) |> slice_max(abs(g2), n = 15, with_ties = FALSE) |> ungroup() |>
  transmute(c = comparacion, lado, t = limpio(termino), g2 = round(abs(g2), 1), a = ya, b = yb)
robustas <- med$robustas |> transmute(c = comparacion, lado, t = limpio(termino))
acuerdo <- med$acuerdo
tfidf <- med$tfidf |> group_by(quien, periodo) |> slice_head(n = 6) |> ungroup() |>
  transmute(q = quien, periodo, t = limpio(termino), n)
redes <- lapply(med$redes, function(r) list(
  nodos = r$nodos |> transmute(t = limpio(termino), n, x = round(x, 3), y = round(y, 3), propio),
  aristas = r$aristas |> transmute(de = limpio(de), a = limpio(a), peso)))
pegar <- function(x) gsub("\\( ", "(", gsub(" ([.,;:!?)])", "\\1", x))
kwic <- med$kwic |> transmute(q = quien, t = termino, antes = pegar(antes), despues = pegar(despues))

datos <- list(volumen = volumen, periodos = periodos, militancias = militancias,
              labor_anno = labor_anno, labor_tipos = labor_tipos, oficios_camara = oficios_camara,
              copatrocinio = copatrocinio, cop_medianas = cop_medianas, socios = socios,
              keyness = keyness, robustas = robustas, acuerdo = acuerdo, tfidf = tfidf, redes = redes, kwic = kwic)
js <- paste0("window.BK2 = ", jsonlite::toJSON(datos, dataframe = "rows", auto_unbox = TRUE, na = "null"), ";\n")
writeLines(js, "post_boric_kast/datos/datos2.js", useBytes = TRUE)
cat("escrito post_boric_kast/datos/datos2.js,", nchar(js), "caracteres\n")
