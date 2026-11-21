# =============================================================================
# 08_texto_medidas.R — las palabras de Boric y Kast con las medidas estándar
# del análisis de texto, y cuánto coinciden entre sí.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 04_textos.R (usa su corpus limpio):
#   Rscript post_boric_kast/R/08_texto_medidas.R
#
# MEDIDAS
#   - Keyness de Boric frente a Kast con la razón de verosimilitud G² (Dunning,
#     1993) y con chi² con corrección de Yates, las dos medidas de
#     quanteda.textstats::textstat_keyness, calculadas aquí con sus fórmulas.
#     Se agrega el log-odds con prior informativo de 04_textos.R (Monroe et
#     al., 2008), con el prior de toda la Cámara del período.
#   - TF-IDF por diputado y período (quanteda::dfm_tfidf, frecuencia relativa
#     por la inversa de la frecuencia de documentos). Cada documento son
#     todos los discursos de un diputado en un período, contra el resto de la
#     Cámara del mismo período.
#   - Coocurrencias (quanteda::fcm) en una ventana de 5 palabras, con las
#     palabras vacías reemplazadas por un hueco para que la distancia sea la
#     del texto original. Red de las 35 palabras más usadas por cada uno.
#   - Concordancias (quanteda::kwic) de algunas palabras clave, para leerlas
#     en su contexto.
#
# EXPRESIONES DE DOS PALABRAS
#   Se unen en un solo término los pares de palabras no vacías que aparecen
#   juntas al menos 30 veces en la Cámara 2002-2022 y en que la menos
#   frecuente de las dos aparece en ese par al menos el 40% de las veces
#   ("derechos humanos", "educación superior"). Los nombres de partidos y
#   coaliciones se unen siempre ("frente amplio", "partido comunista").
#
# FILTROS
#   Términos de al menos 3 letras. En la keyness, términos que el par usa al
#   menos 8 veces, en al menos 4 discursos, como en 04_textos.R.
#
# ESCRIBE (outputs/data/post_boric_kast/)
#   textos_medidas.rds    lista con keyness, acuerdo, tfidf, redes, kwic y
#                         expresiones
#   textos_medidas.md     tablas para revisar
#   y las nubes en post_boric_kast/img/nube_boric.png y nube_kast.png, con el
#   tamaño de la letra proporcional a la raíz de G² (no el área, que agranda
#   las palabras cortas)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(quanteda)
  library(ggplot2)
  library(ggwordcloud)
  library(igraph)
})
source("post_boric_kast/R/00_vacias.R")
quanteda_options(threads = 4)

DIR <- "outputs/data/post_boric_kast"
d <- readRDS(file.path(DIR, "textos_discursos.rds")) |>
  mutate(docid = as.character(id)) |>
  distinct(docid, .keep_all = TRUE)

corp <- corpus(d |> select(docid, limpio, quien, periodo, bloque, id_camara),
               docid_field = "docid", text_field = "limpio")
toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) |>
  tokens_tolower()

# --- Expresiones de dos palabras ------------------------------------------------
toks_pad <- tokens_remove(toks, vacias, padding = TRUE) |> tokens_select(min_nchar = 3, padding = TRUE)
bi <- tokens_ngrams(toks_pad, n = 2, concatenator = " ")
bi_n <- colSums(dfm(bi, tolower = FALSE))
bi_n <- bi_n[!grepl("^ | $|  ", names(bi_n)) & bi_n >= 30]
uni_n <- colSums(dfm(toks_pad, tolower = FALSE))
w <- str_split_fixed(names(bi_n), " ", 2)
menor <- pmin(uni_n[w[, 1]], uni_n[w[, 2]])
expresiones <- tibble(expresion = names(bi_n), n = as.integer(bi_n), menor = as.integer(menor)) |>
  filter(!is.na(menor), n / menor >= 0.4) |> arrange(desc(n))
cat("expresiones de dos palabras:", nrow(expresiones), "\n")

# Partidos y coaliciones se unen siempre, aunque sus palabras no pasen la regla
PARTIDOS <- c("frente amplio", "partido comunista", "renovación nacional", "unión demócrata independiente",
              "democracia cristiana", "nueva mayoría", "chile vamos", "partido socialista", "revolución democrática",
              "partido por la democracia", "partido radical", "evolución política", "convergencia social",
              "partido humanista", "partido republicano", "alianza por chile", "concertación de partidos por la democracia")
toks_c <- tokens_compound(toks, phrase(unique(c(PARTIDOS, expresiones$expresion))), concatenator = "_")
toks_s <- tokens_remove(toks_c, vacias) |> tokens_select(min_nchar = 3)
dfmat <- dfm(toks_s)

# --- Keyness -----------------------------------------------------------------
keyness <- function(sub_a, sub_b, fondo, etiqueta, a = "Boric", b = "Kast", alfa0 = 10000, min_n = 8, min_doc = 4) {
  ca <- colSums(dfm_subset(dfmat, sub_a)); cb <- colSums(dfm_subset(dfmat, sub_b))
  docs <- colSums(dfm_subset(dfmat, sub_a | sub_b) > 0)
  f <- colSums(dfm_subset(dfmat, fondo))
  na <- sum(ca); nb <- sum(cb)
  tibble(termino = featnames(dfmat), ya = as.numeric(ca), yb = as.numeric(cb), discursos = as.numeric(docs),
         fondo = as.numeric(f)) |>
    filter(ya + yb >= min_n, discursos >= min_doc) |>
    mutate(e1 = na * (ya + yb) / (na + nb), e2 = nb * (ya + yb) / (na + nb),
           g2 = 2 * (ifelse(ya > 0, ya * log(ya / e1), 0) + ifelse(yb > 0, yb * log(yb / e2), 0)),
           N = na + nb, c_ = na - ya, d_ = nb - yb,
           chi2 = N * pmax(abs(ya * d_ - yb * c_) - N / 2, 0)^2 / ((ya + yb) * (c_ + d_) * (ya + c_) * (yb + d_)),
           signo = ifelse(ya / na >= yb / nb, 1, -1),
           g2 = signo * g2, chi2 = signo * chi2,
           alfa = alfa0 * fondo / sum(f),
           logodds = log((ya + alfa) / (na + alfa0 - ya - alfa)) - log((yb + alfa) / (nb + alfa0 - yb - alfa)),
           z = logodds / sqrt(1 / (ya + alfa) + 1 / (yb + alfa)),
           lado = ifelse(signo > 0, a, b), comparacion = etiqueta) |>
    filter(alfa > 0) |>
    select(comparacion, termino, lado, ya, yb, g2, chi2, z)
}
dv <- docvars(dfmat)
es <- function(q) !is.na(dv$quien) & dv$quien == q
k_carrera <- keyness(es("Boric"), es("Kast"), !is.na(dv$periodo), "carrera")
k_1418 <- keyness(es("Boric") & dv$periodo == "2014-2018", es("Kast") & dv$periodo == "2014-2018",
                  dv$periodo == "2014-2018", "p1418")
resto <- function(b) is.na(dv$quien) & !is.na(dv$bloque) & dv$bloque == b
k_kast_bloque <- keyness(es("Kast"), resto("right") & dv$periodo %in% c("2002-2006", "2006-2010", "2010-2014", "2014-2018"),
                         dv$periodo %in% c("2002-2006", "2006-2010", "2010-2014", "2014-2018"), "kast_bloque",
                         a = "Kast", b = "resto de la derecha")
k_boric_bloque <- keyness(es("Boric"), resto("left") & dv$periodo %in% c("2014-2018", "2018-2022"),
                          dv$periodo %in% c("2014-2018", "2018-2022"), "boric_bloque",
                          a = "Boric", b = "resto de la izquierda")
key <- bind_rows(k_carrera, k_1418, k_kast_bloque, k_boric_bloque)

# --- ¿Coinciden las medidas? -----------------------------------------------------
acuerdo <- key |> group_by(comparacion, lado) |>
  group_modify(function(g, k) {
    top <- function(v) g$termino[order(-abs(g[[v]]))][1:20]
    tibble(g2_chi2 = length(intersect(top("g2"), top("chi2"))),
           g2_logodds = length(intersect(top("g2"), top("z"))),
           las_tres = length(Reduce(intersect, list(top("g2"), top("chi2"), top("z")))),
           spearman_g2_z = round(cor(abs(g$g2), abs(g$z), method = "spearman"), 2))
  }) |> ungroup()
robustas <- key |> group_by(comparacion, lado) |>
  mutate(r_g2 = rank(-abs(g2)), r_chi2 = rank(-abs(chi2)), r_z = rank(-abs(z))) |>
  filter(r_g2 <= 20, r_chi2 <= 20, r_z <= 20) |> arrange(comparacion, lado, r_g2) |> ungroup()

# --- TF-IDF por diputado y período ------------------------------------------------
tfidf <- bind_rows(lapply(unique(dv$periodo), function(p) {
  m <- dfm_subset(dfmat, dv$periodo == p) |> dfm_group(groups = id_camara)
  quien_doc <- d |> filter(periodo == p, !is.na(quien)) |> distinct(id_camara, quien)
  if (!nrow(quien_doc)) return(NULL)
  t <- dfm_tfidf(m, scheme_tf = "prop", scheme_df = "inverse")
  bind_rows(lapply(seq_len(nrow(quien_doc)), function(i) {
    v <- as.numeric(t[docnames(t) == quien_doc$id_camara[i], ])
    n <- as.numeric(m[docnames(m) == quien_doc$id_camara[i], ])
    o <- order(-v)[1:12]
    tibble(quien = quien_doc$quien[i], periodo = p, termino = featnames(t)[o], tfidf = v[o], n = n[o])
  }))
}))

# --- Redes de coocurrencia ----------------------------------------------------------
toks_pad_c <- tokens_remove(toks_c, vacias, padding = TRUE) |> tokens_select(min_nchar = 3, padding = TRUE)
red <- function(q, n_nodos = 35, n_aristas = 70) {
  tq <- tokens_subset(toks_pad_c, !is.na(quien) & quien == q)
  frec <- colSums(dfm(tq)); frec <- frec[names(frec) != ""]
  nodos <- names(sort(frec, decreasing = TRUE))[1:n_nodos]
  m <- fcm(tq, context = "window", window = 5, tri = TRUE) |> fcm_select(pattern = nodos, valuetype = "fixed")
  mm <- as.matrix(m); mm[lower.tri(mm, diag = TRUE)] <- 0
  ij <- which(mm > 0, arr.ind = TRUE)
  ar <- tibble(de = rownames(mm)[ij[, 1]], a = colnames(mm)[ij[, 2]], peso = mm[ij]) |>
    filter(de != a) |> slice_max(peso, n = n_aristas, with_ties = FALSE)
  g <- graph_from_data_frame(ar, directed = FALSE, vertices = tibble(name = nodos))
  set.seed(20261121)
  xy <- layout_with_fr(g, weights = log1p(E(g)$peso))
  k <- k_carrera |> filter(termino %in% nodos) |> select(termino, g2)
  list(nodos = tibble(termino = nodos, n = as.integer(frec[nodos]), x = xy[, 1], y = xy[, 2]) |>
         left_join(k, by = "termino") |>
         mutate(propio = !is.na(g2) & ((q == "Boric" & g2 > 10.83) | (q == "Kast" & g2 < -10.83))),
       aristas = ar)
}
redes <- list(Boric = red("Boric"), Kast = red("Kast"))

# --- Concordancias -------------------------------------------------------------
toks_k <- tokens(corp, remove_punct = FALSE) |> tokens_compound(phrase(unique(c(PARTIDOS, expresiones$expresion))), concatenator = "_")
conc <- function(q, patron, n = 3) {
  k <- kwic(tokens_subset(toks_k, !is.na(quien) & quien == q), pattern = patron, window = 9, case_insensitive = TRUE)
  set.seed(20261121)
  as_tibble(k) |> slice_sample(n = min(n, nrow(k))) |>
    transmute(quien = q, termino = str_replace_all(keyword, "_", " "), antes = pre, despues = post)
}
kw <- bind_rows(conc("Kast", "educación"), conc("Kast", "calidad"), conc("Kast", "partido_comunista", 2),
                conc("Boric", "trabajadores"), conc("Boric", "derechos_humanos"), conc("Boric", "magallanes", 2))

# --- Nubes ---------------------------------------------------------------------
dir.create("post_boric_kast/img", showWarnings = FALSE)
nube <- function(q, color, archivo) {
  t <- k_carrera |> filter(lado == q) |> slice_max(abs(g2), n = 50, with_ties = FALSE) |>
    mutate(etiqueta = str_replace_all(termino, "_", " "), tam = sqrt(abs(g2)))
  set.seed(20261121)
  p <- ggplot(t, aes(label = etiqueta, size = tam)) +
    geom_text_wordcloud(color = color, eccentricity = 0.65, grid_margin = 1.5, family = "sans", rm_outside = TRUE) +
    scale_radius(range = c(3, 12.5), limits = c(0, NA)) + theme_void() +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  ggsave(archivo, p, width = 7, height = 4.6, dpi = 160, bg = "transparent")
}
nube("Boric", "#d64541", "post_boric_kast/img/nube_boric.png")
nube("Kast", "#2a78d6", "post_boric_kast/img/nube_kast.png")

# --- Guardar y mostrar -----------------------------------------------------------
saveRDS(list(keyness = key, acuerdo = acuerdo, robustas = robustas, tfidf = tfidf, redes = redes, kwic = kw,
             expresiones = expresiones), file.path(DIR, "textos_medidas.rds"))

top_g2 <- key |> group_by(comparacion, lado) |> slice_max(abs(g2), n = 20, with_ties = FALSE) |>
  mutate(g2 = round(g2, 1), chi2 = round(chi2, 1), z = round(z, 1)) |> ungroup()
print.data.frame(acuerdo)
print.data.frame(robustas |> select(comparacion, lado, termino, ya, yb))
print.data.frame(tfidf |> group_by(quien, periodo) |> slice_head(n = 8) |> summarise(terminos = paste(termino, collapse = ", "), .groups = "drop"))
print.data.frame(kw)
md <- c("# Medidas de texto", "", "Generado por `R/08_texto_medidas.R`. Para revisar; no va al repositorio.", "",
        "## Acuerdo entre medidas (top 20 por lado)", "", knitr::kable(acuerdo, format = "pipe"), "",
        "## Términos en el top 20 de las tres medidas", "", knitr::kable(robustas |> select(comparacion, lado, termino, ya, yb), format = "pipe"), "",
        "## Top 20 por G²", "", knitr::kable(top_g2, format = "pipe"), "",
        "## TF-IDF por período", "", knitr::kable(tfidf |> mutate(tfidf = signif(tfidf, 3)), format = "pipe"), "",
        "## Expresiones de dos palabras (primeras 60)", "", knitr::kable(head(expresiones, 60), format = "pipe"), "")
writeLines(md, file.path(DIR, "textos_medidas.md"), useBytes = TRUE)
