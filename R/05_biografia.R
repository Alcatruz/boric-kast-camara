# =============================================================================
# 05_biografia.R — la biografía parlamentaria de Boric y Kast. Distrito,
# partido según la Cámara y según la BCN, bloque y posición B-Call por
# período, discursos en Sala y actividad registrada en Labor Parlamentaria.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 01_base.R y 04_textos.R:
#   Rscript post_boric_kast/R/05_biografia.R
#
# FUENTES
#   data/bcn_biografias.rds        reseñas de la BCN (Historia Política); de
#                                  ahí sale el distrito de cada período
#   data/bcn_mandatos.rds          cargos con fechas (BCN)
#   data/militancias.rds           militancias según la Cámara (opendata)
#   congresochile::bcn_trayectoria militancias según la BCN
#   congresochile::labor_facetas   actividad en Labor por tipo y año
#   outputs/data/post_boric_kast/diputados_periodo.rds   d1, d2, lealtad
#   outputs/data/post_boric_kast/textos_discursos.rds    discursos en Sala
#
# DECISIONES
#   - El distrito se lee de las frases de la reseña que dicen "asume ...
#     diputado por el ... Distrito ... período AAAA". Si hay dos, manda la que
#     nombra el período.
#   - Las dos fuentes de militancia se muestran juntas. No coinciden en las
#     fechas del paso de Kast a independiente ni en el partido de Boric en
#     2018-2020, y no se elige una.
#   - La actividad de Labor es la de toda la persona, incluida la presidencia
#     (los registros de 2022 en adelante de Boric y de 2026 de Kast son pocos).
#
# ESCRIBE (outputs/data/post_boric_kast/)
#   biografia.rds   lista con periodos, militancias, labor_tipos y labor_anno
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(congresochile)
})

DIR <- "outputs/data/post_boric_kast"
PERSONAS <- tibble(quien = c("Kast", "Boric"), nombre = c("Kast Rist", "Boric Font"),
                   id_bcn = c("2231", "4536"), id_camara = c("857", "972"))

# --- Distrito por período, desde la reseña ------------------------------------
bios <- readRDS("data/bcn_biografias.rds")
distritos <- bind_rows(lapply(seq_len(nrow(PERSONAS)), function(i) {
  txt <- bios$texto_biografia[which(bios$id_persona_bcn == PERSONAS$id_bcn[i])][1]
  frases <- str_extract_all(txt, "[^.\\n]*asume[^.\\n]*diputado por el[^.\\n]*per[ií]odo[^.\\n]*")[[1]]
  tibble(quien = PERSONAS$quien[i],
         distrito = as.integer(str_extract(frases, "(?<=Distrito N[º°] )\\d+|\\d+(?=[º°] Distrito)")),
         inicio = as.integer(str_extract(frases, "(?<=per[ií]odo )\\d{4}")))
})) |>
  filter(!is.na(distrito), !is.na(inicio)) |>
  distinct(quien, inicio, .keep_all = TRUE) |>
  mutate(periodo = paste0(inicio, "-", inicio + 4)) |>
  select(quien, periodo, distrito)

# --- Partido según la Cámara y según la BCN ------------------------------------
mil_camara <- readRDS("data/militancias.rds") |>
  filter(id_parlamentario %in% PERSONAS$id_camara) |>
  left_join(PERSONAS |> select(quien, id_camara), by = c("id_parlamentario" = "id_camara")) |>
  transmute(quien, fuente = "Cámara", partido = partido_nombre, inicio = fecha_inicio, fin = fecha_termino)
mil_bcn <- bind_rows(lapply(seq_len(nrow(PERSONAS)), function(i) {
  bcn_trayectoria(PERSONAS$id_bcn[i])$militancias |>
    transmute(quien = PERSONAS$quien[i], fuente = "BCN", partido, inicio, fin)
}))
militancias <- bind_rows(mil_camara, mil_bcn) |> arrange(quien, fuente, inicio)

# --- Por período: distrito, bloque, posición, discursos ------------------------
dp <- readRDS(file.path(DIR, "diputados_periodo.rds")) |>
  filter(id_parlamentario %in% PERSONAS$id_camara) |>
  left_join(PERSONAS |> select(quien, id_camara), by = c("id_parlamentario" = "id_camara")) |>
  select(quien, periodo, partido, bloque, d1_pct, d2_pct, extremo_pct, leal_bloque, participacion)
disc <- readRDS(file.path(DIR, "textos_discursos.rds")) |>
  filter(!is.na(quien)) |>
  group_by(quien, periodo) |>
  summarise(discursos = n(), palabras = sum(palabras), .groups = "drop")
periodos <- dp |> left_join(distritos, by = c("quien", "periodo")) |>
  left_join(disc, by = c("quien", "periodo")) |>
  arrange(quien, periodo)

# --- Labor Parlamentaria -------------------------------------------------------
labor <- lapply(seq_len(nrow(PERSONAS)), function(i) labor_facetas(personas = PERSONAS$id_bcn[i]))
labor_tipos <- bind_rows(lapply(seq_along(labor), function(i) {
  labor[[i]]$tipos |> transmute(quien = PERSONAS$quien[i], tipo = descripcion, id_tipo = id, total = as.integer(total))
})) |> arrange(quien, desc(total))
labor_anno <- bind_rows(lapply(seq_along(labor), function(i) {
  labor[[i]]$annos |> mutate(quien = PERSONAS$quien[i], .before = 1)
})) |> filter(total > 0) |> arrange(quien, anno)

print.data.frame(periodos); print.data.frame(militancias)
print.data.frame(labor_tipos |> group_by(quien) |> slice_max(total, n = 8) |> ungroup())

saveRDS(list(periodos = periodos, militancias = militancias, labor_tipos = labor_tipos, labor_anno = labor_anno),
        file.path(DIR, "biografia.rds"))
