# =============================================================================
# 01_base.R — lealtad, polarización, carreras y votos de Boric y Kast.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de los
# scripts 01 y 02 de post_bcall_periodos/:
#   Rscript post_boric_kast/R/01_base.R
#
# FUENTES
#   post_bcall_periodos/datos/bcall_periodos.csv   d1, d2 y bloque por período
#   outputs/data/post_bcall/votos_periodo.rds      votos nominales 2002-2026
#   outputs/data/post_bcall/votaciones_periodo.rds boletín de cada votación
#   data/bcn_personas.rds, data/bcn_mandatos.rds   cargos posteriores (BCN)
#
# DEFINICIONES
#   lado de un grupo en una votación   la opción (a favor o en contra) que
#                 eligió la mayoría de sus miembros que votaron a favor o en
#                 contra. Si empatan, no hay lado y la votación no cuenta.
#   lealtad       proporción de los votos emitidos de un diputado que van con
#                 el lado de su grupo. Abstenerse cuando el grupo tiene lado
#                 cuenta como no ir con él.
#   bloque        el bloque B-Call de la estimación del período.
#   partido       el vigente al inicio del período; solo partidos con 3 o más
#                 diputados votando en esa votación; independientes fuera.
#   bloque contra bloque  votación en que el lado del bloque de izquierda y
#                 el del de derecha son opuestos.
#   Rice          |a favor - en contra| / (a favor + en contra) por partido y
#                 votación, promediado sobre las votaciones del período.
#   carrera       diputado que dejó la Cámara a más tardar en 2026 (último
#                 período que empieza en 2022 o antes) y asumió después como
#                 senador o presidente. Los que siguen en la Cámara en
#                 2026-2030 quedan fuera, porque su carrera no terminó.
#
# ESCRIBE (outputs/data/post_boric_kast/)
#   diputados_periodo.rds  una fila por diputado y período
#   votos_bk.rds           cada voto de Boric y Kast con su bloque y partido
#   votaciones.rds         una fila por votación, con bloque contra bloque
#   rice.rds               Rice por partido y período
#   carreras.rds           una fila por diputado con carrera terminada
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

DIR_OUT <- "outputs/data/post_boric_kast"
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)

est <- read_csv("post_bcall_periodos/datos/bcall_periodos.csv", show_col_types = FALSE,
                col_types = cols(id_parlamentario = "c")) %>%
  group_by(periodo) %>%
  mutate(d1_pct = percent_rank(d1), d2_pct = percent_rank(d2),
         extremo_pct = percent_rank(abs(d1 - median(d1)))) %>%
  ungroup() %>%
  mutate(ini = as.integer(substr(periodo, 1, 4)))

votos <- read_rds("outputs/data/post_bcall/votos_periodo.rds") %>% filter(!is.na(voto_num))
vot   <- read_rds("outputs/data/post_bcall/votaciones_periodo.rds") %>%
  transmute(id_votacion, fecha, boletin = str_extract(numero_boletin, "^[0-9]+-[0-9]+"))

v <- votos %>%
  inner_join(est %>% select(periodo, id_parlamentario, bloque, partido), by = c("periodo", "id_parlamentario"))
cat("votos emitidos de diputados estimados:", nrow(v), "\n")


# ---------------------------------------------------------------------------
# PASO 1 — lado de cada bloque y de cada partido en cada votación
# ---------------------------------------------------------------------------
lado <- function(si, no) case_when(si > no ~ 1, no > si ~ -1, TRUE ~ NA_real_)

lado_bloque <- v %>% group_by(id_votacion, bloque) %>%
  summarise(si = sum(voto_num == 1), no = sum(voto_num == -1), .groups = "drop") %>%
  mutate(lado_bloque = lado(si, no))

lado_partido <- v %>% filter(partido != "IND") %>% group_by(id_votacion, partido) %>%
  filter(sum(voto_num != 0) >= 3) %>%
  summarise(si = sum(voto_num == 1), no = sum(voto_num == -1), .groups = "drop") %>%
  mutate(lado_partido = lado(si, no), rice = abs(si - no) / (si + no))

v <- v %>%
  left_join(lado_bloque %>% select(id_votacion, bloque, lado_bloque), by = c("id_votacion", "bloque")) %>%
  left_join(lado_partido %>% select(id_votacion, partido, lado_partido), by = c("id_votacion", "partido"))


# ---------------------------------------------------------------------------
# PASO 2 — lealtad por diputado y período
# ---------------------------------------------------------------------------
leal <- v %>% group_by(periodo, id_parlamentario) %>%
  summarise(leal_bloque = mean(voto_num[!is.na(lado_bloque)] == lado_bloque[!is.na(lado_bloque)]),
            leal_partido = if (any(!is.na(lado_partido)))
              mean(voto_num[!is.na(lado_partido)] == lado_partido[!is.na(lado_partido)]) else NA_real_,
            .groups = "drop") %>%
  group_by(periodo) %>% mutate(leal_bloque_pct = percent_rank(leal_bloque)) %>% ungroup()

dp <- est %>% left_join(leal, by = c("periodo", "id_parlamentario"))
cat("correlación d2 y lealtad al bloque, por período:\n")
print(as.data.frame(dp %>% group_by(periodo) %>% summarise(r = round(cor(d2, leal_bloque), 2))))


# ---------------------------------------------------------------------------
# PASO 3 — votaciones de bloque contra bloque y Rice
# ---------------------------------------------------------------------------
vb <- lado_bloque %>% select(id_votacion, bloque, lado_bloque) %>%
  pivot_wider(names_from = bloque, values_from = lado_bloque, names_prefix = "lado_") %>%
  mutate(bloque_contra_bloque = coalesce(lado_left * lado_right == -1, FALSE)) %>%
  left_join(votos %>% distinct(id_votacion, periodo), by = "id_votacion") %>%
  left_join(vot, by = "id_votacion")
print(as.data.frame(vb %>% group_by(periodo) %>%
  summarise(votaciones = n(), bloque_contra_bloque = round(100 * mean(bloque_contra_bloque), 1))))

rice <- lado_partido %>%
  left_join(votos %>% distinct(id_votacion, periodo), by = "id_votacion") %>%
  group_by(periodo, partido) %>%
  summarise(votaciones = n(), rice = mean(rice), .groups = "drop")


# ---------------------------------------------------------------------------
# PASO 4 — los votos de Boric y Kast
# ---------------------------------------------------------------------------
bk_ids <- est %>% filter(str_detect(nombre_completo, "^(Boric Font|Kast Rist),")) %>%
  distinct(id_parlamentario, nombre_completo)
print(bk_ids)
votos_bk <- v %>% filter(id_parlamentario %in% bk_ids$id_parlamentario) %>%
  left_join(bk_ids, by = "id_parlamentario") %>%
  left_join(vot, by = "id_votacion") %>%
  mutate(persona = if_else(str_detect(nombre_completo, "^Boric"), "Boric", "Kast"),
         contra_bloque = !is.na(lado_bloque) & voto_num != lado_bloque)
print(as.data.frame(votos_bk %>% group_by(persona, periodo) %>%
  summarise(votos = n(), contra_bloque = sum(contra_bloque), pct = round(100 * mean(contra_bloque[!is.na(lado_bloque)]), 1),
            .groups = "drop")))


# ---------------------------------------------------------------------------
# PASO 5 — carreras
# ---------------------------------------------------------------------------
personas <- readRDS("data/bcn_personas.rds") %>% filter(!is.na(id_camara)) %>%
  transmute(id_parlamentario = as.character(id_camara), id_bcn)
mandatos <- readRDS("data/bcn_mandatos.rds") %>%
  filter(cargo %in% c("Senador", "Presidente De La República")) %>%
  mutate(anno = as.integer(format(as.Date(fecha_inicio), "%Y")))

ultimo <- dp %>% group_by(id_parlamentario) %>% slice_max(ini, n = 1, with_ties = FALSE) %>% ungroup()
carreras <- ultimo %>% filter(ini <= 2022) %>%
  left_join(personas, by = "id_parlamentario")
cat("\ndiputados con carrera terminada en la Cámara:", nrow(carreras), "| sin id BCN:", sum(is.na(carreras$id_bcn)), "\n")
sube <- mandatos %>% inner_join(carreras %>% select(id_bcn, ini), by = "id_bcn") %>%
  filter(anno >= ini + 4) %>%
  group_by(id_bcn) %>%
  summarise(destino = if_else(any(cargo == "Presidente De La República"), "Presidencia", "Senado"), .groups = "drop")
carreras <- carreras %>% left_join(sube, by = "id_bcn") %>%
  mutate(destino = coalesce(destino, "Ninguno"), sube = destino != "Ninguno")
print(table(carreras$destino, carreras$periodo))

write_rds(dp, file.path(DIR_OUT, "diputados_periodo.rds"))
write_rds(votos_bk, file.path(DIR_OUT, "votos_bk.rds"))
write_rds(vb, file.path(DIR_OUT, "votaciones.rds"))
write_rds(rice, file.path(DIR_OUT, "rice.rds"))
write_rds(carreras, file.path(DIR_OUT, "carreras.rds"))
cat("\nescrito en", DIR_OUT, "\n")
