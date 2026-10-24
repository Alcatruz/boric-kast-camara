# =============================================================================
# 03_indicadores.R — polarización, Rice, modelo de carreras y datos para la
# página.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 01_base.R y 02_materias.R:
#   Rscript post_boric_kast/R/03_indicadores.R
#
# MODELO DE CARRERAS
#   glm logit. Variable dependiente, haber asumido después como senador o
#   presidente. Predictores medidos en el último período del diputado en la
#   Cámara, en percentiles dentro de ese período para que sean comparables
#   entre estimaciones separadas. extremo_pct es la distancia de d1 a la
#   mediana de la Cámara. Efectos fijos por último período, porque el número
#   de cupos al Senado cambia según el calendario de renovación.
#
# ESCRIBE
#   post_boric_kast/datos/*.csv y post_boric_kast/datos/datos.js
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

DIR  <- "outputs/data/post_boric_kast"
REPO <- "post_boric_kast/datos"
dir.create(REPO, showWarnings = FALSE)

dp       <- read_rds(file.path(DIR, "diputados_periodo.rds"))
vb       <- read_rds(file.path(DIR, "votaciones.rds"))
rice     <- read_rds(file.path(DIR, "rice.rds"))
carreras <- read_rds(file.path(DIR, "carreras.rds"))
bk_mat   <- read_rds(file.path(DIR, "contra_bloque_materia.rds"))
valid    <- read_rds(file.path(DIR, "validacion_materias.rds"))
r3 <- function(x) round(x, 3)

GOB <- tibble(periodo = c("2002-2006", "2006-2010", "2010-2014", "2014-2018", "2018-2022", "2022-2026", "2026-2030"),
              presidente = c("Lagos", "Bachelet", "Piñera", "Bachelet", "Piñera", "Boric", "Kast"))


# ---------------------------------------------------------------------------
# Polarización y Rice
# ---------------------------------------------------------------------------
polar <- vb %>% group_by(periodo) %>%
  summarise(votaciones = n(), bloque_contra_bloque = mean(bloque_contra_bloque), .groups = "drop") %>%
  left_join(dp %>% group_by(periodo) %>%
              summarise(leal_bloque_mediana = median(leal_bloque, na.rm = TRUE), .groups = "drop"), by = "periodo") %>%
  left_join(GOB, by = "periodo")
print(as.data.frame(polar %>% mutate(across(where(is.double), r3))))

PARTIDOS <- c("UDI", "RN", "PREP", "DC", "PPD", "PS", "PC", "FA")
rice_tab <- rice %>% filter(partido %in% PARTIDOS, votaciones >= 50) %>%
  select(periodo, partido, rice) %>% mutate(rice = r3(rice)) %>%
  pivot_wider(names_from = periodo, values_from = rice) %>%
  arrange(match(partido, PARTIDOS))
print(as.data.frame(rice_tab))


# ---------------------------------------------------------------------------
# Modelo de carreras
# ---------------------------------------------------------------------------
m <- glm(sube ~ extremo_pct + d2_pct + participacion + factor(periodo),
         family = binomial, data = carreras)
print(summary(m))
coefs <- summary(m)$coefficients[c("extremo_pct", "d2_pct", "participacion"), ]
modelo <- tibble(variable = rownames(coefs), coef = coefs[, 1], ee = coefs[, 2], p = coefs[, 4],
                 odds = exp(coefs[, 1]))
print(as.data.frame(modelo %>% mutate(across(where(is.double), r3))))
desc_carr <- carreras %>% group_by(destino) %>%
  summarise(n = n(), cuartil_d2_sup = mean(d2_pct >= 0.75), extremo_pct = mean(extremo_pct),
            d2_pct = mean(d2_pct), .groups = "drop")
print(as.data.frame(desc_carr %>% mutate(across(where(is.double), r3))))


# ---------------------------------------------------------------------------
# Boric y Kast
# ---------------------------------------------------------------------------
tray <- dp %>% filter(str_detect(nombre_completo, "^(Boric Font|Kast Rist),")) %>%
  mutate(persona = if_else(str_detect(nombre_completo, "^Boric"), "Boric", "Kast")) %>%
  select(persona, periodo, partido, bloque, d1, d1_pct, d2, d2_pct, leal_bloque, leal_bloque_pct, leal_partido)
print(as.data.frame(tray %>% mutate(across(where(is.double), r3))))

camara_1418 <- dp %>% filter(periodo == "2014-2018") %>%
  transmute(n = sub("^(.*), (.*)$", "\\2 \\1", nombre_completo), pa = partido, b = bloque,
            d1 = r3(d1), d2 = r3(d2), leal = r3(leal_bloque),
            bk = case_when(str_detect(nombre_completo, "^Boric Font,") ~ "Boric",
                           str_detect(nombre_completo, "^Kast Rist,") ~ "Kast", TRUE ~ ""))


# ---------------------------------------------------------------------------
# Exportar
# ---------------------------------------------------------------------------
write_csv(polar, file.path(REPO, "polarizacion.csv"))
write_csv(rice_tab, file.path(REPO, "rice_partidos.csv"))
write_csv(modelo, file.path(REPO, "modelo_carreras.csv"))
write_csv(tray, file.path(REPO, "trayectorias_boric_kast.csv"))
write_csv(bk_mat, file.path(REPO, "contra_bloque_materia.csv"))
write_csv(valid, file.path(REPO, "validacion_materias.csv"))
write_csv(carreras %>% transmute(nombre_completo, ultimo_periodo = periodo, partido, destino,
                                 extremo_pct = r3(extremo_pct), d2_pct = r3(d2_pct), participacion = r3(participacion)),
          file.path(REPO, "carreras.csv"))

datos <- list(
  polar = polar %>% mutate(across(where(is.double), r3)),
  rice = rice %>% filter(partido %in% PARTIDOS, votaciones >= 50) %>% mutate(rice = r3(rice)) %>% select(periodo, partido, rice),
  modelo = modelo %>% mutate(across(where(is.double), r3)),
  desc_carr = desc_carr %>% mutate(across(where(is.double), r3)),
  carreras = carreras %>% transmute(n = sub("^(.*), (.*)$", "\\2 \\1", nombre_completo), p = periodo, pa = partido,
                                    destino, x = r3(extremo_pct), y = r3(d2_pct)),
  tray = tray %>% mutate(across(where(is.double), r3)),
  camara_1418 = camara_1418,
  materia = bk_mat %>% mutate(across(where(is.double), r3)),
  validacion = valid %>% mutate(across(where(is.double), r3))
)
writeLines(c("// Generado por R/03_indicadores.R. No editar a mano.",
             paste0("window.BK = ", toJSON(datos, auto_unbox = TRUE, dataframe = "rows", na = "null"), ";")),
           file.path(REPO, "datos.js"), useBytes = TRUE)
cat("\nescrito", file.path(REPO, "datos.js"), round(file.size(file.path(REPO, "datos.js")) / 1024), "KB\n")
