# =============================================================================
# 02_materias.R — materia de cada votación por el título del proyecto, y
# votos contra el propio bloque por materia.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/, después de
# 01_base.R y de post_bcall_periodos/R/04_seguridad.R:
#   Rscript post_boric_kast/R/02_materias.R
#
# MATERIAS (no excluyentes; un proyecto puede caer en varias)
#   seguridad   el diccionario congelado en post_bcall_periodos/R/04_seguridad.R
#   economia    impuestos, presupuesto, pensiones, trabajo, salarios, bonos,
#               banca y crédito, consumidores, comercio exterior
#   valorico    aborto, matrimonio y unión civil, identidad de género,
#               eutanasia, cannabis, divorcio, libertad religiosa
#   politico    elecciones, partidos, plebiscitos y proceso constituyente,
#               reglas del Congreso, dieta, reelección, financiamiento
#               de la política
#
# VALIDACIÓN, SIN CONTAMINAR
#   Igual que seguridad. Referencia, los descriptores de la BCN clasificados
#   con un diccionario por materia fijado antes de mirar los títulos. Misma
#   partición al azar (semilla 20261024). Los diccionarios de títulos se
#   ajustaron mirando solo la mitad de desarrollo y se congelaron el
#   2026-10-24; la mitad de prueba se mide una vez acá. Se mide acuerdo
#   entre dos codificaciones automáticas, no exactitud.
#
# ESCRIBE (outputs/data/post_boric_kast/)
#   materias_boletin.rds      una fila por boletín votado
#   validacion_materias.rds   acuerdo en la muestra de prueba
#   contra_bloque_materia.rds tasa de votos contra el bloque por materia
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
})

DIR <- "outputs/data/post_boric_kast"
norm <- function(x) x %>% str_to_lower() %>% stringi::stri_trans_general("Latin-ASCII") %>% str_squish()


# ---------------------------------------------------------------------------
# PASO 1 — diccionarios (congelados el 2026-10-24)
# ---------------------------------------------------------------------------
SEG_TIT <- paste0(
  "codigo penal|procesal penal|procedimiento penal|responsabilidad penal|\\bpenal(es)?\\b|\\bpenas?\\b|",
  "delit|delincuen|carabiner|policia de investigaciones|\\bpolicias?\\b|gendarmer|ministerio publico|",
  "fiscal nacional|defensoria penal|\\barmas?\\b|municion|explosiv|terroris|droga|estupefac|narco|",
  "\\brobos?\\b|hurto|homicid|femicid|secuestro|abigeato|agravante|prision|carcel|penitenciari|",
  "control (preventivo )?de identidad|seguridad (ciudadana|publica)|orden publico|crimen organizado|",
  "criminal|lavado de activos|flagran|usurpaci|receptaci|sicari|incivil|",
  "libertad (condicional|provisional|vigilada)|reincidencia")
SEG_EXCL <- paste0(
  "seguridad social|fiscalia nacional economica|policia local|lesa humanidad|genocidio|tortura|",
  "prision politica|detenidos desaparecidos|derechos humanos|11 de septiembre|aborto|",
  "minas u otros artefactos|",
  "^(proyecto de acuerdo que )?aprueba (el |la |los |las )?(convenio|convencion|tratado|acuerdo|protocolo|enmienda|estatuto)|",
  "^acuerdo entre")

TIT <- list(
  economia = paste0(
    "impuest|tribut|presupuest|pension|previsi|\\bafp\\b|codigo del trabajo|trabajador|laboral|remuneraci|",
    "salario|sueldo|sindica|negociacion colectiva|empleo|jornada|reajust|royalty|banco|bancari|financier|",
    "credito|deuda|libre competencia|consumidor|arancel|seguro de cesantia|subsidio|\\bbonos?\\b|bonificaci|",
    "aguinaldo|ingreso minimo|pyme|emprend|postnatal|permiso parental|libre comercio|integracion comercial|",
    "doble imposicion|asociacion economica"),
  valorico = paste0(
    "aborto|interrupcion.{0,12}embarazo|matrimonio|union civil|identidad de genero|diversidad sexual|",
    "orientacion sexual|eutanasia|muerte digna|acelerar la muerte|muerte asistida|cannabis|marihuana|",
    "autocultivo|divorcio|19\\.?947|objecion de conciencia|libertad religiosa|cultos|educacion sexual|",
    "anticoncep|pildora"),
  politico = paste0(
    "\\belecci|\\belector|votacion(es)? popular|escrutinio|sufragio|voto (obligatorio|voluntario)|",
    "inscripcion automatica|partidos politicos|plebiscit|constituyente|nueva constitucion|dieta parlamentaria|",
    "reeleccion|financiamiento de la politica|gasto electoral|primarias|gobernador regional|consejeros regionales|",
    "acusacion constitucional|parlamentari|reglamento de la camara|congreso nacional")
)
EXCL <- list(economia = "^(proyecto de acuerdo que )?aprueba (el |la |los |las )?(convenio|convencion)(?!.*(comercio|doble imposicion))",
              valorico = "^$",
              politico = "libre eleccion|bomberos")

DESC <- list(
  economia = "impuesto|tribut|presupuest|pension|prevision|fondos de pensiones|codigo del trabajo|trabajador|remuneraci|salario|sueldo|sindica|negociacion colectiva|empleo|jornada|reajuste|royalty|banco|financier|credito|deuda|libre competencia|consumidor|aranceles|comercio exterior|seguro de cesantia|subsidio|bono",
  valorico = "aborto|interrupcion del embarazo|matrimonio|union civil|identidad de genero|homosexual|diversidad sexual|eutanasia|muerte digna|cannabis|marihuana|divorcio|objecion de conciencia|libertad religiosa|cultos|educacion sexual|anticoncep",
  politico = "eleccion|electoral|votacion|votaciones populares|partidos politicos|plebiscit|constituyente|reforma constitucional|constitucion 1980|parlamentarios|congreso nacional|camara de diputados|senado|dieta|reeleccion|financiamiento electoral|gasto electoral|gobierno regional|gobernador"
)

clasificar <- function(titulo, boletin) {
  t <- norm(coalesce(titulo, ""))
  tibble(
    seguridad = (str_detect(t, SEG_TIT) & !str_detect(t, SEG_EXCL)) | str_detect(boletin, "-25$"),
    economia = str_detect(t, TIT$economia) & !str_detect(t, regex(EXCL$economia)),
    valorico = str_detect(t, TIT$valorico),
    politico = str_detect(t, TIT$politico) & !str_detect(t, EXCL$politico)
  )
}


# ---------------------------------------------------------------------------
# PASO 2 — boletines votados
# ---------------------------------------------------------------------------
proy <- read_parquet("observatorio/parquet_respaldo_20260913/proyecto.parquet") %>% select(boletin, titulo)
vb <- read_rds(file.path(DIR, "votaciones.rds"))
bol <- vb %>% distinct(boletin) %>% filter(!is.na(boletin)) %>% left_join(proy, by = "boletin")
bol <- bind_cols(bol, clasificar(bol$titulo, bol$boletin))
cat("boletines votados:", nrow(bol), "| con título:", sum(!is.na(bol$titulo)), "\n")
print(colSums(bol[, c("seguridad", "economia", "valorico", "politico")]))


# ---------------------------------------------------------------------------
# PASO 3 — validación en la muestra de prueba (una sola medición)
# ---------------------------------------------------------------------------
mat <- read_csv("outputs/data/materias_boletines.csv", show_col_types = FALSE) %>% mutate(m = norm(materia))
ref <- mat %>% group_by(boletin) %>%
  summarise(across(m, list(economia = ~ any(str_detect(.x, DESC$economia)),
                           valorico = ~ any(str_detect(.x, DESC$valorico)),
                           politico = ~ any(str_detect(.x, DESC$politico))), .names = "{.fn}"), .groups = "drop")
vot <- read_rds("outputs/data/post_bcall/votaciones_periodo.rds") %>%
  mutate(boletin = str_extract(numero_boletin, "^[0-9]+-[0-9]+"))
d <- vot %>% distinct(boletin, periodo) %>% left_join(proy, by = "boletin") %>%
  distinct(boletin, titulo) %>% inner_join(mat %>% distinct(boletin), by = "boletin") %>% filter(!is.na(titulo))
set.seed(20261024)
d <- d %>% mutate(muestra = if_else(runif(n()) < 0.5, "desarrollo", "prueba"))
prueba <- d %>% filter(muestra == "prueba")
prueba <- bind_cols(prueba, clasificar(prueba$titulo, prueba$boletin) %>% select(-seguridad)) %>%
  left_join(ref %>% rename_with(~ paste0("ref_", .x), -boletin), by = "boletin")
valid <- map_dfr(c("economia", "valorico", "politico"), function(k) {
  t <- prueba[[k]]; r <- prueba[[paste0("ref_", k)]]
  po <- mean(t == r); pe <- mean(t) * mean(r) + mean(!t) * mean(!r)
  tibble(materia = k, boletines = nrow(prueba), ref = sum(r), titulo = sum(t), ambos = sum(t & r),
         acuerdo = po, kappa = (po - pe) / (1 - pe), recupera = sum(t & r) / sum(r), coincide = sum(t & r) / sum(t))
})
cat("\n== validación, muestra de prueba ==\n"); print(as.data.frame(valid), digits = 3)


# ---------------------------------------------------------------------------
# PASO 4 — votos contra el propio bloque, por materia
# ---------------------------------------------------------------------------
# Para cada diputado y período, proporción de sus votos emitidos que van contra
# el lado de su bloque, en las votaciones de cada materia. Boric y Kast se
# comparan con la mediana de la Cámara en la misma materia y período.
votos <- read_rds("outputs/data/post_bcall/votos_periodo.rds") %>% filter(!is.na(voto_num))
dp <- read_rds(file.path(DIR, "diputados_periodo.rds"))
lado <- vb %>% select(id_votacion, boletin, lado_left, lado_right)
larga <- votos %>%
  inner_join(dp %>% select(periodo, id_parlamentario, nombre_completo, bloque), by = c("periodo", "id_parlamentario")) %>%
  inner_join(lado, by = "id_votacion") %>%
  mutate(lado_bloque = if_else(bloque == "left", lado_left, lado_right)) %>%
  filter(!is.na(lado_bloque)) %>%
  mutate(contra = voto_num != lado_bloque) %>%
  left_join(bol %>% select(boletin, seguridad, economia, valorico, politico), by = "boletin") %>%
  mutate(across(c(seguridad, economia, valorico, politico), ~ coalesce(.x, FALSE)),
         otras = !(seguridad | economia | valorico | politico))

tasas <- map_dfr(c("seguridad", "economia", "valorico", "politico", "otras"), function(k) {
  larga %>% filter(.data[[k]]) %>%
    group_by(periodo, id_parlamentario, nombre_completo) %>%
    summarise(votos = n(), contra = mean(contra), .groups = "drop") %>%
    mutate(materia = k)
})
ref_camara <- tasas %>% filter(votos >= 20) %>% group_by(periodo, materia) %>%
  summarise(mediana_camara = median(contra), p90_camara = quantile(contra, 0.9), .groups = "drop")
bk <- tasas %>% filter(str_detect(nombre_completo, "^(Boric Font|Kast Rist),")) %>%
  mutate(persona = if_else(str_detect(nombre_completo, "^Boric"), "Boric", "Kast")) %>%
  left_join(ref_camara, by = c("periodo", "materia")) %>%
  select(persona, periodo, materia, votos, contra, mediana_camara, p90_camara)
cat("\n"); print(as.data.frame(bk %>% mutate(across(where(is.double), ~ round(.x, 3)))))

write_rds(bol, file.path(DIR, "materias_boletin.rds"))
write_rds(valid, file.path(DIR, "validacion_materias.rds"))
write_rds(bk, file.path(DIR, "contra_bloque_materia.rds"))
write_rds(ref_camara, file.path(DIR, "contra_bloque_camara.rds"))
cat("\nescrito en", DIR, "\n")
