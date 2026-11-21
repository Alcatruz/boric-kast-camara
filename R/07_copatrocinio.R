# =============================================================================
# 07_copatrocinio.R — con quién firmaban sus mociones Boric y Kast, comparado
# con el resto de la Cámara de cada período.
#
# Se corre desde la RAÍZ del proyecto bcall_nominate_chile/:
#   Rscript post_boric_kast/R/07_copatrocinio.R
#
# FUENTES
#   autor.parquet y proyecto.parquet del registro legislativo local
#   (C:/Users/dalca/registro_legislativo_datos/parquet), que cruza la ficha
#   de tramitación del Senado con opendata de la Cámara
#   post_bcall_periodos/datos/bcall_periodos.csv  bloque B-Call por período
#
# DECISIONES
#   - Mociones de origen en la Cámara, por fecha de ingreso, del 11-03-2002 al
#     10-03-2022.
#   - Firmantes. Solo filas de diputados con id de la Cámara. La tabla de
#     autores tiene 1.560 filas sin id ni nombre, que son firmantes duplicados
#     al cruzar las dos fuentes; quedan fuera. El id se usa junto con la
#     cámara, porque cada cámara numera por separado.
#   - Cobertura. La tabla trae 126 mociones de Boric y 255 de Kast; Labor
#     Parlamentaria registra 126 y 256.
#   - El bloque de cada firmante es su bloque B-Call en el período de la
#     moción. Los firmantes sin estimación en ese período (reemplazos con poca
#     participación) cuentan como firmantes pero no entran en las medidas de
#     bloque.
#
# MEDIDAS, por diputado y período
#   mociones        mociones que firmó
#   solo            proporción que firmó sin nadie más
#   cofirmantes     promedio de otros firmantes por moción
#   transversales   proporción de sus mociones con al menos un cofirmante
#                   del otro bloque
#   otro_bloque     proporción de sus cofirmantes, contados por moción, que
#                   son del otro bloque
#   distintos       cofirmantes distintos en el período
#   Para comparar, cada medida va con su percentil entre los diputados con
#   al menos 5 mociones en el período.
#
# ESCRIBE (outputs/data/post_boric_kast/)
#   copatrocinio.rds   lista con por_diputado, bk y socios
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

DIR <- "outputs/data/post_boric_kast"
P <- "C:/Users/dalca/registro_legislativo_datos/parquet"
BK <- c("857" = "Kast", "972" = "Boric")
PERIODOS <- tibble(periodo = c("2002-2006", "2006-2010", "2010-2014", "2014-2018", "2018-2022"),
                   inicio = as.Date(c("2002-03-11", "2006-03-11", "2010-03-11", "2014-03-11", "2018-03-11")),
                   fin    = as.Date(c("2006-03-10", "2010-03-10", "2014-03-10", "2018-03-10", "2022-03-10")))

proy <- arrow::read_parquet(file.path(P, "proyecto.parquet")) |>
  filter(iniciativa == "Moción", camara_origen == "D") |>
  transmute(boletin, ingreso = as.Date(fecha_ingreso)) |>
  left_join(PERIODOS, by = join_by(between(ingreso, inicio, fin))) |>
  filter(!is.na(periodo)) |> select(boletin, periodo)

autores <- arrow::read_parquet(file.path(P, "autor.parquet")) |>
  filter(tipo_parlamentario == "diputado", !is.na(id_parlamentario)) |>
  transmute(boletin, id = as.character(id_parlamentario)) |>
  distinct() |>
  inner_join(proy, by = "boletin")

bloques <- readr::read_csv("post_bcall_periodos/datos/bcall_periodos.csv", show_col_types = FALSE,
                           col_types = readr::cols(id_parlamentario = "c")) |>
  transmute(periodo, id = id_parlamentario, bloque, nombre_completo, partido)

autores <- autores |> left_join(bloques |> select(periodo, id, bloque), by = c("periodo", "id"))
cat("mociones de la Cámara 2002-2022 con firmantes:", n_distinct(autores$boletin), "; firmas:", nrow(autores), "\n")

# Pares: cada firmante con cada otro firmante de la misma moción
pares <- autores |>
  inner_join(autores |> select(boletin, id_otro = id, bloque_otro = bloque), by = "boletin", relationship = "many-to-many") |>
  filter(id != id_otro)

por_moc <- autores |>
  left_join(pares |> group_by(boletin, id) |>
              summarise(n_co = n(), n_co_bloque = sum(!is.na(bloque) & !is.na(bloque_otro)),
                        n_otro = sum(!is.na(bloque) & !is.na(bloque_otro) & bloque != bloque_otro), .groups = "drop"),
            by = c("boletin", "id")) |>
  mutate(across(c(n_co, n_co_bloque, n_otro), ~ coalesce(.x, 0L)))

por_diputado <- por_moc |>
  group_by(periodo, id) |>
  summarise(bloque = first(bloque), mociones = n(), solo = mean(n_co == 0), cofirmantes = mean(n_co),
            transversales = mean(n_otro > 0),
            otro_bloque = if (sum(n_co_bloque) > 0) sum(n_otro) / sum(n_co_bloque) else NA_real_,
            .groups = "drop") |>
  left_join(pares |> group_by(periodo, id) |> summarise(distintos = n_distinct(id_otro), .groups = "drop"),
            by = c("periodo", "id")) |>
  mutate(distintos = coalesce(distintos, 0L)) |>
  left_join(bloques |> select(periodo, id, nombre_completo, partido), by = c("periodo", "id"))

# Percentiles entre los diputados con al menos 5 mociones y bloque en el período
ref <- por_diputado |> filter(mociones >= 5, !is.na(bloque))
pct_en <- function(x, ref_x) vapply(x, function(v) if (is.na(v)) NA_real_ else mean(ref_x <= v, na.rm = TRUE), numeric(1))
bk <- por_diputado |> filter(id %in% names(BK)) |>
  mutate(quien = unname(BK[id])) |>
  rowwise() |>
  mutate(across(c(mociones, solo, cofirmantes, transversales, otro_bloque, distintos),
                ~ pct_en(.x, ref[[cur_column()]][ref$periodo == periodo]), .names = "{.col}_pct")) |>
  ungroup() |>
  arrange(quien, periodo)

medianas <- ref |> group_by(periodo) |>
  summarise(diputados = n(), mociones = median(mociones), solo = median(solo), cofirmantes = median(cofirmantes),
            transversales = median(transversales), otro_bloque = median(otro_bloque, na.rm = TRUE),
            distintos = median(distintos), .groups = "drop")

# Socios: con quién firmó más cada uno, por período
socios <- pares |> filter(id %in% names(BK)) |>
  count(quien = unname(BK[id]), periodo, id_otro, bloque_otro, name = "mociones_juntos") |>
  left_join(bloques |> select(periodo, id_otro = id, nombre_completo, partido), by = c("periodo", "id_otro")) |>
  group_by(quien, periodo) |> slice_max(mociones_juntos, n = 8, with_ties = FALSE) |> ungroup()

r2 <- function(x) round(x, 2)
print.data.frame(bk |> select(quien, periodo, partido, mociones, solo, cofirmantes, transversales, otro_bloque, distintos) |>
                   mutate(across(where(is.double), r2)))
print.data.frame(bk |> select(quien, periodo, ends_with("_pct")) |> mutate(across(where(is.double), r2)))
print.data.frame(medianas |> mutate(across(where(is.double), r2)))
print.data.frame(socios |> select(quien, periodo, nombre_completo, partido, bloque_otro, mociones_juntos))

saveRDS(list(por_diputado = por_diputado, bk = bk, medianas = medianas, socios = socios),
        file.path(DIR, "copatrocinio.rds"))
