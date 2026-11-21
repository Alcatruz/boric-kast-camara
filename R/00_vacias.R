# Palabras que no cuentan en el análisis de texto: las vacías del castellano
# (stopwords-iso) y las fórmulas de la Sala. La usan 04_textos.R y
# 08_texto_medidas.R.
SALA <- c("señor", "señora", "señores", "señorita", "presidente", "presidenta", "diputado", "diputada", "diputados",
          "diputadas", "honorable", "honorables", "colega", "colegas", "sala", "proyecto", "proyectos", "ley", "leyes",
          "artículo", "artículos", "inciso", "número", "boletín", "cámara", "corporación", "sesión", "palabra",
          "intervención", "indicación", "indicaciones", "votación", "votar", "voto", "votos", "comisión", "comisiones",
          "gracias", "muchas", "presidencia", "mesa", "minutos", "tiempo", "intermedio", "su", "don", "doña",
          "particular", "general", "señoría", "señorías")
# Palabras de número, que no son contenido. Se agregaron el 21-11-2026 para
# 08_texto_medidas.R; la corrida publicada de 04_textos.R es anterior.
NUMEROS <- c("mil", "miles", "millón", "millones")
vacias <- unique(c(stopwords::stopwords("es", source = "stopwords-iso"), SALA, NUMEROS))
