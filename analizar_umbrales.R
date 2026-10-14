# ============================================================
# SIGIA · ANÁLISIS DEL HISTORIAL PARA AJUSTAR UMBRALES
#
#   Rscript analizar_umbrales.R                       -> Panajachel desde 2024-01-01
#   Rscript analizar_umbrales.R panajachel 2025-05-01 -> otra estación / fecha de inicio
#
# Descarga todo el historial de la estación, lo limpia y genera en analisis/<estación>/:
#   reporte.md               resumen y sugerencias
#   historial.csv            todas las lecturas limpias
#   maximos_diarios.png      máximo de cada día con los umbrales actuales
#   subidas_30min.png        distribución de subidas en 30 minutos
#
# Las sugerencias son estadísticas: los umbrales de alerta y peligro deben
# validarse en campo (capacidad del cauce, puentes, viviendas) con COMRED.
# ============================================================

options(sigia.no_main = TRUE)
carpeta <- function() {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f) == 1) dirname(normalizePath(f)) else getwd()
}
Sys.setenv(SIGIA_DIR = Sys.getenv("SIGIA_DIR", carpeta()))
source(file.path(Sys.getenv("SIGIA_DIR"), "sigia_monitor.R"), encoding = "UTF-8")

args  <- commandArgs(trailingOnly = TRUE)
id    <- if (length(args) >= 1) args[1] else "panajachel"
desde <- as.Date(if (length(args) >= 2) args[2] else "2024-01-01")

est <- Filter(function(e) e$id == id, lapply(ESTACIONES, preparar_estacion))
if (length(est) == 0) stop("No existe la estación '", id, "' en config.R")
est <- est[[1]]
if (any(is.na(c(est$umbral_preventivo, est$umbral_alerta, est$umbral_peligro)))) {
  log_message("La estación no tiene umbrales: se usan 2.00 / 2.50 / 4.75 solo como referencia")
  est$umbral_preventivo <- 2.00; est$umbral_alerta <- 2.50; est$umbral_peligro <- 4.75
}
salida <- file.path(Sys.getenv("SIGIA_DIR"), "analisis", id)
dir.create(salida, recursive = TRUE, showWarnings = FALSE)

# ---------- 1. Descarga por tramos ----------
feeds_df <- function(d) {
  if (is.null(d) || length(d$feeds) == 0) return(NULL)
  data.frame(fecha  = do.call(c, lapply(d$feeds, function(f) parse_fecha(f$created_at %||% NA))),
             altura = vapply(d$feeds, function(f) suppressWarnings(as.numeric(f$field1 %||% NA)), numeric(1)))
}
bajar_tramo <- function(ini, fin) {
  d <- http_json(paste0(est$base_url, est$datos),
                 list(start = paste(ini, "00:00:00"), end = paste(fin, "23:59:59"), results = 8000))
  df <- feeds_df(d)
  if (!is.null(df) && nrow(df) >= 8000 && fin > ini) {          # tramo lleno: partirlo en dos
    mitad <- ini + floor(as.numeric(fin - ini) / 2)
    return(rbind(bajar_tramo(ini, mitad), bajar_tramo(mitad + 1, fin)))
  }
  df
}
tramos <- seq(desde, Sys.Date(), by = "15 days")
partes <- list()
for (i in seq_along(tramos)) {
  ini <- tramos[i]; fin <- min(ini + 14, Sys.Date())
  cat(sprintf("\rDescargando %s a %s (%d/%d)", ini, fin, i, length(tramos)))
  partes[[i]] <- bajar_tramo(ini, fin)
}
cat("\n")
crudo <- do.call(rbind, Filter(Negate(is.null), partes))
if (is.null(crudo) || nrow(crudo) == 0) stop("No se descargaron datos. Revise la estación y la fecha de inicio.")
crudo <- crudo[!is.na(crudo$fecha) & !is.na(crudo$altura), ]
crudo <- crudo[order(crudo$fecha), ]; crudo <- crudo[!duplicated(crudo$fecha), ]
df <- limpiar(crudo, est)
desc <- attr(df, "descartes")
attr(df$fecha, "tzone") <- TZ
write.csv(data.frame(fecha = format(df$fecha, "%Y-%m-%d %H:%M:%S", tz = TZ), altura_m = round(df$altura, 3)),
          file.path(salida, "historial.csv"), row.names = FALSE)

# ---------- 2. Estadísticas ----------
a <- df$altura; t <- df$fecha
intervalo <- median(diff(as.numeric(t))) / 60
horas_total <- as.numeric(difftime(max(t), min(t), units = "hours"))
horas_datos <- length(unique(format(t, "%Y%m%d%H", tz = TZ)))
q <- quantile(a, c(.5, .9, .95, .99, .999))

dias <- format(t, "%Y-%m-%d", tz = TZ)
diario <- aggregate(a, list(dia = dias), max); names(diario)[2] <- "max"
diario <- diario[order(-diario$max), ]

# subida en 30 min: altura actual menos la mínima de los 30 min anteriores
tn <- as.numeric(t); W <- est$ventana_min * 60
subida30 <- numeric(length(a)); j <- 1
for (i in seq_along(a)) {                      # ventana móvil (datos ordenados)
  while (tn[j] < tn[i] - W) j <- j + 1
  subida30[i] <- a[i] - min(a[j:i])
}
qs <- quantile(subida30, c(.95, .99, .995, .999))
saltos <- abs(diff(a)); qj <- quantile(saltos, c(.99, .999))

dt <- c(diff(as.numeric(t)), 0) / 3600; dt[dt > 1] <- 0
horas_sobre <- function(u) sum(dt[a >= u])

# Simulación: ¿cuántos avisos habría enviado la configuración actual?
dias_subida <- unique(dias[subida30 >= est$subida_minima])
dias_nivel  <- function(u) unique(dias[a >= u])

# subida_minima: un poco por encima de lo que el río sube el 99.9 % de las veces
sug_subida <- max(0.10, ceiling((as.numeric(qs["99.9%"]) + 0.02) * 20) / 20)
dias_sug   <- unique(dias[subida30 >= sug_subida])
# salto_max: 1.5 veces el mayor salto REAL entre lecturas (incluye crecidas), nunca menos de 0.30 m
sug_salto  <- max(0.30, ceiling(max(saltos) * 1.5 * 20) / 20)

# ---------- 3. Gráficas ----------
dmax <- diario[order(diario$dia), ]; dmax$dia <- as.Date(dmax$dia)
g1 <- ggplot(dmax, aes(dia, max)) +
  geom_hline(yintercept = est$umbral_preventivo, color = "#f1c40f", linetype = "dashed") +
  geom_hline(yintercept = est$umbral_alerta, color = "#f5a623", linetype = "dashed") +
  geom_hline(yintercept = est$umbral_peligro, color = "#e53935", linetype = "dashed") +
  geom_col(fill = AZUL, width = 1) +
  labs(title = paste(est$rio, "· máximo de cada día"), subtitle = sprintf("%s a %s · líneas: umbrales actuales", min(dmax$dia), max(dmax$dia)),
       x = NULL, y = "Altura máxima (m)", caption = "Fuente: SIGIA") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold", color = AZUL))
suppressWarnings(ggsave(file.path(salida, "maximos_diarios.png"), g1, width = 11, height = 5, dpi = 110, bg = "white"))

g2 <- ggplot(data.frame(s = subida30[subida30 > 0.01]), aes(s)) +
  geom_histogram(bins = 60, fill = AZUL) +
  geom_vline(xintercept = est$subida_minima, color = "#e67e22", linewidth = 1) +
  geom_vline(xintercept = sug_subida, color = "#27ae60", linewidth = 1, linetype = "dashed") +
  scale_y_log10() +
  labs(title = sprintf("Subidas en %d minutos", est$ventana_min),
       subtitle = sprintf("Naranja: configuración actual (%.2f m) · Verde: sugerida (%.2f m)", est$subida_minima, sug_subida),
       x = "Subida (m)", y = "Cantidad de lecturas (escala log)") +
  theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold", color = AZUL))
suppressWarnings(ggsave(file.path(salida, "subidas_30min.png"), g2, width = 9, height = 5, dpi = 110, bg = "white"))

# ---------- 4. Reporte ----------
top <- head(diario, 10)
rep <- c(
  sprintf("# Análisis de historial · %s", est$rio), "",
  sprintf("Generado: %s", fmt_fecha_larga(Sys.time())), "",
  "## Datos", "",
  sprintf("- Periodo: **%s** a **%s** (%.0f días)", format(min(t), "%d/%m/%Y", tz = TZ), format(max(t), "%d/%m/%Y", tz = TZ), horas_total / 24),
  sprintf("- Lecturas válidas: %s · intervalo típico: %.0f min", format(length(a), big.mark = ","), intervalo),
  sprintf("- Horas con datos: %s de %s (**%.0f %%**)", format(horas_datos, big.mark = ","), format(round(horas_total), big.mark = ","), 100 * horas_datos / max(1, horas_total)),
  sprintf("- Lecturas falsas descartadas por el filtro: %d", length(desc)), "",
  "## Altura del río", "",
  "| Percentil | Altura |", "|---|---|",
  sprintf("| %s | %.2f m |", c("Mediana", "90 %", "95 %", "99 %", "99.9 %"), q),
  sprintf("| Máximo registrado | **%.2f m** (%s) |", max(a), format(t[which.max(a)], "%d/%m/%Y %H:%M", tz = TZ)), "",
  "### Los 10 días con el río más alto", "",
  "| Fecha | Máximo |", "|---|---|",
  sprintf("| %s | %.2f m |", format(as.Date(top$dia), "%d/%m/%Y"), top$max), "",
  "## Tiempo sobre los umbrales actuales", "",
  "| Umbral | Altura | Horas | Días distintos |", "|---|---|---|---|",
  sprintf("| Preventivo | %.2f m | %.1f | %d |", est$umbral_preventivo, horas_sobre(est$umbral_preventivo), length(dias_nivel(est$umbral_preventivo))),
  sprintf("| Alerta | %.2f m | %.1f | %d |", est$umbral_alerta, horas_sobre(est$umbral_alerta), length(dias_nivel(est$umbral_alerta))),
  sprintf("| Peligro | %.2f m | %.1f | %d |", est$umbral_peligro, horas_sobre(est$umbral_peligro), length(dias_nivel(est$umbral_peligro))), "",
  sprintf("## Subidas en %d minutos", est$ventana_min), "",
  sprintf("- Percentil 95: %.2f m · 99: %.2f m · 99.5: %.2f m · 99.9: %.2f m · máxima: **%.2f m**", qs[1], qs[2], qs[3], qs[4], max(subida30)),
  sprintf("- Con la configuración actual (`subida_minima = %.2f`) habría habido avisos de subida en **%d días** del periodo.", est$subida_minima, length(dias_subida)), "",
  "## Sugerencias para config.R", "",
  sprintf("- `subida_minima`: **%.2f m**. El 99.9 %% de las veces el río sube menos de %.2f m en %d min; con este valor habría habido avisos de subida en **%d días** (con el actual, %.2f m: %d días).",
          sug_subida, qs[4], est$ventana_min, length(dias_sug), est$subida_minima, length(dias_subida)),
  sprintf("- `salto_max`: **%.2f m** (1.5 veces el mayor salto real entre dos lecturas, que fue %.2f m, incluidas las crecidas). Actual: %.2f m. Un valor menor podría confundir una crecida rápida con una lectura falsa.",
          sug_salto, max(saltos), est$salto_max),
  "- `umbral_preventivo`, `umbral_alerta`, `umbral_peligro`: **no se calculan solo con estadística**.",
  "  Use la tabla de los 10 días más altos: pregunte a COMRED, bomberos y vecinos qué pasó esos días",
  "  (¿se desbordó?, ¿se cerró el paso?, ¿hubo daños?) y ubique los umbrales en esas alturas.", "",
  "![Máximos diarios](maximos_diarios.png)", "", "![Subidas](subidas_30min.png)")
writeLines(rep, file.path(salida, "reporte.md"))
cat(paste(rep[1:40], collapse = "\n"), "\n\nReporte completo en:", file.path(salida, "reporte.md"), "\n")
