# ============================================================
# SIGIA · MONITOR DE RÍOS PARA TELEGRAM
#
#   Rscript sigia_monitor.R boletin            -> boletín de todas las estaciones activas
#   Rscript sigia_monitor.R alerta             -> revisión de alertas (cada 10 min)
#   Rscript sigia_monitor.R semanal            -> resumen de la semana
#   Rscript sigia_monitor.R boletin panajachel -> solo una estación
#
# La configuración está en config.R
# ============================================================

# Acentos y emojis correctos aunque el sistema no esté en UTF-8
if (!isTRUE(l10n_info()$`UTF-8`)) {
  for (loc in c("C.UTF-8", "en_US.UTF-8", "es_GT.UTF-8")) {
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
  }
}

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(ggplot2)
  library(grid)
  library(png)
  library(jpeg)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

carpeta_script <- function() {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f) == 1) dirname(normalizePath(f)) else getwd()
}
DIR <- Sys.getenv("SIGIA_DIR", carpeta_script())
source(file.path(DIR, "config.R"), encoding = "UTF-8")

TZ             <- GENERAL$zona_horaria
TELEGRAM_TOKEN <- Sys.getenv("TELEGRAM_TOKEN")
CANAL          <- if (nzchar(Sys.getenv("TELEGRAM_CHAT_ID"))) Sys.getenv("TELEGRAM_CHAT_ID") else GENERAL$canal_por_defecto
ADMIN          <- Sys.getenv("ADMIN_CHAT_ID")
WU_KEY         <- Sys.getenv("WU_API_KEY")
GOOGLE_TTS_KEY <- Sys.getenv("GOOGLE_TTS_KEY")

COLORES <- c("#27ae60", "#f1c40f", "#e67e22", "#e74c3c")   # verde, amarillo, naranja, rojo
GRIS    <- "#95a5a6"
AZUL    <- "#003366"
NIVELES <- list(
  list(emoji = "🟢", nombre = "VERDE · NIVEL NORMAL",        clave = "normal"),
  list(emoji = "🟡", nombre = "AMARILLO · NIVEL PREVENTIVO", clave = "preventivo"),
  list(emoji = "🟠", nombre = "NARANJA · NIVEL DE ALERTA",   clave = "alerta"),
  list(emoji = "🔴", nombre = "ROJO · NIVEL DE PELIGRO",     clave = "peligro")
)

# ============================================================
# 1) UTILIDADES
# ============================================================
log_message <- function(m) cat(sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), m))

largo <- function(s) length(utf8ToInt(enc2utf8(s)))
pad   <- function(s, n) paste0(s, strrep(" ", max(0, n - largo(s))))
fila  <- function(etiqueta, valor) paste0(pad(etiqueta, 15), valor)
mayus <- function(s) chartr("áéíóúñ", "ÁÉÍÓÚÑ", toupper(s))

DIAS  <- c("Domingo","Lunes","Martes","Miércoles","Jueves","Viernes","Sábado")
MESES <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
fmt_hora <- function(t) format(t, "%H:%M", tz = TZ)
fmt_fecha_larga <- function(t) sprintf("%s %d %s %s, %s",
  DIAS[as.integer(format(t, "%w", tz = TZ)) + 1], as.integer(format(t, "%d", tz = TZ)),
  MESES[as.integer(format(t, "%m", tz = TZ))], format(t, "%Y", tz = TZ), fmt_hora(t))
dia_corto <- function(t) sprintf("%s %d", tolower(substr(DIAS[as.POSIXlt(t, tz = TZ)$wday + 1], 1, 3)),
                                 as.integer(format(t, "%d", tz = TZ)))
hace <- function(t) {
  m <- round(as.numeric(difftime(Sys.time(), t, units = "mins")))
  if (m < 1) "ahora" else if (m < 60) paste0("hace ", m, " min")
  else if (m < 1440) sprintf("hace %d h %02d min", m %/% 60, m %% 60)
  else sprintf("hace %d d %d h", m %/% 1440, (m %% 1440) %/% 60)
}
duracion <- function(horas) {
  m <- round(horas * 60)
  if (m < 60) paste(m, "min") else if (m < 1440) sprintf("%d h %02d min", m %/% 60, m %% 60)
  else sprintf("%d d %d h", m %/% 1440, (m %% 1440) %/% 60)
}
fmt_lectura <- function(t) {           # como la página: "7/10/2026, 1:23:14 p. m."
  h <- as.integer(format(t, "%H", tz = TZ)); h12 <- if (h %% 12 == 0) 12 else h %% 12
  sprintf("%d/%d/%s, %d:%s %s", as.integer(format(t, "%d", tz = TZ)), as.integer(format(t, "%m", tz = TZ)),
          format(t, "%Y", tz = TZ), h12, format(t, "%M:%S", tz = TZ), if (h < 12) "a. m." else "p. m.")
}
horas_desde <- function(t) as.numeric(difftime(Sys.time(), t, units = "hours"))

http_json <- function(url, query = list(), intentos = 3, espera = 2) {
  for (i in seq_len(intentos)) {
    res <- tryCatch(GET(url, query = query, timeout(25), user_agent("SIGIA-Telegram-Bot")),
                    error = function(e) { log_message(paste("Error de red:", e$message)); NULL })
    if (!is.null(res) && status_code(res) == 200) {
      txt <- content(res, as = "text", encoding = "UTF-8")
      return(tryCatch(fromJSON(txt, simplifyVector = FALSE),
                      error = function(e) { log_message("Respuesta no es JSON"); NULL }))
    }
    if (!is.null(res)) log_message(paste("HTTP", status_code(res), "- intento", i))
    Sys.sleep(espera)
  }
  NULL
}

parse_fecha <- function(x) {
  if (is.null(x) || is.na(x)) return(as.POSIXct(NA, tz = TZ))
  x <- sub("Z$", "+0000", x)
  x <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", x)
  f <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  if (is.na(f)) f <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = TZ)
  attr(f, "tzone") <- TZ
  f
}

# ============================================================
# 2) ESTACIONES
# ============================================================
preparar_estacion <- function(e) {
  d <- POR_DEFECTO
  for (k in names(e)) d[k] <- list(e[[k]])
  d$base_url   <- GENERAL$base_url
  d$menu       <- GENERAL$menu
  d$channel_id <- CANAL
  d$archivo_estado <- file.path(DIR, "estado", paste0("estado_", d$id, ".json"))
  d
}

estaciones_activas <- function(solo = NA) {
  ests <- lapply(ESTACIONES, preparar_estacion)
  ests <- Filter(function(e) isTRUE(e$activa) && (is.na(solo) || e$id == solo), ests)
  Filter(function(e) {
    ok <- all(!is.na(c(e$umbral_preventivo, e$umbral_alerta, e$umbral_peligro)))
    if (!ok) log_message(paste("Estación", e$id, "sin umbrales configurados: se omite"))
    ok
  }, ests)
}

# ============================================================
# 3) TELEGRAM
# ============================================================
tg <- function(metodo, body, multipart = FALSE) {
  url <- sprintf("https://api.telegram.org/bot%s/%s", TELEGRAM_TOKEN, metodo)
  res <- tryCatch(POST(url, body = body, encode = if (multipart) "multipart" else "json", timeout(60)),
                  error = function(e) { log_message(paste("Telegram", metodo, "error:", e$message)); NULL })
  if (is.null(res)) return(NULL)
  r <- tryCatch(content(res, as = "parsed", simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(r) || !isTRUE(r$ok)) {
    log_message(paste("Telegram", metodo, "falló:", r$description %||% status_code(res)))
    return(NULL)
  }
  r$result
}
bool <- function(x) tolower(as.character(isTRUE(x)))

botones <- function(est) {
  fila1 <- list(list(text = "📊 Ver en vivo / descargar datos", url = est$pagina),
                list(text = "🗺️ Otras estaciones", url = est$menu))
  teclado <- list(fila1)
  if (nzchar(GENERAL$bot_usuario))
    teclado <- c(teclado, list(list(list(text = "🤖 Consultar nivel al bot",
                                         url = paste0("https://t.me/", GENERAL$bot_usuario, "?start=nivel_", est$id)))))
  list(inline_keyboard = teclado)
}

enviar_texto <- function(est, html, silencioso, chat = est$channel_id, con_botones = TRUE) {
  body <- list(chat_id = chat, text = html, parse_mode = "HTML",
               disable_web_page_preview = TRUE, disable_notification = silencioso)
  if (con_botones) body$reply_markup <- botones(est)
  tg("sendMessage", body)
}

admin <- function(html) {
  if (!nzchar(ADMIN)) { log_message(paste("[ADMIN sin configurar]", gsub("<[^>]+>", "", html))); return(invisible()) }
  tg("sendMessage", list(chat_id = ADMIN, text = html, parse_mode = "HTML", disable_web_page_preview = TRUE))
}

enviar_foto_archivo <- function(est, ruta, caption, silencioso, con_botones = TRUE) {
  body <- list(chat_id = est$channel_id, photo = upload_file(ruta, type = "image/png"),
               caption = caption, parse_mode = "HTML", disable_notification = bool(silencioso))
  if (con_botones) body$reply_markup <- as.character(toJSON(botones(est), auto_unbox = TRUE))
  tg("sendPhoto", body, multipart = TRUE)
}

# Respaldo si la tarjeta falla: gráfica + foto de cámara en álbum
enviar_album <- function(est, grafica, foto, silencioso = TRUE) {
  media <- list(); archivos <- list()
  if (!is.null(grafica) && file.exists(grafica)) {
    media <- c(media, list(list(type = "photo", media = "attach://grafica",
                                caption = paste0("📈 ", est$rio, " · últimas 24 h"))))
    archivos$grafica <- upload_file(grafica, type = "image/png")
  }
  if (!is.null(foto)) media <- c(media, list(list(type = "photo", media = foto$url,
                                                  caption = paste0("📷 Cámara estación ", est$nombre))))
  if (length(media) == 0) return(invisible())
  if (length(media) == 1 && length(archivos) == 0)
    return(tg("sendPhoto", list(chat_id = est$channel_id, photo = media[[1]]$media,
                                caption = media[[1]]$caption, disable_notification = silencioso)))
  tg("sendMediaGroup", c(list(chat_id = est$channel_id, media = as.character(toJSON(media, auto_unbox = TRUE)),
                              disable_notification = bool(silencioso)), archivos), multipart = TRUE)
}

# Notas de voz grabadas: audios/<estacion>_<clave>_<idioma>.ogg (o .mp3 / .m4a)
enviar_audios <- function(est, clave, silencioso) {
  archivos <- list.files(file.path(DIR, "audios"), full.names = TRUE,
                         pattern = paste0("^", est$id, "_", clave, "(_[a-z]+)?\\.(ogg|oga|opus|mp3|m4a)$"))
  for (a in archivos) {
    idioma <- sub(paste0("^", est$id, "_", clave, "_?"), "", tools::file_path_sans_ext(basename(a)))
    etiqueta <- switch(idioma, es = "Español", kaq = "Kaqchikel", tz = "Tz'utujil", if (nzchar(idioma)) idioma else "")
    cap <- paste0("🔊 Mensaje de voz", if (nzchar(etiqueta)) paste0(" · ", etiqueta) else "", " · ", est$rio)
    ext <- tolower(tools::file_ext(a))
    if (ext %in% c("ogg", "oga", "opus"))
      tg("sendVoice", list(chat_id = est$channel_id, voice = upload_file(a, type = "audio/ogg"),
                           caption = cap, disable_notification = bool(silencioso)), multipart = TRUE)
    else
      tg("sendAudio", list(chat_id = est$channel_id, audio = upload_file(a), title = cap,
                           caption = cap, disable_notification = bool(silencioso)), multipart = TRUE)
  }
  invisible(length(archivos))
}

# Texto con formato de WhatsApp (*negrita*, _cursiva_) a partir del HTML de Telegram
a_whatsapp <- function(html, est) {
  t <- gsub("</?b>", "*", html)
  t <- gsub("</?i>", "_", t)
  t <- gsub("<pre>", "```", t)
  t <- gsub("</pre>", "```\n", t)
  t <- gsub("<[^>]+>", "", t)
  t <- gsub("&lt;", "<", gsub("&gt;", ">", gsub("&amp;", "&", t)))
  paste0(t, "\n\n📊 En vivo: ", est$pagina)
}

# ============================================================
# 4) DATOS DE LA ESTACIÓN
# ============================================================
get_lecturas <- function(est, n = 2000) {
  d <- http_json(paste0(est$base_url, est$datos), list(results = n))
  if (is.null(d) || length(d$feeds) == 0) return(NULL)
  df <- data.frame(
    fecha  = do.call(c, lapply(d$feeds, function(f) parse_fecha(f$created_at %||% NA))),
    altura = vapply(d$feeds, function(f) suppressWarnings(as.numeric(f$field1 %||% NA)), numeric(1)))
  df <- df[!is.na(df$fecha) & !is.na(df$altura), ]
  if (nrow(df) == 0) return(NULL)
  df <- df[order(df$fecha), ]
  df[!duplicated(df$fecha), ]
}

# Filtro de lecturas falsas:
#  - fuera del rango físico posible
#  - picos aislados: saltan más de salto_max y regresan en la lectura siguiente
#  - la última lectura, si salta más de salto_max, queda "por confirmar" hasta la próxima
limpiar <- function(df, est) {
  if (is.null(df) || nrow(df) == 0) return(df)
  fuera <- df$altura < est$altura_min_valida | df$altura > est$altura_max_valida
  descartes <- df$fecha[fuera]
  df <- df[!fuera, ]
  n <- nrow(df); pendiente <- FALSE
  if (n >= 3) {
    a <- df$altura; pico <- rep(FALSE, n)
    for (i in 2:(n - 1)) {
      d1 <- a[i] - a[i - 1]; d2 <- a[i] - a[i + 1]
      if (abs(d1) > est$salto_max && abs(d2) > est$salto_max && sign(d1) == sign(d2) &&
          abs(a[i + 1] - a[i - 1]) < est$salto_max) pico[i] <- TRUE
    }
    descartes <- c(descartes, df$fecha[pico]); df <- df[!pico, ]
  }
  n <- nrow(df)
  if (n >= 2 && abs(df$altura[n] - df$altura[n - 1]) > est$salto_max) {
    pendiente <- TRUE; df <- df[-n, ]
  }
  attr(df, "descartes") <- descartes
  attr(df, "pendiente") <- pendiente
  df
}

get_ultima_foto <- function(est) {
  d <- http_json(paste0(est$base_url, est$fotos), list(t = as.integer(Sys.time())))
  if (is.null(d) || length(d) == 0) return(NULL)
  url <- d[[length(d)]]$url
  if (is.null(url)) return(NULL)
  if (!grepl("^https?://", url)) url <- paste0(est$base_url, sub("^/", "", url))
  nombre <- basename(url)                        # P + AAMMDD HHMMSS -> P26100712040410.jpg
  hora <- fecha <- NA; momento <- as.POSIXct(NA)
  if (startsWith(nombre, "P") && nchar(nombre) >= 13) {
    hora    <- paste0(substr(nombre, 8, 9), ":", substr(nombre, 10, 11))
    fecha   <- paste0(substr(nombre, 6, 7), "/", substr(nombre, 4, 5), "/20", substr(nombre, 2, 3))
    momento <- as.POSIXct(paste0("20", substr(nombre, 2, 7), substr(nombre, 8, 11)), format = "%Y%m%d%H%M", tz = TZ)
  }
  list(url = url, hora = hora, fecha = fecha, momento = momento)
}

leer_imagen <- function(url) {
  res <- tryCatch(GET(URLencode(url), timeout(30)), error = function(e) NULL)
  if (is.null(res) || status_code(res) != 200) return(NULL)
  raw <- content(res, as = "raw")
  if (length(raw) < 8) return(NULL)
  es_png <- identical(as.integer(raw[1:4]), c(137L, 80L, 78L, 71L))
  tryCatch(if (es_png) readPNG(raw) else readJPEG(raw), error = function(e) NULL)
}

# ============================================================
# 5) LLUVIA (Wunderground · cuenca alta)
# ============================================================
lluvia_pws <- function(id) {
  d <- http_json("https://api.weather.com/v2/pws/observations/all/1day",
                 list(stationId = id, format = "json", units = "m", apiKey = WU_KEY, numericPrecision = "decimal"),
                 intentos = 2)
  obs <- d$observations
  if (length(obs) == 0) return(NULL)
  f   <- do.call(c, lapply(obs, function(o) parse_fecha(o$obsTimeUtc %||% NA)))
  tot <- vapply(obs, function(o) as.numeric(o$metric$precipTotal %||% NA), numeric(1))
  rat <- vapply(obs, function(o) as.numeric(o$metric$precipRate  %||% NA), numeric(1))
  ok <- !is.na(f) & !is.na(tot); f <- f[ok]; tot <- tot[ok]; rat <- rat[ok]
  if (length(f) == 0) return(NULL)
  o <- order(f); f <- f[o]; tot <- tot[o]; rat <- rat[o]
  inc <- c(0, diff(tot)); inc[inc < 0] <- tot[inc < 0]      # el acumulado se reinicia a medianoche
  ult <- length(f)
  data.frame(mm_1h = sum(inc[f > f[ult] - 3600]), mm_hoy = tot[ult],
             intensidad = rat[ult] %||% NA, hora = f[ult])
}

obtener_lluvia <- function(est) {
  if (length(est$lluvia) == 0 || !nzchar(WU_KEY)) return(NULL)
  filas <- lapply(est$lluvia, function(p) {
    r <- tryCatch(lluvia_pws(p$id), error = function(e) NULL)
    if (is.null(r) || horas_desde(r$hora) > 2) return(NULL)        # estación de lluvia sin datos recientes
    cbind(nombre = p$nombre, r)
  })
  filas <- Filter(Negate(is.null), filas)
  if (length(filas) == 0) return(NULL)
  ll <- do.call(rbind, filas)
  ll[order(-ll$mm_1h), ]
}

texto_lluvia <- function(ll) {
  if (is.null(ll)) return(NULL)
  top <- ll[1, ]
  if (top$mm_1h < 0.2 && max(ll$mm_hoy) < 0.2) return("🌤️ Sin lluvia en la cuenca alta")
  sprintf("🌧️ Lluvia cuenca alta: <b>%.1f mm</b> en la última hora (%s) · hoy %.1f mm",
          top$mm_1h, top$nombre, top$mm_hoy)
}

# ============================================================
# 6) ANÁLISIS
# ============================================================
nivel_altura <- function(a, est) {
  if (a >= est$umbral_peligro) 3 else if (a >= est$umbral_alerta) 2
  else if (a >= est$umbral_preventivo) 1 else 0
}
barra <- function(a, est, n = 10) {
  llenos <- max(0, min(n, round(a / est$umbral_peligro * n)))
  paste0(strrep("▰", llenos), strrep("▱", n - llenos))
}
distancia_umbral <- function(a, est) {
  if (a < est$umbral_alerta) sprintf("faltan %.2f m para alerta", est$umbral_alerta - a)
  else if (a < est$umbral_peligro) sprintf("faltan %.2f m para peligro", est$umbral_peligro - a)
  else sprintf("%.2f m SOBRE peligro", a - est$umbral_peligro)
}
tendencia <- function(df, ult) {
  previas <- df[df$fecha <= ult$fecha - 3600, ]
  ref <- if (nrow(previas) > 0) previas[nrow(previas), ] else df[1, ]
  delta <- ult$altura - ref$altura
  mins  <- round(as.numeric(difftime(ult$fecha, ref$fecha, units = "mins")))
  per   <- if (mins >= 55 && mins <= 75) "1 h" else paste0(mins, " min")
  if (abs(delta) < 0.02) paste0("▶ estable / ", per)
  else sprintf("%s %+.2f m / %s", if (delta > 0) "▲" else "▼", delta, per)
}
subida_reciente <- function(df, actual, est) {
  ult <- df[nrow(df), ]
  v <- df[df$fecha >= ult$fecha - est$ventana_min * 60, ]
  base <- v[which.min(v$altura), ]
  list(subida = actual - base$altura,
       mins = max(1, round(as.numeric(difftime(ult$fecha, base$fecha, units = "mins")))))
}

# Ritmo de subida (regresión de los últimos minutos) y tiempo estimado al siguiente umbral
proyeccion <- function(df, est) {
  if (is.null(df) || nrow(df) < 3) return(NULL)
  ult <- df[nrow(df), ]
  v <- df[df$fecha >= ult$fecha - max(est$ventana_min, 30) * 60, ]
  if (nrow(v) < 3) return(NULL)
  t <- as.numeric(difftime(v$fecha, ult$fecha, units = "hours"))
  ritmo <- unname(coef(lm(v$altura ~ t))[2])
  p <- list(ritmo = ritmo)
  if (is.na(ritmo) || ritmo < est$ritmo_minimo) return(p)
  sig <- c(alerta = est$umbral_alerta, peligro = est$umbral_peligro)
  sig <- sig[sig > ult$altura]
  if (length(sig) == 0) return(p)
  horas <- (sig[[1]] - ult$altura) / ritmo
  if (horas > est$proyeccion_max_h) return(p)
  p$umbral <- names(sig)[1]; p$minutos <- max(1, round(horas * 60))
  p
}
fmt_eta <- function(min) if (min < 60) paste0("≈ ", min, " min") else sprintf("≈ %d h %02d min", min %/% 60, min %% 60)
texto_proyeccion <- function(p) {
  if (is.null(p) || is.null(p$minutos)) return(NULL)
  sprintf("A este ritmo (%+.2f m/h) alcanzaría el nivel de %s en %s", p$ritmo, p$umbral, fmt_eta(p$minutos))
}

resumen <- function(df, est) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  ult <- df[nrow(df), ]
  offline <- horas_desde(ult$fecha) > est$horas_sin_datos
  d24 <- df[df$fecha >= ult$fecha - 24 * 3600, ]
  n <- nivel_altura(ult$altura, est)
  s <- subida_reciente(df, ult$altura, est)
  et <- NIVELES[[n + 1]]; color <- COLORES[n + 1]
  if (n == 0 && s$subida >= est$subida_minima) {
    et <- list(emoji = "🟡", nombre = "AMARILLO · RÍO EN ASCENSO", clave = "subida"); color <- COLORES[2]
  }
  if (offline) { et <- list(emoji = "⚪", nombre = "ESTACIÓN SIN CONEXIÓN", clave = "offline"); color <- GRIS }
  list(offline = offline, ult = ult, d24 = d24, n = n, s = s, et = et, color = color,
       tend = tendencia(df, ult), proy = if (offline) NULL else proyeccion(df, est),
       imax = which.max(d24$altura), imin = which.min(d24$altura))
}

# ============================================================
# 7) IMÁGENES
# ============================================================
lienzo <- function(ruta, W, H) {
  tipo <- if (isTRUE(capabilities("cairo"))) "cairo" else getOption("bitmapType")
  png(ruta, width = W, height = H, res = 96, type = tipo, bg = "white")
  grid.newpage()
  X <- function(x) unit(x / W, "npc"); Y <- function(y) unit(1 - y / H, "npc")
  L <- list(W = W, H = H, X = X, Y = Y)
  L$vp   <- function(x, y, w, h) viewport(X(x), Y(y), unit(w / W, "npc"), unit(h / H, "npc"), just = c("left", "top"))
  L$txt  <- function(l, x, y, size, col = AZUL, face = "bold", hjust = 0.5)
    grid.text(l, X(x), Y(y), hjust = hjust, gp = gpar(fontsize = size, col = col, fontface = face))
  L$caja <- function(x, y, w, h, fill = "white", col = NA, r = 12)
    grid.roundrect(X(x), Y(y), unit(w / W, "npc"), unit(h / H, "npc"), just = c("left", "top"),
                   r = unit(r, "pt"), gp = gpar(fill = fill, col = col))
  L$rect <- function(x, y, w, h, fill, col = NA, lwd = 1, just = c("left", "top"))
    grid.rect(X(x), Y(y), unit(w / W, "npc"), unit(h / H, "npc"), just = just, gp = gpar(fill = fill, col = col, lwd = lwd))
  L$tarjeta <- function(x, y, w, h) { L$caja(x + 3, y + 6, w, h, fill = "#e3e8ef"); L$caja(x, y, w, h, col = "#eef1f5") }
  L$imagen <- function(img, x, y, w, h) {
    s <- min(w / dim(img)[2], h / dim(img)[1])
    grid.raster(img, X(x + w / 2), Y(y + h / 2), unit(dim(img)[2] * s / W, "npc"), unit(dim(img)[1] * s / H, "npc"))
  }
  L
}

encabezado <- function(L, est, etiqueta, color_etiqueta = "#667788", franja = NULL, derecha = NULL) {
  if (!is.null(franja)) L$rect(0, 0, L$W, 10, fill = franja)
  L$txt(etiqueta, 40, 50, 13, col = color_etiqueta, hjust = 0)
  L$txt(paste("ESTACIÓN", mayus(est$nombre)), L$W / 2, 62, 30)
  ahora <- Sys.time()
  if (is.null(derecha)) {
    L$txt(sub("^0", "", format(ahora, "%I:%M %p", tz = TZ)), L$W - 40, 48, 18, hjust = 1)
    L$txt(format(ahora, "%d/%m/%Y", tz = TZ), L$W - 40, 78, 12, col = "#555555", hjust = 1)
  } else {
    L$txt(derecha[1], L$W - 40, 48, 16, hjust = 1)
    L$txt(derecha[2], L$W - 40, 78, 12, col = "#555555", hjust = 1)
  }
}

pie <- function(L, logos_img, y) {
  if (!is.null(logos_img)) L$imagen(logos_img, 300, y, 1000, 140)
  L$txt("Fuente: SIGIA · www.sigiagt.com   |   Información automática de la estación. Los avisos oficiales los emite CONRED.",
        L$W / 2, y + 195, 11, col = "#8a96a3", face = "plain")
}

tema_grafica <- function() theme_minimal(base_size = 12) +
  theme(axis.title = element_text(color = AZUL, face = "bold", size = 11),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#e5e5e5"))

lineas_umbral <- function(est) list(
  geom_hline(yintercept = est$umbral_peligro, color = "#e53935", linetype = "dashed", linewidth = 0.9),
  geom_hline(yintercept = est$umbral_alerta,  color = "#f5a623", linetype = "dashed", linewidth = 0.9))

# Corta la línea donde faltan datos (más de 1 h sin lecturas)
con_huecos <- function(d, max_h = 1) {
  if (nrow(d) < 2) return(d)
  gap <- which(diff(as.numeric(d$fecha)) > max_h * 3600)
  if (length(gap) == 0) return(d)
  extra <- data.frame(fecha = d$fecha[gap] + 60, altura = NA_real_)
  out <- rbind(d[, c("fecha", "altura")], extra)
  out[order(out$fecha), ]
}

grafica_tarjeta <- function(df, r, est) {
  dd <- if (r$offline) tail(df, 15) else r$d24
  dd <- con_huecos(dd[, c("fecha", "altura")])
  attr(dd$fecha, "tzone") <- TZ
  ymax <- max(5, ceiling((max(dd$altura, na.rm = TRUE) + 0.3) * 2) / 2)
  ggplot(dd, aes(fecha, altura)) + lineas_umbral(est) +
    geom_line(color = AZUL, linewidth = 1, na.rm = TRUE) +
    { if (nrow(dd) <= 40) geom_point(shape = 21, fill = "white", color = AZUL, size = 2, stroke = 1.2, na.rm = TRUE) } +
    scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, 0.5), expand = c(0, 0)) +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(x = "Hora de datos", y = "Altura (m)") + tema_grafica() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# --- Tarjeta principal (boletín y alertas) ---------------------------------
generar_tarjeta <- function(est, df, foto_img, foto, logos_img, ruta, lluvia = NULL, alerta = FALSE) {
  r <- resumen(df, est)
  if (is.null(r)) return(NULL)
  L <- lienzo(ruta, 1600, 1360); on.exit(dev.off(), add = TRUE)
  txt <- L$txt

  encabezado(L, est, if (alerta) "SIGIA · AVISO DEL RÍO" else "SIGIA · BOLETÍN HIDROLÓGICO",
             if (alerta) r$color else "#667788", franja = if (alerta) r$color)

  # Comportamiento del río
  L$tarjeta(40, 120, 750, 640)
  txt("Comportamiento del Río", 415, 168, 16)
  print(grafica_tarjeta(df, r, est), vp = L$vp(70, 195, 690, 420))
  txt("Altura actual (m):", 345, 668, 19, face = "plain", hjust = 1)
  txt(if (r$offline) "-- m" else sprintf("%.2f m", r$ult$altura), 360, 668, 32, col = "black", hjust = 0)
  led <- if (r$offline) "#e74c3c" else "#2ecc71"
  grid.circle(L$X(575), L$Y(668), unit(15, "pt"), gp = gpar(fill = adjustcolor(led, 0.25), col = NA))
  grid.circle(L$X(575), L$Y(668), unit(9, "pt"),  gp = gpar(fill = led, col = NA))
  txt(paste("Última lectura:", fmt_lectura(r$ult$fecha)), 415, 722, 13, col = "#666666", face = "plain")

  # Imágenes de estación
  L$tarjeta(810, 120, 750, 640)
  txt("Imágenes de Estación", 1185, 168, 16)
  L$caja(850, 195, 670, 480, fill = "black", r = 8)
  if (!is.null(foto_img)) L$imagen(foto_img, 850, 195, 670, 435)
  else txt("Imagen no disponible", 1185, 410, 14, col = "#bbbbbb", face = "plain")
  L$rect(850, 630, 670, 45, fill = adjustcolor(AZUL, 0.9))
  cap <- if (!is.null(foto) && !is.na(foto$hora))
    paste0("Captura: ", if (!is.na(foto$fecha)) paste0(foto$fecha, "  ") else "", foto$hora) else "Captura: --"
  txt(cap, 1185, 653, 13, col = "white")
  txt("Cámara de la estación · www.sigiagt.com", 1185, 722, 12, col = "#888888", face = "plain")

  # Estado: semáforo + medidor + proyección + estadísticas
  L$tarjeta(40, 790, 1520, 320)
  L$caja(70, 822, 420, 110, fill = r$color)
  tc <- if (r$color == COLORES[2]) "#3a3000" else "white"
  partes <- strsplit(r$et$nombre, " · ")[[1]]
  principal <- partes[length(partes)]
  txt("ESTADO DEL RÍO", 280, 850, 12, col = tc)
  txt(principal, 280, 885, if (nchar(principal) > 16) 18 else 21, col = tc)
  if (length(partes) > 1) txt(partes[1], 280, 915, 11, col = tc, face = "plain")

  gx <- 540; gw <- 980; gy <- 868; gh <- 30
  tope <- max(5, r$ult$altura + 0.25)
  px <- function(m) gx + gw * min(m, tope) / tope
  zonas <- c(0, est$umbral_preventivo, est$umbral_alerta, est$umbral_peligro, tope)
  for (i in 1:4) L$rect(px(zonas[i]), gy, px(zonas[i + 1]) - px(zonas[i]), gh,
                        fill = adjustcolor(COLORES[i], if (i == 1) 0.75 else 0.85), col = "white", lwd = 2)
  mx <- px(r$ult$altura)
  grid.polygon(L$X(c(mx - 11, mx + 11, mx)), L$Y(c(gy - 20, gy - 20, gy - 2)),
               gp = gpar(fill = if (r$offline) GRIS else AZUL, col = NA))
  L$rect(mx, gy - 3, 3, gh + 6, fill = AZUL, just = c("center", "top"))
  txt(sprintf("%s%.2f m", if (r$offline) "última: " else "", r$ult$altura),
      min(max(mx, gx + 60), gx + gw - 60), gy - 34, 13)
  for (z in list(c(0, "0 m"), c(est$umbral_preventivo, sprintf("%.2f preventivo", est$umbral_preventivo)),
                 c(est$umbral_alerta, sprintf("%.2f alerta", est$umbral_alerta)),
                 c(est$umbral_peligro, sprintf("%.2f peligro", est$umbral_peligro))))
    txt(z[2], px(as.numeric(z[1])), gy + gh + 18, 10, col = "#666666", face = "plain",
        hjust = if (z[2] == "0 m") 0 else 0.5)

  tp <- texto_proyeccion(r$proy)
  if (!is.null(tp)) {
    txt(paste("Proyección:", tp), gx, 948, 13, col = if (r$proy$umbral == "peligro") "#c0392b" else "#d35400", hjust = 0)
  } else if (!r$offline && !is.null(r$proy) && !is.na(r$proy$ritmo) && r$proy$ritmo >= 0.05) {
    txt(sprintf("Sube a %+.2f m/h · no se proyecta llegar a alerta en las próximas %d h",
                r$proy$ritmo, est$proyeccion_max_h), gx, 948, 12, col = "#6b7a8c", face = "plain", hjust = 0)
  }

  if (r$offline) { tend_lab <- "Tendencia"; tend_val <- "sin datos recientes" }
  else { tend_lab <- paste0("Tendencia (", sub(".* / ", "", r$tend), ")"); tend_val <- sub(" / .*", "", r$tend) }
  a <- r$ult$altura
  dist <- if (a < est$umbral_alerta) c("Para nivel de alerta", sprintf("faltan %.2f m", est$umbral_alerta - a))
          else if (a < est$umbral_peligro) c("Para nivel de peligro", sprintf("faltan %.2f m", est$umbral_peligro - a))
          else c("Sobre nivel de peligro", sprintf("+%.2f m", a - est$umbral_peligro))
  d <- r$d24
  per <- if (r$offline) paste0("(", format(r$ult$fecha, "%d/%m", tz = TZ), ")") else "24 h"
  cuarto <- if (!is.null(lluvia)) c("Lluvia cuenca alta (1 h)", sprintf("%.1f mm · hoy %.0f mm", lluvia$mm_1h[1], lluvia$mm_hoy[1]))
            else c(paste("Promedio", per), sprintf("%.2f m", mean(d$altura)))
  tiles <- list(c(tend_lab, tend_val),
    c(paste("Máxima", per), sprintf("%.2f m · %s", d$altura[r$imax], fmt_hora(d$fecha[r$imax]))),
    c(paste("Mínima", per), sprintf("%.2f m · %s", d$altura[r$imin], fmt_hora(d$fecha[r$imin]))),
    cuarto, dist)
  tw <- (1460 - 4 * 20) / 5
  for (i in seq_along(tiles)) {
    tx <- 70 + (i - 1) * (tw + 20)
    L$caja(tx, 975, tw, 105, fill = "#f3f6fa", r = 10)
    txt(tiles[[i]][1], tx + tw / 2, 1005, 11, col = "#6b7a8c", face = "plain")
    txt(tiles[[i]][2], tx + tw / 2, 1047, if (nchar(tiles[[i]][2]) > 16) 15 else 19)
  }

  pie(L, logos_img, 1130)
  ruta
}

# --- Tarjeta del resumen semanal --------------------------------------------
generar_tarjeta_semanal <- function(est, sem, logos_img, ruta) {
  L <- lienzo(ruta, 1600, 1290); on.exit(dev.off(), add = TRUE)
  txt <- L$txt; d <- sem$datos
  encabezado(L, est, "SIGIA · RESUMEN SEMANAL", derecha = c(sem$rango, "últimos 7 días"))

  L$tarjeta(40, 120, 1520, 560)
  txt("Nivel del río · últimos 7 días", 800, 168, 16)
  dd <- con_huecos(d); attr(dd$fecha, "tzone") <- TZ
  ymax <- max(5, ceiling((max(d$altura) + 0.3) * 2) / 2)
  cortes <- seq(sem$inicio, sem$fin, by = "day")
  p <- ggplot(dd, aes(fecha, altura)) + lineas_umbral(est) +
    geom_line(color = AZUL, linewidth = 0.8, na.rm = TRUE) +
    geom_point(data = sem$diario, aes(maxf, max), color = AZUL, size = 2.5, na.rm = TRUE) +
    scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, 0.5), expand = c(0, 0)) +
    scale_x_datetime(limits = c(sem$inicio, sem$fin), breaks = cortes, labels = dia_corto, expand = c(0, 0)) +
    labs(x = NULL, y = "Altura (m)") + tema_grafica()
  print(p, vp = L$vp(70, 195, 1460, 465))

  L$tarjeta(40, 700, 1520, 175)
  txt("Máximo de cada día", 800, 735, 15)
  bw <- (1460 - 6 * 15) / 7
  for (i in seq_len(nrow(sem$diario))) {
    x <- 70 + (i - 1) * (bw + 15); f <- sem$diario[i, ]
    col <- if (is.na(f$max)) "#eeeeee" else adjustcolor(COLORES[nivel_altura(f$max, est) + 1], 0.25)
    L$caja(x, 760, bw, 95, fill = col, r = 10)
    txt(dia_corto(f$dia), x + bw / 2, 785, 12, col = "#6b7a8c", face = "plain")
    txt(if (is.na(f$max)) "sin datos" else sprintf("%.2f m", f$max), x + bw / 2, 825, if (is.na(f$max)) 13 else 19)
  }

  L$tarjeta(40, 895, 1520, 150)
  tw <- (1460 - 4 * 20) / 5
  tiles <- list(
    c("Máxima de la semana", sprintf("%.2f m · %s %s", sem$max, dia_corto(sem$max_f), fmt_hora(sem$max_f))),
    c("Mínima de la semana", sprintf("%.2f m", sem$min)),
    c("Promedio", sprintf("%.2f m", sem$prom)),
    c("Horas en preventivo o más", sprintf("%s h", format(round(sem$horas_sobre, 1)))),
    c("Datos recibidos", sprintf("%d de 168 h (%d %%)", sem$horas_datos, round(sem$horas_datos / 168 * 100))))
  for (i in seq_along(tiles)) {
    tx <- 70 + (i - 1) * (tw + 20)
    L$caja(tx, 917, tw, 105, fill = "#f3f6fa", r = 10)
    txt(tiles[[i]][1], tx + tw / 2, 947, 11, col = "#6b7a8c", face = "plain")
    txt(tiles[[i]][2], tx + tw / 2, 989, if (nchar(tiles[[i]][2]) > 16) 15 else 19)
  }
  pie(L, logos_img, 1060)
  ruta
}

# Gráfica simple de 24 h (respaldo si la tarjeta falla)
graficar <- function(df, est, ruta) {
  d <- df[df$fecha >= Sys.time() - 24 * 3600, ]
  if (nrow(d) < 2) return(NULL)
  attr(d$fecha, "tzone") <- TZ
  p <- ggplot(d, aes(fecha, altura)) + lineas_umbral(est) + geom_line(color = AZUL, linewidth = 1.1) +
    scale_x_datetime(date_labels = "%H:%M") + scale_y_continuous(limits = c(0, max(5, max(d$altura) + 0.4))) +
    labs(title = paste0(est$rio, " · últimas 24 horas"), x = NULL, y = "Altura (m)", caption = "Fuente: SIGIA") +
    theme_minimal(base_size = 14)
  ggsave(ruta, p, width = 10, height = 5.6, dpi = 120, bg = "white")
  ruta
}

# ============================================================
# 8) PUBLICAR
# ============================================================
# Envía la tarjeta con su texto. Si el texto es muy largo para una foto (1024),
# manda la foto con un título corto y el texto completo enseguida.
# Si la imagen falla, manda texto + álbum de respaldo.
publicar <- function(est, ruta, caption, corto, respaldo, silencioso, df = NULL, foto = NULL) {
  msg <- NULL
  if (!is.null(ruta) && file.exists(ruta)) {
    if (largo(caption) <= 1024) {
      msg <- enviar_foto_archivo(est, ruta, caption, silencioso)
    } else {
      msg <- enviar_foto_archivo(est, ruta, corto, silencioso, con_botones = FALSE)
      if (!is.null(msg)) enviar_texto(est, caption, silencioso = TRUE)
    }
  }
  if (is.null(msg)) {
    log_message("Usando envío de respaldo (texto + fotos)")
    msg <- enviar_texto(est, respaldo, silencioso)
    graf <- if (!is.null(df)) tryCatch(graficar(df, est, file.path(tempdir(), "grafica_24h.png")), error = function(e) NULL)
    enviar_album(est, graf, foto, silencioso = TRUE)
  }
  msg
}

imagenes_estacion <- function(est) {
  foto <- tryCatch(get_ultima_foto(est), error = function(e) NULL)
  list(foto = foto,
       foto_img  = if (!is.null(foto)) leer_imagen(foto$url) else NULL,
       logos_img = leer_imagen(paste0(est$base_url, est$logos)))
}

hacer_tarjeta <- function(est, df, im, lluvia = NULL, alerta = FALSE) {
  tryCatch(generar_tarjeta(est, df, im$foto_img, im$foto, im$logos_img,
                           file.path(tempdir(), paste0("tarjeta_", est$id, ".png")), lluvia, alerta),
           error = function(e) { log_message(paste("Tarjeta:", e$message)); NULL })
}

# ============================================================
# 9) IDIOMAS (textos/<idioma>.json)
# ============================================================
textos_idiomas <- function(clave, est, valores) {
  out <- list()
  for (idi in GENERAL$idiomas) {
    ruta <- file.path(DIR, "textos", paste0(idi, ".json"))
    if (!file.exists(ruta)) next
    j <- tryCatch(fromJSON(ruta), error = function(e) NULL)
    if (is.null(j) || !isTRUE(j$activo)) next
    t <- j$mensajes[[clave]] %||% ""
    if (!nzchar(trimws(t))) next
    for (k in names(valores)) t <- gsub(paste0("{", k, "}"), valores[[k]], t, fixed = TRUE)
    out[[j$nombre %||% idi]] <- t
  }
  out
}

# ============================================================
# 10) ESTADO (estado/estado_<id>.json)
# ============================================================
ESTADO_VACIO <- list(nivel = 0, altura = NA_real_, hora = 0, pin = NA_real_,
                     offline = FALSE, caida_desde = NA_real_, aviso_offline = 0,
                     aviso_camara = 0, aviso_picos = 0, aviso_lluvia = 0)

leer_estado <- function(ruta) {
  e <- ESTADO_VACIO
  if (file.exists(ruta)) {
    j <- tryCatch(fromJSON(ruta), error = function(x) list())
    for (k in names(e)) if (!is.null(j[[k]]) && length(j[[k]]) > 0) e[[k]] <- j[[k]]
  }
  e$offline <- isTRUE(e$offline)
  for (k in setdiff(names(e), "offline")) e[[k]] <- as.numeric(e[[k]])
  e
}
guardar_estado <- function(ruta, e) {
  dir.create(dirname(ruta), showWarnings = FALSE, recursive = TRUE)
  nuevo <- as.character(toJSON(e, auto_unbox = TRUE, na = "null", digits = NA, pretty = TRUE))
  viejo <- if (file.exists(ruta)) paste(readLines(ruta, warn = FALSE), collapse = "\n") else ""
  if (!identical(nuevo, viejo)) writeLines(nuevo, ruta)
}
ahora_num <- function() as.numeric(Sys.time())
min_desde <- function(t) (ahora_num() - t) / 60

# ============================================================
# 11) AVISOS AL EQUIPO TÉCNICO
# ============================================================
revisar_equipo <- function(est, crudo, df, foto, e) {
  ult_f <- if (!is.null(crudo) && nrow(crudo) > 0) crudo$fecha[nrow(crudo)] else NULL
  caida <- is.null(ult_f) || horas_desde(ult_f) > est$horas_sin_datos

  if (caida) {
    if (!e$offline) e$caida_desde <- if (is.null(ult_f)) ahora_num() else as.numeric(ult_f)
    if (!e$offline || min_desde(e$aviso_offline) >= GENERAL$repetir_aviso_caida_h * 60) {
      desde <- as.POSIXct(e$caida_desde, origin = "1970-01-01", tz = TZ)
      admin(paste0(
        "🔧 <b>Estación ", est$nombre, " sin datos</b>\n",
        if (is.null(ult_f)) "No se pudo consultar el servidor de datos.\n"
        else sprintf("Última lectura: %s (%s)\n", fmt_lectura(ult_f), hace(ult_f)),
        if (!is.null(foto) && !is.na(foto$momento)) sprintf("Última foto de cámara: %s %s\n", foto$fecha, foto$hora) else "",
        sprintf("Sin datos desde hace %s.\n\n", duracion(horas_desde(desde))),
        "Revisar energía, conexión o sensor. Mientras tanto NO se pueden emitir alertas del río.\n",
        est$pagina))
      e$aviso_offline <- ahora_num()
    }
    e$offline <- TRUE
  } else if (e$offline) {
    dur <- (as.numeric(ult_f) - e$caida_desde) / 3600
    admin(sprintf("✅ <b>Estación %s volvió a reportar</b>\nEstuvo sin datos %s.\nLectura actual: %.2f m (%s)",
                  est$nombre, duracion(max(0, dur)), crudo$altura[nrow(crudo)], fmt_hora(ult_f)))
    e$offline <- FALSE; e$caida_desde <- NA_real_
  }

  if (!caida && !is.null(foto) && !is.na(foto$momento) && horas_desde(foto$momento) > GENERAL$foto_vieja_h &&
      min_desde(e$aviso_camara) >= 24 * 60) {
    admin(sprintf("📷 <b>Cámara de %s sin fotos nuevas</b>\nÚltima captura: %s %s (%s). Los datos de nivel sí llegan.",
                  est$nombre, foto$fecha, foto$hora, hace(foto$momento)))
    e$aviso_camara <- ahora_num()
  }

  desc <- attr(df, "descartes")
  n24 <- sum(desc > Sys.time() - 24 * 3600)
  if (n24 >= GENERAL$picos_para_avisar && min_desde(e$aviso_picos) >= 24 * 60) {
    admin(sprintf("⚠️ <b>%s: %d lecturas falsas descartadas en 24 h</b>\nPosible problema del sensor (obstrucción, vibración, cable). Últimas: %s",
                  est$nombre, n24, paste(fmt_hora(tail(desc, 3)), collapse = ", ")))
    e$aviso_picos <- ahora_num()
  }
  e
}

# ============================================================
# 12) MENSAJES
# ============================================================
caption_boletin <- function(est, df, lluvia) {
  r <- resumen(df, est)
  cab <- paste0("🌊 <b>SIGIA · Boletín hidrológico</b>\n<b>", est$rio, "</b> — ", fmt_fecha_larga(Sys.time()), "\n\n")
  if (is.null(r)) return(paste0(cab, "⚪ <b>SIN DATOS</b> · No fue posible consultar la estación.\n\n", est$hashtags))
  lineas <- if (r$offline)
    sprintf("📏 Última lectura: <b>%.2f m</b> · %s (%s)", r$ult$altura, format(r$ult$fecha, "%d/%m %H:%M", tz = TZ), hace(r$ult$fecha))
  else c(sprintf("📏 Altura: <b>%.2f m</b> (%s)", r$ult$altura, hace(r$ult$fecha)),
         paste("📈 Tendencia:", r$tend),
         paste("🎯 Margen:", distancia_umbral(r$ult$altura, est)),
         if (!is.null(texto_proyeccion(r$proy))) paste("⏱", texto_proyeccion(r$proy)),
         texto_lluvia(lluvia))
  paste0(cab, r$et$emoji, " <b>", r$et$nombre, "</b>\n", paste(lineas, collapse = "\n"), "\n\n", est$hashtags)
}

boletin_html <- function(est, df, lluvia) {      # texto completo de respaldo
  r <- resumen(df, est)
  if (is.null(r) || r$offline) return(caption_boletin(est, df, lluvia))
  d <- r$d24
  paste0("🌊 <b>SIGIA · BOLETÍN HIDROLÓGICO</b>\n<b>", est$rio, "</b> — ", fmt_fecha_larga(Sys.time()), "\n\n",
         r$et$emoji, " <b>", r$et$nombre, "</b>\n", barra(r$ult$altura, est), "\n<pre>",
         fila("Altura actual", sprintf("%.2f m  (%s)", r$ult$altura, hace(r$ult$fecha))), "\n",
         fila("Tendencia", r$tend), "\n",
         fila("Máx. 24 h", sprintf("%.2f m  %s", d$altura[r$imax], fmt_hora(d$fecha[r$imax]))), "\n",
         fila("Mín. 24 h", sprintf("%.2f m  %s", d$altura[r$imin], fmt_hora(d$fecha[r$imin]))), "\n",
         fila("Margen", distancia_umbral(r$ult$altura, est)), "</pre>",
         if (!is.null(texto_lluvia(lluvia))) paste0(texto_lluvia(lluvia), "\n") else "",
         "\n", est$hashtags)
}

alerta_html <- function(est, ult, s, motivo, n, proy, lluvia, idiomas) {
  clave <- clave_motivo(motivo, n)
  titulo <- switch(motivo,
    nivel  = paste0(NIVELES[[n + 1]]$emoji, " <b>", NIVELES[[n + 1]]$nombre, "</b>"),
    subida = "🟡 <b>AVISO · EL RÍO ESTÁ SUBIENDO</b>",
    lluvia = "🌧️ <b>AVISO PREVENTIVO · LLUVIA FUERTE EN LA CUENCA ALTA</b>",
    normal = "✅ <b>EL RÍO REGRESA A NIVEL NORMAL</b>")
  recomendacion <- switch(clave,
    normal  = "Se mantiene el monitoreo continuo de la estación.",
    peligro = "‼️ <b>Aléjese del cauce del río.</b> Si vive en zonas cercanas, prepárese para evacuar y siga las indicaciones de las autoridades locales.",
    alerta  = "‼️ <b>Evite acercarse o cruzar el río.</b> Manténgase atento a nuevos avisos.",
    lluvia  = "⚠️ El río puede subir en las próximas horas. Evite cruzar el cauce y manténgase atento.",
    "⚠️ Evite acercarse al cauce y manténgase atento a nuevos avisos.")
  tp <- texto_proyeccion(proy)
  tabla <- c(fila("Altura actual", sprintf("%.2f m  (%s)", ult$altura, fmt_hora(ult$fecha))),
             if (motivo %in% c("nivel", "subida") && s$subida > 0) fila("Subida", sprintf("+%.2f m en %d min", s$subida, s$mins)),
             if (!is.null(proy) && !is.na(proy$ritmo) && proy$ritmo > 0.05 && motivo != "normal") fila("Ritmo", sprintf("%+.2f m/h", proy$ritmo)),
             if (!is.null(tp)) fila("Llegaría", paste(fmt_eta(proy$minutos), "a", proy$umbral)),
             fila("Margen", distancia_umbral(ult$altura, est)))
  extra <- if (length(idiomas)) paste0(vapply(names(idiomas), function(nm) paste0("🗣️ <b>", nm, ":</b> ", idiomas[[nm]]), ""),
                                       collapse = "\n\n") else NULL
  paste0("🌊 <b>SIGIA · ", mayus(est$rio), "</b>\n", titulo, "\n", fmt_fecha_larga(Sys.time()), "\n\n",
         if (motivo == "lluvia") texto_lluvia(lluvia) else barra(ult$altura, est), "\n",
         "<pre>", paste(tabla, collapse = "\n"), "</pre>\n",
         if (!is.null(tp) && motivo != "normal") paste0("⏱ <b>", tp, "</b>\n") else "",
         if (motivo != "lluvia" && !is.null(lluvia) && lluvia$mm_1h[1] >= 1) paste0(texto_lluvia(lluvia), "\n") else "",
         "\n", recomendacion, "\n\n",
         if (!is.null(extra)) paste0(extra, "\n\n") else "",
         if (motivo != "normal") paste0("📞 <b>Emergencias:</b> ", GENERAL$emergencias, "\n\n") else "",
         "<i>Información automática de la estación de monitoreo. Los avisos oficiales los emite CONRED.</i>\n",
         est$hashtags, " #Alerta")
}

clave_motivo <- function(motivo, n) switch(motivo, nivel = NIVELES[[n + 1]]$clave, motivo)

# ============================================================
# 12b) VOZ AUTOMÁTICA (nota de voz que lee el boletín y las alertas)
#   Motor principal: edge-tts (voces neuronales de Guatemala, es-GT).
#   Respaldo: espeak-ng. Se instalan solos en GitHub la primera vez que se usan.
# ============================================================
MESES_LARGOS <- c("enero","febrero","marzo","abril","mayo","junio","julio","agosto",
                  "septiembre","octubre","noviembre","diciembre")

voz_metros <- function(a) {
  cm <- round(abs(a) * 100)
  cent <- function(x) paste(x, if (x == 1) "centímetro" else "centímetros")
  if (cm < 100) return(cent(cm))
  m <- cm %/% 100; r <- cm %% 100
  paste0(m, if (m == 1) " metro" else " metros", if (r > 0) paste0(" con ", cent(r)) else "")
}
voz_hora <- function(t) {
  h <- as.integer(format(t, "%H", tz = TZ)); mi <- as.integer(format(t, "%M", tz = TZ))
  h12 <- if (h %% 12 == 0) 12 else h %% 12
  paste0(if (h12 == 1) "la una" else paste("las", h12), if (mi > 0) paste(" y", mi) else "",
         if (h < 12) " de la mañana" else if (h == 12) " del mediodía" else if (h < 19) " de la tarde" else " de la noche")
}
voz_saludo <- function(t) { h <- as.integer(format(t, "%H", tz = TZ))
  if (h < 12) "Buenos días." else if (h < 19) "Buenas tardes." else "Buenas noches." }
voz_fecha <- function(t) sprintf("%s %d de %s", tolower(DIAS[as.integer(format(t, "%w", tz = TZ)) + 1]),
                                 as.integer(format(t, "%d", tz = TZ)), MESES_LARGOS[as.integer(format(t, "%m", tz = TZ))])
voz_minutos <- function(m) if (m < 60) paste(m, "minutos") else {
  h <- m %/% 60; r <- m %% 60
  paste0(h, if (h == 1) " hora" else " horas", if (r > 0) paste(" y", r, "minutos") else "")
}
voz_emergencias <- "Si tiene una emergencia, llame a CONRED al uno, cinco, seis, seis. O a los Bomberos, al uno, dos, dos."

voz_tendencia <- function(df, ult) {
  previas <- df[df$fecha <= ult$fecha - 3600, ]
  ref <- if (nrow(previas) > 0) previas[nrow(previas), ] else df[1, ]
  d <- ult$altura - ref$altura
  if (abs(d) < 0.02) "y se mantiene estable, sin cambios"
  else if (d > 0) paste("y subió", voz_metros(d), "en la última hora")
  else paste("y bajó", voz_metros(d), "en la última hora")
}
voz_proyeccion <- function(p) {
  if (is.null(p) || is.null(p$minutos)) return(NULL)
  sprintf("Si sigue subiendo a este ritmo, podría llegar al nivel de %s en aproximadamente %s.", p$umbral, voz_minutos(p$minutos))
}

# Cambio en un periodo, en palabras ("subió 12 centímetros", "se mantuvo igual")
voz_cambio <- function(d, verbo_igual = "se mantuvo prácticamente igual") {
  if (is.na(d) || abs(d) < 0.02) verbo_igual
  else paste(if (d > 0) "subió" else "bajó", voz_metros(d))
}
# Altura más cercana a un momento dado (máximo 40 min de diferencia)
altura_en <- function(df, t) {
  dif <- abs(as.numeric(difftime(df$fecha, t, units = "mins")))
  if (min(dif) > 40) NA else df$altura[which.min(dif)]
}
voz_tendencia <- function(df, ult) {
  previas <- df[df$fecha <= ult$fecha - 3600, ]
  ref <- if (nrow(previas) > 0) previas[nrow(previas), ] else df[1, ]
  d <- ult$altura - ref$altura
  if (abs(d) < 0.02) "y se mantiene estable, sin cambios"
  else if (d >= 0.20) paste("y está subiendo rápido. En la última hora subió", voz_metros(d))
  else if (d > 0) paste("y está subiendo poco a poco. En la última hora subió", voz_metros(d))
  else paste("y está bajando. En la última hora bajó", voz_metros(d))
}
# Hora del siguiente boletín, según GENERAL$voz$horarios_boletin
voz_proximo_boletin <- function(despedida = TRUE) {
  hs <- sort(GENERAL$voz$horarios_boletin %||% c(6, 12, 18))
  h  <- as.integer(format(Sys.time(), "%H", tz = TZ))
  sig <- hs[hs > h]
  hora <- function(x) if (x == 12) "a las 12 del mediodía" else if (x < 12) sprintf("a las %d de la mañana", x)
                      else if (x < 19) sprintf("a las %d de la tarde", x - 12) else sprintf("a las %d de la noche", x - 12)
  adios <- if (!despedida) "" else if (h < 12) " Que tenga buen día." else if (h < 19) " Que tenga buena tarde." else " Que pase buena noche."
  if (length(sig)) paste0("Le daremos el próximo informe ", hora(sig[1]), ".", adios)
  else paste0("Le daremos el próximo informe mañana ", hora(hs[1]), ".", adios)
}

texto_voz_boletin <- function(est, df, lluvia) {
  ahora <- Sys.time()
  cab <- sprintf("%s Le compartimos cómo está el %s este %s, a %s.", voz_saludo(ahora), est$rio, voz_fecha(ahora), voz_hora(ahora))
  r <- resumen(df, est)
  if (is.null(r)) return(paste(cab, "En este momento no fue posible consultar la estación de monitoreo. Intentaremos de nuevo en el próximo boletín."))
  if (r$offline) return(paste(cab, sprintf("La estación de monitoreo no envía datos desde el %s, a %s. La última altura registrada fue de %s. Mientras la estación esté sin conexión, tome precauciones si llueve fuerte.",
                                           voz_fecha(r$ult$fecha), voz_hora(r$ult$fecha), voz_metros(r$ult$altura))))
  a <- r$ult$altura; t <- r$ult$fecha
  nivel <- c("normal", "preventivo", "de alerta", "de peligro")[r$n + 1]
  partes <- c(cab, sprintf("En este momento, el río tiene una altura de %s. Está en nivel %s %s.", voz_metros(a), nivel, voz_tendencia(df, r$ult)))

  a1 <- altura_en(df, t - 3600); sube_1h <- !is.na(a1) && (a - a1) >= 0.10
  a6 <- altura_en(df, t - 6 * 3600); a24 <- altura_en(df, t - 24 * 3600)
  urgente <- r$n >= 2                      # en alerta o peligro el audio va directo a lo importante

  # Desde el boletín anterior (6 horas) y comparación con ayer a la misma hora
  if (!is.na(a6) && abs(a - a6) >= 0.05) partes <- c(partes, paste0("En las últimas seis horas el río ", voz_cambio(a - a6), "."))
  if (!urgente && !is.na(a24)) partes <- c(partes, if (abs(a - a24) < 0.03) "Está más o menos igual que ayer a esta hora."
                               else sprintf("Comparado con ayer a esta hora, está %s más %s.", voz_metros(a - a24), if (a > a24) "alto" else "bajo"))

  # Resumen de 24 horas
  d <- r$d24
  if (!urgente && nrow(d) >= 6) {
    rango <- diff(range(d$altura))
    a_las <- function(x) sub("^las", "a las", sub("^la una", "a la una", voz_hora(x)))
    partes <- c(partes,
      if (rango < 0.10) sprintf("Durante el último día, el río estuvo tranquilo, entre %s y %s.", sub(" centímetros?$", "", voz_metros(min(d$altura))), voz_metros(max(d$altura)))
      else if (r$imax == nrow(d) || as.numeric(difftime(t, d$fecha[r$imax], units = "mins")) < 30)
        sprintf("Es el nivel más alto del último día. El más bajo fue de %s.", voz_metros(d$altura[r$imin]))
      else sprintf("Durante el último día, lo más alto que llegó fue %s, %s. Y lo más bajo, %s.",
                   voz_metros(d$altura[r$imax]), a_las(d$fecha[r$imax]), voz_metros(d$altura[r$imin])))
  }

  # Margen y proyección
  falta <- function(x) { v <- voz_metros(x); paste(if (grepl("^1 (metro|centímetro)($| )", v)) "Falta" else "Faltan", v) }
  partes <- c(partes, voz_proyeccion(r$proy),
              if (a < est$umbral_alerta) paste("Todavía", tolower(falta(est$umbral_alerta - a)), "para llegar al nivel de alerta.")
              else if (a < est$umbral_peligro) paste(falta(est$umbral_peligro - a), "para llegar al nivel de peligro.")
              else "El río está por encima del nivel de peligro.")

  # Lluvia en la cuenca alta (si hay estaciones configuradas)
  if (!is.null(lluvia)) partes <- c(partes,
    if (lluvia$mm_1h[1] >= 1) sprintf("En la parte alta de la cuenca está lloviendo: %s milímetros en la última hora y %s en lo que va del día.",
                                      round(lluvia$mm_1h[1]), round(lluvia$mm_hoy[1]))
    else if (lluvia$mm_hoy[1] >= 1) sprintf("En la parte alta de la cuenca han caído %s milímetros de lluvia hoy, pero no llueve en este momento.", round(lluvia$mm_hoy[1]))
    else "No se registra lluvia en la parte alta de la cuenca.")

  # Recomendación según el nivel
  partes <- c(partes, switch(as.character(r$n),
    "0" = if (r$s$subida >= est$subida_minima || sube_1h) "Como el río está subiendo, le pedimos estar atento a los próximos avisos y no cruzar el cauce."
          else "Por ahora no hay peligro. Pero recuerde: si llueve fuerte, mejor no cruce el río.",
    "1" = "Le pedimos estar atento, evitar cruzar el río y seguir los próximos avisos.",
    "2" = c("No se acerque ni cruce el río.", voz_emergencias),
    "3" = c("Aléjese del cauce del río y siga las indicaciones de las autoridades.", voz_emergencias)))
  paste(c(partes, voz_proximo_boletin(despedida = !urgente)), collapse = " ")
}

texto_voz_alerta <- function(est, ult, s, motivo, n, proy, lluvia) {
  ahora <- Sys.time()
  if (motivo == "normal")
    return(sprintf("Aviso del %s, %s. El río regresó a su nivel normal. Ahora tiene una altura de %s. Seguimos vigilando el río.",
                   est$rio, voz_hora(ahora), voz_metros(ult$altura)))
  titulo <- switch(motivo,
    nivel  = c("", "Aviso preventivo", "Alerta naranja", "Alerta roja. Peligro")[n + 1],
    subida = "Aviso. El río está subiendo",
    lluvia = "Aviso preventivo por lluvia fuerte en la parte alta")
  partes <- c(sprintf("Atención. Atención. %s en el %s. Son %s.", titulo, est$rio, voz_hora(ahora)))
  if (motivo == "lluvia")
    partes <- c(partes, sprintf("Se registraron %s milímetros de lluvia en la última hora en %s. El río puede subir en las próximas horas.",
                                round(lluvia$mm_1h[1]), lluvia$nombre[1]))
  partes <- c(partes, sprintf("El río tiene una altura de %s%s.", voz_metros(ult$altura),
                              if (motivo != "lluvia" && s$subida >= 0.05) paste0(" y subió ", voz_metros(s$subida), " en ", voz_minutos(s$mins)) else ""),
              voz_proyeccion(proy),
              switch(as.character(n),
                     "3" = "Aléjese del cauce del río. Si vive cerca, prepárese para salir y siga las indicaciones de las autoridades.",
                     "2" = "No se acerque ni cruce el río. Esté atento a nuevos avisos.",
                     "No se acerque al cauce del río y esté atento a nuevos avisos."),
              voz_emergencias, "Le avisaremos si la situación cambia.")
  paste(partes, collapse = " ")
}
mayus1 <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))

# Google Cloud Text-to-Speech · Chirp 3 HD (devuelve OGG/Opus listo para nota de voz)
voz_google <- function(texto, ogg) {
  cfg <- GENERAL$voz
  nombre <- cfg$google_voz %||% "es-US-Chirp3-HD-Aoede"
  idioma <- sub("^([a-z]{2}-[A-Z]{2}).*", "\\1", nombre)
  res <- tryCatch(POST("https://texttospeech.googleapis.com/v1/text:synthesize",
                       query = list(key = GOOGLE_TTS_KEY), encode = "json", timeout(60),
                       body = list(input = list(text = texto),
                                   voice = list(languageCode = idioma, name = nombre),
                                   audioConfig = list(audioEncoding = "OGG_OPUS",
                                                      speakingRate = cfg$google_velocidad %||% 1))),
                  error = function(e) { log_message(paste("Google TTS error de red:", e$message)); NULL })
  if (is.null(res)) return(NULL)
  j <- tryCatch(fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE), error = function(e) list())
  if (status_code(res) != 200) {
    log_message(sprintf("Google TTS falló (HTTP %d): %s", status_code(res), j$error$message %||% ""))
    return(NULL)
  }
  if (is.null(j$audioContent)) return(NULL)
  writeBin(jsonlite::base64_dec(j$audioContent), ogg)
  if (file.exists(ogg) && file.size(ogg) > 1000) ogg else NULL
}

# Instala lo necesario para la voz (solo en GitHub/Linux)
asegurar_motor <- function() {
  if (.Platform$OS.type != "unix") return(invisible())
  if (system2("python3", c("-c", shQuote("import edge_tts")), stdout = FALSE, stderr = FALSE) != 0) {
    log_message("Instalando edge-tts…")
    out <- system2("python3", c("-m", "pip", "install", "--quiet", "--user", "--upgrade", "--break-system-packages", "edge-tts"),
                   stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(out, "status"))) log_message(paste("pip:", paste(tail(out, 3), collapse = " | ")))
  }
  if (!nzchar(Sys.which("ffmpeg"))) {
    log_message("Instalando ffmpeg…")
    system("sudo -n apt-get install -y -qq ffmpeg > /dev/null 2>&1 || (sudo -n apt-get update -qq > /dev/null 2>&1 && sudo -n apt-get install -y -qq ffmpeg > /dev/null 2>&1)")
  }
}

# Voz neuronal de Microsoft (edge-tts) usando la librería directamente, con 2 intentos
voz_microsoft <- function(txt, mp3) {
  cfg <- GENERAL$voz
  py <- file.path(tempdir(), "voz_edge.py")
  writeLines(c("import asyncio, sys, edge_tts",
               "texto = open(sys.argv[1], encoding='utf-8').read()",
               "asyncio.run(edge_tts.Communicate(texto, sys.argv[3], rate=sys.argv[4]).save(sys.argv[2]))"), py)
  for (i in 1:2) {
    unlink(mp3)
    out <- system2("python3", c(shQuote(py), shQuote(txt), shQuote(mp3), cfg$voz %||% "es-GT-MartaNeural",
                                shQuote(cfg$velocidad %||% "+0%")), stdout = TRUE, stderr = TRUE)
    if (file.exists(mp3) && file.size(mp3) > 2000) return(mp3)
    log_message(paste0("Voz Microsoft falló (intento ", i, "): ", paste(tail(out, 2), collapse = " | ")))
    Sys.sleep(3)
  }
  NULL
}

# Genera el audio y devuelve la ruta (.ogg para nota de voz)
# Orden: Google Chirp 3 HD → Microsoft es-GT → (voz básica solo si se permite en config.R)
generar_voz <- function(texto, nombre) {
  cfg <- GENERAL$voz
  dir <- tempdir()
  if (nzchar(GOOGLE_TTS_KEY)) {
    r <- voz_google(texto, file.path(dir, paste0(nombre, "_google.ogg")))
    if (!is.null(r)) { log_message("Voz generada con Google Chirp 3 HD"); return(r) }
    log_message("Google TTS no disponible: probando voz de Microsoft")
  }
  txt <- file.path(dir, paste0(nombre, ".txt")); writeLines(enc2utf8(texto), txt, useBytes = TRUE)
  mp3 <- file.path(dir, paste0(nombre, ".mp3")); ogg <- file.path(dir, paste0(nombre, ".ogg"))
  unlink(c(mp3, ogg))
  asegurar_motor()

  fuente <- voz_microsoft(txt, mp3)
  if (!is.null(fuente)) log_message(paste("Voz generada con Microsoft", cfg$voz))
  if (is.null(fuente) && isTRUE(cfg$permitir_voz_basica)) {
    if (!nzchar(Sys.which("espeak-ng"))) system("sudo -n apt-get install -y -qq espeak-ng > /dev/null 2>&1")
    wav <- file.path(dir, paste0(nombre, ".wav"))
    if (system2("espeak-ng", c("-v", "es-419", "-s", "145", "-f", shQuote(txt), "-w", shQuote(wav)), stdout = FALSE, stderr = FALSE) == 0 &&
        file.exists(wav)) { fuente <- wav; log_message("Voz generada con la voz básica (espeak-ng)") }
  }
  if (is.null(fuente)) {
    log_message("No se generó audio: ninguna voz natural disponible (se omite la nota de voz)")
    admin("🔇 <b>No se pudo generar la nota de voz</b> (voces de Google y Microsoft no disponibles). El mensaje escrito sí se publicó. Revise el registro en GitHub → Actions.")
    return(NULL)
  }
  if (nzchar(Sys.which("ffmpeg")) &&
      system2("ffmpeg", c("-y", "-loglevel", "error", "-i", shQuote(fuente), "-c:a", "libopus", "-b:a", "48k",
                          "-ac", "1", "-ar", "48000", shQuote(ogg)), stdout = FALSE, stderr = FALSE) == 0 &&
      file.exists(ogg) && file.size(ogg) > 1000) return(ogg)
  fuente
}

enviar_voz <- function(est, ruta, caption, silencioso = TRUE) {
  if (is.null(ruta)) return(invisible())
  if (grepl("\\.ogg$", ruta))
    tg("sendVoice", list(chat_id = est$channel_id, voice = upload_file(ruta, type = "audio/ogg"),
                         caption = caption, disable_notification = bool(silencioso)), multipart = TRUE)
  else
    tg("sendAudio", list(chat_id = est$channel_id, audio = upload_file(ruta), title = caption,
                         caption = caption, disable_notification = bool(silencioso)), multipart = TRUE)
}

# ============================================================
# 13) MODOS
# ============================================================
modo_boletin <- function(est) {
  df <- limpiar(get_lecturas(est), est)
  ll <- obtener_lluvia(est)
  im <- imagenes_estacion(est)
  cap <- caption_boletin(est, df, ll)
  cat(cap, "\n")
  ruta <- if (!is.null(df) && nrow(df) > 0) hacer_tarjeta(est, df, im, ll) else NULL
  publicar(est, ruta, cap, cap, boletin_html(est, df, ll), silencioso = TRUE, df = df, foto = im$foto)
  if (isTRUE(GENERAL$voz$en_boletin)) {
    txt <- texto_voz_boletin(est, df, ll); log_message(paste("Voz:", txt))
    voz <- tryCatch(generar_voz(txt, paste0("boletin_", est$id)), error = function(e) NULL)
    enviar_voz(est, voz, paste0("🔊 Boletín en audio · ", est$rio), silencioso = TRUE)
  }
}

modo_alerta <- function(est) {
  e <- leer_estado(est$archivo_estado)
  crudo <- get_lecturas(est)
  df <- limpiar(crudo, est)
  foto <- tryCatch(get_ultima_foto(est), error = function(x) NULL)
  e <- revisar_equipo(est, crudo, df, foto, e)
  on.exit(guardar_estado(est$archivo_estado, e), add = TRUE)

  if (is.null(df) || nrow(df) < 2 || horas_desde(df$fecha[nrow(df)]) > est$horas_sin_datos) {
    log_message(paste(est$id, ": sin datos recientes, no se evalúan alertas")); return(invisible())
  }
  if (isTRUE(attr(df, "pendiente"))) log_message(paste(est$id, ": última lectura con salto brusco, esperando confirmación"))

  ult  <- df[nrow(df), ]
  conf <- min(tail(df$altura, est$confirmar_lecturas))         # debe sostenerse en N lecturas
  n    <- nivel_altura(conf, est)
  s    <- subida_reciente(df, conf, est)
  sube <- s$subida >= est$subida_minima
  ll   <- obtener_lluvia(est)
  proy <- proyeccion(df, est)

  motivo <- NULL
  if (n > e$nivel && n >= 1) motivo <- "nivel"
  else if (sube && (is.na(e$altura) || conf >= e$altura + est$repetir_si_sube || min_desde(e$hora) >= est$espera_min)) motivo <- "subida"
  else if (n == 0 && e$nivel >= 1) motivo <- "normal"
  else if (!is.null(ll) && ll$mm_1h[1] >= est$lluvia_aviso_1h && min_desde(e$aviso_lluvia) >= est$lluvia_espera_min) motivo <- "lluvia"

  if (is.null(motivo)) {
    log_message(sprintf("%s: sin alerta · %.2f m · subida %.2f m en %d min", est$id, conf, s$subida, s$mins))
    if (n < e$nivel) e$nivel <- n
    return(invisible())
  }

  clave   <- clave_motivo(motivo, n)
  idiomas <- textos_idiomas(clave, est, list(rio = est$rio, altura = sprintf("%.2f", ult$altura),
                                             mm = if (!is.null(ll)) sprintf("%.0f", ll$mm_1h[1]) else ""))
  html  <- alerta_html(est, ult, s, motivo, n, proy, ll, idiomas)
  corto <- paste0(strsplit(html, "\n\n")[[1]][1], "\n", est$hashtags, " #Alerta")
  cat(html, "\n")

  im   <- list(foto = foto, foto_img = if (!is.null(foto)) leer_imagen(foto$url) else NULL,
               logos_img = leer_imagen(paste0(est$base_url, est$logos)))
  ruta <- hacer_tarjeta(est, df, im, ll, alerta = TRUE)
  msg  <- publicar(est, ruta, html, corto, html, silencioso = FALSE, df = df, foto = foto)   # alertas SÍ suenan
  if (isTRUE(GENERAL$voz$en_alertas)) {
    txt <- texto_voz_alerta(est, ult, s, motivo, n, proy, ll); log_message(paste("Voz:", txt))
    voz <- tryCatch(generar_voz(txt, paste0("alerta_", est$id)), error = function(e) NULL)
    enviar_voz(est, voz, paste0("🔊 Escuche la alerta · ", est$rio), silencioso = TRUE)
  }
  enviar_audios(est, clave, silencioso = TRUE)          # grabaciones de la comunidad (si existen)

  if (motivo == "normal" && !is.na(e$pin)) {
    tg("unpinChatMessage", list(chat_id = est$channel_id, message_id = e$pin)); e$pin <- NA_real_
  }
  if (!is.null(msg) && motivo == "nivel" && n >= est$fijar_desde) {
    if (!is.na(e$pin)) tg("unpinChatMessage", list(chat_id = est$channel_id, message_id = e$pin))
    if (!is.null(tg("pinChatMessage", list(chat_id = est$channel_id, message_id = msg$message_id, disable_notification = TRUE))))
      e$pin <- msg$message_id
  }
  if (isTRUE(GENERAL$whatsapp_al_equipo) && nzchar(ADMIN)) {
    admin("📲 <b>Alerta publicada.</b> Texto listo para copiar y reenviar por WhatsApp 👇")
    tg("sendMessage", list(chat_id = ADMIN, text = a_whatsapp(html, est), disable_web_page_preview = TRUE))
  }

  if (motivo == "lluvia") e$aviso_lluvia <- ahora_num()
  else { e$nivel <- n; e$altura <- if (motivo == "normal") NA_real_ else conf; e$hora <- ahora_num() }
  log_message(paste(est$id, ": alerta enviada ·", motivo))
}

datos_semana <- function(df, est) {
  fin <- as.POSIXct(format(Sys.time(), "%Y-%m-%d", tz = TZ), tz = TZ)
  inicio <- fin - 7 * 86400
  d <- df[df$fecha >= inicio & df$fecha < fin, ]
  if (nrow(d) == 0) return(NULL)
  dias <- seq(inicio, by = "day", length.out = 7)
  diario <- do.call(rbind, lapply(dias, function(x) {
    v <- d[d$fecha >= x & d$fecha < x + 86400, ]
    if (nrow(v) == 0) return(data.frame(dia = x, max = NA_real_, maxf = as.POSIXct(NA)))
    data.frame(dia = x, max = max(v$altura), maxf = v$fecha[which.max(v$altura)])
  }))
  dt <- c(diff(as.numeric(d$fecha)), 0) / 3600; dt[dt > 1] <- 0     # no contar huecos
  i <- which.max(d$altura)
  list(datos = d, diario = diario, inicio = inicio, fin = fin,
       rango = sprintf("%d %s – %d %s %s", as.integer(format(inicio, "%d", tz = TZ)), MESES[as.integer(format(inicio, "%m", tz = TZ))],
                       as.integer(format(fin - 1, "%d", tz = TZ)), MESES[as.integer(format(fin - 1, "%m", tz = TZ))], format(fin - 1, "%Y", tz = TZ)),
       max = d$altura[i], max_f = d$fecha[i], min = min(d$altura), prom = mean(d$altura),
       horas_sobre = sum(dt[d$altura >= est$umbral_preventivo]),
       horas_datos = length(unique(format(d$fecha, "%Y%m%d%H", tz = TZ))))
}

modo_semanal <- function(est) {
  df  <- limpiar(get_lecturas(est, n = 5000), est)
  sem <- if (!is.null(df) && nrow(df) > 0) datos_semana(df, est) else NULL
  if (is.null(sem)) {
    enviar_texto(est, paste0("📅 <b>SIGIA · Resumen semanal</b>\n<b>", est$rio, "</b>\n\n⚪ La estación no envió datos esta semana.\n\n",
                             est$hashtags, " #ResumenSemanal"), silencioso = TRUE)
    admin(paste0("🔧 <b>", est$nombre, ":</b> sin datos en toda la semana."))
    return(invisible())
  }
  pct <- round(sem$horas_datos / 168 * 100)
  cap <- paste0("📅 <b>SIGIA · Resumen semanal</b>\n<b>", est$rio, "</b> — ", sem$rango, "\n\n",
                sprintf("📈 Máxima: <b>%.2f m</b> (%s, %s)\n", sem$max, dia_corto(sem$max_f), fmt_hora(sem$max_f)),
                sprintf("📉 Mínima: %.2f m · Promedio: %.2f m\n", sem$min, sem$prom),
                sprintf("⚠️ Horas en nivel preventivo o más: %s h\n", format(round(sem$horas_sobre, 1))),
                sprintf("📡 Datos recibidos: %d de 168 h (%d %%)\n\n", sem$horas_datos, pct),
                est$hashtags, " #ResumenSemanal")
  cat(cap, "\n")
  logos <- leer_imagen(paste0(est$base_url, est$logos))
  ruta <- tryCatch(generar_tarjeta_semanal(est, sem, logos, file.path(tempdir(), paste0("semanal_", est$id, ".png"))),
                   error = function(e) { log_message(paste("Tarjeta semanal:", e$message)); NULL })
  publicar(est, ruta, cap, cap, cap, silencioso = TRUE)
  if (pct < 90) admin(sprintf("📡 <b>%s:</b> solo llegaron datos en %d de 168 horas esta semana (%d %%). Conviene revisar la estación.",
                              est$nombre, sem$horas_datos, pct))
}

main <- function() {
  if (!nzchar(TELEGRAM_TOKEN)) stop("Falta TELEGRAM_TOKEN (secreto de GitHub o .Renviron)")
  args <- commandArgs(trailingOnly = TRUE)
  modo <- tolower(args[1]); if (is.na(modo)) modo <- "boletin"
  ests <- estaciones_activas(if (length(args) >= 2) args[2] else NA)
  for (est in ests) {
    log_message(paste("SIGIA ·", est$rio, "- modo:", modo))
    tryCatch(switch(modo, alerta = modo_alerta(est), semanal = modo_semanal(est), modo_boletin(est)),
             error = function(e) {
               log_message(paste("ERROR en", est$id, ":", conditionMessage(e)))
               admin(sprintf("❗ <b>Error del monitor</b> (%s · %s)\n<code>%s</code>", est$nombre, modo,
                             gsub("<", "&lt;", conditionMessage(e))))
             })
  }
  log_message("Proceso finalizado.")
}

if (!interactive() && !isTRUE(getOption("sigia.no_main"))) main()
