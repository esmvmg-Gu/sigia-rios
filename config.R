# ============================================================
# CONFIGURACIÓN · SIGIA Monitor de Ríos para Telegram
# Normalmente este es el ÚNICO archivo que necesitas editar.
#
# Secretos (GitHub → Settings → Secrets and variables → Actions):
#   TELEGRAM_TOKEN    obligatorio · token del bot (@BotFather)
#   TELEGRAM_CHAT_ID  opcional    · canal público (si falta, usa canal_por_defecto)
#   ADMIN_CHAT_ID     recomendado · chat privado del equipo técnico
#   WU_API_KEY        opcional    · llave de Wunderground para la lluvia
#   GOOGLE_TTS_KEY    opcional    · llave de Google Text-to-Speech (voz más natural)
# ============================================================

GENERAL <- list(
  zona_horaria      = "America/Guatemala",
  base_url          = "https://www.sigiagt.com/",
  menu              = "https://www.sigiagt.com/index.html",
  canal_por_defecto = "-1003701304473",   # canal ESTACION PANAJACHEL / RIO
  bot_usuario       = "",     # usuario del bot sin @ (ej. "SigiaRiosBot") → agrega botón "Consultar al bot"
  emergencias       = "CONRED 1566 · Bomberos 122 / 123 · PNC 110",

  # Avisos privados al equipo técnico (ADMIN_CHAT_ID)
  repetir_aviso_caida_h = 24,  # recordar cada 24 h mientras la estación siga caída
  foto_vieja_h          = 6,   # avisar si la cámara no manda foto en este tiempo
  picos_para_avisar     = 3,   # avisar si se descartan tantas lecturas falsas en 24 h
  whatsapp_al_equipo    = TRUE,# enviar al equipo una copia de cada alerta lista para WhatsApp

  # Idiomas adicionales en las alertas (archivos en textos/<idioma>.json)
  idiomas = c("kaqchikel"),

  # Nota de voz automática que lee el boletín y las alertas
  # Motor principal: Google Chirp 3 HD (la más natural; requiere el secreto GOOGLE_TTS_KEY).
  # Si no hay llave o falla, usa la voz de Guatemala de Microsoft, y luego una voz básica.
  voz = list(
    en_boletin = TRUE,
    en_alertas = TRUE,
    google_voz       = "es-US-Chirp3-HD-Aoede",  # mujer. Otras: Kore, Leda, Despina (mujer) · Charon, Puck, Orus, Fenrir (hombre)
    google_velocidad = 0.95,                     # 1.0 = normal
    voz        = "es-GT-MartaNeural",            # respaldo (Microsoft). Hombre: "es-GT-AndresNeural"
    velocidad  = "-5%"
  )
)

# Valores que usan todas las estaciones (cada estación puede cambiarlos abajo)
POR_DEFECTO <- list(
  horas_sin_datos = 2,

  # Alertas por subida
  subida_minima   = 0.15,   # m que debe subir...
  ventana_min     = 30,     # ...en estos minutos
  repetir_si_sube = 0.10,   # nueva alerta si sigue subiendo esta cantidad
  espera_min      = 30,     # mínimo entre alertas repetidas
  fijar_desde     = 2,      # fija en el canal alertas naranja (2) y roja (3)

  # Filtro de lecturas falsas
  altura_min_valida  = 0,     # m · se descartan lecturas fuera de este rango
  altura_max_valida  = 8,
  salto_max          = 0.50,  # m entre dos lecturas seguidas: si no se repite, es lectura falsa
  confirmar_lecturas = 2,     # lecturas seguidas que deben superar un umbral para alertar

  # Proyección de llegada
  ritmo_minimo      = 0.10,   # m/h · por debajo de esto no se proyecta
  proyeccion_max_h  = 6,      # no se muestran proyecciones más lejanas

  # Lluvia en la cuenca alta (estaciones Wunderground)
  lluvia            = list(),
  lluvia_aviso_1h   = 20,     # mm en 1 h para enviar aviso preventivo por lluvia
  lluvia_espera_min = 60      # mínimo entre avisos por lluvia
)

ESTACIONES <- list(

  list(
    id       = "panajachel",
    activa   = TRUE,
    nombre   = "Panajachel",
    rio      = "Río Panajachel",
    datos    = "datos_pana.php",
    fotos    = "get_fotos_pana.php",
    pagina   = "https://www.sigiagt.com/panajachel.html",
    logos    = "Logos-Panajachel.png",
    hashtags = "#Panajachel #SIGIA #NivelDelRío",

    # Umbrales (m). Alerta y peligro son los de la gráfica de la página.
    # Usa analizar_umbrales.R y valida con COMRED / Municipalidad.
    umbral_preventivo = 2.00,
    umbral_alerta     = 2.50,
    umbral_peligro    = 4.75,

    # Estaciones de lluvia en la parte ALTA de la cuenca (código de Wunderground).
    # Ejemplo: list(id = "IXXXXXX", nombre = "San Andrés Semetabaj")
    lluvia = list(
    )
  ),

  # ---- Camotán: misma plataforma SIGIA. Activar cuando se confirmen los datos. ----
  # En camotan.html: clic derecho → "Ver código fuente" → buscar "fetch(" para ver
  # los nombres de los archivos .php, y "yMin" para ver los umbrales de la gráfica.
  list(
    id       = "camotan",
    activa   = FALSE,
    nombre   = "Camotán",
    rio      = "Río en Camotán",          # VERIFICAR nombre del río
    datos    = "datos_camotan.php",       # VERIFICAR
    fotos    = "get_fotos_camotan.php",   # VERIFICAR
    pagina   = "https://www.sigiagt.com/camotan.html",
    logos    = "LOGOS 2.png",
    hashtags = "#Camotán #SIGIA #NivelDelRío",
    umbral_preventivo = NA,               # VERIFICAR
    umbral_alerta     = NA,               # VERIFICAR
    umbral_peligro    = NA,               # VERIFICAR
    lluvia = list()
  )
)
