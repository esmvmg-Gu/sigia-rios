/**
 * SIGIA · Bot de consulta de nivel de ríos (Google Apps Script)
 *
 * Responde en Telegram a:
 *   /start        bienvenida y botones
 *   /nivel        nivel actual de todas las estaciones
 *   /panajachel   nivel de una estación (el id de cada estación)
 *   /estaciones   lista de estaciones
 *   /ayuda        ayuda
 *
 * Instalación: ver apps_script/LEEME.md
 * IMPORTANTE: mantenga los umbrales iguales a los de config.R
 */

const ZONA = 'America/Guatemala';
const BASE = 'https://www.sigiagt.com/';
const MENU = 'https://www.sigiagt.com/index.html';
const HORAS_SIN_DATOS = 2;

const ESTACIONES = {
  panajachel: {
    nombre: 'Panajachel', rio: 'Río Panajachel',
    datos: 'datos_pana.php', pagina: 'https://www.sigiagt.com/panajachel.html',
    preventivo: 2.00, alerta: 2.50, peligro: 4.75,
  },
  // camotan: { nombre: 'Camotán', rio: 'Río en Camotán', datos: 'datos_camotan.php',
  //            pagina: 'https://www.sigiagt.com/camotan.html', preventivo: 0, alerta: 0, peligro: 0 },
};

const NIVELES = [
  ['🟢', 'NORMAL'], ['🟡', 'PREVENTIVO'], ['🟠', 'ALERTA'], ['🔴', 'PELIGRO'],
];

// ------------------------------------------------------------------
function doPost(e) {
  const lock = LockService.getScriptLock();
  if (!lock.tryLock(10000)) return ok_();
  try {
    const upd = JSON.parse(e.postData.contents);
    const props = PropertiesService.getScriptProperties();
    // Telegram puede reenviar el mismo mensaje: se ignoran los repetidos
    const ultimo = Number(props.getProperty('ULTIMO_UPDATE') || 0);
    if (upd.update_id <= ultimo) return ok_();
    props.setProperty('ULTIMO_UPDATE', String(upd.update_id));

    const msg = upd.message || upd.edited_message;
    if (!msg || !msg.text) return ok_();
    responder_(msg.chat.id, msg.text.trim());
  } catch (err) {
    console.error(err);
  } finally {
    lock.releaseLock();
  }
  return ok_();
}

function responder_(chat, texto) {
  const partes = texto.toLowerCase().split(/\s+/);
  const cmd = partes[0].replace(/^\//, '').split('@')[0];
  const arg = (partes[1] || '').replace(/^nivel_/, '');

  if (cmd === 'start' && arg && ESTACIONES[arg]) return enviar_(chat, nivelEstacion_(arg), botonesEstacion_(arg));
  if (cmd === 'start' && partes[1] === 'nivel') return enviar_(chat, nivelTodas_(), botonesMenu_());
  if (cmd === 'start') return enviar_(chat, bienvenida_(), botonesMenu_());
  if (cmd === 'nivel' || cmd === 'rio' || cmd === 'río') {
    if (arg && ESTACIONES[arg]) return enviar_(chat, nivelEstacion_(arg), botonesEstacion_(arg));
    return enviar_(chat, nivelTodas_(), botonesMenu_());
  }
  if (ESTACIONES[cmd]) return enviar_(chat, nivelEstacion_(cmd), botonesEstacion_(cmd));
  if (cmd === 'estaciones') return enviar_(chat, listaEstaciones_(), botonesMenu_());
  return enviar_(chat, ayuda_(), botonesMenu_());
}

// ------------------------------------------------------------------
function leerDatos_(est) {
  const r = UrlFetchApp.fetch(BASE + est.datos + '?results=40', { muteHttpExceptions: true });
  if (r.getResponseCode() !== 200) return null;
  const d = JSON.parse(r.getContentText());
  const f = (d.feeds || [])
    .map(x => ({ t: new Date(x.created_at), a: parseFloat(x.field1) }))
    .filter(x => !isNaN(x.a) && !isNaN(x.t.getTime()))
    .sort((x, y) => x.t - y.t);
  return f.length ? f : null;
}

function nivel_(a, est) {
  if (a >= est.peligro) return 3;
  if (a >= est.alerta) return 2;
  if (a >= est.preventivo) return 1;
  return 0;
}

function hace_(t) {
  const m = Math.round((Date.now() - t.getTime()) / 60000);
  if (m < 1) return 'ahora';
  if (m < 60) return `hace ${m} min`;
  if (m < 1440) return `hace ${Math.floor(m / 60)} h ${m % 60} min`;
  return `hace ${Math.floor(m / 1440)} d ${Math.floor((m % 1440) / 60)} h`;
}

function nivelEstacion_(id, corto) {
  const est = ESTACIONES[id];
  let f;
  try { f = leerDatos_(est); } catch (e) { f = null; }
  const cab = `🌊 <b>${est.rio}</b>\n`;
  if (!f) return cab + '⚪ No fue posible consultar la estación en este momento.';

  const ult = f[f.length - 1];
  const horas = (Date.now() - ult.t.getTime()) / 3600000;
  const hora = Utilities.formatDate(ult.t, ZONA, 'dd/MM HH:mm');
  if (horas > HORAS_SIN_DATOS)
    return cab + `⚪ <b>Estación sin conexión</b>\nÚltima lectura: ${ult.a.toFixed(2)} m · ${hora} (${hace_(ult.t)})`;

  const n = nivel_(ult.a, est);
  // tendencia contra la lectura de hace ~1 hora
  const ref = f.filter(x => x.t <= new Date(ult.t.getTime() - 3600000)).pop() || f[0];
  const delta = ult.a - ref.a;
  const tend = Math.abs(delta) < 0.02 ? '▶ estable' : `${delta > 0 ? '▲' : '▼'} ${delta > 0 ? '+' : ''}${delta.toFixed(2)} m en 1 h`;
  const margen = ult.a < est.alerta ? `faltan ${(est.alerta - ult.a).toFixed(2)} m para alerta`
    : ult.a < est.peligro ? `faltan ${(est.peligro - ult.a).toFixed(2)} m para peligro`
    : `${(ult.a - est.peligro).toFixed(2)} m SOBRE peligro`;

  if (corto) return `${NIVELES[n][0]} <b>${est.nombre}:</b> ${ult.a.toFixed(2)} m · ${tend} (${hace_(ult.t)})`;
  return cab +
    `${NIVELES[n][0]} <b>${NIVELES[n][1]}</b>\n` +
    `📏 Altura: <b>${ult.a.toFixed(2)} m</b> (${hace_(ult.t)}, ${hora})\n` +
    `📈 Tendencia: ${tend}\n` +
    `🎯 ${margen}\n\n` +
    `<i>Umbrales: alerta ${est.alerta.toFixed(2)} m · peligro ${est.peligro.toFixed(2)} m</i>\n` +
    '<i>Dato automático de la estación. Los avisos oficiales los emite CONRED.</i>';
}

function nivelTodas_() {
  const ids = Object.keys(ESTACIONES);
  if (ids.length === 1) return nivelEstacion_(ids[0]);
  return '🌊 <b>SIGIA · Nivel de los ríos</b>\n\n' + ids.map(id => nivelEstacion_(id, true)).join('\n') +
    '\n\nPara ver el detalle escriba el nombre, por ejemplo /' + ids[0];
}

function bienvenida_() {
  return '👋 <b>Bienvenido al bot de SIGIA</b>\n\n' +
    'Consulte en cualquier momento el nivel de los ríos monitoreados.\n\n' +
    '• /nivel – nivel actual\n• /estaciones – estaciones disponibles\n• /ayuda – ayuda\n\n' +
    'Para recibir alertas automáticas únase al canal de SIGIA.\n\n' +
    '📞 Emergencias: CONRED 1566 · Bomberos 122 / 123 · PNC 110';
}
function ayuda_() {
  return 'ℹ️ <b>Comandos</b>\n\n/nivel – nivel actual de los ríos\n' +
    Object.keys(ESTACIONES).map(id => `/${id} – ${ESTACIONES[id].rio}`).join('\n') +
    '\n/estaciones – lista de estaciones';
}
function listaEstaciones_() {
  return '📍 <b>Estaciones</b>\n\n' + Object.keys(ESTACIONES)
    .map(id => `• <b>${ESTACIONES[id].nombre}</b> – ${ESTACIONES[id].rio}  /${id}`).join('\n');
}

function botonesMenu_() {
  return { inline_keyboard: [[{ text: '🗺️ Ver estaciones en vivo', url: MENU }]] };
}
function botonesEstacion_(id) {
  return { inline_keyboard: [[{ text: '📊 Ver en vivo', url: ESTACIONES[id].pagina },
                              { text: '🗺️ Otras estaciones', url: MENU }]] };
}

// ------------------------------------------------------------------
function enviar_(chat, html, teclado) {
  const token = PropertiesService.getScriptProperties().getProperty('TELEGRAM_TOKEN');
  UrlFetchApp.fetch(`https://api.telegram.org/bot${token}/sendMessage`, {
    method: 'post', contentType: 'application/json', muteHttpExceptions: true,
    payload: JSON.stringify({ chat_id: chat, text: html, parse_mode: 'HTML',
                              disable_web_page_preview: true, reply_markup: teclado }),
  });
}
function ok_() { return ContentService.createTextOutput('ok'); }

// ------------------------------------------------------------------
// Ejecutar UNA VEZ desde el editor después de publicar la app web
function configurarBot() {
  const props = PropertiesService.getScriptProperties();
  const token = props.getProperty('TELEGRAM_TOKEN');
  if (!token) throw new Error('Falta TELEGRAM_TOKEN en Propiedades del script');
  const url = ScriptApp.getService().getUrl();
  if (!url) throw new Error('Primero publique: Implementar → Nueva implementación → Aplicación web');
  const api = `https://api.telegram.org/bot${token}/`;
  const r1 = UrlFetchApp.fetch(api + 'setWebhook', { method: 'post', contentType: 'application/json',
    payload: JSON.stringify({ url: url, drop_pending_updates: true, allowed_updates: ['message', 'edited_message'] }) });
  const r2 = UrlFetchApp.fetch(api + 'setMyCommands', { method: 'post', contentType: 'application/json',
    payload: JSON.stringify({ commands: [
      { command: 'nivel', description: 'Nivel actual de los ríos' },
      ...Object.keys(ESTACIONES).map(id => ({ command: id, description: ESTACIONES[id].rio })),
      { command: 'estaciones', description: 'Estaciones disponibles' },
      { command: 'ayuda', description: 'Ayuda' },
    ] }) });
  console.log(r1.getContentText()); console.log(r2.getContentText());
}

// Prueba sin Telegram: Ejecutar → probar
function probar() { console.log(nivelTodas_()); }
