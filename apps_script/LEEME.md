# Bot de consulta /nivel (Google Apps Script)

Permite que cualquier persona le escriba al bot por privado y reciba el nivel actual del río al instante, a cualquier hora. Es gratis y no necesita servidor.

## Instalación (10 minutos, una sola vez)

1. Entre a https://script.google.com con la cuenta de Google de SIGIA y cree un **Proyecto nuevo**. Póngale de nombre *SIGIA Bot*.
2. Borre el contenido de `Código.gs` y pegue todo el archivo `bot_nivel.gs`.
3. **Guarde el token del bot:** ⚙️ *Configuración del proyecto* → *Propiedades del script* → *Agregar propiedad*.
   - Propiedad: `TELEGRAM_TOKEN`
   - Valor: el token del bot (el mismo que usa GitHub)
4. **Publique:** *Implementar* → *Nueva implementación* → tipo **Aplicación web**.
   - Ejecutar como: **Yo**
   - Quién tiene acceso: **Cualquier usuario**
   - Clic en *Implementar* y autorice los permisos que pide Google.
5. En el editor, elija la función **`configurarBot`** y presione **Ejecutar**. Esto conecta Telegram con el script y carga el menú de comandos.
6. Abra el bot en Telegram y escriba `/nivel`.

Para que el canal muestre el botón **🤖 Consultar nivel al bot**, ponga el usuario del bot (sin @) en `bot_usuario` dentro de `config.R`.

## Comandos

| Comando | Respuesta |
|---|---|
| `/nivel` | Nivel actual, estado, tendencia y margen |
| `/panajachel` | Detalle de esa estación |
| `/estaciones` | Lista de estaciones |
| `/ayuda` | Ayuda |

## Al cambiar algo

- **Umbrales o estaciones:** edite el bloque `ESTACIONES` al inicio de `bot_nivel.gs`, con los **mismos valores que `config.R`**.
- Después de editar: *Implementar* → *Gestionar implementaciones* → ✏️ → *Versión: nueva versión* → *Implementar*. No hace falta volver a ejecutar `configurarBot`.

## Probar sin Telegram

Elija la función `probar` y presione *Ejecutar*. El resultado aparece en el registro de ejecución.

## Nota

El bot y el monitor de GitHub usan el mismo token sin estorbarse: GitHub solo **envía** mensajes al canal y Apps Script solo **responde** mensajes privados.
