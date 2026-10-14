# Notas de voz para las alertas

Cuando se envía una alerta, el sistema busca aquí grabaciones con este nombre y las publica en el canal justo después de la tarjeta:

```
<estación>_<tipo>_<idioma>.ogg
```

| Parte | Valores |
|---|---|
| estación | `panajachel` (o el `id` de otra estación en `config.R`) |
| tipo | `subida`, `preventivo`, `alerta`, `peligro`, `lluvia`, `normal` |
| idioma | `es` (español), `kaq` (kaqchikel), `tz` (tz'utujil) |

Ejemplos:

```
panajachel_alerta_es.ogg
panajachel_alerta_kaq.ogg
panajachel_peligro_kaq.ogg
panajachel_lluvia_es.ogg
```

## Recomendaciones

- **Mensajes generales, sin números.** La grabación se usa en cualquier alerta de ese tipo, así que no debe decir la altura. Por ejemplo: *"Atención: el río Panajachel está en nivel de alerta. No se acerque ni cruce el río. Esté atento a nuevos avisos."*
- **Duración:** 10 a 25 segundos, voz clara y tranquila.
- **Quién graba:** idealmente una voz conocida en la comunidad (COMRED, radio local, autoridad indígena).
- **Formato:** `.ogg` se publica como nota de voz de Telegram (lo ideal). También se aceptan `.mp3` y `.m4a`, que aparecen como archivo de audio.
- Para grabar en `.ogg` puede usar la grabadora de WhatsApp o Telegram y reenviarse el audio, o convertir un `.mp3` en https://cloudconvert.com/mp3-to-ogg

Si no hay grabación para un tipo de alerta, simplemente no se envía audio.
