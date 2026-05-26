# Monitor-IPC-Bolivia
El script de R (versión 4.3.3) genera un archivo HTML interactivo para el Índice de Precios al Consumidor (IPC) de Bolivia y sus 12 divisiones de la Clasificación de Consumo Individual por Finalidades (CCIF).

Las librerias requeridas son:
- readxl 1.4.3
- dplyr 1.1.4
- stringr 1.5.1
- jsonlite 1.8.8

## Descripción
Este dashboard interactivo en HTML permite calcular y visualizar indicadores del Índice de Precios al Consumidor (IPC) utilizando información de 397 productos de la canasta del IPC 2016 y sus ponderaciones correspondientes, para el periodo comprendido entre enero de 2018 y abril de 2026.

La aplicación:
- Normaliza los índices de los productos seleccionados.
- Calcula indicadores agregados para cada una de las 12 divisiones del IPC.
- Permite combinar una o más divisiones para construir agregaciones personalizadas.
- Calcula automáticamente:
  - IPC
  - Inflación mensual
  - Inflación acumulada
  - Inflación a 12 meses
- La selección conjunta de las 12 divisiones reproduce el IPC general.

El dashboard reproduce las ponderaciones oficiales de las 12 divisiones del IPC publicadas por el INE, permitiendo calcular agregaciones personalizadas y reconstruir el IPC general mediante la selección conjunta de todas las divisiones.
