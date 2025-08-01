---
title: '¿Suicidios?'
date: 2024-09-01
permalink: /posts/2024/09/suicidios/
tags:
 - The Pax Narca Files
---

<div style="text-align: justify;" >

En las últimas semanas, mi investigación sobre los efectos del cierre de la Base de Manta a finales de 2009 ha despertado un intenso debate en la sociedad ecuatoriana.<br>

<br>Entre las diversas reacciones, ha emergido una pregunta válida: ¿Será posible que las muertes violentas catalogados como "de intención no determinada" se traten simplemente de suicidios?<br>

<br>(<strong>Recordatorio:</strong> La Clasificación Internacional de Enfermedades <a href="https://ais.paho.org/classifications/Chapters/">define</a> a las muertes violentas "de intención no determinada" como aquellos eventos donde "la información disponible es insuficiente para que la autoridad médica o legal pueda distinguir entre accidente, lesión autoinfligida [suicidio] y agresión [homicidio].")<br>

<br>Este artículo aborda brevemente esta pregunta. En concreto, examino lo siguiente: Tras el cierre de la Base de Manta a finales del 2009, ¿presentan las tasas de suicidio patrones geográficos análogos a los observados en las muertes violentas "de intención no determinada" para el año 2010? ¿Podemos identificar también incrementos en las tasas de suicidio que agrupen provincias estratégicas en las rutas de la cocaína?<br>

<br>Los mapas presentados a continuación grafican incrementos en las variaciones anuales de las tasas en cuestión. Específicamente, se destacan las variaciones superiores al 25% — elección que permite visualizar de manera clara las provincias que experimentaron aumentos significativos.<br>

<br>En cuanto a las muertes violentas "de intención no determinada", como he detallado en mi publicación "El antropófago", disponible en este link, el siguiente gráfico ilustra los incrementos porcentuales tras la salida de la Base de Manta.<br>
</div>

![Figure 2](/images/2010ff.png)

<div style="text-align: justify;" >

<br>Realizando exactamente el mismo ejercicio pero con las cifras de suicidios, se obtiene el siguiente gráfico:<br>
</div>


![Figure 4](/images/suicidios10.jpg)

<div style="text-align: justify;" >
<br>Un rápido examen visual indica que, tras el cierre de la Base de Manta a finales de 2009, la concentración geográfica en provincias clave para el tráfico de cocaína se manifiesta únicamente en las muertes catalogadas como "de intención no determinada" (específicamente, agrupando provincias como las fronterizas con Colombia). Este fenómeno, sin embargo, no se observa para el caso de suicidios.<br>

<br>Este breve análisis sugiere que la (válida) hipótesis de que las muertes violentas "de intención no determinada" pudieran corresponder simplemente a suicidios no encuentra respaldo en los patrones geográficos observados.<br>

<br>La concentración de las muertes violentas "de intención no determinada" en provincias clave para el tráfico de cocaína, apunta a una relación con actividades del narcotráfico más que con un simple aumento en la tasa de suicidios. Esta evidencia  extra refuerza la necesidad de seguir indagando sobre el impacto que el cierre de la Base de Manta tuvo en las dinámicas del crimen organizado en Ecuador.<br>
</div>

<br>
<br>

<hr>
<hr>
<br>

<div style="text-align: center;" >
<strong>Replicación</strong>

</div>
<br>

<div style="text-align: justify;" >

<br>Para garantizar la transparencia y reproducibilidad de este análisis, los datos utilizados son de acceso público y pueden ser verificados independientemente. A continuación, detallo el proceso de obtención y procesamiento de los datos:<br>


<strong>1. Fuente de datos. </strong> Los datos de los años 2009 y 2010 se obtuvieron directamente del Instituto Nacional de Estadística y Censos (INEC) de Ecuador. Estos están disponibles en el siguiente enlace: https://anda.inec.gob.ec/anda/index.php/catalog/SOCDEMO. Los invito a descargar estos datos para verificar por su cuenta la información utilizada y los resultados presentados.<br> 

<br><strong>2. Variables de estudio.</strong> "Var" representa la variación anual en las tasas de muertes de intención no determinada; y, "VarS", la variación anual en las tasas de suicidios. Ambas variables deben ser a nivel provincial, y cuantifican la variación porcentual del logaritmo natural de las tasas entre 2009 y 2010.<br>

<br><strong>3. Metodología de cálculo.</strong> Para mejorar la comparabilidad entre provincias y evitar problemas con valores cero, apliqué la siguiente transformación: log(2 + Tasa respectiva). Esta transformación se realizó tanto para las tasas de 2009 como para las de 2010. Luego, calculé la variación porcentual entre estos dos valores transformados.<br>

<br><strong>4. Mapeo geográfico.</strong> Para la representación geográfica, utilicé el archivo Shapefile de Ecuador. El archivo específico usado fue "gadm41_ECU_1.shp", que puede ser descargado desde: https://gadm.org/download_country.html.<br>

<br><strong>5. </strong> Correr el siguiente código en R:<br>

</div>

<br>

<div style="text-align: justify;" >

<div style="display: flex; justify-content: center;">

<pre style="background-color: #f4f4f4; color: #333; padding: 20px; border: 2px solid #000; box-shadow: 0 4px 6px rgba(0,0,0,0.1); font-family: 'Courier New', Courier, monospace; line-height: 1.5; overflow-x: auto; font-size: 12px;">
    <code>
    # Cargar las bibliotecas necesarias
library(tidyverse)
library(sf)

# Cargar los datos 
data <- read_csv('/Datos.csv')
provinces <- st_read('/gadm41_ECU_1.shp')

# Filtrar los datos para el año 2010 y combinar con provincias
final_data <- st_as_sf(provinces) %>% 
  left_join(data %>% filter(Year == 2010), by = "CC_1")

# Función para crear mapas
crear_mapa <- function(data, var, titulo, subtitulo, filename) {
  plot <- ggplot(data) +
    geom_sf(aes_string(fill = var), color = "black") +
    scale_fill_gradient(
      low = "green", 
      high = "black", 
      limits = c(25, 50), 
      breaks = c(25, 50), 
      labels = c("25%", "50%"),
      na.value = "black", 
      oob = scales::squish
    ) +
    geom_sf_text(aes(label = Provincia), size = 2.7, color = "black", check_overlap = TRUE) +
    coord_sf(xlim = c(-81.5, -75)) +
    labs(
      title = titulo, 
      subtitle = subtitulo,
      fill = " "
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 13),
      legend.position = "bottom",
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(5, 0, 5, 0)
    )
  
  # Mostrar el gráfico
  print(plot)
  
  # Generar el gráfico en buena calidad
  ggsave(filename, plot, width = 10, height = 8, dpi = 300)
}

# Generar los mapas
crear_mapa(final_data, "Var", "Muertes Violentas de Intención no Determinada", "Aumentos en variación porcentual anual. Año 2010", "Muertes_Violentas_2010.png")

crear_mapa(final_data, "VarS", "Tasa de Suicidios", "Aumentos en variación porcentual anual. Año 2010", "Suicidios_2010.png")
    </code>
</pre>
</div>
</div>
