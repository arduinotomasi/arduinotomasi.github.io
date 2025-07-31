---
title: 'El antropófago: Un cortometraje estadístico'
date: 2024-08-20
permalink: /posts/2024/08/cortometraje/
tags:
 - The Pax Narca Files
---

<div style="text-align: justify;" >

En mi investigación sobre la posible manipulación de las cifras de homicidios durante el gobierno de Rafael Correa, me encontré con un hallazgo escandaloso: una marcada concentración de muertes violentas clasificadas como "de intención no determinada", tanto en el tiempo como en el espacio (geografía). Esta concentración parecía correlacionarse con caídas en las incautaciones de cocaína y se agrupaba en provincias vecinas.
El punto de partida de esta pieza de mi investigación fue una premisa simple pero clave: si estas muertes fueran verdaderamente accidentales o aleatorias, deberíamos esperar una distribución geográfica y temporal relativamente uniforme a lo largo del Ecuador. Después de todo, no estamos hablando de un fenómeno que se propaga como una enfermedad. Con esta idea en mente, comencé a examinar la distribución anual de estas muertes. Mi razonamiento: si encontraba una distribución más o menos uniforme, no podría atribuir directamente los resultados a eventos específicos, como la clausura de la Base de Manta en 2009.<br>

Sin embargo, al analizar las variaciones de estas muertes violentas, observé patrones que concentraban ciertas provincias juntas, sugiriendo una dinámica mucho más compleja que simples accidentes.
Recientemente, me percaté de que podía refinar aún más mi análisis aumentando la granularidad temporal. En lugar de examinar las concentraciones de manera anual, vi la oportunidad de analizarlas por meses e incluso por días de cada año. Esto permitiría descartar la presencia de provincias con problemas estructurales en el registro de muertes violentas.<br>

Mi razonamiento fue el siguiente: Si existieran provincias con problemas estructurales consistentes temporalmente, esperaríamos ver una concentración espacial más o menos constante a lo largo de los días del año. Sin embargo, si las concentraciones se movieran a lo largo del espacio geográfico conforme pasaran los días, y de manera concentrada o secuencial (por ejemplo, de Pichincha hacia las provincias por donde sale la cocaína, tal como Esmeraldas, Manabí o Guayas), esto sugeriría que la clasificación de muertes como "de intención no determinada" no obedece simplemente a problemas estructurales, sino que indicaría algo idiosincrático, posiblemente relacionado con el movimiento del crimen organizado a lo largo de rutas logísticas para el tráfico de cocaína.<br>

En otras palabras, las muertes violentas "de intención no determinada" podrían ser como un rastro sombrío dejado por el tránsito de la droga, revelando patrones de movimiento que de otra manera permanecerían ocultos. La secuencia geográfica de estos eventos, partiendo de centros urbanos como Pichincha y extendiéndose hacia provincias costeras como Esmeraldas, Manabí o Guayas, podría estar trazando, con tinta invisible, el movimiento del narcotráfico y del crimen organizado a lo largo de Ecuador.<br>

Para explorar esta hipótesis, decidí enfocarme en los dos años con máximos históricos entre 2007 y 2018; a saber, 2013 y 2014. Creé un vídeo de mapas de calor diarios para visualizar los patrones, utilizando un código de colores sencillo: verde para indicar la ausencia de muertes violentas "de intención no determinada" en una provincia en un día determinado, y negro para señalar al menos una muerte con esta clasificación. Esta representación visual no solo permitiría identificar concentraciones geográficas, sino también seguir su movimiento a través del tiempo, potencialmente revelando el movimiento del narco.<br>

Para facilitar la reproducibilidad y transparencia de mi investigación, he incluido al final de este artículo los códigos en R utilizados tanto para extraer la información de la microdata como para generar los mapas de calor.<br>

A continuación, la evidencia estadística se despliega en una secuencia casi cinematográfica. Los números, cual testigos, parecen conspirar para contar su propia historia. Dejo al lector la tarea de descifrar este relato tan extraño como doloroso. Para una experiencia más inmersiva de descubrimiento (si alguno), recomiendo el uso de audífonos.

</div>

<br>

<div style="text-align: center;">
<div style="background-color: #dddddd; padding: 8px;">
<strong>
El antropófago: Un cortometraje estadístico</strong>
</div>
<div style="text-align: center;">
<video width="100%" controls>
  <source src="https://arduinotomasi.github.io/assets/videos/MVIND.mp4" type="video/mp4">
</video>
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

<strong>1.</strong> Descargar los microdatos en este link: https://anda.inec.gob.ec/anda/index.php/catalog de tipo .sav, y ponerlos en una sola carpeta con el nombre "EDG_2013" y "EDG_2014".<br> 

<br><strong>2.</strong> Descargar el Shapefile para realizar el mapeo, con el archivo pertinente denominado "gadm41_ECU_1.shp", en este link: https://gadm.org/download_country.html.<br>

<br><strong>3.</strong> Correr el siguiente código en R:

</div>

<br>

<div style="text-align: justify;" >

<div style="display: flex; justify-content: center;">

<pre style="background-color: #f4f4f4; color: #333; padding: 20px; border: 2px solid #000; box-shadow: 0 4px 6px rgba(0,0,0,0.1); font-family: 'Courier New', Courier, monospace; line-height: 1.5; overflow-x: auto; font-size: 12px;">
    <code>
    #Primer paso: 

    library(dplyr)
    library(haven)
    library(purrr)
    library(readr)

    data_dir <- "C:/Users/"

    years <- 2013 # Luego 2014
    spss_files <- paste0(data_dir, "EDG_", years, ".sav")

    process_file <- function(file) {
    data <- read_sav(file)
    year <- as.numeric(gsub(".*_(\\d{4})\\.sav", "\\1", file))
    
    data <- data %>% mutate(across(everything(), as.character))
    
    month_col <- names(data)[tolower(names(data)) %in% c("mesf", "mes_fall", "MESF")]
    
    prov_col <- names(data)[tolower(names(data)) %in% c("prov_fall")]
    
    day_col <- names(data)[tolower(names(data)) %in% c("dia_fall")]
    
    if (length(month_col) == 1 && length(prov_col) == 1 && length(day_col) == 1) {
        data <- data %>%
        mutate(!!sym(month_col) := as.numeric(!!sym(month_col)),
                !!sym(prov_col) := as.numeric(!!sym(prov_col)),
                !!sym(day_col) := as.numeric(!!sym(day_col)))
        
        y_codes <- paste0("Y", 10:34)
        
        data <- data %>%
        mutate(is_Y_code = ifelse(if_any(everything(), ~ . %in% y_codes), 1, 0))
        
        counts <- data %>%
        group_by(year, month = !!sym(month_col), day = !!sym(day_col), province = !!sym(prov_col)) %>%
        summarise(total_count = sum(is_Y_code), .groups = 'drop')
        
        return(counts)
    } else {
        warning("No se encontró: ", file)
        return(NULL)  
    }
    }

    results <- spss_files %>%
    map(process_file) %>%
    compact() %>%  
    bind_rows()    


    write_csv(results, "C:/Users/PRIMERPASO.csv")

    # Segundo Paso

    library(ggplot2)
    library(tidyverse) 
    library(sf)
    library(readr)

    data <- read_csv("C:/Users/PRIMERPASO.csv")
    provinces <- st_read('C:/Users/gadm41_ECU_1.shp')

    provinces <- provinces %>% mutate(CC_1 = as.numeric(CC_1))

    data <- data %>% filter(year %in% c(2013, 2014))

    all_combinations <- expand_grid(
    province = unique(provinces$CC_1),
    year = c(2013, 2014),
    month = 1:12,
    day = 1:31
    )

    combined_data <- all_combinations %>%
    left_join(data, by = c("province", "year", "month", "day"))

    combined_data <- combined_data %>%
    mutate(total_count = ifelse(is.na(total_count), 0, total_count))

    final_data <- provinces %>%
    select(CC_1, NAME_1, geometry) %>%
    right_join(combined_data, by = c("CC_1" = "province"))

    final_data <- st_as_sf(final_data)

    month_name <- function(month_num) {
    month.abb[month_num]
    }

    days_2013_2014 <- unique(final_data %>% select(year, month, day))

    for (i in 1:nrow(days_2013_2014)) {
    day <- days_2013_2014$day[i]
    month <- days_2013_2014$month[i]
    year <- days_2013_2014$year[i]
    
    data_for_day <- final_data %>% filter(year == !!year & month == !!month & day == !!day)
    
    p <- ggplot(data_for_day) +
        geom_sf(aes(fill = total_count, geometry = geometry), color = "black") + 
        scale_fill_gradient(
        low = "green", 
        high = "black", 
        limits = c(0, 1),  # Ajustar la escala de color
        breaks = c(0, 1),  # Mostrar solo 0 y 1 en la leyenda
        labels = c("0", "1"),  # Etiquetas sin decimales
        na.value = "grey", 
        oob = scales::squish 
        ) +
        geom_sf_text(aes(label = NAME_1, geometry = geometry), size = 2.7, color = "black", check_overlap = TRUE) + 
        coord_sf(xlim = c(-81.5, -75), ylim = NULL) + 
        labs( 
        title = "Muertes violentas de intención no determinada",
        subtitle = paste("Fecha:", day, month_name(month), year),
        caption = "Tomasi, Arduino (Agosto 21, 2024). Microdata: Instituto Nacional de Estadísticas y Censos (INEC)."
        ) +
        theme_minimal() + 
        theme(
        plot.caption = element_text(hjust = 0.5, size = 10, margin = margin(t = 5, b = 0), face = "plain"),
        plot.title = element_text(hjust = 0.5, size = 15, margin = margin(t = 0, b = 8), face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(t = 0, b = -8), face = "plain"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.margin = margin(t = -15, b = 10),
        legend.position = "bottom",
        legend.title = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white"),
        plot.margin = margin(t = 5, r = -5, b = 5, l = -5, unit = "mm")
        )
        file_name <- paste0("C:/Users/ardui/Dropbox/00 narc/Python/mapas_", year, "_", sprintf("%02d", month), "_", sprintf("%02d", day), ".png")
    ggsave(file_name, p, width = 10, height = 8, dpi = 200)
    }



    </code>
</pre>
</div>
</div>
