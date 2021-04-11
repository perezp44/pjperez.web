library(tidyverse)
library(sf)

#- datos padrón continuo ------------------------
#- tabla 5: relacion con el municipio 
padron_tt_5 <- readr::read_csv("/home/pjpv/Escritorio/my_datos_2021/datos/INE/ine_padron_continuo/tablas_muni/tabla_5_relacion_muni.csv")

# datos geo ---
geo_muni <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_muni_2020_LAU2_canarias.rds")
geo_prov <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_prov_2020_LAU2_canarias.rds")
geo_morocco <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  dplyr::filter(sovereignt %in% c("Morocco")) %>% select(sovereignt)

#- datos poblacion: ya no hacen falta, he fusionado
pob_2000_2020 <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/INE/pob_mun/ine_pob_mun_1996_2020.rds") %>% 
  filter(year == 2020) %>% 
  filter(poblacion == "Total") %>% 
  rename(values_pob_total = values) %>% 
  select(ine_muni, ine_muni.n, ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n, capital_prov, capital_ccaa, values_pob_total)

#- me concentro solo en 2020   y dejo solo rel_nacimiento =  Total
df <- padron_tt_5 %>% 
  filter(year == 2020) %>% 
  #filter(sexo != "Total") %>%  
  filter(rel_nacimiento == "Total") %>% 
  select(sexo, values,ine_muni, ine_muni.n, ine_prov.n, ine_prov,ine_ccaa, ine_ccaa.n, capital_prov, capital_ccaa) %>% 
  pivot_wider(names_from = sexo, values_from = values) %>% 
  mutate(mas_mujeres_percent = (Mujeres - Hombres)/Hombres*100 ) %>% 
  mutate(mas_mujeres = case_when(
     Mujeres - Hombres > 0L ~ "Más mujeres",
     Mujeres - Hombres == 0L ~ "Igual",
     Mujeres - Hombres < 0L ~ "Más hombres")) %>% 
  select(ine_muni, ine_muni.n, ine_prov.n, ine_prov, Mujeres, Hombres, Total, mas_mujeres_percent, mas_mujeres,ine_ccaa, ine_ccaa.n, capital_prov, capital_ccaa) 
  

janitor::tabyl(df, mas_mujeres )
#- en todas las capitales y en todos los munis de mas de 100.000 siempre hay mas mujeres
zz <- df %>% filter(capital_prov == "Sí")
zz <- df %>% filter(Total >= 50000)
zz <- df %>% filter(Total >= 50000)
zz <- df %>% filter(mas_mujeres == "Más hombres")
names(df)

#- theme -----------------
my_theme_maps <- theme_minimal() + theme(
  text = element_text(family = "Ubuntu Regular", color = "#22211d"),
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
  panel.grid.major = element_line(color = NA, size = 0.2), #- "#ebebe5"
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "#f5f5f2", color = NA), 
  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
  legend.background = element_rect(fill = "#f5f5f2", color = NA),
  panel.border = element_blank(),
  strip.text.x = element_blank(),
  strip.background = element_rect(colour="white", fill="white"),
  legend.position = c(.9,.2),
  plot.title = element_text(size = 16, face = "bold")  )


#- decido q geometrías uso ----
pob_muni <- df  #- es para no tener que cambiar los nombres en el mapa
my_geo <- geo_muni #- AQUI-AQUI
my_geo <- inner_join(my_geo, pob_muni)  #- AQUI-AQUI (aqui no fa falta xq ya le meti la poblacion)
my_canarias <- my_geo %>% 
  filter(ine_ccaa == "05") #- solo para el cuadrito, no para las geo
ceuta <- my_geo %>% filter(ine_ccaa %in% c(18,19))
capitales <- pob_muni %>% 
  filter(capital_prov == "Sí") %>% 
  left_join(. , st_drop_geometry(geo_muni)) %>% 
  mutate(capital_prov.2 = ifelse(capital_prov == "Sí", "Capital provincial", "NO"))

str(pob_muni)
#- elección de variables q grafíco ----
my_vv <- expr(mas_mujeres)      #- AQUI-AQUI   (para la leyenda)
my_vv_2 <- expr(mas_mujeres)      #- AQUI-AQUI  para el label text (es lo de Ceuta y melilla)

janitor::tabyl(pob_muni, !!my_vv)
my_subtitle <- glue::glue(
  "(En 2020, la población española era de 47,45 millones de personas, el 50,99% de ellas mujeres, de forma que, en 2020, había\n
   casi <B><span style='color:#8a2be2'>un millón más de mujeres</span></B> que de hombres.
  A pesar de ello, el 75,3% de los municipios españoles tienen <B><span style='color:#e0c56e'>más hombres</span></B> que mujeres)" )



names(my_geo)
#- Plot con variable discreta (factor ---------------------------)
p <- ggplot() + 
  geom_sf(data = geo_morocco, aes(geometry = geometry)) +
  geom_sf(data = my_geo, aes(geometry = geometry, fill = !!my_vv), color = "white", size = 0.09) + 
  scale_fill_manual(name = NULL,  values = c( "brown1",  "#e0c56e", "blueviolet")) +
  geom_point(data = capitales, aes(x = X, y = Y, color = capital_prov.2)) +
  geom_text(data = ceuta, aes(x = X1+0.5, y = Y1, label = !!my_vv_2), 
            color = "black",  
            check_overlap = TRUE, size = 3) + #- fontface = "bold"
  geom_sf(data = geo_prov, aes(geometry = geometry ), fill = NA) + 
  coord_sf(xlim = c(st_bbox(my_geo)[1]-0.2, st_bbox(my_geo)[3]+0.3), 
           ylim = c(st_bbox(my_geo)[2]-0.1, st_bbox(my_geo)[4]+0.3), expand = FALSE) + 
  geom_rect(aes(xmin = st_bbox(my_canarias)[1]-2.5, xmax = st_bbox(my_canarias)[3]+0.1, 
                ymin = st_bbox(my_canarias)[2]-2.5, ymax = st_bbox(my_canarias)[4]+0.1), 
            fill = NA, colour = "black", size = 0.3, show.legend = FALSE, linejoin= "round", linetype = 2) +
  scale_color_manual(name = NULL,  values = c("black")) +
labs(title = "Municipios con más mujeres que hombres en 2020",
       #subtitle = str_wrap(my_subtitle, 20) , x = "", y = "", #- no acaba de funcionar
       subtitle = my_subtitle , x = "", y = "",
       caption = "Datos de población del INE. Geometrías del paquete LAU2boundaries4spain. Visualización: @pjpv4444")  +  
  my_theme_maps + 
  theme(plot.subtitle = ggtext::element_markdown(size = 11, 
                                                 family = "Roboto Condensed", face = "bold", color = "dimgrey", lineheight = .8)) +
  theme(plot.margin = unit(c(0.2, 0.03, 0.03, 0.08), "cm")) 
  

p
my_textito <- glue::glue("La población femenina se concentra en las <B><span style='color:#8a2be2'>grandes ciudades</span></B>. Todas las capitales y municipios de más de 100.000 habitantes, excepto Ceuta y Melilla, tienen <B><span style='color:#8a2be2'>más mujeres</span></B>. Sorprende la abundancia de municipios con más mujeres en Galicia, costa de Asturias, y en menor medida Andalucia.")

p <- p + ggtext::geom_textbox(
  aes(label = my_textito, x = -12.1, y = 40.0), 
  color = "black", size = 3.5, width = grid::unit(0.25, "npc"),
  # remove box padding, since we have removed the box outline
  #box.padding = grid::unit(rep(0.1, 4), "pt") 
  fill = "white", box.color = "black", family = "Tahoma", hjust = 0L)

p

#- para guardar el plot
ggsave(p, filename = here::here("imagenes", "2021_03_04_mapa_mas_mujeres.png"),
       device = "png", width = 29, height = 20, units = "cm")
knitr::plot_crop(here::here("imagenes", "2021_03_04_mapa_mas_mujeres.png"))
