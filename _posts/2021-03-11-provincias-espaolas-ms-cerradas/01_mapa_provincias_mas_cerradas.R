library(tidyverse)
library(sf)

#- datos padrón continuo ------------------------
#- tabla 5: relacion con el municipio 
padron_tt_5 <- readr::read_csv("/home/pjpv/Escritorio/my_datos_2021/datos/INE/ine_padron_continuo/tablas_muni/tabla_5_relacion_muni.csv")

#- datos de 2020, pob total y misma prov
df_muni <- padron_tt_5 %>% 
  filter(year == "2020") %>% 
  filter(sexo == "Total") %>% 
  filter(rel_nacimiento == "Mismo municipio") %>% 
  #filter(values_total >= 50000) %>% 
  filter(capital_prov == "Sí") %>% 
  select(ine_muni, ine_muni.n, ine_prov.n, values, values_total, values_percent, values_prov_percent, values_ESP_percent, capital_prov)

df_prov <- padron_tt_5 %>% 
  filter(year == "2020") %>% 
  filter(sexo == "Total") %>% 
  filter(rel_nacimiento == "Misma provincia") %>% 
  #filter(values_total >= 50000) %>% 
  filter(capital_prov == "Sí") %>% 
  select(ine_muni, ine_muni.n, ine_prov.n, values, values_total, values_percent, values_prov_percent, values_ESP_percent, capital_prov)


# datos geo ---
geo_muni <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_muni_2020_LAU2_canarias.rds")
geo_prov <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_prov_2020_LAU2_canarias.rds")
geo_morocco <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  dplyr::filter(sovereignt %in% c("Morocco")) %>% select(sovereignt)


#- me concentro solo en 2020  y en la poblacion Total
df <- padron_tt_5 %>% filter(year == 2020) %>% filter(sexo == "Total")  

#- selecciono una relación con el municipio: y me quedo con las PROVINCIAS
my_relacion <-    "Misma provincia"   #-Mismo municipio"
df_x <- df %>% filter(rel_nacimiento == my_relacion) %>%  #- salen más municipios de los q tocan Oza Cesuras y más (YA NO)
  select(ine_prov, ine_prov.n, values_prov_percent, values_ESP_percent) %>% distinct()

#- creo valores para el label en el mapa
df_x <- df_x %>% mutate(values_prov_percent.l = paste0(round(values_prov_percent, digits = 1), "%"))



#- discretizo la variable "per-capita" --------------
#- 2. La forma de agrupar en intervalos: br <- classIntervals(zz$crec_habi_percent,5,"kmeans")$brks
#br <- classInt::classIntervals(zz$crec_habi_percent,5,"kmeans")
#br <- classInt::classIntervals(zz$crec_habi_percent,5,"kmeans")$brks
aa_media <- mean(df_x$values_ESP_percent)

df_x <- df_x %>% 
  #mutate(values_prov_percent.d = cut_interval(values_prov_percent, 10), .after= values_prov_percent.l) 
  #mutate(values_prov_percent.d = cut_interval(values_prov_percent, 10), .after= values_prov_percent.l) 
  mutate(values_prov_percent.d = cut(values_prov_percent, 
                                 breaks=c(-Inf, aa_media-10, aa_media-3, aa_media+3,  aa_media+10, Inf), 
                                 labels=c("10 puntos más abierta","3 puntos más abierta","En torno a la media", "3 puntos más cerrada", "10 puntos más cerrada")))

janitor::tabyl(df_x, values_prov_percent.d)


#- he querido usar esta paleta pero no me salido muy guay: https://github.com/sebdalgarno/tinter
hex <- "#8deeee"
scala_hex <- tinter::tinter(hex, steps = 11,  crop = 2, direction = "tints")   #-   adjust = 0.3

#scale_fill_gradientn(colours = tinter(hex)) +



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
pob_muni <- df_x  #- es para no tener que cambiar los nombres en el mapa
my_geo <- geo_prov #- AQUI-AQUI
my_geo <- inner_join(my_geo, pob_muni)  #- AQUI-AQUI
my_canarias <- my_geo %>% 
  filter(ine_ccaa == "05") #- solo para el cuadrito, no para las geo
ceuta <- my_geo %>% filter(ine_ccaa %in% c(18,19))


#- elección de variables q grafíco ----
my_vv <- expr(values_prov_percent.d)      #- AQUI-AQUI   (para la leyenda)
my_vv_2 <- expr(values_prov_percent.l)      #- AQUI-AQUI  para el label text (es lo de Ceuta y melilla)

janitor::tabyl(pob_muni, !!my_vv)
my_subtitle <- glue::glue(
  "(En 2020, el 67,2% de la población española residía en la misma provincia en la que nació)")


my_escala_manual <- c("#9acd32", "#b3ee3a","#98f5ff",  "darkorange", "darkorange1")
#- Plot con variable discreta (factor ---------------------------)
p <- ggplot() + 
  geom_sf(data = geo_morocco, aes(geometry = geometry)) +
  geom_sf(data = my_geo, aes(geometry = geometry, fill = !!my_vv), color = "white", size = 0.09) + 
  scale_fill_manual(name = NULL,  values = my_escala_manual) +
  #scale_fill_manual(name = NULL,  values = scala_hex) +
  #scale_fill_gradientn(colours = scala_hex) +
    geom_text(data = my_geo, aes(x = X1, y = Y1, label = !!my_vv_2), #- v. continua
            color = "black",  
            check_overlap = TRUE, size = 3) + #- fontface = "bold"
  #geom_point(data = capitales, aes(x = X, y = Y, color = capital_prov.2)) +
  geom_sf(data = geo_prov, aes(geometry = geometry ), fill = NA) + 
  coord_sf(xlim = c(st_bbox(my_geo)[1]-0.2, st_bbox(my_geo)[3]+0.3), 
           ylim = c(st_bbox(my_geo)[2]-0.1, st_bbox(my_geo)[4]+0.3), expand = FALSE) + 
  geom_rect(aes(xmin = st_bbox(my_canarias)[1]-2.5, xmax = st_bbox(my_canarias)[3]+0.1, 
                ymin = st_bbox(my_canarias)[2]-2.5, ymax = st_bbox(my_canarias)[4]+0.1), 
            fill = NA, colour = "black", size = 0.3, show.legend = FALSE, linejoin= "round", linetype = 2) +
  scale_color_manual(name = NULL,  values = c("black")) +
labs(title = "Provincias españolas más cerradas en 2020",
       #subtitle = str_wrap(my_subtitle, 20) , x = "", y = "", #- no acaba de funcionar
       subtitle = my_subtitle , x = "", y = "",
       caption = "Datos de población del INE. Geometrías del paquete LAU2boundaries4spain. Visualización: @pjpv4444")  +  
  my_theme_maps + 
  theme(plot.subtitle = ggtext::element_markdown(size = 11, 
                                                 family = "Roboto Condensed", face = "bold", color = "dimgrey")) 

p
my_textito <- glue::glue("El gráfico muestra las 12 provincias con valores <B><span style='color:#98f5ff'>en torno a la media</span></B> española. Las provincias <B><span style='color:#9acd32'>más abiertas</span></B> son las asociadas habitualmente a un mayor dinámismo económico como Cataluña y Madrid y alrededores. Hay 25 provincias con al menos 3 puntos porcentuales <B><span style='color:#ff7f00'>más cerradas</span></B> que la media española.")




p <- p + ggtext::geom_textbox(
  aes(label = my_textito, x = -12.1, y = 40.0), 
  color = "black", size = 3.5, width = grid::unit(0.25, "npc"),
  # remove box padding, since we have removed the box outline
  #box.padding = grid::unit(rep(0.1, 4), "pt") 
  fill = "white", box.color = "black", family = "Tahoma", hjust = 0L)

p

#- para guardar el plot
ggsave(p, filename = here::here("plots", "mapa_prov_mas_cerrada.png"),
       device = "png", width = 29, height = 20, units = "cm")
knitr::plot_crop(here::here("plots", "mapa_prov_mas_cerrada.png"))

