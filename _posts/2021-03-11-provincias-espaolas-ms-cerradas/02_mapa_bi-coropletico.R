library(tidyverse)
library(sf)

#- AQUI. este post hace mas cosas con el bicoropletico: `https://ctompkins.netlify.app/post/bivariate_transit_map/`

#- para hacerlo bi-coroplético
#hacer una coropleta pero con dos escalas, como hace Timo Grossenbacher [aquí](https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/)
# Hay q usar el paquete bi-scale: <https://github.com/slu-openGIS/biscale>
# El tutorial del mapa de Suiza: <https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/>
# El de Dominic Roye: <https://dominicroye.github.io/en/2021/bivariate-dasymetric-map/>
#El de katalinnsky: <https://gist.github.com/ikashnitsky/90483ee3c3c230aa874dffb856d6074b> <https://twitter.com/ikashnitsky/status/1347325289496502272/photo/1>
# Otro de Katalinsky: <https://twitter.com/ikashnitsky/status/1347325289496502272>
#- Ademas: el de tricolore: @ikashnitsky: Educational composition of population in Spanish municipalities 🤩 https://twitter.com/dr_xeo/status/1369345011867918336


padron_tt_5 <- readr::read_csv("/home/pjpv/Escritorio/my_datos_2021/datos/INE/ine_padron_continuo/tablas_muni/tabla_5_relacion_muni.csv")

#- me concentro solo en 2020  y en la poblacion Total
df <- padron_tt_5 %>% filter(year == 2020) %>% filter(sexo == "Total")  
rm(padron_tt_5)
zz <- pjpv2020.01::pjp_f_valores_unicos(df, nn_pjp = 70)

#- selecciono una relación con el municipio: y me quedo con las PROVINCIAS
my_relacion_2 <-c("Mismo municipio", "Extranjero")   #-Mismo municipio"
df_x <- df %>% filter(rel_nacimiento %in% my_relacion_2) %>%  #- salen más municipios de los q tocan Oza Cesuras y más (YA NO)
  select(ine_muni, ine_muni.n, ine_prov, ine_prov.n, rel_nacimiento, values_percent, capital_prov) %>% distinct() %>% 
  pivot_wider(names_from = rel_nacimiento, values_from = values_percent) %>% 
  rename(mismo_muni = `Mismo municipio`)
rm(df)
#- lo primero es crear escalas bivariantes
#- discretizo de forma bivariate
pointData <- biscale::bi_class(df_x, x = mismo_muni, y = Extranjero, style = "quantile", dim = 3)

#- geo
geo_muni <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_muni_2020_LAU2_canarias.rds")
geo_prov <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_prov_2020_LAU2_canarias.rds")

# Morocco geometría
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") 
zz <- world %>% st_set_geometry(NULL)
geo_morocco <- world %>% dplyr::filter(sovereignt %in% c("Morocco")) %>% select(sovereignt)
rm(world)

#- decido q geometrías uso ----
pob_muni <- pointData  #- es para no tener que cambiar los nombres en el mapa
my_geo <- geo_muni #- AQUI-AQUI
my_geo <- inner_join(my_geo, pob_muni)  #- AQUI-AQUI
my_canarias <- my_geo %>% 
  filter(ine_ccaa == "05") #- solo para el cuadrito, no para las geo
ceuta <- my_geo %>% filter(ine_ccaa %in% c(18,19))
capitales <- pob_muni %>% 
  filter(capital_prov == "Sí") %>% 
  left_join(. , st_drop_geometry(geo_muni)) %>% 
  mutate(capital_prov.2 = ifelse(capital_prov == "Sí", "Capital provincial", "NO"))



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




# create map
library(biscale)

map <- ggplot() +
  geom_sf(data = geo_morocco, aes(geometry = geometry)) +
  geom_sf(data = my_geo, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  geom_point(data = capitales, aes(x = X, y = Y, color = capital_prov.2)) +
  geom_sf(data = geo_prov, aes(geometry = geometry), fill = NA, size = 0.3) + 
  coord_sf(xlim = c(st_bbox(my_geo)[1]-0.2, st_bbox(my_geo)[3]+0.3), 
           ylim = c(st_bbox(my_geo)[2]-0.1, st_bbox(my_geo)[4]+0.3), expand = FALSE) + 
  geom_rect(aes(xmin = st_bbox(my_canarias)[1]-2.5, xmax = st_bbox(my_canarias)[3]+0.1, 
                ymin = st_bbox(my_canarias)[2]-2.5, ymax = st_bbox(my_canarias)[4]+0.1), 
            fill = NA, colour = "black", size = 0.3, show.legend = FALSE, linejoin= "round", linetype = 2) +
  scale_color_manual(name = NULL,  values = c("black")) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(x = "", y = "")  +  
  my_theme_maps +
  bi_theme() +
  theme(legend.position = "none")   #- para quitar la legend de capital de provincia
  


#- la leyenda
legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Mayor % mismo muni",
                    ylab = "Mayor % extranjeros",
                    size = 8)


#- leyenda (de https://ctompkins.netlify.app/post/bivariate_transit_map/)
bi_var_legend <- bi_legend(pal = "DkBlue",
                           dim = 3,
                           xlab = " More drove alone",
                           ylab = "More used public transit",
                           size = 26) +
  theme(plot.background = element_rect(fill = alpha("white", 0)),
        panel.background = element_rect(fill = alpha("white", 0)))

built_legend <- ggplot_build(bi_var_legend) #- takes the plot object, and performs all steps necessary to produce an object that can be rendered

legend_palette <- built_legend$data[[1]] %>%
  mutate(bi_class = str_c(x, y, sep = "-")) %>% 
  select(fill, bi_class)

legend_palette %>%  kableExtra::kbl()



#- juntar los trozitos del plot -------------
#- este tio: https://ctompkins.netlify.app/post/bivariate_transit_map/
#- junta los trozitos con patchwork



library(cowplot)

header <- cowplot::ggdraw() +
  draw_text("Municipios españoles según porcentaje de extranjeros y nacidos en el municipio (2020)", x= 0, y = 0.8, hjust=0, size = 20, family = "Verdana") 
 

footer <- cowplot::ggdraw() +
  draw_text("Datos de población del INE. Geometrías del paquete LAU2boundaries4spain. Visualización: @pjpv4444", x= 1, y = 0, hjust=1, size = 10, family = "Verdana Pro Light") 


p <- cowplot::ggdraw() +
  draw_plot(map, 0, 0.0, 1, 1) +
  #draw_plot(legend, 0.6, .1, 0.25, 0.25) +
  draw_plot(legend, 0.7, .1, 0.25, 0.25) +
  draw_text(" Datos de población del INE. Geometrías del paquete LAU2boundaries4spain. Visualización: @pjpv4444 ", x= 0.95, y = 0.05, hjust=1, size = 8, family = "Verdana Pro Light") +
  draw_text("Municipios españoles según porcentaje de extranjeros y nacidos en el municipio (2020)", x= 0.05, y = 0.95, hjust=0, size = 17, family = "Verdana") 



#- para guardar el plot
ggsave(p, filename = here::here("plots", "mapa_bi_cloropletico_3.png"),
       device = "png", width = 29, height = 20, units = "cm")
knitr::plot_crop(here::here("plots", "mapa_bi_cloropletico_3.png"))

#- deberia tb poerse hacer la composicion asin, de hecho un poco más arriba tengo un link que lo hace

#- aqui haces la composicion del plot final
library(patchwork)
pp <- header / p / footer + plot_layout(height = c(0.5, 2, 0.5))


design = c(area(t = 2, l = 4, b = 20, r = 20),
           area(t = 1, l = 1, b = 6, r = 6))

plot(design)