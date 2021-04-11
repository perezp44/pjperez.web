#- quiero hacer un mapa con el crecimiento de la población (por provincias) en el periodo 2000-2020
library(tidyverse)
library(sf)

# datos geo originales de LAU2 (pero con Canarias shifted) ----
geo_prov <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_prov_2020_LAU2_canarias.rds")

# Morocco geometría
geo_morocco <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  dplyr::filter(sovereignt %in% c("Morocco")) %>% select(sovereignt)

#- datos de población
pob_2000_2020 <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/INE/pob_mun/ine_pob_mun_1996_2020.rds")

#- rtdos (crec. poblacion en España)
pob_esp <- pob_2000_2020 %>% 
  filter(year %in% c(2000, 2020)) %>% 
  select(year, poblacion, values) %>% 
  group_by(year, poblacion) %>% 
  summarise(habitantes = sum(values)) %>% 
  group_by(poblacion) %>% 
  mutate(crec_abs = habitantes - lag(habitantes)) %>% 
  mutate(crec_percent = crec_abs /lag(habitantes)) %>% ungroup()

#- resultados pob provincial
#my_breaks <- c(-Inf, 0.0, 10.0, pob_esp$crec_percent[6]*100, )

pob_prov <- pob_2000_2020 %>% 
  filter(year %in% c(2000, 2020)) %>% 
  filter(poblacion == "Total") %>% 
  group_by(ine_prov, ine_prov.n, year) %>% 
  mutate(habitantes = sum(values)) %>% #- hago mutate y no summarise xq quiero guardar CCAA
  select(ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n, year, habitantes) %>%
  distinct() %>% 
  pivot_wider(names_from = c("year"), values_from = c("habitantes")) %>% 
  mutate(crec_2020_2000 = `2020`- `2000`) %>% 
  mutate(crec_porcentual = (`2020`- `2000`) / `2000` *100) %>% 
  arrange(desc(crec_porcentual)) %>% ungroup() %>% 
  mutate(crec_porcentual.n = paste0(round(crec_porcentual, digits = 1), "%")) %>% 
  mutate(crec_porcentual_d = cut(crec_porcentual, 
                                 breaks=c(-Inf, 0.0, 10, pob_esp$crec_percent[6]*100, 30, Inf), 
                                 labels=c("Negativo","[ 0% - 10%)","[10% - media)", "[media, 30%)", " >30%")))
#- para discretizar:  cut_interval(x = crec_porcentual, n = 5))
#- para discretizar: https://www.rdocumentation.org/packages/arules/versions/1.6-6/topics/discretize


#- El theme -----------------
#+  theme_map() #- https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
#+ ggthemes::theme_map(base_size = 9, base_family = "")

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
  panel.border = element_blank() )


# ELECCION de q geometrias --------------------
#- generalmente fusionando con datos
my_geo <- geo_prov #- AQUI-AQUI
my_geo <- left_join(my_geo, pob_prov)  #- AQUI-AQUI
my_canarias <- my_geo %>% filter(ine_ccaa == "05")
names(my_geo)
# ELECCION (q v. grafico) ---------------------
my_vv <- expr(crec_porcentual_d)  #- AQUI-AQUI   (para la leyenda)
my_vv_2 <- expr(crec_porcentual.n)  #- AQUI-AQUI  para el label text

# el color cadetblue1: #8ee5ee
my_subtitle <- glue::glue("En 2000-2020, la población en España creció un 17,16%.
                            Por provincias, 13 presentaron un <span style='color:#f08080'>crecimiento negativo</span>,\n
                            20 tuvieron un <span style='color:#7ac5cd'>crecimiento inferior a la media</span> española y 
                            19 experimentaron un <span style='color:#9acd32'>crecimiento superior a la media</span>.")



#- Plot con labels
p <- ggplot() + 
  geom_sf(data = geo_morocco, aes(geometry = geometry)) +
  geom_sf(data = my_geo, aes(geometry = geometry, fill = !!my_vv)) + 
  # coord_sf(xlim = c(-12.2, 4.9), ylim = c(34.7, 44), expand = FALSE) + 
  coord_sf(xlim = c(st_bbox(my_geo)[1]-0.2, st_bbox(my_geo)[3]+0.3), 
           ylim = c(st_bbox(my_geo)[2]-0.1, st_bbox(my_geo)[4]+0.3), expand = FALSE) + 
  geom_rect(aes(xmin = st_bbox(my_canarias)[1]-2.5, xmax = st_bbox(my_canarias)[3]+0.1, 
                ymin = st_bbox(my_canarias)[2]-2.5, ymax = st_bbox(my_canarias)[4]+0.1), 
            fill = NA, colour = "black", size = 0.3, show.legend = FALSE, linejoin= "round", linetype = 2) +
  geom_text(data = my_geo, aes(x = X1, y = Y1, label = !!my_vv_2), #- v. continua
            color = "black",  
            check_overlap = TRUE, size = 3) + #- fontface = "bold"
  scale_fill_manual(name = NULL,  values = c("#f08080", "#98f5ff", "#8ee5ee", "#b3ee3a", "#9acd32")) +
  labs(title = "Crecimiento de la población por provincias: 2000-2020",
       subtitle = my_subtitle,
       x = "", 
       y = "",
       caption = "Datos de población del INE. Geometrías del paquete LAU2boundaries4spain. Visualización: @pjpv4444") +  
  my_theme_maps + 
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position = c(.9,.2),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = ggtext::element_markdown(size = 11, family = "Roboto Condensed", face = "bold", color = "dimgrey")) 

p 

janitor::tabyl(pob_prov, crec_porcentual_d)

my_textito <- glue::glue("Las provincias con crecimientos <B><span style='color:#9acd32'>superiores a la media</span></B> se concentran a lo largo de la costa mediterranea, Baleares, Canarias, área de influencia de Madrid, Navarra y la Rioja. Trece provincias presentan <B><span style='color:#f08080'>crecimiento negativo</span></B>, principalmente en las zonas interiores de Galicia (Lugo, Orense) y las provincias más occidentales de Castilla y León")

p <- p + ggtext::geom_textbox(
  aes(label = my_textito, x = -12.1, y = 39.7), 
  color = "black", size = 3.5, width = grid::unit(0.25, "npc"),
  # remove box padding, since we have removed the box outline
  #box.padding = grid::unit(rep(0.1, 4), "pt") 
  fill = "white", box.color = "black", family = "Tahoma", hjust = 0L)

p
#- para guardar el plot
ggsave(p, filename = here::here("imagenes", "map_crec_pob_provincial.png"),
       device = "png", width = 29, height = 20, units = "cm")
knitr::plot_crop(here::here("imagenes", "map_crec_pob_provincial.png"))

#- para mi esto lo guarda peor
ggsave(p, filename = here::here("imagenes", "map_crec_pob_provincial_2.png"), device = "png", type = "cairo", width = 24, height = 12, dpi = 300)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
