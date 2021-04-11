#- calcular los municipios que han cambiado de nombre y cuantas veces
library(tidyverse)
library(reactable)
library(gt)
# datos ----
pob_2000_2020 <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/INE/pob_mun/ine_pob_mun_1996_2020.rds")

#- filtro años y quito variables
#select(year, ine_muni, ine_muni.n, ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n, capital_prov) %>% 
df <- pob_2000_2020  %>% 
  filter(poblacion == "Total") %>% 
  select(year, ine_muni, ine_muni.n, ine_muni.n.orig, ine_prov, ine_prov.n) %>% 
  group_by(ine_muni) %>% arrange(year) %>% 
  mutate(cambio_nombre = ifelse(ine_muni.n.orig != lag(ine_muni.n.orig), 1, 0)) %>% 
  mutate(nn_cambios = sum(cambio_nombre, na.rm = TRUE)) %>% ungroup() %>% 
  filter(year == 2020)

#- tabla
janitor::tabyl(df, nn_cambios) %>% janitor::adorn_pct_formatting() %>% janitor::adorn_totals("row") %>% 
gt::gt() %>% tab_header(title = md("**Municipios con cambios de nombre durante 1996-2020**")) %>% tab_source_note(md("Datos provenientes de los ficheros de población municipal del INE")) %>% cols_label(nn_cambios = "Nº de cambios", n = "N", percent = "Porcentaje")

#- ver los 5 q cambiaron 3 veces de nombre
zz <- df %>% filter(nn_cambios == 3)
zz_pueblos <- zz$ine_muni
zz <- pob_2000_2020 %>% filter(ine_muni %in% zz_pueblos) %>% filter(poblacion == "Total")
zz_palma <- zz %>% filter(ine_muni == zz_pueblos[2])  #- Palma

names(pob_2000_2020)


#- que provincia ha tenido mas cambios?
 df_prov <- df %>% 
  mutate(si_cambio = ifelse(nn_cambios >= 1, 1, 0)) %>% 
  group_by(ine_prov, ine_prov.n) %>%
  mutate(nn_muni = n()) %>% 
  mutate(nn_si_cambiaron = sum(si_cambio, na.rm = TRUE)) %>% 
  mutate(percent = nn_si_cambiaron/nn_muni*100) %>% ungroup() %>% 
  select(ine_prov, ine_prov.n, nn_muni, nn_si_cambiaron, percent) %>% 
  distinct()
  
  

#- juntamos con df_original para hacer el mapa
df_2020 <- pob_2000_2020 %>% filter(year == 2020) %>% filter(poblacion == "Total") %>% select(-c(year, poblacion, values, ine_muni.n.orig))
#df <- df %>% mutate(nn_cambios = as.character(nn_cambios))
df <- inner_join(df_2020, df)

df_si_cambiaron = df %>% filter(nn_cambios >= 1) %>% mutate(nn_cambios = factor(nn_cambios, labels = c("1 cambio", "2 cambios", "3 cambios")))
names(df_si_cambiaron)
str(df_si_cambiaron)
#- mapa -------------------
library(sf)

# geometrias
geo_muni <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_muni_2020_LAU2_canarias.rds")
geo_prov <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_prov_2020_LAU2_canarias.rds")
geo_morocco <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  dplyr::filter(sovereignt %in% c("Morocco")) %>% select(sovereignt)



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
my_geo <- geo_muni #- AQUI-AQUI
my_geo <- inner_join(my_geo, df)  #- AQUI-AQUI
my_canarias <- my_geo %>% 
  filter(ine_ccaa == "05") #- solo para el cuadrito, no para las geo
ceuta <- my_geo %>% filter(ine_ccaa %in% c(18,19))
capitales <- df %>% 
  filter(capital_prov == "Sí") %>% 
  left_join(. , st_drop_geometry(geo_muni)) %>% 
  mutate(capital_prov.2 = ifelse(capital_prov == "Sí", "Capital provincial", "NO"))

my_geo_si_cambiaron <- inner_join(geo_muni, df_si_cambiaron)


names(my_geo)
#- elección de variables q grafíco ----
my_vv <- expr(nn_cambios)  #- AQUI-AQUI   (para la leyenda)
my_vv_2 <- expr(crec_porcentual.n)  #- AQUI-AQUI  para el label text

my_subtitle <- glue::glue(
  "En 2000-2020, la población en España creció un 17,16%.
   Por municipios, un 63% presentaron un <span style='color:#ee3b3b'>crecimiento negativo</span>,\n
   un 15% tuvieron un <span style='color:#8ee5ee'>crecimiento inferior a la media</span> española, mientras que 1.806 municipios,\n
   un 22%, presentaron <span style='color:#9acd32'>crecimiento superior a la media</span>.")


#- Plot con labels
p <- ggplot() + 
  geom_sf(data = geo_morocco, aes(geometry = geometry)) +
  geom_sf(data = my_geo, aes(geometry = geometry), color = "white", size = 0.08) + 
  geom_sf(data = my_geo_si_cambiaron, aes(geometry = geometry, fill = nn_cambios)) +
  geom_point(data = capitales, aes(x = X, y = Y, color = capital_prov.2)) +
  geom_sf(data = geo_prov, aes(geometry = geometry, fill = NA )) + 
  coord_sf(xlim = c(st_bbox(my_geo)[1]-0.2, st_bbox(my_geo)[3]+0.3), 
           ylim = c(st_bbox(my_geo)[2]-0.1, st_bbox(my_geo)[4]+0.3), expand = FALSE) + 
  geom_rect(aes(xmin = st_bbox(my_canarias)[1]-2.5, xmax = st_bbox(my_canarias)[3]+0.1, 
                ymin = st_bbox(my_canarias)[2]-2.5, ymax = st_bbox(my_canarias)[4]+0.1), 
            fill = NA, colour = "black", size = 0.3, show.legend = FALSE, linejoin= "round", linetype = 2) +
  scale_color_manual(name = NULL,  values = c("black")) +
  scale_fill_manual(name = NULL,  values = c( "#8ee5ee",  "#9acd32", "#ee3b3b"))  +
  labs(title = "Municipios que cambiaron de nombre durante 1996-2020",
       #subtitle = my_subtitle, x = "", y = "",
       caption = "Nombres de los municipios del INE. Geometrías del paquete LAU2boundaries4spain. Visualización: @pjpv4444")  +  
  my_theme_maps + 
  theme(plot.subtitle = ggtext::element_markdown(size = 11, 
                                                 family = "Roboto Condensed", face = "bold", color = "dimgrey")) 
 


my_textito <- glue::glue("Durante 1996-2020, el 11,23% de los municipios españoles cambiaron su nombre oficial. Los cambios se concentran en las CC.AA con 2 lenguas cooficiales pero también en provincias como Santa Cruz de Tenerife donde casi el 30% de sus municipios cambió su nombre, Sevilla (27%), Cádiz (18%) o  Avila (14%).")

p <- p + ggtext::geom_textbox(
  aes(label = my_textito, x = -12.1, y = 40.0), 
  color = "black", size = 3.5, width = grid::unit(0.25, "npc"),
  # remove box padding, since we have removed the box outline
  #box.padding = grid::unit(rep(0.1, 4), "pt") 
  fill = "white", box.color = "black", family = "Tahoma", hjust = 0L)

p



#- para guardar el plot
ggsave(p, filename = here::here("imagenes", "blog_cambios_de_nombre.png"),
       device = "png", width = 29, height = 20, units = "cm")
knitr::plot_crop(here::here("imagenes", "blog_cambios_de_nombre.png"))
