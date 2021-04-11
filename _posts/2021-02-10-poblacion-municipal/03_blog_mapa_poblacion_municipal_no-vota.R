#- quiero hacer un mapa con el crecimiento de la población en el periodo 2000-2020
#- land does not vote: no me ha salido
#- @loreabad6: "El territorio no vota, las personas sí" de Lorena Abad, lo hace con tmap(): https://twitter.com/loreabad6/status/1361265268916379654/photo/1


library(tidyverse)
library(sf)

# datos ----
geo_muni <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_muni_2020_LAU2_canarias.rds")
geo_prov <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/geo_datos_mios/geo_prov_2020_LAU2_canarias.rds")
pob_2000_2020 <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/INE/pob_mun/ine_pob_mun_1996_2020.rds")
geo_morocco <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
               dplyr::filter(sovereignt %in% c("Morocco")) %>% select(sovereignt)

#- crecimiento de la poblacon en España 2000-2020
crec_esp <- pob_2000_2020 %>% 
  filter(year %in% c(2000, 2020)) %>% 
  filter(poblacion == "Total") %>% 
  select(year, values) %>% 
  group_by(year) %>% 
  summarise(habitantes = sum(values)) %>% 
  mutate(crec_abs = habitantes - lag(habitantes)) %>% 
  mutate(crec_percent = crec_abs /lag(habitantes)) %>% ungroup()
media_crec_esp <-  crec_esp[[2,4]]*100
#- calculo crecimiento 2000-20 -----
pob_muni <- pob_2000_2020 %>% 
  filter(year %in% c(2000, 2020)) %>% 
  filter(poblacion == "Total") %>% 
  group_by(ine_muni, ine_muni.n, year) %>% 
  mutate(habitantes = sum(values)) %>% 
  select(ine_muni, ine_muni.n, capital_prov, ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n, year, habitantes) %>%
  distinct() %>% 
  pivot_wider(names_from = c("year"), values_from = c("habitantes")) %>% 
  mutate(crec_2020_2000 = `2020`- `2000`) %>% 
  mutate(crec_porcentual = (`2020`- `2000`) / `2000` *100) %>% 
  arrange(desc(crec_porcentual)) %>% ungroup() %>% 
  mutate(crec_porcentual.n = paste0(round(crec_porcentual, digits = 1), "%")) %>% 
  mutate(crec_porcentual_d = as_factor(case_when(
    crec_porcentual < 0 ~ "Negativo",
    between(crec_porcentual, 0, media_crec_esp) ~ "< media",
    crec_porcentual > media_crec_esp ~ "> media")))
pob_muni <- pob_muni %>% mutate(crec_porcentual_d = fct_relevel(crec_porcentual_d, "Negativo", "< media"))

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
my_geo <- inner_join(my_geo, pob_muni)  #- AQUI-AQUI
my_canarias <- my_geo %>% 
  filter(ine_ccaa == "05") #- solo para el cuadrito, no para las geo
ceuta <- my_geo %>% filter(ine_ccaa %in% c(18,19))
capitales <- pob_muni %>% 
  filter(capital_prov == "Sí") %>% 
  left_join(. , st_drop_geometry(geo_muni)) %>% 
  mutate(capital_prov.2 = ifelse(capital_prov == "Sí", "Capital provincial", "NO"))


#- elección de variables q grafíco ----
my_vv <- expr(crec_porcentual_d)  #- AQUI-AQUI   (para la leyenda)
my_vv_2 <- expr(crec_porcentual.n)  #- AQUI-AQUI  para el label text

janitor::tabyl(pob_muni, crec_porcentual_d)
my_subtitle <- glue::glue(
  "En 2000-2020, la población en España creció un 17,16%.
   Por municipios, un 63% presentaron un <span style='color:#ee3b3b'>crecimiento negativo</span>,\n
   un 15% tuvieron un <span style='color:#8ee5ee'>crecimiento inferior a la media</span> española, mientras que 1.806 municipios,\n
   un 22%, presentaron <span style='color:#9acd32'>crecimiento superior a la media</span>.")


#- Plot con labels
p <- ggplot() + 
  geom_sf(data = geo_morocco, aes(geometry = geometry)) +
   #geom_sf(data = geo_prov, aes(geometry = geometry, fill = NA )) + 
  coord_sf(xlim = c(st_bbox(my_geo)[1]-0.2, st_bbox(my_geo)[3]+0.3), 
           ylim = c(st_bbox(my_geo)[2]-0.1, st_bbox(my_geo)[4]+0.3), expand = FALSE) + 
  geom_rect(aes(xmin = st_bbox(my_canarias)[1]-2.5, xmax = st_bbox(my_canarias)[3]+0.1, 
                ymin = st_bbox(my_canarias)[2]-2.5, ymax = st_bbox(my_canarias)[4]+0.1), 
            fill = NA, colour = "black", size = 0.3, show.legend = FALSE, linejoin= "round", linetype = 2) +
  #scale_color_manual(name = NULL,  values = c("black")) +
  scale_fill_manual(name = NULL,  values = c("#ee3b3b", "#8ee5ee",  "#9acd32")) +
  labs(title = "Crecimiento de la población por provincias: 2000-2020",
       subtitle = my_subtitle, x = "", y = "",
       caption = "Datos de población del INE. Geometrías del paquete LAU2boundaries4spain. Visualización: @pjpv4444")  +  
  geom_point(data = radios, aes(x = X, y = Y, size = radius, color = crec_porcentual_d) ) + 
    my_theme_maps + 
  theme(plot.subtitle = ggtext::element_markdown(size = 11, 
          family = "Roboto Condensed", face = "bold", color = "dimgrey")) 

my_textito <- glue::glue("Los municipios con crecimientos de población <span style='color:#9acd32'>superiores a la media</span>, se concentran a lo largo de la costa mediterranea, Baleares, Canarias, área de influencia de Madrid y en menor medida en el País Vasco")
  
p + ggtext::geom_textbox(
	aes(label = my_textito, x = -12.1, y = 40.0), 
	color = "black", size = 3.5, width = grid::unit(0.25, "npc"),
	# remove box padding, since we have removed the box outline
	#box.padding = grid::unit(rep(0.1, 4), "pt") 
	fill = "white", box.color = "black", family = "Tahoma", hjust = 0L)

p

zz <- my_geo %>% st_drop_geometry()


# Prep function inputs (specify radius factor)
radios <- zz %>% mutate(radius = sqrt(abs(crec_2020_2000)/(2*pi))) 
radios <- zz %>% mutate(radius = 3) 

#- para guardar el plot
# ggsave(p, filename = here::here("plots", "2021_02_10_poblacion_municipal_01.png"), 
#        device = "png", width = 29, height = 20, units = "cm")
# knitr::plot_crop(here::here("plots", "2021_02_10_poblacion_municipal_01.png"))

# primero intenté poner la caja de texto con geom_rect() y geom_text() 
# la posición lo hice con ggannotate::ggannotate(p)   pero ....
# p <- p + geom_rect(data = data.frame(xmin = -12.10151567, xmax = -7.7004555485185, 
#                                 ymin = 37.249165749241, ymax = 41.581415225288), 
#               mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), size = 0.07, colour = "black", 
#               fill = "lightblue", alpha = 0.08, inherit.aes = FALSE) 
# p + geom_text(data = data.frame(x = -11.7042272283212, y = 41.1706865724875, 
#                             label = "Los municipios con crecimientos de población importantes se conc"), 
#                              mapping = aes(x = x, y = y, label = label), hjust = 0L, 

                
                
                  
                 

#- pare el título
#- https://cran.r-project.org/web/packages/ggtext/vignettes/plotting_text.html
#- https://wilkelab.org/ggtext/articles/theme_elements.html

#- Here is the full set of things you can change in element_text: (para el título)
#element_text(family = NULL, face = NULL, colour = NULL, size = NULL, hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, color = NULL)
#- colores: https://www.infoworld.com/article/3527449/add-color-to-your-ggplot2-text-in-r.html

#- como guardar el plot: https://www.jumpingrivers.com/blog/knitr-rmarkdown-image-size/
#- http://zevross.com/blog/2017/06/19/tips-and-tricks-for-working-with-images-and-figures-in-r-markdown-documents/
