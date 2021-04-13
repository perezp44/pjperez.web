#- R script para hacer el flower post de 11 de Abril de 2021
#- voy a usar datos de elevación de aqui: https://rspatialdata.github.io/elevation.html

library(tidyverse)
library(raster)
library(elevatr)
library(sf)
library(viridis)

#library(rgeoboundaries) #- remotes::install_gitlab("dickoa/rgeoboundaries")
#swiss_bound <- geoboundaries("Switzerland")
#- aa <- rgeoboundaries::gb_adm0("Spain")

#- hace falta tener la geometría, en este caso de Pancrudo
#- remotes::install_github("rOpenSpain/LAU2boundaries4spain")
municipios <- LAU2boundaries4spain::municipios_2021 
municipios <- pjpv.datos.01::LAU2_muni_2020_canarias
pancrudo_bound <- municipios %>% filter(ine_muni.n == "Pancrudo")

#- bajamos datos de elevación (Pancrudo) -----------------
elevation_data <- elevatr::get_elev_raster(locations = pancrudo_bound, z = 9, clip = "locations")
#- convertimos en data.frame y arreglamos
elevation_data <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data)[3] = "elevation"
#- quitamos NA's
elevation_data <- elevation_data[complete.cases(elevation_data),] 

#- ahora el Joy Division plot: https://danielredondo.com/posts/20200125_joy_division/
library(ggplot2)
library(ggridges)
library(mapproj)

# Primera aproximación
p0 <- ggplot(elevation_data, aes(x = x, y = y, group = y, height = elevation)) +
  geom_density_ridges(stat = "identity", scale = 70)

ggsave(p0, filename = here::here("imagenes", "joy_division_Pancrudo_00.png"),
       device = "png", width = 10, height = 13, units = "cm")

#- grafico final de Daniel
p <- ggplot(elevation_data, aes(x = x, y = y, group = y, height = elevation)) +
  geom_density_ridges(stat = "identity", scale = 30,
                      fill = "black", color = "orange") +
  scale_x_continuous(name = NULL) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "black"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour = "white", size = 18)) +
  coord_map()
p

ggsave(p, filename = here::here("imagenes", "joy_division_Pancrudo_01.png"),
       device = "png", width = 12, height = 10, units = "cm")


#- intento hacer una linea en verde
elevation_verde <- elevation_data %>% filter(near(y, 40.79164, tol = 0.000081))
p + geom_density_ridges(data = elevation_verde, aes(x = x, y = y, group = y, height = elevation), stat = "identity", scale = 30, fill = "black", color = "white")


#- intento de ponerle un marco GOOD -------------------
#pancrudo_xx <- pancrudo_bound %>% sf::st_set_geometry(NULL)
p2 <- p + coord_sf(xlim = c(-1.095, -0.933), ylim = c(40.73, 40.91), expand = FALSE) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm")) +
  #geom_point(data = pancrudo_bound, aes(x = X, y = Y), color = "white", fill = NA, shape = 15, size = 2) +
  theme(plot.background = element_rect(colour = "snow2", size = 10.7)) +
  geom_text(data = data.frame(x = -0.987, y = 40.736, label = "Pancrudo, Teruel"),
            mapping = aes(x = x, y = y, label = label),
            size = 5.2, hjust = 0, vjust = 0, 
            colour = "orange", inherit.aes = FALSE) 

p2

ggsave(p2, filename = here::here("imagenes", "joy_division_Pancrudo_02_naranja.png"),
       device = "png", width = 15, height = 20, units = "cm")


ggsave(p2, filename = here::here("imagenes", "joy_division_Pancrudo_02_naranja_x.png"),
       device = "png", width = 6, height = 8, units = "in")
knitr::plot_crop(here::here("imagenes", "joy_division_Pancrudo_02_naranja_x.png"))

