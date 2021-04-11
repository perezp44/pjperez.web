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
municipios <- pjpv.datos.01::LAU2_muni_2020_canarias
pancrudo_bound <- municipios %>% filter(ine_muni.n == "Pancrudo")


#- bajamos datos de elevación (Pancrudo) -----------------
elevation_data <- elevatr::get_elev_raster(locations = pancrudo_bound, z = 9, clip = "locations")
#- convertimos en data.frame y arreglamos
elevation_data <- as.data.frame(elevation_data, xy = TRUE)
colnames(elevation_data)[3] = "elevation"
#- quitamos NA's
elevation_data <- elevation_data[complete.cases(elevation_data),] 


#- el plot (sin vecinos) ------------------
p <- ggplot() +
  #geom_sf(data = pancrudo_vecinos, color = "grey", fill = NA) +
  #geom_label(data = pancrudo_vecinos, color = "grey", fill = NA) +
  geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = pancrudo_bound, color = "black", fill = NA) +
  coord_sf() +
  #scale_fill_gradient(colours = terrain.colors(10)) +
  scale_fill_gradient(low = "white", high = "brown") +
  #scale_fill_gradient(low = "grey90", high = "black") +
  #scale_fill_viridis_c(direction = -1, guide = guide_legend(direction = "horizontal")) +
  ggtitle("Relieve de Pancrudo") + 
  pjpv2020.01::theme_pjp_maps() +
  ggplot2::theme(legend.position = c(0.1, 0.75)) +
  ggplot2::theme(legend.background = 
                   ggplot2::element_rect(fill = NA , color = NA)) +
  labs(caption = "Datos de elevación del paquete elevatr\n Geometrías del paquete LAU2boundaries4spain | Visualización: @pjpv4444") +
  theme(plot.caption = element_text(size = 7))
p


#- ahora el Joy Division plot: https://danielredondo.com/posts/20200125_joy_division/

library(ggplot2)
library(ggridges)
library(mapproj)



# Primera aproximación
ggplot(elevation_data, aes(x = x, y = y, group = y, height = elevation)) +
  geom_density_ridges(stat = "identity", scale = 70)



ggplot(elevation_data, aes(x = x, y = y, group = y, height = elevation)) +
  geom_density_ridges(stat = "identity", scale = 30,
                      fill = "black", color = "white") +
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



