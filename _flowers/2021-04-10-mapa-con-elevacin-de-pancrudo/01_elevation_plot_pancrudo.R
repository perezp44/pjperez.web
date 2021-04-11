#- R script para hacer el flower post de 10 de Abril de 2021
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
#- remotes::install_github("rOpenSpain/LAU2boundaries4spain")
municipios <- LAU2boundaries4spain::municipios_2021 

pancrudo_bound <- municipios %>% filter(ine_muni.n == "Pancrudo")
aa <- sf::st_touches(pancrudo_bound, municipios)[[1]]
pancrudo_vecinos <- municipios %>% slice(aa)


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

ggsave(p, filename = here::here("imagenes", "relieve_elevacion_Pancrudo.png"),
       device = "png", width = 10, height = 13, units = "cm")

#- OK, conseguido. A ver si puedo mejorarlo algo!!!
#- primero simplement cambiar el color y ...
#- whit tinter pkg: https://github.com/sebdalgarno/tinter
library(tinter)
hex <- "#335CAC"
tinter(hex, steps = 10, crop = 7)

p_blue <- p +  scale_fill_gradientn(colours = tinter(hex, steps = 10)) +
  theme(legend.position ="bottom")

ggsave(p_blue, filename = here::here("imagenes", "relieve_elevacion_Pancrudo_blue.png"),
       device = "png", width = 10, height = 13, units = "cm")


library(patchwork)

p_mas_blue <- p + p_blue

ggsave(p_mas_blue, filename = here::here("imagenes", "relieve_elevacion_Pancrudo_2.png"),
       device = "png", width = 20, height = 15, units = "cm")

#- Pueblos vecinos --------------------------
#- bajamos datos de elevación (vecinos Pancrudo) 
elevation_data_vecinos <- elevatr::get_elev_raster(locations = pancrudo_vecinos, z = 9, clip = "locations")
#- convertimos en data.frame y arreglamos
elevation_data_vecinos <- as.data.frame(elevation_data_vecinos, xy = TRUE)
colnames(elevation_data_vecinos)[3] = "elevation"
#- quitamos NA's
elevation_data_vecinos <- elevation_data_vecinos[complete.cases(elevation_data_vecinos),] 

#- altitud de los vecinos (segun IGN)
pueblos_ign <- rio::import("/home/pjpv/Escritorio/my_datos_2021/datos/codigos/IGN_municipios.xlsx") %>% dplyr::select(ine_muni, ine_muni.n, ALTITUD, ine_prov.n)
names(pueblos_ign)
pancrudo_vecinos <- left_join(pancrudo_vecinos, pueblos_ign) %>% dplyr::select(ine_muni, ine_muni.n, ALTITUD, ine_prov.n, X, Y)
pancrudo_vecinos <- pancrudo_vecinos %>% mutate(textito = glue::glue("{ine_muni.n}\n({ALTITUD})"))



#- el plot (con vecinos) --------------
p_vecinos <- ggplot() +
  #- datos de elevación del terreno
  geom_raster(data = elevation_data_vecinos, aes(x = x, y = y, fill = elevation)) +
  #- geometrias de los municipios vecinos
  geom_sf(data = pancrudo_vecinos, color = "grey5", fill = NA) +
  #- los municipios vecinos
  geom_point(data = pancrudo_vecinos, aes(x = X, y = Y), color = "grey5", fill = NA) +
  geom_text(data = pancrudo_vecinos, 
            aes(x = X, y = Y-0.009, label = glue::glue("{ine_muni.n}\n({ALTITUD})")),
            color = "grey5", fontface = "bold",  check_overlap = TRUE, size = 2.2) +
  #- datos de elevacion de Pancrudo
  geom_raster(data = elevation_data, aes(x = x, y = y, fill = elevation)) +
  #- geometria de Pancrudo
  geom_sf(data = pancrudo_bound, color = "black", fill = NA, size = 0.8) +
  #- municipio de Pancrudo
  geom_point(data = pancrudo_bound, aes(x = X, y = Y), color = "darkgreen", fill = NA, shape = 15, size = 2) +
  geom_text(data = pancrudo_bound, 
            aes(x = X+0.007, y = Y-0.01, label = glue::glue("{ine_muni.n}\n(1233)")),
            color = "darkgreen", fontface = "bold",  check_overlap = TRUE, size = 3.1) +
  coord_sf() +
  #- pruebo varias escalas
  #scale_fill_gradient(colours = terrain.colors(10)) +
  scale_fill_gradient(low = "white", high = "brown") +
  #scale_fill_gradient(low = "grey90", high = "black") +
  #scale_fill_viridis_c(direction = -1, guide = guide_legend(direction = "horizontal")) +
  ggtitle("Relieve de Pancrudo (y vecinos)") + 
  pjpv2020.01::theme_pjp_maps() +
  ggplot2::theme(legend.position = c(0.07, 0.82)) +
  ggplot2::theme(legend.background = 
                   ggplot2::element_rect(fill = NA , color = NA)) +
  labs(caption = "Datos de elevación del paquete elevatr\n Geometrías del paquete LAU2boundaries4spain | Visualización: @pjpv4444") +
  theme(plot.caption = element_text(size = 7))

p_vecinos


ggsave(p_vecinos, filename = here::here("imagenes", "relieve_elevacion_Pancrudo_vecinos.png"),
       device = "png", width = 18, height = 14, units = "cm")

#- Poner los 4 pueblos de Pancrudo -----------------
pueblos1 <- rio::import("/home/pjpv/Escritorio/my_datos_2021/datos/codigos/IGN_municipios.xlsx") %>% filter(stringr::str_detect(COD_INE_CAPITAL, "^44177"))

p + geom_point(data = pueblos1, aes(x = LONGITUD_ETRS89, y = LATITUD_ETRS89))
names(pueblos1)

#- voy a geolocalizar ----------
pueblos <- tibble::tibble(
  pueblo = c("Cervera del Rincón" ,   "Cuevas de Portalrubio", "Pancrudo","Portalrubio"  ),
  provincia = "Teruel",
  pais = "Spain")
pueblos <- pueblos %>% mutate(text_to_geocode = paste(pueblo, provincia, pais, sep = ", "))

# now geocode -----
library(tidygeocoder)
pueblos <- pueblos %>% tidygeocoder::geocode(text_to_geocode, method = "osm") %>% dplyr::select(- text_to_geocode)

# convert coordinates to an sf object
#pueblos <- pueblos %>% sf::st_as_sf(coords = c("long", "lat"), crs = 4326)


#- situo los 4 pueblos como puntos
pp <- p + geom_point(data = pueblos, aes(x = long, y = lat), shape = 15, color = "darkgreen", size = 2.5) + 
  #geom_point(data = pueblos1, aes(x = LONGITUD_ETRS89, y = LATITUD_ETRS89), color = "purple") + 
  #ggrepel::geom_label_repel(data = pueblos, aes(x = long, y = lat, label = pueblo)) +
  geom_text(data = pueblos, 
           aes(x = long, y = lat+0.003, label = pueblo),
           color = "darkgreen", fontface = "bold",  check_overlap = TRUE, size = 3.1) 
pp
#- meto los accidentes geograficos de Pancrudo (nomenclator geografico del IGN)
accidentes <- rio::import("/home/pjpv/Escritorio/my_datos_2021/datos/IGN/pancrudo_IGN_nomenclator_geografico.rds")
tipos_acc <- accidentes %>% distinct(codigo_ngbe, codigo_ngbe_text)
accidentes_montes <- accidentes %>% filter(codigo_ngbe %in% c("4.1.3", "4.1.2"))
accidentes_fuentes <- accidentes %>% filter(codigo_ngbe %in% c("5.5"))
accidentes_vertices <- accidentes %>% filter(codigo_ngbe %in% c("2.3.1"))

janitor::tabyl(accidentes, codigo_ngbe_text )

pp_montes <- pp + 
  geom_point(data = accidentes_montes, aes(x = long_etrs89_regcan95, y = lat_etrs89_regcan95), color = "black", shape = 17) +
  geom_point(data = accidentes_vertices, aes(x = long_etrs89_regcan95, y = lat_etrs89_regcan95), color = "black", shape = 8) +
  geom_text(data = accidentes_vertices, aes(x = long_etrs89_regcan95, y = lat_etrs89_regcan95+0.003, label = identificador_geografico),
            color = "black", check_overlap = TRUE, size = 3) +
  geom_text(data = accidentes_montes, aes(x = long_etrs89_regcan95, y = lat_etrs89_regcan95-0.002, label = identificador_geografico),
            color = "black", check_overlap = TRUE, size = 2.5) +
  labs(title = "Montes de Pancrudo",
       caption = "") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(color = "saddlebrown"))+
    theme(plot.title = element_text(hjust = 0.5))     # Center ggplot title
  
  
pp_fuentes <- pp + 
  geom_point(data = accidentes_fuentes, aes(x = long_etrs89_regcan95, y = lat_etrs89_regcan95), color = "navyblue") +
  geom_text(data = accidentes_fuentes, aes(x = long_etrs89_regcan95, y = lat_etrs89_regcan95-0.002, label = identificador_geografico),
       color = "navyblue", check_overlap = TRUE, size = 2.5) +
  labs(title = "Fuentes de Pancrudo", caption = "") +
  theme(plot.title = element_text(color = "midnightblue"))+
  theme(plot.title = element_text(hjust = 0.5))     # Center ggplot title


#- OK voy a juntar pp_fuentes y pp_montes en un gráfico con patch y luego le voy a añadir label y caption con cowplot
library(patchwork)
ppp <- pp_montes + pp_fuentes 


library(cowplot)
title <- ggdraw() +
  draw_label("Relieve del municipio de Pancrudo",
             fontfamily = "DejaVu Serif Condensed",
             #colour = "lightpink4",
             hjust = 0.5, size = 22)

caption <- ggdraw() +
  draw_label("\n Datos de montes y fuentes del IGN | Datos de elevación del paquete elevatr\n Geometrías del paquete LAU2boundaries4spain | Visualización: @pjpv4444",
             #fontfamily = "DejaVu Serif Condensed",
             #colour = "lightpink4",
             hjust = 0.5,
             size = 8) 


#-juntos el plot, el label y la caption
pppp <- plot_grid(title,
               #plot_grid(ppp, nrow = 1, rel_widths = c(0.5, 0.5)),
               ppp,
               caption,
               ncol = 1,
               rel_heights = c(0.15, 0.754, 0.06))


pppp


#- no consigo quitar el trozo blanco
zz <- pppp + pjpv2020.01::theme_pjp_maps()

zz  + theme(panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = "#f5f5f2"),
            plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
            panel.border = ggplot2::element_rect(fill = "#f5f5f2", color = NA))

zz

ggsave(zz, filename = here::here("imagenes", "relieve_elevacion_Pancrudo_fuentes.png"),
       device = "png", width = 24, height = 17, units = "cm")
#knitr::plot_crop(here::here("imagenes", "relieve_elevacion_Pancrudo_fuentes.png"))



#- HEXAGONOS -------------------------------------------------------------------


#- intente hacer esto el mapa con hexágonos: https://danielredondo.com/posts/20180723_granada_hex/
# Seleccionamos el tamaño de los hexágonos, que variará en función a:
#   - El tamaño del área que queramos cubrir
#   - El número de logos que queramos incluir
library(rgdal)
library(sp)
library(ggplot2)
library(sf)
#- guardamos como .shp
pancrudo_bound %>% sf::st_write("./_pruebas/aa.shp", delete_dsn=TRUE)
#- leemos el .shp
aa <- readOGR("./_pruebas/aa.shp",  use_iconv = TRUE, encoding = "UTF-8")
#- cramos los hexagonos
hex_points <- aa %>% spsample(type = "hexagonal", cellsize = .04)
# Aquí podemos ver cuántos hexágonos hay:
as_tibble(hex_points@coords)

# Especificamos de nuevo el tamaño de los hexágonos 
pancrudo_hex <- HexPoints2SpatialPolygons(hex_points, dx = .04)
pancrudo_hex <- pancrudo_hex %>% sf::st_as_sf()
# Representamos el mapa original con los hexágonos superpuestos, para ver cómo quedaría:
ggplot() + 
  geom_sf(data =pancrudo_bound) + 
  geom_sf(data = pancrudo_hex, colour = "blue", fill = NA)

#- falta rellenar con los hexágonos: https://www.mitchelloharawild.com/blog/user-2018-feature-wall/
#- https://www.mitchelloharawild.com/blog/user-2018-feature-wall/
source("./assets/function_hexwall.R")
plotito <- hexwall("./_pruebas/samplehex", sticker_width = 500,
  coords = hex_points@coords,
  sort_mode = "colour")
plot(plotito)


