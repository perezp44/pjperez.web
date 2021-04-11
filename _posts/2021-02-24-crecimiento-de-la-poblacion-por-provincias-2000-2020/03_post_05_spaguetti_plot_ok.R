#- quiero hacer plot(spaguetti) con el creciminetod e la población provincial
library(tidyverse)

#- datos de población
pob_2000_2020 <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/INE/pob_mun/ine_pob_mun_1996_2020.rds")

#- rtdos (crec. poblacion en España)
pob_esp <- pob_2000_2020 %>% 
  filter(year >= 2000) %>% 
  filter(poblacion == "Total") %>% 
  select(year, values) %>% 
  group_by(year) %>% 
  summarise(habitantes = sum(values)) %>% 
  mutate(crec_abs = habitantes - lag(habitantes)) %>% 
  mutate(crec_percent = crec_abs /lag(habitantes)) %>% 
  mutate(pob_indice = habitantes / first(habitantes) *100) %>% ungroup()


#- resultados pob provincial
pob_prov <- pob_2000_2020 %>% 
  filter(year >= 2000) %>% 
  filter(poblacion == "Total") %>% 
  select(ine_ccaa, ine_prov, ine_prov.n, year, values) %>% 
  group_by(ine_ccaa, ine_prov, ine_prov.n, year) %>% 
  summarise(habitantes = sum(values))  %>% distinct() %>% 
  arrange(year) %>% 
  mutate(crec_habi = habitantes - lag(habitantes)) %>% 
  mutate(crec_habi_0 = crec_habi) %>% 
  mutate(crec_habi_0 = ifelse(year == 2000, 0, crec_habi_0)) %>% 
  mutate(crec_habi_percent = crec_habi / lag(habitantes)*100) %>% 
  mutate(crec_habi_acu = cumsum(crec_habi_0)) %>% 
  mutate(crec_habi_percent_acu = crec_habi_acu / first(habitantes)*100) %>% 
  mutate(pob_prov_indice = habitantes / first(habitantes) *100) %>% ungroup()


pob_prov_crec_wide <- pob_prov %>% 
  select(ine_prov, ine_prov.n, year, crec_habi_percent) %>% 
  pivot_wider(names_from = year, values_from = crec_habi_percent) %>% 
  select(-`2000`) %>% ungroup()

pob_prov_crec <- pob_prov %>% 
  select(ine_prov, ine_prov.n, year, crec_habi_percent) %>% 
  filter(year != 2000) %>% ungroup()

my_prov_selected <- c("Guadalajara", "Teruel", "Madrid", "Barcelona", "Lugo", "Sevilla", "Valencia", "Cáceres", "Burgos", "Almería", "Tarragona", "Zamora")
pob_prov_selected <- pob_prov %>% filter(ine_prov.n %in% my_prov_selected)

zz_prov_2020 <- pob_prov %>% filter(year == 2020) %>% 
                filter(ine_prov.n %in% my_prov_selected)

#- gráfico lineas para ver como crecen las provincias ------------

p <- ggplot() + 
  #geom_point(data = pob_prov, aes(x = year, y = pob_prov_indice), color = "grey") +
  geom_line(data = pob_prov, aes(x = year, y = pob_prov_indice, group = ine_prov.n), color = "grey")  +
  geom_point(data = pob_prov_selected, aes(x = year, y = pob_prov_indice, color = ine_prov.n)) +
  geom_line(data = pob_prov_selected, aes(x = year, y = pob_prov_indice, group = ine_prov.n, color = ine_prov.n)) +
  geom_line(data = pob_esp, aes(x = year, y = pob_indice), size = 1.5, color = "brown2") +
  geom_label(data = zz_prov_2020, aes(x = year, y = pob_prov_indice, label = ine_prov.n), nudge_x = 0.9) +
  theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"),
                          legend.position = "none",
                          text = element_text(family = "Ubuntu Regular", color = "#22211d"),
                          plot.background = element_rect(fill = "ghostwhite", color = NA),
                          plot.caption = element_text(hjust = 0.1)) +
labs(title = "Evolución de la población provincial (2000-2020)",
     subtitle = "(Números índice, 2000 = 100)",
     x = NULL,
     y = "Habitantes",
     caption = "Datos de población del INE.Visualización: @pjpv4444", 
     color = NULL) 

  #annotate(geom = "text", x = 11.75, y = 81.2, label ="Pancrudo", color = "black", size = 5) +
  #scale_x_discrete(expand = expansion(mult = c(0.04, .08))) 
  

p <- p + geom_text(aes(x = 2020.7, y = 117.5, label= "España"), nudge_x = 0.2, size = 5, fontface = "bold", color = "brown2")

p
#- para guardar el plot
ggsave(p, filename = here::here("imagenes", "spaguetti_crec_pob_provincial.png"),
       device = "png", width = 29, height = 20, units = "cm")
knitr::plot_crop(here::here("imagenes", "spaguetti_crec_pob_provincial.png"))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

  
#- ver si me sale un facetted plot (SÍ, pero salía feo) ----
prov_2020 <- pob_prov %>% filter(year == 2020)

p <- ggplot() + 
  geom_line(data = pob_prov, aes(x = year, y = pob_prov_indice, group = ine_prov.n, color = ine_prov.n))  +
  geom_point(data = pob_prov, aes(x = year, y = pob_prov_indice, color = ine_prov.n)) +
  geom_label(data = prov_2020, aes(x = year, y = pob_prov_indice, label = ine_prov.n)) +
  geom_line(data = pob_esp, aes(x = year, y = pob_indice), size = 1.5, color = "brown2") + 
  theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"),
                          legend.position = "none",
                          text = element_text(family = "Ubuntu Regular", color = "#22211d"),
                          plot.background = element_rect(fill = "ghostwhite", color = NA),
                          plot.caption = element_text(hjust = 0.1)) +
  labs(title = "Evolución de la población provincial (2000-2020)",
       subtitle = "(Números índice, 2000 = 100)",
       x = NULL,
       y = "Habitantes",
       caption = "Datos de población del INE.Visualización: @pjpv4444", 
       color = NULL) +
# geom_text(aes(x = 2020.7, y = 117.5, label= "España"), nudge_x = 0.2, size = 5, fontface = "bold", color = "brown2") +
    facet_wrap( ~ ine_ccaa, ncol = 2)

p

p <- ggplot() + 
  #geom_point(data = pob_prov, aes(x = year, y = pob_prov_indice), color = "grey") +
  geom_line(data = pob_prov, aes(x = year, y = pob_prov_indice, group = ine_prov.n), color = "grey")  +
  geom_point(data = pob_prov_selected, aes(x = year, y = pob_prov_indice, color = ine_prov.n)) +
  geom_line(data = pob_prov_selected, aes(x = year, y = pob_prov_indice, group = ine_prov.n, color = ine_prov.n)) +
  geom_line(data = pob_esp, aes(x = year, y = pob_indice), size = 1.5, color = "brown2") +
  geom_label(data = zz_prov_2020, aes(x = year, y = pob_prov_indice, label = ine_prov.n), nudge_x = 0.9) +
  theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"),
                          legend.position = "none",
                          text = element_text(family = "Ubuntu Regular", color = "#22211d"),
                          plot.background = element_rect(fill = "ghostwhite", color = NA),
                          plot.caption = element_text(hjust = 0.1)) +
  labs(title = "Evolución de la población provincial (2000-2020)",
       subtitle = "(Números índice, 2000 = 100)",
       x = NULL,
       y = "Habitantes",
       caption = "Datos de población del INE.Visualización: @pjpv4444", 
       color = NULL) 

