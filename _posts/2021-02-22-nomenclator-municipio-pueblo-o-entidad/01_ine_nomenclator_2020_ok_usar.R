#- script para usar los datos del Nomenclator
library(tidyverse)
nomenclator <- readr::read_csv("/home/pjpv/Escritorio/my_datos_2021/datos/INE/nomenclator/ine_nomenclator_2010_2020.csv")


#- cuantas entidades hay en 2020
zz_2020 <- nomenclator %>% filter(year == "2020") %>% filter(poblacion == "Total")
zz <- zz_2020 %>% group_by(ine_muni, ine_muni.n) %>% summarise (NN = n())


#- PANCRUDO
#pancrudo_2020 <- nomenclator %>% filter(ine_muni.n == "Pancrudo") %>% filter(year == 2020) %>% filter(poblacion == "Total")

library(tidyverse)
nomenclator <- readr::read_csv("/home/pjpv/Escritorio/my_datos_2021/datos/INE/nomenclator/ine_nomenclator_2010_2020.csv")

pancrudo <- nomenclator %>% filter(ine_muni.n == "Pancrudo") %>% 
  filter(tipo == "Entidad singular") %>% 
  filter(poblacion == "Total") %>% 
  mutate(year1 = as.character(year)) %>% 
  mutate(name = as_factor(case_when(
    name == "PANCRUDO" ~ "Pancrudo",
    name %in% c("CERVERA DEL RINCON", "CERVERA DEL RINCÓN") ~ "Cervera del Rincón",
    name == "PORTALRUBIO" ~ "Portalrubio",
    name == "CUEVAS DE PORTALRUBIO" ~ "Cuevas de Portalrubio"))) %>% 
  mutate(name = fct_relevel(name, c("Pancrudo", "Portalrubio", "Cervera del Rincón", "Cuevas de Portalrubio")))

pancrudo_2020 <- pancrudo %>% filter(year == 2020) %>% select(year, year1, name, values)


p <- ggplot() + 
  #geom_point(data = pancrudo, aes(x = year1, y = values, color = name)) +
  geom_line(data = pancrudo, aes(x = year1, y = values, color = name, group = name), size = 1.5) +
  geom_label(data = pancrudo, aes(x = year1, y = values, label = values)) +
  #geom_text(data = pancrudo_2020, aes( x = year1, y = values,label = name), nudge_x = 1, check_overlap = TRUE, hjust = 0.5) +
  scale_color_manual(name = NULL,  values = c("blueviolet", "deeppink2", "cadetblue4", "brown1")) +
  labs(title = "Evolución de la población de los 4 pueblos del municipio de Pancrudo",
       subtitle = "",
       x = NULL,
       y = "Habitantes",
       caption = "Datos de población del Nomenclator del INE.Visualización: @pjpv4444", 
       color = NULL) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none") +
  annotate(geom = "text", x = 11.75, y = 81.2, label ="Pancrudo", color = "black", size = 5) +
  annotate(geom = "text", x = 11.75, y = 18.5, label ="Portalrubio", color = "black", size = 5) +
  annotate(geom = "text", x = 12.135, y = 13.2, label ="Cervera del Rincón", color = "black", size = 5) +
  annotate(geom = "text", x = 12.24, y = 6.8, label ="Cuevas de Portalrubio", color = "black", size = 5) +
  scale_x_discrete(expand = expansion(mult = c(0.04, .225))) +
  theme_minimal() + theme(plot.title = element_text(size = 16, face = "bold"),
    legend.position = "none",
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    plot.background = element_rect(fill = "ghostwhite", color = NA))
  


#- para guardar el plot
ggsave(p, filename = here::here("evolucion_poblacion_pancrudo.png"),
       device = "png", width = 32, height = 20, units = "cm")


#- CURIOSIDADES ------------------------

#- municipios con mas entidades colectivas

df <- nomenclator %>% filter(poblacion == "Total") %>% filter(year == 2020)


zz_colectivas <- df %>% filter(tipo == "Entidad colectiva") %>% 
  group_by(ine_muni) %>% mutate(NN = n()) %>% select(ine_muni, ine_muni.n, ine_prov.n, NN) %>% distinct()
zz_Lugo <- df %>% filter(ine_muni == 27028) %>% filter(tipo == "Entidad colectiva")



zz_singulares <- df %>% filter(tipo == "Entidad singular") %>% 
  group_by(ine_muni) %>% mutate(NN = n()) %>% select(ine_muni, ine_muni.n, ine_prov.n, NN) %>% distinct()

zz_Mieres <- df %>% filter(ine_muni == 33037) %>% filter(tipo == "Entidad singular")
  
janitor::tabyl(zz_Mieres, values)
zz_nucleos <- df %>% filter(tipo == "Nucleo") %>% 
  group_by(ine_muni) %>% mutate(NN = n()) %>% select(ine_muni, ine_muni.n, ine_prov.n, NN) %>% distinct()


zz_diseminados <- df %>% filter(tipo == "Diseminado") %>% 
  group_by(ine_muni) %>% mutate(NN = n()) %>% select(ine_muni, ine_muni.n, ine_prov.n, NN) %>% distinct()
zz_Villalba <- df %>% filter(ine_muni == 27065)



#-  Zaragoza
df <- nomenclator %>% filter(poblacion == "Total")



zz_Zaragoza <- df %>% filter(ine_muni.n == "Zaragoza") %>% filter(tipo == "Municipio")
zz_Villa <- df %>% filter(ine_muni == "50903") %>% filter(tipo == "Municipio")

#- EL Palmar de Troya
df <- nomenclator %>% filter(poblacion == "Total") 

my_codes <- c("41095000000", "41904000000", "41095000201")

zz <- df %>% filter(code %in% my_codes)


zz <- zz %>% 
  mutate(year1 = as.character(year)) 

%>% 
  mutate(name = as_factor(case_when(
    name == "PANCRUDO" ~ "Pancrudo",
    name %in% c("CERVERA DEL RINCON", "CERVERA DEL RINCÓN") ~ "Cervera del Rincón",
    name == "PORTALRUBIO" ~ "Portalrubio",
    name == "CUEVAS DE PORTALRUBIO" ~ "Cuevas de Portalrubio"))) %>% 
  mutate(name = fct_relevel(name, c("Pancrudo", "Portalrubio", "Cervera del Rincón", "Cuevas de Portalrubio")))



p <- ggplot() + 
  #geom_point(data = pancrudo, aes(x = year1, y = values, color = name)) +
  geom_line(data = zz, aes(x = year1, y = values, color = name, group = name), size = 1.5) +
  geom_label(data = zz, aes(x = year1, y = values, label = values)) +
  #geom_text(data = pancrudo_2020, aes( x = year1, y = values,label = name), nudge_x = 1, check_overlap = TRUE, hjust = 0.5) +
  scale_color_manual(name = NULL,  values = c("blueviolet", "deeppink2", "cadetblue4")) 
 

#- para guardar el plot
ggsave(p, filename = here::here("evolucion_poblacion_pancrudo.png"),
       device = "png", width = 32, height = 20, units = "cm")














zz_Utrera <- df %>% filter(ine_muni == "41095") %>% filter(tipo == "Municipio")
zz_Palmar <- df %>% filter(ine_muni == "41904") %>% filter(tipo == "Municipio")


zz_Utrera2 <- df %>% filter(ine_muni == "41095") 

zz_Palmar2 <- df %>% filter(ine_muni == "41095") %>% filter(code == "41095000201")

41095000201 (nucleo)

41095000299
41095000200   (entidad singular)
