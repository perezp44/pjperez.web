#- quiero hacer un mapa con el crecimiento de la población en el periodo 2000-2020
library(tidyverse)
library(reactable)

# datos ----
pob_2000_2020 <- readr::read_rds("/home/pjpv/Escritorio/my_datos_2021/datos/INE/pob_mun/ine_pob_mun_1996_2020.rds")
#- filtro años y quito variables
df <- pob_2000_2020  %>% 
  filter(year > 1999) %>% 
  filter(poblacion == "Total") %>% select(-c(poblacion, ine_muni.n.orig)) %>% rename(pob_muni = values)  

#- crecimiento de la población municipal 2000-2020
df <- df %>% 
 group_by(ine_muni) %>% arrange(year) %>% 
    mutate(pob_muni_crec = pob_muni - lag(pob_muni), .after = pob_muni) %>% 
    mutate(pob_muni_crec_acu = pob_muni - first(pob_muni), .after = pob_muni_crec) %>% 
    mutate(pob_muni_perc = (pob_muni - lag(pob_muni))/lag(pob_muni)*100, .after = pob_muni_crec_acu) %>% 
    mutate(pob_muni_perc_acu = (pob_muni - first(pob_muni))/first(pob_muni)*100, .after = pob_muni_perc) %>% ungroup()

#- resulta q hay municipios q no existen todos los año y eso repercute en como calculas los crecimientos provinciales y ccaa y esp
#- crecimientos provinciales
#- SALE MAL el crecimiento de CCAA y no lo entiendo
#- resulta que no sale siempre NA en 2000 ASI QUE LO HE TENIDO QUE TROZEAR -----

crec_provs_ccaa_esp <- df %>% 
  group_by(year) %>% mutate(pob_esp = sum(pob_muni, na.rm = TRUE), .after = pob_muni) %>% ungroup() %>%
  group_by(year, ine_ccaa) %>% mutate(pob_ccaa = sum(pob_muni, na.rm = TRUE), .after = pob_muni) %>% ungroup() %>% 
  group_by(year, ine_prov) %>% mutate(pob_prov = sum(pob_muni, na.rm = TRUE), .after = pob_muni) %>% ungroup() %>% 
  select(year, ine_prov, ine_ccaa, pob_prov, pob_ccaa, pob_esp) %>% distinct() 


zz_prov <- crec_provs_ccaa_esp %>% 
    select(year, ine_prov, pob_prov) %>% distinct() %>% 
    group_by(ine_prov) %>% arrange(year) %>%
    mutate(pob_prov_perc = (pob_prov - lag(pob_prov))/lag(pob_prov)*100, .after = pob_prov) %>% 
    mutate(pob_prov_perc_acu = (pob_prov - first(pob_prov))/first(pob_prov)*100, .after = pob_prov_perc) %>% ungroup() 



zz_ccaa <- crec_provs_ccaa_esp %>% 
  select(year, ine_ccaa, pob_ccaa) %>% distinct() %>% 
  group_by(ine_ccaa) %>% arrange(year) %>%
  mutate(pob_ccaa_perc = (pob_ccaa - lag(pob_ccaa))/lag(pob_ccaa)*100, .after = pob_ccaa) %>% 
  mutate(pob_ccaa_perc_acu = (pob_ccaa - first(pob_ccaa))/first(pob_ccaa)*100, .after = pob_ccaa_perc) %>% ungroup() 


zz_esp <- crec_provs_ccaa_esp %>% 
  select(year, pob_esp) %>% distinct() %>% 
  arrange(year) %>%
  mutate(pob_esp_perc = (pob_esp - lag(pob_esp))/lag(pob_esp)*100, .after = pob_esp) %>% 
  mutate(pob_esp_perc_acu = (pob_esp - first(pob_esp))/first(pob_esp)*100, .after = pob_esp_perc) %>% ungroup()
  
  



#- fusionar df y crec_provs para añadir los creciminetos prov ccaa y en España
df <- left_join(df, zz_prov) %>% left_join(. , zz_ccaa) %>% left_join(. , zz_esp)

rm(crec_provs_ccaa_esp, zz_prov, zz_ccaa, zz_esp)  

#- indicadores de super-crecimiento -----------

zz <- df %>% 
  mutate(crece_mas_esp = ifelse(pob_muni_perc >= pob_esp_perc, 1, 0)) %>% 
  mutate(crece_mas_prov = ifelse(pob_muni_perc >= pob_prov_perc, 1, 0)) %>% 
  group_by(ine_muni) %>% 
  #- crec por encima de españa
  mutate(crece_mas_esp_nn = sum(crece_mas_esp, na.rm = TRUE)) %>% 
  mutate(crece_mas_prov_nn = sum(crece_mas_prov, na.rm = TRUE)) %>% 
  #- creciminetos negativos
  mutate(crece_negativo = ifelse(pob_muni_perc < 0 ,1, 0)) %>% 
  mutate(crece_negativo_nn = sum(crece_negativo, na.rm = TRUE)) %>%  ungroup()
  

  
  
zz_1 <- zz %>% filter(crece_mas_esp_nn == 20)  %>% filter(year == 2020) %>% select(ine_muni.n, ine_prov.n, pob_muni, pob_muni_crec_acu, pob_muni_perc_acu)

#- super decrecimineto
zz2 <- zz %>% filter(crece_negativo_nn == 20)  %>% filter(year == 2020) %>% select(ine_muni.n, ine_prov.n, pob_muni, pob_muni_crec_acu, pob_muni_perc_acu)



#- gráfico lineas para ver como crecen las provincias ------------
zz_prov <- df %>% select(year, ine_prov, ine_prov.n, ine_ccaa.n, pob_prov, pob_prov_perc, pob_prov_perc_acu, pob_esp, pob_esp_perc, pob_esp_perc_acu) %>%  distinct() %>% group_by(ine_prov) %>% arrange(year) %>% 
  mutate(pob_prov_indice = pob_prov / first(pob_prov) *100) %>% ungroup()

zz_prov_2020 <- zz_prov %>% filter(year == 2020)

zz_esp <- zz_prov %>% select(year, pob_esp, pob_esp_perc, pob_esp_perc_acu) %>% distinct() %>% arrange(year) %>% 
  mutate(esp_indice = pob_esp / first(pob_esp) *100) %>% ungroup()


ggplot() + geom_point(data = zz_prov, aes(x = year, y = pob_prov_indice,color = ine_prov.n)) +
  geom_line(data = zz_prov, aes(x = year, y = pob_prov_indice, group = ine_prov.n, color = ine_ccaa.n)) +
  geom_line(data = zz_esp, aes(x = year, y = esp_indice), size = 2) +
  geom_label(data = zz_prov_2020, aes(x = year, y = pob_prov_indice,label = ine_prov.n)) +
  theme(legend.position = "none") 
  



