#- quiero hacer un mapa con el crecimiento de la población (por provincias) en el periodo 2000-2020
library(tidyverse)

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
  mutate(crec_habi_percent_acu = crec_habi_acu / first(habitantes)*100)
  
pob_prov_crec_wide <- pob_prov %>% 
  select(ine_prov, ine_prov.n, year, crec_habi_percent) %>% 
  pivot_wider(names_from = year, values_from = crec_habi_percent) %>% 
  select(-`2000`) %>% ungroup()

pob_prov_crec <- pob_prov %>% 
  select(ine_prov, ine_prov.n, year, crec_habi_percent) %>% 
  filter(year != 2000) %>% ungroup()


#- para discretizar:  cut_interval(x = crec_porcentual, n = 5))
#- para discretizar: https://www.rdocumentation.org/packages/arules/versions/1.6-6/topics/discretize
mutate(crec_porcentual.n = paste0(round(crec_porcentual, digits = 1), "%")) %>% 
  mutate(crec_porcentual_d = cut(crec_porcentual, 
                                 breaks=c(-Inf, 0.0, 10, pob_esp$crec_percent[6]*100, 30, Inf), 
                                 labels=c("Negativo","[ 0% - 10%)","[10% - media)", "[media, 30%)", " >30%")))

#- biblio
#- GUAU!! https://jbengler.github.io/tidyheatmaps/articles/tidyheatmap.html
#- https://rkabacoff.github.io/datavis/Other.html#heatmaps
#- https://www.royfrancis.com/a-guide-to-elegant-tiled-heatmaps-in-r-2019/
#- https://rlbarter.github.io/superheat/basic-usage.html


#- solo lo hago con geom_tile() y lo intento mejorar con Dominic Roye: https://twitter.com/dr_xeo/status/1034016258616950784   El repo: https://github.com/dominicroye/obesity
#- 2: con geom_tile() -----------------------------------------------------

zz <- pob_prov_crec %>% arrange(ine_ccaa, ine_prov.n)  %>% #- no funciona xq es texto, tiene q ser factor
  mutate(ine_prov.n.f = factor(ine_prov.n, levels = rev(sort(unique(ine_prov.n))))) %>% 
  # quiero hacer crec. por encima de esp
  # create a new variable from count
  mutate(crec_habi_percent.d = cut(crec_habi_percent , breaks = c(-4,-2, 0, 2, 4, 7),
                         labels=c("[-4 ; -2)","[-2 ;  0)", "[ 0 ;  2)", "[ 2 ;  4)", "[ 4 ;  6]" ))) %>% 
  mutate(crec_habi_percent.d=factor(as.character(crec_habi_percent.d),levels=rev(levels(crec_habi_percent.d))))
         
         
         
#- con variable continua
ggplot() + 
  geom_tile(data = zz, aes(x = year, y = ine_prov.n.f, fill = crec_habi_percent), 
            colour = "white", size = 0.15) +
  labs(title = "Evolución de la población provincial (2000-2020)",
       x = NULL,
       y = NULL,
       caption = "Datos de población del INE. Visualización: @pjpv4444", 
       fill = NULL) +
  scale_x_continuous(expand=c(0,0))+
  theme_grey(base_size=8) +
  theme(
    legend.text=element_text(face="bold"),     #bold font for legend text
    axis.ticks=element_line(size=0.4),     #set thickness of axis ticks
    plot.background=element_blank(),     #remove plot background
    panel.border=element_blank())    #remove plot border

#- mejorar el heatmap con lo de Domic Roye. Sus mejorar son:
#- 1. el theme
#- 2. La forma de agrupar en intervalos: br <- classIntervals(zz$crec_habi_percent,5,"kmeans")$brks

theme_heatmap <- theme_minimal()+
  theme(panel.grid=element_blank(),
        axis.text.y = element_text(color="white", size=rel(.9)),
        axis.text.x = element_text(color="white", size=rel(.8)),
        strip.switch.pad.grid=unit(-0.5,"lines"),
        strip.text = element_blank(),
        plot.background = element_rect(fill="gray20"),
        panel.background = element_rect(fill="gray20"),
        legend.title=element_blank(),
        plot.caption=element_text(colour="white"),
        plot.title = element_text(colour="white"),
        legend.text = element_text(colour="white", margin=margin(l = 5)),
        panel.border = element_rect(fill=NA,color="gray20", size=0.5,
                                    linetype="solid"))

library(classInt)
br <- classInt::classIntervals(zz$crec_habi_percent,5,"kmeans")$brks


library(viridis)
ggplot() + 
  geom_tile(data = zz, aes(x = year, y = ine_prov.n.f, fill = crec_habi_percent), 
            colour = "white", size = 0.15) +
  geom_tile(colour="grey20")+
  scale_fill_viridis(option="B",na.value="grey20",breaks=br)+
  #scale_x_date("",date_breaks = "2 year",date_labels = "%Y",expand=c(0,0))+
  labs(y="",caption="Dominic Royé (@dr_xeo) | Data: ourworldindata.org",
       title="Share of adults defined as obese (%) ordered by 2016\n[18+ years with body-mass index > 30]")+
  guides(fill=guide_colorbar(barheight=22,barwidth = 1))+
  theme_heatmap



#- my heatmap con variable dicotomica
#- con variable discreta (factor)
ggplot() + 
  geom_tile(data = zz, aes(x = year, y = ine_prov.n.f, fill = crec_habi_percent.d), 
            colour = "white", size = 0.15) +
  labs(title = "Evolución de la población provincial (2000-2020)",
       x = NULL,
       y = NULL,
       caption = "Datos de población del INE. Visualización: @pjpv4444", 
       fill = NULL) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_manual( values=c("cyan4", "cyan3", "#e6f598", "#f46d43", "#d53e4f")) +
  guides(fill = guide_legend(title="Crecimiento en %\n de la población"))+
  theme_grey(base_size=8) +
  theme(
    legend.text=element_text(face="bold"),     #bold font for legend text
     axis.ticks=element_line(size=0.4),     #set thickness of axis ticks
     plot.background=element_blank(),     #remove plot background
     panel.border=element_blank())        #remove plot border


#- ahora quiero solo 2 valores discretos (crece mas o menos que Spain)





#-3: con pkg superheat de Rebeca Carter -----------------------
#- https://rlbarter.github.io/superheat/basic-usage.html
#- sí, pero no lo acabe- acabe xq preferí las otras 2 opciones

zz <- pob_prov_crec_wide %>% select(-c(1,2)) %>% as.matrix() #- ha de ser una matriz

superheat::superheat(zz,
          # scale the matrix columns
          scale = TRUE)

#- el heatmap normal con geom_tile()
pob_prov_crec <- pob_prov %>% 
  select(ine_prov, ine_prov.n, year, crec_habi_percent) %>% 
  filter(year != 2000)

#- basic
p <- ggplot(pob_prov_crec, aes(x = year,y = ine_prov.n, fill = crec_habi_percent))+
  geom_tile()

#- tuneando
p <- ggplot(pob_prov_crec, aes(x = year,y = ine_prov.n, fill = crec_habi_percent)) +
  geom_tile(colour="white", size = 0.1) + #add border white colour of line thickness 0.25 
  labs(x = "", y = "") +                  #remove x and y axis labels
  scale_y_discrete(expand = c(0,0)) +      #remove extra space
  #scale_x_discrete(expand = c(0,0), breaks = as.character(c(2001:2020) )) + 
  #scale_x_discrete(expand = c(0,0), breaks = c("2005", "2010", "2015", "2020") ) +
  scale_x_discrete(expand = c(0,0), breaks = c(2005, 2010, 2015, 2020)) +
  theme_grey(base_size = 8) +  #set a base size for all fonts
  #theme options
  theme(
    #bold font for legend text
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank())


p <- ggplot(m3,aes(x=year,y=state,fill=count))+
  geom_tile()

zz <- pob_prov_crec %>% mutate(xx = ntile(crec_habi_percent, 8))


summarise(x = quantile(x, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))




#- LINKS para tablas ------------------------------
#- Links para tablas

@icymi_r: 📦📦 reactablefmtr • An R package to simplify formatting and customization of tables made with reactable

👤 Kyle Cuilla @kc_analytics

🔗 https://github.com/kcuilla/reactablefmtr
#rstats #datascience https://twitter.com/icymi_r/status/1361335073249914881/photo/1


- reacttable: https://www.infoworld.com/article/3543297/how-to-create-tables-in-r-with-expandable-rows.html

- reasctable good: https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/
  
  
  La tabla parecida a esta: <https://www.eldiario.es/sociedad/vacuna-covid-19-mapas-graficos-proceso-vacunacion-espana-mundo-febrero-10_1_6782953.html> y los gráficos tb son muy chulos


