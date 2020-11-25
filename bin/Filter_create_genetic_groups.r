library(sf)
library(ggplot2)
library(ggsn)  
library(dplyr)
library(tibble)
library(ggrepel)
library(tidyr)

source("create_scale_bar.R")
source("create_orientation_arrow.R")
source("scale_bar.R")



M_haag_alb_bios_pca <- read.csv("../out/files/BD_M_haageana_albilanata_bios_PCA.cvs")

M_haag_alb_bios_pca <- M_haag_alb_bios_pca %>%
  rownames_to_column() %>%
  unite(ID, Long, Lat, sep = "_")

compare <- read.csv("../meta/Base_datos_albilanata_haageana.csv")

compare <- compare %>%
  select(Lat, Long, prub) %>%
  rownames_to_column() %>%
  unite(ID, Long, Lat, sep = "_")


M_haag_alb_bios_pca_env_group <- left_join(M_haag_alb_bios_pca, compare, by = "ID") %>%
  dplyr::select(Especie:X19, prub) %>%
  rownames_to_column() %>%
  separate(ID, c("Long", "Lat"), sep = "_") %>%
  mutate(Long = as.numeric(Long), Lat = as.numeric(Lat), prub = as.character(prub)) %>%
  replace_na(list(prub = "unknown"))


M_haag_alb_bios_pca_env_group <- M_haag_alb_bios_pca_env_group %>%
  mutate(env_group = ifelse(prub == "unknown" & Long > -97.8 & Long < -96.6 & Lat < 18, "Oaxacana",
                            ifelse(prub == "Acajete", "Acajete",
                                   ifelse(prub == "Acultingensis", "Acultingensis",
                                          ifelse(prub == "Albilanata", "Albilanata",
                                                 ifelse(prub == "Balsas", "Balsas",
                                                        ifelse(prub == "Cuicatlan", "Cuicatlan",
                                                               ifelse(prub == "Haageana", "Haageana",
                                                                      ifelse(prub == "Huauclilla", "Huauclilla",
                                                                             ifelse(prub == "Mixe", "Mixe",
                                                                                    ifelse(prub == "Oaxacana", "Oaxacana",
                                                                                           ifelse(prub == "Puebla", "Puebla",
                                                                                                  ifelse(prub == "Reppenhagenii", "Reppenhagenii",
                                                                                                         ifelse(prub == "San_angelensis", "San_angelensis",
                                                                                                                ifelse(prub == "Sto_domingo", "Sto_domingo",
                                                                                                                       ifelse(prub == "Tegelbergiana", "Tegelbergiana",
                                                                                                                              ifelse(prub == "Tehuantepec", "Tehuantepec",
                                                                                                                                     ifelse(prub == "unknown" & Long > -97.8 & Long < -96.6 & Lat > 19, "Haageana", 
                                                                                                                                            ifelse(prub == "unknown" & Long > -99.5 & Long < -99 & Lat > 18.4, "San_angelensis",
                                                                                                                                                   ifelse(prub == "unknown" & Long > -98.4 & Long < -97 & Lat > 18.6, "Puebla",
                                                                                                                                                          ifelse(prub == "unknown" & Long < -102, "Reppenhagenii", 
                                                                                                                                                                 ifelse(prub == "unknown" & Lat < 17, "Tehuantepec",
                                                                                                                                                                        ifelse(prub == "unknown" & Long < -98, "Albilanata",
                                                                                                                                                                               ifelse(prub == "unknown" & X1 > 5, "Cuicatlan",
                                                                                                                                                                                      ifelse(prub == "unknown" & X1 < -2.5, "Oaxacana",
                                                                                                                                                                                             ifelse(prub == "unknown" & rowname == 511, "Huauclilla", "Balsas"))))))))))))))))))))))))))

M_haag_alb_bios_pca_env_group <- M_haag_alb_bios_pca_env_group %>%
  mutate(env_group_2 = ifelse(env_group == "Acajete", "Acajete",
                              ifelse(env_group == "Acultingensis", "Acultingensis",
                                     ifelse(env_group == "Albilanata", "Albilanata",
                                            ifelse(env_group == "Balsas", "Balsas",
                                                   ifelse(env_group == "Cuicatlan", "Cuicatlan",
                                                          ifelse(env_group == "Haageana", "Haageana",
                                                                 ifelse(env_group == "Huauclilla" & Lat > 18, "Balsas",
                                                                        ifelse(env_group == "Mixe", "Mixe",
                                                                               ifelse(env_group == "Oaxacana" & Lat > 18, "Balsas",
                                                                                      ifelse(env_group == "Puebla", "Puebla",
                                                                                             ifelse(env_group == "Reppenhagenii", "Reppenhagenii",
                                                                                                    ifelse(env_group == "San_angelensis", "San_angelensis",
                                                                                                           ifelse(env_group == "Sto_domingo", "Sto_domingo",
                                                                                                                  ifelse(env_group == "Tegelbergiana", "Tegelbergiana",
                                                                                                                         ifelse(env_group == "Tehuantepec", "Tehuantepec",
                                                                                                                                ifelse(env_group == "Huauclilla", "Huauclilla", 
                                                                                                                                       ifelse(env_group == "Oaxacana", "Oaxacana", "")))))))))))))))))) 
                              



M_haag_alb_bios_pca_genetic <-
  M_haag_alb_bios_pca_env_group %>% 
  filter(env_group != "Acajete" & env_group != "Cuicatlan" & env_group != "Mixe" & env_group != "Puebla" &
           env_group != "Tegelbergiana")


M_haag_alb_bios_pca_genetic <- M_haag_alb_bios_pca_genetic %>%
  mutate(env_group_3 = ifelse(rowname %in% c(511), "M_conspicua",
                              ifelse(rowname %in% c(9, 86, 78), "M_meissneri",
                                     ifelse(env_group_2 == "Acultingensis", "Acultingensis",
                                            ifelse(env_group_2 == "Albilanata", "Albilanata",
                                                   ifelse(env_group_2 == "Oaxacana", "Oaxacana",
                                                          ifelse(env_group_2 == "Haageana", "Haageana",
                                                                 ifelse(env_group_2 == "Reppenhagenii", "Reppenhagenii",
                                                                        ifelse(env_group_2 == "San_angelensis", "San_angelensis",
                                                                               ifelse(rowname %in% c(172, 181), "Oaxacana", 
                                                                                      ifelse(rowname == c(9, 511), "Huauclilla", 
                                                                                             ifelse(env_group_2 == "Tehuantepec", "Tehuantepec",
                                                                                                    ifelse(env_group_2 == "Huauclilla", "Huauclilla", 
                                                                                                           ifelse(env_group_2 == "Balsas" & X1 > 0.815 & X1 < 4 & X2 >0, "M_meissneri", 
                                                                                                                  ifelse(env_group_2 == "Balsas" &  X1 > -2.2 & X1 < 0.815 & X2 > 0, "M_conspicua",
                                                                                                                         ifelse(env_group_2 == "Balsas" & X1 < 0 & X2 <0, "M_conspicua",  
                                                                                                                                ifelse(env_group_2 == "Sto_domingo", "Sto_domingo", 
                                                                                                                                       ifelse(X1 < -2.5, "Oaxacana", 
                                                                                                                                              ifelse(X1 > 0 & X1 < 2.5 & X2 <0, "M_conspicua", "Sto_domingo")))))))))))))))))))
                                                                                                           
                                                                                                                         
                                                                        
                              
                                                   
                                                          
                                                                 
                                                                        
                                                                               
                                                                                     
                                                                                             
                                                                                                                 
                                                                                                                         

  

write.csv(M_haag_alb_bios_pca_genetic, file = "../out/files/M_haag_alb_bios_pca_only_genetic.csv", row.names = FALSE)

## read .shp file, Mexico map
mex <- st_read("../data/Mexico_map/dest2018gw.shp")

## Rename states 
levels(mex$NOM_ENT) <- gsub("Michoacán de Ocampo", "Michoacán", levels(mex$NOM_ENT))
levels(mex$NOM_ENT) <- gsub("Veracruz de Ignacio de la Llave", "Veracruz", levels(mex$NOM_ENT))

## centroids to put states labels
states <- cbind(mex, st_coordinates(st_centroid(mex)))

## plot of M. albilanata
## Mexico Map with states labels
pl_al <- ggplot(data = mex) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = NOM_ENT), size = 5)  +
  labs(y = "Latitude", x = "Longitude") +
  coord_sf(xlim = c(-104.5, -91), ylim = c(15.5, 19.8), expand = FALSE) +
  scale_bar(lon = -99.4, lat = 16.3, 
            distance_lon = 30, distance_lat = 10, distance_legend = 15, 
            dist_unit = "km", orientation = FALSE) +
  north(x.min = -95.3, x.max = -95.7, y.min = 19.1, y.max = 19.4, scale = 1) +
  theme(axis.text.x = element_text(face="bold", size=12, colour = "gray16"), 
        axis.text.y = element_text(face="bold", size=12, colour = "gray16")) +
  theme(axis.title.y = element_text(face="bold", size = rel(1.2), angle = 90, colour = "grey32")) +
  theme(axis.title.x = element_text(face="bold", size = rel(1.2), angle = 00, colour = "grey32")) 

pl_al +  
  geom_point(data = filter(M_haag_alb_bios_pca_genetic, env_group_3 == "Tehuantepec"), aes(x = Long, y = Lat), size = 3) +
  theme(legend.title = element_text(size = 17), legend.text = element_text(size=17), legend.position = "right", 
        legend.text.align = 0, text = element_text(size=18)) +
  geom_vline(xintercept = c(-98.4, -97.84)) +
  geom_hline(yintercept = 17.89) 



  ggplot(data = M_haag_alb_bios_pca_genetic, aes(x = X1, y = X2, color = env_group_3, shape = env_group_3, label = colecta)) +
  geom_point(size = 5) +
  geom_vline(xintercept = c(-2.2, 4)) +
  geom_hline(yintercept = 0) +
  scale_shape_manual(values = 1:19) +
  geom_text(color = "black", nudge_x = 0.1, nudge_y = 0.1)
  

  ppp <- ggplot(data = M_haag_alb_bios_pca_genetic, aes(x = X1, y = X2)) +
    geom_point(aes(color = env_group_3), size = 5) +
    geom_vline(xintercept = c(-2.2, 4)) +
    geom_hline(yintercept = 0) +
    scale_shape_manual(values = 0:19) 
    
ppp + geom_point(data = filter(M_haag_alb_bios_pca_genetic, BD == "gbs"), aes(x = X1, y = X2, shape = env_group_3)) +
  scale_shape_manual(values = 1:25)



## plot of M. albilanata
## Mexico Map with states labels
pl_al_balsas <- ggplot(data = mex) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = NOM_ENT), size = 5)  +
  labs(y = "Latitude", x = "Longitude") +
  coord_sf(xlim = c(-98.2, -97), ylim = c(17.5, 18.7), expand = FALSE) +
  theme(axis.text.x = element_text(face="bold", size=12, colour = "gray16"), 
        axis.text.y = element_text(face="bold", size=12, colour = "gray16")) +
  theme(axis.title.y = element_text(face="bold", size = rel(1.2), angle = 90, colour = "grey32")) +
  theme(axis.title.x = element_text(face="bold", size = rel(1.2), angle = 00, colour = "grey32")) 

pl_al_balsas +  
  geom_point(data = M_haag_alb_bios_pca_genetic, aes(x = Long, y = Lat, color = env_group_3, shape = env_group_3), size = 3) +
  theme(legend.title = element_text(size = 17), legend.text = element_text(size=17), legend.position = "right", 
        legend.text.align = 0, text = element_text(size=18)) +
  geom_vline(xintercept = c(-98.4, -97)) +
  geom_hline(yintercept = 18) +
  scale_shape_manual(values = 1:19) +
  geom_text(data = M_haag_alb_bios_pca_genetic, aes(x = Long, y = Lat, label = rowname))


pl_al_balsas +  
  geom_point(data = M_haag_alb_bios_pca_genetic, aes(x = Long, y = Lat, color = env_group_3, shape = env_group_3), size = 3) +
  theme(legend.title = element_text(size = 17), legend.text = element_text(size=17), legend.position = "right", 
        legend.text.align = 0, text = element_text(size=18)) +
  geom_vline(xintercept = c(-98.4, -97)) +
  geom_hline(yintercept = 18) +
  scale_shape_manual(values = 1:19) 







pl_al +  
  geom_point(data = M_haag_alb_bios_pca_genetic, aes(x = Long, y = Lat, color = env_group_2, shape = env_group_2), size = 3) +
  theme(legend.title = element_text(size = 17), legend.text = element_text(size=17), legend.position = "right", 
        legend.text.align = 0, text = element_text(size=18)) +
  geom_vline(xintercept = c(-98.4, -97.84)) +
  geom_hline(yintercept = 17.89) +
  scale_shape_manual(values = 1:19) 


ggplot(data = M_haag_alb_bios_pca_genetic, aes(x = X1, y = X2, color = env_group_3, shape = env_group_3, label = rowname)) +
  geom_point(size = 5) +
  geom_vline(xintercept = c(-2.2, 4)) +
  geom_hline(yintercept = 0) +
  scale_shape_manual(values = 1:19) +
  geom_text(color = "black", nudge_x = 0.1, nudge_y = 0.1, size = 3)
