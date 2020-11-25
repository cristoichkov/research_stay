library(raster)
library(readr)

## Cragar la base de datos de la distribucion de M. haageana y M. albilanata, con grupos geneticos
BD_M_haag_alb <- read_csv("../out/files/M_haag_alb_bios_pca_only_genetic.csv") %>%
  dplyr::select(Especie:Lat, BD, colecta, env_group_3)

######## Extraer datod de capas (bios)
## Convertir los datos de longitud y latitud a puntos espaciales
Datos <- SpatialPoints(BD_M_haag_alb[,c("Long", "Lat")])
DB_mypoints <- SpatialPointsDataFrame(Datos, BD_M_haag_alb)

elev <- raster("../data/topograficas/Elevacion_12.grd") 

## Extraer valores de elevacion
valores <- data.frame(elev = (extract(elev, DB_mypoints)))

BD_M_haag_alb <- cbind(BD_M_haag_alb, valores)

BD_M_haag_alb$env_group_3 <- factor(BD_M_haag_alb$env_group_3)

pallete_clades <- c(Huauclilla = "#4bb193", Acultingensis = "#9f5b2b", Albilanata = "#6cb64d", M_conspicua = "#7265ce", 
                   Haageana = "#d5498e", M_meissneri = "#d9622f", Oaxacana = "#c892ce", San_angelensis = "#94538c",
                   Sto_domingo = "#a54a5b", Tehuantepec = "#b047b7",  Reppenhagenii = "#e48283")

box_elev <- BD_M_haag_alb %>%
  ggplot(aes(x = env_group_3, y = elev, fill = env_group_3)) +
  geom_boxplot() +
  scale_fill_manual(name = "", values = alpha(pallete_clades, 0.5),
                    labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                                        Albilanata = expression(italic("M. albilanata")),
                                        M_conspicua = expression(italic("M. conspicua")),
                                        M_meissneri = expression(italic("M. meissneri")),
                                        Haageana = expression(italic("M. haageana")),
                                        Huauclilla = "Huauclilla",
                                        Oaxacana= expression(italic("M. oaxacana")),
                                        Reppenhagenii = expression(italic("M. reppenhagenii")),
                                        San_angelensis = expression(italic("M. san-angelensis")),
                                        Sto_domingo = "Sto. Dom. Ton.",
                                        Tehuantepec = "Tehuantepec")) +
  labs(y = "Elevation") +
  theme(axis.text.x = element_text(size=0), 
        axis.text.y = element_text(face="bold", size=15, colour = "gray16"),
        axis.title.y = element_text(face="bold", size = rel(1.45), angle = 90, colour = "grey32"),
        axis.title.x = element_text(size = 0),
        legend.text = element_text(size = 20),
        legend.text.align = 0)

ggsave(box_elev, file="../out/topography/Box_elev.png", device="png", dpi = 300, width = 20, height = 12)

