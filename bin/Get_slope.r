library(raster)
library(readr)
library(dplyr)
library(ggplot2)

## Cragar la base de datos de la distribucion de M. haageana y M. albilanata, con grupos geneticos
BD_M_haag_alb <- read_csv("../out/files/M_haag_alb_bios_pca_only_genetic.csv") %>%
  dplyr::select(Especie:Lat, BD, colecta, env_group_3)

######## Extraer datod de capas (bios)
## Convertir los datos de longitud y latitud a puntos espaciales
Datos <- SpatialPoints(BD_M_haag_alb[,c("Long", "Lat")])
DB_mypoints <- SpatialPointsDataFrame(Datos, BD_M_haag_alb)

## Cargar capa de elevacion de gtopo30ln
Slope <- raster("../data/topograficas/Slope.asc")

## Cortar capa Elevacion 
map_slope_crop <- crop(Slope, c(-104.5,-92, 15.5, 19.8)) 

## Extraer valores de elevacion
valores <- data.frame(slope = (raster::extract(map_slope_crop, DB_mypoints)))

BD_M_haag_alb <- cbind(BD_M_haag_alb, valores)

BD_M_haag_alb %>%
  ggplot(aes(x = env_group_3, y = slope, color = env_group_3)) +
  geom_boxplot()

