library(rasterVis)
library(raster)       
library(rgl)          
library(rgdal)
library(elevatr)
library(readr)
library(ggplot2)

## Cragar la base de datos de la distribucion de M. haageana y M. albilanata, con grupos geneticos
BD_M_haag_alb <- read_csv("../out/files/M_haag_alb_bios_pca_only_genetic.csv") %>%
  dplyr::select(Especie:Lat, BD, colecta, env_group_3)

######## Extraer datod de capas (bios)
## Convertir los datos de longitud y latitud a puntos espaciales
Datos <- SpatialPoints(BD_M_haag_alb[,c("Long", "Lat")])
DB_mypoints <- SpatialPointsDataFrame(Datos, BD_M_haag_alb)

Mex_map <- shapefile("../data/Mexico_map/dest2018gw.shp")

## Cortar capa Elevacion 
Mex_crop <- crop(Mex_map, c(-104.5,-92, 15.5, 19.8)) 

elevation <- get_elev_raster(Mex_crop, z = 12)

## save crop bios layers 
writeRaster(elevation, filename = "../data/topograficas/Elevacion_12", format = "raster")

plot(elevation)
points(DB_mypoints)

## Extraer valores de elevacion
valores <- data.frame(elev = (extract(elevation, DB_mypoints)))


BD_M_haag_alb <- cbind(BD_M_haag_alb, valores)

BD_M_haag_alb %>%
  ggplot(aes(x = env_group_3, y = elev, color = env_group_3)) +
  geom_boxplot()
  