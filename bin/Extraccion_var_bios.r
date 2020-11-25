library(sp)
library(raster)

## Cragar la base de datos de la distribucion de M. haageana y M. albilanata
BD_M_haag_alb <- read.csv("../out/files/Base_datos_coordenadas_all.csv")

######## Extraer datod de capas (bios)
## Convertir los datos de longitud y latitud a puntos espaciales
Datos <- SpatialPoints(BD_M_haag_alb[,c("Long", "Lat")])
DB_mypoints <- SpatialPointsDataFrame(Datos, BD_M_haag_alb)

## Visualizar la distribución de los puntos 
plot(DB_mypoints)

## Enlistar las 19 capas bioclimaticas
bios_files <- list.files("../data/58var/", pattern = "bio") 

## Realizar la extraccion de los valores de las 19 capas bioclimaticas para cada punto
for (i in 1:length(bios_files)) {
  
  bio_raster <- raster(paste0("../data/58var/", bios_files[i]))
  
  name <- stringr::str_extract(bios_files[i], pattern = "bio[0-9]+")
  
  valores <- data.frame(extract(bio_raster, DB_mypoints))
  
  colnames(valores) <- name
  
  BD_M_haag_alb <- cbind(BD_M_haag_alb, valores)
  
}

## Guardar el resultado
write.csv(BD_M_haag_alb, file = "../out/files/BD_M_haageana_albilanata_bios.cvs", row.names = FALSE)
