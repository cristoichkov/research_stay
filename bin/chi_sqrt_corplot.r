library(raster)
library(readr)
library(dplyr)
library(ggplot2)
library(gplots)
library(tidyr)


## Cragar la base de datos de la distribucion de M. haageana y M. albilanata, con grupos geneticos
BD_M_haag_alb <- read_csv("../out/files/M_haag_alb_bios_pca_only_genetic.csv") %>%
  dplyr::select(Especie:Lat, BD, colecta, env_group_3)

write.csv(BD_M_haag_alb, file = "../out/files/BD_haageana_albilanata_geoformas.csv")

######## Extraer datod de capas (bios)
## Convertir los datos de longitud y latitud a puntos espaciales
Datos <- SpatialPoints(BD_M_haag_alb[,c("Long", "Lat")])
DB_mypoints <- SpatialPointsDataFrame(Datos, BD_M_haag_alb)

## Cargar capa de elevacion de gtopo30ln
fao <- shapefile("../data/FAO/DSMW.shp")

## Cortar capa Elevacion 
map_fao_crop <- crop(fao, c(-104.5,-92, 15.5, 19.8)) 

## Extraer valores de elevacion
valores <- data.frame(raster::extract(map_fao_crop, DB_mypoints))

BD_M_haag_alb <- cbind(BD_M_haag_alb, valores)

anch_format <- BD_M_haag_alb %>%
  group_by(env_group_3, DOMSOI) %>%
  count() %>%
  spread(DOMSOI, n) %>%
  replace_na(list(Bd = 0, Be = 0, Bk = 0, E = 0, Hh = 0, Lc = 0, Lo = 0, Nd = 0, Re = 0, Th = 0, To = 0, Tv = 0, Vp = 0, Ws = 0))


anch_format <- data.frame(anch_format)

rownames(anch_format) <- anch_format$env_group_3

anch_format <- anch_format[, -1]

anch_format <- as.table(as.matrix(anch_format))

# 2. Graph
balloonplot(t(anch_format), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)


chisq <- chisq.test(anch_format)
chisq
chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)

library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
