library(rgbif)
library(dplyr)
library(tidyr)
library(readr)

######## Obtener Informacion de Gbif ##########

## Mammillaria haageana y M. albilanata
Mamm_haag_alb_df <- data.frame()

for (i in c("albilanata", "haageana")) {
  
  ## Buscar infomacion para cada especie de la serie supertextae
  sp_super_gbif <- occ_search(scientificName = paste("Mammillaria", i),  limit=15000) 
  
  ## Obtner la base de datos 
  df_sp_super_gbif <- sp_super_gbif$data
  
  ## depuracion de la base de datos
  df_sp_super_gbif <- df_sp_super_gbif  %>%
    dplyr::filter(decimalLatitude != "NA" & basisOfRecord == "HUMAN_OBSERVATION" & 
                    identifiedBy != "NO DISPONIBLE" & identifiedBy != "NA") %>%
    distinct_at(vars(decimalLatitude, decimalLongitude), .keep_all = TRUE) %>%
    select(scientificName, decimalLatitude, decimalLongitude, taxonRank, taxonomicStatus, dateIdentified,
           coordinateUncertaintyInMeters, stateProvince, year, month, day, eventDate, references, gadm.level2.name,
           rightsHolder, verbatimLocality, recordedBy, identifiedBy) 
  
  ## almacenar resultados para cada erspecie
  Mamm_haag_alb_df <- bind_rows(Mamm_haag_alb_df, df_sp_super_gbif)
  
}

## Eliminar registros de las siguintes personas (no confiables)
recordedBy_delete <- c("mavadesa75", "danha_2010", "macortes", "Daniel Dorantes", "josuedeleonlux")

## seprarar la columna scientificName 
BD_gbif <- Mamm_haag_alb_df %>% 
  mutate(BD = rep("gbif", times = nrow(Mamm_haag_alb_df))) %>%
  separate(scientificName, c("genero", "especie", "ssp", "subsp")) %>%
  filter(identifiedBy != "ASAM; GIMM" & decimalLatitude > 0 & !recordedBy %in% recordedBy_delete) %>%
  rename(Especie = especie, Lat = decimalLatitude, Long = decimalLongitude) %>%
  select(Especie, Lat, Long, BD)


####### Base de datos herbarios #######
BD_herbarium <- read.csv("../meta/BD_albilanata_haageana_herbarios.csv")

BD_herbarium <- BD_herbarium %>%
  select(Colector, NumeroColecta, Especie, CatInfraespecifica, NomInfraespecifico, Estado, Municipio, DescripLocalidad, 
         AltitudMinima, AltitudMaxima, LatGrados, LatMinutos, LatSegundos, LongGrados, LongMinutos, LongSegundos,
         OtrosColectores, DiaColecta, MesColecta, AñoColecta, Herbario, DeterminoFecha, Vegetacion) %>%
  mutate(Lat = (LatGrados + (LatMinutos/60) + (LatSegundos/3600)), Long = (LongGrados + (LongMinutos/60) + (LongSegundos/3600)) * -1,
         BD = rep("herbarios", times = nrow(BD_herbarium))) %>%
  select(Especie, Lat, Long, BD)


####### colectas de Cristian Cervantes #######
BD_gbs <- readr::read_csv("../meta/BD_colectas_CC.csv", 
                          col_names = c("Id", "colecta", "clado", "sp", "clave",
                                        "decimalLatitude", "decimalLongitude"))

BD_gbs <- BD_gbs %>%
  rename(Especie = sp, Lat = decimalLatitude, Long = decimalLongitude) %>%
  mutate(BD = rep("gbs", times = nrow(BD_gbs))) %>%
  select(Especie, Lat, Long, BD, colecta) %>%
  filter(!Especie %in% c("crucigera", "dixanthocentron", "flavicentra", "huitzilopochtli", "lanata", "out", "supertexta"))



###### Base de datos de la coleccion de cactaceas ########
basecactus <- read.csv("../meta/BD_coleccion_cactaceas.csv")

BD_col_cactaceas <- basecactus %>%
  select(Colector, NumeroColecta, Genero, Especie, Localidad, AltitudMinima, AltitudMaxima, LatGrados, LatMinutos,
         LatSegundos, LongGrados, LongMinutos, LongSegundos, OtrosColectores, DiaColecta, MesColecta, AnoColecta, Herbario,
         DeterminoFecha) %>%
  filter(Especie %in% c("haageana", "albilanata") & Localidad != "" & NumeroColecta != "s.n." & LatMinutos != 0) %>%
  mutate(Lat = (LatGrados + (LatMinutos/60) + (LatSegundos/3600)), Long = (LongGrados + (LongMinutos/60) + (LongSegundos/3600)) * -1) %>%
  mutate(BD = rep("col_cactus", times = nrow(.))) %>%
  select(Especie, Lat, Long, BD)


######## Unir todas las bases de datos ##########
BD_coords_final <- dplyr::bind_rows(BD_gbif, BD_herbarium, BD_gbs, BD_col_cactaceas) 

BD_coords_final$Especie <- recode(BD_coords_final$Especie, collina = "haageana")

write.csv(BD_coords_final, file = "../out/files/Base_datos_coordenadas_all.csv", row.names = FALSE)
