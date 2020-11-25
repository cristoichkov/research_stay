library(dplyr)
library(readr)
library(raster)
library(formattable)

source("export_formattable.R")

bio_08_crop <- raster("../out/maxent/crop_bio_maps/bio08.asc")

M_alb_haag <- read_csv("../out/files/BD_haageana_albilanata_geoformas.csv")

n_obs_per_sam <- data.frame()

for (i in levels(factor(M_alb_haag$env_group_3))) {
  
  ### filtering and generating point map
  balsas <- M_alb_haag %>%
    dplyr::filter(env_group_3 == i) %>%
    dplyr::select(Lat, Long, env_group_3)
  
  mypuntos <- SpatialPoints(balsas[,c("Long", "Lat")])
  mypuntos <- SpatialPointsDataFrame(mypuntos, balsas)
  
  ### modify raster file and generating a cell uniquely
  bio_08_crop[] <- c(1:ncell(bio_08_crop))
  
  ### extract the values of my points in the new raster file
  Idcelda <- extract(bio_08_crop, mypuntos)
  
  ### combine database with cell id
  NuevaBase <- cbind(balsas, Idcelda)
  
  ###  unique Idcelda rows
  BaseUnicos <- NuevaBase %>%
    distinct(Idcelda, .keep_all = TRUE) %>%
    dplyr::select(env_group_3, Long, Lat) %>%
    rename(name = env_group_3, longitude = Long, latitude = Lat)
  
  ### save database
  write.csv(BaseUnicos, file = paste0("../out/maxent/db_csv/", i), row.names = FALSE)
  
  n_obs_per_sam <- rbind(n_obs_per_sam, BaseUnicos)
  
}

n_obs_per_sam_1 <- n_obs_per_sam %>%
  group_by() %>%
  count(env_group_3) %>%
  rename(occ_record = n)

make_italic <- formatter("span", style =  "font-style:italic")

n_obs_per_sam_1 <- data.frame(n_obs_per_sam_1)

rownames(n_obs_per_sam_1) <- n_obs_per_sam_1$env_group_3

n_obs_per_sam_1 <- n_obs_per_sam_1 %>%
  select(occ_record)

rownames(n_obs_per_sam_1)[1] <- make_italic("M. acultingensis")
rownames(n_obs_per_sam_1)[2] <- make_italic("M. albilanata")
rownames(n_obs_per_sam_1)[3] <- make_italic("M. haageana")
rownames(n_obs_per_sam_1)[5] <- make_italic("M. conspicua")
rownames(n_obs_per_sam_1)[6] <- make_italic("M. meissneri")
rownames(n_obs_per_sam_1)[7] <- make_italic("M. oaxacana")
rownames(n_obs_per_sam_1)[8] <- make_italic("M. reppenhagenii")
rownames(n_obs_per_sam_1)[9] <- make_italic("M. san-angelensis")
rownames(n_obs_per_sam_1)[10] <- "Sto. Ton. Dom."

n_obs_per_sam_tab <- formattable(n_obs_per_sam_1, align = "c")

export_formattable(n_obs_per_sam_tab, file = "../out/maxent/n_occ_per_sample.png", width = 300)
