library(raster)
library(ggplot2)
library(ggsn)
library(sf)
library(dplyr)

source("create_scale_bar.R")
source("create_orientation_arrow.R")
source("scale_bar.R")

dir_wallace <- list.files("../out/maxent/wallace_out/")

maps_wallace_sdm <- data.frame()

for (i in dir_wallace) {
  
  asc_file <- list.files(paste0("../out/maxent/wallace_out/", i,"/"), pattern = ".asc")
  
  map_wallace <- raster(paste0("../out/maxent/wallace_out/", i, "/", asc_file))
  
  map_wallace.p <- raster::rasterToPoints(map_wallace)
  
  map_wallace_df <- data.frame(map_wallace.p)
  
  colnames(map_wallace_df) <- c("Longitude", "Latitude", "VAL")
  
  map_wallace_df <- map_wallace_df %>%
    filter(VAL == 1) %>%
    mutate(MAP = rep(i, nrow(.))) %>%
    dplyr::select(Longitude, Latitude, MAP)
  
  maps_wallace_sdm <- rbind(maps_wallace_sdm, map_wallace_df)
}



## read .shp file, Mexico map
mex <- st_read("../data/Mexico_map/dest2018gw.shp")

## Rename states 
levels(mex$NOM_ENT) <- gsub("Michoacán de Ocampo", "Michoacán", levels(mex$NOM_ENT))
levels(mex$NOM_ENT) <- gsub("Veracruz de Ignacio de la Llave", "Veracruz", levels(mex$NOM_ENT))

## centroids to put states labels
states <- cbind(mex, st_coordinates(st_centroid(mex)))

states_df <- as(st_geometry(states), "Spatial")


pallete_clades <- c(Huauclilla = "#4bb193", M_albilanata = "#6cb64d", M_conspicua = "#7265ce", M_haageana = "#d5498e",
                    M_meissneri = "#d9622f", M_oaxacana = "#c892ce", M_san_angelensis = "#94538c", San_Dom_Ton = "#a54a5b",
                    Tehuantepec = "#b047b7")

ppp <- ggplot() +
  geom_raster(data = filter(maps_wallace_sdm, MAP == "M_conspicua"), aes(y=Latitude, x=Longitude, fill= factor(MAP))) +
  scale_fill_manual(name = "", values = alpha(pallete_clades, 0.5)) +
  coord_sf(xlim = c(-99.7, -92), ylim = c(15.5, 19.8), expand = FALSE) +
  geom_text(data = states, aes(X, Y, label = NOM_ENT), colour = "grey29", size = 5) +
  geom_polygon(data=states_df, aes(x=long, y=lat, group=group), inherit.aes=F, colour='grey29', fill=NA, lwd=0.5) +
  scale_bar(lon = -99, lat = 15.8, 
            distance_lon = 45, distance_lat = 15, distance_legend = 25, 
            dist_unit = "km", orientation = FALSE, legend_size = 5) +
  north(x.min = -93.5, x.max = -94.5, y.min = 19, y.max = 19.3, scale = 1.2) +
  labs(y = "Latitude", x = "Longitude") +
  theme(axis.text.x = element_text(face="bold", size=12, colour = "gray16"), 
        axis.text.y = element_text(face="bold", size=12, colour = "gray16")) +
  theme(axis.title.y = element_text(face="bold", size = rel(1.5), angle = 90, colour = "grey32")) +
  theme(axis.title.x = element_text(face="bold", size = rel(1.5), angle = 00, colour = "grey32"))  

ppp_2 <- ppp + geom_raster(data = filter(maps_wallace_sdm, MAP == "M_meissneri"), aes(y=Latitude, x=Longitude, fill= factor(MAP))) +
  scale_fill_manual(name = "", values = alpha(pallete_clades, 0.5)) 

ppp_3 <- ppp_2 + geom_raster(data = filter(maps_wallace_sdm, MAP == "M_haageana"), aes(y=Latitude, x=Longitude, fill= factor(MAP))) +
  scale_fill_manual(name = "", values = alpha(pallete_clades, 0.5)) 

ppp_4 <- ppp_3 + geom_raster(data = filter(maps_wallace_sdm, MAP == "M_oaxacana"), aes(y=Latitude, x=Longitude, fill= factor(MAP))) +
  scale_fill_manual(name = "", values = alpha(pallete_clades, 0.5), 
                    labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                               Albilanata = expression(italic("M. albilanata")),
                               M_conspicua = expression(italic("M. conspicua")),
                               M_meissneri = expression(italic("M. meissneri")),
                               M_haageana = expression(italic("M. haageana")),
                               Huauclilla = "Huauclilla",
                               M_oaxacana= expression(italic("M. oaxacana")),
                               Reppenhagenii = expression(italic("M. reppenhagenii")),
                               M_san_angelensis = expression(italic("M. san-angelensis")),
                               Sto_domingo = "Sto. Dom. Ton.",
                               Tehuantepec = "Tehuantepec")) +
  theme(legend.text = element_text(size = 20),
        legend.text.align = 0,
        text = element_text(size = 12))

ggsave(ppp_4, file="../out/maxent/out_files/map_sdm_cons_haag_meis_oax.png", device="png", dpi = 300, width = 24, height = 14)

ppp_5 <- ppp_4 + geom_raster(data = filter(maps_wallace_sdm, MAP == "M_san_angelensis"), aes(y=Latitude, x=Longitude, fill= factor(MAP))) +
  scale_fill_manual(name = "", values = alpha(pallete_clades, 0.5), 
                    labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                               Albilanata = expression(italic("M. albilanata")),
                               M_conspicua = expression(italic("M. conspicua")),
                               M_meissneri = expression(italic("M. meissneri")),
                               M_haageana = expression(italic("M. haageana")),
                               Huauclilla = "Huauclilla",
                               M_oaxacana= expression(italic("M. oaxacana")),
                               Reppenhagenii = expression(italic("M. reppenhagenii")),
                               M_san_angelensis = expression(italic("M. san-angelensis")),
                               Sto_domingo = "Sto. Dom. Ton.",
                               Tehuantepec = "Tehuantepec")) +
  theme(legend.text = element_text(size = 20),
        legend.text.align = 0,
        text = element_text(size = 12))

ggsave(ppp_5, file="../out/maxent/out_files/map_sdm_cons_haag_meis_oax_angel.png", device="png", dpi = 300, width = 24, height = 14)

ppp_6 <- ppp_4 + geom_raster(data = filter(maps_wallace_sdm, MAP == "M_albilanata"), aes(y=Latitude, x=Longitude, fill= factor(MAP))) +
  scale_fill_manual(name = "", values = alpha(pallete_clades, 0.5), 
                    labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                               M_albilanata = expression(italic("M. albilanata")),
                               M_conspicua = expression(italic("M. conspicua")),
                               M_meissneri = expression(italic("M. meissneri")),
                               M_haageana = expression(italic("M. haageana")),
                               Huauclilla = "Huauclilla",
                               M_oaxacana= expression(italic("M. oaxacana")),
                               Reppenhagenii = expression(italic("M. reppenhagenii")),
                               M_san_angelensis = expression(italic("M. san-angelensis")),
                               Sto_domingo = "Sto. Dom. Ton.",
                               Tehuantepec = "Tehuantepec")) +
  theme(legend.text = element_text(size = 20),
        legend.text.align = 0,
        text = element_text(size = 12))

ggsave(ppp_6, file="../out/maxent/out_files/map_sdm_cons_haag_meis_oax_albil.png", device="png", dpi = 300, width = 24, height = 14)

ppp_7 <- ppp_4 + geom_raster(data = filter(maps_wallace_sdm, MAP == "Tehuantepec"), aes(y=Latitude, x=Longitude, fill= factor(MAP))) +
  scale_fill_manual(name = "", values = alpha(pallete_clades, 0.5), 
                    labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                               M_albilanata = expression(italic("M. albilanata")),
                               M_conspicua = expression(italic("M. conspicua")),
                               M_meissneri = expression(italic("M. meissneri")),
                               M_haageana = expression(italic("M. haageana")),
                               Huauclilla = "Huauclilla",
                               M_oaxacana= expression(italic("M. oaxacana")),
                               Reppenhagenii = expression(italic("M. reppenhagenii")),
                               M_san_angelensis = expression(italic("M. san-angelensis")),
                               Sto_domingo = "Sto. Dom. Ton.",
                               Tehuantepec = "Tehuantepec")) +
  theme(legend.text = element_text(size = 20),
        legend.text.align = 0,
        text = element_text(size = 12))

ggsave(ppp_7, file="../out/maxent/out_files/map_sdm_cons_haag_meis_oax_tehuan.png", device="png", dpi = 300, width = 24, height = 14)


ppp_8 <- ppp_4 + geom_raster(data = filter(maps_wallace_sdm, MAP == "Huauclilla"), aes(y=Latitude, x=Longitude, fill= factor(MAP))) +
  scale_fill_manual(name = "", values = alpha(pallete_clades, 0.5), 
                    labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                               M_albilanata = expression(italic("M. albilanata")),
                               M_conspicua = expression(italic("M. conspicua")),
                               M_meissneri = expression(italic("M. meissneri")),
                               M_haageana = expression(italic("M. haageana")),
                               Huauclilla = "Huauclilla",
                               M_oaxacana= expression(italic("M. oaxacana")),
                               Reppenhagenii = expression(italic("M. reppenhagenii")),
                               M_san_angelensis = expression(italic("M. san-angelensis")),
                               Sto_domingo = "Sto. Dom. Ton.",
                               Tehuantepec = "Tehuantepec")) +
  theme(legend.text = element_text(size = 20),
        legend.text.align = 0,
        text = element_text(size = 12))

ggsave(ppp_8, file="../out/maxent/out_files/map_sdm_cons_haag_meis_oax_huau.png", device="png", dpi = 300, width = 24, height = 14)


ppp_9 <- ppp_4 + geom_raster(data = filter(maps_wallace_sdm, MAP == "San_Dom_Ton"), aes(y=Latitude, x=Longitude, fill= factor(MAP))) +
  scale_fill_manual(name = "", values = alpha(pallete_clades, 0.5), 
                    labels = c(Acultingensis = expression(italic("M. acultzingensis")),
                               M_albilanata = expression(italic("M. albilanata")),
                               M_conspicua = expression(italic("M. conspicua")),
                               M_meissneri = expression(italic("M. meissneri")),
                               M_haageana = expression(italic("M. haageana")),
                               Huauclilla = "Huauclilla",
                               M_oaxacana= expression(italic("M. oaxacana")),
                               Reppenhagenii = expression(italic("M. reppenhagenii")),
                               M_san_angelensis = expression(italic("M. san-angelensis")),
                               Sto_domingo = "Sto. Dom. Ton.",
                               Tehuantepec = "Tehuantepec")) +
  theme(legend.text = element_text(size = 20),
        legend.text.align = 0,
        text = element_text(size = 12))

ggsave(ppp_9, file="../out/maxent/out_files/map_sdm_cons_haag_meis_oax_santodom.png", device="png", dpi = 300, width = 24, height = 14)
