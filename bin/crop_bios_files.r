library(raster)

## list bios layer 
bios_files <- list.files("../data/58var/", pattern = "bio")

## filter filter bios layers to model
bios_files_mod <- bios_files[c(8, 13, 7, 3, 4, 14, 18, 19)]


for (i in bios_files_mod) {
  
  ## load bios layers
  bio_file <- raster(paste0("../data/58var/", i))
  
  ## crop bios layers with coordinates
  map_bio_clip <- crop(bio_file, c(-104.5,-91, 15.5, 19.8)) 
  
  crs(map_bio_clip) <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  ## save crop bios layers 
  writeRaster(map_bio_clip, filename = paste0("../out/maxent/crop_bio_maps/", i), format = "ascii")
  
}

