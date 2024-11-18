#17.11.2024; GC
#NOT SURE WE HAVE THE CORRECT LIFE VERSION
library(httr)
library(jsonlite)
library(terra)
library(stringr)

#Read in dependencies ####
base_raster <- rast("rasters/world_base_moll_1km.tif")

#Read in LIFE (not sure if this is the latest version)
LIFE_path <- "rasters/Eyres_et_al_LIFE-2024/Eyres_et_al_LIFE/"
LIFE <- rast(paste0(LIFE_path, "restore_0.25.tif"))

#sum value for amphibians, birds, mammals and reptiles to get LIFE score 
#across taxa 
LIFE$score <- app(LIFE, sum)
plot(LIFE)

# Reproject LIFE to match the resolution and CRS of base raster
LIFE_r<- project(LIFE, base_raster)

#save output ####

# Define the output file path
output_path <- "rasters/LIFE_moll_1km.tif"

# Save the reprojected raster
writeRaster(LIFE_r, output_path, overwrite = TRUE)


