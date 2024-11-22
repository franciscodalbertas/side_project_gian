#17.11.2024; GC
#NOT SURE WE HAVE THE CORRECT LIFE VERSION
library(httr)
library(jsonlite)
library(terra)
library(stringr)

#Read in dependencies ####
base_raster <- rast("rasters/world_base_moll_1km.tif")

#Read in LIFE (not sure if this is the latest version)
#LIFE_path <- "rasters/Eyres_et_al_LIFE-2024/Eyres_et_al_LIFE/" #old version 
LIFE_path <- "rasters/deltap_final/deltap_final/" #old version 

LIFE <- rast(paste0(LIFE_path, "summed_scaled_restore_0.25.tif"))

# Ensure LIFE matches the extent of base_raster by cropping it
#LIFE_cropped <- crop(LIFE, ext(base_raster))

# Reproject LIFE to match base_raster's CRS, extent, and resolution
LIFE_r <- project(LIFE_cropped, base_raster, method = "bilinear") 

# Check the output
print(LIFE_r)
plot(LIFE_r)

# Save the reprojected raster if needed
output_path <- "rasters/LIFE_moll_1km.tif"

# Save the reprojected raster
writeRaster(LIFE_r, output_path, overwrite = TRUE)


