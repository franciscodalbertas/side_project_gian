# library(httr)
# library(jsonlite)
library(terra)
# library(stringr)

#downloaded from https://zenodo.org/records/14188450

#Read in dependencies 
base_raster <- rast("rasters/base_raster.tif")

#Read in LIFE (not sure if this is the latest version)
LIFE_path <- "rasters/Eyres_et_al_2025/" 
LIFE <- rast(paste0(LIFE_path, "scaled_restore_0.25.tif"))
LIFE <- LIFE[[1]]

#Read in the area that could be returned to native cover 
option_space <- rast(paste0(LIFE_path, "revert_to_natural_diff_area.tif"))


# Compute min and max for the raster
#min_max <- global(LIFE, fun = range, na.rm = TRUE)
#print(min_max)
# -0.9067187 0.1266626
# min_max <- global(option_space, fun = range, na.rm = TRUE)
# print(min_max) 
# 0, 3419187 - likely in m2
#LIFE_cropped <- crop(LIFE, ext(-179, 179, -90, 90))

# Reproject LIFE to match the resolution and CRS of base raster
LIFE_r<- project(LIFE, base_raster,"bilinear")
plot(LIFE_r)

#calculate area restorable at 1km2 by assuming that restorable areas at 3.14km are 
#distributed evenly in 1km2 
option_space_r <- project(option_space, base_raster,"bilinear")
min_max <- global(option_space_r, fun = range, na.rm = TRUE)
# this calcualtes fraction of the pixel covered. Dont think its what we need
# we just need area in km2
option_space_1km <- (option_space_r/3419187)* base_raster
option_space_km2 <- (option_space_r/10^6)* base_raster
min_max <- global(option_space_1km, fun = range, na.rm = TRUE)

#save output ####

# Save the reprojected raster
writeRaster(LIFE_r, "rasters/LIFE_latlong_1km.tif", overwrite = TRUE)
writeRaster(option_space_1km, "rasters/restorable_area_1km.tif", overwrite = TRUE)
writeRaster(option_space_km2, "rasters/restorable_area_km2.tif", overwrite = TRUE)
