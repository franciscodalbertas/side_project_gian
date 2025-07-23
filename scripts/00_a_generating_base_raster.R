# --- load packages ---

library(terra)

# ---------------------

#  use life as base

LIFE_path <- "rasters/Eyres_et_al_2025/" 
LIFE <- rast(paste0(LIFE_path, "scaled_restore_0.25.tif"))
LIFE <- LIFE[[1]]

# no crop the extent to oc data

opp_cost <- rast("rasters/02_opportunity_cost.tif")

LIFE_m <- crop(LIFE, ext(opp_cost))

base_r <- terra::ifel(is.na(LIFE_m), NA, 1)

plot(base_r)
# save base raster

writeRaster(base_r, "rasters/base_raster.tif", overwrite = TRUE)  
