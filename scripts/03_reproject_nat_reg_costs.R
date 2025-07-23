# Load necessary library
# library(httr)
# library(jsonlite)
library(terra)
#library(stringr)
#-------------------------------------------------------------------------------
# source:
# https://zenodo.org/records/11372275
# folder
opp_cost <- rast("rasters/02_opportunity_cost.tif")

plot(opp_cost)

# adjust resol and proj
# read base raster (chico: went back to the mollweide)
#base_raster <- rast("rasters/nat_regeneration_cost_latlong_1km.tif")
base_raster <- rast("rasters/base_raster.tif")

# Reproject oppcost to match the resolution and CRS of base raster
opp_cost_r <- project(opp_cost, base_raster,"bilinear")

plot(opp_cost_r)

# Save the reprojected raster
writeRaster(opp_cost_r, "rasters/opp_cost_reprojected.tif", overwrite = TRUE)
#writeRaster(opp_cost_r, "rasters/opp_cost_reprojected_mollweide_1km.tif", overwrite = TRUE)
