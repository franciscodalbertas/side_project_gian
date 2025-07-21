#reproject carbon 
library(terra)
#downloaded from 
#https://zenodo.org/records/15090826

#Raster description 
# max_rate
#- **Units**:  Mg C ha<sup>-1</sup> yr<sup>-1</sup>
#  - **Description**: This raster is the maximum annual rate derived from the champan richards curve parameters.

### max_removal_potential_benefit_25
#- **Units**:  %
#- **Description**: This raster represents the difference in potential accumulation between the first 25 years of growth, and the 25-year period of the maximum potential accumulation.

base_raster <- rast("rasters/nat_regeneration_cost_latlong_1km.tif")

#read in max rate
carbon <- rast("rasters/max_rate.tif")
carbon25 <- rast("rasters/max_removal_potential_benefit_25.tif")
plot(carbon)

#reproject to 
carbon_r <- project(carbon, base_raster,"bilinear")
carbon25_r <- project(carbon25, base_raster,"bilinear")

# Save the reprojected raster
writeRaster(carbon_r, "rasters/max_carbon_rate_reprojected.tif", overwrite = TRUE)
writeRaster(carbon25_r, "rasters/carbon_gain25_reprojected.tif", overwrite = TRUE)

