library(terra)

baser <- rast("rasters/base_raster.tif")
nat_regen <- rast("rasters/mosaic_nat_reg_latlong_1km.tif")
nat_regen <- project(nat_regen, baser, "bilinear")

writeRaster(nat_regen,"rasters/mosaic_nat_reg_latlong_1km_reprojected.tif")
