library(terra)

# Load raster files into a list
raster_list <- list.files("rasters/nat_reg_latlong/", pattern = "\\.tif$", full.names = TRUE)
rasters <- lapply(raster_list, rast)

# Specify the output file path
#output_path <- "rasters/mosaic_nat_reg_mollweide_1km.tif"
output_path <- "rasters/mosaic_nat_reg_latlong_1km.tif"
# Merge the rasters and save to disk
merged_raster <- do.call(merge, c(rasters, list(filename = output_path, overwrite = TRUE)))

# Check the result
print(merged_raster)


