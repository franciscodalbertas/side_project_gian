# Load necessary library
# library(httr)
# library(jsonlite)
library(terra)
#library(stringr)
#-------------------------------------------------------------------------------
# source:
# https://zenodo.org/records/11372275
# maybe keep this as reference!
# folder

f <- "../../Data/Busch_et_al_24/"

imp_costs <- rast(paste0(f,"03_implementation_cost.tif"))

# adjust resol and proj
# read base raster
base <- rast("rasters/world_base_moll_1km.tif")
rstr_nm <- c("nat_regeneration_cost","native_spp_plantation_cost")

for(i in 1:2){
  r <- imp_costs[[i]]
  #r2 <- project(imp_costs[[i]],base)
  # writeRaster(r2, paste0("rasters/",rstr_nm[i],"_mollweide_1km.tif"), 
  #             filetype = "GTiff", 
  #             datatype = "INT4S", # Suitable for signed integer data 
  #             gdal = c("COMPRESS=LZW"),
  #             overwrite =T)
  writeRaster(r, paste0("rasters/",rstr_nm[i],"_latlong_1km.tif"),
              filetype = "GTiff",
              datatype = "INT4S", # Suitable for signed integer data
              gdal = c("COMPRESS=LZW"),
              overwrite =T)
     
  
}

r1 <- rast("rasters/nat_regeneration_cost_latlong_1km.tif")
r2 <- rast("rasters/native_spp_plantation_cost_mollweide_1km.tif")
# plot(r1)
# plot(r2)
# 
# print(crs(base))
# print(ext(base))
