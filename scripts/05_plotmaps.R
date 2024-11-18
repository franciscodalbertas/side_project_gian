#17.11.2024; GC
#NOT SURE WE HAVE THE CORRECT LIFE VERSION
library(terra)

#read in dependencies #### 

oppcost <- rast("rasters/nat_regeneration_cost_mollweide_1km.tif")
life  <-  rast("rasters/LIFE_moll_1km.tif")
natR_mosaic <- rast("rasters/mosaic_nat_reg_mollweide_1km.tif")

#plot rasters 
plot(oppcost)
plot(life)
plot(natR_mosaic)

#Filter rasters by conditions #### 

#take lowest 30% of oppcost  
oppcost_lowest30 <- ifel(oppcost <= quantile(values(oppcost), 0.3, na.rm = TRUE), 
                         oppcost, NA)
plot(oppcost)
plot(oppcost_lowest30)

#take the top 30% values of LIFE 
life_combined <- subset(life, "score") #take life combined across taxa
life_top30 <- ifel(life_combined <= quantile(values(life_combined), 0.7, na.rm = TRUE), life_combined, NA)

#only take natR values where regeneration potential >50 


