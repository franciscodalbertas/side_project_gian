#17.11.2024; GC
#NOT SURE WE HAVE THE CORRECT LIFE VERSION
library(terra)
library(ggplot2)
library(viridis)  
library(tidyverse)
#read in dependencies #### 

oppcost <- rast("rasters/nat_regeneration_cost_mollweide_1km.tif")
life  <-  rast("rasters/LIFE_moll_1km.tif")
natR_mosaic <- rast("rasters/mosaic_nat_reg_mollweide_1km.tif")

#plot rasters 
plot(oppcost)
plot(life)
plot(natR_mosaic)

# Stack the rasters
r_stack <- c(oppcost, life, natR_mosaic)  # Combine the rasters into a SpatRaster

# Assign names to layers
names(r_stack) <- c("oppcost", "life","natRegen")

# Convert to a tidy dataframe - RUN TOMORROW ON MY DESKTOP 
tidy_df <- as.data.frame(r_stack, xy = TRUE) %>%
  rename(X = x, Y = y)

#Filter rasters by conditions #### 

#take lowest 30% and highest 30% of oppcost  
oppcost_lowest30 <- ifel(oppcost <= quantile(values(oppcost), 0.3, na.rm = TRUE), 
                         oppcost, NA)
oppcost_highest30 <- ifel(oppcost <= quantile(values(oppcost), 0.7, na.rm = TRUE), 
                         oppcost, NA)
plot(oppcost)
plot(oppcost_lowest30)
plot(oppcost_highest30)

#take the top 30% and bottom 30% values of LIFE 
life_combined <- subset(life, "score") #take life combined across taxa
life_top30 <- ifel(life_combined <= quantile(values(life_combined), 0.7, na.rm = TRUE), life_combined, NA)
life_bottom30 <- ifel(life_combined <= quantile(values(life_combined), 0.3, na.rm = TRUE), life_combined, NA)

plot(life_combined)
plot(life_top30)
plot(life_bottom30)

#take areas that are of higher  (>=50) and lower (<50) than average likihood to naturally regenerate 
natR_top50 <- ifel(natR_mosaic >= 50, natR_mosaic, NA)
natR_bottom50 <- ifel(natR_mosaic < 50, natR_mosaic, NA)

plot(natR_mosaic)
plot(natR_top50)
plot(natR_bottom50)

#Create combined outputs ####

#low oppcost, high biod, high regen - these are good places to restore 
combined_mask1 <- !(is.na(natR_top50) | is.na(oppcost_lowest30) | is.na(life_top30))

#high oppcost, low biod, high regen - places where nat regen has high leakage and lower relative biod benefits 
combined_mask2 <-!(is.na(natR_top50) | is.na(oppcost_highest30) | is.na(life_bottom30))

#high biod, low oppcost, low regen - #places where biodiversity benefits are high and oppcost low - but we may need active restoration approaches as regen potential is lower 
combined_mask3 <-!(is.na(natR_bottom50) | is.na(oppcost_lowest30) | is.na(life_top30))


#EXPORT RESULTS 
writeRaster(combined_mask, "outputs/high50Regen_lifetop30_oppcostbottom30.tif" )
writeRaster(combined_mask2, "outputs/high50Regen_lifebottom30_oppcosthighest30.tif" )
writeRaster(combined_mask3, "outputs/bottom50Regen_lifetop30_oppcostbottom30.tif" )

