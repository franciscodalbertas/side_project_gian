#17.11.2024; GC
#NOT SURE WE HAVE THE CORRECT LIFE VERSION
library(terra)
library(ggplot2)
library(viridis)  

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

plot(life_combined)
plot(life_top30)

#take areas that are of higher than average (>=50) likihood to naturally regenerate 
natR_top50 <- ifel(natR_mosaic >= 50, natR_mosaic, NA)

plot(natR_mosaic)
plot(natR_top50)

#save environment so don't need to recompute intermediate rasters 
save.image(file = "my_environment.RData")

# Create a binary combined mask where no NA values exist in any raster
combined_mask <- !(is.na(natR_top50) | is.na(oppcost_lowest30) | is.na(life_top30))
plot(combined_mask)

#Or we can plot only the areas of high Nat regen that also have high biod and low opcost
highRegen_lifetop30_oppcostbottom30<- mask(natR_top50, combined_mask)
plot(highRegen_lifetop30_oppcostbottom30)

#make nice plot ####

# Step 1: Plot the natural regeneration potential (base layer) with light green color
# We use a lighter green color scheme for the regeneration potential
plot(natR_mosaic, 
     main = "Total Area for Ecological Restoration", 
     col = terrain.colors(100),  # Choose a suitable color palette
     legend = TRUE,  # Add a legend
     axes = TRUE,  # Show axes
     box = TRUE,  # Show the plot box
     cex.main = 1.5,  # Title size
     cex.axis = 1.2)  # Axis label size

# Step 2: Add the mask (result raster) in dark green with transparency on top
# We plot the combined mask using a dark green color with some transparency
plot(combined_mask, 
     col = rgb(0, 0.5, 0, 0.6),  # Dark green with transparency (alpha = 0.6)
     add = TRUE)  # Add this plot on top of the previous one
