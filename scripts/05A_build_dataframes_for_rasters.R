#join all rasters

library(terra)
library(rnaturalearth)
library(dplyr)

carbon <- rast("rasters/max_carbon_rate_reprojected.tif")

plot(carbon)

biod <- rast("rasters/LIFE_latlong_1km.tif")
opp_cost <- rast("rasters/opp_cost_reprojected.tif")
nat_regen <- rast("rasters/mosaic_nat_reg_latlong_1km_reprojected.tif")
area_restorable <- rast("rasters/restorable_area_km2.tif") # this is actual area!

# sum all values!



names(carbon) <- "carbon"
names(biod) <- "biodiversity"
names(opp_cost) <- "opp_cost"
names(nat_regen) <- "nat_regen"
names(area_restorable) <- "area_restorable_km2"


# Only keep cells which have some restorable land, as defined by Eyres et al regen layer
# Mask rasters to keep only cells where nat_regen is not NA

#COME BACK TO THIS DECISION - lots of cells that could be restored in Brook-Williams have NA values 
#in Eyres et al. layer

carbon_masked <- mask(carbon, biod)
#biod_masked <- mask(biod, biod)
opp_cost_masked <- mask(opp_cost, biod)
nat_regen_masked <- mask(nat_regen, biod)
area_restorable_masked <- mask(area_restorable, biod)

# get totla area restorable

s <- global(area_restorable_masked, "sum", na.rm = TRUE)
# 13647209


# plot(carbon_masked)
# plot(biod)
# plot(opp_cost_masked)
# plot(nat_regen_masked)
# plot(area_restorable_masked)

#combine all
all <- c(nat_regen_masked, carbon_masked,opp_cost_masked,area_restorable_masked,biod)
all_df <- as.data.frame(all, xy = TRUE) #23 million cells

#remove rows where any layer value is NA 
#all_df <- all_df %>% drop_na() #down to 2 million cells
  
#plot(all)

#add country info:
# Load country boundaries using rnaturalearth
countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Convert to SpatVector for compatibility with terra
countries_vect <- vect(countries)
# Rasterize country polygons (e.g., each cell has a country ID)

# Define a target raster grid based on one of your existing rasters
target_raster <- rast(nat_regen)  # Use `nat_regen` raster as the template

# Rasterize the country vector onto the target raster
country_raster <- rasterize(countries_vect, target_raster, field = "adm0_a3")  # Use a field like 'adm0_a3' for country codes
#plot(country_raster)

# Reproject countries to match raster CRS if necessary
if (!identical(crs(all), crs(country_raster))) {
  country_raster <- project(country_raster, crs(all))
}

# Extract pixel coordinates from the raster
coords <- all_df %>% select(x,y)  # Includes X and Y coordinates

# Convert `coords` to a SpatVector
pixel_points <- vect(coords[, c("x", "y")], geom = c("x", "y"), crs = crs(nat_regen))

# Extract country information for each pixel
#terraOptions(threads = parallel::detectCores() - 1)
country_info <- terra::extract(country_raster, pixel_points)

#combine df with country and coords info
full_df <- country_info %>% cbind(all_df) 

#head(full_df)


# remove rows for which everything is NA!
cols <- c("carbon", "biodiversity", "opp_cost", "area_restorable_km2", "nat_regen")
# 
full_df2 <- full_df %>%
  filter(rowSums(!is.na(across(all_of(cols)))) > 0)

#Save output for further investigation
saveRDS(full_df2, "output_tables/full_dataframe_carb_bio_opp_regen.rds")

