library(terra)

# load dataframe

df <- readRDS("output_tables/life_values_df.rds")

costs <- rast("rasters/nat_regeneration_cost_latlong_1km.tif")
reg_prob <- rast("rasters/mosaic_nat_reg_latlong_1km.tif")
rasters <- list(costs, reg_prob)
# Assign meaningful names to raster layers
names(rasters) <- c("cost", "prob_reg")
# Convert dataframe to terra SpatVector
points <- vect(df, geom = c("x", "y"), crs = crs(costs))

# Extract values for each raster
values <- lapply(rasters, function(r) extract(r, points)[, 2])

# Combine extracted values into dataframe
values_df <- do.call(cbind, values)
colnames(values_df) <- names(rasters)

# Add extracted values as new columns to the original dataframe
df <- cbind(df, values_df)

# clean NAs

df2 <- df %>%
  filter(!is.na(cost),
         !is.na(prob_reg)
         )

# save table

saveRDS(df2,"output_tables/raster_values.rds")
