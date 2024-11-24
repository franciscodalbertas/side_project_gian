#load packages ---------------------------------------------

library(terra)
library(dplyr)

# ----------------------------------------------------------

#load life

LIFE <- rast("rasters/LIFE_latlong_1km.tif")
LIFE_df <- as.data.frame(LIFE,xy=T)
# clean NAs
names(LIFE_df)[3] <- "life_val"
# save it as rds
LIFE_df_na_cleaned <- LIFE_df %>%
  filter(!is.na(life_val))

dir.create("output_tables")

# save

saveRDS(LIFE_df_na_cleaned,"output_tables/life_values_df.rds")
