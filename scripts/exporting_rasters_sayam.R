#---- load packages ------

library(terra)
library(tidyverse)
library(countrycode)
library(naniar)
library(dplyr)
library(ggpubr)
library(ggplot2)
#library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geobr)
#---------------------------

# --- load data ---
#read in table of values
df <- readRDS("output_tables/full_dataframe_carb_bio_opp_regen.rds")
df$country_name <- countrycode(df$adm0_a3, origin = "iso3c", destination = "country.name")  
df <- df %>%   mutate(
  country_name = case_when(
    country_name == "Myanmar (Burma)" ~ "Myanmar",
    country_name == "Congo - Kinshasa" ~ "DR Congo",
    TRUE ~ country_name
  )
)

#remove areas without nat regen potential
df <- df %>%  
  filter(!is.na(nat_regen))

# --- global analysis ---

#calculate quantiles for carbon, biodiversity
# Assign decile bins (Q1 to Q10)
df_global <- df %>%
  mutate(
    carbon_decile = ntile(carbon, 10),
    biodiversity_decile =  ntile(biodiversity, 10), 
    oppcost_decile = ntile(opp_cost, 10)
  )

#assign regen potential thresholds
df_global <- df_global %>% mutate(
  regen_05 = case_when(
    nat_regen >= 50 ~ 1,
    nat_regen < 50  ~ 0,
    TRUE ~ NA_real_  )
)


# top 30% pixels for biodiversity 
top30_biod_map <- df_global %>% filter(biodiversity_decile <= 3) 

# top 30% pixels for carbon
top30_C_map <- df_global %>% filter(carbon_decile >= 7)

# I need a base raster!
rbase <- rast("rasters/LIFE_latlong_1km.tif")
rbase <- rbase/rbase

# test with global coverage!
top30_biod_map <- top30_biod_map %>% mutate(regen_05 = as.numeric(regen_05))
top30_biod_map_vec <- vect(st_as_sf(top30_biod_map,coords = c("x","y"),crs = crs(rbase)))

top30_C_map <- top30_C_map %>% mutate(regen_05 = as.numeric(regen_05))
top30_C_map_vec <- vect(st_as_sf(top30_C_map,coords = c("x","y"),crs = crs(rbase)))


biod_rast <- terra::rasterize(
  top30_biod_map_vec,
  rbase,
  field     = "regen_05",
  filename  = "rasters/top30biod_rast.tif",  # <- path to save on disk
  overwrite = TRUE
)

rm(df_global,df)


C_rast <- terra::rasterize(
  top30_C_map_vec,
  rbase,
  field     = "regen_05",
  filename  = "rasters/top30C_rast.tif",  # <- path to save on disk
  overwrite = TRUE
)



