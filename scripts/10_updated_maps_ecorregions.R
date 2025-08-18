#---- load packages ------

#library(terra)
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

#---- ploting regional maps ------

# Load world basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

# load ecoregions

ecoregions <- st_read("../../Data/Ecoregions2017/Ecoregions2017.shp")

# project

ecoregions_pj <- st_transform(ecoregions,st_crs(world))%>%
  st_make_valid()

# append continent info to df_global

df_global <- df_global %>%
  left_join(world %>% select(adm0_a3, continent, subregion,region_un), by = c("adm0_a3"))


# append ecoregion info

#Make df into an sf point object (assuming lon/lat in WGS84)
pts <- st_as_sf(df_global, coords = c("x","y"), crs = st_crs(world))   # change crs if needed

#  Spatial join (left join keeps all points)
pts_joined <- st_join(pts, ecoregions_pj[, c("BIOME_NAME","ECO_NAME","BIOME_NUM","ECO_ID")], left = TRUE)

# 5) Back to data.frame if you prefer
df_out <- pts_joined %>%
  st_drop_geometry()   # now df_out has BIOME_NAME and ECO_NAME added


df_global <- left_join(df_global,df_out)

# drop spatial info.
df_global <- st_drop_geometry(df_global)


# exclude EUA and Canada from world

world_filt <- world %>% filter(!subregion %in% c("Northern America"))


# figure on top with all tropics showing! I will need to add squares to zoom-in

# Reusable plotting function
make_regen_map <- function(data, title
                           #, filename
) {
  plot_df <- data %>%
    mutate(
      regen_label = if_else(regen_05 == 1, "High regeneration potential", "Low regeneration potential")
    )
  
  map <- ggplot() +
    geom_sf(data = world_filt, fill = "grey95", color = "white", size = 0.3) +
    geom_raster(
      data = plot_df,
      aes(x = x, y = y, fill = regen_label),
      alpha = 0.7
    ) +
    scale_fill_manual(
      values = c(
        "Low regeneration potential" = "#E69F00",
        "High regeneration potential" = "#009E73"
      )
    ) +
    #facet_wrap(~ continent)+
    coord_sf(xlim = c(-100, 160), ylim = c(-30, 30), expand = FALSE) +
    theme_minimal(base_size = 7) +
    theme(
      plot.margin = margin(0,0,0,0),
      panel.background = element_rect(fill = "white", color = NA),
      #panel.grid = element_blank(),
      legend.title = element_blank(),
      legend.position = "top",
      axis.title = element_blank(),
      #axis.text = element_blank(),
      #axis.ticks = element_blank()
    ) +
    ggtitle(title) +
    guides(color = guide_legend(override.aes = list(size = 2)))
  
  return(map)
}

# Run function for biodiversity and carbon maps

# top 30% pixels for biodiversity 
top30_biod_map <- df_global %>% filter(biodiversity_decile <= 3) 


# need to focus in the tropics only, exclude the rest!!

bio_map <- make_regen_map(top30_biod_map,"")+
  # Tropics lines
  coord_sf(
    xlim = c(-110, 130),
    ylim = c(-23.4366, 23.4366),
    expand = FALSE
  )

bio_map

# check ecoregions in latin america

unique(top30_biod_map$subregion)

top30_biod_map %>%
  filter(subregion %in% c("Central America"))%>%
  distinct(BIOME_NAME)
  
top30_biod_map %>%
  filter(adm0_a3 %in% c("COL","VEN"))%>%
  distinct(BIOME_NAME)

# a, b,c, zoom in f ang and e

# how to select and facet it!!
# precisa filtrar tudo q nao for...
# e criar uma coluna pra plotar

# test Caribbean

ggplot() +
  #geom_sf(data = world, fill = "lightblue", color = "black") + # world map
  geom_sf(data =  world %>% filter(subregion %in% c("Central America","Caribbean")), fill = NA, color = "black", size = 1) +
  geom_sf(data =  world %>% filter(adm0_a3 %in% c("COL","VEN")), fill = NA, color = "black", size = 1) +
  # Tropics lines
  coord_sf(
    xlim = c(-110, 130),
    ylim = c(-23.4366, 23.4366),
    expand = FALSE
  )


world_filt <- world_filt %>%
  mutate(
    facet_column = case_when(
      adm0_a3 %in% c("COL","VEN") ~ "Inset_A",
      subregion %in% c("Central America","Caribbean") ~ "Inset_A",
      TRUE                        ~ "Inset_B"   # everything else
    )
  )

# change "inset" to your column name (e.g., "facet_column")
inset_col <- "facet_column"

# 1) Split data by inset
by_inset <- world_filt %>%
  group_split(.keep = TRUE, .by = all_of(inset_col))  # dplyr >= 1.1.0

inset_levels <- world_filt %>% distinct(.data[[inset_col]]) %>% pull()

# 2) Build one plot per inset using its own bbox
make_inset_plot <- function(dat, title = NULL) {
  bb <- st_bbox(dat)
  ggplot() +
    geom_sf(data = dat, linewidth = 0.2) +
    coord_sf(
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"]),
      expand = FALSE
    ) +
    labs(title = title) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 10, hjust = 0.5))
}

plots <- map2(by_inset, inset_levels, ~ make_inset_plot(.x, .y))

plots[[1]]
plots[[2]]
