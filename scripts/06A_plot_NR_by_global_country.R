#build plots

library(terra)
library(tidyverse)
library(countrycode)
library(naniar)
library(dplyr)
library(ggpubr)


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

# Summarize the number of NAs per column
na_summary <- miss_var_summary(df)
#87% if arable lands are missing nat regen data; likely because 
#(1)Nat regen is only calculated for moist tropics  (for countries located at least in part within humid tropical and subtropical forest biomes (tropical and subtropical dry broadleaf forests, tropical and subtropical moist broadleaf forests, and tropical and subtropical coniferous forests). )

#remove areas without nat regen potential
df <- df %>%  
  filter(!is.na(nat_regen))
#--------------------------------------
#global analysis

#calculate quantiles for carbon, biodiversity
# Assign decile bins (Q1 to Q10)
df_global <- df %>%
  mutate(
    carbon_decile = ntile(carbon, 10),
    biodiversity_decile =  ntile(biodiversity, 10), 
    oppcost_decile = ntile(opp_cost, 10)
  )

# check NAs

df_nas <- filter(df_global, is.na(country_name))
# why so many values without any country attributed?
#export to shape to check
df_nas_shp <- sf::st_as_sf(df_nas, coords = c("x", "y"), crs = 4326)

sf::st_write(df_nas_shp, "output_tables/df_nas_country_name.shp")

# its pixels on the edges!! I can filter them out!

#Note - quantile of 10 for carbon = most carbon 
#       quantile of 10 for biodiversity = worse = fewer extinctions averted 
df_global %>% filter(biodiversity == min(biodiversity, na.rm = TRUE))
df_global %>% filter(biodiversity == max(biodiversity, na.rm = TRUE))
df_global %>% filter(carbon == min(carbon, na.rm = TRUE))

#distribution of nat reg potential
hist(df_global$nat_regen)

#assign regen potential thresholds
df_global <- df_global %>% mutate(
  regen_05 = case_when(
    nat_regen >= 50 ~ 1,
    nat_regen < 50  ~ 0,
    TRUE ~ NA_real_  )
  )

#calculate the total area of restoration potential by country 
# NB tot_restor is showing the total area, rather than the total of priority areas (which changes by metric)

summary(df_global$area_restorable)# this is fraction...of 3.419187 km2


#calculate total restorable area by country
tot_restor <- df_global %>% select(country_name,area_restorable) %>% 
  group_by(country_name) %>% 
  # calculate area in km2
  #summarise(country_restorable_area_km = sum(area_restorable))
  summarise(country_restorable_area_km = sum(area_restorable*3.419187, na.rm = TRUE))

#biodiversity plot
top30_biod <- df_global %>% filter(biodiversity_decile <= 3) %>%  
  group_by(country_name, regen_05) %>%  
  #calculate amount of "priority areas (Chico: for 0 and 1 regen)
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable*3.419187)) %>% 
  ungroup() %>%
  group_by(country_name) %>% 
  #calculate amount of "priority areas' by regeneration potential (chico: this is
  # total restorable area for each country)
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km)) %>%  
  #add total restorable area
   left_join(tot_restor)

head(top30_biod)

#carbon plot 
top30_carbon <- df_global %>% filter(carbon_decile >= 7) %>%  
  group_by(country_name, regen_05) %>%  
  #calculate amount of "priority areas
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable*3.419187)) %>% 
  ungroup() %>%
  group_by(country_name) %>%
  #calculate amount of "priority areas' by regeneration potential
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km))%>%  
  #add total restorable area
  left_join(tot_restor)

#oppcost plot
bottom_30_oppcost <- df_global %>% filter(oppcost_decile <= 3) %>%  
  group_by(country_name, regen_05) %>% 
  #calculate amount of "priority areas
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable*3.419187)) %>% 
  ungroup() %>%
  group_by(country_name) %>%
  #calculate amount of "priority areas' by regeneration potential
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km))%>%  
  #add total restorable area
  left_join(tot_restor)%>%
  #filter out nas
  filter(!is.na(country_name))



#------------------------------------------------------
# Now make global plots
#------------------------------------------------------

# ---------- 1. Define a function to create the plot ----------
options(scipen = 999)

make_priority_plot <- function(df) {
  # Filter for the top 30 countries by total restorable area
  top_countries <- df %>%
    group_by(country_name) %>%
    summarise(total_area = unique(country_restorable_area_km), .groups = "drop") %>%
    arrange(desc(total_area)) %>%
    slice(1:30) %>%
    pull(country_name)
  
  # Filter the data and order countries by total restorable area
  df <- df %>%
    filter(country_name %in% top_countries) %>%
    mutate(
      regen_label = if_else(regen_05 == 1, "High regeneration potential", "Low regeneration potential"),
      country_name = factor(country_name, levels = rev(top_countries)), # Order by descending area
      country_restorable_area_km = country_restorable_area_km/10^6,
      priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km/10^6
      )
  
  # Plot
  ggplot() +
    # Total restorable area (grey background bar)
    geom_col(
      data = df %>% distinct(country_name, country_restorable_area_km),
      aes(
        x = country_restorable_area_km,
        y = country_name
      ),
      fill = "grey80",
      alpha = 0.7
    ) +
    # Priority restorable area (stacked bar for regeneration categories)
    geom_col(
      data = df,
      aes(
        x = priority_restorable_area_by_regen_km,
        y = country_name,
        fill = regen_label
      )
    ) +
    scale_fill_manual(
      values = c(
        "Low regeneration potential" = "#E69F00",
        "High regeneration potential" = "#009E73"
      )
    ) +
    scale_x_continuous(name = "Restorable area (million km²)", labels = function(x) format(x, scientific = FALSE)) +
    theme_minimal(base_size = 7) +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      plot.margin = margin(10, 20, 10, 10)
    )
}

# ---------- 2. Generate plots ----------
biodiv_plot <- make_priority_plot(top30_biod)+ggtitle("biodiversity priority areas")
carbon_plot <- make_priority_plot(top30_carbon)+ggtitle("carbon priority areas")
opp_cost_plot <- make_priority_plot(bottom_30_oppcost)+ggtitle("cost priority areas")

# ---------- 3. Print or save ----------
biodiv_plot
carbon_plot
opp_cost_plot # check this NA on OC!
#cowplot::plot_grid(biodiv_plot,carbon_plot)


# save as a combined plot

panel_plot <- ggarrange(
  biodiv_plot,
  carbon_plot,
  opp_cost_plot,
  ncol = 3,    # Number of columns
  nrow = 1,    # Single row
  labels = c("A", "B", "C"),  # Optional: adds subplot labels
  align = "hv",               # Align plots horizontally and vertically
  common.legend = TRUE,       # Set to TRUE if all plots share the same legend
  legend = "top"           # Position of the shared legend
)

# Optionally save them:
 ggsave("figures/NR_top_biod.png", biodiv_plot, width = 6, height = 6, dpi = 300,bg = "white")
 ggsave("figures/NR_top_carbon.png", carbon_plot, width = 6, height = 6, dpi = 300,bg = "white")
 ggsave("figures/NR_lowest_oppcost.png", opp_cost_plot, width = 6, height = 6, dpi = 300,bg = "white")
 
 ggsave(filename = "figures/panel_top_biod_top_carbon_loweroc.png", panel_plot, width = 16, 
        height = 8, dpi = 300,bg = "white",units = "cm")
 
 
#-----------------------------------------------------
#Make a quick map of high regen and lot regen areas
#-----------------------------------------------------
 library(ggplot2)
 #library(dplyr)
 library(sf)
 library(rnaturalearth)
 library(rnaturalearthdata)
 
 # Load world basemap
 world <- ne_countries(scale = "medium", returnclass = "sf")
 
 # Reusable plotting function
 make_regen_map <- function(data, title
                            #, filename
                            ) {
   plot_df <- data %>%
     mutate(
       regen_label = if_else(regen_05 == 1, "High regeneration potential", "Low regeneration potential")
     )
   
   map <- ggplot() +
     geom_sf(data = world, fill = "grey95", color = "white", size = 0.3) +
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
     coord_sf(xlim = c(-100, 160), ylim = c(-30, 30), expand = FALSE) +
     theme_minimal(base_size = 8) +
     theme(
       panel.background = element_rect(fill = "white", color = NA),
       panel.grid = element_blank(),
       legend.title = element_blank(),
       legend.position = "top",
       axis.title = element_blank(),
       axis.text = element_blank(),
       axis.ticks = element_blank()
     ) +
     ggtitle(title) +
     guides(color = guide_legend(override.aes = list(size = 2)))
   
   #ggsave(filename, map, width = 10, height = 6, dpi = 300, bg = "white")
 }
 
# Run function for biodiversity and carbon maps
 
# top 30% pixels for biodiversity 
top30_biod_map <- df_global %>% filter(biodiversity_decile <= 3) 
top30_carbon_map <- df_global %>% filter(carbon_decile >= 7)   
   
 
bio_map <- make_regen_map(top30_biod_map
                          , "Regeneration potential for top biodiversity areas"
                          #,"Figures/topBio_regeneration_map.png"
                          )
carbon_map <- make_regen_map(top30_carbon_map,
                             "Regeneration potential for top carbon areas"
                             #, "Figures/topCarbon_regeneration_map.png"
                             )
 

# saving

ggsave("Figures/topBio_regeneration_map.png", bio_map, width = 16, height = 6, 
       dpi = 300, bg = "white",units = "cm")


ggsave("Figures/topCarbon_regeneration_map.png", carbon_map, width = 16, 
       height = 6,dpi = 300, bg = "white",units = "cm")
# top30_biod
# 
#  library(ggplot2)
#  library(ggthemes)
#  library(rnaturalearth)
#  library(rnaturalearthdata)
#  library(tidyverse)
#  
#  # Load base map (world)
#  world <- ne_countries(scale = "medium", returnclass = "sf")
#  
#  # Prepare your data
#  plot_df <- top30_biod %>%
#    mutate(
#      regen_label = if_else(regen_05 == 1, "High regeneration potential", "Low regeneration potential")
#    )
#  
#  # Create plot
#  map<- ggplot() +
#    # Base map
#    geom_sf(data = world, fill = "grey95", color = "white", size = 0.3) +
#    
#    # Points for regeneration
#    geom_point(
#      data = plot_df,
#      aes(x = x, y = y, color = regen_label),
#      alpha = 0.7,
#      size = 0.7, 
#      shape = 15  # Shape 15 = filled square
# 
#    ) +
#    
#    # Color scheme
#    scale_color_manual(
#      values = c("Low regeneration potential" = "#E69F00",  # orange
#                 "High regeneration potential" = "#009E73")  # green
#    ) +
#    
#    # Coordinate system for tropical focus
#    coord_sf(xlim = c(-100, 160), ylim = c(-30, 30), expand = FALSE) +
#    
#    # Themes and labels
#    theme_minimal(base_size = 13) +
#    theme(
#      panel.background = element_rect(fill = "white", color = NA),
#      panel.grid = element_blank(),
#      legend.title = element_blank(),
#      legend.position = "top",
#      axis.title = element_blank(),
#      axis.text = element_blank(),
#      axis.ticks = element_blank()
#    ) +
#    ggtitle("Regeneration potential for top biodiversity areas") +
#    guides(color = guide_legend(override.aes = list(size = 3)))
# 
#  ggsave("Figures/topBio_regeneration_map.png", width = 10, height = 6, dpi = 300, bg = "white")
#  
#  
# 
# 
# 
# # Clean and prep
# df_top20 <- global_bio %>%
#   mutate(
#     n = as.numeric(str_trim(n)),
#     regen_label = if_else(regen_05 == 1, "High regeneration potential", "Low regeneration potential")
#   ) %>%
#   group_by(country_name) %>%
#   slice(1) %>%
#   ungroup() %>%
#   arrange(desc(restorable_cells)) %>%
#   slice(1:20) %>%
#   pull(country_name)
# 
# # Filter to top 20 countries and apply factor order (reverse for top at top)
# global_bio <- global_bio %>%
#   filter(country_name %in% df_top20) %>%
#   mutate(
#     n = as.numeric(str_trim(n)),
#     regen_label = if_else(regen_05 == 1, "High regeneration potential", "Low regeneration potential"),
#     country_name = factor(country_name, levels = rev(df_top20))  # Top countries at top
#   )
# 
# # Generate alternating background colors
# bg_df <- tibble(
#   country_name = levels(global_bio$country_name),
#   y = seq_along(country_name)
# )
# 
# # Plot
# ggplot(global_bio, aes(x = proportion * 100, y = country_name, color = regen_label)) +
#   # Background rectangles
#   geom_rect(data = bg_df, aes(ymin = y - 0.5, ymax = y + 0.5),
#             xmin = -Inf, xmax = Inf, fill = rep(c("grey95", "white"), length.out = 20),
#             inherit.aes = FALSE) +
#   # Points
#   geom_point(size = 3) +
#   # Colors
#   scale_color_manual(values = c("Low regeneration potential" = "#E69F00",  # orange
#                                 "High regeneration potential" = "#009E73")) +  # green
#   # Axes and theme
#   scale_x_continuous(name = "Top global biodiversity areas (%)", limits = c(0, 100)) +
#   theme_minimal(base_size = 12) +
#   theme(
#     legend.title = element_blank(),
#     legend.position = "top",
#     axis.title.y = element_blank(),
#     axis.ticks.y = element_line(),         # Ensure y-axis ticks are shown
#     axis.line.y = element_line(),          # Optional: add a y-axis line
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     panel.border = element_rect(color = "black", fill = NA),  # Add border
#     plot.margin = margin(10, 20, 10, 10)
#   )
#--------------------------------------------------------
#Country analysis 
#--------------------------------------------------------

#calculate quantiles for carbon, biodiversity
# Assign decile bins (Q1 to Q10)
df_country <- df %>%
  group_by(country_name) %>% 
  mutate(
    carbon_decile = ntile(carbon, 10),
    biodiversity_decile =  ntile(biodiversity, 10)
  )
#Note - quantile of 10 for carbon = most carbon 
#       quantile of 10 for biodiversity = worse = fewer extinctions averted 
df_global %>% filter(biodiversity == min(biodiversity, na.rm = TRUE))
df_global %>% filter(biodiversity == max(biodiversity, na.rm = TRUE))
df_global %>% filter(carbon == min(carbon, na.rm = TRUE))

#distribution of nat reg potential
hist(df_global$nat_regen)

#assign regen potential thresholds
df_global <- df_global %>% mutate(
  regen_05 = case_when(
    nat_regen >= 0.5 ~ 1,
    nat_regen < 0.5  ~ 0,
    TRUE ~ NA_real_  )
)

#calculate the total area of restoration potential by country 
tot_restor <- df_global %>% select(country_name) %>% 
  group_by(country_name) %>% 
  count()%>%
  rename(restorable_cells = n)

df_global <- df_global %>% left_join(tot_restor)

#biodiversity plot
top30_biod <- df_global %>% filter(biodiversity_decile <= 3)
top30_carbon <- df_global %>% filter(carbon_decile >= 7)


