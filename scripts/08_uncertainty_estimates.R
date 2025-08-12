#build plots
# --- load packages ---
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
library(rlang)

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

#87% if arable lands are missing nat regen data; likely because 
#(1)Nat regen is only calculated for moist tropics  (for countries located at 
# least in part within humid tropical and subtropical forest biomes 
# (tropical and subtropical dry broadleaf forests, tropical and subtropical 
# moist broadleaf forests, and tropical and subtropical coniferous forests). )

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

# check NAs

#assign regen potential thresholds
# Here I will do 0.4 and 0.6
df_global <- df_global %>% mutate(
  regen_40= case_when(
    nat_regen >= 40 ~ 1,
    nat_regen < 40  ~ 0,
    TRUE ~ NA_real_  ),
  regen_60= case_when(
    nat_regen >= 60 ~ 1,
    nat_regen < 60  ~ 0,
    TRUE ~ NA_real_  )
  )

#calculate the total area of restoration potential by country 
# NB tot_restor is showing the total area, rather than the total of priority areas (which changes by metric)

#calculate total restorable area by country
tot_restor <- df_global %>% select(country_name,area_restorable_km2) %>% 
  group_by(country_name) %>% 
  # calculate area in km2
  #summarise(country_restorable_area_km = sum(area_restorable))
  summarise(country_restorable_area_km = sum(area_restorable_km2, na.rm = TRUE))

# ----  dfs with treshold 40% ----
top30_biod_40 <- df_global %>% filter(biodiversity_decile <= 3) %>%  
  group_by(country_name, regen_40) %>%  
  #calculate amount of "priority areas (Chico: for 0 and 1 regen)
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
  ungroup() %>%
  group_by(country_name) %>% 
  #calculate amount of "priority areas' by regeneration potential (chico: this is
  # total restorable area for each country)
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km)) %>%  
  #add total restorable area
   left_join(tot_restor)

# carbon plot 
top30_carbon_40 <- df_global %>% filter(carbon_decile >= 7) %>%  
  group_by(country_name, regen_40) %>%  
  #calculate amount of "priority areas
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
  ungroup() %>%
  group_by(country_name) %>%
  #calculate amount of "priority areas' by regeneration potential
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km))%>%  
  #add total restorable area
  left_join(tot_restor)

# oppcost plot
bottom_30_oppcost_40 <- df_global %>% filter(oppcost_decile <= 3) %>%  
  group_by(country_name, regen_40) %>% 
  #calculate amount of "priority areas
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
  ungroup() %>%
  group_by(country_name) %>%
  #calculate amount of "priority areas' by regeneration potential
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km))%>%  
  #add total restorable area
  left_join(tot_restor)%>%
  #filter out nas
  filter(!is.na(country_name))

# ----  dfs with treshold 60% ----
top30_biod_60 <- df_global %>% filter(biodiversity_decile <= 3) %>%  
  group_by(country_name, regen_60) %>%  
  #calculate amount of "priority areas (Chico: for 0 and 1 regen)
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
  ungroup() %>%
  group_by(country_name) %>% 
  #calculate amount of "priority areas' by regeneration potential (chico: this is
  # total restorable area for each country)
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km)) %>%  
  #add total restorable area
  left_join(tot_restor)

# carbon plot 
top30_carbon_60 <- df_global %>% filter(carbon_decile >= 7) %>%  
  group_by(country_name, regen_60) %>%  
  #calculate amount of "priority areas
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
  ungroup() %>%
  group_by(country_name) %>%
  #calculate amount of "priority areas' by regeneration potential
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km))%>%  
  #add total restorable area
  left_join(tot_restor)

# oppcost plot
bottom_30_oppcost_60 <- df_global %>% filter(oppcost_decile <= 3) %>%  
  group_by(country_name, regen_60) %>% 
  #calculate amount of "priority areas
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
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

make_priority_plot <- function(df,threshold) {
  
  
  regen_col <- sym(paste0("regen_", threshold))
  
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
      regen_label = if_else(
        !!regen_col == 1, 
        "High regeneration potential", 
        "Low regeneration potential"
      ),
      country_name = factor(country_name, levels = rev(top_countries)), # Order by descending area
      country_restorable_area_km = country_restorable_area_km/10^3,
      priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km/10^3
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
    scale_x_continuous(name = "Restorable area (thousand km²)", labels = function(x) format(x, scientific = FALSE)) +
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
# 40% treshold
biodiv_plot <- make_priority_plot(top30_biod_40,threshold = 40)+ggtitle("Biodiversity priority areas")
carbon_plot <- make_priority_plot(top30_carbon_40,threshold = 40)+ggtitle("Carbon priority areas")
opp_cost_plot <- make_priority_plot(bottom_30_oppcost_40,threshold = 40)+ggtitle("Cost priority areas")

# 60% treshold
biodiv_plot_60 <- make_priority_plot(top30_biod_60,threshold = 60)+ggtitle("Biodiversity priority areas")
carbon_plot_60 <- make_priority_plot(top30_carbon_60,threshold = 60)+ggtitle("Carbon priority areas")
opp_cost_plot_60 <- make_priority_plot(bottom_30_oppcost_60,threshold = 60)+ggtitle("Cost priority areas")



# ---------- 3. Print or save ----------

# save as a combined plot


panel_plot_40 <- ggarrange(
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

panel_plot_60 <- ggarrange(
  biodiv_plot_60,
  carbon_plot_60,
  opp_cost_plot_60,
  ncol = 3,    # Number of columns
  nrow = 1,    # Single row
  labels = c("D", "E", "F"),  # Optional: adds subplot labels
  align = "hv",               # Align plots horizontally and vertically
  common.legend = TRUE,       # Set to TRUE if all plots share the same legend
  legend = "top"           # Position of the shared legend
)


final <- ggarrange(panel_plot_40,panel_plot_60,nrow = 2,common.legend = T)



# Optionally save them:
 # ggsave("figures/NR_top_biod.png", biodiv_plot, width = 6, height = 6, dpi = 300,bg = "white")
 # ggsave("figures/NR_top_carbon.png", carbon_plot, width = 6, height = 6, dpi = 300,bg = "white")
 # ggsave("figures/NR_lowest_oppcost.png", opp_cost_plot, width = 6, height = 6, dpi = 300,bg = "white")
 
 ggsave(filename = "figures/panel_top_biod_top_carbon_loweroc_uncertainty.png", 
        final, width = 16, 
        height = 16, 
        dpi = 300,
        bg = "white",
        units = "cm")
 
 
