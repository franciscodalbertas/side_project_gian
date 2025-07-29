# --- load packages ---
library(tidyverse)
library(countrycode)
library(naniar)
library(dplyr)
# library(ggpubr)
# library(ggplot2)
#library(dplyr)
# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
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

#calculate the total area of restoration potential by country 
# NB tot_restor is showing the total area, rather than the total of priority areas (which changes by metric)


#calculate total restorable area by country
tot_restor <- df_global %>% select(country_name,area_restorable_km2) %>% 
  group_by(country_name) %>% 
  # calculate area in km2
  #summarise(country_restorable_area_km = sum(area_restorable))
  summarise(country_restorable_area_km = sum(area_restorable_km2, na.rm = TRUE))

#biodiversity plot
top30_biod <- df_global %>% filter(biodiversity_decile <= 3) %>%  
  group_by(country_name, regen_05) %>%  
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
top30_carbon <- df_global %>% filter(carbon_decile >= 7) %>%  
  group_by(country_name, regen_05) %>%  
  #calculate amount of "priority areas
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
  ungroup() %>%
  group_by(country_name) %>%
  #calculate amount of "priority areas' by regeneration potential
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km))%>%  
  #add total restorable area
  left_join(tot_restor)

# oppcost plot
bottom_30_oppcost <- df_global %>% filter(oppcost_decile <= 3) %>%  
  group_by(country_name, regen_05) %>% 
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


# function to summarise data

make_priority_table <- function(df) {
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
      country_restorable_area_km = country_restorable_area_km,
      priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km
    )
  
  
}

biodiv_df <- make_priority_table(top30_biod)





