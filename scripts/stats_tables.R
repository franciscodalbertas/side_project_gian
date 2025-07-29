# --- load packages ---
library(tidyverse)
library(countrycode)
library(naniar)
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
df <- df %>% filter(!is.na(nat_regen))

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
tot_restor <- df_global %>%
  select(country_name, area_restorable_km2) %>%
  group_by(country_name) %>%
  summarise(country_restorable_area_km = sum(area_restorable_km2, na.rm = TRUE), .groups = "drop")
  

total_area_restorable <- sum(tot_restor$country_restorable_area_km, na.rm = TRUE)


tot_restor$prop_restor <- tot_restor$country_restorable_area_km / total_area_restorable

tot_restor <- tot_restor%>%
arrange(desc(prop_restor)) %>%
  mutate(cum_prop_restor = cumsum(prop_restor))

head(tot_restor,n = 20)


total_by_country <- df_global %>%
  filter(biodiversity_decile <= 3) %>%
  group_by(country_name, regen_05) %>%
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2, na.rm = TRUE), .groups = "drop")

total_summary <- total_by_country %>%
  group_by(country_name) %>%
  summarise(total_priority_area = sum(priority_restorable_area_by_regen_km), .groups = "drop") %>%
  left_join(tot_restor, by = "country_name")

top30_biod <- left_join(total_by_country, total_summary, by = "country_name")

rm(df)
# to do !
# carbon plot 
# top30_carbon <- df_global %>% filter(carbon_decile >= 7) %>%  
#   group_by(country_name, regen_05) %>%  
#   #calculate amount of "priority areas
#   summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
#   ungroup() %>%
#   group_by(country_name) %>%
#   #calculate amount of "priority areas' by regeneration potential
#   mutate(total_priority_area = sum(priority_restorable_area_by_regen_km))%>%  
#   #add total restorable area
#   left_join(tot_restor)
# 
# # oppcost plot
# bottom_30_oppcost <- df_global %>% filter(oppcost_decile <= 3) %>%  
#   group_by(country_name, regen_05) %>% 
#   #calculate amount of "priority areas
#   summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
#   ungroup() %>%
#   group_by(country_name) %>%
#   #calculate amount of "priority areas' by regeneration potential
#   mutate(total_priority_area = sum(priority_restorable_area_by_regen_km))%>%  
#   #add total restorable area
#   left_join(tot_restor)%>%
#   #filter out nas
#   filter(!is.na(country_name))

# I want total priority area by country, and how much of it is high
# regeneration. Then I can point out the top relevant countris...
# like the ones that concentrate 50 or so of the areas


#biodiv_df <- make_priority_table(top30_biod)

# total area under the 30%
total_priority <- top30_biod %>%
  select(country_name,total_priority_area ) %>%
  distinct()%>%
  ungroup()%>%
  summarise(total_priority = sum(total_priority_area, na.rm = TRUE), .groups = "drop")%>%
  pull()


# total priority area per country

top30_biod <- top30_biod %>%
   mutate(prop_priority = total_priority_area / total_priority,
         prop_resorable = priority_restorable_area_by_regen_km / total_priority)
  
  
print(head(top30_biod, n = 10), width = Inf)

top30_biod_priority_areas <-  top30_biod %>%
  select(country_name,prop_priority ) %>%
  distinct()%>%
         # arrange by priority area)
  arrange(desc(prop_priority)) %>%
         # this needs to be dune following the order of area!!
         mutate(cum_priority = cumsum(prop_priority))

print(head(top30_biod_priority_areas, n = 20), width = Inf)
# Brazil, Madagascar, Mexico, India, Colombia and Peru


# summarise how much is high regen potential and how much is low, per country

top30_biod_priority_areas_regen <- top30_biod %>%
  group_by(regen_05 ) %>%
  distinct()%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority)


print(head(top30_biod_priority_areas_regen, n = 20), width = Inf)


top30_biod_priority_areas_regen_country <- top30_biod %>%
  group_by(regen_05,country_name ) %>%
  distinct()%>%
  filter(regen_05 ==1)%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority) %>%
  arrange(desc(prop)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_prop = cumsum(prop))

print(head(top30_biod_priority_areas_regen_country, n = 20), width = Inf)
