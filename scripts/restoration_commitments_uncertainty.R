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

# ---- global analysis ----

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
    TRUE ~ NA_real_  ),
  regen_04 = case_when(
    nat_regen >= 40 ~ 1,
    nat_regen < 40  ~ 0,
    TRUE ~ NA_real_  ),
  regen_06 = case_when(
    nat_regen >= 60 ~ 1,
    nat_regen < 60  ~ 0,
    TRUE ~ NA_real_  )
)


#calculate total restorable area by country
tot_restor <- df_global %>% select(country_name,area_restorable_km2) %>% 
  group_by(country_name) %>% 
  # calculate area in km2
  #summarise(country_restorable_area_km = sum(area_restorable))
  summarise(country_restorable_area_km = sum(area_restorable_km2, na.rm = TRUE))


#---- central analysis 50% ----


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

#---- 40% ----


#biodiversity plot
top30_biod_40 <- df_global %>% filter(biodiversity_decile <= 3) %>%  
  group_by(country_name, regen_04) %>%  
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
  group_by(country_name, regen_04) %>%  
  #calculate amount of "priority areas
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
  ungroup() %>%
  group_by(country_name) %>%
  #calculate amount of "priority areas' by regeneration potential
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km))%>%  
  #add total restorable area
  left_join(tot_restor)


#---- 60% ----


#biodiversity plot
top30_biod_60 <- df_global %>% filter(biodiversity_decile <= 3) %>%  
  group_by(country_name, regen_06) %>%  
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
  group_by(country_name, regen_06) %>%  
  #calculate amount of "priority areas
  summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2)) %>% 
  ungroup() %>%
  group_by(country_name) %>%
  #calculate amount of "priority areas' by regeneration potential
  mutate(total_priority_area = sum(priority_restorable_area_by_regen_km))%>%  
  #add total restorable area
  left_join(tot_restor)

#---- load national commitments ----

#---- load data ----

comm <- read.csv("input_data/restoration_commitments/grih-countries-commitments.csv")

#---- fixing names ----

comm <- comm %>%
  mutate(country = case_when(country == "Democratic Republic of Congo"~ "DR Congo",
                             country == "Lao"~"Laos",
                             country == "Viet Nam"~"Vietnam",
                             .default = country),
         single_highest_commitment_km2= single_highest_commitment/100)



#---- will use the single highest commitment ----

top_countries <-top30_biod %>%
  group_by(country_name) %>%
  summarise(total_area = unique(country_restorable_area_km), .groups = "drop") %>%
  arrange(desc(total_area)) %>%
  slice(1:30) %>%
  pull(country_name)


# top_countries_40 <-top30_biod_40 %>%
#   group_by(country_name) %>%
#   summarise(total_area = unique(country_restorable_area_km), .groups = "drop") %>%
#   arrange(desc(total_area)) %>%
#   slice(1:30) %>%
#   pull(country_name)
# 
# top_countries_60 <-top30_biod %>%
#   group_by(country_name) %>%
#   summarise(total_area = unique(country_restorable_area_km), .groups = "drop") %>%
#   arrange(desc(total_area)) %>%
#   slice(1:30) %>%
#   pull(country_name)


#---- add to carbon and bio ----

# 50%
top30_biod2 <- top30_biod %>%
  filter(country_name %in%top_countries) %>%
  left_join(comm %>% select(country,single_highest_commitment_km2),
            by= join_by("country_name"=="country"))%>%
  filter(regen_05 == 1) %>%
  mutate(
    country_name = factor(country_name, levels = rev(top_countries)),
    single_highest_commitment = single_highest_commitment_km2/ 10^3, # transform in 1000 km2
    priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km / 10^3,
    proportion = priority_restorable_area_by_regen_km/single_highest_commitment,
    proportion = if_else(proportion>1,1,proportion)
  )


top30_carbon2 <- top30_carbon %>%
  filter(country_name %in%top_countries) %>%
  left_join(comm %>% select(country,single_highest_commitment_km2),
            by= join_by("country_name"=="country"))%>%
  filter(regen_05 == 1) %>%
  mutate(
    country_name = factor(country_name, levels = rev(top_countries)),
    single_highest_commitment = single_highest_commitment_km2/ 10^3, # transform in 1000 km2
    priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km / 10^3,
    proportion = priority_restorable_area_by_regen_km/single_highest_commitment,
    proportion = if_else(proportion>1,1,proportion)
  )

# 40%
top30_biod2_40 <- top30_biod_40 %>%
  filter(country_name %in%top_countries) %>%
  left_join(comm %>% select(country,single_highest_commitment_km2),
            by= join_by("country_name"=="country"))%>%
  filter(regen_04 == 1) %>%
  mutate(
    country_name = factor(country_name, levels = rev(top_countries)),
    single_highest_commitment = single_highest_commitment_km2/ 10^3, # transform in 1000 km2
    priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km / 10^3,
    proportion = priority_restorable_area_by_regen_km/single_highest_commitment,
    proportion = if_else(proportion>1,1,proportion)
  )


top30_carbon2_40 <- top30_carbon_40 %>%
  filter(country_name %in%top_countries) %>%
  left_join(comm %>% select(country,single_highest_commitment_km2),
            by= join_by("country_name"=="country"))%>%
  filter(regen_04 == 1) %>%
  mutate(
    country_name = factor(country_name, levels = rev(top_countries)),
    single_highest_commitment = single_highest_commitment_km2/ 10^3, # transform in 1000 km2
    priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km / 10^3,
    proportion = priority_restorable_area_by_regen_km/single_highest_commitment,
    proportion = if_else(proportion>1,1,proportion)
  )


# 60%
top30_biod2_60 <- top30_biod_60 %>%
  filter(country_name %in%top_countries) %>%
  left_join(comm %>% select(country,single_highest_commitment_km2),
            by= join_by("country_name"=="country"))%>%
  filter(regen_06 == 1) %>%
  mutate(
    country_name = factor(country_name, levels = rev(top_countries)),
    single_highest_commitment = single_highest_commitment_km2/ 10^3, # transform in 1000 km2
    priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km / 10^3,
    proportion = priority_restorable_area_by_regen_km/single_highest_commitment,
    proportion = if_else(proportion>1,1,proportion)
  )


top30_carbon2_60 <- top30_carbon_60 %>%
  filter(country_name %in%top_countries) %>%
  left_join(comm %>% select(country,single_highest_commitment_km2),
            by= join_by("country_name"=="country"))%>%
  filter(regen_06 == 1) %>%
  mutate(
    country_name = factor(country_name, levels = rev(top_countries)),
    single_highest_commitment = single_highest_commitment_km2/ 10^3, # transform in 1000 km2
    priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km / 10^3,
    proportion = priority_restorable_area_by_regen_km/single_highest_commitment,
    proportion = if_else(proportion>1,1,proportion)
  )




