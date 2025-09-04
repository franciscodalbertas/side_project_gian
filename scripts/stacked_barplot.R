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

# --- load data ---

# I need to collapse it to have total area non-overlapping...or something that
# makes the axis with restorable area correct!

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

# its pixels on the edges!! I can filter them out!

#Note - quantile of 10 for carbon = most carbon 
#       quantile of 10 for biodiversity = worse = fewer extinctions averted 
df_global %>% filter(biodiversity == min(biodiversity, na.rm = TRUE))
df_global %>% filter(biodiversity == max(biodiversity, na.rm = TRUE))
df_global %>% filter(carbon == min(carbon, na.rm = TRUE))

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


# combine them in a single dataframe
bio <- top30_biod %>% 
  filter(regen_05==1)%>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(variable = "bio")


carbon <- top30_carbon %>% 
  filter(regen_05==1)%>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(variable = "carbon")


costs <- bottom_30_oppcost %>% 
  filter(regen_05==1)%>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(variable = "cost")

combined_df <- rbind(bio,carbon,costs)



#------------------------------------------------------
# Now make global plots
#------------------------------------------------------

top30 <- combined_df %>%
  group_by(country_name) %>%
  summarise(total_area = sum(priority_restorable_area_by_regen_km, na.rm = TRUE)) %>%
  arrange(desc(total_area)) %>%
  slice_head(n = 30)

p <- combined_df %>%
  inner_join(top30, by = "country_name") %>%
  ggplot(aes(x = fct_reorder(country_name, total_area),
             y = priority_restorable_area_by_regen_km,
             fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Country", y = "Restorable area (km²)", fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
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
  )+
  coord_flip()

p



