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


#--- function to generate priority table ---

make_priority_table <- function(df, criteria = NULL, threshold = NULL) {
  
  
  regen_col <- sym(paste0("regen_", threshold))
  
  if (criteria == "biodiversity") {
    
    filter_by <- "biodiversity_decile"
    
    total_by_country <- df %>%
      filter(.data[[filter_by]] <= 3) %>%
      group_by(country_name, !!regen_col) %>%
      summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2, na.rm = TRUE), .groups = "drop")
    
  } else if (criteria == "carbon") {
    filter_by <- "carbon_decile"
    
    total_by_country <- df %>%
      filter(.data[[filter_by]] >= 7) %>%
      group_by(country_name, !!regen_col) %>%
      summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2, na.rm = TRUE), .groups = "drop")
  } else if (criteria == "opp_cost") {
    filter_by <- "oppcost_decile"
    
    total_by_country <- df %>%
      filter(.data[[filter_by]] <= 3) %>%
      group_by(country_name, !!regen_col) %>%
      summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2, na.rm = TRUE), .groups = "drop")
  } else {
    stop("Invalid criteria specified. Use 'biodiversity', 'carbon', or 'opp_cost'.")
  }
  
  
  total_summary <- total_by_country %>%
    group_by(country_name) %>%
    summarise(total_priority_area = sum(priority_restorable_area_by_regen_km), .groups = "drop") %>%
    left_join(tot_restor, by = "country_name")
  
  top30 <- left_join(total_by_country, total_summary, by = "country_name")
  
  return(top30)
}

#--- generate tables ---

# 50%
top30_biod <- make_priority_table(df_global, criteria = "biodiversity",threshold = "05") 
top30_carbon <- make_priority_table(df_global, criteria = "carbon",threshold = "05") 
top30_costs <- make_priority_table(df_global, criteria = "opp_cost",threshold = "05") 


# 40%
top30_biod_40 <- make_priority_table(df_global, criteria = "biodiversity",threshold = "04") 
top30_carbon_40 <- make_priority_table(df_global, criteria = "carbon",threshold = "04") 
top30_costs_40 <- make_priority_table(df_global, criteria = "opp_cost",threshold = "04") 

# 60%
top30_biod_60 <- make_priority_table(df_global, criteria = "biodiversity",threshold = "06") 
top30_carbon_60 <- make_priority_table(df_global, criteria = "carbon",threshold = "06") 
top30_costs_60 <- make_priority_table(df_global, criteria = "opp_cost",threshold = "06") 


#---- stats for biodiversity for 50%----


# total area under the 30%
total_priority <- top30_biod %>%
  select(country_name,total_priority_area ) %>%
  distinct()%>%
  ungroup()%>%
  summarise(total_priority = sum(total_priority_area, na.rm = TRUE), .groups = "drop")%>%
  pull()

#---- total priority area per country ----

top30_biod <- top30_biod %>%
   mutate(prop_priority = total_priority_area / total_priority,
         prop_resorable = priority_restorable_area_by_regen_km / total_priority)
  

top30_biod_40 <- top30_biod_40 %>%
  mutate(prop_priority = total_priority_area / total_priority,
         prop_resorable = priority_restorable_area_by_regen_km / total_priority)

top30_biod_60 <- top30_biod_60 %>%
  mutate(prop_priority = total_priority_area / total_priority,
         prop_resorable = priority_restorable_area_by_regen_km / total_priority)


  
print(head(top30_biod, n = 10), width = Inf)
print(head(top30_biod_40, n = 10), width = Inf)
#---- 50% ----
top30_biod_priority_areas <-  top30_biod %>%
  select(country_name,prop_priority ) %>%
  distinct()%>%
         # arrange by priority area)
  arrange(desc(prop_priority)) %>%
         # this needs to be dune following the order of area!!
         mutate(cum_priority = cumsum(prop_priority))

print(head(top30_biod_priority_areas, n = 20), width = Inf)
# Brazil, Madagascar, Mexico, India, Colombia and Peru

#- summarise how much is high regen potential and how much is low --------------

# 50%
top30_biod_priority_areas_regen <- top30_biod %>%
  group_by(regen_05 ) %>%
  distinct()%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority)


print(head(top30_biod_priority_areas_regen, n = 20), width = Inf)

# 40%
top30_biod_priority_areas_regen_40 <- top30_biod_40 %>%
  group_by(regen_04 ) %>%
  distinct()%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority)


print(head(top30_biod_priority_areas_regen_40, n = 20), width = Inf)

# 60%
top30_biod_priority_areas_regen_60 <- top30_biod_60 %>%
  group_by(regen_06 ) %>%
  distinct()%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority)


print(head(top30_biod_priority_areas_regen_60, n = 20), width = Inf)

#---- summarise it per country ----

# 50
top30_biod_priority_areas_regen_country <- top30_biod %>%
  group_by(regen_05,country_name ) %>%
  distinct()%>%
  filter(regen_05 ==1)%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority) %>%
  arrange(desc(prop)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_prop = cumsum(prop))

print(head(top30_biod_priority_areas_regen_country, n = 6), width = Inf)
# Nicaragua, colombia, mexico, indoneia, Brazil

# 40
top30_biod_priority_areas_regen_country_40 <- top30_biod_40 %>%
  group_by(regen_04,country_name ) %>%
  distinct()%>%
  filter(regen_04 ==1)%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority) %>%
  arrange(desc(prop)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_prop = cumsum(prop))

print(head(top30_biod_priority_areas_regen_country_40, n = 6), width = Inf)
# top 5: nicaragua, colombia, mexico, indonesia, brazil

# 60
top30_biod_priority_areas_regen_country_60 <- top30_biod_60 %>%
  group_by(regen_06,country_name ) %>%
  distinct()%>%
  filter(regen_06 ==1)%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority) %>%
  arrange(desc(prop)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_prop = cumsum(prop))

print(head(top30_biod_priority_areas_regen_country_60, n = 6), width = Inf)
# Nicaragua, Mexico, Madagascar, Indonesia, Colombia

# ---- stats for carbon ----

# total area on top 30% of carbon priority areas

# 50%
total_priority_C <- top30_carbon %>%
  select(country_name,total_priority_area ) %>%
  distinct()%>%
  ungroup()%>%
  summarise(total_priority = sum(total_priority_area, na.rm = TRUE), .groups = "drop")%>%
  pull()


# total priority area per country

top30_carbon <- top30_carbon %>%
  mutate(prop_priority = total_priority_area / total_priority_C,
         prop_resorable = priority_restorable_area_by_regen_km / total_priority_C)


top30_carbon_priority_areas <-  top30_carbon %>%
  select(country_name,prop_priority ) %>%
  distinct()%>%
  # arrange by priority area)
  arrange(desc(prop_priority)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = cumsum(prop_priority))

print(head(top30_carbon_priority_areas, n = 20), width = Inf)

#---- summarise how much is high regen potential and how much is low, per country-

# 50%
top30_carbon_priority_areas_regen <- top30_carbon %>%
  group_by(regen_05 ) %>%
  distinct()%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_C)


print(head(top30_carbon_priority_areas_regen, n = 20), width = Inf)


# 40%
top30_carbon_priority_areas_regen_40 <- top30_carbon_40 %>%
  group_by(regen_04 ) %>%
  distinct()%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_C)


print(head(top30_carbon_priority_areas_regen_40, n = 6), width = Inf)

# 60%
top30_carbon_priority_areas_regen_60 <- top30_carbon_60 %>%
  group_by(regen_06 ) %>%
  distinct()%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_C)


print(head(top30_carbon_priority_areas_regen_60, n = 6), width = Inf)

#---- summarise per country ------
# 50%
top30_carbon_priority_areas_regen_country <- top30_carbon %>%
  group_by(regen_05,country_name ) %>%
  distinct()%>%
  filter(regen_05 ==1)%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_C) %>%
  arrange(desc(prop)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_prop = cumsum(prop))

print(head(top30_carbon_priority_areas_regen_country, n = 5), width = Inf)
# Brazil, colombia, mexico, nicaragua, indonesia

# 40%
top30_carbon_priority_areas_regen_country_40 <- top30_carbon_40 %>%
  group_by(regen_04,country_name ) %>%
  distinct()%>%
  filter(regen_04 ==1)%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_C) %>%
  arrange(desc(prop)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_prop = cumsum(prop))

print(head(top30_carbon_priority_areas_regen_country_40, n = 5), width = Inf)
# brazil, colombia, Mexico, Nicaragua, Indonesia

# 60%
top30_carbon_priority_areas_regen_country_60 <- top30_carbon_60 %>%
  group_by(regen_06,country_name ) %>%
  distinct()%>%
  filter(regen_06 ==1)%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_C) %>%
  arrange(desc(prop)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_prop = cumsum(prop))

print(head(top30_carbon_priority_areas_regen_country_60, n = 6), width = Inf)
# Brazil, nicaracgua, mexico, colombia, guatemala

# --- stats for costs ---

# total area on top 30% of carbon priority areas

total_priority_oc <- top30_costs %>%
  select(country_name,total_priority_area ) %>%
  distinct()%>%
  ungroup()%>%
  summarise(total_priority = sum(total_priority_area, na.rm = TRUE), .groups = "drop")%>%
  pull()


# total priority area per country

top30_costs <- top30_costs %>%
  mutate(prop_priority = total_priority_area / total_priority_oc,
         prop_resorable = priority_restorable_area_by_regen_km / total_priority_oc)


top30_costs_priority_areas <-  top30_costs %>%
  select(country_name,prop_priority ) %>%
  distinct()%>%
  # arrange by priority area)
  arrange(desc(prop_priority)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = cumsum(prop_priority))

print(head(top30_costs_priority_areas, n = 20), width = Inf)

#---- summarise how much is high regen potential and how much is low, per country -

# 50%
top30_costs_priority_areas_regen <- top30_costs %>%
  group_by(regen_05 ) %>%
  distinct()%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_oc)


print(head(top30_costs_priority_areas_regen, n = 20), width = Inf)

# 40%
top30_costs_priority_areas_regen_40 <- top30_costs_40 %>%
  group_by(regen_04 ) %>%
  distinct()%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_oc)


print(head(top30_costs_priority_areas_regen_40, n = 20), width = Inf)

# 60%
top30_costs_priority_areas_regen_60 <- top30_costs_60 %>%
  group_by(regen_06 ) %>%
  distinct()%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_oc)


print(head(top30_costs_priority_areas_regen_60, n = 5), width = Inf)

# ---- summarise per country ------
#50%
top30_costs_priority_areas_regen_country <- top30_costs %>%
  group_by(regen_05,country_name ) %>%
  distinct()%>%
  filter(regen_05 ==1)%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_oc) %>%
  arrange(desc(prop)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_prop = cumsum(prop))

print(head(top30_costs_priority_areas_regen_country, n = 5), width = Inf)
# mexico, brazil, colombia, madagascar, nicaragua

#40%
top30_costs_priority_areas_regen_country_40 <- top30_costs_40 %>%
  group_by(regen_04,country_name ) %>%
  distinct()%>%
  filter(regen_04 ==1)%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_oc) %>%
  arrange(desc(prop)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_prop = cumsum(prop))

print(head(top30_costs_priority_areas_regen_country_40, n = 7), width = Inf)
# brazil, mexico, colombia, madagascar, nicaragua

#60%
top30_costs_priority_areas_regen_country_60 <- top30_costs_60 %>%
  group_by(regen_06,country_name ) %>%
  distinct()%>%
  filter(regen_06 ==1)%>%
  summarise(total = sum(priority_restorable_area_by_regen_km))%>%
  mutate(prop = total/total_priority_oc) %>%
  arrange(desc(prop)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_prop = cumsum(prop))

print(head(top30_costs_priority_areas_regen_country_60, n = 5), width = Inf)
# mexico, brazil, colombia,  nicaragua, madagascar