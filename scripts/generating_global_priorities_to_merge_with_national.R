# ---- load packages ----
library(tidyverse)
library(countrycode)
library(naniar)
#library(dplyr)
library(forcats)
# library(ggpubr)
# library(ggplot2)
#library(dplyr)
# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
# ---- load data -----------

#read in table of values
df <- readRDS("output_tables/full_dataframe_carb_bio_opp_regen.rds")


# correct country names
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
# remove ares with NA for countries
df <- df %>% filter(!is.na(country_name))

#calculate quantiles for carbon, biodiversity
# Assign decile bins (Q1 to Q10)
df_global <- df %>%
  mutate(
    carbon_decile = ntile(carbon, 10),
    biodiversity_decile =  ntile(biodiversity, 10), 
    oppcost_decile = ntile(opp_cost, 10)
  )

# 307,676,066

# assign regen potential thresholds
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

# calculate opp_cost in km2
df_global$opp_cost <- df_global$opp_cost *100
df_global$opp_cost <- df_global$opp_cost * df_global$area_restorable_km2

# calculate carbon in km2!
df_global$carbon <- df_global$carbon *100
df_global$carbon <- df_global$carbon * df_global$area_restorable_km2

#---- calculate total restorable area (global) ----

total_restorable <- df_global %>%
  summarise(total_restorable_km = sum(area_restorable_km2))%>%
  pull()


#calculate total restorable area by country
tot_restor_country <- df_global %>%
  select(country_name, area_restorable_km2) %>%
  group_by(country_name) %>%
  summarise(country_restorable_area_km = sum(area_restorable_km2, na.rm = TRUE), .groups = "drop")

# proportion of total restoration in each country
tot_restor_country$prop_restor <- round(tot_restor_country$country_restorable_area_km / total_restorable,4)

# calculate cummulative restoration proportion

tot_restor_country <- tot_restor_country%>%
  arrange(desc(prop_restor)) %>%
  mutate(cum_prop_restor = cumsum(prop_restor),
         country_restorable_area_1000km = round(country_restorable_area_km/1000,4))

#---- calculate top 30% for the biodiversity, carbon, costs ----

# function to generate priority table for the top 30% by high and low pot!

make_priority_table <- function(df, criteria = NULL, threshold = NULL) {
  
  
  regen_col <- sym(paste0("regen_", threshold))
  
  # filtering the top 30% 
  # for biodiversity
  if (criteria == "biodiversity") {
    
    filter_by <- "biodiversity_decile"
    
    total_by_country <- df %>%
      filter(.data[[filter_by]] <= 3) %>%
      group_by(country_name, !!regen_col) %>%
      summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2, na.rm = TRUE),
                biodiversity = sum(biodiversity,na.rm = T),
                carbon = sum(carbon,na.rm = T),
                opp_cost = sum(opp_cost,na.rm = T))
    # for carbon
  } else if (criteria == "carbon") {
    filter_by <- "carbon_decile"
    
    total_by_country <- df %>%
      filter(.data[[filter_by]] >= 7) %>%
      group_by(country_name, !!regen_col) %>%
      summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2, na.rm = TRUE),
                biodiversity = sum(biodiversity,na.rm = T),
                carbon = sum(carbon,na.rm = T),
                opp_cost = sum(opp_cost,na.rm = T))
  } 
  # for opp. cost
  else if (criteria == "opp_cost") {
    filter_by <- "oppcost_decile"
    
    # summarise by country (the total should be the sum of this table!) teh top 30%
    total_by_country <- df %>%
      filter(.data[[filter_by]] <= 3) %>%
      group_by(country_name, !!regen_col) %>%
      summarise(priority_restorable_area_by_regen_km = sum(area_restorable_km2, na.rm = TRUE),
                biodiversity = sum(biodiversity,na.rm = T),
                carbon = sum(carbon,na.rm = T),
                opp_cost = sum(opp_cost,na.rm = T))
  } else {
    stop("Invalid criteria specified. Use 'biodiversity', 'carbon', or 'opp_cost'.")
  }
  
  #  total area disconsidering the 30%
  total_summary <- total_by_country %>%
    group_by(country_name) %>%
    summarise(total_priority_area = sum(priority_restorable_area_by_regen_km), .groups = "drop") %>%
    left_join(tot_restor_country, by = "country_name")
  
  top30 <- left_join(total_by_country, total_summary, by = "country_name")
  
  return(top30)
}


#---- generate tables for total restorable areas ----
# selecting the top 30% areas per country, considering high and low prop. of 
# nat. regen
# 50%
top30_biod_50 <- make_priority_table(df_global, criteria = "biodiversity",threshold = "05") 
top30_carbon_50 <- make_priority_table(df_global, criteria = "carbon",threshold = "05") 
top30_costs_50 <- make_priority_table(df_global, criteria = "opp_cost",threshold = "05")

# 40%
top30_biod_40 <- make_priority_table(df_global, criteria = "biodiversity",threshold = "04") 
top30_carbon_40 <- make_priority_table(df_global, criteria = "carbon",threshold = "04") 
top30_costs_40 <- make_priority_table(df_global, criteria = "opp_cost",threshold = "04")

# 60%
top30_biod_60 <- make_priority_table(df_global, criteria = "biodiversity",threshold = "06") 
top30_carbon_60 <- make_priority_table(df_global, criteria = "carbon",threshold = "06") 
top30_costs_60 <- make_priority_table(df_global, criteria = "opp_cost",threshold = "06")


# # generate a table of total restorable areas (here the treshold does not matter!!)
# # can't really drop the zeros!!
# 
total_bio <- top30_biod_50 %>%
  select(priority_restorable_area_by_regen_km,country_restorable_area_1000km,biodiversity,carbon,opp_cost)%>%
  ungroup()%>%
  summarise(area_top30_1000km2 = sum(priority_restorable_area_by_regen_km/1000),
            LIFE =  sum(biodiversity),
            carbon = sum(carbon),
            opp_cost = sum(opp_cost))%>%
  mutate(priority= "biodiversity")

total_carbon <- top30_carbon_50 %>%
  select(priority_restorable_area_by_regen_km,country_restorable_area_1000km,biodiversity,carbon,opp_cost)%>%
  ungroup()%>%
  summarise(area_top30_1000km2 = sum(priority_restorable_area_by_regen_km/1000),
            LIFE =  sum(biodiversity),
            carbon = sum(carbon),
            opp_cost = sum(opp_cost))%>%
  mutate(priority= "carbon")



total_costs <- top30_costs_50 %>%
  select(priority_restorable_area_by_regen_km,country_restorable_area_1000km,biodiversity,carbon,opp_cost)%>%
  ungroup()%>%
  summarise(area_top30_1000km2 = sum(priority_restorable_area_by_regen_km/1000),
            LIFE =  sum(biodiversity),
            carbon = sum(carbon),
            opp_cost = sum(opp_cost))%>%
  mutate(priority= "cost")

# # combine all
# 
# top_30_combined <- rbind(total_bio,total_carbon,total_costs)


# # export
# write.csv(top_30_combined,"output_tables/total_benefits_top30.csv",row.names = F)

#---- total top30 per country --------------------------------------------------

# here I only need proportional area to the total top

total_top30_area_bio <- total_bio %>%
  pull(area_top30_1000km2)

total_top30_area_C <- total_carbon %>%
  pull(area_top30_1000km2)

total_top30_area_oc <- total_costs %>%
  pull(area_top30_1000km2)

total_bio_country <- top30_biod_50 %>%
  select(priority_restorable_area_by_regen_km,country_restorable_area_1000km,biodiversity,carbon,opp_cost)%>%
  group_by(country_name)%>%
  summarise(area_top30_1000km2 = sum(priority_restorable_area_by_regen_km/1000),
            #LIFE =  sum(biodiversity),
            #carbon = sum(carbon),
            #opp_cost = sum(opp_cost)
            )%>%
  mutate(
    proportion = round(area_top30_1000km2/total_top30_area_bio,4),
    priority= "biodiversity")


total_carbon_country <- top30_carbon_50 %>%
  select(priority_restorable_area_by_regen_km,country_restorable_area_1000km,biodiversity,carbon,opp_cost)%>%
  group_by(country_name)%>%
  summarise(
    area_top30_1000km2 = sum(priority_restorable_area_by_regen_km/1000),
            #LIFE =  sum(biodiversity),
            #carbon = sum(carbon),
            #opp_cost = sum(opp_cost)
    )%>%
  mutate(
    proportion = round(area_top30_1000km2/total_top30_area_C,4),
    priority= "carbon"
    )



total_costs_country <- top30_costs_50 %>%
  select(priority_restorable_area_by_regen_km,country_restorable_area_1000km,biodiversity,carbon,opp_cost)%>%
  group_by(country_name)%>%
  summarise(
    area_top30_1000km2 = sum(priority_restorable_area_by_regen_km/1000),
    #LIFE =  sum(biodiversity),
    #carbon = sum(carbon),
    #opp_cost = sum(opp_cost)
    )%>%
  mutate(
    proportion = round(area_top30_1000km2/total_top30_area_C,4),
    priority= "cost"
    )

# combine all, slice only top 30 countries and make a long format ordered by
# country area and B, C OC!

top_30_country_combined <- rbind(total_bio_country,total_carbon_country,total_costs_country)

# select only the top 30 countries and order it by country and b, C, oc

# prepare top 30 countries to export. It needs to be always the same!!

countries_30 <- tot_restor_country%>%
  arrange(desc(country_restorable_area_1000km)) %>%
  slice(1:30) %>%
  pull(country_name)

#---- generate tables for proportion of the areas with high nat. regen. pot ----

# variables: C,B,Cost  x tresholds
# get proportion (and areas) of nat reg. (compared with the total) for each prio-
#rity and treshold

top_30_area <- sum(top30_biod_50$priority_restorable_area_by_regen_km)


# 1) for Biodiversity and 0.5

prop_high_reg_05_bio <- top30_biod_50 %>%
  filter(regen_05 == 1) %>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(prop_high = round(priority_restorable_area_by_regen_km/top_30_area,4))%>%
  # arrange by priority area)
  arrange(desc(prop_high)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = round(cumsum(prop_high),4))%>%
  # this needs to be dune following the order of area!!
  ungroup() %>%   # <–– ensures no grouping
  mutate(cum_priority = round(cumsum(prop_high),4),
         priority = "biodiversity",
         threshold = "0.5",
         rank = row_number()) 

# 2) for Biodiversity and 0.4

prop_high_reg_04_bio <- top30_biod_40 %>%
  filter(regen_04 == 1) %>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(prop_high = round(priority_restorable_area_by_regen_km/top_30_area,4))%>%
  # arrange by priority area)
  arrange(desc(prop_high)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = round(cumsum(prop_high),4))%>%
  # this needs to be dune following the order of area!!
  ungroup() %>%   # <–– ensures no grouping
  mutate(cum_priority = round(cumsum(prop_high),4),
         priority = "biodiversity",
         threshold = "0.4",
         rank = row_number()) 

# 3) for Biodiversity and 0.6
prop_high_reg_06_bio <- top30_biod_60 %>%
  filter(regen_06 == 1) %>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(prop_high = round(priority_restorable_area_by_regen_km/top_30_area,4))%>%
  # arrange by priority area)
  arrange(desc(prop_high)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = round(cumsum(prop_high),4))%>%
  # this needs to be dune following the order of area!!
  ungroup() %>%   # <–– ensures no grouping
  mutate(cum_priority = round(cumsum(prop_high),4),
         priority = "biodiversity",
         threshold = "0.6",
         rank = row_number()) 

# 4) for C and 0.5

prop_high_reg_05_C <- top30_carbon_50 %>%
  filter(regen_05 == 1) %>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(prop_high = round(priority_restorable_area_by_regen_km/top_30_area,4))%>%
  # arrange by priority area)
  arrange(desc(prop_high)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = round(cumsum(prop_high),4))%>%
  # this needs to be dune following the order of area!!
  ungroup() %>%   # <–– ensures no grouping
  mutate(cum_priority = round(cumsum(prop_high),4),
         priority = "carbon",
         threshold = "0.5",
         rank = row_number()) 

# 5) for C and 0.4

prop_high_reg_04_C <- top30_carbon_40 %>%
  filter(regen_04 == 1) %>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(prop_high = round(priority_restorable_area_by_regen_km/top_30_area,4))%>%
  # arrange by priority area)
  arrange(desc(prop_high)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = round(cumsum(prop_high),4))%>%
  # this needs to be dune following the order of area!!
  ungroup() %>%   # <–– ensures no grouping
  mutate(cum_priority = round(cumsum(prop_high),4),
         priority = "carbon",
         threshold = "0.4",
         rank = row_number()) 

# 6) for C and 0.6
prop_high_reg_06_C <- top30_carbon_60 %>%
  filter(regen_06 == 1) %>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(prop_high = round(priority_restorable_area_by_regen_km/top_30_area,4))%>%
  # arrange by priority area)
  arrange(desc(prop_high)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = round(cumsum(prop_high),4))%>%
  # this needs to be dune following the order of area!!
  ungroup() %>%   # <–– ensures no grouping
  mutate(cum_priority = round(cumsum(prop_high),4),
         priority = "carbon",
         threshold = "0.6",
         rank = row_number()) 

# 7) for costs and 0.5

prop_high_reg_05_oc <- top30_costs_50 %>%
  filter(regen_05 == 1) %>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(prop_high = round(priority_restorable_area_by_regen_km/top_30_area,4))%>%
  # arrange by priority area)
  arrange(desc(prop_high)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = round(cumsum(prop_high),4))%>%
  # this needs to be dune following the order of area!!
  ungroup() %>%   # <–– ensures no grouping
  mutate(cum_priority = round(cumsum(prop_high),4),
         priority = "cost",
         threshold = "0.5",
         rank = row_number()) 

# 8) for cost and 0.4

prop_high_reg_04_oc <- top30_costs_40 %>%
  filter(regen_04 == 1) %>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(prop_high = round(priority_restorable_area_by_regen_km/top_30_area,4))%>%
  # arrange by priority area)
  arrange(desc(prop_high)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = round(cumsum(prop_high),4))%>%
  # this needs to be dune following the order of area!!
  ungroup() %>%   # <–– ensures no grouping
  mutate(cum_priority = round(cumsum(prop_high),4),
         priority = "cost",
         threshold = "0.4",
         rank = row_number()) 

# 9) for C and 0.6
prop_high_reg_06_oc <- top30_costs_60 %>%
  filter(regen_06 == 1) %>%
  select(country_name,priority_restorable_area_by_regen_km) %>%
  mutate(prop_high = round(priority_restorable_area_by_regen_km/top_30_area,4))%>%
  # arrange by priority area)
  arrange(desc(prop_high)) %>%
  # this needs to be dune following the order of area!!
  mutate(cum_priority = round(cumsum(prop_high),4))%>%
  # this needs to be dune following the order of area!!
  ungroup() %>%   # <–– ensures no grouping
  mutate(cum_priority = round(cumsum(prop_high),4),
         priority = "cost",
         threshold = "0.6",
         rank = row_number()) 


#---- combine ------------------------------------------------------------------

top30_high_bio_combined <- rbind(prop_high_reg_05_bio,prop_high_reg_06_bio,prop_high_reg_04_bio)%>%
  # add total restorable area
  left_join(tot_restor_country[,c(1,5)])%>%
  mutate(high_prop_restorable_1000 = round(priority_restorable_area_by_regen_km/1000,4))

top30_high_C_combined <- rbind(prop_high_reg_05_C,prop_high_reg_06_C,prop_high_reg_04_C)%>%
  # add total restorable area
  left_join(tot_restor_country[,c(1,5)])%>%
  mutate(high_prop_restorable_1000 = round(priority_restorable_area_by_regen_km/1000,4))

top30_high_oc_combined <- rbind(prop_high_reg_05_oc,prop_high_reg_06_oc,prop_high_reg_04_oc)%>%
  # add total restorable area
  left_join(tot_restor_country[,c(1,5)])%>%
  mutate(high_prop_restorable_1000 = round(priority_restorable_area_by_regen_km/1000,4))

# # I can get totals...perhaps total combining all variables!!
# 
# bio_sum <- top30_high_bio_combined %>%
#   group_by(priority,threshold)%>%
#   summarise(overlap = sum(prop_high),
#             area_restorable = sum(priority_restorable_area_by_regen_km))
# 
# C_sum <- top30_high_C_combined %>%
#   group_by(priority,threshold)%>%
#   summarise(overlap = sum(prop_high),
#             area_restorable = sum(priority_restorable_area_by_regen_km))
# 
# oc_sum <- top30_high_oc_combined %>%
#   group_by(priority,threshold)%>%
#   summarise(overlap = sum(prop_high),
#             area_restorable = sum(priority_restorable_area_by_regen_km))
# 
# 
# combined <- bind_rows(bio_sum, C_sum, oc_sum) %>%
#   mutate(area_restorable_1000km= area_restorable/1000)
#   
# 
# # # save the totals combined!!
# 
# write.csv(combined,"output_tables/high_nr_overlap_top_areas.csv",row.names = F)



# for biodiversity

top30_high_bio_combined_30countries <- top30_high_bio_combined %>%
  filter(country_name %in% countries_30)

# for C

top30_high_C_combined_30countries <- top30_high_C_combined %>%
  filter(country_name %in% countries_30)

# for oc

top30_high_oc_combined_30countries <- top30_high_oc_combined %>%
  filter(country_name %in% countries_30)


# combine 

full_data <- rbind(top30_high_bio_combined_30countries,
                   top30_high_C_combined_30countries,
                   top30_high_oc_combined_30countries)


# arrange to save. then create another script where the deciles are normalized
# per country!!

full_data <- full_data  %>%
  select(country_name,priority,threshold,country_restorable_area_1000km,high_prop_restorable_1000)%>%
  mutate(scale = "global")




write.csv(full_data,"full_data_to_combine_with_nationals.csv",row.names = F)





