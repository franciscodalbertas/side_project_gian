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


#---- add to carbon and bio ----

top30_biod2 <- top30_biod %>%
  filter(country_name %in%top_countries) %>%
  left_join(comm %>% select(country,single_highest_commitment_km2),
            by= join_by("country_name"=="country"))


top30_carbon2 <- top30_carbon %>%
  filter(country_name %in%top_countries) %>%
  left_join(comm %>% select(country,single_highest_commitment_km2),
            by= join_by("country_name"=="country"))


#---- ploting ------

# probably need one plot with total commitments and one with
# %...panel with 3 plots

my_theme <- theme(
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


total_area <- top30_biod2 %>%
  filter(regen_05 == 1) %>%
  mutate(
    country_name = factor(country_name, levels = rev(top_countries)),
    single_highest_commitment = single_highest_commitment_km2/ 10^3 # transform in 1000 km2
  ) %>%
  ggplot() +
  geom_col( aes(
      x = single_highest_commitment,
      y = country_name
    ),
    fill = "grey80",
    alpha = 0.7
  ) +
  scale_x_continuous(
    name = "National commitment (thousand km²)",
    labels = function(x) format(x, scientific = FALSE)
  ) +
  theme_minimal(base_size = 7) +
  my_theme


# priority area

high_regen_pot <- top30_biod2 %>%
  filter(regen_05 == 1) %>%
  mutate(
    country_name = factor(country_name, levels = rev(top_countries)),
    single_highest_commitment = single_highest_commitment_km2/ 10^3, # transform in 1000 km2
    priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km / 10^3,
    proportion = priority_restorable_area_by_regen_km/single_highest_commitment,
    proportion = if_else(proportion>1,1,proportion)
  
    ) %>%
  ggplot() +
  geom_col( aes(
    x = proportion,
    y = country_name
  ),
  fill = "#009E73",
  alpha = 0.7
  ) +
  scale_x_continuous(
    name = "Proportion in areas with high reg. pot.",
    labels = function(x) format(x, scientific = FALSE)
  ) +
  theme_minimal(base_size = 7) +
  ggtitle("Biodiversity priority areas") +
  my_theme

high_regen_pot_C <- top30_carbon2 %>%
  filter(regen_05 == 1) %>%
  mutate(
    country_name = factor(country_name, levels = rev(top_countries)),
    single_highest_commitment = single_highest_commitment_km2/ 10^3, # transform in 1000 km2
    priority_restorable_area_by_regen_km = priority_restorable_area_by_regen_km / 10^3,
    proportion = priority_restorable_area_by_regen_km/single_highest_commitment,
    proportion = if_else(proportion>1,1,proportion)
    
  ) %>%
  ggplot() +
  geom_col( aes(
    x = proportion,
    y = country_name
  ),
  fill = "#009E73",
  alpha = 0.7
  ) +
  scale_x_continuous(
    name = "Proportion in areas with high reg. pot.",
    labels = function(x) format(x, scientific = FALSE)
  ) +
  theme_minimal(base_size = 7) +
  ggtitle("Carbon priority areas") +
  my_theme



panel <- ggarrange(total_area,high_regen_pot,high_regen_pot_C,ncol = 3,
                   labels = c("A","B","C"),
                   align = "hv")


# saving

ggsave("Figures/prop_commitments.png", panel, width = 16, height = 8, 
       dpi = 300, bg = "white",units = "cm")
