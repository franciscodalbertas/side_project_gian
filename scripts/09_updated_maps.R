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


# will redo all the above to zoom over ecoregions !

# box for caribbean and central america
box_caribbean <- world %>%
  filter(subregion%in% c("Caribbean","Central America"))
 

# I will need to cut mexio in half as there is not much in it
bbox_caribbean <- st_bbox(box_caribbean)

# Convert bbox to an sf polygon so it can be drawn
bbox_poly_caribbean <- st_as_sfc(bbox_caribbean)


# box for Andean region (??- will need to name it)
#"VEN","COL",


box_eastern_SA <- world %>%
  filter(adm0_a3%in% c("VEN","COL","ECU","PER","BOL"))


bbox_eastern_SA <- st_bbox(box_eastern_SA)

# Convert bbox to an sf polygon so it can be drawn
bbox_poly_eastern_SA <- st_as_sfc(bbox_eastern_SA)

# Difference in y (in degrees) so the box don't overlap
shift_deg <- bbox_caribbean["ymin"] - bbox_eastern_SA["ymax"]

bbox_esa_shifted <- st_geometry(bbox_poly_eastern_SA) + c(0, shift_deg)
# fix crs
st_crs(bbox_esa_shifted) <- st_crs(bbox_poly_eastern_SA)

# box for AF 

af <- read_biomes(year = 2019) %>%
  filter(code_biome ==4)


bbox_af <- st_bbox(af)


bbox_poly_af <- st_as_sfc(bbox_af)

# box madagascar

box_madag <- world %>%
  filter(adm0_a3%in% c("MDG"))

# box for madagascar

box_madag <- world %>%
  filter(adm0_a3%in% c("MDG"))

# I will need to cut mexio in half as there is not much in it
bbox_madag <- st_bbox(box_madag)

# Convert bbox to an sf polygon so it can be drawn
bbox_poly_madag <- st_as_sfc(bbox_madag)

# box for central africa (DRC,Uganda, QUENIA)

box_eafr <- world %>%
  filter(sovereignt%in% c("Democratic Republic of the Congo",
                         "Kenya",
                         "Uganda"))

bbox_eafr <- st_bbox(box_eafr)

bbox_poly_eafr <- st_as_sfc(bbox_eafr)

# Narrow a bbox polygon by moving its left edge right and bottom edge up
narrow_top_right <- function(rect_sfc, dx_right = 20, dy_up = 20) {
  if (is.na(st_crs(rect_sfc))) stop("Box has no CRS set.")
  bb <- st_bbox(st_transform(rect_sfc, 4326))  # work in degrees
  
  bb["xmin"] <- bb["xmin"] + dx_right  # move left edge to the right
  bb["ymin"] <- bb["ymin"] + dy_up     # move bottom edge up
  
  # guard against inverted boxes
  if (bb["xmin"] >= bb["xmax"] || bb["ymin"] >= bb["ymax"]) {
    stop("Narrowing collapses the box (check dx/dy values).")
  }
  
  st_as_sfc(bb, crs = st_crs(bbox_af_eafr)) |> st_transform(st_crs(rect_sfc))
}

# narrow 20° up and 20° right (i.e., shrink from bottom & left)
bbox_poly_eafr_n  <- narrow_top_right(bbox_poly_eafr, dx_right = 10, dy_up = 10)

# southern asia (India+Sri lanka)
box_sa <- world %>%
  filter(sovereignt%in% c("India",
                          "Sri Lanka"))

bbox_sa <- st_bbox(box_sa)

bbox_poly_sa <- st_as_sfc(bbox_sa)

# southeast-asia
box_sea <- world %>%
  filter(subregion%in% c("South-Eastern Asia"))

bbox_sea <- st_bbox(box_sea)

bbox_poly_sea <- st_as_sfc(bbox_sea)

# Difference in y (in degrees) so the box don't overlap
shift_deg_Asia <- bbox_sa["xmax"] - bbox_sea["xmin"]

bbox_sa_shifted <- st_geometry(bbox_poly_sa) - c(shift_deg_Asia,0)
# fix crs
st_crs(bbox_sa_shifted) <- st_crs(bbox_poly_sea)




# cap_to_tropics <- function(rect_sfc, lat = 23.4366) {
#   # ensure geographic CRS (degrees)
#   if (is.na(st_crs(rect_sfc))) stop("Box has no CRS set.")
#   rect_ll <- st_transform(rect_sfc, 4326)
#   
#   bb <- st_bbox(rect_ll)
#   bb["ymin"] <- max(bb["ymin"], -lat)
#   bb["ymax"] <- min(bb["ymax"],  lat)
#   
#   # if the box ends up inverted (entirely outside tropics), return empty
#   if (bb["ymin"] >= bb["ymax"]) return(st_sfc(geometrycollection(), crs = 4326))
#   
#   st_as_sfc(bb, crs = 4326) |> st_transform(st_crs(rect_sfc))
# }


cap_to_tropics <- function(rect_sfc, lat = 23.4366, lon_min = -110, lon_max = 130) {
  if (is.na(st_crs(rect_sfc))) stop("Box has no CRS set.")
  crs0 <- st_crs(rect_sfc)
  
  # work in lon/lat
  rect_ll <- st_transform(rect_sfc, 4326)
  
  bb <- st_bbox(rect_ll)
  # cap lat
  bb["ymin"] <- max(bb["ymin"], -lat)
  bb["ymax"] <- min(bb["ymax"],  lat)
  # cap lon
  bb["xmin"] <- max(bb["xmin"], lon_min)
  bb["xmax"] <- min(bb["xmax"], lon_max)
  
  # empty if completely outside caps
  if (bb["ymin"] >= bb["ymax"] || bb["xmin"] >= bb["xmax"]) {
    return(st_sfc(st_geometrycollection(), crs = crs0))
  }
  
  st_as_sfc(bb, crs = 4326) |> st_transform(crs0)
}


# Apply to boxes
bbox_caribbean_trop <- cap_to_tropics(bbox_poly_caribbean)
bbox_esa_trop       <- cap_to_tropics(bbox_esa_shifted)
bbox_mdg_trop       <- cap_to_tropics(bbox_poly_madag)
bbox_af_trop       <- cap_to_tropics(bbox_poly_af)
bbox_eafr_trop       <- cap_to_tropics(bbox_poly_eafr_n)
bbox_sa_trop       <- cap_to_tropics(bbox_sa_shifted)
bbox_sea_trop       <- cap_to_tropics(bbox_poly_sea)

# Place a label 1° left and 1° down from the top-left corner of a box
lab_pos_deg <- function(poly, lab, dx = +2, dy = -2) {
  if (length(poly) == 0 || all(st_is_empty(poly))) return(NULL)
  bb <- st_bbox(st_transform(poly, 4326))  # ensure degrees
  tibble(
    x = as.numeric(bb["xmin"] + dx),  # left: negative dx
    y = as.numeric(bb["ymax"] + dy),  # down: negative dy
    label = lab
  )
}

labels_df <- bind_rows(
  lab_pos_deg(bbox_caribbean_trop, "A"),
  lab_pos_deg(bbox_esa_trop,       "B"),
  lab_pos_deg(bbox_mdg_trop,       "E"),
  lab_pos_deg(bbox_af_trop,        "C"),
  lab_pos_deg(bbox_eafr_trop,      "D"),
  lab_pos_deg(bbox_sa_trop,        "F"),
  lab_pos_deg(bbox_sea_trop,       "G")
)


ggplot() +
  geom_sf(data = world, fill = "lightblue", color = "black") + # world map
  geom_sf(data = bbox_caribbean_trop, fill = NA, color = "black", size = 1) + # bbox outline
  geom_sf(data = bbox_esa_trop, fill = NA, color = "black", size = 1)+
  geom_sf(data = bbox_mdg_trop, fill = NA, color = "black", size = 1)+
  geom_sf(data = bbox_af_trop, fill = NA, color = "black", size = 1)+
  geom_sf(data = bbox_eafr_trop, fill = NA, color = "black", size = 1)+
  geom_sf(data = bbox_sa_trop, fill = NA, color = "black", size = 1)+
  geom_sf(data = bbox_sea_trop, fill = NA, color = "black", size = 1)+
  # Tropics lines
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-23.4366, 23.4366),
    expand = FALSE
  )+
  # labels (white halo under black text for readability)
  geom_text(data = labels_df, aes(x, y, label = label),
            color = "white", size = 5, fontface = "bold") +
  geom_text(data = labels_df, aes(x, y, label = label),
            color = "black", size = 4, fontface = "bold") +
  theme_minimal()
  

# append continent info to df_global

df_global <- df_global %>%
  left_join(world %>% select(adm0_a3, continent, subregion,region_un), by = c("adm0_a3"))

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

# generate raster to make map on QGIS!

terra::rasterize()





# need to focus in the tropics only, exclude the rest!!

bio_map <- make_regen_map(top30_biod_map,"")+
  # "Caribbean","Central America"
  geom_sf(data = bbox_caribbean_trop, fill = NA, color = "black", size = 1) + # bbox outline
  geom_sf(data = bbox_esa_trop, fill = NA, color = "black", size = 1)+
  geom_sf(data = bbox_mdg_trop, fill = NA, color = "black", size = 1)+
  geom_sf(data = bbox_af_trop, fill = NA, color = "black", size = 1)+
  geom_sf(data = bbox_eafr_trop, fill = NA, color = "black", size = 1)+
  geom_sf(data = bbox_sa_trop, fill = NA, color = "black", size = 1)+
  geom_sf(data = bbox_sea_trop, fill = NA, color = "black", size = 1)+
  # Tropics lines
  coord_sf(
    xlim = c(-110, 130),
    ylim = c(-23.4366, 23.4366),
    expand = FALSE
  )+
  # labels (white halo under black text for readability)
  geom_text(data = labels_df, aes(x, y, label = label),
            color = "white", size = 2, fontface = "bold") +
  geom_text(data = labels_df, aes(x, y, label = label),
            color = "black", size = 3, fontface = "bold")

bio_map

# now make the zoomed-in areas

# a_p <- bio_map +
#   # zoom in box A
#   coord_sf(
#     xlim = c(bbox_caribbean_trop["xmin"], bbox_caribbean_trop["xmax"]),
#     ylim = c(bbox_caribbean_trop["ymin"], bbox_caribbean_trop["ymax"]),
#     expand = FALSE
#   )

# 1) Base plot (whatever your function returns)

base <-  make_regen_map(top30_biod_map,"")+
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) 


#  put boxes in a *named* list (letters A–G as names)
boxes <- list(
  A = bbox_caribbean_trop,
  B = bbox_esa_trop,
  C = bbox_af_trop,
  D = bbox_eafr_trop,
  E = bbox_mdg_trop,
  F = bbox_sa_trop,
  G = bbox_sea_trop
)

#  Helper to zoom the base plot to a box's bbox (with optional padding in degrees)
zoom_from_box <- function(base_plot, box_sfc, pad = 0) {
  if (length(box_sfc) == 0 || all(st_is_empty(box_sfc))) return(NULL)
  # Use lon/lat degrees; adjust if your base is in a different CRS
  bb <- st_bbox(st_transform(box_sfc, 4326))
  xlim <- c(unname(bb["xmin"]) - pad, unname(bb["xmax"]) + pad)
  ylim <- c(unname(bb["ymin"]) - pad, unname(bb["ymax"]) + pad)
  base_plot + coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)+
    theme(
      plot.tag = element_text(face = "bold", size = 7),
      plot.tag.position = c(0.01, 0.99),
      plot.margin = margin(0,0,0,0),
      legend.position = "none"            # <<< hide legend in each tile
    )
}

# Build a named list of zoomed plots
zoom_plots <- imap(boxes, ~ zoom_from_box(base, .x, pad = 0))  


# no margin

theme_no_margin <- theme_void() +  # removes axes, grid, background
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 0), "pt"),  # no plot margin
    panel.spacing = unit(0, "pt")             # no spacing between panels (if faceted)
  )

# # Usage:
# # print a single one
# a_p <- zoom_plots$A +theme_no_margin
# b_p <- zoom_plots$B +theme_no_margin
# c_p <- zoom_plots$C +theme_no_margin
# d_p <- zoom_plots$D +theme_no_margin
# 
# teste <- ggarrange(plotlist = list(a_p,b_p,c_p,d_p),
#                    widths = c(1,1), 
#                    heights = c(1,1),
#                   align = "none",
#                   hjust = 0, 
#                   vjust = 0,
#                   # This is key:
#                   labels = NULL)
                   
# save top figure
(110+130)/2
width_cm  <- 18
aspect    <- 120 / (23.4366 - (-23.4366))
height_cm <- width_cm / aspect  # ~2.21 cm

# lon_min <- -110
# lon_max <-  130
# lat_cap <-  23.4366
# 
# lon_span <- lon_max - lon_min          # 240
# lat_span <- 2 * lat_cap                # 46.8732
# aspect   <- lon_span / lat_span        # ≈ 5.1202
# 
# width_cm  <- 18
# height_cm <- width_cm / aspect         # ≈ 3.125 cm
# 

# ggsave(plot = bio_map,
#        "figures/panel_plot/top_map.svg",
#        width = width_cm,
#        height = height_cm,
#        units = "cm",
#        bg = "white")

ggsave(plot = bio_map,
       "figures/panel_plot/top_map.png",
       width = width_cm,
       height = 5,
       units = "cm",
       bg = "white")

# save individual figures

list_bbox <- list(bbox_caribbean,
                  st_bbox(box_eastern_SA),
                  bbox_sa,
                  bbox_eafr,
                  bbox_madag,
                  bbox_sa,
                  bbox_sea
                  )


# bboxes: list of named numeric vectors with xmin, ymin, xmax, ymax
# width_cm: desired fixed width in cm
get_heights_from_bboxes <- function(bboxes, width_cm) {
  sapply(bboxes, function(bb) {
    dx <- bb["xmax"] - bb["xmin"]
    dy <- bb["ymax"] - bb["ymin"]
    aspect <- dx / dy
    width_cm / aspect
  })
}

zoom_width <- 5.3 # cm
heights_cm <- get_heights_from_bboxes(list_bbox, zoom_width)

# saving individual plots

name_plot <- LETTERS[seq( from = 1, to = 7 )]
for (i in seq_along(zoom_plots)) {
  p <- zoom_plots[[i]]
  ggsave(plot = p,
         paste0("figures/panel_plot/",name_plot[i],"_map.svg"),
         width = zoom_width,
         height = heights_cm[i],
         units = "cm",
         bg = "white")
  
  
}
