
# load data

df <- readRDS("output_tables/raster_values.rds")


#take lowest 30% and highest 30% of oppcost
lowest_30_oc <- quantile(df$cost, 0.3, na.rm = TRUE)
highest_30_oc <- quantile(df$cost, 0.7, na.rm = TRUE)

#take the top 30% and bottom 30% values of LIFE
life_top30 <- quantile(df$life_val*-1, 0.7, na.rm = TRUE)

life_bottom30 <- quantile(df$life_val*-1, 0.3, na.rm = TRUE)


#take areas that are of higher  (>=50) and lower (<50) than average likihood to naturally regenerate 
natR_top50 <- ifel(natR_mosaic >= 50, natR_mosaic, NA)
natR_bottom50 <- ifel(natR_mosaic < 50, natR_mosaic, NA)

#low oppcost, high biod, high regen - these are good places to restore 
combined_mask1 <- df %>%
  filter(prob_reg>=50,
         cost<=lowest_30_oc,
         life_val>=life_top30)

# !!!!!!!!!! 2 pixels??

# this would define the pixels under those conditions
# we can add country ID or anything we want! than aggregate by country (or not to get the values)


# ex of how to plot:

# Plot as a map
ggplot(combined_mask1, aes(x = x, y = y, fill = prob_reg)) +
  geom_raster() + # Use geom_raster() for a faster alternative
  scale_fill_viridis_c() + # A color scale suitable for continuous values
  coord_equal() + # Ensures square tiles for a proper map
  theme_void() +
  labs(fill = "Value", x = "X Coordinate", y = "Y Coordinate", title = "Map Plot")
