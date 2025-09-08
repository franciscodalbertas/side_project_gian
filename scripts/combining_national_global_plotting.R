# ---- load packages ----
library(tidyverse)
library(ggplot2)
library(scales)

#------------------------

national <- read.csv("full_data_to_combine_with_global.csv") %>%
  mutate(high_national_1000km2= high_prop_restorable_1000)
global <- read.csv("full_data_to_combine_with_nationals.csv")%>%
  mutate(high_global_1000km2= high_prop_restorable_1000)

combined <- left_join(global%>%select(c(1:3,7)),national%>%select(c(1:3,7)))

# calculate delta

combined <- combined %>%
  mutate(delta = high_national_1000km2 - high_global_1000km2)

# get the order

total_restorable <- read.csv("output_tables/total_restorable_area_per_country.csv")

# make ordering vector
order_vec <- total_restorable %>%
  arrange(desc(country_restorable_area_km)) %>%
  pull(country_name)

# apply to plotting df
combined$country_name <- factor(combined$country_name, levels = rev(order_vec))

combined %>%
  filter(threshold == "0.5") %>%
  ggplot( aes(x = delta*1000, y = country_name, shape = priority, colour = priority)) +
    geom_point(size = 3) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    labs(
      x = "Δ (High national - High global,km²)",
      y = "Country",
      shape = "Priority"
    ) +
    theme_classic() +
    theme(
      axis.text.y = element_text(size = 9),
      axis.text.x = element_text(size = 9)
    )


# Summarise per country/priority
plot_df <- combined %>%
  filter(threshold %in% c(0.4, 0.5, 0.6)) %>%
  group_by(country_name, priority) %>%
  summarise(
    delta_mid = delta[threshold == 0.5],
    delta_low = delta[threshold == 0.4],
    delta_high = delta[threshold == 0.6],
    .groups = "drop"
  )

# ggplot(plot_df, aes(x = delta_mid * 1000,
#                     y = country_name,
#                     shape = priority,
#                     colour = priority)) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
#   geom_errorbarh(aes(xmin = delta_low * 1000,
#                      xmax = delta_high * 1000),
#                  height = 0.2) +
#   geom_point(size = 3) +
#   labs(
#     x = "Δ (High national – High global, km²)",
#     y = "Country",
#     shape = "Priority",
#     colour = "Priority"
#   ) +
#   theme_classic() +
#   theme(
#     axis.text.y = element_text(size = 9),
#     axis.text.x = element_text(size = 9)
#   )

ggplot(plot_df,
       aes(x = delta_mid * 1000,
           y = country_name,
           shape = priority,
           colour = priority)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_errorbarh(
    aes(xmin = delta_low * 1000,
        xmax = delta_high * 1000),
    height = 0.2,
    position = position_dodge(width = 1)
  ) +
  geom_point(size = 3,
             position = position_dodge(width = 1)) +
  labs(
    x = "Δ (High national – High global, km²)",
    y = "",
    shape = "Priority",
    colour = "Priority"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9)
  )

# ploting with pseudo-log

#pick a small linear window around zero; tweak sigma to taste
sigma_val <- 0.05 * max(abs(c(plot_df$delta_low, plot_df$delta_high)) * 1000, na.rm = TRUE)

# ggplot(plot_df,
#        aes(x = delta_mid * 1000,
#            y = country_name,
#            shape = priority,
#            colour = priority)) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
#   geom_errorbarh(aes(xmin = delta_low * 1000, xmax = delta_high * 1000),
#                  height = 0.2,
#                  position = position_dodge(width = 1)) +
#   geom_point(size = 3,
#              position = position_dodge(width = 1)) +
#   scale_colour_manual(values = c(
#     "biodiversity" = "blue",
#     "cost"         = "salmon",
#     "carbon"       = "forestgreen"
#   )) +
#   scale_shape_manual(values = c(
#     "biodiversity" = 16,  # solid circle
#     "cost"         = 17,  # triangle
#     "carbon"       = 15   # square
#   )) +
#   labs(
#     x = "Δ (High national – High global, km²)",
#     y = "",
#     shape = "Priority",
#     colour = "Priority"
#   ) +
#   theme_classic() +
#   theme(
#     axis.text.y = element_text(size = 9),
#     axis.text.x = element_text(size = 9)
#   )

plot <- ggplot(plot_df,
       aes(x = delta_mid * 1000,
           y = country_name,
           shape = priority,
           colour = priority)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_errorbarh(aes(xmin = delta_low * 1000, xmax = delta_high * 1000),
                 height = 0.2,
                 position = position_dodge(width = 1)) +
  geom_point(size = 3, position = position_dodge(width = 1)) +
  scale_x_continuous(
    trans  = scales::pseudo_log_trans(base = 10, sigma = sigma_val),
    breaks = scales::pretty_breaks(5),
    labels = scales::label_number(big.mark = ",")
  ) +
  scale_colour_manual(values = c(
    "biodiversity" = "blue",
    "cost"         = "salmon",
    "carbon"       = "forestgreen"
  )) +
  labs(x = "Δ (High national – High global, km²)", y = "", shape = "Priority", colour = "Priority") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7))+
  theme(legend.position = "top",
        text = element_text(size = 8) )


ggsave("Figures/GlobalvsNational.png", plot, width = 15, height = 18, 
       dpi = 300, bg = "white",units = "cm")
