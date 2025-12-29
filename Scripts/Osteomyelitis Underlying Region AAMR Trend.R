source(".Rprofile")

# Load region-specific datasets
northeast_data <- read_excel(
  "/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Region_Underlying_Mortality_Trend.xlsx",
  sheet = "Northeast"
)

midwest_data <- read_excel(
  "/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Region_Underlying_Mortality_Trend.xlsx",
  sheet = "Midwest"
)

south_data <- read_excel(
  "/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Region_Underlying_Mortality_Trend.xlsx",
  sheet = "South"
)

west_data <- read_excel(
  "/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Region_Underlying_Mortality_Trend.xlsx",
  sheet = "West"
)

# Add region labels
northeast_data <- northeast_data %>% mutate(AAMR = as.numeric(AAMR), Region = "Northeast")
midwest_data   <- midwest_data   %>% mutate(AAMR = as.numeric(AAMR), Region = "Midwest")
south_data     <- south_data     %>% mutate(AAMR = as.numeric(AAMR), Region = "South")
west_data      <- west_data      %>% mutate(AAMR = as.numeric(AAMR), Region = "West")

# Combine all regions
combined_data <- bind_rows(
  northeast_data,
  midwest_data,
  south_data,
  west_data
)

# Fit log-linear models and compute APCs
apc_values <- combined_data %>%
  group_by(Region) %>%
  do({
    model <- lm(log(AAMR) ~ Year, data = .)
    conf_int <- confint(model)["Year", ]
    beta1 <- coef(model)["Year"]
    
    data.frame(
      APC = (exp(beta1) - 1) * 100,
      APC_lower = (exp(conf_int[1]) - 1) * 100,
      APC_upper = (exp(conf_int[2]) - 1) * 100,
      p_value = summary(model)$coefficients["Year", "Pr(>|t|)"]
    )
  }) %>%
  ungroup()

# Manual label positions (adjust if needed)
label_y_positions <- c(
  "Northeast" = 3.8,
  "Midwest"   = 3.5,
  "South"     = 3.2,
  "West"      = 2.9
)

apc_labels <- apc_values %>%
  mutate(
    label = paste0("APC (", Region, "): ", round(APC, 2), "%"),
    label_x = min(combined_data$Year) + 1,
    label_y = label_y_positions[Region]
  )

# Region colors and shapes
region_colors <- c(
  "Northeast" = "#1f77b4",
  "Midwest"   = "#ff7f0e",
  "South"     = "#d62728",
  "West"      = "#2ca02c"
)

region_shapes <- c(
  "Northeast" = 21,
  "Midwest"   = 22,
  "South"     = 23,
  "West"      = 24
)

# Plot
p <- ggplot(
  combined_data,
  aes(x = Year, y = AAMR, color = Region, shape = Region, fill = Region)
) +
  geom_point(size = 3, stroke = 1.1) +
  geom_smooth(method = "lm", se = FALSE, size = 1.1, show.legend = FALSE) +
  geom_text(
    data = apc_labels,
    aes(x = label_x, y = label_y, label = label, color = Region),
    hjust = 0, size = 5, fontface = "italic", inherit.aes = FALSE
  ) +
  scale_color_manual(values = region_colors) +
  scale_fill_manual(values = region_colors) +
  scale_shape_manual(values = region_shapes) +
  scale_x_continuous(breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  labs(
    x = "Year",
    y = "AAMR (per 100,000)"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.title = element_blank(),
    legend.position = "top"
  )

print(p)

# Save high-resolution figure
ggsave(
  "/Users/ksanaka/Desktop/Research/Osteomyelitis/Figures/AAMR_Region_Trend_Plot.png",
  plot = p,
  width = 7,
  height = 6,
  dpi = 900
)

rm(list = ls())
