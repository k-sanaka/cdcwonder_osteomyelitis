source(".Rprofile")

# Load race-specific datasets
white_data    <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Race_Mortality_Trend.xlsx", sheet = "White")
black_data    <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Race_Mortality_Trend.xlsx", sheet = "Black")
hispanic_data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Race_Mortality_Trend.xlsx", sheet = "Hispanic")
asian_data    <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Race_Mortality_Trend.xlsx", sheet = "Asian")
native_data   <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Race_Mortality_Trend.xlsx", sheet = "Native American")

# Add race group labels
white_data    <- white_data    %>% mutate(AAMR = as.numeric(AAMR), Race = "White")
black_data    <- black_data    %>% mutate(AAMR = as.numeric(AAMR), Race = "Black")
hispanic_data <- hispanic_data %>% mutate(AAMR = as.numeric(AAMR), Race = "Hispanic")
asian_data    <- asian_data    %>% mutate(AAMR = as.numeric(AAMR), Race = "Asian")
native_data   <- native_data   %>% mutate(AAMR = as.numeric(AAMR), Race = "Native American")

# Combine all into one dataset
combined_data <- bind_rows(white_data, black_data, hispanic_data, asian_data, native_data)

# Fit log models and calculate APC, 95% CI, and p-value
apc_values <- combined_data %>%
  group_by(Race) %>%
  do({
    model <- lm(log(AAMR) ~ Year, data = .)
    summary_model <- summary(model)
    conf_int <- confint(model)["Year", ]
    beta1 <- coef(model)["Year"]
    
    APC       <- (exp(beta1) - 1) * 100
    APC_lower <- (exp(conf_int[1]) - 1) * 100
    APC_upper <- (exp(conf_int[2]) - 1) * 100
    p_value   <- summary_model$coefficients["Year", "Pr(>|t|)"]
    
    data.frame(
      APC = APC,
      APC_lower = APC_lower,
      APC_upper = APC_upper,
      p_value = p_value
    )
  }) %>%
  ungroup()

# Create annotation positions for APC labels (adjust Y values manually based on your scale)
label_y_positions <- c(
  "White" = 3.75,
  "Black" = 3.5,
  "Hispanic" = 3.25,
  "Asian" = 3,
  "Native American" = 2.75
)

# Merge with label positions
apc_labels <- apc_values %>%
  mutate(
    label = paste0("APC (", Race, "): ", round(APC, 2), "%"),
    label_x = min(combined_data$Year) + 1,
    label_y = label_y_positions[Race]
  )

# Define custom color and shape palettes
race_colors <- c(
  "White" = "#1f77b4",
  "Black" = "#d62728",
  "Hispanic" = "#2ca02c",
  "Asian" = "#9467bd",
  "Native American" = "#ff7f0e"
)

race_shapes <- c(
  "White" = 21,
  "Black" = 22,
  "Hispanic" = 23,
  "Asian" = 24,
  "Native American" = 25
)

# Build the plot
p <- ggplot(combined_data, aes(x = Year, y = AAMR, color = Race, shape = Race, fill = Race)) +
  geom_point(size = 3, stroke = 1.1) +
  geom_smooth(method = "lm", se = FALSE, size = 1.1, linetype = "solid", show.legend = FALSE) +
  geom_text(data = apc_labels,
            aes(x = label_x, y = label_y, label = label, color = Race),
            hjust = 0, size = 5, fontface = "italic", inherit.aes = FALSE) +
  scale_color_manual(values = race_colors) +
  scale_fill_manual(values = race_colors) +
  scale_shape_manual(values = race_shapes) +
  scale_x_continuous(breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  labs(
    x = "Year",
    y = "AAMR (per 100,000)"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "grey85", size = 0.4),
    panel.grid.minor = element_line(color = "grey90", size = 0.2),
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.5),
    legend.title = element_blank(),
    legend.position = "top"
  )

# Show plot
print(p)

# Save high-res image
ggsave(
  "/Users/ksanaka/Desktop/Research/Osteomyelitis/Figures/AAMR_Race_Trend_Plot.png",
  plot = p, width = 7, height = 6, dpi = 900
)

rm(list = ls())
