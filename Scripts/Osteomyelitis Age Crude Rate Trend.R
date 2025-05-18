source(".Rprofile")

# Load age-specific data
young_data  <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Age_Mortality_Trend.xlsx", sheet = "<35")
middle_data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Age_Mortality_Trend.xlsx", sheet = "35-64")
old_data    <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Age_Mortality_Trend.xlsx", sheet = "65+")

# Ensure Crude is numeric and label age groups
young_data  <- young_data  %>% mutate(Crude = as.numeric(Crude), AgeGroup = "0–34")
middle_data <- middle_data %>% mutate(Crude = as.numeric(Crude), AgeGroup = "35–64")
old_data    <- old_data    %>% mutate(Crude = as.numeric(Crude), AgeGroup = "65+")

# Combine all into one dataset
combined_data <- bind_rows(young_data, middle_data, old_data)

# Calculate APC, 95% CI, and p-value per age group
apc_values <- combined_data %>%
  group_by(AgeGroup) %>%
  do({
    model <- lm(log(Crude) ~ Year, data = .)
    summary_model <- summary(model)
    conf_int <- confint(model)["Year", ]
    beta1 <- coef(model)["Year"]
    
    APC <- (exp(beta1) - 1) * 100
    APC_lower <- (exp(conf_int[1]) - 1) * 100
    APC_upper <- (exp(conf_int[2]) - 1) * 100
    p_value <- summary_model$coefficients["Year", "Pr(>|t|)"]
    
    data.frame(
      APC = APC,
      APC_lower = APC_lower,
      APC_upper = APC_upper,
      p_value = p_value
    )
  }) %>%
  ungroup()

# Manually assign label positions (adjust y-values for spacing)
label_y_positions <- c(
  "0–34" = 12,
  "35–64" = 11,
  "65+"   = 10
)

# Format APC labels
apc_labels <- apc_values %>%
  mutate(
    label = paste0("APC (", AgeGroup, "): ", round(APC, 2), "%"),
    label_x = min(combined_data$Year) + 1,
    label_y = label_y_positions[AgeGroup]
  )

# Define color/shape mappings
age_colors <- c("0–34" = "#1f77b4", "35–64" = "#2ca02c", "65+" = "#d62728")
age_shapes <- c("0–34" = 21, "35–64" = 22, "65+" = 24)

# Build plot
p <- ggplot(combined_data, aes(x = Year, y = Crude, color = AgeGroup, shape = AgeGroup, fill = AgeGroup)) +
  geom_point(size = 3, stroke = 1.1) +
  geom_smooth(method = "lm", se = FALSE, size = 1.1, show.legend = FALSE) +
  geom_text(data = apc_labels,
            aes(x = label_x, y = label_y, label = label, color = AgeGroup),
            hjust = 0, size = 5, fontface = "italic", inherit.aes = FALSE) +
  scale_color_manual(values = age_colors) +
  scale_fill_manual(values = age_colors) +
  scale_shape_manual(values = age_shapes) +
  scale_x_continuous(breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  labs(
    x = "Year",
    y = "Crude (per 100,000)"
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

# Save high-res plot
ggsave(
  "/Users/ksanaka/Desktop/Research/Osteomyelitis/Figures/Crude_AgeGroup_Trend_Plot.png",
  plot = p, width = 7, height = 6, dpi = 900
)

rm(list = ls())
