source(".Rprofile")

# Load in data sets:
male_data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Sex_Mortality_Trend.xlsx", sheet = "Male")
female_data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Sex_Mortality_Trend.xlsx", sheet = "Female")

# Add sex labels
male_data <- male_data %>% mutate(Sex = "Male")
female_data <- female_data %>% mutate(Sex = "Female")

# Combine datasets
combined_data <- bind_rows(male_data, female_data)

# Fit log models and calculate APC, 95% CI, and p-value
apc_values <- combined_data %>%
  group_by(Sex) %>%
  do({
    model <- lm(log(AAMR) ~ Year, data = .)
    summary_model <- summary(model)
    conf_int <- confint(model)["Year", ]  # 95% CI for beta1
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
  })

# Merge APC values back for annotation (ungroup before assigning y-values)
apc_labels <- apc_values %>%
  ungroup() %>%
  mutate(
    label = paste0("APC (", Sex, "): ", round(APC, 2), "%"),
    label_x = min(combined_data$Year) + 1,
    label_y = ifelse(Sex == "Male", 2.75, 2.5)
  )

# Build plot
p <- ggplot(combined_data, aes(x = Year, y = AAMR, color = Sex, shape = Sex, fill = Sex)) +
  geom_point(size = 3, stroke = 1.1) +
  geom_smooth(method = "lm", se = FALSE, size = 1.1, linetype = "solid", show.legend = FALSE) +
  geom_text(data = apc_labels,
            aes(x = label_x, y = label_y, label = label, color = Sex),
            hjust = 0, size = 5, fontface = "italic", inherit.aes = FALSE) +
  scale_color_manual(values = c("Male" = "dodgerblue3", "Female" = "deeppink3")) +
  scale_fill_manual(values = c("Male" = "dodgerblue3", "Female" = "deeppink3")) +
  scale_shape_manual(values = c("Male" = 21, "Female" = 24)) +
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
  "/Users/ksanaka/Desktop/Research/Osteomyelitis/Figures/AAMR_Sex_Trend_Plot.png",
  plot = p, width = 7, height = 5.5, dpi = 900
)

rm(list = ls())
