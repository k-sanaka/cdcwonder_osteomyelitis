source(".Rprofile")

# Load in the data set:
data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Contributing_Mortality_Trend.xlsx", sheet = "Analyzable")

# Create the linear regression model:
model <- lm(AAMR ~ Year, data = data)

tidy(model)

# Calculate APC:
model_log <- lm(log(AAMR) ~ Year, data = data)

# Extract the slope (Year coefficient)
beta1 <- coef(model_log)["Year"]

# Calculate APC
APC <- (exp(beta1) - 1) * 100

# Get standard error of the slope
se_beta1 <- summary(model_log)$coefficients["Year", "Std. Error"]

# 95% CI for the slope
lower_beta1 <- beta1 - 1.96 * se_beta1
upper_beta1 <- beta1 + 1.96 * se_beta1

# Convert to APC CI
lower_APC <- (exp(lower_beta1) - 1) * 100
upper_APC <- (exp(upper_beta1) - 1) * 100

# Refit the log model for APC
model_log <- lm(log(AAMR) ~ Year, data = data)
beta1 <- coef(model_log)["Year"]
APC <- (exp(beta1) - 1) * 100
apc_label <- paste0("APC: ", round(APC, 2), "%")

# Plot
p <- ggplot(data, aes(x = Year, y = AAMR)) +
  geom_point(size = 3, shape = 21, fill = "white", color = "black", stroke = 1.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  annotate("text", x = min(data$Year) + 1, y = max(data$AAMR), 
           label = apc_label, hjust = 0, vjust = 1.2, size = 5, fontface = "italic") +
  scale_x_continuous(breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  labs(
    x = "Year",
    y = "AAMR (per 100,000)"
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    
    # Faint, subtle gridlines
    panel.grid.major = element_line(color = "grey85", size = 0.4),
    panel.grid.minor = element_line(color = "grey90", size = 0.2),
    
    # Clean axis lines and ticks
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.5),
    
    # No border
    panel.border = element_blank()
  )

# Show the plot
print(p)

# Save the plot
ggsave(
  filename = "/Users/ksanaka/Desktop/Research/Osteomyelitis/Figures/AAMR_Trend_Plot.png",
  plot = p,                          
  width = 7,                        
  height = 5.5,                     
  dpi = 900                          
)

rm(list = ls())

