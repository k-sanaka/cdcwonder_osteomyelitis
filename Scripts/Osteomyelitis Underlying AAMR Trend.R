source(".Rprofile")

library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

# Load in the data set:
df <- read_excel(
  "/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Underlying_Mortality_Trend.xlsx",
  sheet = "Analyzable"
)

# Create the linear regression model (levels)
model <- lm(AAMR ~ Year, data = df)
broom::tidy(model)

# Log-linear model for APC
model_log <- lm(log(AAMR) ~ Year, data = df)

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

# Label for plot
apc_label <- paste0("APC: ", round(APC, 2), "%")

# Plot overall trend
p <- ggplot(df, aes(x = Year, y = AAMR)) +
  geom_point(size = 3, shape = 21, fill = "white", color = "black", stroke = 1.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
  annotate(
    "text",
    x = min(df$Year) + 1,
    y = max(df$AAMR),
    label = apc_label,
    hjust = 0,
    vjust = 1.2,
    size = 5,
    fontface = "italic"
  ) +
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
    panel.grid.major = element_line(color = "grey85", size = 0.4),
    panel.grid.minor = element_line(color = "grey90", size = 0.2),
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.5),
    panel.border = element_blank()
  )

print(p)

ggsave(
  filename = "/Users/ksanaka/Desktop/Research/Osteomyelitis/Figures/AAMR_Underlying_Trend_Plot.png",
  plot = p,
  width = 7,
  height = 5.5,
  dpi = 900
)

# ---- 5-year segment APC analysis ----

max_year <- max(df$Year)

df_5yr <- df %>%
  mutate(
    period_start = floor((Year - 1999) / 5) * 5 + 1999,
    period_end   = pmin(period_start + 4, max_year),
    period       = paste0(period_start, "–", period_end)
  ) %>%
  group_by(period, period_start, period_end) %>%
  do({
    m <- lm(log(AAMR) ~ Year, data = .)
    beta1 <- coef(m)["Year"]
    se_beta1 <- summary(m)$coefficients["Year", "Std. Error"]
    
    apc <- (exp(beta1) - 1) * 100
    lower_beta1 <- beta1 - 1.96 * se_beta1
    upper_beta1 <- beta1 + 1.96 * se_beta1
    lower_apc <- (exp(lower_beta1) - 1) * 100
    upper_apc <- (exp(upper_beta1) - 1) * 100
    
    tibble(
      APC       = apc,
      lower_APC = lower_apc,
      upper_APC = upper_apc
    )
  }) %>%
  ungroup() %>%
  arrange(period_start)

# Look at 5-year APCs
df_5yr

# If you still want to clear the environment AFTER inspecting:
# rm(list = ls())
