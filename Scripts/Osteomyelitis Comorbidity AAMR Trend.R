source(".Rprofile")

# Load comorbidity data
diabetes_data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Diabetes_Mortality_Trend.xlsx", sheet = "Analyzable")
foot_data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Foot_Mortality_Trend.xlsx", sheet = "Analyzable")
pvd_data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_PVD_Mortality_Trend.xlsx", sheet = "Analyzable")
trauma_data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Trauma_Mortality_Trend.xlsx", sheet = "Analyzable")
ulcer_data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Ulcer_Mortality_Trend.xlsx", sheet = "Analyzable")
vertebral_data <- read_excel("/Users/ksanaka/Desktop/Research/Osteomyelitis/Data/Osteomyelitis_Vertebral_Mortality_Trend.xlsx", sheet = "Analyzable")

# Add group labels
diabetes_data   <- diabetes_data   %>% mutate(AAMR = as.numeric(AAMR), Group = "Diabetes")
foot_data       <- foot_data       %>% mutate(AAMR = as.numeric(AAMR), Group = "Non-Pressure Foot Ulcer")
pvd_data        <- pvd_data        %>% mutate(AAMR = as.numeric(AAMR), Group = "Peripheral Vascular Disease")
trauma_data     <- trauma_data     %>% mutate(AAMR = as.numeric(AAMR), Group = "Trauma")
ulcer_data      <- ulcer_data      %>% mutate(AAMR = as.numeric(AAMR), Group = "Decubitus Ulcer")
vertebral_data  <- vertebral_data  %>% mutate(AAMR = as.numeric(AAMR), Group = "Vertebral")

# Combine data
data_plot1 <- bind_rows(diabetes_data, foot_data, pvd_data)
data_plot2 <- bind_rows(trauma_data, ulcer_data, vertebral_data)

# Function to create APC plot and return stats
create_apc_plot <- function(df, save_path, label_y_vals) {
  # APC calculation with CI and p-value
  apc_values <- df %>%
    dplyr::filter(!is.na(AAMR), AAMR > 0) %>%
    dplyr::group_by(Group) %>%
    group_split() %>%
    map_df(function(group_df) {
      group_name <- unique(group_df$Group)
      model <- lm(log(AAMR) ~ Year, data = group_df)
      summary_model <- summary(model)
      conf_int <- confint(model)["Year", ]
      beta1 <- coef(model)["Year"]
      
      APC <- (exp(beta1) - 1) * 100
      APC_lower <- (exp(conf_int[1]) - 1) * 100
      APC_upper <- (exp(conf_int[2]) - 1) * 100
      p_value <- summary_model$coefficients["Year", "Pr(>|t|)"]
      
      data.frame(
        Group = group_name,
        APC = APC,
        APC_lower = APC_lower,
        APC_upper = APC_upper,
        p_value = p_value
      )
    })
  
  # Prepare label positions
  apc_labels <- apc_values %>%
    mutate(
      label = paste0(
        "APC (", Group, "): ",
        round(APC, 2), "%"
      ),
      label_x = min(df$Year, na.rm = TRUE) + 1,
      label_y = label_y_vals[Group]
    )
  
  # Colors and shapes
  group_names <- unique(df$Group)
  color_palette <- brewer.pal(n = length(group_names), name = "Dark2")
  names(color_palette) <- group_names
  shape_palette <- c(21, 22, 24)[1:length(group_names)]
  names(shape_palette) <- group_names
  
  # Plot
  p <- ggplot(df, aes(x = Year, y = AAMR, color = Group, shape = Group, fill = Group)) +
    geom_point(size = 3, stroke = 1.1) +
    geom_smooth(method = "lm", se = FALSE, size = 1.1, show.legend = FALSE) +
    geom_text(data = apc_labels,
              aes(x = label_x, y = label_y, label = label, color = Group),
              hjust = 0, size = 5, fontface = "italic", inherit.aes = FALSE) +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    scale_shape_manual(values = shape_palette) +
    scale_x_continuous(breaks = pretty_breaks(n = 6)) +
    scale_y_continuous(breaks = pretty_breaks(n = 6)) +
    labs(x = "Year", y = "AAMR (per 100,000)") +
    theme_minimal(base_family = "Helvetica", base_size = 14) +
    theme(
      plot.title = element_blank(),
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 12),
      panel.grid.major = element_line(color = "grey85", size = 0.4),
      panel.grid.minor = element_line(color = "grey90", size = 0.2),
      axis.line = element_line(color = "black", size = 0.8),
      axis.ticks = element_line(color = "black", size = 0.5),
      legend.title = element_blank(),
      legend.position = "top"
    )
  
  # Save plot
  ggsave(save_path, plot = p, width = 7.5, height = 6, dpi = 900)
  
  # Return plot and APC stats
  return(list(plot = p, apc_stats = apc_values))
}

# Label positions for plot 1
label_y_1 <- c(
  "Diabetes" = 0.7,
  "Non-Pressure Foot Ulcer" = 0.6,
  "Peripheral Vascular Disease" = 0.5
)

# Label positions for plot 2
label_y_2 <- c(
  "Trauma" = 0.7,
  "Decubitus Ulcer" = 0.6,
  "Vertebral" = 0.5
)

# Generate plots and APC tables
result1 <- create_apc_plot(
  df = data_plot1,
  save_path = "/Users/ksanaka/Desktop/Research/Osteomyelitis/Figures/AAMR_Comorbidities_Plot1.png",
  label_y_vals = label_y_1
)
plot1 <- result1$plot
apc1 <- result1$apc_stats

result2 <- create_apc_plot(
  df = data_plot2,
  save_path = "/Users/ksanaka/Desktop/Research/Osteomyelitis/Figures/AAMR_Comorbidities_Plot2.png",
  label_y_vals = label_y_2
)
plot2 <- result2$plot
apc2 <- result2$apc_stats

# Display plots
print(plot1)
print(plot2)

# Clean environment
rm(list = ls())
