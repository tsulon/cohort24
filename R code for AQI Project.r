# aqi_comparison_bar_chart.R
# -------------------------------
# 1. Load Libraries
# -------------------------------
library(tidyverse)   # includes readr, dplyr, tidyr, ggplot2

# -------------------------------
# 2. Collecting Data
# -------------------------------
# Read AQI data from CSV (ensure the file path is correct)
# CSV must contain: City, AQI
AQI_raw <- read_csv("Nigeria AQI Data.csv")

# -------------------------------
# 3. Cleaning & Preparing Data
# -------------------------------
AQI_data <- AQI_raw %>%
  rename(
    state = City,
    current_AQI = AQI
  ) %>%
  select(state, current_AQI) %>%
  filter(!is.na(current_AQI)) %>%
  mutate(
    state = as.factor(state),
    # Project AQI 20 years ahead by applying a 17% increase
    projected_AQI = round(current_AQI * 1.17)
  )

# -------------------------------
# 4. Reshape for Plotting
# -------------------------------
# Pivot longer so we can plot current vs projected side by side
plot_data <- AQI_data %>%
  pivot_longer(
    cols = c(current_AQI, projected_AQI),
    names_to  = "type",
    values_to = "AQI_value"
  ) %>%
  mutate(
    type = recode(type,
                  current_AQI   = "Current AQI",
                  projected_AQI = "Projected AQI (2045)"))

# -------------------------------
# 5. Plotting Bar Chart
# -------------------------------
plot_comparison <- ggplot(plot_data, aes(x = AQI_value, y = state, fill = type)) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(
    title = "Current vs Projected AQI by State",
    x     = "Air Quality Index (AQI)",
    y     = "State",
    fill  = "AQI Type"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 16, face = "bold"),
    axis.title    = element_text(size = 14),
    axis.text     = element_text(size = 12),
    legend.title  = element_text(size = 13),
    legend.text   = element_text(size = 12)
  )

# Display the bar chart
print(plot_comparison)

# -------------------------------
# 6. Save the Plot
# -------------------------------
# ggsave("AQI_Current_vs_Projected.png", plot_comparison, width = 10, height = 6, dpi = 300)
