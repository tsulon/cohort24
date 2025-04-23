# aqi_bar_chart_from_csv.R

# -------------------------------
# 1. Load Libraries
# -------------------------------
library(tidyverse)   # includes readr, ggplot2, dplyr, tidyr

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
    city = City,
    AQI  = AQI
  ) %>%
  select(city, AQI) %>%
  filter(!is.na(AQI)) %>%
  mutate(city = as.factor(city))

# -------------------------------
# 4. Exploratory Data Analysis (EDA)
# -------------------------------
# Bar Chart: Average AQI by City
avg_aqi <- AQI_data %>%
  group_by(city) %>%
  summarize(avg_AQI = mean(AQI, na.rm = TRUE))

plot_avg_aqi_bar <- ggplot(avg_aqi, aes(x = city, y = avg_AQI)) +
  geom_col(fill = "red") +
  labs(
    title = "Average Air Quality Index by City",
    x     = "City",
    y     = "Average AQI"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 12)
  )

# Display the bar chart
print(plot_avg_aqi_bar)

# -------------------------------
# 5. Deploying Final Visualization
# -------------------------------
# Save the bar chart to file
# ggsave("Average_AQI_by_city.png", plot_avg_aqi_bar, width = 10, height = 6, dpi = 300)
