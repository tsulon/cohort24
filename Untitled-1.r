# 1. Load libraries
install.packages(c("tidyverse","lubridate","forecast","scales"))
library(tidyverse); library(lubridate); library(forecast); library(scales)

# 2. Read CSV
aqi_raw <- read_csv("data.csv")  
# 3. Clean & aggregate
aqi <- aqi_raw %>%
  mutate(Date = ymd(Date)) %>%
  select(Date, PM2.5, PM10) %>%
  group_by(Date) %>%
  summarize(pm25 = mean(PM2.5, na.rm=TRUE),
            pm10 = mean(PM10, na.rm=TRUE)) %>%
  ungroup()

# 4. Define breakpoints and AQI function (US-EPA)
bp_pm25 <- tibble(
  C_low  = c(0.0, 12.1, 35.5, 55.5, 150.5, 250.5),
  C_high = c(12.0,35.4,55.4,150.4,250.4,500.4),
  I_low  = c(0,50,100,150,200,300),
  I_high = c(50,100,150,200,300,400)
)
calc_aqi <- function(Cp, bp){
  row <- bp %>% filter(Cp >= C_low & Cp <= C_high)
  with(row, ((I_high - I_low)/(C_high - C_low))*(Cp - C_low) + I_low)
}

# 5. Compute AQI
aqi <- aqi %>%
  mutate(aqi_pm25 = map_dbl(pm25, ~calc_aqi(.x, bp=bp_pm25)),
         aqi_pm10 = map_dbl(pm10, ~calc_aqi(.x, bp=bp_pm25)),
         AQI       = pmax(aqi_pm25, aqi_pm10, na.rm=TRUE))

# 6. Forecast
aqi_ts <- ts(aqi$AQI, frequency=365, start=c(year(min(aqi$Date)), yday(min(aqi$Date))))
model <- auto.arima(aqi_ts)
fcast <- forecast(model, h=365)

# 7. Prepare forecast frame
fcast_df <- tibble(
  Date     = seq(max(aqi$Date) + days(1), by="day", length.out=365),
  Forecast = as.numeric(fcast$mean),
  Lo80     = fcast$lower[,"80%"],
  Hi80     = fcast$upper[,"80%"],
  Lo95     = fcast$lower[,"95%"],
  Hi95     = fcast$upper[,"95%"]
)

# 8. Plot with ggplot2
ggplot() +
  geom_line(data=aqi, aes(x=Date, y=AQI), color="steelblue") +
  geom_line(data=fcast_df, aes(x=Date, y=Forecast), color="firebrick", linetype="dashed") +
  geom_ribbon(data=fcast_df, aes(x=Date, ymin=Lo95, ymax=Hi95), fill="pink", alpha=0.2) +
  geom_ribbon(data=fcast_df, aes(x=Date, ymin=Lo80, ymax=Hi80), fill="salmon", alpha=0.3) +
  labs(
    title = "Historical AQI and 1-Year Forecast",
    x     = "Date",
    y     = "AQI"
  ) +
  scale_x_date(date_labels="%b\n%Y", breaks="3 months") +
  theme_minimal()
