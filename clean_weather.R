weather <- read_csv("columbus_weather.csv")

weather$dt_est <- 
  strptime(weather$dt_iso, "%Y-%m-%d %H:%M:%S", tz="EST") - dseconds(x = abs(weather$timezone))

weather$year <- year(weather$dt_est)
weather$month <- month(weather$dt_est)
weather$time <- format(weather$dt_est, "%H:%M:%S")

weather <- weather %>% 
  filter(year >= 2013)

weather$time <- format(weather$dt_est, "%H:%M:%S")

weather <- weather %>% 
  select(dt_est, year, month, time, temp, feels_like, temp_min, temp_max, pressure,
         humidity, wind_speed, wind_deg) %>%
  distinct_all()

write.csv(weather, "weather.csv")
