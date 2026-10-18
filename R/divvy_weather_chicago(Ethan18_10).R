# Divvy rides (Oct-Dec 2025) vs Chicago weather (Open-Meteo)
# ------------------------------------------------------------
# Files expected in your working directory:
#   divvy_oct_dec_2025_clean.csv                  (cleaned + combined trips)
#   open-meteo-41_86N87_65W241m_10-12__ef8fe3.csv (weather, Oct-Dec only)

# install.packages(c("tidyverse", "scales"))   # run once if needed
library(tidyverse)
library(scales)

dir.create("figures", showWarnings = FALSE)

# ---- 1. Load -------------------------------------------------------------
trips <- read_csv(
  "divvy_oct_dec_2025_clean.csv",
  col_select = c(started_at, date, hour, rideable_type, member_casual, duration_min),
  show_col_types = FALSE
)

# Weather file: 3 metadata lines (location header, values, blank), then the
# data header. Dates are ISO (yyyy-mm-dd), so readr parses them as Dates.
weather <- read_csv(
  "open-meteo-41.86N87.65W241m(10-12).csv",
  skip = 3, show_col_types = FALSE
) |>
  set_names(c("date", "weather_code", "temp_max_f", "temp_min_f", "temp_mean_f",
              "precip_in", "rain_in", "snow_in", "precip_hours", "wind_max_mph")) |>
  mutate(date = as.Date(date)) |>
  filter(date >= as.Date("2025-10-01"), date <= as.Date("2025-12-31"))

# ---- 2. Daily summary joined to weather ---------------------------------
daily <- trips |>
  group_by(date) |>
  summarise(
    rides        = n(),
    median_dur   = median(duration_min),
    ebike_share  = mean(rideable_type == "electric_bike"),
    member_share = mean(member_casual == "member"),
    .groups = "drop"
  ) |>
  inner_join(weather, by = "date") |>
  mutate(
    precip_cat = cut(precip_in, breaks = c(-Inf, 0, 0.1, 0.3, Inf),
                     labels = c("Dry", "Light (<0.1 in)", "Moderate (0.1-0.3 in)", "Heavy (>0.3 in)")),
    temp_band  = cut(temp_mean_f, breaks = c(-Inf, 32, 45, 60, Inf),
                     labels = c("Freezing (<32°F)", "Cold (32-45°F)", "Cool (45-60°F)", "Mild (>60°F)")),
    snow_day   = if_else(snow_in > 0, "Snow", "No snow")
  )

theme_set(theme_minimal(base_size = 12))
save_plot <- function(p, name, w = 9, h = 5.5) {
  ggsave(file.path("figures", name), p, width = w, height = h, dpi = 150, bg = "white")
  print(p)
}

# ---- 3. Plots ------------------------------------------------------------

# 3a. Daily rides and mean temperature over time (dual axis)
scale_f <- max(daily$rides) / max(daily$temp_mean_f)
p1 <- ggplot(daily, aes(date)) +
  geom_col(aes(y = rides), fill = "grey70") +
  geom_line(aes(y = temp_mean_f * scale_f), colour = "firebrick", linewidth = 1) +
  scale_y_continuous(
    name = "Rides per day", labels = comma,
    sec.axis = sec_axis(~ . / scale_f, name = "Mean temperature (°F)")
  ) +
  labs(title = "Daily Divvy rides vs. mean temperature",
       subtitle = "Bars = rides, red line = mean temp", x = NULL)
save_plot(p1, "01_daily_rides_and_temp.png")

# 3b. Rides vs temperature, by rider type
by_type <- trips |>
  count(date, member_casual, name = "rides") |>
  inner_join(weather, by = "date")

p2 <- ggplot(by_type, aes(temp_mean_f, rides, colour = member_casual)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, formula = y ~ x) +
  scale_y_continuous(labels = comma) +
  labs(title = "Daily rides vs. mean temperature",
       x = "Mean temperature (°F)", y = "Rides per day", colour = "Rider type")
save_plot(p2, "02_rides_vs_temp_by_rider.png")

# 3c. Rides by precipitation category
p3 <- ggplot(daily, aes(precip_cat, rides, fill = precip_cat)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = 21) +
  scale_y_continuous(labels = comma) +
  labs(title = "Daily rides by precipitation level",
       x = NULL, y = "Rides per day")
save_plot(p3, "03_rides_by_precip.png")

# 3d. Rides vs max wind speed
p4 <- ggplot(daily, aes(wind_max_mph, rides)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", colour = "steelblue", formula = y ~ x) +
  scale_y_continuous(labels = comma) +
  labs(title = "Daily rides vs. maximum wind speed",
       x = "Max wind speed (mph)", y = "Rides per day")
save_plot(p4, "04_rides_vs_wind.png")

# 3e. Rides: snow vs no snow, split by temperature band
p5 <- ggplot(daily, aes(temp_band, rides, fill = snow_day)) +
  geom_boxplot() +
  scale_y_continuous(labels = comma) +
  labs(title = "Daily rides by temperature band and snowfall",
       x = NULL, y = "Rides per day", fill = NULL)
save_plot(p5, "05_rides_temp_band_snow.png")

# 3f. Median trip duration vs temperature, by rider type
dur <- trips |>
  group_by(date, member_casual) |>
  summarise(median_dur = median(duration_min), .groups = "drop") |>
  inner_join(weather, by = "date")

p6 <- ggplot(dur, aes(temp_mean_f, median_dur, colour = member_casual)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, formula = y ~ x) +
  labs(title = "Median trip duration vs. mean temperature",
       x = "Mean temperature (°F)", y = "Median duration (min)", colour = "Rider type")
save_plot(p6, "06_duration_vs_temp.png")

# 3g. Electric-bike share vs temperature
p7 <- ggplot(daily, aes(temp_mean_f, ebike_share)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", colour = "darkgreen", formula = y ~ x) +
  scale_y_continuous(labels = percent) +
  labs(title = "Share of rides on e-bikes vs. mean temperature",
       x = "Mean temperature (°F)", y = "E-bike share of rides")
save_plot(p7, "07_ebike_share_vs_temp.png")

# 3h. Hour-of-day profile by temperature band (avg rides per day)
hourly <- trips |>
  inner_join(select(daily, date, temp_band), by = "date") |>
  count(temp_band, hour) |>
  left_join(count(daily, temp_band, name = "n_days"), by = "temp_band") |>
  mutate(avg_rides = n / n_days)

p8 <- ggplot(hourly, aes(hour, avg_rides, colour = temp_band)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Average rides per hour by temperature band",
       x = "Hour of day", y = "Avg rides per day", colour = NULL)
save_plot(p8, "08_hourly_profile_by_temp.png")

# 3i. Correlation summary (printed)
daily |>
  select(rides, temp_mean_f, precip_in, snow_in, wind_max_mph) |>
  cor() |>
  round(2) |>
  print()

# Optional: save the daily joined table for further analysis
write_csv(daily, "daily_rides_weather.csv")
