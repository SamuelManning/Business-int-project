library(tidyverse) 
library(scales)

# Folder for the chart currently just outputs in root
dir.create("figures", showWarnings = FALSE)
data_dir <- "./data_clean/ethan_chicago_data"

trips <- read_csv(
  file.path(data_dir, "divvy_trips_cleaned.csv"),

  # Read only the 6 columns
  col_select = c(ride_date, start_hour, rideable_type, member_casual,
                 ride_duration_min, valid_duration),

  # make sure read_csv knows what type each column is.
  col_types = cols(
    ride_date         = col_date(format = "%d/%m/%Y"),
    start_hour        = col_integer(),
    ride_duration_min = col_double(),
    valid_duration    = col_logical()
  ),
  show_col_types = FALSE
) |>
  # Rename two columns to match the columns from weather data
  # deal with negative or invalid durations as well so we can make sure the later medians don't break
  transmute(
    date = ride_date,
    hour = start_hour,
    rideable_type,
    member_casual,
    duration_min = if_else(valid_duration, ride_duration_min, NA_real_)
  )


weather <- read_csv(
  file.path(data_dir, "open-meteo-41.86N87.65W179m(10-12).csv"),
  skip = 3, # first three are info about the dataset
  # Read `time` as text
  col_types = cols(time = col_character(), .default = col_double()),
  show_col_types = FALSE
) |>
  # Give the 9 columns short names. Order matters here (unfortunately)
  # time, temperature (C), weather code, feels-like (C), snowfall (cm),
  # rain (mm), snow depth (m), wind speed (km/h), wind gusts (km/h).
  set_names(c("time", "temp_c", "weather_code", "feels_like_c", "snow_cm",
              "rain_mm", "snow_depth_m", "wind_kmh", "gust_kmh")) |>
  mutate(
    # "2025-10-01T00:00" so we have to take first 10 charachters ( this is frustrating)
    date = as.Date(substr(time, 1, 10)),

    # Convert to the units used in the charts (F, inches, mph), might change so its metric
    temp_f = temp_c * 9 / 5 + 32,
    rain_in = rain_mm / 25.4,
    snow_in = snow_cm / 2.54,

    # Total precipitation as liquid water. `rain` excludes snow, so add the
    # snow's water equivalent. Open-Meteo's rule: 7 cm of snow = 10 mm of water.
    precip_in = (rain_mm + snow_cm / 0.7) / 25.4,

    wind_mph = wind_kmh / 1.609344
  ) |>
  # 24 hourly rows is 1 daily row.
  group_by(date) |>
  summarise(
    temp_max_f   = max(temp_f),
    temp_min_f   = min(temp_f),
    temp_mean_f  = mean(temp_f),       # average of the 24 hourly temperatures
    precip_in    = sum(precip_in),     # add up the hours, daily total
    rain_in      = sum(rain_in),
    snow_in      = sum(snow_in),
    wind_max_mph = max(wind_mph),      # windiest hour of the day
    .groups = "drop"
  ) |>
  # Keep Oct 1 - Dec 31 2025 only.
  filter(between(date, as.Date("2025-10-01"), as.Date("2025-12-31")))

daily <- trips |>
  group_by(date) |>
  summarise(
    rides        = n(),                                   # number of trips that day
    median_dur   = median(duration_min, na.rm = TRUE),    # typical trip length
    ebike_share  = mean(rideable_type == "electric_bike"),# fraction of trips on e-bikes (TRUE=1, FALSE=0)
    member_share = mean(member_casual == "member"),       # fraction of trips by members
    .groups = "drop"                                      # return a normal, ungrouped table
  ) |>
  # add that day's weather. inner_join keeps only days present in BOTH tables. (I miss sql)
  inner_join(weather, by = "date") |>
  mutate(
    # cut() sorts a number into labelled bins
    # Were gonna take the following to be true: (-Inf,0] = Dry, (0,0.1] = Light, (0.1,0.3] = Moderate, (0.3,Inf) = Heavy.
    precip_cat = cut(precip_in,
                     breaks = c(-Inf, 0, 0.1, 0.3, Inf),
                     labels = c("Dry", "Light (<0.1 in)",
                                "Moderate (0.1-0.3 in)", "Heavy (>0.3 in)")),

    # same idea for temperature 
    temp_band = cut(temp_mean_f,
                    breaks = c(-Inf, 32, 45, 60, Inf),
                    labels = c("Freezing (<32°F)", "Cold (32-45°F)",
                               "Cool (45-60°F)", "Mild (>60°F)")),

    # check if snowfall that day
    snow_day = if_else(snow_in > 0, "Snow", "No snow")
  )

theme_set(theme_minimal(base_size = 12))   # one look for every chart

# give it all the same save format and settings
save_plot <- function(p, name, w = 9, h = 5.5) {
  ggsave(file.path("figures", name), p, width = w, height = h, dpi = 150, bg = "white")
}

# y axis I'll use for each chart, using 1,000 not 1000
y_comma <- scale_y_continuous(labels = comma)


# chart 1: bars = rides per day, line = mean temperature (two y-axes).
# A second axis can only be a re-scaling of the first, so we multiply
# temperature by scale_f to make its biggest value just as big as the biggest rides value, 
# then divide by scale_f again for the right-hand axis labels and it should look correct
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

# Chart 2: rides vs temperature, separate curve for members and casual riders.
# First count rides per day per rider type then just add weather
by_type <- trips |>
  count(date, member_casual, name = "rides") |>
  inner_join(weather, by = "date")

p2 <- ggplot(by_type, aes(temp_mean_f, rides, colour = member_casual)) +
  geom_point(alpha = 0.6) +                                    # one dot per day & rider type
  geom_smooth(method = "loess", se = TRUE, formula = y ~ x) + # smooth trend and trying an uncertainty band
  y_comma +
  labs(title = "Daily rides vs. mean temperature",
       x = "Mean temperature (°F)", y = "Rides per day", colour = "Rider type")
save_plot(p2, "02_rides_vs_temp_by_rider.png")

# Chart 3: spread of daily rides within each precipitation category.
p3 <- ggplot(daily, aes(precip_cat, rides, fill = precip_cat)) +
  geom_boxplot(show.legend = FALSE, outlier.shape = 21) +
  y_comma +
  labs(title = "Daily rides by precipitation level", x = NULL, y = "Rides per day")
save_plot(p3, "03_rides_by_precip.png")

# Chart 4: rides vs max wind speed with a straight-line (lm) fit.
p4 <- ggplot(daily, aes(wind_max_mph, rides)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", colour = "steelblue", formula = y ~ x) +
  y_comma +
  labs(title = "Daily rides vs. maximum wind speed",
       x = "Max wind speed (mph)", y = "Rides per day")
save_plot(p4, "04_rides_vs_wind.png")

# Chart 5: rides per temperature band, split by snow / no snow.
p5 <- ggplot(daily, aes(temp_band, rides, fill = snow_day)) +
  geom_boxplot() +
  y_comma +
  labs(title = "Daily rides by temperature band and snowfall",
       x = NULL, y = "Rides per day", fill = NULL)
save_plot(p5, "05_rides_temp_band_snow.png")

# Chart 6: median trip length vs temperature, per rider type.
# median duration per day per rider type then attach the weather again
dur <- trips |>
  group_by(date, member_casual) |>
  summarise(median_dur = median(duration_min, na.rm = TRUE), .groups = "drop") |>
  inner_join(weather, by = "date")

p6 <- ggplot(dur, aes(temp_mean_f, median_dur, colour = member_casual)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, formula = y ~ x) +
  labs(title = "Median trip duration vs. mean temperature",
       x = "Mean temperature (°F)", y = "Median duration (min)", colour = "Rider type")
save_plot(p6, "06_duration_vs_temp.png")

# Chart 7: share of rides on e-bikes vs temperature (already in `daily`).
p7 <- ggplot(daily, aes(temp_mean_f, ebike_share)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", colour = "darkgreen", formula = y ~ x) +
  scale_y_continuous(labels = percent) +
  labs(title = "Share of rides on e-bikes vs. mean temperature",
       x = "Mean temperature (°F)", y = "E-bike share of rides")
save_plot(p7, "07_ebike_share_vs_temp.png")

# Chart 8: average rides in each hour of the day, one line per temperature band.
# Have to give every trip its day's temp_band then count trips per band & hour,
# finally divide by the number of days in that band to get an average per day (frustrating).
days_per_band <- count(daily, temp_band, name = "n_days")

hourly <- trips |>
  inner_join(select(daily, date, temp_band), by = "date") |>
  count(temp_band, hour) |>
  left_join(days_per_band, by = "temp_band") |>
  mutate(avg_rides = n / n_days)

p8 <- ggplot(hourly, aes(hour, avg_rides, colour = temp_band)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +    # tick marks at 0, 3, 6, ... 21
  labs(title = "Average rides per hour by temperature band",
       x = "Hour of day", y = "Avg rides per day", colour = NULL)
save_plot(p8, "08_hourly_profile_by_temp.png")

# make the correlation (-1 to +1) between rides and each weather variable.
daily |>
  select(rides, temp_mean_f, precip_in, snow_in, wind_max_mph) |>
  cor() |>
  round(2) |>
  print()

# saving the joined table
write_csv(daily, "daily_rides_weather.csv")