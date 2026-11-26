library(tidyverse)  # readr, dplyr, tidyr, purrr, ggplot2, lubridate ...
library(httr2)      # request(), req_perform(): download data from a web API
library(scales)     # comma() axis labels

START <- as.Date("2025-10-01")
END   <- as.Date("2025-12-31")

# trying to make the script work from root folder
PROJECT_ROOT <- if (dir.exists("data_clean")) "." else ".."
stopifnot("Run this script from the repository root or the R directory." =
            dir.exists(file.path(PROJECT_ROOT, "data_clean")))

DATA_DIR    <- file.path(PROJECT_ROOT, "data_clean", "ethan_chicago_data")
FIGURES_DIR <- file.path(PROJECT_ROOT, "figures")
DIVVY_CSV   <- file.path(DATA_DIR, "divvy_trips_cleaned.csv")
WEATHER_CSV <- file.path(DATA_DIR, "open-meteo-41.86N87.65W179m(10-12).csv")
TNC_CACHE   <- file.path(PROJECT_ROOT, "tnc_hourly_oct_dec_2025.csv")  # saved download

dir.create(FIGURES_DIR, showWarnings = FALSE)

# Chicago's open-data portal (Socrata). We ask the SERVER to count trips per
# hour for us, because downloading every individual trip would be millions of rows.
TNC_URL <- "https://data.cityofchicago.org/resource/6dvr-xwnh.csv"

# soda(): send ONE query to that API and return the answer as a table.
# `...` passes through whatever query pieces ($select, $where, ...) we give it.
soda <- function(...) {
  req <- request(TNC_URL) |>
    req_url_query(...) |>                         # add the query to the URL
    req_timeout(600) |>                           # wait up to 10 minutes
    req_retry(max_tries = 4, backoff = ~ 10)      # if it fails, retry up to 4 times, 10 s apart

  # An optional free API token (stored as an environment variable) gives
  # higher rate limits. Only add it if it is set (nzchar = "not empty").
  token <- Sys.getenv("SOCRATA_APP_TOKEN")
  if (nzchar(token)) req <- req_headers(req, `X-App-Token` = token)

  resp <- req_perform(req)                        # actually send the request
  # The reply is CSV text; read it. I() tells read_csv "this is text, not a path".
  read_csv(I(resp_body_string(resp)), col_types = cols(.default = "c"),
           show_col_types = FALSE)
}

get_tnc_hourly <- function() {

  # checking by asking for 1 row and confirm the column we rely on exists
   # limit here means max rows, unfortunate
  probe <- soda(`$limit` = 1)
  if (!"trip_start_timestamp" %in% names(probe)) {
    stop("Column 'trip_start_timestamp' not found. Columns are: ",
         paste(names(probe), collapse = ", "))
  }

  # Each chunk runs from `starts` up to (but not including) `ends`;
  # the last chunk is cut off at END + 1 (midnight after Dec 31).
  starts <- seq(START, END, by = "7 days")
  ends   <- pmin(starts + 7, END + 1)

  # Same SoQL text is used for grouping, making it a var
  day_and_hour <- "date_trunc_ymd(trip_start_timestamp), date_extract_hh(trip_start_timestamp)"

  # map2() loopinh over `starts` and `ends`
  map2(starts, ends, \(s, e) {
    message("Fetching TNC trips ", s, " to ", e - 1, " ...")
    soda(
      # Using sql effects!: SELECT: the day, the hour (0-23) and how many trips were in it
      `$select` = paste0("date_trunc_ymd(trip_start_timestamp) AS day, ",
                         "date_extract_hh(trip_start_timestamp) AS hr, ",
                         "count(*) AS tnc_trips"),
      `$where`  = sprintf("trip_start_timestamp >= '%sT00:00:00' AND trip_start_timestamp < '%sT00:00:00'", s, e),
      `$group`  = day_and_hour,    
      `$order`  = day_and_hour,
      `$limit`  = 50000 
    )
  }) |>
    bind_rows() |>                 # stack the weekly tables
    transmute(
      # making a label and, here: the values really are Chicago local time.
      datetime  = as.POSIXct(paste(substr(day, 1, 10), sprintf("%02d:00:00", as.integer(hr))),
                             tz = "UTC"),
      tnc_trips = as.numeric(tnc_trips)
    ) |>
    # If the same hour appears twice just add them.
    group_by(datetime) |>
    summarise(tnc_trips = sum(tnc_trips), .groups = "drop")
}

# Download only once if the cache file exists, use it save it.
if (file.exists(TNC_CACHE)) {
  tnc <- read_csv(TNC_CACHE, show_col_types = FALSE)
} else {
  tnc <- get_tnc_hourly()
  write_csv(tnc, TNC_CACHE)
}

# 92 days x 24 hours = 2,208 rows expected.if too many are gone (5% plus) warn
if (nrow(tnc) < 92 * 24 * 0.95) {
  warning("Only ", nrow(tnc), " rows found")
}


# a row per hour (2,208 rows = 92 days x 24 hours)
weather <- read_csv(
  WEATHER_CSV,
  skip = 3,                                   # 3 lines of location info + a blank line
  col_types = cols(time = col_character(), .default = col_double()),
  show_col_types = FALSE
) |>
  # Short names and the order matters
  set_names(c("time", "temp_c", "weather_code", "feels_like_c", "snow_cm",
              "rain_mm", "snow_depth_m", "wind_kmh", "gust_kmh")) |>
  mutate(
    # "2025-10-01T05:00" -> a date-time. 
    date     = as.Date(datetime),
    hour     = hour(datetime),               # 0-23

    # convert to imperial as the americans like it, review later to try and make use of metric
    temp_f   = temp_c * 9 / 5 + 32,
    rain_in  = rain_mm / 25.4,
    snow_in  = snow_cm / 2.54,
    wind_mph = wind_kmh / 1.609344,

    # adding total precipitation as rain + the snow's water equivalent
    # (Open-Meteo's rule: 7 cm of snow = 10 mm of water). 
    precip_in = (rain_mm + snow_cm / 0.7) / 25.4
  ) |>
  filter(between(date, START, END)) |>
  select(date, hour, datetime, temp_f, precip_in, rain_in, snow_in, wind_mph)


divvy <- read_csv(
  DIVVY_CSV,
  col_select = c(ride_date, start_hour),                  # only the 2 columns we need
  col_types  = cols(
    ride_date  = col_date(format = "%d/%m/%Y"),           # day/month/year
    start_hour = col_integer()
  ),
  show_col_types = FALSE
) |>
  # Count rows per (date, hour) = number of trips starting in that hour
  count(date = ride_date, hour = start_hour, name = "divvy_trips")

# split the TNC date-time back into a date and an hour still using 0-23
tnc_hourly <- tnc |>
  mutate(date = as.Date(datetime), hour = hour(datetime)) |>
  select(date, hour, tnc_trips)

# The weather table already has one row for each hour so ill attach the trip counts to it so that hours without trips still have rows
hourly <- weather |>

  # hours with no trips don't exist in the counts, so left_join should give NA
  left_join(divvy,      by = c("date", "hour")) |>
  left_join(tnc_hourly, by = c("date", "hour")) |>

  mutate(
    # now turn all those NAs into 0 trips.
    divvy_trips = replace_na(divvy_trips, 0),
    tnc_trips   = replace_na(tnc_trips, 0),


    # making an id for days and their times so monday at 8am is 108
    hour_of_week = wday(date, week_start = 1) * 100 + hour,

    # Precipitation  from the current hour (right-closed bins:
    # Making the cut-offs small because they are per HOUR. No hour exceeds 0.3 so the usual daily cut-offs would leave "Heavy" empty.
    rain_cat = cut(precip_in, breaks = c(-Inf, 0, 0.02, 0.1, Inf),
                   labels = c("Dry", "Light (<=0.02 in/hr)",
                              "Moderate (0.02-0.1 in/hr)", "Heavy (>0.1 in/hr)")),

    # 5-degree temperature bin
    temp_bin = floor(temp_f / 5) * 5 + 2.5
  ) |>

  # 1.0 = a normal Monday 8am, 0.5 = half as busy as normal
  # Trying to handle the rush hour effect so weather effects stand out and Divvy and rideshare share the same scale.
  group_by(hour_of_week) |>
  mutate(divvy_idx = divvy_trips / mean(divvy_trips),
         tnc_idx   = tnc_trips   / mean(tnc_trips)) |>
  ungroup()

# Daytime only (6:00-22:59) for the indexed charts: night-time Divvy counts are small, so their index would be very noisy
day_hours <- filter(hourly, hour >= 6, hour <= 22)


theme_set(theme_minimal(base_size = 12))

save_plot <- function(p, name, w = 9.5, h = 5.5) {
  ggsave(file.path(FIGURES_DIR, name), p, width = w, height = h, dpi = 150, bg = "white")
}

col_divvy <- "#1f77b4"   # blue for Divvy
col_tnc   <- "#e07b00"   # orange for rideshare

# short helper adds the two lines + two y-axes used by charts 1 and 5.
# mutliplying the rideshare by sf so they fit right-hand axis divides by `sf` again so its labels show the acutal rideshare counts.
dual_axis_layers <- function(sf, point_size = 2) {
  list(
    geom_line(aes(y = divvy), colour = col_divvy, linewidth = 1.2),
    geom_point(aes(y = divvy), colour = col_divvy, size = point_size),
    geom_line(aes(y = tnc * sf), colour = col_tnc, linewidth = 1.2),
    geom_point(aes(y = tnc * sf), colour = col_tnc, size = point_size),
    scale_y_continuous(
      name = "Divvy trips per hour (avg)", labels = comma,
      sec.axis = sec_axis(~ . / sf, name = "Rideshare trips per hour (avg)", labels = comma)
    ),
    theme(axis.title.y.left  = element_text(colour = col_divvy),
          axis.title.y.right = element_text(colour = col_tnc),
          plot.subtitle      = element_text(size = 9))
  )
}

by_temp <- hourly |>
  group_by(temp_bin) |>
  summarise(n_hours = n(),                       # how many hours fall in this bin
            divvy = mean(divvy_trips),
            tnc   = mean(tnc_trips),
            .groups = "drop") |>
  filter(n_hours >= 10)                          # ignore bins with too little data

sf1 <- max(by_temp$divvy) / max(by_temp$tnc)     # scaling factor for the 2nd axis

p1 <- ggplot(by_temp, aes(temp_bin)) +
  dual_axis_layers(sf1) +
  labs(title = "Average hourly trips by temperature: Divvy vs. rideshare",
       subtitle = "Each axis is scaled independently, so where the lines cross is arbitrary. See the indexed charts for a fair comparison.",
       x = "Hourly temperature (°F, 5° bins)")
save_plot(p1, "wx1_dual_axis_trips_vs_temp.png")

idx_temp <- day_hours |>
  group_by(temp_bin) |>
  summarise(n_hours = n(), Divvy = mean(divvy_idx), Rideshare = mean(tnc_idx),
            .groups = "drop") |>
  filter(n_hours >= 10) |>
  # changing from 2 columns (Divvy, Rideshare) into 2 rows per bin
  # (mode, index) - the "long" format ggplot needs for one line per mode.
  pivot_longer(c(Divvy, Rideshare), names_to = "mode", values_to = "index")

p2 <- ggplot(idx_temp, aes(temp_bin, index, colour = mode)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +   # 1 = typical
  geom_line(linewidth = 1.2) + geom_point(size = 2) +
  scale_colour_manual(values = c(Divvy = col_divvy, Rideshare = col_tnc)) +
  labs(title = "Demand relative to a typical hour, by temperature",
       subtitle = "Index = trips / average trips for the same weekday & hour (6 AM-10 PM). Weather values are hourly; same axis for both modes.",
       x = "Hourly temperature (°F, 5° bins)", y = "Demand index (1 = typical)", colour = NULL) +
  theme(plot.subtitle = element_text(size = 9))
save_plot(p2, "wx2_indexed_demand_vs_temp.png")

# For every candidate breakpoint `bp` in `grid`, fit a line that is allowed to change slope at bp:   y = a + b*x + c*max(x - bp, 0)
# (pmax(x - bp, 0) is 0 below bp and grows above it). Keep the bp whose fit has the smallest sum of squared errors.
find_breakpoint <- function(x, y, grid) {
  sse <- map_dbl(grid, \(bp) sum(resid(lm(y ~ x + pmax(x - bp, 0)))^2))
  grid[which.min(sse)]
}

d <- day_hours |>
  filter(is.finite(divvy_idx), is.finite(tnc_idx), tnc_idx > 0) |>   # drop 0/0 and divide-by-0 rows
  mutate(bike_vs_car = divvy_idx / tnc_idx)    # >1: bikes doing better than cars, relative to normal

#  every whole degree between the 10th and 90th percentile of temperature, avoid extremes
bp_grid <- seq(quantile(d$temp_f, 0.10), quantile(d$temp_f, 0.90), by = 1)

breakpoints <- c(
  `Divvy demand`      = find_breakpoint(d$temp_f, d$divvy_idx,    bp_grid),
  `Rideshare demand`  = find_breakpoint(d$temp_f, d$tnc_idx,      bp_grid),
  `Bike-vs-car ratio` = find_breakpoint(d$temp_f, d$bike_vs_car,  bp_grid)
)
cat("\nEstimated temperature breakpoints (°F):\n")
print(round(breakpoints))

ratio_bins <- d |>
  group_by(temp_bin) |>
  summarise(n_hours = n(), ratio = mean(bike_vs_car), .groups = "drop") |>
  filter(n_hours >= 10)

bp_ratio <- breakpoints[["Bike-vs-car ratio"]]

p3 <- ggplot(ratio_bins, aes(temp_bin, ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 1.1, colour = "grey30") + geom_point(size = 2) +
  geom_vline(xintercept = bp_ratio, colour = "firebrick", linetype = "dotted", linewidth = 1) +
  annotate("text", x = bp_ratio, y = max(ratio_bins$ratio), hjust = -0.1, colour = "firebrick",
           label = sprintf("Estimated tipping point ≈ %.0f°F", bp_ratio)) +
  labs(title = "Bike demand relative to rideshare demand, by temperature",
       subtitle = "Ratio of Divvy index to rideshare index (above 1 = bikes doing better than usual vs. cars)",
       x = "Hourly temperature (°F, 5° bins)", y = "Divvy index / rideshare index") +
  theme(plot.subtitle = element_text(size = 9))
save_plot(p3, "wx3_bike_vs_car_tipping_point.png")

rain <- day_hours |>
  mutate(temp_group = if_else(temp_f < 40, "Cold (<40°F)", "Milder (≥40°F)")) |>
  group_by(temp_group, rain_cat) |>
  summarise(n_hours = n(), Divvy = mean(divvy_idx), Rideshare = mean(tnc_idx),
            .groups = "drop") |>
  filter(n_hours >= 10) |>
  pivot_longer(c(Divvy, Rideshare), names_to = "mode", values_to = "index")

p4 <- ggplot(rain, aes(rain_cat, index, fill = mode)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_col(position = "dodge") +                 # Divvy and rideshare bars side by side
  facet_wrap(~ temp_group) +                     # one panel for cold, one for milder
  scale_fill_manual(values = c(Divvy = col_divvy, Rideshare = col_tnc)) +
  labs(title = "Demand relative to a typical hour, by hourly precipitation",
       subtitle = "Hourly precipitation (rain + melted snow); split by hourly temperature",
       x = NULL, y = "Demand index (1 = typical)", fill = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        plot.subtitle = element_text(size = 9))
save_plot(p4, "wx4_rain_effect.png", w = 10)


by_rain <- hourly |>
  group_by(rain_cat) |>
  summarise(n_hours = n(), divvy = mean(divvy_trips), tnc = mean(tnc_trips),
            .groups = "drop") |>
  filter(n_hours >= 10)

sf2 <- max(by_rain$divvy) / max(by_rain$tnc)

p5 <- ggplot(by_rain, aes(rain_cat, group = 1)) +     # group = 1: join all categories in ONE line
  dual_axis_layers(sf2, point_size = 2.5) +
  labs(title = "Average hourly trips by hourly precipitation: Divvy vs. rideshare",
       subtitle = "Hourly precipitation (rain + melted snow); raw averages are not adjusted for time of day or temperature.",
       x = NULL) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
save_plot(p5, "wx5_dual_axis_trips_vs_rain.png")

# save it

write_csv(hourly, file.path(PROJECT_ROOT, "hourly_divvy_tnc_weather.csv"))