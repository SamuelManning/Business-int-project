# Divvy 2025: hourly, station and grid-cell tables
# Run AFTER the cleaning script (needs the object `clean`).
# If `clean` is not in memory, it is read from divvy_clean.csv (slow, ~1 GB).

library(tidyverse)
library(lubridate)

# ---- 0. Settings -------------------------------------------------------
out_dir <- "C:/Users/samma/Documents/college/Business-int-project/data_clean/samuel_chicago_data/Bike_Data"
clean_csv <- file.path(out_dir, "divvy_clean.csv")
cell_size <- 0.005          # grid size in degrees (about 550 m); make it bigger for fewer cells

if (!exists("clean")) {
  clean <- read_csv(clean_csv, show_col_types = FALSE)
}
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

all_days <- seq(ymd("2025-01-01"), ymd("2025-12-31"), by = "day")

# ---- 1. Hourly table (Q1 / Q2: peak hours, weather by hour) -----------
hourly <- clean %>%
  group_by(date, hour) %>%
  summarise(
    member   = sum(member_casual == "member"),
    casual   = sum(member_casual == "casual"),
    electric = sum(rideable_type == "electric_bike"),
    classic  = sum(rideable_type == "classic_bike"),
    total    = n(),
    .groups  = "drop"
  ) %>%
  complete(date = all_days, hour = 0:23,
           fill = list(member = 0, casual = 0, electric = 0, classic = 0, total = 0))

# ---- 2. Daily table with bike type (Q1: electric vs classic) ----------
daily_bike <- hourly %>%
  group_by(date) %>%
  summarise(across(c(member, casual, electric, classic, total), sum), .groups = "drop")

# ---- 3. Station tables (named stations only) --------------------------
# net_inflow = ends - starts: positive = bikes pile up, negative = bikes run short
st_start <- clean %>%
  filter(!is.na(start_station_name), start_station_name != "") %>%
  mutate(station = start_station_name)

st_end <- clean %>%
  filter(!is.na(end_station_name), end_station_name != "") %>%
  mutate(station = end_station_name)

station_summary <- full_join(
  st_start %>% group_by(station) %>%
    summarise(lat = mean(start_lat), lng = mean(start_lng), starts = n(), .groups = "drop"),
  st_end %>% group_by(station) %>%
    summarise(lat_end = mean(end_lat, na.rm = TRUE), lng_end = mean(end_lng, na.rm = TRUE),
              ends = n(), .groups = "drop"),
  by = "station"
) %>%
  mutate(lat = coalesce(lat, lat_end), lng = coalesce(lng, lng_end),
         starts = replace_na(starts, 0), ends = replace_na(ends, 0),
         net_inflow = ends - starts) %>%
  select(station, lat, lng, starts, ends, net_inflow)

station_hour_profile <- full_join(
  st_start %>% count(station, hour, name = "starts"),
  st_end   %>% count(station, hour = lubridate::hour(ended_at), name = "ends"),
  by = c("station", "hour")
) %>%
  mutate(starts = replace_na(starts, 0), ends = replace_na(ends, 0),
         net_inflow = ends - starts)

# ---- 4. Grid-cell table (ALL trips, including e-bikes with no station) -
to_cell <- function(x) floor(x / cell_size) * cell_size + cell_size / 2

grid_starts <- clean %>%
  filter(!is.na(start_lat), !is.na(start_lng)) %>%
  count(cell_lat = to_cell(start_lat), cell_lng = to_cell(start_lng), hour, name = "starts")

grid_ends <- clean %>%
  filter(!is.na(end_lat), !is.na(end_lng)) %>%
  count(cell_lat = to_cell(end_lat), cell_lng = to_cell(end_lng),
        hour = lubridate::hour(ended_at), name = "ends")

grid_hour_profile <- full_join(grid_starts, grid_ends,
                               by = c("cell_lat", "cell_lng", "hour")) %>%
  mutate(starts = replace_na(starts, 0), ends = replace_na(ends, 0),
         net_inflow = ends - starts)

# ---- 5. Sanity checks (all should say TRUE) ----------------------------
cat("hourly has 8760 rows:      ", nrow(hourly) == 24 * 365, "\n")
cat("hourly total = clean rows: ", sum(hourly$total) == nrow(clean), "\n")
cat("daily_bike has 365 rows:   ", nrow(daily_bike) == 365, "\n")
cat("grid starts = trips w/ coordinates:",
    sum(grid_hour_profile$starts) == sum(!is.na(clean$start_lat) & !is.na(clean$start_lng)), "\n")
cat("Stations found:", nrow(station_summary), " | Grid cells:",
    n_distinct(grid_hour_profile$cell_lat, grid_hour_profile$cell_lng), "\n")

# ---- 6. Save -----------------------------------------------------------
write_csv(hourly,               file.path(out_dir, "divvy_hourly.csv"))
write_csv(daily_bike,           file.path(out_dir, "divvy_daily_bike.csv"))
write_csv(station_summary,      file.path(out_dir, "divvy_station_summary.csv"))
write_csv(station_hour_profile, file.path(out_dir, "divvy_station_hour_profile.csv"))
write_csv(grid_hour_profile,    file.path(out_dir, "divvy_grid_hour_profile.csv"))

# File sizes in MB (check these are small before git add)
files <- list.files(out_dir, pattern = "^divvy_(hourly|daily_bike|station|grid).*csv$", full.names = TRUE)
print(round(file.size(files) / 1e6, 1) %>% setNames(basename(files)))
