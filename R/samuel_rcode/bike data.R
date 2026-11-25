
library(tidyverse)
library(lubridate)

# ---- 1. Load ----
files <- c("C:/Users/samma/Downloads/divvy_tripdata_2025/divvy_tripdata_2025.csv")

raw <- files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c"))) %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at   = ymd_hms(ended_at),
    across(c(start_lat, start_lng, end_lat, end_lng), as.numeric)
  )

cat("Rows loaded:", nrow(raw), "\n")

# ---- 2. Inspect ----
raw %>% summarise(across(everything(), ~ sum(is.na(.x) | as.character(.x) == ""))) %>% 
  pivot_longer(everything(), names_to = "column", values_to = "missing") %>% 
  print(n = Inf)
count(raw, rideable_type) %>% print()
count(raw, member_casual) %>% print()

# ---- 3. Clean ----
clean <- raw %>%
  distinct(ride_id, .keep_all = TRUE) %>%
  mutate(
    duration_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
    date    = as_date(started_at),
    hour    = hour(started_at),
    weekday = wday(date, label = TRUE, week_start = 1),
    month   = month(date, label = TRUE)
  ) %>%
  filter(
    !is.na(started_at), !is.na(ended_at),
    year(date) == 2025,
    member_casual %in% c("member", "casual"),
    duration_min >= 1,
    duration_min <= 24 * 60
  ) %>%
  mutate(has_station = !is.na(start_station_name) & start_station_name != "")

cat("Rows after cleaning:", nrow(clean), "\n")
cat("Rows removed:", nrow(raw) - nrow(clean), "\n")

# ---- 4. Daily table ----
daily <- clean %>%
  count(date, member_casual, name = "trips") %>%
  pivot_wider(names_from = member_casual, values_from = trips, values_fill = 0) %>%
  mutate(total = member + casual) %>%
  complete(date = seq(ymd("2025-01-01"), ymd("2025-12-31"), by = "day"),
           fill = list(member = 0, casual = 0, total = 0))

# ---- 5. Save ----
dir.create("data_clean", showWarnings = FALSE)
write_csv(clean, "data_clean/divvy_clean.csv")
write_csv(daily, "data_clean/divvy_daily.csv")