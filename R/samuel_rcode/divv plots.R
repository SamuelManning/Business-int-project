# Divvy 2025: simple descriptive plots for Q1
# Uses only the small tables made earlier, so it runs in seconds.

library(tidyverse)
library(lubridate)

# ---- 1. Where the files are --------------------------------------------
project <- "C:/Users/samma/Documents/college/Business-int-project"
tables  <- file.path(project, "data_clean/samuel_chicago_data/Bike_Data")
figures <- file.path(project, "figures/samuel_divvy")
dir.create(figures, showWarnings = FALSE, recursive = TRUE)

# ---- 2. Read the two small tables ---------------------------------------
d <- read_csv(file.path(tables, "divvy_daily_bike.csv"), show_col_types = FALSE)  # one row per day
h <- read_csv(file.path(tables, "divvy_hourly.csv"),     show_col_types = FALSE)  # one row per hour

# ---- 3. Add weekday, month and season to the daily table ----------------
d <- d %>%
  mutate(
    weekday = wday(date, label = TRUE, week_start = 1),   # Mon, Tue, ...
    month   = month(date, label = TRUE),                  # Jan, Feb, ...
    season  = case_when(
      month(date) %in% c(12, 1, 2) ~ "Winter",
      month(date) %in% 3:5         ~ "Spring",
      month(date) %in% 6:8         ~ "Summer",
      TRUE                         ~ "Autumn"
    ),
    season  = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

# ---- Plot 1: trips per day over the year, members vs casual -------------
p1 <- d %>%
  select(date, member, casual) %>%
  pivot_longer(c(member, casual), names_to = "rider", values_to = "trips") %>%
  ggplot(aes(date, trips, colour = rider)) +
  geom_line() +
  labs(title = "Daily Divvy trips, 2025", x = NULL, y = "Trips per day", colour = "Rider") +
  theme_minimal()
print(p1)
ggsave(file.path(figures, "01_daily_trips.png"), p1, width = 8, height = 4, dpi = 150)

# ---- Plot 2: average trips per day by weekday ---------------------------
p2 <- d %>%
  group_by(weekday) %>%
  summarise(member = mean(member), casual = mean(casual)) %>%
  pivot_longer(c(member, casual), names_to = "rider", values_to = "avg_trips") %>%
  ggplot(aes(weekday, avg_trips, fill = rider)) +
  geom_col(position = "dodge") +
  labs(title = "Average trips per day by weekday", x = NULL, y = "Average trips per day", fill = "Rider") +
  theme_minimal()
print(p2)
ggsave(file.path(figures, "02_weekday.png"), p2, width = 8, height = 4, dpi = 150)

# ---- Plot 3: average trips per day by season ----------------------------
p3 <- d %>%
  group_by(season) %>%
  summarise(member = mean(member), casual = mean(casual)) %>%
  pivot_longer(c(member, casual), names_to = "rider", values_to = "avg_trips") %>%
  ggplot(aes(season, avg_trips, fill = rider)) +
  geom_col(position = "dodge") +
  labs(title = "Average trips per day by season", x = NULL, y = "Average trips per day", fill = "Rider") +
  theme_minimal()
print(p3)
ggsave(file.path(figures, "03_season.png"), p3, width = 8, height = 4, dpi = 150)

# ---- Plot 4: electric vs classic bikes by month -------------------------
p4 <- d %>%
  group_by(month) %>%
  summarise(electric = mean(electric), classic = mean(classic)) %>%
  pivot_longer(c(electric, classic), names_to = "bike", values_to = "avg_trips") %>%
  ggplot(aes(month, avg_trips, colour = bike, group = bike)) +
  geom_line() +
  geom_point() +
  labs(title = "Average trips per day by month and bike type", x = NULL, y = "Average trips per day", colour = "Bike") +
  theme_minimal()
print(p4)
ggsave(file.path(figures, "04_bike_type.png"), p4, width = 8, height = 4, dpi = 150)

# ---- Plot 5: trips by hour of the day, weekdays vs weekends -------------
p5 <- h %>%
  mutate(day_type = if_else(wday(date, week_start = 1) >= 6, "Weekend", "Weekday")) %>%
  group_by(day_type, hour) %>%
  summarise(avg_trips = mean(total), .groups = "drop") %>%
  ggplot(aes(hour, avg_trips, colour = day_type)) +
  geom_line() +
  labs(title = "Average trips by hour of the day", x = "Hour (0 = midnight)", y = "Average trips per hour", colour = NULL) +
  theme_minimal()
print(p5)
ggsave(file.path(figures, "05_hour_of_day.png"), p5, width = 8, height = 4, dpi = 150)