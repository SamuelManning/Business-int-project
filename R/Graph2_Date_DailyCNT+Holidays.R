library(tidyverse)
library(lubridate)
library(plotly)
library(dplyr)
library(zoo)

# Load datasets
bikes    <- read_csv("hour.csv")
holidays <- read_csv("us_federal_holidays_2011_2012.csv")

# Convert dates
bikes <- bikes |> mutate(dteday = ymd(dteday))

holidays <- holidays |>
  mutate(date = dmy(date)) |>
  select(date, holiday_name = holiday) |>   # rename avoids clash with bikes$holiday
  distinct(date, .keep_all = TRUE)

# Join holidays to the bike dataset
bikes <- bikes |>
  left_join(holidays, by = c("dteday" = "date"), relationship = "many-to-one") |>
  mutate(
    day_type = if_else(!is.na(holiday_name), "Holiday", "Non-holiday"),
    day_category = case_when(
      !is.na(holiday_name)  ~ "Holiday",
      weekday %in% c(0, 6)  ~ "Weekend",
      TRUE                  ~ "Weekday"
    )
  )

# Aggregate hourly rentals into daily totals
daily <- bikes |>
  group_by(dteday, day_type, holiday_name) |>
  summarise(daily_rentals = sum(cnt), .groups = "drop") |>
  arrange(dteday)

daily <- daily |>
  mutate(rolling_7d = rollmean(daily_rentals, k = 7, fill = NA, align = "center"))

fig2 <- plot_ly() |>
  add_lines(
    data = daily, x = ~dteday, y = ~daily_rentals,
    name = "Daily total", line = list(color = "lightgrey")
  ) |>
  add_lines(
    data = daily, x = ~dteday, y = ~rolling_7d,
    name = "7-day average", line = list(color = "#1f77b4", width = 3)
  ) |>
  add_markers(
    data = dplyr::filter(daily, day_type == "Holiday"),
    x = ~dteday, y = ~daily_rentals,
    name = "Holiday",
    marker = list(color = "#d62728", size = 10, symbol = "diamond"),
    text = ~holiday_name,
    hovertemplate = "%{text}<br>%{x|%d %b %Y}<br>%{y:,} rentals<extra></extra>"
  ) |>
  layout(
    title = "Daily Bike Rentals (Holidays Highlighted)",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Total daily rentals")
  )

fig2
