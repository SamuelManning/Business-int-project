library(tidyverse)
library(lubridate)
library(plotly)
library(dplyr)
library(zoo)       # for the rolling average

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

print(head(daily))
print(count(daily, day_type))
