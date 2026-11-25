##########################################################
# BUINT Group Project - Stage 2: CTA data (buses + trains)
# Yassin
#
# What this script does:
#   - reads the CTA ridership data (daily boardings)
#   - keeps only the year 2025
#   - checks the data quality
#   - adds some extra columns (weekday, weekend, holiday, season)
#   - saves a clean csv file for the group
#   - makes some plots for the paper
#
# Data source: City of Chicago Data Portal
# "CTA - Ridership - Daily Boarding Totals"
##########################################################


# ---- Packages ----
# tidyverse = dplyr, ggplot2, readr etc.
# lubridate = for working with dates
# zoo = for the 7 day average (install once with install.packages("zoo"))
library(tidyverse)
library(lubridate)
library(zoo)

# IMPORTANT: open bike-sharing-repo.Rproj first!
# then R starts in the main folder of our GitHub project
# and all the paths below work for everyone in the group

# R uses German weekday names on my laptop (Mo, Di, ...)
# so I switch it to English
Sys.setlocale("LC_TIME", "English")


# ---- 1. Read the data ----
# the raw file is not on GitHub (data_raw is in .gitignore)
# you need to download it yourself and save it here:
#   data_raw/yassin_cta_daily_boardings/cta_daily_boardings.csv
# Download (CSV):
#   https://data.cityofchicago.org/api/views/6iiy-9s97/rows.csv?accessType=DOWNLOAD
# if the file is missing, the script stops with a message
if (!file.exists("data_raw/yassin_cta_daily_boardings/cta_daily_boardings.csv")) {
  stop("File data_raw/yassin_cta_daily_boardings/cta_daily_boardings.csv not found. Please download it first (link in the comment above).")
}

cta_raw <- read_csv("data_raw/yassin_cta_daily_boardings/cta_daily_boardings.csv")

# first look at the data
head(cta_raw)
str(cta_raw)
# -> service_date is text (chr), not a date yet


# ---- 2. Make a real date and keep only 2025 ----
# the date looks like "01/31/2025" -> month/day/year -> mdy()
cta <- cta_raw %>%
  mutate(date = mdy(service_date)) %>%
  filter(year(date) == 2025) %>%
  arrange(date)

nrow(cta)   # 2025 has 365 days, so this should be 365


# ---- 3. Data quality checks ----

# are there days that appear twice?
sum(duplicated(cta$date))

# are there days missing?
all_days <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
missing_days <- all_days[!(all_days %in% cta$date)]
missing_days

# are there empty values (NA)?
colSums(is.na(cta))

# are there negative numbers? (not possible for passengers)
sum(cta$total_rides < 0)

# does bus + rail give the total?
sum(cta$bus + cta$rail_boardings != cta$total_rides)

# min, max, mean
summary(cta)

# Result: 365 days, no duplicates, no missing values,
# no negative values and the totals are correct.


# ---- 4. Cleaning ----

# remove duplicates (there are none, but just to be safe)
cta <- distinct(cta, date, .keep_all = TRUE)

# US holidays 2025 (on these days less people use the CTA)
holidays <- data.frame(
  date = as.Date(c("2025-01-01", "2025-01-20", "2025-02-17", "2025-05-26",
                   "2025-06-19", "2025-07-04", "2025-09-01", "2025-10-13",
                   "2025-11-11", "2025-11-27", "2025-12-25")),
  holiday_name = c("New Year's Day", "Martin Luther King Jr. Day",
                   "Presidents' Day", "Memorial Day", "Juneteenth",
                   "Independence Day", "Labor Day", "Columbus Day",
                   "Veterans Day", "Thanksgiving", "Christmas Day")
)

# add the holidays to our data
cta <- left_join(cta, holidays, by = "date")

# add new columns
cta <- cta %>%
  mutate(
    weekday = wday(date, label = TRUE, week_start = 1),  # Mon, Tue, ...
    weekend = weekday %in% c("Sat", "Sun"),              # TRUE or FALSE
    holiday = !is.na(holiday_name),                      # TRUE if it is a holiday
    month   = month(date),
    season  = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5)  ~ "Spring",
      month %in% c(6, 7, 8)  ~ "Summer",
      TRUE                   ~ "Autumn"
    )
  )

# put the seasons in the right order for the plots
cta$season <- factor(cta$season, levels = c("Winter", "Spring", "Summer", "Autumn"))


# ---- 5. Find unusual days (outliers) ----
# I compare every day with the mean of the same day type
# (W = weekday, A = Saturday, U = Sunday/holiday)
# z-score = how many standard deviations away from the mean
# if the z-score is bigger than 2.5 -> unusual day
# I do NOT delete them, because they are real (holidays, events)

cta <- cta %>%
  group_by(day_type) %>%
  mutate(z_score = (total_rides - mean(total_rides)) / sd(total_rides)) %>%
  ungroup() %>%
  mutate(outlier = abs(z_score) > 2.5)

# show the unusual days
cta %>%
  filter(outlier == TRUE) %>%
  select(date, weekday, holiday_name, total_rides)

# Explanation of the unusual days:
#   20.01. MLK Day, 17.02. Presidents Day -> holidays
#   21.01. very cold day (check with the weather data)
#   28.11. day after Thanksgiving
#   24.12. - 29.12. Christmas holidays
#   15.03. (Saturday, high) St. Patrick's Day parade
#   12.10. (Sunday, high) Chicago Marathon


# ---- 6. Save the clean data ----
dir.create("data_clean/yassin_cta_daily_boardings", showWarnings = FALSE)
# rename the columns so everyone in the group knows they are CTA columns
cta_clean <- cta %>%
  select(date, day_type, weekday, weekend, holiday, holiday_name, month, season,
         cta_bus   = bus,
         cta_rail  = rail_boardings,
         cta_total = total_rides,
         outlier)

nrow(cta_clean)   # should still be 365

write_csv(cta_clean, "data_clean/yassin_cta_daily_boardings/cta_daily_2025.csv")


# ---- 7. Quality report for the paper ----
# sink() writes everything that is printed into a text file
sink("data_clean/yassin_cta_daily_boardings/cta_quality_report.txt")

cat("CTA Daily Boarding Totals 2025 - Data quality report\n\n")
cat("Rows in raw file (2001-2026):", nrow(cta_raw), "\n")
cat("Rows in 2025:", nrow(cta_clean), "\n")
cat("Duplicate days:", sum(duplicated(cta_clean$date)), "\n")
cat("Missing days:", length(missing_days), "\n")
cat("Missing values:", sum(is.na(cta_clean$cta_total)), "\n")
cat("Days where bus + rail is not the total:",
    sum(cta_clean$cta_bus + cta_clean$cta_rail != cta_clean$cta_total), "\n\n")

cat("Summary of boardings per day:\n")
print(summary(cta_clean[, c("cta_bus", "cta_rail", "cta_total")]))

cat("\nMean boardings per day type:\n")
print(cta_clean %>% group_by(day_type) %>% summarise(days = n(), mean_total = round(mean(cta_total))))

cat("\nUnusual days (kept in the data):\n")
print(cta_clean %>% filter(outlier == TRUE) %>% select(date, weekday, holiday_name, cta_total))

sink()   # stop writing into the file


# ---- 8. Plots ----
# width 3.5 inch = one column in the IEEE paper

# Plot 1: bus and rail over the year (7 day average, so the line is smoother)
# rollmean() takes the average of 7 days
cta_clean$bus_7d  <- rollmean(cta_clean$cta_bus,  k = 7, fill = NA)
cta_clean$rail_7d <- rollmean(cta_clean$cta_rail, k = 7, fill = NA)

plot_trend <- ggplot(cta_clean, aes(x = date)) +
  geom_line(aes(y = bus_7d / 1000,  colour = "Bus"),  na.rm = TRUE) +
  geom_line(aes(y = rail_7d / 1000, colour = "Rail"), na.rm = TRUE) +
  labs(x = NULL, y = "Boardings (thousands, 7-day avg.)", colour = NULL) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top")

plot_trend
ggsave("figures/cta_trend_7d.png", plot_trend, width = 3.5, height = 2.4, dpi = 300)

# Plot 2: boardings per weekday
plot_weekday <- ggplot(cta_clean, aes(x = weekday, y = cta_total / 1000)) +
  geom_boxplot() +
  labs(x = NULL, y = "Boardings (thousands)") +
  theme_minimal(base_size = 9)

plot_weekday
ggsave("figures/cta_weekday.png", plot_weekday, width = 3.5, height = 2.4, dpi = 300)

# Plot 3: boardings per season, weekday vs weekend
plot_season <- ggplot(cta_clean, aes(x = season, y = cta_total / 1000, fill = weekend)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey80", "grey40"), labels = c("Weekday", "Weekend")) +
  labs(x = NULL, y = "Boardings (thousands)", fill = NULL) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top")

plot_season
ggsave("figures/cta_season.png", plot_season, width = 3.5, height = 2.4, dpi = 300)

# Done!
# Output: data_clean/yassin_cta_daily_boardings/cta_daily_2025.csv
#         data_clean/yassin_cta_daily_boardings/cta_quality_report.txt
#         figures/cta_trend_7d.png, cta_weekday.png, cta_season.png
