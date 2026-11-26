# Divvy x weather, Chicago 2025, charts built around the three business questions
#
#   Q1  What drives Divvy demand?             (descriptive + diagnostic)
#   Q2  Do bikes, transit and rideshare compete? (diagnostic + predictive)
#   Q3  What should the operator do about it?  (predictive + prescriptive)
#
# Data used: Divvy_Combined.csv (all 2025 trips) + Open-Meteo hourly weather for Chicago.
# NOT available yet: CTA boardings, traffic speeds, rideshare (TNP) trips.
#   Where a chart would compare against those, it's marked "ADD CTA / RIDESHARE HERE".
#   Join them onto `daily` by date (one row per day) and they flow into the models too.
#
# Run from the project root (the folder that contains R/ and data_clean/).

library(tidyverse)
library(scales)
library(randomForest)   # install.packages("randomForest") if missing

data_dir <- "./data_clean/ethan_chicago_data"
fig_dir  <- file.path(data_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE)

# Colours: one fixed colour per thing, used the same in every chart
col_member <- "#257de0"   # blue
col_casual <- "#f47230"   # orange
rider_cols <- c(member = col_member, casual = col_casual)
bike_cols  <- c(electric_bike = "#00b894", classic_bike = "#f2a600")
model_cols <- c(Actual = "grey20", `Linear model` = col_member,
                `Random forest` = col_casual, `Naive (last week)` = "#00b894")


# 1. LOAD + CLEAN TRIPS ---------------------------------------------------------

trips <- read_csv(
  file.path(data_dir, "Divvy_Combined.csv"),
  col_select = c(rideable_type, started_at, ended_at, member_casual),
  col_types  = cols(started_at = col_datetime(), ended_at = col_datetime(),
                    .default = col_character())
) |>
  mutate(duration_min = as.numeric(difftime(ended_at, started_at, units = "mins"))) |>
  # Keep 2025 only (the January file has 53 trips from 31 Dec 2024).
  # Drop trips under 1 minute (false starts / re-docks) and over 24 hours
  # (lost or stolen bikes); this also removes the 29 negative durations.
  filter(year(started_at) == 2025, between(duration_min, 1, 1440)) |>
  transmute(
    date = as_date(started_at),
    hour = hour(started_at),
    rideable_type,
    member_casual = factor(member_casual, levels = c("member", "casual")),
    duration_min
  )


# 2. LOAD WEATHER (hourly -> daily) --------------------------------------------

weather <- read_csv(
  file.path(data_dir, "open-meteo-41.86N87.65W179m.csv"),
  skip = 3,   # first three lines describe the location
  col_types = cols(time = col_character(), .default = col_double())
) |>
  # Short names, in the file's column order
  set_names(c("time", "temp_c", "humidity", "feels_like_c", "precip_mm",
              "snow_cm", "snow_depth_m", "rain_mm", "wind_kmh", "gust_kmh")) |>
  mutate(date = as.Date(substr(time, 1, 10))) |>   # "2025-10-01T00:00" -> date
  group_by(date) |>
  summarise(
    temp_mean_f   = mean(temp_c) * 9 / 5 + 32,
    temp_max_f    = max(temp_c)  * 9 / 5 + 32,
    temp_min_f    = min(temp_c)  * 9 / 5 + 32,
    feels_like_f  = mean(feels_like_c) * 9 / 5 + 32,
    humidity      = mean(humidity),
    precip_in     = sum(precip_mm) / 25.4,   # rain + snow as liquid water
    rain_in       = sum(rain_mm)   / 25.4,
    snow_in       = sum(snow_cm)   / 2.54,
    snow_depth_in = max(snow_depth_m) * 39.37,
    wind_max_mph  = max(wind_kmh) / 1.609344,
    .groups = "drop"
  )


# 3. CALENDAR FEATURES ---------------------------------------------------------

# US federal holidays 2025
holidays_2025 <- as.Date(c("2025-01-01", "2025-01-20", "2025-02-17", "2025-05-26",
                           "2025-06-19", "2025-07-04", "2025-09-01", "2025-10-13",
                           "2025-11-11", "2025-11-27", "2025-12-25"))

# Hours of daylight in Chicago (lat 41.88). Captures "time of year" smoothly,
# so the model can be tested on Oct-Dec without ever seeing those months.
daylight_hours <- function(date, lat = 41.88) {
  decl <- 23.44 * sin(2 * pi * (284 + yday(date)) / 365) * pi / 180
  24 / pi * acos(-tan(lat * pi / 180) * tan(decl))
}


# 4. ONE ROW PER DAY -----------------------------------------------------------

daily <- trips |>
  group_by(date) |>
  summarise(
    rides        = n(),
    member_rides = sum(member_casual == "member"),
    casual_rides = sum(member_casual == "casual"),
    ebike_share  = mean(rideable_type == "electric_bike"),
    median_dur   = median(duration_min),
    .groups = "drop"
  ) |>
  inner_join(weather, by = "date") |>
  # ADD CTA / RIDESHARE / TRAFFIC HERE, e.g. left_join(cta_daily, by = "date")
  mutate(
    weekday  = wday(date, label = TRUE, week_start = 1),   # Mon..Sun
    day_type = if_else(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday"),
    holiday  = date %in% holidays_2025,
    month    = month(date, label = TRUE),
    season   = case_when(month(date) %in% c(12, 1, 2) ~ "Winter",
                         month(date) %in% 3:5         ~ "Spring",
                         month(date) %in% 6:8         ~ "Summer",
                         TRUE                         ~ "Autumn") |>
               factor(levels = c("Winter", "Spring", "Summer", "Autumn")),
    daylight_h = daylight_hours(date),
    precip_cat = cut(precip_in, breaks = c(-Inf, 0.01, 0.1, 0.3, Inf),
                     labels = c("Dry", "Light (<0.1 in)",
                                "Moderate (0.1-0.3 in)", "Heavy (>0.3 in)")),
    snow_day = snow_in > 0.1
  )

# Same table in long format: one row per day per rider type
daily_by_rider <- daily |>
  select(date, member = member_rides, casual = casual_rides, weekday, day_type,
         season, month, precip_cat, temp_mean_f) |>
  pivot_longer(c(member, casual), names_to = "member_casual", values_to = "rides") |>
  mutate(member_casual = factor(member_casual, levels = c("member", "casual")))


theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold"),
                  legend.position = "top"))

save_plot <- function(p, name, w = 9, h = 5.5) {
  ggsave(file.path(fig_dir, name), p, width = w, height = h, dpi = 150, bg = "white")
}


# ==============================================================================
# Q1  WHAT DRIVES DIVVY DEMAND?
# ==============================================================================

# --- Descriptive ---------------------------------------------------------------

# Q1.1 The year at a glance: daily rides, members vs casual, with a 7-day average
q1_trend <- daily_by_rider |>
  group_by(member_casual) |>
  arrange(date, .by_group = TRUE) |>
  mutate(rides_7d = stats::filter(rides, rep(1 / 7, 7), sides = 2) |> as.numeric())

p <- ggplot(q1_trend, aes(date, colour = member_casual)) +
  geom_line(aes(y = rides), alpha = 0.25) +
  geom_line(aes(y = rides_7d), linewidth = 1) +
  scale_colour_manual(values = rider_cols) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Daily Divvy rides in 2025, members vs casual riders",
       subtitle = "Faint line = each day, bold line = 7-day average",
       x = NULL, y = "Rides per day", colour = NULL)
save_plot(p, "q1_01_daily_rides_trend.png")

# Q1.2 Weekly pattern in each season: members commute on weekdays, casual riders peak at weekends
q1_week <- daily_by_rider |>
  group_by(season, weekday, member_casual) |>
  summarise(avg_rides = mean(rides), .groups = "drop")

p <- ggplot(q1_week, aes(weekday, avg_rides, fill = member_casual)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~ season, nrow = 1) +
  scale_fill_manual(values = rider_cols) +
  scale_y_continuous(labels = comma) +
  labs(title = "Average rides by day of week and season",
       x = NULL, y = "Avg rides per day", fill = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
save_plot(p, "q1_02_weekday_by_season.png", w = 11)

# Q1.3 Electric vs classic bikes each month, by rider type
q1_bike <- trips |>
  count(month = month(date, label = TRUE), member_casual, rideable_type)

p <- ggplot(q1_bike, aes(month, n, fill = rideable_type)) +
  geom_col(width = 0.75) +
  facet_wrap(~ member_casual) +
  scale_fill_manual(values = bike_cols, labels = c(classic_bike = "Classic",
                                                    electric_bike = "Electric")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Monthly rides by bike type",
       x = NULL, y = "Rides per month", fill = NULL)
save_plot(p, "q1_03_bike_type_by_month.png", w = 11)

# Q1.4 Hour-of-day profile: weekday vs weekend, members vs casual
days_per_type <- count(daily, day_type, name = "n_days")

q1_hour <- trips |>
  mutate(day_type = if_else(wday(date, week_start = 1) >= 6, "Weekend", "Weekday")) |>
  count(day_type, hour, member_casual) |>
  left_join(days_per_type, by = "day_type") |>
  mutate(avg_rides = n / n_days)

p <- ggplot(q1_hour, aes(hour, avg_rides, colour = member_casual)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ day_type) +
  scale_colour_manual(values = rider_cols) +
  scale_x_continuous(breaks = seq(0, 21, 3)) +
  scale_y_continuous(labels = comma) +
  labs(title = "When people ride: average rides by hour",
       subtitle = "Weekday commuter peaks at 8am and 5pm come mostly from members",
       x = "Hour of day", y = "Avg rides in that hour", colour = NULL)
save_plot(p, "q1_04_hourly_profile.png", w = 11)

# --- Diagnostic ----------------------------------------------------------------

# Q1.5 Rides vs temperature
p <- ggplot(daily_by_rider, aes(temp_mean_f, rides, colour = member_casual)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
  scale_colour_manual(values = rider_cols) +
  scale_y_continuous(labels = comma) +
  labs(title = "Daily rides vs mean temperature",
       x = "Mean temperature (\u00b0F)", y = "Rides per day", colour = NULL)
save_plot(p, "q1_05_rides_vs_temp.png")

# Q1.6 Rain: compared within each season, so summer vs winter doesn't swamp the effect
p <- ggplot(daily_by_rider, aes(precip_cat, rides, fill = member_casual)) +
  geom_boxplot(outlier.size = 0.8) +
  facet_wrap(~ season, nrow = 1) +
  scale_fill_manual(values = rider_cols) +
  scale_y_continuous(labels = comma) +
  labs(title = "Daily rides by precipitation level, within each season",
       x = NULL, y = "Rides per day", fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p, "q1_06_rides_by_precip_season.png", w = 12)

# Q1.7 Correlation of daily rides with each weather variable
weather_vars <- c(temp_mean_f = "Mean temp", feels_like_f = "Feels-like temp",
                  daylight_h = "Daylight hours", humidity = "Humidity",
                  precip_in = "Precipitation", snow_in = "Snowfall",
                  snow_depth_in = "Snow depth", wind_max_mph = "Max wind")
# ADD CTA / RIDESHARE / TRAFFIC HERE: add their column names to weather_vars

q1_cor <- daily |>
  select(Total = rides, Member = member_rides, Casual = casual_rides,
         all_of(names(weather_vars))) |>
  cor() |>
  as_tibble(rownames = "outcome") |>
  filter(outcome %in% c("Total", "Member", "Casual")) |>
  pivot_longer(-outcome, names_to = "var", values_to = "r") |>
  filter(var %in% names(weather_vars)) |>
  mutate(var = factor(weather_vars[var], levels = rev(weather_vars)),
         outcome = factor(outcome, levels = c("Total", "Member", "Casual")))

p <- ggplot(q1_cor, aes(outcome, var, fill = r)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 4) +
  scale_fill_gradient2(low = col_casual, mid = "grey95", high = col_member,
                       limits = c(-1, 1)) +
  labs(title = "Correlation of daily rides with weather",
       subtitle = "Blue = more rides when the variable is higher, orange = fewer",
       x = NULL, y = NULL, fill = "r")
save_plot(p, "q1_07_correlation_heatmap.png", w = 7, h = 6)

# Q1.8 Regression: % change in rides for each factor, holding the others fixed.
# log(rides) means each coefficient reads as a % change.
fit_q1 <- function(df) {
  lm(log(rides) ~ I(temp_mean_f / 10) + I(precip_in * 10) + snow_in +
       I(wind_max_mph / 10) + day_type + holiday, data = df)
}
term_labels <- c("I(temp_mean_f/10)"  = "+10\u00b0F mean temp",
                 "I(precip_in * 10)"  = "+0.1 in precipitation",
                 "snow_in"            = "+1 in snowfall",
                 "I(wind_max_mph/10)" = "+10 mph max wind",
                 "day_typeWeekend"    = "Weekend (vs weekday)",
                 "holidayTRUE"        = "Public holiday")

q1_models <- daily_by_rider |>
  left_join(select(daily, date, precip_in, snow_in, wind_max_mph, holiday), by = "date") |>
  group_by(member_casual) |>
  group_modify(~ broom::tidy(fit_q1(.x), conf.int = TRUE)) |>
  ungroup() |>
  filter(term != "(Intercept)") |>
  mutate(term = factor(term_labels[term], levels = rev(term_labels)),
         across(c(estimate, conf.low, conf.high), ~ exp(.x) - 1))

p <- ggplot(q1_models, aes(estimate, term, colour = member_casual)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.5), size = 0.5) +
  scale_colour_manual(values = rider_cols) +
  scale_x_continuous(labels = percent) +
  labs(title = "What moves daily demand? Regression on weather and calendar",
       subtitle = "% change in rides per day, with 95% confidence intervals",
       x = "Change in daily rides", y = NULL, colour = NULL)
save_plot(p, "q1_08_regression_effects.png")

cat("\n--- Q1 regression (log rides) ---\n")
print(q1_models |> select(member_casual, term, estimate, conf.low, conf.high, p.value),
      n = Inf)


# ==============================================================================
# Q2  DO BIKES, TRANSIT AND RIDESHARE COMPETE? + FORECASTING
# ==============================================================================

# --- Diagnostic: how Divvy responds to bad / extreme weather --------------------
# Each day is compared with the average for the same month and day type
# (e.g. a rainy Tuesday in May vs all May weekdays), so season isn't the cause.
# Once CTA and rideshare are joined, run the same calculation on their daily
# counts and add them as extra bars: if Divvy drops while they rise, that
# points to substitution.

q2_resp <- daily_by_rider |>
  left_join(select(daily, date, precip_in, snow_in, temp_max_f), by = "date") |>
  group_by(member_casual, month, day_type) |>
  mutate(pct_vs_normal = rides / mean(rides) - 1) |>
  ungroup()

conditions <- list(
  "Dry"                     = quote(precip_in <= 0.01),
  "Rain > 0.1 in"           = quote(precip_in > 0.1 & snow_in <= 0.1),
  "Heavy rain > 0.3 in"     = quote(precip_in > 0.3 & snow_in <= 0.1),
  "Snow day"                = quote(snow_in > 0.1),
  "Hot (max > 85\u00b0F)"        = quote(temp_max_f > 85),
  "Freezing (mean < 32\u00b0F)"  = quote(temp_mean_f < 32)
)

q2_resp_sum <- imap_dfr(conditions, function(cond, name) {
  q2_resp |>
    filter(!!cond) |>
    group_by(member_casual) |>
    summarise(pct = mean(pct_vs_normal), n_days = n(), .groups = "drop") |>
    mutate(condition = name)
}) |>
  mutate(condition = factor(condition, levels = names(conditions)))

p <- ggplot(q2_resp_sum, aes(condition, pct, fill = member_casual)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = paste0("n=", n_days),
                vjust = if_else(pct >= 0, -0.4, 1.3)),
            position = position_dodge(width = 0.8), size = 3, colour = "grey30") +
  scale_fill_manual(values = rider_cols) +
  scale_y_continuous(labels = percent, expand = expansion(mult = 0.15)) +
  labs(title = "How Divvy demand reacts to weather",
       subtitle = paste("% difference from a normal day in the same month and day type.",
                        "n = number of days.\nCTA and rideshare bars to be added once that data is joined."),
       x = NULL, y = "Rides vs normal", fill = NULL)
save_plot(p, "q2_01_weather_response.png", w = 10)

# --- Predictive: linear model vs random forest ---------------------------------
# Train on Jan-Sep, test on Oct-Dec (time-based split, so no peeking ahead).
# Features: weather + calendar. Daylight hours stands in for "time of year".
# ADD CTA / RIDESHARE HERE as extra predictors once joined.

model_data <- daily |>
  arrange(date) |>
  mutate(naive = lag(rides, 7))   # benchmark: same weekday last week

train <- filter(model_data, date <= as.Date("2025-09-30"))
test  <- filter(model_data, date >  as.Date("2025-09-30"))

lm_fit <- lm(rides ~ temp_mean_f + I(temp_mean_f^2) + precip_in + snow_in +
               snow_depth_in + wind_max_mph + humidity + daylight_h +
               weekday + holiday, data = train)

set.seed(42)
rf_fit <- randomForest(rides ~ temp_mean_f + temp_max_f + feels_like_f + precip_in +
                         snow_in + snow_depth_in + wind_max_mph + humidity +
                         daylight_h + weekday + holiday,
                       data = train, ntree = 500, importance = TRUE)

# A straight-line model can dip below zero on very cold days, so floor it at
# the quietest day seen in training.
test <- test |>
  mutate(`Linear model`                = pmax(predict(lm_fit, test), min(train$rides)),
         `Random forest`               = predict(rf_fit, test),
         `Naive (last week)`  = naive)

model_names <- c("Naive (last week)", "Linear model", "Random forest")

metrics <- test |>
  pivot_longer(all_of(model_names), names_to = "model", values_to = "pred") |>
  group_by(model) |>
  summarise(RMSE = sqrt(mean((rides - pred)^2)),
            MAE  = mean(abs(rides - pred)),
            MAPE = mean(abs(rides - pred) / rides),
            .groups = "drop") |>
  mutate(model = factor(model, levels = model_names))

cat("\n--- Q2 forecast accuracy on Oct-Dec test set ---\n")
print(metrics)

# Q2.2 Forecast vs actual over the test period
q2_pred <- test |>
  select(date, Actual = rides, all_of(model_names)) |>
  pivot_longer(-date, names_to = "series", values_to = "rides") |>
  mutate(series = factor(series, levels = names(model_cols)))

p <- ggplot(q2_pred, aes(date, rides, colour = series, linewidth = series == "Actual")) +
  geom_line() +
  scale_colour_manual(values = model_cols) +
  scale_linewidth_manual(values = c(`TRUE` = 1.2, `FALSE` = 0.7), guide = "none") +
  scale_y_continuous(labels = comma) +
  labs(title = "Forecast vs actual daily rides, Oct-Dec 2025 (test set)",
       subtitle = "Models trained on Jan-Sep only",
       x = NULL, y = "Rides per day", colour = NULL)
save_plot(p, "q2_02_forecast_vs_actual.png", w = 11)

# Q2.3 Error comparison
p <- metrics |>
  pivot_longer(c(RMSE, MAE), names_to = "measure", values_to = "value") |>
  ggplot(aes(model, value, fill = model)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = comma(value, accuracy = 1)), vjust = -0.4, size = 3.5) +
  facet_wrap(~ measure) +
  scale_fill_manual(values = model_cols) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Forecast error on the Oct-Dec test set (lower is better)",
       x = NULL, y = "Rides per day") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
save_plot(p, "q2_03_model_error.png")

# Q2.4 Which inputs matter most to the random forest
q2_imp <- importance(rf_fit, type = 1) |>
  as_tibble(rownames = "var") |>
  rename(importance = `%IncMSE`) |>
  arrange(importance) |>
  mutate(var = fct_inorder(var))

p <- ggplot(q2_imp, aes(importance, var)) +
  geom_col(fill = col_member, width = 0.6) +
  labs(title = "Random forest: which inputs matter most",
       subtitle = "% increase in error when the variable is shuffled",
       x = "% increase in MSE", y = NULL)
save_plot(p, "q2_04_rf_importance.png", w = 8)


# ==============================================================================
# Q3  WHAT SHOULD THE OPERATOR DO?
# ==============================================================================
# Uses the best Q2 model's test-period forecasts.
#
# ASSUMPTIONS, change these to test other scenarios:
rides_per_bike   <- 4      # rides one bike can serve in a day
revenue_per_ride <- 4.00   # $ lost when a ride can't happen (no bike)
idle_cost_bike   <- 1.50   # $ per bike per day deployed but not needed
#                            (maintenance, rebalancing trucks, dock space)
#
# Caveats:
# - Daily totals hide station-level shortages, so real missed rides are higher.
# - Observed rides are already limited by bikes on the street (true demand is
#   at least this high), so these estimates are conservative.

best_model <- as.character(metrics$model[which.min(metrics$RMSE)])
cat("\nBest model for Q3:", best_model, "\n")

q3 <- test |>
  transmute(date, actual = rides, forecast = .data[[best_model]],
            naive = `Naive (last week)`)

# Q3.1 Flag high- and low-demand days from the forecast
hi_cut <- quantile(q3$forecast, 0.75)
lo_cut <- quantile(q3$forecast, 0.25)
q3 <- q3 |>
  mutate(flag = case_when(forecast >= hi_cut ~ "High demand: add bikes",
                          forecast <= lo_cut ~ "Low demand: pull bikes for maintenance",
                          TRUE               ~ "Normal") |>
           factor(levels = c("High demand: add bikes", "Normal",
                             "Low demand: pull bikes for maintenance")))

p <- ggplot(q3, aes(date)) +
  geom_line(aes(y = actual), colour = "grey70", linewidth = 0.8) +
  geom_line(aes(y = forecast), colour = "grey20", linewidth = 0.6, linetype = "dashed") +
  geom_point(aes(y = forecast, colour = flag), size = 2.5) +
  scale_colour_manual(values = c("High demand: add bikes" = col_casual,
                                 "Normal" = "grey50",
                                 "Low demand: pull bikes for maintenance" = col_member)) +
  scale_y_continuous(labels = comma) +
  labs(title = paste("Forecast-flagged high and low demand days,", best_model),
       subtitle = sprintf("Dashed = forecast, grey = actual. High >= %s rides, low <= %s rides",
                         comma(hi_cut), comma(lo_cut)),
       x = NULL, y = "Rides per day", colour = NULL) +
  guides(colour = guide_legend(nrow = 1))
save_plot(p, "q3_01_flagged_days.png", w = 11)

# Simulate a plan: capacity (in rides) for each day -> missed rides, idle bikes, cost
evaluate_plan <- function(capacity, actual) {
  missed <- pmax(actual - capacity, 0)
  idle_bikes <- pmax(capacity - actual, 0) / rides_per_bike
  tibble(missed_rides = sum(missed),
         idle_bike_days = sum(idle_bikes),
         cost = sum(missed) * revenue_per_ride + sum(idle_bikes) * idle_cost_bike)
}

# Simple optimisation: try safety margins from -20% to +50% on top of the forecast
# and pick the one with the lowest total cost.
# (Margin is tuned on the test period itself to keep things simple; for a real
#  deployment you'd tune it on earlier data.)
margins <- seq(-0.2, 0.5, by = 0.01)

margin_curve <- map_dfr(margins, function(m) {
  bind_rows(
    evaluate_plan(q3$forecast * (1 + m), q3$actual) |> mutate(plan = best_model),
    evaluate_plan(q3$naive    * (1 + m), q3$actual) |> mutate(plan = "Naive (last week)")
  ) |> mutate(margin = m)
})

best_margins <- margin_curve |> group_by(plan) |> slice_min(cost, n = 1, with_ties = FALSE)
m_star <- best_margins$margin[best_margins$plan == best_model]

# Benchmark: a fixed fleet, same number of bikes every day.
# Best possible fixed size picked with hindsight, so the comparison favours it.
fixed_grid  <- seq(min(q3$actual), max(q3$actual), length.out = 200)
fixed_costs <- map_dfr(fixed_grid, ~ evaluate_plan(rep(.x, nrow(q3)), q3$actual) |>
                         mutate(capacity = .x))
fixed_cap   <- fixed_costs$capacity[which.min(fixed_costs$cost)]
fixed_bikes <- fixed_cap / rides_per_bike

# Q3.2 Cost vs safety margin
p <- ggplot(margin_curve, aes(margin, cost, colour = plan)) +
  geom_line(linewidth = 1) +
  geom_point(data = best_margins, size = 3) +
  geom_hline(yintercept = min(fixed_costs$cost), linetype = "dashed", colour = "grey40") +
  annotate("text", x = max(margins), y = min(fixed_costs$cost), hjust = 1, vjust = -0.5,
           label = "Best fixed fleet", colour = "grey40", size = 3.5) +
  scale_colour_manual(values = model_cols) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Choosing the safety margin: total cost over Oct-Dec",
       subtitle = sprintf("Cost = missed rides x $%.2f + idle bike-days x $%.2f (%d rides per bike per day)",
                         revenue_per_ride, idle_cost_bike, rides_per_bike),
       x = "Bikes deployed above (+) or below (-) the forecast", y = "Total cost",
       colour = NULL)
save_plot(p, "q3_02_cost_vs_margin.png")

# Q3.3 Benefit: missed rides and idle bikes under each plan
plans <- bind_rows(
  evaluate_plan(rep(fixed_cap, nrow(q3)), q3$actual) |> mutate(plan = "Fixed fleet"),
  evaluate_plan(q3$naive * (1 + best_margins$margin[best_margins$plan != best_model]),
                q3$actual) |> mutate(plan = "Naive (last week)"),
  evaluate_plan(q3$forecast * (1 + m_star), q3$actual) |> mutate(plan = best_model)
) |>
  mutate(plan = factor(plan, levels = c("Fixed fleet", "Naive (last week)", best_model)))

cat("\n--- Q3 plan comparison, Oct-Dec ---\n")
print(plans)
cat(sprintf(paste("Forecast plan vs fixed fleet: missed rides %s -> %s,",
                  "idle bike-days %s -> %s, total cost %s -> %s\n"),
            comma(plans$missed_rides[1]), comma(plans$missed_rides[3]),
            comma(plans$idle_bike_days[1]), comma(plans$idle_bike_days[3]),
            dollar(plans$cost[1]), dollar(plans$cost[3])))

p <- plans |>
  pivot_longer(c(missed_rides, idle_bike_days), names_to = "measure", values_to = "value") |>
  mutate(measure = recode(measure, missed_rides = "Missed rides",
                          idle_bike_days = "Idle bike-days")) |>
  ggplot(aes(plan, value, fill = plan)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = comma(value, accuracy = 1)), vjust = -0.4, size = 3.5) +
  facet_wrap(~ measure, scales = "free_y") +
  scale_fill_manual(values = c("Fixed fleet" = "grey60",
                               model_cols[model_names])) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Expected benefit of forecast-driven fleet sizing, Oct-Dec 2025",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
save_plot(p, "q3_03_plan_benefit.png", w = 10)

# Q3.4 The operating rule: "when forecast is in band X, add/remove Y bikes"
q3_rule <- q3 |>
  mutate(band = cut(forecast, breaks = pretty(forecast, n = 6), dig.lab = 6,
                    include.lowest = TRUE),
         bikes_needed = forecast * (1 + m_star) / rides_per_bike) |>
  group_by(band) |>
  summarise(days = n(),
            bikes_to_deploy = mean(bikes_needed),
            change_vs_fixed = mean(bikes_needed) - fixed_bikes,
            .groups = "drop")

cat(sprintf("\n--- Q3 operating rule (fixed fleet = %s bikes, margin = %+.0f%%) ---\n",
            comma(fixed_bikes), m_star * 100))
print(q3_rule)

p <- ggplot(q3_rule, aes(band, change_vs_fixed, fill = change_vs_fixed > 0)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%+s bikes\n(%d days)",
                                comma(change_vs_fixed, accuracy = 10), days),
                vjust = if_else(change_vs_fixed > 0, -0.2, 1.2)), size = 3.3) +
  scale_fill_manual(values = c(`TRUE` = col_casual, `FALSE` = col_member)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = 0.25)) +
  labs(title = "Operating rule: bikes to add or remove for each forecast level",
       subtitle = sprintf("Compared with a fixed fleet of %s bikes. Forecast + %.0f%% margin, %d rides per bike per day",
                         comma(fixed_bikes, accuracy = 10), m_star * 100, rides_per_bike),
       x = "Forecast rides for the day", y = "Bikes to add (+) / remove (-)")
save_plot(p, "q3_04_operating_rule.png", w = 10)


# SAVE TABLES -------------------------------------------------------------------
write_csv(daily,   file.path(data_dir, "daily_rides_weather_2025.csv"))
write_csv(metrics, file.path(data_dir, "q2_model_metrics.csv"))
write_csv(q3_rule, file.path(data_dir, "q3_operating_rule.csv"))
