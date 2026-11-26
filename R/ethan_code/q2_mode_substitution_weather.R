# Q2 diagnostic: do bikes, transit and cars respond differently to rain, snow and heat?

# We're not doing rideshare data, so road speed is a stand-in for car use:
#  lower speed = more congestion. Caveat: rain slows drivers down even without extra cars

library(tidyverse)
library(scales)

fig_dir <- "figures/ethan_mode_substitution"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Using fixed colors for every mode so it looks consistent
mode_cols <- c("Divvy bikes" = "#257de0", "CTA rail" = "#f47230",
               "CTA bus" = "#00b894", "Road speed" = "#f2a600")

divvy <- read_csv("data_clean/ethan_chicago_data/divvy_daily_bike.csv",
                  show_col_types = FALSE) |>
  select(date, divvy = total)

# CTA's day_type is W = weekday, A = Saturday, U = Sunday/holiday.
# `outlier` marks holidays
cta <- read_csv("data_clean/yassin_cta_daily_boardings/cta_daily_2025.csv",
                show_col_types = FALSE) |>
  select(date, day_type, holiday, outlier, cta_rail, cta_bus)

# Dates here are day/month/year. 
traffic <- read_csv("data_clean/danicka_data/daily_traffic.csv",
                    col_types = cols(DATE = col_date("%d/%m/%Y"), .default = col_double())) |>
  select(date = DATE, road_speed = AVG_SPEED_MOVING)

# Using the hourly
weather <- read_csv("data_clean/ethan_chicago_data/WeatherDataChicago_2025.csv",
                    skip = 3,   # first three lines describe the location
                    col_types = cols(time = col_character(), .default = col_double())) |>
  set_names(c("time", "temp_c", "humidity", "feels_like_c", "precip_mm",
              "snow_cm", "snow_depth_m", "rain_mm", "wind_kmh", "gust_kmh")) |>
  mutate(date = as.Date(substr(time, 1, 10))) |>
  group_by(date) |>
  summarise(temp_mean_f = mean(temp_c) * 9 / 5 + 32,
            temp_max_f  = max(temp_c)  * 9 / 5 + 32,
            precip_in   = sum(precip_mm) / 25.4,   # rain + melted snow
            snow_in     = sum(snow_cm) / 2.54,
            .groups = "drop")


# 2. ONE ROW PER DAY, ONE COLUMN PER MODE ---------------------------------------

daily <- cta |>
  left_join(divvy,   by = "date") |>
  left_join(traffic, by = "date") |>
  left_join(weather, by = "date") |>
  mutate(
    month = month(date),
    # Drop holidays and event days because they mess with the graphs
    exclude = holiday | outlier,
    weather_type = case_when(snow_in > 0.1     ~ "Snow",
                             precip_in > 0.1   ~ "Rain",
                             temp_max_f > 85   ~ "Hot",
                             precip_in <= 0.01 ~ "Dry",
                             TRUE              ~ "Light rain"),
    precip_cat = cut(precip_in, breaks = c(-Inf, 0.01, 0.1, 0.3, Inf),
                     labels = c("Dry", "Light\n(<0.1 in)",
                                "Moderate\n(0.1-0.3 in)", "Heavy\n(>0.3 in)"))
  )

# Making it long so we can go by mode more easily 
long <- daily |>
  pivot_longer(c(divvy, cta_rail, cta_bus, road_speed),
               names_to = "mode", values_to = "value") |>
  mutate(mode = recode(mode, divvy = "Divvy bikes", cta_rail = "CTA rail",
                       cta_bus = "CTA bus", road_speed = "Road speed") |>
           factor(levels = names(mode_cols))) |>
  filter(!is.na(value))



# To compare them fairly, every day is measured against a normal day value: the median of the same mode, same day type (weekday / Saturday / Sunday), within 14 days either side.

reference_days <- long |>
  filter(!exclude) |>
  select(mode, day_type, ref_date = date, ref_value = value)

normal <- long |>
  select(mode, day_type, date) |>
  inner_join(reference_days, by = c("mode", "day_type"),
             relationship = "many-to-many") |>
  filter(ref_date != date, abs(as.numeric(ref_date - date)) <= 14) |>
  group_by(mode, date) |>
  summarise(normal = median(ref_value), .groups = "drop")

long <- long |>
  left_join(normal, by = c("mode", "date")) |>
  mutate(pct_vs_normal = value / normal - 1) |>
  filter(!exclude)

# Mean % change with 95% confidence interval 
summarise_pct <- function(df) {
  summarise(df, pct = mean(pct_vs_normal),
            se  = sd(pct_vs_normal) / sqrt(n()),
            n_days = n(), .groups = "drop") |>
    mutate(low = pct - 1.96 * se, high = pct + 1.96 * se)
}
 
# might have to change this theme I'm stealing it currently
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = "bold"),
                  legend.position = "top"))

save_plot <- function(p, name, w = 10, h = 6) {
  ggsave(file.path(fig_dir, name), p, width = w, height = h, dpi = 150, bg = "white")
}


# First chart how each mode reacts to rain, snow and heat 

conditions <- list(
  "Rain > 0.1 in"       = quote(precip_in > 0.1 & snow_in <= 0.1),
  "Heavy rain > 0.3 in" = quote(precip_in > 0.3 & snow_in <= 0.1),
  "Snow day"            = quote(snow_in > 0.1),
  "Hot (max > 85°F)" = quote(temp_max_f > 85)
)

by_condition <- imap_dfr(conditions, function(cond, name) {
  long |> filter(!!cond) |> group_by(mode) |> summarise_pct() |> mutate(condition = name)
}) |>
  mutate(condition = factor(condition, levels = names(conditions)))

cat("\n--- % vs normal day, by weather condition ---\n")
print(by_condition |> select(condition, mode, pct, low, high, n_days), n = Inf)

p1 <- ggplot(by_condition, aes(condition, pct, fill = mode)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = position_dodge(width = 0.85), width = 0.25, colour = "grey30") +
  scale_fill_manual(values = mode_cols) +
  scale_y_continuous(labels = percent) +
  labs(title = "Bikes drop sharply in bad weather; transit and roads barely move",
       subtitle = paste("Average % difference from a normal day (same mode, day type, ±14 days).",
                        "Error bars = 95% CI.\nRoad speed: below 0 means slower, more congested roads."),
       x = NULL, y = "Change vs normal day", fill = NULL)
save_plot(p1, "q2_mode_01_response_by_condition.png")


# Second chart: dose-response, does heavier rain push the modes further apart?

by_rain <- long |>
  filter(snow_in <= 0.1) |>      # rain only
  group_by(mode, precip_cat) |>
  summarise_pct()

p2 <- ggplot(by_rain, aes(precip_cat, pct, colour = mode, group = mode)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_line(linewidth = 1, position = position_dodge(width = 0.3)) +
  geom_pointrange(aes(ymin = low, ymax = high), position = position_dodge(width = 0.3)) +
  geom_text(data = filter(by_rain, mode == "Divvy bikes"),
            aes(label = paste0("n=", n_days)), colour = "grey40", size = 3,
            vjust = 2.2, position = position_dodge(width = 0.3)) +
  scale_colour_manual(values = mode_cols) +
  scale_y_continuous(labels = percent) +
  labs(title = "The heavier the rain, the bigger the bike drop",
       subtitle = "Average % vs normal day by daily precipitation (snow days excluded). Bars = 95% CI",
       x = "Daily precipitation", y = "Change vs normal day", colour = NULL)
save_plot(p2, "q2_mode_02_rain_dose_response.png")


# Third chart: day-by-day, when Divvy falls, does CTA rise? 
# (bikes down, CTA up) would be substitution

scatter <- long |>
  filter(mode %in% c("Divvy bikes", "CTA rail", "CTA bus")) |>
  select(date, mode, pct_vs_normal, weather_type, value, normal) |>
  # Combine bus + rail into one CTA figure
  mutate(mode = if_else(mode == "Divvy bikes", "divvy", "cta")) |>
  group_by(date, mode, weather_type) |>
  summarise(value = sum(value), normal = sum(normal), .groups = "drop") |>
  mutate(pct = value / normal - 1) |>
  select(-value, -normal) |>
  pivot_wider(names_from = mode, values_from = pct) |>
  filter(!is.na(divvy), !is.na(cta)) |>
  mutate(weather_type = factor(weather_type,
                               levels = c("Dry", "Light rain", "Rain", "Snow", "Hot")))

rain_days <- filter(scatter, weather_type == "Rain")
rain_cor  <- cor(rain_days$divvy, rain_days$cta)
cat(sprintf("\nCorrelation of Divvy vs CTA change on rainy days: %.2f (n = %d)\n",
            rain_cor, nrow(rain_days)))

p3 <- ggplot(scatter, aes(divvy, cta, colour = weather_type)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
           fill = "#f47230", alpha = 0.06) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2.2, alpha = 0.8) +
  annotate("text", x = -1, y = Inf, hjust = 0, vjust = 1.5, size = 3.5, colour = "grey30",
           label = "Bikes down, CTA up\n= substitution") +
  annotate("text", x = -1, y = -Inf, hjust = 0, vjust = -0.5, size = 3.5, colour = "grey30",
           label = "Both down\n= trips not made") +
  scale_colour_manual(values = c(Dry = "grey70", "Light rain" = "#84b9f3",
                                 Rain = "#257de0", Snow = "#0e3d7e", Hot = "#f47230")) +
  scale_x_continuous(labels = percent) +
  # Divvy can't fall below -100%; zoom to +/-100% 
  coord_cartesian(xlim = c(-1, 1)) +
  scale_y_continuous(labels = percent) +
  labs(title = "Day by day: when Divvy falls, CTA does not pick up the riders",
       subtitle = sprintf("Each point is one day, %% vs normal. On rainy days the correlation is %.2f.",
                          rain_cor),
       x = "Divvy change vs normal day", y = "CTA (bus + rail) change vs normal day",
       colour = NULL) +
  guides(colour = guide_legend(nrow = 1))
save_plot(p3, "q2_mode_03_divvy_vs_cta_scatter.png", w = 9, h = 7)


# Fourth chart , regression, effect of each weather factor holding the others fixed ---
# Rain, snow and heat each tend to come with a season. So Im gonna separate
# them: log(mode) ~ rain + snow + hot day + day type + month.

fit_mode <- function(df) {
  lm(log(value) ~ I(precip_in / 0.25) + snow_in + I(temp_max_f > 85) +
       day_type + factor(month), data = df)
}
term_labels <- c("I(precip_in/0.25)"     = "+0.25 in precipitation",
                 "snow_in"               = "+1 in snowfall",
                 "I(temp_max_f > 85)TRUE" = "Hot day (max > 85°F)")

effects <- long |>
  group_by(mode) |>
  group_modify(~ broom::tidy(fit_mode(.x), conf.int = TRUE)) |>
  ungroup() |>
  filter(term %in% names(term_labels)) |>
  mutate(term = factor(term_labels[term], levels = term_labels),
         across(c(estimate, conf.low, conf.high), ~ exp(.x) - 1))

cat("\n--- Regression: % change per factor, holding the others fixed ---\n")
print(effects |> select(mode, term, estimate, conf.low, conf.high, p.value), n = Inf)

p4 <- ggplot(effects, aes(estimate, fct_rev(mode), colour = mode)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.5,
                  show.legend = FALSE) +
  facet_wrap(~ term, ncol = 1) +
  scale_colour_manual(values = mode_cols) +
  scale_x_continuous(labels = percent) +
  labs(title = "Weather effects on each mode, holding month and day type fixed",
       subtitle = "% change in daily trips (road: speed), with 95% confidence intervals",
       x = "Change", y = NULL) +
  theme(strip.text = element_text(face = "bold", hjust = 0))
save_plot(p4, "q2_mode_04_regression_effects.png", w = 9, h = 7)


write_csv(daily, "data_clean/ethan_chicago_data/daily_modes_weather_2025.csv")
