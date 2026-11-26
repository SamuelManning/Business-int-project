library(readr)
library(dplyr)
library(lubridate)

traffic <- read_csv(file.choose(), show_col_types = FALSE)

traffic$TIME <- mdy_hms(traffic$TIME, quiet = TRUE)

traffic_2025 <- traffic %>%
  filter(
    !is.na(TIME),
    year(TIME) == 2025
  )

traffic_2025 <- traffic_2025 %>%
  select(
    TIME,
    REGION_ID,
    SPEED,
    REGION,
    BUS_COUNT,
    NUM_READS,
    HOUR,
    DAY_OF_WEEK,
    MONTH,
    DESCRIPTION
  )
  arrange(TIME)

print(head(traffic_2025))

print(range(traffic_2025$TIME, na.rm = TRUE))

print(traffic_2025 %>% count(MONTH))

write_csv(
  traffic_2025,
  "traffic_congestion_2025.csv"
)

# Read traffic data
traffic <- read_csv(
  "data_clean/danicka_data/traffic_congestion_2025.csv"
)

# combining traffic data by day
daily_traffic <- traffic %>%
  mutate(DATE = as.Date(TIME)) %>%
  group_by(DATE) %>%
  summarise(
    AVG_SPEED = mean(SPEED, na.rm = TRUE),
    AVG_BUS_COUNT = mean(BUS_COUNT, na.rm = TRUE),
    AVG_NUM_READS = mean(NUM_READS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(DATE)

write_csv(
  daily_traffic,
  "data_clean/danicka_data/daily_traffic.csv"
)

#Checking the avg_speed quantiles
summary(daily_traffic$AVG_SPEED)

quantile(
  daily_traffic$AVG_SPEED,
  probs = c(0.25, 0.50, 0.75),
  na.rm = TRUE
)
#This gave too similar traffic speeds of 25th percentile - 23.52, Median - 23.72, 75th percentile - 23.98

#need to check individual traffic speeds
summary(traffic$SPEED)

quantile(
  traffic$SPEED,
  probs = c(0.25, 0.50, 0.75),
  na.rm = TRUE
) #gave more varied speeds - 25th - 21.82, mediaun - 25.23, 75th - 27.95

# Examine unusually high speeds
traffic %>%
  filter(SPEED > 60) %>%
  select(TIME, SPEED, REGION, DESCRIPTION) %>%
  arrange(desc(SPEED)) %>%
  print(n = 20) #204 records over 100mph which is unusual

# Count zero-speed observations
sum(traffic$SPEED == 0, na.rm = TRUE) #103,879 when speed is 0

#which zero speeds actually have traffic readings
traffic %>%
  filter(SPEED == 0) %>%
  summarise(
    TOTAL_ZERO_SPEED = n(),
    AVG_NUM_READS = mean(NUM_READS, na.rm = TRUE),
    MIN_READS = min(NUM_READS, na.rm = TRUE),
    MAX_READS = max(NUM_READS, na.rm = TRUE)
  )

#need to see if the zero speed reads are concentrated in areas
traffic %>%
  filter(SPEED == 0) %>%
  group_by(REGION) %>%
  summarise(
    ZERO_SPEED_COUNT = n(),
    AVG_NUM_READS = mean(NUM_READS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(ZERO_SPEED_COUNT))

# calculating the percentage of zero-speed observations per region
traffic %>%
  group_by(REGION) %>%
  summarise(
    TOTAL_RECORDS = n(),
    ZERO_SPEED = sum(SPEED == 0, na.rm = TRUE),
    ZERO_SPEED_PERCENT = round(
      (ZERO_SPEED / TOTAL_RECORDS) * 100, 2
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(ZERO_SPEED_PERCENT))


#improving the daily traffic table to include zero speed observations and moving observations

daily_traffic <- traffic %>%
  mutate(DATE = as.Date(TIME)) %>%
  group_by(DATE) %>%
  summarise(
    AVG_SPEED_ALL = mean(SPEED, na.rm = TRUE),

    AVG_SPEED_MOVING = mean(
      SPEED[SPEED > 0],
      na.rm = TRUE
    ),

    ZERO_SPEED_PERCENT = mean(
      SPEED == 0,
      na.rm = TRUE
    ) * 100,

    .groups = "drop"
  ) %>%
  arrange(DATE)

write_csv(
  daily_traffic,
  "data_clean/danicka_data/daily_traffic.csv"
)

#turning the traffic congestion to hourly
traffic <- read_csv("data_clean/danicka_data/traffic_congestion_2025.csv")

traffic <- traffic %>%
  mutate(TIME = ymd_hms(TIME, tz = "UTC"))

hourly_traffic <- traffic %>%
  mutate(
    DATE = as.Date(TIME),
    HOUR = hour(TIME)
  ) %>%
  group_by(DATE, HOUR, REGION_ID, REGION) %>%
  summarise(
    AVG_SPEED = mean(SPEED[SPEED > 0 & SPEED <= 60], na.rm = TRUE),
    ZERO_SPEED_PERCENT = mean(SPEED == 0, na.rm = TRUE) * 100,
    TOTAL_READINGS = n(),
    .groups = "drop"
  ) %>%
  arrange(DATE, HOUR, REGION_ID)

write_csv(
  hourly_traffic,
  "data_clean/danicka_data/hourly_traffic.csv"
)