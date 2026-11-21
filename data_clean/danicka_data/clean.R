
# ==========================================
# CHICAGO TRAFFIC DATA - 2025 CLEANING
# ==========================================

library(readr)
library(dplyr)
library(lubridate)

# STEP 1: Select your original CSV file
traffic <- read_csv(file.choose(), show_col_types = FALSE)

# STEP 2: Check that the dataset imported
cat("Original records:", nrow(traffic), "\n")
print(names(traffic))

# STEP 3: Convert TIME column
traffic$TIME <- mdy_hms(traffic$TIME, quiet = TRUE)

# STEP 4: Filter records for 2025
traffic_2025 <- traffic %>%
  filter(
    !is.na(TIME),
    year(TIME) == 2025
  )

# STEP 5: Select relevant columns
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

# STEP 6: Check the filtered dataset
cat("2025 records:", nrow(traffic_2025), "\n")
cat("Number of columns:", ncol(traffic_2025), "\n")

print(head(traffic_2025))

# STEP 7: Check date range
print(range(traffic_2025$TIME, na.rm = TRUE))

# STEP 8: Check monthly records
print(traffic_2025 %>% count(MONTH))

# STEP 9: Export cleaned dataset
write_csv(
  traffic_2025,
  "traffic_congestion_2025.csv"
)

cat("SUCCESS: 2025 dataset created and exported!\n")
