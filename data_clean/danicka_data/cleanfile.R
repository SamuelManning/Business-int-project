# ============================================================
# DIVVY BIKE-SHARE DATA: CLEANING AND COMBINING
# October, November and December 2025
# ============================================================

library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(geosphere)

# 1. Select the ZIP file ---------------------------------------

zip_file <- file.choose()

# 2. Create output and extraction folders ----------------------

output_dir <- "cleaned_divvy_data"
extraction_dir <- file.path(output_dir, "extracted")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

unzip(zip_file, exdir = extraction_dir)

# 3. Find CSV files --------------------------------------------

csv_files <- list.files(
  extraction_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
)

print(basename(csv_files))

# 4. Import and clean each CSV ---------------------------------

read_divvy <- function(file) {
  df <- read_csv(
    file,
    col_types = cols(.default = col_character()),
    na = c("", "NA"),
    show_col_types = FALSE
  )

  # Standardise column names
  names(df) <- names(df) |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove("^_") |>
    str_remove("_$")

  required_columns <- c(
    "ride_id", "rideable_type", "started_at", "ended_at",
    "start_station_name", "start_station_id",
    "end_station_name", "end_station_id",
    "start_lat", "start_lng", "end_lat", "end_lng",
    "member_casual"
  )

  missing_columns <- setdiff(required_columns, names(df))

  if (length(missing_columns) > 0) {
    stop(
      "Missing columns in ", basename(file), ": ",
      paste(missing_columns, collapse = ", ")
    )
  }

  # Trim whitespace and convert empty text to NA
  text_columns <- c(
    "ride_id", "rideable_type",
    "start_station_name", "start_station_id",
    "end_station_name", "end_station_id",
    "member_casual"
  )

  df <- df |>
    mutate(across(
      all_of(text_columns),
      ~ na_if(str_trim(.x), "")
    )) |>
    mutate(
      ride_id = str_to_upper(ride_id),
      rideable_type = str_to_lower(rideable_type),
      member_casual = str_to_lower(member_casual)
    )

  # Parse timestamps
  df <- df |>
    mutate(
      started_at = ymd_hms(started_at, quiet = TRUE),
      ended_at = ymd_hms(ended_at, quiet = TRUE)
    )

  # Parse coordinates
  coordinate_columns <- c(
    "start_lat", "start_lng", "end_lat", "end_lng"
  )

  df <- df |>
    mutate(across(
      all_of(coordinate_columns),
      ~ parse_number(.x, na = c("", "NA"))
    ))

  # Record the original CSV filename
  df$source_file <- basename(file)

  df
}

datasets <- map(csv_files, read_divvy)

# 5. Combine the datasets --------------------------------------

combined <- bind_rows(datasets)

if (nrow(combined) == 0) {
  stop("The CSV files were found, but contain no data rows.")
}

if (!"ride_id" %in% names(combined)) {
  stop("The combined dataset does not contain a ride_id column.")
}

cat("Rows imported:", nrow(combined), "\n")

# 6. Flag missing fields and calculate duration ----------------

combined <- combined |>
  mutate(
    missing_ride_id = is.na(ride_id) | ride_id == "",
    missing_ride_type = is.na(rideable_type),
    missing_start_time = is.na(started_at),
    missing_end_time = is.na(ended_at),
    missing_rider_type = is.na(member_casual),
    ride_duration_min = as.numeric(
      difftime(ended_at, started_at, units = "mins")
    ),
    invalid_duration = is.na(ride_duration_min) |
      ride_duration_min <= 0,
    over_24_hours = !is.na(ride_duration_min) &
      ride_duration_min > 1440
  )

# 7. Remove records missing essential fields -------------------

cleaned <- combined |>
  filter(
    !missing_ride_id,
    !missing_ride_type,
    !missing_start_time,
    !missing_end_time,
    !missing_rider_type
  )

# 8. Remove duplicate trip IDs ---------------------------------

cleaned <- cleaned |>
  distinct(ride_id, .keep_all = TRUE)

# 9. Correct invalid coordinates -------------------------------

cleaned <- cleaned |>
  mutate(
    start_lat = if_else(
      !is.na(start_lat) & (start_lat < -90 | start_lat > 90),
      NA_real_, start_lat
    ),
    start_lng = if_else(
      !is.na(start_lng) & (start_lng < -180 | start_lng > 180),
      NA_real_, start_lng
    ),
    end_lat = if_else(
      !is.na(end_lat) & (end_lat < -90 | end_lat > 90),
      NA_real_, end_lat
    ),
    end_lng = if_else(
      !is.na(end_lng) & (end_lng < -180 | end_lng > 180),
      NA_real_, end_lng
    )
  )

# 10. Add analysis variables -----------------------------------

cleaned <- cleaned |>
  mutate(
    ride_date = as.Date(started_at),
    ride_month = format(started_at, "%Y-%m"),
    start_hour = hour(started_at),
    day_of_week = wday(
      started_at,
      label = TRUE,
      abbr = FALSE
    ),
    is_weekend = wday(started_at, week_start = 1) >= 6,
    valid_duration = !is.na(ride_duration_min) &
      ride_duration_min > 0,
    straight_line_distance_km = {
      coords_available <-
        !is.na(start_lat) & !is.na(start_lng) &
          !is.na(end_lat) & !is.na(end_lng)

      distance <- rep(NA_real_, n())

      if (any(coords_available)) {
        distance[coords_available] <- distHaversine(
          cbind(
            start_lng[coords_available],
            start_lat[coords_available]
          ),
          cbind(
            end_lng[coords_available],
            end_lat[coords_available]
          )
        ) / 1000
      }

      distance
    }
  )

# 11. Export only the cleaned dataset --------------------------

output_file <- file.path(
  output_dir,
  "divvy_trips_cleaned.csv"
)

write_csv(cleaned, output_file)

cat("Cleaning complete.\n")
cat("Rows retained:", nrow(cleaned), "\n")
cat("Output file:", normalizePath(output_file), "\n")
