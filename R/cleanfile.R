library(readr)      # read_csv(), write_csv(), parse_number()
library(dplyr)      # mutate(), filter(), distinct(), bind_rows()
library(tidyr)      # drop_na()
library(lubridate)  # ymd_hms(), hour(), wday()
library(stringr)    # str_trim(), str_to_lower(), str_replace_all()
library(purrr)      # map(): lets you apply functions to every item
library(geosphere)  # distHaversine(): distance between two lat/lng points (USING FOR THE STRAIGHT LINE FUNCTION THING)

# This lets you open a folder to find the file when the script runs.
zip_file <- file.choose()

# Putting the output in our current dir, should probably go to data_clean.
output_dir     <- "cleaned_divvy_data"
extraction_dir <- file.path(output_dir, "extracted")   # cleaned_divvy_data/extracted

# Delete any old extraction so stale CSVs from a previous run can't sneak in.
unlink(extraction_dir, recursive = TRUE)

# Create the folder. recursive = TRUE also creates output_dir if it is missing.
dir.create(extraction_dir, recursive = TRUE)

# Unzip everything into extraction_dir. If the ZIP is buste we use unzip()
unzip(zip_file, exdir = extraction_dir)

csv_files <- list.files(
  extraction_dir,
  pattern     = "\\.csv$",   # googled a reg expression
  recursive   = TRUE,        # also check inside sub-folders return full paths, not just file names, also match .CSV
  ignore.case = TRUE,
  full.names  = TRUE,
)

# If the zip is empty just stop
if (length(csv_files) == 0) {
  stop()
}

print(basename(csv_files))   # print the file names

read_divvy <- function(file) {

  # try and read all the columns as text then fix later.
  df <- read_csv(
    file,
    col_types      = cols(.default = col_character()),
    na             = c("", "NA"),     # these strings become real NA
    show_col_types = FALSE
  )

  # trying to standardize column names, capitals / spaces / symbols:
  #   trim spaces -> lower case -> any run of non a-z/0-9 becomes "_"
  names(df) <- names(df) |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove_all("^_|_$") #try delete leading and trailing underscores

  # The columns we actually want (forcing)
  required_columns <- c(
    "ride_id", "rideable_type", "started_at", "ended_at",
    "start_station_name", "start_station_id",
    "end_station_name", "end_station_id",
    "start_lat", "start_lng", "end_lat", "end_lng",
    "member_casual"
  )

  # setdiff(a, b) = items in a that are not in b -> the missing columns.
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    stop("Missing columns in ", basename(file), ": ",
         paste(missing_columns, collapse = ", "))
  }

  text_columns       <- c("ride_id", "rideable_type",
                          "start_station_name", "start_station_id",
                          "end_station_name", "end_station_id",
                          "member_casual")
  coordinate_columns <- c("start_lat", "start_lng", "end_lat", "end_lng")

  # Trying to add all the fixes in one big mutate().
  df |>
    mutate(
      # Text columns: trim spaces; if nothing is left (""), make it NA.
      across(all_of(text_columns), ~ na_if(str_trim(.x), "")),

      # fix casing so member and Member are the same etc.
      ride_id       = str_to_upper(ride_id),
      rideable_type = str_to_lower(rideable_type),
      member_casual = str_to_lower(member_casual),

      # Text -> date-time. using quiet so it turns NA without an error.
      started_at = ymd_hms(started_at, quiet = TRUE),
      ended_at   = ymd_hms(ended_at,   quiet = TRUE),

      # Text -> number for the four coordinate columns.
      across(all_of(coordinate_columns), ~ parse_number(.x, na = c("", "NA"))),

      # find the file it all came from (keeping track)
      source_file = basename(file)
    )
}

# the map() runs the function from earlier read_divvy() on each path 
# bind_rows() puts each table on top of eachother.
combined <- csv_files |>
  map(read_divvy) |>
  bind_rows()

if (nrow(combined) == 0) stop() # no rows in the csv so stop

cleaned <- combined |>

  # the trip length in minutes = (end - start)
  mutate(ride_duration_min = as.numeric(
    difftime(ended_at, started_at, units = "mins")
  )) |>

  # Drop any row missing one of the  fields.
  drop_na(ride_id, rideable_type, started_at, ended_at, member_casual) |>

  # Rmoves duplicate trips
  distinct(ride_id, .keep_all = TRUE) |>

  mutate( #put lat and long in acceptable ranges
    # abs(x) > 90 so impossible values become NA, if x is already NA the comparison is NA and the result stays NA.
    across(c(start_lat, end_lat), ~ if_else(abs(.x) > 90,  NA_real_, .x)),
    across(c(start_lng, end_lng), ~ if_else(abs(.x) > 180, NA_real_, .x)),

    ride_date  = as.Date(started_at),               # calendar date
    ride_month = format(started_at, "%Y-%m"),       # like "2025-10"
    start_hour = hour(started_at),                  # 0-23
    day_of_week = wday(started_at, label = TRUE, abbr = FALSE),  # "Monday"
    is_weekend  = wday(started_at, week_start = 1) >= 6,         # Mon = 1, so 6/7 = Sat/Sun

    # TRUE when the trip had a possible length
    valid_duration = ride_duration_min > 0,

    # TRUE for trips longer than 24 hours (1440 minutes), bike probably never docked.
    over_24_hours = ride_duration_min > 1440
  )

# Check if all four coords still here
has_coords <- complete.cases(
  cleaned[c("start_lng", "start_lat", "end_lng", "end_lat")]
)

# Setting it to default NA then change if it has the coords.
cleaned$straight_line_distance_km <- NA_real_

if (any(has_coords)) {
  # distHaversine() wants two 2-column matrices: (longitude, latitude).
  # It returns metres so change it to km.
  cleaned$straight_line_distance_km[has_coords] <- distHaversine(
    as.matrix(cleaned[has_coords, c("start_lng", "start_lat")]),
    as.matrix(cleaned[has_coords, c("end_lng",   "end_lat")])
  ) / 1000
}

output_file <- file.path(output_dir, "divvy_trips_cleaned.csv")
write_csv(cleaned, output_file)

cat("Cleaning complete.\n")
cat("Rows retained:", nrow(cleaned), "\n")
cat("Output file:", normalizePath(output_file), "\n")