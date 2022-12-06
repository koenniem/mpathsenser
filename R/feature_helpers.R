# The S3 generic
map_feature <- function(data, ...) {
  UseMethod("map_feature", data)
}

# Different maps to reduce overlap in the sensor-specific functions
map_feature_average<- function(data, ...) {
  data %>%
    mutate(data = map(data, ~.x[, setdiff(colnames(.x), "time")])) %>%
    unnest(data, keep_empty = TRUE) %>%
    group_by(participant_id, time)
}

map_feature_duration <- function(data, fun, categories, ...) {
  data$duration <- furrr::future_map(.x = data$data,
                                     .f = fun,
                                     .options = furrr::furrr_options(seed = TRUE))
  data <- unnest(data, "duration")
  data <- pivot_wider(data, names_from = categories, values_from = "duration")
  data
}

map_feature_count <- function(data, ...) {
  data %>%
    mutate(features = future_map2(.x = data,
                                  .y = gap,
                                  .f = fun,
                                  .options = furrr_options(seed = TRUE))) %>%
    unnest(features)
}


# Sensor specific map functions ===========

map_feature.accelerometer <- function(data, ...) {
  map_feature_average(data, ...) %>%
    select(-dplyr::any_of(c("gap", "original_time"))) %>%
    accelerometer_magnitude() %>%
    ungroup()
}

map_feature.activity <- function(data, ..., unknown_threshold = 0.50) {
  data <- map_feature_duration(data, ...)
  data %>%
    filter(type != "UNKNOWN") %>%
    rename(activity_active = ACTIVE,
           activity_still = STILL)
}

map_feature.appusage <- function(data, ...) {
  NA
}

map_feature.battery <- function(data, ...) {
  data <- map_feature_duration(data = data,
                               fun = battery_charging_time,
                               categories = "battery_status")
  data <- select(data, -"data")
  data
}

map_feature.bluetooth <- function(data, ...) {
  data %>%
    map_feature_count(bluetooth_n_beacons) %>%
    map_feature_count(bluetooth_unique_beacons) %>%
    select(-"data")
}

map_feature.calendar <- function(data, ...) {
  data %>%
    mutate(gap = 0) %>% # temporary solution
    map_feature_count(calendar_appointments) %>%
    select(-"data")
}

map_feature.connectivity <- function(data, ...) {
  NA
}

map_feature.gyroscope <- function(data,...) {
  map_feature_average(data, ...) %>%
    select(-dplyr::any_of(c("gap", "original_time"))) %>%
    gyroscope_magnitude() %>%
    ungroup()
}

map_feature.light <- function(data, ...) {
  map_feature_average(data, ...) %>%
    select(-dplyr::any_of(c("gap", "original_time"))) %>%
    light_intensity() %>%
    ungroup()
}

map_feature.location <- function(data, ...) {
  NA
}

map_feature.noise <- function(data, ...) {
  map_feature_average(data) %>%
    select(-dplyr::any_of(c("gap", "original_time"))) %>%
    noise_intensity() %>%
    ungroup()
}

map_feature.pedometer <- function(data, ...) {
  NA
}

map_feature.screen <- function(data, ...) {
  data %>%
    map_feature_count(screen_unlocks) %>%
    map_feature_count(screen_on) %>%
    map_feature_duration(data = data,
                         fun = screen_duration,
                         categories = "screen_event")
    # select(-data)
}

map_feature.weather <- function(data, ...) {
  # TODO: decide which features get selected

  map_feature_average(data, ...) %>%
    select(-dplyr::any_of(c("gap", "original_time"))) %>%
    select("participant_id", "time", "cloudiness", "humidity",
           "pressure", "rain_last_hour", "temperature") %>%
    weather_info() %>%
    ungroup()
}

map_feature.wifi <- function(data, ...) {
  NA
}
