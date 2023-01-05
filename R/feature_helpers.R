# The S3 generic

#' @export
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

map_feature_duration_old <- function(data,
                                 fun,
                                 categories,
                                 start = NULL,
                                 end = NULL,
                                 map_args = NULL,
                                 ...) {
  if (!is.null(start) && !is.null(end)) {
    start <- as.integer(data$time) - start
    end <- as.integer(data$time) + end
  }

  # Construct a list of arguments to map over
  # That is, the data, start time, end time, and any other arguments specified in `map_args`.
  map_list <- list(data = data$data, start = start, end = end)
  if (!missing(map_args)) {
    map_list <- append(map_list, rlang::eval_tidy(rlang::enexpr(map_args), data = data))
  }

  # Map over the function
  data$duration <- furrr::future_pmap(.l = map_list,
                                      .f = fun,
                                      ...,
                                      .options = furrr::furrr_options(seed = TRUE))

  data <- unnest(data, "duration", keep_empty = TRUE)
  data <- pivot_wider(data, names_from = any_of(categories), values_from = "duration")
  data
}

map_feature_duration <- function(data,
                                  fun,
                                  categories_col,
                                  start = NULL,
                                  end = NULL,
                                  map_args = NULL,
                                  ...) {

  # Ensure there is always a measurement at the start of the interval and a measurement at the end
  # of the interval
  if (!is.null(start) && !is.null(end)) {
    data <- data |>
      mutate(.start = time - start) |>
      mutate(.end = time + end) |>
      mutate(data = pmap(
        .l = list(data, .start, .end),
        .f = \(data, .start, .end) impute_bounds(
          data = data,
          start = .start,
          end = .end,
          fill = rlang::list2({{ categories_col }} := "GAP")))) |>
      select(-c(.start, .end))
  }

  # Create group ids so that we have less data to work with
  data <- data |>
    tibble::rowid_to_column(var = ".id")

  # Apply the function to each id
  res <- data |>
    select(.id, data) |>
    unnest(data, keep_empty = TRUE) |>
    group_by(.id) |>
    rlang::exec(.fn = fun,
                data = _,
                ...,
                !!!map_args) |>
    pivot_wider(names_from = any_of(categories_col), values_from = "duration") |>
    mutate(across(everything(), \(col) ifelse(is.na(col), 0, col)))

  # Merge back with original data
  data |>
    left_join(res, by = ".id") |>
    select(-.id)
}

map_feature_count <- function(data, fun, ...) {
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
    select(-any_of(c("gap", "original_time"))) %>%
    accelerometer_magnitude() %>%
    ungroup()
}

map_feature.activity <- function(data,
                                 ...,
                                 start = NULL,
                                 end = NULL,
                                 unknown_threshold = 50) {

  data <- map_feature_duration(data = data,
                               fun = activity_duration,
                               categories_col = "type",
                               start = start,
                               end = end)

  data %>%
    filter(UNKNOWN <= unknown_threshold) %>%
    rename(activity_active = ACTIVE,
           activity_still = STILL,
           activity_gap = GAP) %>%
    nest(activity_features = c(activity_active, activity_still, activity_gap)) |>
    select(-c("data", "UNKNOWN"))
}

map_feature.appusage <- function(data, ...) {
  NA
}

map_feature.battery <- function(data, ..., start = NULL, end = NULL) {
  map_feature_duration(data = data,
                       fun = battery_charging_time,
                       categories = "battery_status",
                       start = start,
                       end = end) %>%
    select(-"data")
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

map_feature.connectivity <- function(data, ..., start = NULL, end = NULL) {
  map_feature_duration(data = data,
                       fun = connectivity_duration,
                       categories = "connectivity_status",
                       start = start,
                       end = end) %>%
    select(-"data")
}

map_feature.gyroscope <- function(data,...) {
  map_feature_average(data, ...) %>%
    select(-any_of(c("gap", "original_time"))) %>%
    gyroscope_magnitude() %>%
    ungroup()
}

map_feature.light <- function(data, ...) {
  map_feature_average(data, ...) %>%
    select(-any_of(c("gap", "original_time"))) %>%
    light_intensity() %>%
    ungroup()
}

map_feature.location <- function(data, ...) {
  NA
}

map_feature.noise <- function(data, ...) {
  map_feature_average(data) %>%
    select(-any_of(c("gap", "original_time"))) %>%
    noise_intensity() %>%
    ungroup()
}

map_feature.pedometer <- function(data, ...) {
  NA
}

#' @export
#' @keywords internal
map_feature.screen <- function(data, ..., start = NULL, end = NULL) {
  data %>%
    map_feature_count(screen_unlocks) %>%
    map_feature_count(screen_on) %>%
    map_feature_duration(data = .,
                         fun = screen_duration,
                         categories = "screen_event",
                         start = start,
                         end = end) %>%
    select(-data)
}

map_feature.weather <- function(data, ...) {
  # TODO: decide which features get selected

  map_feature_average(data, ...) %>%
    select(-any_of(c("gap", "original_time"))) %>%
    select("participant_id", "time", "cloudiness", "humidity",
           "pressure", "rain_last_hour", "temperature") %>%
    weather_info() %>%
    ungroup()
}

map_feature.wifi <- function(data, ..., start = NULL, end = NULL) {
  data %>%
    map_feature_duration(data = .,
                         fun = wifi_time_recurring,
                         categories = "wifi_recurring",
                         start = start,
                         end = end,
                         map_args = list(ssids = recurring_ssids)) %>%
    map_feature_duration(data = .,
                         fun = wifi_time_home,
                         categories = "wifi_home",
                         start = start,
                         end = end,
                         map_args = list(ssid = home_ssid)) %>%
    select(-c("data", "home_ssid", "recurring_ssids"))
}

impute_bounds <- function(data, start, end, fill = NULL) {
  # If the `start` is not equal to the start of the data, add a "GAP"
  start_dat <- NULL
  end_dat <- NULL
  if (abs(difftime(data$time[1], start)) >= 1e-3) {
    start_dat <- tibble(
      time = start,
      !!!fill
    )
  }

  # If the `end` is not equal to the end of the data add a "GAP"
  if (abs(difftime(tail(data$time, n = 1), end)) >= 1e-3) {
    end_dat <- tibble(
      time = end,
      !!!fill
    )
  }

  # Assign the result
  if (!is.null(start_dat) || !is.null(end_dat)) {
    data <- bind_rows(start_dat, data, end_dat)
  }

  data
}
