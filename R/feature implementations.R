# Feature implementation ===========
# Feature implementations and helper functions

## General sensor functions ================
feature_duration <- function(data, by = NULL, weight = NULL, categories = NULL) {
  # check_arg(data, "data.frame")
  # check_arg(by, "character", n = 1, allow_null = TRUE)

  if (nrow(data) == 0 || all(data[, by] == "GAP")) {
    return(tibble(
      !!ensym(by) := categories,
      duration = 0
    ))
  }

  # Convert time to integer for efficiency
  data <- data %>%
    mutate(time = as.integer(time))

    start <- min(data$time, na.rm = TRUE)
    end <- max(data$time, na.rm = TRUE)

  # Take the time of the next measurement. If there are multiple measurements at the same time, take
  # for both measurements the time after their own measurement
  data <- data %>%
    nest(data = !c("time")) %>%
    complete(time = seq.int(from = start,
                            to = end,
                            by = 1L)) %>%
    fill(data) %>%
    mutate(next_time = lead(time)) %>%
    unnest(data)

  # Create bins to calculate the duration by
  data <- data %>%
    mutate(bin = findInterval(x = time,
                              vec = seq.int(from = start,
                                            to = end,
                                            length.out = 31L),
                              left.open = TRUE, rightmost.closed = TRUE))

  # Calculate the duration
  # Apply reweighing, if needed
  if (is.null(weight)) {
    data <- mutate(data, duration = next_time - time)
  } else {
    weight <- rlang::ensym(weight)
    data <- mutate(data, duration = (next_time - time) * (weight / 100))
  }

  # Calculate the duration
  total_duration <- sum(data$duration, na.rm = TRUE)
  data %>%
    filter(if_any(by, ~.x != "GAP")) %>%
    group_by(bin) %>%
    mutate(duration = duration / total_duration * 100) %>%
    group_by(across(by)) %>%
    summarise(duration = sum(duration, na.rm = TRUE), .groups = "drop")
    # mutate(duration = duration / sum(duration) * 100)
}

feature_average <- function(data, ..., na.rm = TRUE) {
  if (rlang::dots_n(...) != 0) {
    if (dplyr::is_grouped_df(data)) {
      data <- select(data, dplyr::group_cols(), ...)
    } else {
      data <- select(data, ...)
    }
  }

  data <- summarise(data, across(.fns = ~mean(.x, na.rm = na.rm)), .groups = "keep")
  data <- mutate(data, across(.fns = ~ifelse(is.nan(.x), NA, .x)))

  data
}

feature_count <- function(data, gap = 0, expr = NULL) {
  if (any("original_time" == colnames(data))) {
    data <- data[is.na(data$original_time), ]
  }

  if (!missing(expr)) {
    expr <- rlang::enexpr(expr)
    data <- filter(!!expr)
  }

  if (gap > 1) {
    abort(c("Argument `gap` must be between 0 and 1.",
            x = paste0("You supplied a `gap` value of ", gap, ".")))
  }

  data %>%
    summarise(n = n() / (1 - gap), .groups = "drop_last") %>%
    mutate(n = ifelse(is.infinite(n), 0, n))
}



## Sensor-specific functions ============
accelerometer_magnitude <- function(data, ...) {
  feature_average(data, ...)
}

activity_duration <- function(data) {
  # If there are multiple activities at the same moment, average the confidence values
  # This also prevents from the activities being counted more than once
  # Note: this drops original_time
  data <- data %>%
    group_by(time, type) %>%
    summarise(confidence = mean(confidence), .groups = "drop")

  # Remove original_time, if present
  data <- data[, setdiff(colnames(data), "original_time")]

  # Calculate duration
  feature_duration(data, by = type, weight = "confidence")
}

app_usage <- function(data) {

}

battery_charging_time <- function(data) {
  # Select relevant column
  data <- data[, c("time", "battery_status")]
  res <- feature_duration(data = data,
                          by = "battery_status",
                          categories = c("charging", "discharging"))
}

bluetooth_n_beacons <- function(data, gap = 0) {
  data %>%
    feature_count(gap) %>%
    dplyr::rename(beacons_n = n)
}

bluetooth_unique_beacons <- function(data, gap = 0) {
  data %>%
    distinct(bluetooth_device_id, .keep_all = TRUE) %>%
    feature_count(gap) %>%
    dplyr::rename(beacons_unique = n)
}

calendar_appointments <- function(data, gap = 0) {
  data %>%
    distinct(event_id) %>%
    feature_count(gap) %>%
    dplyr::rename(appointments = n)
}

calendar_meeting_time <- function(data) {

}

calendar_attendees <- function(data) {
  feature_count(data)
}

connectivity_duration <- function(data) {

}

location_circadian <- function(data) {

}

location_distance <- function(data) {

}

location_unique_places <- function(data) {

}

location_variance <- function(data) {

}

gyroscope_magnitude <- function(data, ...) {
  feature_average(data, ...)
}

light_intensity <- function(data, ...) {
  feature_average(data, ...)
}

noise_intensity <- function(data, ...) {
  feature_average(data, ...)
}

screen_duration <- function(data) {
  data <- data[, c("time", "screen_event")]
  res <- feature_duration(data = data,
                          by = "screen_event",
                          categories = c("SCREEN_OFF", "SCREEN_ON", "SCREEN_UNLOCKED"))
}

screen_on <- function(data, gap = 0) {
  data %>%
    filter(screen_event == "SCREEN_ON") %>%
    feature_count(gap) %>%
    dplyr::rename(screen_on = n)
}

screen_unlocks <- function(data, gap = 0) {
  data %>%
    filter(screen_event == "SCREEN_UNLOCKED") %>%
    feature_count(gap) %>%
    dplyr::rename(screen_unlocks = n)
}

weather_info <- function(data, ...) {
  feature_average(data, ...)
}

wifi_duration <- function(data) {

}

wifi_duration_recurring <- function(data) {

}

wifi_home <- function(data) {

}


# Old ========================
## Accelerometer ==========
# Input: Accelerometer data from a single participant
accelerometer_fun <- function(data) {
  out <- data.frame(
    acc_x = NA,
    acc_y = NA,
    acc_z = NA,
    acc_l1_norm = NA,
    acc_l2_norm = NA
  )

  if (nrow(data) == 0) {
    return(out)
  }

  out$acc_x = mean(data$x, na.rm = TRUE)
  out$acc_y = mean(data$y, na.rm = TRUE)
  out$acc_z = mean(data$z, na.rm = TRUE)
  out$acc_l1_norm = mean(data$l1_norm, na.rm = TRUE)
  out$acc_l2_norm = mean(data$l2_norm, na.rm = TRUE)
  return(out)
}

# Input: A tibble of length ESM data, containing the columns participant_id, time, gap (for the
# beep), and data, which is a list column of nested tibbles containing the accelerometer values.
map_accelerometer_avgs <- function(data) {
  data %>%
    mutate(features = future_map(data, accelerometer_fun, .options = furrr_options(seed = TRUE))) %>%
    unnest(features) %>%
    select(-data)
}

## Activity ==========

# Function for calculating duration per activity type
activity_fun <- function(data, start = NULL, end = NULL) {
  if (nrow(data) == 0) {
    return(tibble(type = c("STILL", "ACTIVE", "UNKNOWN"),
                  duration = 0))
  }

  # Convert time to integer for efficiency
  data <- data %>%
    mutate(time = as.integer(time))

  if (is.null(start)) {
    start <- min(data$time, na.rm = TRUE)
  } else {
    start <- as.integer(start)
  }
  if (is.null(end)) {
    end <- max(data$time, na.rm = TRUE)
  } else {
    end <- as.integer(end)
  }

  # If there are multiple activities at the same moment, average the confidence values
  # This also prevents from the activities being counted more than once
  # Note: this drops original_time
  data <- data %>%
    group_by(time, type) %>%
    summarise(confidence = mean(confidence), .groups = "drop")

  # Take the time of the next measurement. If there are multiple measurements at the same time, take
  # for both measurements the time after their own measurement
  data <- data %>%
    nest(data = c(confidence, type)) %>%
    complete(time = seq.int(from = start,
                            to = end,
                            by = 1L)) %>%
    fill(data) %>%
    mutate(next_time = lead(time)) %>%
    unnest(data)

  # Create bins to calculate the duration by
  data <- data %>%
    mutate(bin = findInterval(x = time,
                              vec = seq.int(from = start,
                                            to = end,
                                            length.out = 31L),
                              left.open = TRUE, rightmost.closed = TRUE))

  # Finally, calculate and sum the duration
  data %>%
    mutate(duration = (next_time - time) * (confidence / 100)) %>%
    filter(type != "GAP") %>%
    group_by(bin) %>%
    mutate(duration = duration / sum(.data$duration, na.rm = TRUE) * 100) %>%
    group_by(type) %>%
    summarise(duration = sum(duration, na.rm = TRUE), .groups = "drop") %>%
    mutate(duration  = duration / sum(duration) * 100)
}

# helper function
map_activity_over_unknown <- function(.x, .y) {
  if (.y) {
    .x <- .x %>%
      mutate(activity_unknown = future_map(data, activity_fun, .options = furrr_options(seed = TRUE))) %>%
      mutate(activity_unknown = vapply(activity_unknown, function(x) {
        x <- x[x$type == "UNKNOWN", "duration"][[1]]
        if (length(x) > 0) x else 0
      }, FUN.VALUE = double(1)))
  } else {
    .x$activity_unknown <- 0
  }

  .x
}

# helper function
map_activity_duration <- function(.x) {
  .x %>%
    mutate(duration = future_map(data, activity_fun, .options = furrr_options(seed = TRUE))) %>%
    unnest(duration) %>%
    filter(type != "UNKNOWN") %>%
    pivot_wider(names_from = "type", values_from = "duration") %>%
    rename(activity_active = ACTIVE,
           activity_still = STILL) %>%
    select(-data)
}

## Battery ===================

# Input: weather data from a single participant
battery_fun <- function(data, start = NULL, end = NULL) {
  if (nrow(data) == 0) {
    return(tibble(type = c("charging", "discharging"),
                  duration = 0))
  }

  # Convert time to integer for efficiency
  data <- data %>%
    mutate(time = as.integer(time))

  if (is.null(start)) {
    start <- min(data$time, na.rm = TRUE)
  } else {
    start <- as.integer(start)
  }
  if (is.null(end)) {
    end <- max(data$time, na.rm = TRUE)
  } else {
    end <- as.integer(end)
  }

  # Take the time of the next measurement. If there are multiple measurements at the same time, take
  # for both measurements the time after their own measurement
  data <- data %>%
    nest(data = c(battery_status)) %>%
    complete(time = seq.int(from = start,
                            to = end,
                            by = 1L)) %>%
    fill(data) %>%
    mutate(next_time = lead(time)) %>%
    unnest(data)

  # Create bins to calculate the duration by
  data <- data %>%
    mutate(bin = findInterval(x = time,
                              vec = seq.int(from = start,
                                            to = end,
                                            length.out = 31L),
                              left.open = TRUE, rightmost.closed = TRUE))

  # Finally, calculate and sum the duration
  data %>%
    mutate(duration = (next_time - time)) %>%
    filter(battery_status != "GAP") %>%
    group_by(bin) %>%
    mutate(duration = duration / sum(.data$duration, na.rm = TRUE) * 100) %>%
    ungroup() %>%
    group_by(battery_status) %>%
    summarise(duration = sum(duration, na.rm = TRUE), .groups = "drop") %>%
    mutate(duration  = duration / sum(duration) * 100)
}

# Input: A tibble of length ESM data, containing the columns participant_id, time, gap (for the
# beep), and data, which is a list column of nested tibbles containing the light values.
map_charging_time <- function(data) {
  data %>%
    mutate(duration = future_map(data, battery_fun, .options = furrr_options(seed = TRUE))) %>%
    unnest(duration) %>%
    pivot_wider(names_from = "battery_status", values_from = "duration") %>%
    select(participant_id, time, gap, battery_charging_time = charging)
}

## Pedometer =============
step_fun <- function(data) {
  data %>%
    arrange(time) %>%
    mutate(next_count = lead(step_count, default = NA)) %>%
    mutate(step_count = ifelse(step_count > next_count, NA, step_count)) %>%
    mutate(steps = next_count - step_count) %>%
    summarise(steps = sum(steps, na.rm = TRUE), .groups = "drop") %>%
    pull(steps)
}

## Screen ===========

screen_fun_duration <- function(data, start = NULL, end = NULL) {
  if (nrow(data) == 0) {
    return(tibble(screen_event = c("SCREEN_OFF", "SCREEN_ON", "SCREEN_UNLOCKED"),
                  duration = 0))
  }

  # Convert time to integer for efficiency
  data <- data %>%
    mutate(time = as.integer(time))

  if (is.null(start)) {
    start <- min(data$time, na.rm = TRUE)
  } else {
    start <- as.integer(start)
  }
  if (is.null(end)) {
    end <- max(data$time, na.rm = TRUE)
  } else {
    end <- as.integer(end)
  }

  # Take the time of the next measurement. If there are multiple measurements at the same time, take
  # for both measurements the time after their own measurement
  data <- data %>%
    nest(data = screen_event) %>%
    complete(time = seq.int(from = start,
                            to = end,
                            by = 1L)) %>%
    fill(data) %>%
    mutate(next_time = lead(time)) %>%
    unnest(data)

  # Create bins to calculate the duration by
  data <- data %>%
    mutate(bin = findInterval(x = time,
                              vec = seq.int(from = start,
                                            to = end,
                                            length.out = 31L),
                              left.open = TRUE, rightmost.closed = TRUE))

  # Finally, calculate and sum the duration
  data %>%
    # mutate(screen_event = if_else(screen_event == "SCREEN_ON", "SCREEN_OFF", screen_event)) %>%
    mutate(duration = next_time - time) %>%
    filter(screen_event != "GAP") %>%
    group_by(bin) %>%
    mutate(duration = duration / sum(.data$duration, na.rm = TRUE) * 100) %>%
    group_by(screen_event) %>%
    summarise(duration = sum(duration, na.rm = TRUE), .groups = "drop")  %>%
    mutate(duration = duration / sum(duration) * 100)
}

# helper function
map_screen_unlocks <- function(.x) {
  .x %>%
    mutate(n_screen_unlocks = future_map_int(.x = data,
                                             .y = gap,
                                             .f = screen_fun_unlocks,
                                             .options = furrr_options(seed = TRUE))) %>%
    select(-data)
}

# helper function
map_screen_duration <- function(.x) {
  .x %>%
    mutate(duration = future_map(data, screen_fun_duration,
                                 .options = furrr_options(seed = TRUE))) %>%
    unnest(duration) %>%
    pivot_wider(names_from = "screen_event", values_from = "duration") %>%
    rename(screen_unlocked = SCREEN_UNLOCKED,
           screen_on = SCREEN_ON,
           screen_off = SCREEN_OFF) %>%
    select(-data)
}


