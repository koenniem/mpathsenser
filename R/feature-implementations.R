# Feature implementation ===========
# Feature implementations and helper functions

## General sensor functions ================
feature_duration <- function(data,
                              by = NULL,
                              gap_value = "GAP",
                              weight_by = NULL,
                              categories = NULL) {

  # If `data` is empty, contains only NA's, or only GAPs
  force(data)
  if (nrow(data) == 0 || all(is.na(data[[by]])) || all(data[[by]] == gap_value, na.rm = TRUE)) {
    out <- tibble(
      {{ by }} := categories,
      duration = 0
    )
    return(out)
  }

  # Remove subsequent measurements of the same type to further compress the data
  # data <- data |>
  #   filter(!({{ by }} == lead({{ by }}, default = "foo") &
  #              {{ by }} == lag({{ by }}, default = "foo")))
  data <- purge_redundant(data, {{ by }})

  # Convert time to integer for efficiency
  data <- mutate(data, time = as.integer(time))

  # If there are multiple measurements at the same time, retain them only as long as there is not a
  # gap
  data <- data |>
    group_by(time, .add = TRUE) |>
    mutate(.n = n()) |> # The number of measurements per timestamp
    mutate(.any_gap = any({{ by }} == gap_value)) |> # Whether there is a gap present
    filter(.n == 1 | # Only one measurement
             (.n > 1 & .any_gap & {{ by }} == gap_value) | # A gap, drop all non-gaps
             (.n > 1 & !.any_gap)) |>  # No gap present, retain all
    select(-c(.n, .any_gap)) |>
    ungroup(time)

  # Calculate the next time value
  # Because there can be multiple measurements with the same timestamps, calculate the next time
  # value based on the unique timestamp (by group)
  unique_time <- data |>
    distinct(time) |>
    mutate(next_time = lead(time))

  # Join with original data
  data <- left_join(data, unique_time, by = intersect(names(data), names(unique_time)))

  # Calculate duration
  data <- mutate(data, duration = next_time - time)

  # Reweigh if needed
  if (!missing(weight_by) && !is.null(weight_by)) {
    weight_by <- rlang::ensym(weight_by)
    data <- data |>
      mutate(!!weight_by := ifelse(is.na({{ weight_by }}), 100, {{ weight_by }})) |>
      group_by(time, .add = TRUE) |>
      mutate(duration = duration * ({{ weight_by }} / sum({{ weight_by }}, na.rm = TRUE))) |>
      ungroup(time)
  } else {
    data <- data |>
      group_by(time, .add = TRUE) |>
      mutate(duration = duration / n()) |>
      ungroup(time)
  }

  data <- data |>
    group_by(across({{ by }}), .add = TRUE) |>
    summarise(duration = sum(duration, na.rm = TRUE), .groups = "drop_last") |>
    mutate(duration = duration / sum(duration) * 100)

  # Remove GAP from by
  # Drops groups that only contains gaps
  # data <- data |>
  #   filter(if_any({{ by }}, ~.x != gap_value))

  # Fill in missing categories
  # Removed for now as it is computationally very intense
  # data <- data |>
  #   complete({{ by }} := categories, fill = list(duration = 0))

  data
}

feature_duration2 <- function(data,
                             by = NULL,
                             start = NULL,
                             end = NULL,
                             weight_by = NULL,
                             categories = NULL) {

  # check_arg(data, "data.frame")
  # check_arg(by, "character", n = 1, allow_null = TRUE)
  # check_arg(start, c("integerish", "POSIXt"), n = 1, allow_null = TRUE)
  # check_arg(end, c("integerish", "POSIXt"), n = 1, allow_null = TRUE)
  # check_arg(categories, "character", allow_null = TRUE)

  if (nrow(data) == 0 || all(is.na(data[[by]])) || all(data[[by]] == "GAP", na.rm = TRUE)) {
    out <- tibble(
      foo = categories,
      duration = 0
    )
    colnames(out) <- c(by, "duration")
    return(out)
  }

  # Convert time to integer for efficiency
  data$time <- as.integer(data$time)

  # If there is a gap starting at the same time as concurrent measurements, drop the measurements
  # while keeping the gap
  # In other words, keep all measurement that are not gaps and also not duplicated AND all gaps
  # Find which rows are duplicates
  dups_ind <- duplicated(data$time) | duplicated(data$time, fromLast = TRUE) # Find duplicates
  dups <- data[which(dups_ind), ] # Duplicated rows i nthe data
  data <- data[which(!dups_ind), ] # Data without duplicated rows

  # split and loop over the duplicated groups. If there is no gap, return everything.
  # If there is a gap, return only the gap
  dups2 <- lapply(split(dups, dups$time), function(x) {
    gap_ind <- x[[by]]== "GAP"
    if (any(gap_ind, na.rm = TRUE)) {
      x <- x[gap_ind, ]
    }
    return(x)
  })

  # Bind duplicated groups together
  data <- dups2 %>%
    bind_rows() %>%
    bind_rows(data) %>%
    .[order(.$time), ]

  # Format start and end, if provided
  if (!is.null(start)) {
    start <- as.integer(start)

    # If the `start` is not equal to the start of the data
    # add a "GAP"
    if (isTRUE(all.equal(data$time[1], start, check.tzone = FALSE))) {
      start_dat <- data.frame(
        time = start,
        foo = "GAP"
      )
      colnames(start_dat) <- c("time", by)
      data <- bind_rows(start_dat, data)
    }
  }

  if (!is.null(end)) {
    end <- as.integer(end)

    # If the `end` is not equal to the end of the data
    # add a "GAP"
    if (isTRUE(all.equal(tail(data$time, n = 1), end, check.tzone = FALSE))) {
      end_dat <- data.frame(
        time = end,
        foo = "GAP"
      )
      colnames(end_dat) <- c("time", by)
      data <- bind_rows(data, end_dat)
    }
  }

  # Use the `unique()` function from the `dplyr` package to remove duplicate timestamps
  unique_time <- unique(data$time)
  next_time <- lead(unique_time)

  # Use the `merge()` function to join the original data frame with the computed durations
  data <- merge(data, data.frame(time = unique_time,
                                 next_time = next_time),
                by = c("time"))

  # Calculate the duration
  data$duration <- data$next_time - data$time

  # Apply reweighing, if needed
  if (missing(weight_by)) {
    # Calculate the duration per unique time stamp
    data$duration <- data$duration %>%
      split(data$time) %>%
      lapply(FUN = function(x) x / length(x)) %>%
      unsplit(data$time)
  } else {
    weight_by <- as.character(substitute(weight_by, env = environment()))
    # Impute weight_by with 1. Missing value will lead to missing duration as well
    data[is.na(data[[weight_by]]), weight_by] <- 1
    data <- data %>%
      split(.$time) %>%
      lapply(function(x) {
        x$duration <- x$duration * (x[[weight_by]] / sum(x[[weight_by]]))
        x
      }) %>%
      unsplit(data$time)
  }

  # Sum by group
  groups <- data %>%
    split(.[[by]]) %>%
    vapply(FUN = function(x) sum(x$duration, na.rm = TRUE),
           FUN.VALUE = double(1))

  # Create a new data frame for the result
  res <- data.frame(
    foo = names(groups),
    duration = unname(groups)
  )
  colnames(res) <- c(by, "duration")

  # Normalise to 100
  res$duration <- res$duration / sum(res$duration) * 100

  # Remove "GAP", if any
  res <- res[res[, 1] != "GAP", ]

  # Fill in missing categories
  ref <- data.frame(
    foo = categories
  )
  colnames(ref) <- c(by)
  res <- merge(res, ref, all = TRUE)
  res$duration[is.na(res$duration)] <- 0
  tibble::as_tibble(res)
}

feature_average <- function(data, ..., na.rm = TRUE) {
  if (rlang::dots_n(...) != 0) {
    if (dplyr::is_grouped_df(data)) {
      data <- select(data, dplyr::group_cols(), ...)
    } else {
      data <- select(data, ...)
    }
  }

  data <- data |>
    summarise(across(.fns = ~mean(.x, na.rm = na.rm)), .groups = "keep") |>
    mutate(across(.fns = ~ifelse(is.nan(.x), NA, .x)))

  data
}

feature_count <- function(data, gap = 0, expr = NULL) {
  if (any("original_time" == colnames(data))) {
    data <- data[is.na(data$original_time), ]
  }

  if (!missing(expr)) {
    expr <- rlang::enexpr(expr)
    data <- filter(data, !!expr)
  }

  if (gap > 1 || gap < 0) {
    abort(c("Argument `gap` must be between 0 and 1.",
            x = paste0("You supplied a `gap` value of ", gap, ".")))
  }

  data |>
    summarise(n = n() / (1 - gap), .groups = "drop_last") %>%
    mutate(n = ifelse(is.infinite(n), 0, n))
}



## Sensor-specific functions ============
accelerometer_magnitude <- function(data, ...) {
  feature_average(data, ...)
}


#' Activity duration feature
#'
#' @param data The activity data
#' @param weight_by A single column name in `data` to weigh the observations by.
#'
#' @return
#' @export
#' @family activity
activity_duration <- function(data,
                              weight_by = NULL,
                              gap_value = "GAP",
                              categories = c("ACTIVE", "STILL", "UNKNOWN")) {
  # check_arg(weight_by, "character", n = 1, allow_null = TRUE)
  # check_arg(gap_value, "character", n = 1)
  # check_arg(categories, "character")

  # If there are multiple activities at the same moment, average the confidence values
  # This also prevents from the activities being counted more than once
  # Note: this drops original_time
  if (!is.null(weight_by)) {
    data <- data %>%
      group_by(time, type, .add = TRUE) %>%
      summarise(across(all_of(weight_by), \(weights) mean(weights, na.rm = TRUE)),
                .groups = "keep") |>
      ungroup(time, type)
  }

  # Select relevant columns
  data <- data |>
    select(dplyr::group_cols(), time, type, any_of(weight_by))

  res <- feature_duration(data = data,
                          by = "type",
                          gap_value = gap_value,
                          weight_by = weight_by,
                          categories = categories)
  res
}

app_usage <- function(data) {

}

battery_charging_time <- function(data,
                                  gap_value = "GAP",
                                  categories = c("charging", "discharging")) {
  # Select relevant columns
  data <- data |>
    select(dplyr::group_cols(), time, battery_status)

  res <- feature_duration(data = data,
                          by = "battery_status",
                          gap_value = gap_value,
                          categories = categories)
  res
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

calendar_meeting_time <- function(data, start = NULL, end = NULL) {
  # data <- data[, c("time", "battery_status")]
  # res <- feature_duration(data = data,
  #                         by = "battery_status",
  #                         start = start,
  #                         end = end,
  #                         categories = c("charging", "discharging"))
}

calendar_attendees <- function(data) {
  feature_count(data)
}

connectivity_duration <- function(data,
                                  gap_value = "GAP",
                                  categories = c("mobile", "none", "wifi")) {
  # Select relevant columns
  data <- data |>
    select(dplyr::group_cols(), time, connectivity_status)

  res <- feature_duration(data = data,
                          by = "connectivity_status",
                          gap_value = gap_value,
                          categories = categories)
  res
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

screen_duration <- function(data,
                            gap_value = "GAP",
                            categories = c("SCREEN_OFF", "SCREEN_ON", "SCREEN_UNLOCKED")) {
  # Select relevant columns
  data <- data |>
    select(dplyr::group_cols(), time, screen_event)

  res <- feature_duration(data = data,
                          by = "screen_event",
                          gap_value = gap_value,
                          categories = categories)
  res
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

wifi_duration <- function(data, start = NULL, end = NULL) {
  # Impossible to measure, since there is no such thing as "no wifi" status.
  # data <- data[, c("time", "ssid")]
  # res <- feature_duration(data = data,
  #                         by = "screen_event",
  #                         start = start,
  #                         end = end,
  #                         categories = c("SCREEN_OFF", "SCREEN_ON", "SCREEN_UNLOCKED"))
  # res
}

wifi_time_recurring <- function(data,
                                ssids,
                                gap_value = "GAP",
                                categories = c("recurring", "not_recurring")) {
  # Select relevant columns
  data <- data |>
    select(dplyr::group_cols(), time, ssid)
  browser()

  data <- data[data$ssid %in% ssids, ]
  names(data)[names(data) == "ssid"] <- "wifi_recurring"
  data$wifi_recurring <- ifelse(data$wifi_recurring %in% ssids, "recurring", "not_recurring")

  res <- feature_duration(data = data,
                          by = "wifi_recurring",
                          gap_value = gap_value,
                          categories = categories)
  res
}

wifi_time_home <- function(data,
                           ssid,
                           gap_value = "GAP",
                           categories = c("wifi_home", "wifi_not_home")) {
  # Select relevant columns
  data <- data |>
    select(dplyr::group_cols(), time, ssid) |>
    dplyr::rename(wifi_home = ssid) |>
    mutate(wifi_home = ifelse(wifi_home == ssid, "wifi_home", "wifi_not_home"))

  # names(data)[names(data) == "ssid"] <- "wifi_home"
  # data$wifi_home <- ifelse(data$wifi_home == ssid, "wifi_home", "wifi_not_home")

  res <- feature_duration(data = data,
                          by = "wifi_home",
                          gap_value = gap_value,
                          categories = categories)
  res
}
