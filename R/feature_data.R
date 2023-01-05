# Feature data generic and methods

# * Accelerometer: The default method with a moving average of 1 minute and binned down to 1 minute.
# * Activity: Similar to the default function, but "TILTING" is removed early, and every activity type except UNKNOWN and STILL are set to  ACTIVE
# * AppUsage
# * Battery: The default method, then it replaces "full" with "charging" as a battery status (if present) since "full" implies "charging".
# * Bluetooth
# * Calendar
# * Connectivity
# * Gyroscope: The default method with a moving average of 1 minute and binned down to 1 minute.
# * Light: The default method
# * Location
# * Noise: First the default method, then removing duplicate entries.
# * Pedometer: The default method
# * Screen: The default method
# * Weather: The default method
# * Wifi
#
# All other sensors use the default `feature_data()` method.


#' Extract cleaned data from an mpathsenser database
#'
#' @param db A database connection to an m-Path Sense database.
#' @param sensor The name of a sensor. See \link[mpathsenser]{sensors} for a list of available
#'   sensors.
#' @param cols One or multiple column names to extract for this sensor. These names must correspond
#'   to the ones in the database. If left empty, all variables will be extracted.
#' @param bin Whether to bin the data. Only 1-minute bins are possible.
#' @param moving_average The number of seconds to calculate a moving average over. Defaults to no
#'   moving average.
#' @param min_confidence Activity data only. The minimum confidence value needed to keep the measurement. See \href{https://developers.google.com/android/reference/com/google/android/gms/location/DetectedActivity#getConfidence()}{Android's documentation} for a more precise description of what the confidence value entails.
#' @param key A decryption key string. Only applicable when extraction location data.
#' @param tz The time zone used to convert the time column to. In the database this defaults to UTC
#'   but this function uses the user's current time zone as this is likely the same time zone as the
#'   data was collected in.
#'
#' @returns A [tibble] containing the columns `participant_id`, `time`, and other specified
#'   columns.
#' @export
feature_data <- function(db,
                         sensor,
                         cols = NULL,
                         ...,
                         tz = Sys.timezone()) {
  check_db(db)
  check_arg(sensor, "character", n = 1)
  check_arg(cols, "character", allow_null = TRUE)
  check_arg(tz, "character", n = 1)

  UseMethod("feature_data", structure(list(), class = tolower(sensor)))
}



#' @param call Internal use only. A call to execute before [dplyr::collect] is called. Note that
#'   these columns must carry the symbol `data` as their target data frame.
#'
#' @inherit feature_data
#' @export
#' @keywords internal
feature_data.default <- function(db,
                                 sensor,
                                 cols = NULL,
                                 ...,
                                 bin = FALSE,
                                 moving_average = NULL,
                                 tz = Sys.timezone(),
                                 call = NULL) {

  if (is.numeric(moving_average) && moving_average > 0 &&
      !(tolower(sensor) %in% c("accelerometer", "gyroscope", "light", "noise"))) {
    warn(c("`moving_average` is only supported for accelerometer, gyroscope, light, or noise data.",
           i = "Setting `moving_average` to NULL."))
    moving_average <- NULL
  }
  if (is.numeric(moving_average) && moving_average > 0) {
    data <- moving_average(db = db,
                           sensor = sensor,
                           cols = cols,
                           n = moving_average) %>%
      dplyr::rename(time = .data$datetime)
  } else {
    data <- get_data(db = db, sensor = sensor) %>%
      mutate(time = paste(.data$date, .data$time)) %>%
      select(-c("measurement_id", "date"))
  }

  if (!is.null(cols)) {
    data <- select(data, any_of(c("participant_id", "time", cols)))
  }

  if (bin) {
    data <- data %>%
      mutate(time = paste0(strftime('%Y-%m-%dT%H:%M', .data$time), ":00")) %>%
      group_by(.data$participant_id, .data$time) %>%
      summarise(dplyr::across(.fns = mean, na.rm = TRUE), .groups = "drop")
  }

  if (!is.null(call)) {
    for (i in seq_along(call)) {
      data <- rlang::eval_tidy(call[[i]], data = data)
    }
  }

  # Drop rows where all values except participant_id and time are NA
  data <- data %>%
    filter(!dplyr::if_all(.cols = !c("participant_id", "time"), .fns = is.na))

  data <- data %>%
    distinct() %>%
    collect() %>%
    mutate(time = lubridate::ymd_hms(.data$time)) %>%
    mutate(across(where(lubridate::is.POSIXt), ~lubridate::with_tz(.x, tzone = tz))) %>%
    arrange(.data$participant_id, .data$time)

  # class(data) <- c(sensor, class(data))
  data
}

#' @rdname feature_data.default
#' @export
#' @keywords internal
feature_data.accelerometer <- function(db,
                                       sensor = "Accelerometer",
                                       cols = NULL,
                                       ...,
                                       bin = TRUE,
                                       moving_average = NULL,
                                       tz = Sys.timezone()) {

  check_arg(bin, "logical", n = 1)
  check_arg(moving_average, "integerish", n = 1, allow_null = TRUE)

  if (is.null(cols)) {
    cols <- c("x", "y", "z", "l1_norm", "l2_norm")
  }

  data <- feature_data.default(db = db,
                               sensor = "Accelerometer",
                               cols = cols,
                               bin = bin,
                               moving_average = moving_average,
                               tz = tz)

  data
}

#' @rdname feature_data.default
#' @export
feature_data.activity <- function(db,
                                  sensor = "Activity",
                                  cols = NULL,
                                  ...,
                                  min_confidence = 50,
                                  tz = Sys.timezone()) {

  # # Leave out TILTING since it says nothing about the actual activities
  # # Set IN_VEHICLE, ON_BICYCLE, ON_FOOT, RUNNING, and WALKING to ACTIVE
  call <- list(
    rlang::call2(.fn = rlang::expr(filter),
                 .data = rlang::expr(data),
                 rlang::expr(type != "TILTING")),
    rlang::call2(.fn = rlang::expr(mutate),
                 .data = rlang::expr(data),
                 type = rlang::expr(if_else(type %in% c("UNKNOWN", "STILL"), type, "ACTIVE"))),
    rlang::call2(.fn = rlang::expr(filter),
                 .data = rlang::expr(data),
                 rlang::expr(confidence >= !!min_confidence))
  )

  data <- feature_data.default(
    db = db,
    sensor = "Activity",
    cols = cols,
    tz = tz,
    call = call
  )

  data
}

#' @rdname feature_data.default
#' @export
#' @keywords internal
feature_data.battery <- function(db,
                                 sensor = "Battery",
                                 cols = NULL,
                                 ...,
                                 tz = Sys.timezone()) {

  data <- feature_data.default(db = db,
                               sensor = "Battery",
                               cols = cols,
                               tz = tz)

  if ("colnames" %in% colnames(data)) {
    data <- mutate(data, battery_status = if_else(battery_status == "full",
                                                  "charging",
                                                  battery_status))
  }

  data
}

#' @rdname feature_data.default
#' @export
#' @keywords internal
feature_data.bluetooth <- function(db,
                                   sensor = "Bluetooth",
                                   cols = NULL,
                                   ...,
                                   tz = Sys.timezone()) {

  call <- list(
    rlang::expr(dplyr::filter(.data = data, !dplyr::if_all(-c(participant_id, time), is.na)))
  )

  data <- feature_data.default(db = db,
                               sensor = "Bluetooth",
                               cols = cols,
                               tz = tz,
                               call = call)

  data
}

#' @rdname feature_data.default
#' @export
#' @keywords internal
feature_data.gyroscope <- function(db,
                                   sensor = "Gyroscope",
                                   cols = NULL,
                                   ...,
                                   bin = FALSE,
                                   moving_average = NULL,
                                   tz = Sys.timezone()) {

  check_arg(bin, "logical", n = 1)
  check_arg(moving_average, "integerish", n = 1, allow_null = TRUE)

  if (is.null(cols)) {
    cols <- c("x", "y", "z", "l1_norm", "l2_norm")
  }

  data <- feature_data.default(db = db,
                               sensor = "Gyroscope",
                               cols = cols,
                               bin = bin,
                               moving_average = moving_average,
                               tz = tz)

  data
}

#' @rdname feature_data.default
#' @export
#' @keywords internal
feature_data.location <- function(db,
                                  sensor = "Location",
                                  cols = NULL,
                                  ...,
                                  key = NULL,
                                  cache = NULL,
                                  tz = Sys.timezone()) {

  check_arg(key, "character", n = 1, allow_null = TRUE)

  data <- feature_data.default(db = db,
                               sensor = "Location",
                               cols = cols,
                               tz = tz)

  if (!is.null(key)) {
    if (is.null(cache)) {
      cache <- memoise::cache_memory()
    }
    mem_decrypt_gps <- memoise::memoise(decrypt_gps, cache = cache)
    data <- data %>%
      mutate(across(any_of(c("latitude", "longitude")), mem_decrypt_gps, key = key))
  }

  data
}

#' @rdname feature_data.default
#' @export
#' @keywords internal
feature_data.noise <- function(db,
                               sensor = "Noise",
                               cols = NULL,
                               ...,
                               bin = FALSE,
                               moving_average = NULL,
                               tz = Sys.timezone()) {

  check_arg(bin, "logical", n = 1)
  check_arg(moving_average, "integerish", n = 1, allow_null = TRUE)

  data <- feature_data.default(db = db,
                               sensor = "Noise",
                               cols = cols,
                               bin = bin,
                               moving_average = moving_average,
                               tz = tz)

  # Remove duplicate entries
  if (is.null(cols)) {
    data <- data %>%
      group_by(participant_id) %>%
      filter(!(lag(mean_decibel) == mean_decibel
               & lag(std_decibel) == std_decibel
               & lag(min_decibel) == min_decibel
               & lag(max_decibel) == max_decibel)) %>%
      ungroup()
  } else {
    data <- data %>%
      group_by(participant_id) %>%
      filter(!if_all(cols, ~lag(.x) == .x)) %>%
      ungroup()
  }

  data
}

feature_data.wifi <- function(db,
                              sensor,
                              cols = NULL,
                              ...,
                              add_home = TRUE,
                              n_recurring = 10,
                              tz = Sys.timezone()) {
  check_arg(add_home, "logical", n = 1)
  check_arg(n_recurring, "integerish", n = 1)

  data <- feature_data.default(db = db,
                               sensor = "Wifi",
                               cols = cols,
                               tz = tz)

  if (add_home) {
    ssid <- data %>%
      group_by(participant_id) %>%
      wifi_home_ssid() %>%
      ungroup() %>%
      dplyr::rename(home_ssid = ssid)
    data <- left_join(data, ssid, by = "participant_id")
  }

  if (n_recurring > 0) {
    ssid <- data %>%
      group_by(participant_id) %>%
      wifi_recurring_ssids() %>%
      summarise(recurring_ssids = list(ssid), .groups = "drop")
    data <- left_join(data, ssid, by = "participant_id")
  }

  data
}
