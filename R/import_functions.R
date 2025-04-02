rand <- function(n, chars = TRUE, numbers = TRUE, uppercase = FALSE) {
  if (!chars && !numbers) {
    abort("You must select either letters, numbers, or both.")
  }

  data <- NULL
  if (chars) {
    if (uppercase) {
      data <- c(data, LETTERS[1:6])
    } else {
      data <- c(data, letters[1:6])
    }
  }

  if (numbers) {
    data <- c(data, 0:9)
  }

  paste0(sample(data, n, TRUE), collapse = "")
}

gen_id <- function(uppercase = FALSE) {
  res <- paste(rand(8), rand(4), rand(4), rand(4), rand(12), sep = "-")

  if (uppercase) {
    res <- toupper(res)
  }

  res
}

# Make a data frame, handling missing columns, filling with NA
safe_data_frame <- function(...) {
  x <- suppressWarnings(list(...))
  x <- lapply(x, function(x) {
    if (is.null(x)) {
      NA
    } else {
      x
    }
  })
  x <- as.data.frame(x)
  x
}

safe_tibble <- function(...) {
  x <- suppressWarnings(list(...))
  x <- lapply(x, function(x) {
    if (is.null(x)) {
      NA
    } else {
      x
    }
  })
  x <- lapply(x, function(x) {
    if (length(x[[1]]) == 0) {
      NA
    } else {
      x
    }
  }) # lists
  x <- tibble::as_tibble(x)
  x
}


#' Unpack raw sensor data
#'
#' This function takes raw sensor data coming from [import()] and unpacks it into tidy data frames
#' so that it can be written to the database. Note that this function is internal and should not
#' be used for other purposes.
#'
#' @param data A data frame containing the raw sensor data.
#' @inheritParams rlang::args_dots_empty
#'
#' @return A data frame with the sensor data unpacked.
#' @keywords internal
#'
#' @examples
#' x <- tibble::tibble(
#'   study_id = "test-study",
#'   participant_id = "12345",
#'   data_format = "cams 1.0.0",
#'   start_time = "2021-11-14 16:40:00.123456",
#'   end_time = NULL,
#'   sensor = "Activity",
#'   data = list(list(
#'     confidence = 80,
#'     type = "WALKING"
#'   ))
#' )
#' class(x) <- c("activity", class(x))
#' mpathsenser:::unpack_sensor_data(x)
unpack_sensor_data <- function(data, ...) {
  rlang::check_dots_empty()
  UseMethod("unpack_sensor_data")
}

#' @export
#' @keywords internal
unpack_sensor_data.default <- function(data, sensor, ...) {
  data <- tidyr::unnest_wider(data, "data")
  data$data <- NULL

  # Add a measurement_id column if it doesn't exist
  if (!any(c("measurement_id", "id") %in% colnames(data))) {
    gen_id <- Vectorize(gen_id)
    data <- tibble(
      measurement_id = gen_id(seq_len(nrow(data))),
      data
    )
  }

  # Remap column names
  class(data) <- c(sensor, class(data))
  data <- alias_column_names(data)

  data
}

#' @export
#' @keywords internal
unpack_sensor_data.accelerometer <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "accelerometer", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    end_time = data$end_time,
    n = data$n,
    x_mean = data$x_mean,
    y_mean = data$y_mean,
    z_mean = data$z_mean,
    x_median = data$x_median,
    y_median = data$y_median,
    z_median = data$z_median,
    x_std = data$x_std,
    y_std = data$y_std,
    z_std = data$z_std,
    x_aad = data$x_aad,
    y_aad = data$y_aad,
    z_aad = data$z_aad,
    x_min = data$x_min,
    y_min = data$y_min,
    z_min = data$z_min,
    x_max = data$x_max,
    y_max = data$y_max,
    z_max = data$z_max,
    x_max_min_diff = data$x_max_min_diff,
    y_max_min_diff = data$y_max_min_diff,
    z_max_min_diff = data$z_max_min_diff,
    x_mad = data$x_mad,
    y_mad = data$y_mad,
    z_mad = data$z_mad,
    x_iqr = data$x_iqr,
    y_iqr = data$y_iqr,
    z_iqr = data$z_iqr,
    x_neg_n = data$x_neg_n,
    y_neg_n = data$y_neg_n,
    z_neg_n = data$z_neg_n,
    x_pos_n = data$x_pos_n,
    y_pos_n = data$y_pos_n,
    z_pos_n = data$z_pos_n,
    x_above_mean = data$x_above_mean,
    y_above_mean = data$y_above_mean,
    z_above_mean = data$z_above_mean,
    x_energy = data$x_energy,
    y_energy = data$y_energy,
    z_energy = data$z_energy,
    avg_res_acc = data$avg_res_acc,
    sma = data$sma
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.activity <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "activity", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    confidence = data$confidence,
    type = data$type
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.airquality <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "airquality", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    air_quality_index = data$air_quality_index,
    air_quality_level = data$air_quality_level,
    source = data$source,
    place = data$place,
    latitude = data$latitude,
    longitude = data$longitude
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.appusage <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "appusage", ...)

  if (!is.null(data$usage) && !all(is.na(data$usage))) {
    data$usage <- lapply(data$usage, bind_rows)
    data <- unnest(data, "usage", keep_empty = TRUE)
  }

  # If `startDate` and `endDate` columns exist, remove `start` and `end` columns as these are
  # duplicate of start_time and end_time.
  # TODO: add synonyms of these columns if new ones are added. `alias_column_names()` does not solve
  # this problem as it will throw an error that there are duplicate names (which is true).
  if (any(c("startDate", "endDate", "start_date", "end_date") %in% colnames(data))) {
    data <- select(data, -any_of(c("start", "end")))
  }

  # Remap column names
  class(data) <- c("appusage", class(data))
  data <- alias_column_names(data)

  # TODO: Consider unique ID constraint Temporary fix
  ids <- stats::ave(numeric(nrow(data)) + 1, data$measurement_id, FUN = seq_along)
  data$measurement_id <- paste0(data$measurement_id, "_", ids)

  if ("last_foreground" %in% colnames(data)) {
    data$last_foreground[grepl("1970-01", data$last_foreground)] <- NA
  }

  # In this case, the end_time of the measurement is not relevant and already included in the data.
  # There is an end_date in the data, but this is already incorporated in the more precise `end`
  # column.
  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    end_time = data$end_time,
    start = data$start,
    end = data$end,
    usage = data$usage,
    app = data$app,
    package_name = data$package_name,
    last_foreground = data$last_foreground
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.battery <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "battery", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    battery_level = data$battery_level,
    battery_status = data$battery_status
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.bluetooth <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "bluetooth", ...)

  if ("scan_result" %in% colnames(data) && !all(is.na(data$scan_result))) {
    data$scan_result <- lapply(data$scan_result, bind_rows)
    data <- unnest(data, "scan_result", keep_empty = TRUE)

    # Remap column names again, now with unnested data
    class(data) <- c("bluetooth", class(data))
    data <- alias_column_names(data)
  }

  # TODO: Consider unique ID constraint Temporary fix
  ids <- stats::ave(numeric(nrow(data)) + 1, data$measurement_id, FUN = seq_along)
  data$measurement_id <- paste0(data$measurement_id, "_", ids)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    start_scan = data$start_scan,
    end_scan = data$end_scan,
    advertisement_name = data$advertisement_name,
    bluetooth_device_id = data$bluetooth_device_id,
    bluetooth_device_name = data$bluetooth_device_name,
    bluetooth_device_type = data$bluetooth_device_type,
    connectable = data$connectable,
    rssi = data$rssi,
    tx_power_level = data$tx_power_level
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.connectivity <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "connectivity", ...)

  # double unnes on connectivity_status in case this is a list, which means it can contain multiple
  # entries of a connectivity_status in  the same measurement
  if ("connectivity_status" %in% colnames(data) && is.list(data[["connectivity_status"]])) {
    data <- unnest(data, "connectivity_status")
    data <- unnest(data, "connectivity_status")

    # TODO: Consider unique ID constraint Temporary fix
    ids <- stats::ave(numeric(nrow(data)) + 1, data$measurement_id, FUN = seq_along)
    data$measurement_id <- paste0(data$measurement_id, "_", ids)
  }

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    connectivity_status = data$connectivity_status
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.device <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "device", ...)

  # Try to add sdk and OS version info
  if ("device_data" %in% colnames(data)) {
    android_osv <- purrr::map_vec(
      data$device_data,
      \(x) purrr::pluck(x, "version", "release", .default = NA)
    )
    android_sdk <- purrr::map_vec(
      data$device_data,
      \(x) purrr::pluck(x, "version", "sdkInt", .default = NA)
    )
    ios_osv <- purrr::map_vec(
      data$device_data,
      \(x) purrr::pluck(x, "systemVersion", .default = NA)
    )
    ios_sdk <- purrr::map_vec(
      data$device_data,
      \(x) purrr::pluck(x, "utsname", "release", .default = NA)
    )

    # Use iOS values if Android values are missing
    # If the other turns out to be missing as well, it doesn't matter which one we use
    osv <- if (all(is.na(android_osv))) ios_osv else android_osv
    sdk <- if (all(is.na(android_sdk))) ios_sdk else android_sdk

    data$operating_system_version <- osv
    data$sdk <- sdk
  }

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    device_id = data$device_id,
    hardware = data$hardware,
    device_name = data$device_name,
    device_manufacturer = data$device_manufacturer,
    device_model = data$device_model,
    operating_system = data$operating_system,
    platform = data$platform,
    operating_system_version = data$operating_system_version,
    sdk = data$sdk
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.error <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "error", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    message = data$message
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.geofence <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "geofence", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    center = data$center,
    dwell = data$dwell,
    name = data$name,
    radius = data$radius,
    state = data$state
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.gyroscope <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "gyroscope", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    x = data$x,
    y = data$y,
    z = data$z
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.heartbeat <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "heartbeat", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    period = data$period,
    device_type = data$device_type,
    device_role_name = data$device_role_name
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.keyboard <- function(data, ...) {
  warn("Function for implementing keyboard data currently not implemented.")
  return(NULL)
}

#' @export
#' @keywords internal
unpack_sensor_data.light <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "light", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    end_time = data$end_time,
    mean_lux = data$mean_lux,
    std_lux = data$std_lux,
    min_lux = data$min_lux,
    max_lux = data$max_lux
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.location <- function(data, ...) {
  # If a column 'time' is already present in the data, remove it from the sensor data. Otherwise
  # alias_column_names will throw an error for a duplicate column 'time' and the time column in the
  # data is not in UTC, so `start_time` is preferred.
  data$data <- lapply(data$data, \(x) x[setdiff(names(x), "time")])

  data <- unpack_sensor_data.default(data, "location", ...)

  # Remap column names
  class(data) <- c("location", class(data))
  data <- alias_column_names(data)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    latitude = data$latitude,
    longitude = data$longitude,
    altitude = data$altitude,
    accuracy = data$accuracy,
    vertical_accuracy = data$vertical_accuracy,
    speed = data$speed,
    speed_accuracy = data$speed_accuracy,
    heading = data$heading,
    heading_accuracy = data$heading_accuracy,
    is_mock = data$is_mock
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.memory <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "memory", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    free_physical_memory = data$free_physical_memory,
    free_virtual_memory = data$free_virtual_memory
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.mpathinfo <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "mpathinfo", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    connection_id = data$connection_id,
    account_code = data$account_code,
    study_name = data$study_name,
    sense_version = data$sense_version
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.noise <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "noise", ...)


  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    end_time = data$end_time,
    mean_decibel = data$mean_decibel,
    std_decibel = data$std_decibel,
    min_decibel = data$min_decibel,
    max_decibel = data$max_decibel
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.pedometer <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "pedometer", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    step_count = data$step_count
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.screen <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "screen", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    screen_event = data$screen_event
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.timezone <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "timezone", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    timezone = data$timezone
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.weather <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "weather", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    country = data$country,
    area_name = data$area_name,
    weather_main = data$weather_main,
    weather_description = data$weather_description,
    sunrise = data$sunrise,
    sunset = data$sunset,
    latitude = data$latitude,
    longitude = data$longitude,
    pressure = data$pressure,
    wind_speed = data$wind_speed,
    wind_degree = data$wind_degree,
    humidity = data$humidity,
    cloudiness = data$cloudiness,
    rain_last_hour = data$rain_last_hour,
    rain_last_3hours = data$rain_last_3hours,
    snow_last_hour = data$snow_last_hour,
    snow_last_3hours = data$snow_last_3hours,
    temperature = data$temperature,
    temp_min = data$temp_min,
    temp_max = data$temp_max
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.wifi <- function(data, ...) {
  data <- unpack_sensor_data.default(data, "wifi", ...)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 23),
    ssid = data$ssid,
    bssid = data$bssid,
    ip = data$ip
  )
}
