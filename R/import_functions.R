#' @keywords internal
unpack_sensor_data <- function(data){
  UseMethod("unpack_sensor_data")
}

rand <- function(n, chars = TRUE, numbers = TRUE, uppercase = FALSE) {
  data <- NULL

  if (!chars && !numbers) abort("Select either letters, numbers, or both.")

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

  paste(sample(data, n, TRUE), collapse = "")
}

gen_id <- function(uppercase = FALSE) {
  if (uppercase) {
    paste0(
      rand(8, uppercase = TRUE), "-", rand(4, uppercase = TRUE), "-",
      rand(4, uppercase = TRUE), "-", rand(4, uppercase = TRUE), "-",
      rand(12, uppercase = TRUE)
    )
  } else {
    paste0(rand(8), "-", rand(4), "-", rand(4), "-", rand(4), "-", rand(12))
  }
}

#' @export
#' @keywords internal
unpack_sensor_data.default <- function(data) {
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

  data
}

#' @export
#' @keywords internal
unpack_sensor_data.accelerometer <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "accelerometer"),
    )

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
unpack_sensor_data.activity <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "activity"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    confidence = data$confidence,
    type = data$type
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.airquality <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "airquality"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
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
unpack_sensor_data.appusage <- function(data) {
  data <- unpack_sensor_data.default(data)

  data$usage <- lapply(data$usage, bind_rows)
  data <- unnest(data, "usage", keep_empty = TRUE)

  # If `startDate` and `endDate` columns exist, remove `start` and `end` columns as these are duplicate
  # of start_time and end_time.
  if (any(c("startDate", "endDate") %in% colnames(data))) {
    data <- data |>
      select(-any_of(c("start", "end")))
  }

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "appusage"),
    )

  # TODO: Consider unique ID constraint Temporary fix
  ids <- stats::ave(numeric(nrow(data)) + 1, data$measurement_id, FUN = seq_along)
  data$measurement_id <- paste0(data$measurement_id, "_", ids)

  if ("last_foreground" %in% colnames(data)) {
    data$last_foreground[grepl("1970-01", data$last_foreground)] <- NA
  }

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
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
unpack_sensor_data.battery <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "battery"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    battery_level = data$battery_level,
    battery_status = data$battery_status
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.bluetooth <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "bluetooth"),
    )

  if (!all(is.na(data$scan_result))) {
    data$scan_result <- lapply(data$scan_result, bind_rows)
    data <- unnest(data, "scan_result", keep_empty = TRUE)

    # Remap column names again, now with unnested data
    data <- data |>
      dplyr::rename_with(
        .fn = \(x) alias_column_names(x, "bluetooth"),
      )

  }

  # TODO: Consider unique ID constraint Temporary fix
  ids <- stats::ave(numeric(nrow(data)) + 1, data$measurement_id, FUN = seq_along)
  data$measurement_id <- paste0(data$measurement_id, "_", ids)

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
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
unpack_sensor_data.connectivity <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "connectivity"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    connectivity_status = data$connectivity_status
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.device <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "device"),
    )

  # Try to add sdk and OS version info
  if (unique(data$platform)[[1]] == "Android") {
    data$operating_system_version <- vapply(
      data$deviceData,
      \(x) purrr::pluck(x, "version", "release"),
      character(1)
    )
    data$sdk <- purrr::map_vec(
      data$deviceData,
      \(x) purrr::pluck(x, "version", "sdkInt")
    )
    data$sdk <- as.character(data$sdk)
  } else if (unique(data$platform)[[1]] == "iOS") {
    data$operating_system_version <- vapply(
      data$deviceData,
      \(x) purrr::pluck(x, "systemVersion"),
      character(1)
    )
    data$sdk <- vapply(
      data$deviceData,
      \(x) purrr::pluck(x, "utsname", "release"),
      character(1)
    )
  }

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
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
unpack_sensor_data.error <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "error"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    message = data$message
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.geofence <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "geofence"),
    )

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    center = data$center,
    dwell = data$dwell,
    name = data$name,
    radius = data$radius,
    state = data$state
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.gyroscope <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "gyroscope"),
    )

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
unpack_sensor_data.heartbeat <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "heartbeat"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    period = data$period,
    device_type = data$device_type,
    device_role_name = data$device_role_name
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.keyboard <- function(data) {
  warn("Function for implementing keyboard data currently not implemented.")
  return(NULL)
}

#' @export
#' @keywords internal
unpack_sensor_data.light <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "light"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    end_time =  data$end_time,
    mean_lux = data$mean_lux,
    std_lux = data$std_lux,
    min_lux = data$min_lux,
    max_lux = data$max_lux
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.location <- function(data) {
  data <- unpack_sensor_data.default(data)

  # If a column 'time' is already present in the data and it is not NA, use this instead of the
  # sensor's start_time, as the timestamp from the location service is likely to be more accurate
  if ("time" %in% colnames(data) && !all(is.na(data$time))) {
    data$start_time <- data$time
    data$time <- NULL
  }

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "location"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
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
unpack_sensor_data.memory <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "memory"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    free_physical_memory = data$free_physical_memory,
    free_virtual_memory = data$free_virtual_memory
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.noise <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "noise"),
    )


  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    mean_decibel = data$mean_decibel,
    std_decibel = data$std_decibel,
    min_decibel = data$min_decibel,
    max_decibel = data$max_decibel
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.pedometer <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "pedometer"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    step_count = data$step_count
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.screen <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "screen"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    screen_event = data$screen_event
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.timezone <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "timezone"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    timezone = data$timezone
  )
}

#' @export
#' @keywords internal
unpack_sensor_data.weather <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "weather"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
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
unpack_sensor_data.wifi <- function(data) {
  data <- unpack_sensor_data.default(data)

  # Remap column names
  data <- data |>
    dplyr::rename_with(
      .fn = \(x) alias_column_names(x, "wifi"),
    )

  safe_data_frame(
    measurement_id = data$measurement_id,
    participant_id = data$participant_id,
    date = substr(data$time, 1, 10),
    time = substr(data$time, 12, 19),
    ssid = data$ssid,
    bssid = data$bssid,
    ip = data$ip
  )
}
