# This function is needed because calling the function on the fly from the sensor name
# (i.e. dynamic evaluation) poses a problem from the globals package, which would then not
# include these import functions in the futures.
which_sensor <- function(data, sensor) {
  switch(tolower(sensor),
    accelerometer = accelerometer_fun(data),
    activity = activity_fun(data),
    airquality = airquality_fun(data),
    appusage = app_usage_fun(data),
    battery = battery_fun(data),
    bluetooth = bluetooth_fun(data),
    calendar = calendar_fun(data),
    connectivity = connectivity_fun(data),
    device = device_fun(data),
    error = error_fun(data),
    geofence = geofence_fun(data),
    gyroscope = gyroscope_fun(data),
    heartbeat = heartbeat_fun(data),
    installedapps = installedapps_fun(data),
    keyboard = keyboard_fun(data),
    light = light_fun(data),
    location = location_fun(data),
    memory = memory_fun(data),
    mobility = mobility_fun(data),
    noise = noise_fun(data),
    phone_log = phone_log_fun(data),
    pedometer = pedometer_fun(data),
    screen = screen_fun(data),
    text_message = text_message_fun(data),
    timezone = timezone_fun(data),
    weather = weather_fun(data),
    wifi = wifi_fun(data)
  )
}

# safe_data_frame and safe_tibble moved to import_functions.R

default_fun <- function(data) {
  data$body <- lapply(data$body, function(x) x$body)
  data <- dplyr::bind_cols(data, bind_rows(data$body))
  data$body <- NULL

  data
}

accelerometer_fun <- function(data) {
  # Determine whether this is a continuous accelerometer or periodic
  if (!is.null(data$body[[1]]$body$data)) {
    data <- periodic_accelerometer_fun(data)
  } else {
    data <- default_fun(data)
  }

  # Put into right data format
  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$timestamp, 1, 10),
    time = substr(data$timestamp, 12, 23),
    n = data$n,
    x_mean = data$xm,
    y_mean = data$ym,
    z_mean = data$zm,
    x_energy = data$xms,
    y_energy = data$yms,
    z_energy = data$zms
  )
}

# Periodic accelerometer is archived with the introduction of on-phone accelerometer binning
periodic_accelerometer_fun <- function(data) {
  data$id <- vapply(data$body, function(x) x$body$id, character(1), USE.NAMES = FALSE)
  data$body <- lapply(data$body, function(x) x$body$data)

  data <- unnest(data, "body", keep_empty = TRUE)

  data$timestamp <- lapply(data$body, function(x) x$timestamp)
  data$x <- lapply(data$body, function(x) x$x)
  data$y <- lapply(data$body, function(x) x$y)
  data$z <- lapply(data$body, function(x) x$z)
  data$body <- NULL
  data <- unnest(data, "x":"z", keep_empty = TRUE)

  # TODO: Consider unique ID constraint Temporary fix
  ids <- stats::ave(numeric(nrow(data)) + 1, data$id, FUN = seq_along)
  data$id <- paste0(data$id, "_", ids)
  data
}

activity_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    confidence = data$confidence,
    type = data$type
  )
}

airquality_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    air_quality_index = data$air_quality_index,
    air_quality_level = data$air_quality_level,
    source = data$source,
    place = data$place,
    latitude = data$latitude,
    longitude = data$longitude
  )
}

app_usage_fun <- function(data) {
  data$body <- lapply(data$body, function(x) x$body)
  data$body <- suppressWarnings(lapply(data$body, bind_rows))
  data <- unnest(data, body, keep_empty = TRUE)

  if ("usage" %in% colnames(data)) {
    data$app <- names(data$usage)
    data$usage <- suppressWarnings(as.numeric(as.character(data$usage)))
  } else {
    data$app <- NA
    data$usage <- NA
  }

  # TODO: Consider unique ID constraint Temporary fix
  ids <- stats::ave(numeric(nrow(data)) + 1, data$id, FUN = seq_along)
  data$id <- paste0(data$id, "_", ids)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    start = data$start,
    end = data$end,
    usage = data$usage,
    app = data$app
  )
}

battery_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    battery_level = data$battery_level,
    battery_status = data$battery_status
  )
}

bluetooth_fun <- function(data) {
  data$id <- vapply(data$body, function(x) x$body$id, character(1))
  data$timestamp <- sapply(data$body, function(x) x$body$timestamp)
  data$body <- lapply(data$body, function(x) x$body$scan_result)
  data$body <- lapply(data$body, bind_rows)
  data <- unnest(data, body, keep_empty = TRUE)

  # TODO: Consider unique ID constraint Temporary fix
  ids <- stats::ave(numeric(nrow(data)) + 1, data$id, FUN = seq_along)
  data$id <- paste0(data$id, "_", ids)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    advertisement_name = data$advertisement_name,
    bluetooth_device_id = data$bluetooth_device_id,
    bluetooth_device_name = data$bluetooth_device_name,
    bluetooth_device_type = data$bluetooth_device_type,
    connectable = data$connectable,
    rssi = data$rssi,
    tx_power_level = data$tx_power_level
  )
}

# TODO: Check attendees TODO: Check if multiple entries (from different calendars?) are possible
# Currently, multiple entries are already possible but it's not clear why they are
# wrapped in another list as well.
calendar_fun <- function(data) {
  data$id <- vapply(data$body, function(x) x$body$id, character(1))
  data$body <- lapply(data$body, function(x) {
    x$body$calendar_events
  })

  if (!is.null(data$body[[1]])) {
    data$body <- lapply(data$body, function(x) {
      lapply(x, function(y) {
        safe_tibble(
          event_id = y$event_id,
          calendar_id = y$calendar_id,
          title = y$title,
          description = y$description,
          start = y$start,
          end = y$end,
          all_day = y$all_day,
          location = y$location,
          attendees = list(y$attendees)
        )
      })
    })
  }


  data$body <- lapply(data$body, bind_rows)
  data <- unnest(data, body, keep_empty = TRUE)

  # Collapse attendees list
  if (any("attendees" == colnames(data))) {
    data$attendees <- sapply(data$attendees, function(x) {
      if (!is.null(x)) {
        x <- paste0(x, collapse = ", ")
        if (x == "NA") {
          x <- NA_character_
        }
        return(x)
      } else {
        NA_character_
      }
    })
  }

  # TODO: Consider unique ID constraint Temporary fix
  ids <-
    stats::ave(numeric(nrow(data)) + 1, data$id, FUN = seq_along)
  data$id <- paste0(data$id, "_", ids)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    event_id = data$event_id,
    calendar_id = data$calendar_id,
    title = data$title,
    description = data$description,
    start = data$start,
    end = data$end,
    all_day = data$all_day,
    location = data$location,
    attendees = data$attendees
  )
}

connectivity_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    connectivity_status = data$connectivity_status
  )
}

device_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    platform = data$platform,
    device_id = data$device_id,
    hardware = data$hardware,
    device_name = data$device_name,
    device_manufacturer = data$device_manufacturer,
    device_model = data$device_model,
    operating_system = data$operating_system
  )
}

error_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    message = data$message
  )
}

geofence_fun <- function(data) {
  data <- default_fun(data)

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

gyroscope_fun <- function(data) {
  # Determine whether this is a continuous accelerometer or periodic
  if (!is.null(data$body[[1]]$body$data)) {
    data <- periodic_accelerometer_fun(data)
  } else {
    data <- default_fun(data)
  }

  # Put into right data format
  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$timestamp, 1, 10),
    time = substr(data$timestamp, 12, 23),
    x = data$x,
    y = data$y,
    z = data$z
  )
}

heartbeat_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    period = data$period,
    device_type = data$device_type,
    device_role_name = data$device_role_name
  )
}

# TODO: Simplify
installedapps_fun <- function(data) {
  data$body <- lapply(data$body, function(x) x$body)
  data$body <- lapply(data$body, function(x) {
    tibble(
      id = x$id,
      timestamp = x$timestamp,
      apps = list(x$installed_apps)
    )
  })
  data <- unnest(data, "body", keep_empty = TRUE)
  data <- unnest(data, "apps", keep_empty = TRUE)
  data <- unnest(data, "apps", keep_empty = TRUE)

  # TODO: Consider unique ID constraint Temporary fix
  ids <- stats::ave(numeric(nrow(data)) + 1, data$id, FUN = seq_along)
  data$id <- paste0(data$id, "_", ids)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    app = data$apps
  )
}

keyboard_fun <- function(data) {
  warn("Function for implementing keyboard data currently not implemented.")
  return(NULL)
}

light_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    mean_lux = data$mean_lux,
    std_lux = data$std_lux,
    min_lux = data$min_lux,
    max_lux = data$max_lux
  )
}

location_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    latitude = data$latitude,
    longitude = data$longitude,
    altitude = data$altitude,
    accuracy = data$accuracy,
    speed = data$speed,
    speed_accuracy = data$speed_accuracy,
    heading = data$heading
  )
}

memory_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    free_physical_memory = data$free_physical_memory,
    free_virtual_memory = data$free_virtual_memory
  )
}

mobility_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    number_of_places = data$number_of_places,
    location_variance = data$location_variance,
    entropy = data$entropy,
    normalized_entropy = data$normalized_entropy,
    home_stay = data$home_stay,
    distance_travelled = data$distance_travelled
  )
}

noise_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    mean_decibel = data$mean_decibel,
    std_decibel = data$std_decibel,
    min_decibel = data$min_decibel,
    max_decibel = data$max_decibel
  )
}

phone_log_fun <- function(data) {
  data$id <- vapply(data$body, function(x) x$body$id, character(1))

  data$body <- lapply(data$body, function(x) x$body$phone_log)
  data$body <- lapply(data$body, bind_rows)
  data$body <- lapply(data$body, function(x) {
    # Replace double timestamp name
    if (nrow(x) > 0 & "timestamp" %in% colnames(x)) {
      dplyr::rename(x, "datetime" = "timestamp")
    } else {
      x
    }
  })
  data <- unnest(data, body, keep_empty = TRUE)

  # TODO: Consider unique ID constraint Temporary fix
  ids <- stats::ave(numeric(nrow(data)) + 1, data$id, FUN = seq_along)
  data$id <- paste0(data$id, "_", ids)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    call_type = data$call_type,
    datetime = data$datetime,
    duration = data$duration,
    formatted_number = data$formatted_number,
    name = data$name,
    number = data$number
  )
}

pedometer_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    step_count = data$step_count
  )
}

screen_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    screen_event = data$screen_event
  )
}

# TODO: Check if text_message can be unnested
text_message_fun <- function(data) {
  data$id <- vapply(data$body, function(x) x$body$id, character(1))
  data$timestamp <- sapply(data$body, function(x) x$body$timestamp)
  data$body <- lapply(data$body, function(x) x$body$text_message)
  data$body <- lapply(data$body, bind_rows)
  data <- unnest(data, body, keep_empty = TRUE)

  # TODO: Consider unique ID constraint Temporary fix
  ids <- stats::ave(numeric(nrow(data)) + 1, data$id, FUN = seq_along)
  data$id <- paste0(data$id, "_", ids)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    address = data$address,
    body = data$body,
    text_date = data$date,
    date_sent = data$date_sent,
    is_read = data$is_read,
    kind = data$kind,
    size = data$size,
    state = data$state
  )
}

timezone_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    timezone = data$timezone
  )
}

# TODO: Check date, sunrise, and sunset time in UTC
weather_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
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
    rain_last_3hours = data$rain_last3_hours,
    snow_last_hour = data$snow_last_hour,
    snow_last_3hours = data$snow_last3_hours,
    temperature = data$temperature,
    temp_min = data$temp_min,
    temp_max = data$temp_max
  )
}

wifi_fun <- function(data) {
  data <- default_fun(data)

  safe_data_frame(
    measurement_id = data$id,
    participant_id = data$participant_id,
    date = substr(data$start_time, 1, 10),
    time = substr(data$start_time, 12, 19),
    ssid = data$ssid,
    bssid = data$bssid,
    ip = data$ip
  )
}
