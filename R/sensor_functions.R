# save_sensor_data <- function(data, name) {
# 	if(exists(name, mode = "list")) {
# 		assign(name, dplyr::bind_rows(get(name), data), envir = .GlobalEnv)
# 	} else {
# 		assign(name, data, envir = .GlobalEnv)
# 	}
# }

which_sensor <- function(data, sensor) {
	switch(sensor,
				 accelerometer = accelerometer_fun(data),
				 activity = activity_fun(data),
				 air_quality = air_quality_fun(data),
				 app_usage = app_usage_fun(data),
				 apps = apps_fun(data),
				 battery = battery_fun(data),
				 bluetooth = bluetooth_fun(data),
				 calendar = calendar_fun(data),
				 connectivity = connectivity_fun(data),
				 device = device_fun(data),
				 gyroscope = gyroscope_fun(data),
				 light = light_fun(data),
				 location = location_fun(data),
				 memory = memory_fun(data),
				 mobility = mobility_fun(data),
				 screen = screen_fun(data),
				 text_message = text_message_fun(data),
				 weather = weather_fun(data),
				 wifi = wifi_fun(data))
}

default_fun <- function(data) {
	data$body <- lapply(data$body, function(x) x$body)
	data$body <- lapply(data$body, dplyr::bind_rows)
	data <- tidyr::unnest(data, body)
	# data$timestamp <- as.POSIXct(data$timestamp, "%Y-%m-%dT%H:%M:%S", tz="Europe/Brussels")
	data$id <- NULL

	if(nrow(data) > 0) {
		return(data)
	} else {
		return(NA)
	}
}

accelerometer_fun <- function(data) {
	data$body <- lapply(data$body, function(x) x$body$data)
	data$body <- lapply(data$body, dplyr::bind_rows)
	data <- tidyr::unnest(data, body)
	# data$timestamp <- as.POSIXct(data$timestamp, "%Y-%m-%dT%H:%M:%S", tz="Europe/Brussels")
	data
}

activity_fun <- function(data) {
	default_fun(data)
}

air_quality_fun <- function(data) {
	default_fun(data)
}

app_usage_fun <- function(data) {
	default_fun(data)
}

apps_fun <- function(data) {
	data$body <- lapply(data$body, function(x) x$body)
	data$body <- lapply(data$body, function(x) tibble::tibble(apps = list(x$installed_apps)))
	data <- tidyr::unnest(data, body)
	# data$timestamp <- as.POSIXct(data$timestamp, "%Y-%m-%dT%H:%M:%S", tz="Europe/Brussels")
	data
}

# TODO: incorrect app usage
battery_fun <- function(data) {
	default_fun(data)
}

bluetooth_fun <- function(data) {
	data$body <- lapply(data$body, function(x) x$body$scan_result)
	data$body <- lapply(data$body, dplyr::bind_rows)
	data <- tidyr::unnest(data,body)
	data
}

# TODO: Check attendees
calendar_fun <- function(data) {
	data$body <- lapply(data$body, function(x) x$body$calendar_events)
	data$body <- lapply(data$body, function(x) lapply(x, function(y) unlist(y, recursive = FALSE)))
	data$body <- lapply(data$body, dplyr::bind_rows)
	data <- tidyr::unnest(data, body, keep_empty = TRUE)

	if(nrow(data) > 0) {
		data$start <- as.POSIXct(data$start, "%Y-%m-%dT%H:%M:%S", tz="Europe/Brussels")
		data$end <- as.POSIXct(data$end, "%Y-%m-%dT%H:%M:%S", tz="Europe/Brussels")
	}
	data
}

connectivity_fun <- function(data) {
	default_fun(data)
}

device_fun <- function(data) {
	default_fun(data)
}

gyroscope_fun <- function(data) {
	accelerometer_fun(data)
}

light_fun <- function(data) {
	default_fun(data)
}

location_fun <- function(data) {
	default_fun(data)
}

memory_fun <- function(data) {
	default_fun(data)
}

# TODO: find out how this works
mobility_fun <- function(data) {
	default_fun(data)
}

screen_fun <- function(data) {
	default_fun(data)
}

# TODO: Check if text_message can be unnested
text_message_fun <- function(data) {
	default_fun(data)
}

weather_fun <- function(data) {
	default_fun(data)
}

wifi_fun <- function(data) {
	default_fun(data)
}
