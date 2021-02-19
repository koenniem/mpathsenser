save2db <- function(db, name, data) {
		dbAppendTable(db, name, data)
}

which_sensor <- function(db, data, sensor) {
	switch(sensor,
				 accelerometer = accelerometer_fun(db, data),
				 activity = activity_fun(db, data),
				 air_quality = air_quality_fun(db, data),
				 app_usage = app_usage_fun(db, data),
				 apps = apps_fun(db, data),
				 battery = battery_fun(db, data),
				 bluetooth = bluetooth_fun(db, data),
				 calendar = calendar_fun(db, data),
				 connectivity = connectivity_fun(db, data),
				 device = device_fun(db, data),
				 geofence = geofence_fun(db, data),
				 gyroscope = gyroscope_fun(db, data),
				 keyboard = keyboard_fun(db, data),
				 light = light_fun(db, data),
				 location = location_fun(db, data),
				 memory = memory_fun(db, data),
				 mobility = mobility_fun(db, data),
				 noise = noise_fun(db, data),
				 phone_log = phone_log_fun(db, data),
				 pedometer = pedometer_fun(db, data),
				 screen = screen_fun(db, data),
				 text_message = text_message_fun(db, data),
				 weather = weather_fun(db, data),
				 wifi = wifi_fun(db, data),
			   default_fun(data)) # default
}

# # Make a data frame, handling and ensuring no columns are missing
# tidy_df <- function(data, names) {
# 	missing <- setdiff(names, colnames(data)) # Find if any column is missing
# 	data[,missing] <- NA # If so, fill with NA
# 	data <- data[,names] # Select the relevant columns, in the right order
# 	colnames(data) <- names(names) # Replace those names with the new names
# 	data
# }
safe.data.frame <- function(...) {
	x <- suppressWarnings(list(...))
	x <- lapply(x, function(x) if(is.null(x)) NA else x)
	x <- as.data.frame(x)
	x
}

default_fun <- function(data) {
	data$body <- lapply(data$body, function(x) x$body)
	data <- dplyr::bind_cols(data, dplyr::bind_rows(data$body))
	data$body <- NULL

	if(nrow(data) > 0) {
		return(data)
	} else {
		return(NA)
	}
}

accelerometer_fun <- function(db, data) {
	# Transform data
	data <- default_fun(data)

	# Put into right data format
	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		x = data$x,
		y = data$y,
		z = data$z
	)

	# Save to database
	save2db(db, "Accelerometer", data)
}

activity_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		confidence = data$confidence,
		type = data$type
	)

	save2db(db, "Activity", data)
}

air_quality_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		air_quality_index = data$air_quality_index,
		air_quality_level = data$air_quality_level,
		source = data$source,
		place = data$place,
		latitude = data$latitude,
		longitude = data$longitude
	)

	save2db(db, "AirQuality", data)
}

app_usage_fun <- function(db, data) {
	data$body <- lapply(data$body, function(x) x$body)
	data$body <- suppressWarnings(lapply(data$body, dplyr::bind_rows))
	data <- tidyr::unnest(data, body)

	if(nrow(data) == 0) {
		return(invisible(NULL))
	}

	if("usage" %in% colnames(data)) {
		# Get and delete entries that have null usage
		# nulls <- unlist(lapply(data$usage, is.null))
		# data <- data[!nulls,]

		data$app <- names(data$usage)
		data$usage <- suppressWarnings(as.numeric(as.character(data$usage)))
	} else {
		data$app <- NA
		data$usage <- NA
	}

	# TODO: Consider unique ID constraint
	# Temporary fix
	data <- group_by(data, id)
	data <- mutate(data, id = paste0(id, "_", 1:n()))
	data <- ungroup(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		start = data$start,
		end = data$end,
		usage = data$usage,
		app = data$app
	)

	save2db(db, "AppUsage", data)
}

# TODO: Simplify and add timestamp and id
apps_fun <- function(db, data) {
	data$body <- lapply(data$body, function(x) x$body)
	data$body <- lapply(data$body, function(x) tibble::tibble(id = x$id,
																														timestamp = x$timestamp,
																														apps = list(x$installed_apps)))
	data <- tidyr::unnest(data, body)
	data$apps <- sapply(data$apps, function(x) paste0(x, collapse = "|"))
	# data <- tidyr::unnest(data, apps)
	# data$timestamp <- as.POSIXct(data$timestamp, "%Y-%m-%dT%H:%M:%S", tz="Europe/Brussels")
	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		apps = data$apps
	)

	save2db(db, "InstalledApps", data)
}

battery_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		battery_level = data$battery_level,
		battery_status = data$battery_status
	)

	save2db(db, "Battery", data)
}

bluetooth_fun <- function(db, data) {
	data$id <- sapply(data$body, function(x) x$body$id)
	data$timestamp <- sapply(data$body, function(x) x$body$timestamp)
	data$body <- lapply(data$body, function(x) x$body$scan_result)
	data$body <- lapply(data$body, dplyr::bind_rows)
	data <- tidyr::unnest(data,body)

	if(nrow(data) == 0) {
		return(invisible(NULL))
	}

	# TODO: Consider unique ID constraint
	# Temporary fix
	data <- group_by(data, id)
	data <- mutate(data, id = paste0(id, "_", 1:n()))
	data <- ungroup(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$start_time,
		advertisement_name = data$advertisement_name,
		bluetooth_device_id = data$bluetooth_device_id,
		bluetooth_device_name = data$bluetooth_device_name,
		bluetooth_device_type = data$bluetooth_device_type,
		connectable = data$connectable,
		rssi = data$rssi,
		tx_power_level = data$tx_power_level
	)

	save2db(db, "Bluetooth", data)
}

# TODO: Check attendees
# TODO: save to db
calendar_fun <- function(db, data) {
	data$id <- sapply(data$body, function(x) x$body$id)
	data$timestamp <- sapply(data$body, function(x) x$body$timestamp)
	data$body <- lapply(data$body, function(x) x$body$calendar_events)
	data$body <- lapply(data$body, function(x) lapply(x, function(y)
		tibble(event_id = y[["event_id"]],
					 calendar_id = y[["calendar_id"]],
					 title = y[["title"]],
					 description = y[["description"]],
					 start = y[["start"]],
					 end = y[["end"]],
					 all_day = y[["all_day"]],
					 location = y[["location"]],
					 attendees = I(y[["attendees"]])
		)))
	data$body <- lapply(data$body, dplyr::bind_rows)
	data <- tidyr::unnest(data, body)

	# This is actually a bug. If a measurement is performed but no useful data was collected,
	# the measurement itself should still be registered.
	# Ignored for now.
	if(nrow(data) == 0) {
		return(invisible(TRUE))
	}

	data$attendees <- sapply(data$attendees, function(x) paste0(x, collapse = ", "))

	# TODO: Consider unique ID constraint
	# Temporary fix
	data <- group_by(data, id)
	data <- mutate(data, id = paste0(id, "_", 1:n()))
	data <- ungroup(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$start_time,
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

	save2db(db, "Calendar", data)
}

connectivity_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		connectivity_status = data$connectivity_status
	)

	save2db(db, "Connectivity", data)
}

device_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		device_id = data$device_id,
		hardware = data$hardware,
		device_name = data$device_name,
		device_manufacturer = data$device_manufacturer,
		device_model = data$device_model,
		operating_system = data$operating_system
	)

	save2db(db, "Device", data)
}

geofence_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		center = data$center,
		dwell = data$dwell,
		name = data$name,
		radius = data$radius,
		state = data$state
	)

	save2db(db, "Geofence", data)
}

gyroscope_fun <- function(db, data) {
	# Transform data
	data <- default_fun(data)

	# Put into right data format
	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		x = data$x,
		y = data$y,
		z = data$z
	)

	# Save to database
	save2db(db, "Gyroscope", data)
}

keyboard_fun <- function(db, data) {
	error("Function not implemented")
}

light_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		mean_lux = data$mean_lux,
		std_lux = data$std_lux,
		min_lux = data$min_lux,
		max_lux = data$max_lux
	)

	save2db(db, "Light", data)
}

location_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		latitude = data$latitude,
		longitude = data$longitude,
		altitude = data$altitude,
		accuracy = data$accuracy,
		speed = data$speed,
		speed_accuracy = data$speed_accuracy,
		heading = data$heading
	)

	save2db(db, "Location", data)
}

memory_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		free_physical_memory = data$free_physical_memory,
		free_virtual_memory = data$free_virtual_memory
	)

	save2db(db, "Memory", data)
}

# TODO: find out how this works
mobility_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		number_of_places = data$number_of_places,
		location_variance = data$location_variance,
		entropy = data$entropy,
		normalized_entropy = data$normalized_entropy,
		home_stay = data$home_stay,
		distance_travelled = data$distance_travelled
	)

	save2db(db, "Mobility", data)
}

noise_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		mean_decibel = data$mean_decibel,
		std_decibel = data$std_decibel,
		min_decibel = data$min_decibel,
		max_decibel = data$max_decibel
	)

	save2db(db, "Noise", data)
}

phone_log_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		call_type = data$call_type,
		datetime = data$datetime,
		duration = data$duration,
		formatted_number = data$formatted_number,
		name = data$name,
		number = data$number
	)

	save2db(db, "PhoneLog", data)
}

pedometer_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		step_count = data$step_count
	)

	save2db(db, "Pedometer", data)
}

screen_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		screen_event = data$screen_event
	)

	save2db(db, "Screen", data)
}

# TODO: Check if text_message can be unnested
text_message_fun <- function(db, data) {
	# data <- default_fun(data)
	data$id <- sapply(data$body, function(x) x$body$id)
	data$timestamp <- sapply(data$body, function(x) x$body$timestamp)
	data$body <- lapply(data$body, function(x) x$body$text_message)
	data$body <- lapply(data$body, dplyr::bind_rows)
	data <- tidyr::unnest(data,body)

	if(nrow(data) == 0) {
		return(invisible(NULL))
	}

	# TODO: Consider unique ID constraint
	# Temporary fix
	data <- group_by(data, id)
	data <- mutate(data, id = paste0(id, "_", 1:n()))
	data <- ungroup(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		address = data$address,
		body = data$body,
		date = data$date,
		date_sent = data$date_sent,
		is_read = data$is_read,
		kind = data$kind,
		size = data$size,
		state = data$state
	)

	save2db(db, "TextMessage", data)
}

# TODO: Check date, sunrise, and sunset time in UTC
weather_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
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

	save2db(db, "Weather", data)
}

wifi_fun <- function(db, data) {
	data <- default_fun(data)

	data <- safe.data.frame(
		measurement_id = data$id,
		participant_id = data$participant_id,
		time = data$timestamp,
		ssid = data$ssid,
		bssid = data$bssid
	)

	save2db(db, "Wifi", data)
}
