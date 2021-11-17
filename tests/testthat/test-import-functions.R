# Tests for import_functions.R

common_test <- function(sensor, ...) {
	tibble::tibble(
		body = list(...),
		study_id = "test-study",
		participant_id = "12345",
		start_time = "2021-11-14T16:40:00.123456Z",
		data_format = "carp",
		sensor = sensor
	)
}


unit_test <- function(sensor, ...) {
	# Define the input
	dat <- common_test(sensor, list(
		body = list(
			id = "12345a",
			timestamp = "2021-11-14T16:40:01.123456Z",
			...
		)
	))

	# Execute the sensor function based on its name
	res <- do.call(paste0(sensor, "_fun"), list(dat))
	res_which <- which_sensor(dat, sensor)

	# Check if there is a list column present since this must be unested first
	depth <- lapply(list(...), function(x) length(x))
	if (any(depth > 1)) {
		true <- tibble::tibble(
			measurement_id = "12345a",
			participant_id = "12345",
			date = "2021-11-14",
			time = "16:40:00",
			...
		)
		true$measurement_id <- paste0(true$measurement_id, "_", 1:nrow(true))

		true <- tidyr::unnest_wider(true, names(which(depth > 1)))
		true <- as.data.frame(true)
	} else {
		true <- data.frame(
			measurement_id = "12345a",
			participant_id = "12345",
			date = "2021-11-14",
			time = "16:40:00",
			list(...)
		)
	}

	# Make sure columns are in the same order
	true <- true[,colnames(res)]

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
}

test_that("safe_data_frame", {
	dat <- data.frame(a = 1, b = NA)
	res <- safe_data_frame(a = dat$a, b = dat$b, c = dat$c)
	true <- data.frame(a = 1, b = NA, c = NA)
	expect_identical(res, true)
})

test_that("safe_tibble", {
	dat <- tibble::tibble(a = 1, b = NA, c = vector("list", 1))
	dat2 <- tibble::tibble(a = 1, b = NA, c = vector("list", 0))
	res <- safe_tibble(a = dat$a, b = dat$b, c = dat$c, d = dat$d)
	res2 <- safe_tibble(a = dat$a, b = dat$b, c = dat$c, d = dat$d)
	true <- tibble::tibble(a = 1, b = NA, c = NA, d = NA)
	expect_identical(res, true)
	expect_identical(res2, true)
})

test_that("accelerometer", {
	dat <- common_test("accelerometer",
										 list(
										 	body = list(
										 		id = "12345a",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		x = 0.123456789,
										 		y = 0.123456789,
										 		z = 9.123456789
										 	)
										 )
	)

	res <- accelerometer_fun(dat)
	res_which <- which_sensor(dat, "accelerometer")
	true <- data.frame(
		measurement_id = "12345a",
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:01.123",
		x = 0.123456789,
		y = 0.123456789,
		z = 9.123456789
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

test_that("gyroscope", {
	dat <- common_test("gyroscope",
										 list(
										 	body = list(
										 		id = "12345a",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		x = 0.123456789,
										 		y = 0.123456789,
										 		z = 9.123456789
										 	)
										 )
	)

	res <- gyroscope_fun(dat)
	res_which <- which_sensor(dat, "gyroscope")
	true <- data.frame(
		measurement_id = "12345a",
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:01.123",
		x = 0.123456789,
		y = 0.123456789,
		z = 9.123456789
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

test_that("periodic_accelerometer", {
	dat <- common_test("accelerometer",
										 list(
										 	body = list(
										 		id = "12345a",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		data = list(
										 			list(
										 				timestamp = "2021-11-14T16:40:01.223456Z",
										 				x = 1.12345,
										 				y = -0.1234,
										 				z = 0.123456
										 			),
										 			list(
										 				timestamp = "2021-11-14T16:40:01.323456Z",
										 				x = 1.12345,
										 				y = -0.1234,
										 				z = 0.123456
										 			)
										 		)
										 	)
										 ),
										 list(
										 	body = list(
										 		id = "12345b",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		data = list(
										 			list(
										 				timestamp = "2021-11-14T16:40:01.223456Z",
										 				x = 1.12345,
										 				y = -0.1234,
										 				z = 0.123456
										 			),
										 			list(
										 				timestamp = "2021-11-14T16:40:01.323456Z",
										 				x = 1.12345,
										 				y = -0.1234,
										 				z = 0.123456
										 			)
										 		)
										 	)
										 )
	)
	res <- accelerometer_fun(dat)
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345b_1", "12345b_2"),
		participant_id = rep("12345", 2),
		date = "2021-11-14",
		time = rep(c("16:40:01.223","16:40:01.323"), 2),
		x = rep(1.12345, 4),
		y = rep(-0.1234, 4),
		z = rep(0.123456, 4)
	)
	expect_identical(res, true)
})

test_that("periodic_gyroscope", {
	dat <- common_test("gyroscope",
										 list(
										 	body = list(
										 		id = "12345a",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		data = list(
										 			list(
										 				timestamp = "2021-11-14T16:40:01.223456Z",
										 				x = 1.12345,
										 				y = -0.1234,
										 				z = 0.123456
										 			),
										 			list(
										 				timestamp = "2021-11-14T16:40:01.323456Z",
										 				x = 1.12345,
										 				y = -0.1234,
										 				z = 0.123456
										 			)
										 		)
										 	)
										 ),
										 list(
										 	body = list(
										 		id = "12345b",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		data = list(
										 			list(
										 				timestamp = "2021-11-14T16:40:01.223456Z",
										 				x = 1.12345,
										 				y = -0.1234,
										 				z = 0.123456
										 			),
										 			list(
										 				timestamp = "2021-11-14T16:40:01.323456Z",
										 				x = 1.12345,
										 				y = -0.1234,
										 				z = 0.123456
										 			)
										 		)
										 	)
										 )
	)
	res <- gyroscope_fun(dat)
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345b_1", "12345b_2"),
		participant_id = rep("12345", 2),
		date = "2021-11-14",
		time = rep(c("16:40:01.223","16:40:01.323"), 2),
		x = rep(1.12345, 4),
		y = rep(-0.1234, 4),
		z = rep(0.123456, 4)
	)
	expect_identical(res, true)
})

test_that("activity", {
	unit_test("activity",
						confidence = 80,
						type = "WALKING")
})

test_that("air_quality", {
	unit_test("air_quality",
						air_quality_index = 30,
						air_quality_level = "GOOD",
						source = "IRCEL-CELINE - Belgian Interregional Environment Agency",
						place = "Aarschot, Belgium",
						latitude = 50.12345678901234,
						longitude = 4.12345678901234)
})

test_that("installed_apps", {
	dat <- common_test("apps",
										 list(
										 	body = list(
										 		id = "12345a",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		installed_apps = list("a", "b", "c")
										 	)
										 )
	)
	res <- apps_fun(dat)
	res_which <- which_sensor(dat, "apps")
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345a_3"),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:00",
		apps = c("a", "b", "c")
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

test_that("app_usage", {
	dat <- common_test("accelerometer",
										 list(
										 	body = list(
										 		id = "12345a",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		start = "2021-11-15T14:05:00.123456Z",
										 		end = "2021-11-15T14:35.00.123456Z",
										 		usage = list(
										 			a = 10,
										 			b = 5,
										 			c = 7
										 		)
										 	)
										 )
	)

	res <- app_usage_fun(dat)
	res_which <- which_sensor(dat, "app_usage")
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345a_3"),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:00",
		start = "2021-11-15T14:05:00.123456Z",
		end = "2021-11-15T14:35.00.123456Z",
		usage = c(10, 5, 7),
		app = c("a", "b", "c")
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

test_that("battery", {
	unit_test("battery",
						battery_level = 85,
						battery_status = "discharging")
})

test_that("bluetooth", {
	unit_test("bluetooth",
						scan_result = list(
							list(
								advertisement_name = "123abc",
								bluetooth_device_id = "def456",
								bluetooth_device_name = "789abc",
								bluetooth_device_type = "le",
								connectable = TRUE,
								tx_power_level = 50,
								rssi = -70
							),
							list(
								advertisement_name = "123abc",
								bluetooth_device_id = "def456",
								bluetooth_device_name = "789abc",
								bluetooth_device_type = "le",
								connectable = TRUE,
								tx_power_level = 50,
								rssi = -70
							)
						)
	)
})

test_that("calendar", {
	dat <- common_test("calendar",
										 list(
										 	body = list(
										 		id = "12345a",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		calendar_events = list(
										 			list(
										 				event_id = "2F65B0DC-71DE-FE06-0740-0C55292406ED:B74C2CB09DEBFF8A5BFDA",
										 				calendar_id = "C3EC959A-1385-4DB5-6F7F-CEC281D12D72",
										 				title = "cdd072aceed4665e7290ad11f8bf8b772edc0123",
										 				description = "631487392a0468f564736a0aba9659998c7462aa",
										 				start = "2021-11-14T13:00:00.000Z",
										 				end = "2021-11-14T13:30:00.000Z",
										 				all_day = FALSE,
										 				location = "Microsoft Teams Meeting",
										 				attendees = list(
										 					"a",
										 					"b"
										 				)
										 			),
										 			list(
										 				event_id = "2F65B0DC-71DE-FE06-0740-0C55292406ED:B74C2CB09DEBFF8A5BFDA",
										 				calendar_id = "C3EC959A-1385-4DB5-6F7F-CEC281D12D72",
										 				title = "cdd072aceed4665e7290ad11f8bf8b772edc0123",
										 				description = "631487392a0468f564736a0aba9659998c7462aa",
										 				start = "2021-11-14T13:00:00.000Z",
										 				end = "2021-11-14T13:30:00.000Z",
										 				all_day = FALSE,
										 				location = "Microsoft Teams Meeting",
										 				attendees = vector("list", 0)
										 			),
										 			list(
										 				event_id = "2F65B0DC-71DE-FE06-0740-0C55292406ED:B74C2CB09DEBFF8A5BFDA",
										 				calendar_id = "C3EC959A-1385-4DB5-6F7F-CEC281D12D72",
										 				title = "cdd072aceed4665e7290ad11f8bf8b772edc0123",
										 				description = "631487392a0468f564736a0aba9659998c7462aa",
										 				start = "2021-11-14T13:00:00.000Z",
										 				end = "2021-11-14T13:30:00.000Z",
										 				all_day = FALSE,
										 				location = "Microsoft Teams Meeting"
										 			)
										 		)
										 	)
										 )
	)

	res <- calendar_fun(dat)
	res_which <- which_sensor(dat, "calendar")
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345a_3"),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:00",
		event_id = "2F65B0DC-71DE-FE06-0740-0C55292406ED:B74C2CB09DEBFF8A5BFDA",
		calendar_id = "C3EC959A-1385-4DB5-6F7F-CEC281D12D72",
		title = "cdd072aceed4665e7290ad11f8bf8b772edc0123",
		description = "631487392a0468f564736a0aba9659998c7462aa",
		start = "2021-11-14T13:00:00.000Z",
		end = "2021-11-14T13:30:00.000Z",
		all_day = FALSE,
		location = "Microsoft Teams Meeting",
		attendees = c("a, b", NA, NA)
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

test_that("connectivity", {
	unit_test("connectivity",
						connectivity_status = "wifi")
})

test_that("device", {
	unit_test("device",
						platform = "IOS",
						device_id = "AB12CD34F5-12AA-34B5-67890-123AA45678901",
						hardware = "iPhone10,4",
						device_name = "Dory",
						device_manufacturer = "Apple",
						device_model = "iPhone",
						operating_system = "iOS")
})

test_that("error", {
	unit_test("error",
						message = "WeatherStation plugin returned null.")
})

test_that("keyboard", {
	expect_error(keyboard_fun(data.frame()), "Function not implemented")
})

test_that("light", {
	unit_test("light",
						mean_lux = 110,
						std_lux = 5,
						min_lux = 0,
						max_lux = 200)
})

test_that("location", {
	unit_test("location",
						latitude= "e13344b52140327c0ffcc9b419692a2b3fdc6babf73ced8dc7de68bac3528966600f74dadf33a880907719e27668fdbf7e38cc58ccef2007625192adcb3d1969f1",
						longitude = "773549b214208436841554c01423535062f5a248142bf8b7e1707e69fd351abb67045a330337d236482f54601724e4f7f3c5f53608bdafec7f01713b8f7e2422a6",
						altitude = 4.123456789012345,
						accuracy = 8.123456789012354,
						speed = 5.123456879012345,
						speed_accuracy = 0,
						heading = 123.456789012354567)
})

test_that("memory", {
	unit_test("memory",
						free_physical_memory = 12345678,
						free_virtual_memory = 123456789)
})

test_that("mobility", {
	unit_test("mobility",
						number_of_places = 1,
						location_variance = 0,
						entropy = 0,
						normalized_entropy = 0,
						home_stay = -1,
						distance_travelled = 0)
})

test_that("noise", {
	unit_test("noise",
						mean_decibel = 50.123456789,
						std_decibel = 10.123456789,
						min_decibel = 5.123456789,
						max_decibel = 80.123456789)
})

test_that("light", {
	unit_test("light",
						mean_lux = 110,
						std_lux = 5,
						min_lux = 0,
						max_lux = 200)
})

test_that("pedometer", {
	unit_test("pedometer",
						step_count = 12345)
})

test_that("screen", {
	unit_test("screen",
						screen_event = "SCREEN_OFF")
})

test_that("weather", {
	unit_test("weather",
						country = "BE",
						area_name = "Arrondissement Leuven",
						weather_main = "Clouds",
						weather_description = "broken clouds",
						sunrise = "2021-11-14T08:01:00.000",
						sunset = "2021-11-14T18:35:00.000",
						latitude = 50.1234,
						longitude = 4.1234,
						pressure = 1020,
						wind_speed = 6.25,
						wind_degree = 110,
						humidity = 82,
						cloudiness = 75,
						rain_last_hour = NA,
						rain_last_3hours = NA,
						snow_last_hour = NA,
						snow_last_3hours = NA,
						temperature = 13.123456789012345,
						temp_min = 12.123456789012345,
						temp_max = 14.123456789012345)
})

test_that("wifi", {
	unit_test("wifi",
						ssid = "84f4004087530a720eca1db5408ac0389b16b346",
						bssid = "52ba0ac0edffccac15f10f4e099208e22a603784",
						ip = "10.15.21.05")
})
