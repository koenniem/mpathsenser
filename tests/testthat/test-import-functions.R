# Tests for import_functions.R

# common_test ===========
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

# unit_test ===========
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

test_that("save2db", {
	db <- create_db(system.file("testdata", package = "CARP"), "foo.db")
	DBI::dbExecute(db, "INSERT INTO Study VALUES('12345', 'CARP')")
	DBI::dbExecute(db, "INSERT INTO Participant VALUES('12345', '12345')")
	db_size <- file.size(system.file("testdata", "foo.db", package = "CARP"))
	expect_error(save2db(db, "Accelerometer", data.frame(
		measurement_id = paste0("12345_", 1:100),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:01.123",
		x = 0.123456789,
		y = 0.123456789,
		z = 9.123456789
	)), NA)
	db_size2 <- file.size(system.file("testdata", "foo.db", package = "CARP"))
	expect_gt(db_size2, db_size)

	# Entry with the same ID should simply be skipped and give on error
	expect_error(save2db(db, "Accelerometer", data.frame(
		measurement_id = paste0("12345_", 1:100),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:01.123",
		x = 0.123456789,
		y = 0.123456789,
		z = 9.123456789
	)), NA)
	db_size3 <- file.size(system.file("testdata", "foo.db", package = "CARP"))
	expect_identical(db_size2, db_size3)
	expect_identical(DBI::dbGetQuery(db, "SELECT COUNT(*) FROM Accelerometer")[[1]], 100L)

	# Cleanup
	DBI::dbDisconnect(db)
	file.remove(system.file("testdata", "foo.db", package = "CARP"))
})

# safe_data_frame  ===========
test_that("safe_data_frame", {
	dat <- data.frame(a = 1, b = NA)
	res <- safe_data_frame(a = dat$a, b = dat$b, c = dat$c)
	true <- data.frame(a = 1, b = NA, c = NA)
	expect_identical(res, true)
})

# safe_tibble ===========
test_that("safe_tibble", {
	dat <- tibble::tibble(a = 1, b = NA, c = vector("list", 1))
	dat2 <- tibble::tibble(a = 1, b = NA, c = vector("list", 0))
	res <- safe_tibble(a = dat$a, b = dat$b, c = dat$c, d = dat$d)
	res2 <- safe_tibble(a = dat$a, b = dat$b, c = dat$c, d = dat$d)
	true <- tibble::tibble(a = 1, b = NA, c = NA, d = NA)
	expect_identical(res, true)
	expect_identical(res2, true)
})

# Accelerometer ===========
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
										 ),
										 list(
										 	body = list(
										 		id = "12345b",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		x = NA,
										 		y = NA,
										 		z = NA
										 	)
										 )
	)

	res <- accelerometer_fun(dat)
	res_which <- which_sensor(dat, "accelerometer")
	true <- data.frame(
		measurement_id = c("12345a", "12345b"),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:01.123",
		x = c(0.123456789, NA),
		y = c(0.123456789, NA),
		z = c(9.123456789, NA)
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

# Gyroscope ===========
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
										 ),
										 list(
										 	body = list(
										 		id = "12345b",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		x = NA,
										 		y = NA,
										 		z = NA
										 	)
										 )
	)

	res <- gyroscope_fun(dat)
	res_which <- which_sensor(dat, "gyroscope")
	true <- data.frame(
		measurement_id = c("12345a", "12345b"),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:01.123",
		x = c(0.123456789, NA),
		y = c(0.123456789, NA),
		z = c(9.123456789, NA)
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

# Periodic accelerometer ===========
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
										 ),
										 list(
										 	body = list(
										 		id = "12345c",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		data = list(
										 			list(
										 				timestamp = "2021-11-14T16:40:01.223456Z",
										 				x = NA,
										 				y = NA,
										 				z = NA
										 			),
										 			list(
										 				timestamp = "2021-11-14T16:40:01.323456Z",
										 				x = NA,
										 				y = NA,
										 				z = NA
										 			)
										 		)
										 	)
										 )
	)
	res <- accelerometer_fun(dat)
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345b_1", "12345b_2", "12345c_1", "12345c_2"),
		participant_id = rep("12345", 3),
		date = "2021-11-14",
		time = rep(c("16:40:01.223","16:40:01.323"), 3),
		x = c(rep(1.12345, 4), NA, NA),
		y = c(rep(-0.1234, 4), NA, NA),
		z = c(rep(0.123456, 4), NA, NA)
	)
	expect_identical(res, true)
})

# Periodic gyroscope ===========
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
										 ),
										 list(
										 	body = list(
										 		id = "12345c",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		data = list(
										 			list(
										 				timestamp = "2021-11-14T16:40:01.223456Z",
										 				x = NA,
										 				y = NA,
										 				z = NA
										 			),
										 			list(
										 				timestamp = "2021-11-14T16:40:01.323456Z",
										 				x = NA,
										 				y = NA,
										 				z = NA
										 			)
										 		)
										 	)
										 )
	)
	res <- gyroscope_fun(dat)
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345b_1", "12345b_2", "12345c_1", "12345c_2"),
		participant_id = rep("12345", 3),
		date = "2021-11-14",
		time = rep(c("16:40:01.223","16:40:01.323"), 3),
		x = c(rep(1.12345, 4), NA, NA),
		y = c(rep(-0.1234, 4), NA, NA),
		z = c(rep(0.123456, 4), NA, NA)
	)
	expect_identical(res, true)
})

# Activity ===========
test_that("activity", {
	unit_test("activity",
						confidence = 80,
						type = "WALKING")
	unit_test("activity",
						confidence = NA,
						type = NA)
})

# Air Quality ===========
test_that("air_quality", {
	unit_test("air_quality",
						air_quality_index = 30,
						air_quality_level = "GOOD",
						source = "IRCEL-CELINE - Belgian Interregional Environment Agency",
						place = "Aarschot, Belgium",
						latitude = 50.12345678901234,
						longitude = 4.12345678901234)
	unit_test("air_quality",
						air_quality_index = NA,
						air_quality_level = NA,
						source = NA,
						place = NA,
						latitude = NA,
						longitude = NA)
})

# Installed Apps ===========
test_that("installed_apps", {
	dat <- common_test("apps",
										 list(
										 	body = list(
										 		id = "12345a",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		installed_apps = list("a", "b", "c")
										 	)),
										 	list(
										 		body = list(
										 		id = "12345a",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		installed_apps = list()
										 	))
	)
	res <- apps_fun(dat)
	res_which <- which_sensor(dat, "apps")
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345a_3", "12345a_4"),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:00",
		app = c("a", "b", "c", NA)
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

# App usage ===========
test_that("app_usage", {
	dat <- common_test("app_usage",
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
										 ),
										 list(
										 	body = list(
										 		id = "12345b",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		start = "2021-11-15T14:05:00.123456Z",
										 		end = "2021-11-15T14:35.00.123456Z",
										 		usage = list(
										 			a = 10,
										 			b = 5,
										 			c = 7
										 		)
										 	)
										 ),
										 list(
										 	body = list(
										 		id = "12345c",
										 		timestamp = "2021-11-14T16:40:01.123456Z",
										 		start = NA,
										 		end = NA,
										 		usage = list()
										 	)
										 )
	)

	res <- app_usage_fun(dat)
	res_which <- which_sensor(dat, "app_usage")
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345a_3", "12345b_1", "12345b_2", "12345b_3", "12345c_1"),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:00",
		start = c(rep("2021-11-15T14:05:00.123456Z", 6), NA),
		end = c(rep("2021-11-15T14:35.00.123456Z", 6), NA),
		usage = c(rep(c(10, 5, 7), 2), NA),
		app = c(rep(c("a", "b", "c"), 2), "")
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

# Battery ===========
test_that("battery", {
	unit_test("battery",
						battery_level = 85,
						battery_status = "discharging")
	unit_test("battery",
						battery_level = NA,
						battery_status = NA)
})

# Bluetooth ===========
test_that("bluetooth", {
	dat <- common_test("bluetooth",
							list(
								body = list(
									id = "12345a",
									timestamp = "2021-11-14T16:40:00.123456Z",
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
							),
							list(
								body = list(
									id = "12345b",
									timestamp = "2021-11-14T16:40:00.123456Z",
									scan_result = list(
										list(
											advertisement_name = NA,
											bluetooth_device_id = NA,
											bluetooth_device_name = NA,
											bluetooth_device_type = NA,
											connectable = NA,
											tx_power_level = NA,
											rssi = NA
										)
									)
								)
							),
							list(
								body = list(
									id = "12345c",
									timestamp = "2021-11-14T16:40:00.123456Z",
									scan_result = list()
								)
							)
	)

	res <- bluetooth_fun(dat)
	res_which <- which_sensor(dat, "bluetooth")
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345b_1", "12345c_1"),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:00",
		advertisement_name = c("123abc", "123abc", NA, NA),
		bluetooth_device_id = c("def456", "def456", NA, NA),
		bluetooth_device_name = c("789abc", "789abc", NA, NA),
		bluetooth_device_type = c("le", "le", NA, NA),
		connectable = c(TRUE, TRUE, NA, NA),
		rssi = c(-70, -70, NA, NA),
		tx_power_level = c(50, 50, NA, NA)
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

# Calendar ===========
test_that("calendar", {
	dat <- common_test("calendar",
										 list(
										 	body = list(
										 		id = "12345a",
										 		calendar_events = list(
										 			list(
										 				event_id = "8752301D-3AE5-A7FF-6822-867418B8CC3E:F81E8964C1BC1C48365F9",
										 				calendar_id = "45ED76B4-87A1-D7E0-FA93-A7A1F64CF3E7",
										 				title = "96475fc78435bef84354fc05dd185ac944c5c3c1",
										 				description = "81af04ac942e1bbf4f3c638b086395dfabe2164a",
										 				start = "2021-11-14T13:00:00.000Z",
										 				end = "2021-11-14T13:30:00.000Z",
										 				all_day = FALSE,
										 				location = "Microsoft Teams Meeting",
										 				attendees = list(
										 					"a",
										 					"b",
										 					NA
										 				)
										 			),
										 			list(
										 				event_id = "8752301D-3AE5-A7FF-6822-867418B8CC3E:F81E8964C1BC1C48365F9",
										 				calendar_id = "45ED76B4-87A1-D7E0-FA93-A7A1F64CF3E7",
										 				title = "96475fc78435bef84354fc05dd185ac944c5c3c1",
										 				description = "81af04ac942e1bbf4f3c638b086395dfabe2164a",
										 				start = "2021-11-14T13:00:00.000Z",
										 				end = "2021-11-14T13:30:00.000Z",
										 				all_day = FALSE,
										 				location = "Microsoft Teams Meeting",
										 				attendees = vector("list", 0)
										 			),
										 			list(
										 				event_id = "8752301D-3AE5-A7FF-6822-867418B8CC3E:F81E8964C1BC1C48365F9",
										 				calendar_id = "45ED76B4-87A1-D7E0-FA93-A7A1F64CF3E7",
										 				title = "96475fc78435bef84354fc05dd185ac944c5c3c1",
										 				description = "81af04ac942e1bbf4f3c638b086395dfabe2164a",
										 				start = "2021-11-14T13:00:00.000Z",
										 				end = "2021-11-14T13:30:00.000Z",
										 				all_day = FALSE,
										 				location = "Microsoft Teams Meeting"
										 			)
										 		)
										 	)
										 ),
										 list(
										 	body = list(
										 		id = "12345b",
										 		calendar_events = list()
										 	)
										 )
	)

	res <- calendar_fun(dat)
	res_which <- which_sensor(dat, "calendar")
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345a_3", "12345b_1"),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:00",
		event_id = c(rep("8752301D-3AE5-A7FF-6822-867418B8CC3E:F81E8964C1BC1C48365F9", 3), NA),
		calendar_id = c(rep("45ED76B4-87A1-D7E0-FA93-A7A1F64CF3E7", 3), NA),
		title = c(rep("96475fc78435bef84354fc05dd185ac944c5c3c1", 3), NA),
		description = c(rep("81af04ac942e1bbf4f3c638b086395dfabe2164a", 3), NA),
		start = c(rep("2021-11-14T13:00:00.000Z", 3), NA),
		end = c(rep("2021-11-14T13:30:00.000Z", 3), NA),
		all_day = c(rep(FALSE, 3), NA),
		location = c(rep("Microsoft Teams Meeting", 3), NA),
		attendees = c("a, b, NA", NA, NA, NA)
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

# Connectivity ===========
test_that("connectivity", {
	unit_test("connectivity",
						connectivity_status = "wifi")
	unit_test("connectivity",
						connectivity_status = NA)
})


# Device ===========
test_that("device", {
	unit_test("device",
						platform = "IOS",
						device_id = "AB12CD34F5-12AA-34B5-67890-123AA45678901",
						hardware = "iPhone10,4",
						device_name = "Dory",
						device_manufacturer = "Apple",
						device_model = "iPhone",
						operating_system = "iOS")
	unit_test("device",
						platform = NA,
						device_id = NA,
						hardware = NA,
						device_name = NA,
						device_manufacturer = NA,
						device_model = NA,
						operating_system = NA)
})

# Error ===========
test_that("error", {
	unit_test("error",
						message = "WeatherStation plugin returned null.")
	unit_test("error",
						message = NA)
})

# Geofence ===========
test_that("geofence", {
	unit_test("geofence",
						center = "ed1007174d0668bb262d702652f3b3f81d6be2d6e08db967810f8d128a0042014cc8e04792d8cdfe51da2158fd3efbedaf23fc02da9e5fea4c896ecb81c81672bf",
						dwell = 123456,
						name = "Home",
						radius = 50,
						state = "ENTER")
	unit_test("geofence",
						center = NA,
						dwell = NA,
						name = NA,
						radius = NA,
						state = NA)
})

# Keyboard ===========
test_that("keyboard", {
	expect_error(keyboard_fun(data.frame()), "Function not implemented")
	expect_error(which_sensor(data.frame(), "keyboard"), "Function not implemented")
})

# Light ===========
test_that("light", {
	unit_test("light",
						mean_lux = 110,
						std_lux = 5,
						min_lux = 0,
						max_lux = 200)
	unit_test("light",
						mean_lux = NA,
						std_lux = NA,
						min_lux = NA,
						max_lux = NA)
})

# Location ===========
test_that("location", {
	unit_test("location",
						latitude= "69daf931cc38118ce450d5bfd9437324d1ad9b463e22d97a3ec5338c5de1f3a3a5bfc163eabdc8b0c99320b0c6fbc6ca4be89dac7db9d1f1d86fb1776534dddc89",
						longitude = "1704226c422d7182cc960e6630b5f69a2c7ce8de2e673574fc7fb89fbab4e2c2d848aa30920abb2396de254666213f087f3c929da0b57b7257a58dc166b1ef1db1",
						altitude = 4.123456789012345,
						accuracy = 8.123456789012354,
						speed = 5.123456879012345,
						speed_accuracy = 0,
						heading = 123.456789012354567)
	unit_test("location",
						latitude= NA,
						longitude = NA,
						altitude = NA,
						accuracy = NA,
						speed = NA,
						speed_accuracy = NA,
						heading = NA)
})

# Memory  ===========
test_that("memory", {
	unit_test("memory",
						free_physical_memory = 12345678,
						free_virtual_memory = 123456789)
	unit_test("memory",
						free_physical_memory = NA,
						free_virtual_memory = NA)
})

# Mobility ===========
test_that("mobility", {
	unit_test("mobility",
						number_of_places = 1,
						location_variance = 0,
						entropy = 0,
						normalized_entropy = 0,
						home_stay = -1,
						distance_travelled = 0)
	unit_test("mobility",
						number_of_places = NA,
						location_variance = NA,
						entropy = NA,
						normalized_entropy = NA,
						home_stay = NA,
						distance_travelled = NA)
})

# Noise ===========
test_that("noise", {
	unit_test("noise",
						mean_decibel = 50.123456789,
						std_decibel = 10.123456789,
						min_decibel = 5.123456789,
						max_decibel = 80.123456789)
	unit_test("noise",
						mean_decibel = NA,
						std_decibel = NA,
						min_decibel = NA,
						max_decibel = NA)
})

# Pedometer ===========
test_that("pedometer", {
	unit_test("pedometer",
						step_count = 12345)
	unit_test("pedometer",
						step_count = NA)
})

# Screen ===========
test_that("screen", {
	unit_test("screen",
						screen_event = "SCREEN_OFF")
	unit_test("screen",
						screen_event = NA)
})

# Text message ===========
test_that("text_message", {
	dat <- common_test("text_message",
										 list(
										 	body = list(
										 		id = "12345a",
										 		start_time = "2021-11-14T16:40:01.123456Z",
										 		text_message = list(
										 		list(
										 			address = "123",
										 			body = "abc",
										 			date = "2021-11-13",
										 			date_sent = "2021-11-13T13:00:00.123456T",
										 			is_read = TRUE,
										 			kind = "outgoing",
										 			size = 12345,
										 			state = "sent"
										 		),
										 		list(
										 			address = "456",
										 			body = "def",
										 			date = "2021-11-12",
										 			date_sent = "2021-11-12T14:00:00.123456T",
										 			is_read = FALSE,
										 			kind = "incoming",
										 			size = 67890,
										 			state = "received"
										 		)
										 	)
										 )
										 ),
										 list(
										 	body = list(
										 		id ="12345b",
										 		start_time = "2021-11-14T16:40:01.123456Z",
										 		text_message = list()
										 	)
										 )
	)
	res <- text_message_fun(dat)
	res_which <- which_sensor(dat, "text_message")
	true <- data.frame(
		measurement_id = c("12345a_1", "12345a_2", "12345b_1"),
		participant_id = "12345",
		date = "2021-11-14",
		time = "16:40:00",
		address = c("123", "456", NA),
		body = c("abc", "def", NA),
		text_date = c("2021-11-13", "2021-11-12", NA),
		date_sent = c("2021-11-13T13:00:00.123456T", "2021-11-12T14:00:00.123456T", NA),
		is_read = c(TRUE, FALSE, NA),
		kind = c("outgoing", "incoming", NA),
		size = c(12345, 67890, NA),
		state = c("sent", "received", NA)
	)

	expect_identical(res, res_which)
	expect_identical(res, true)
	expect_identical(res_which, true)
})

# Weather ===========
test_that("weather", {
	unit_test("weather",
						country = "BE",
						area_name = "Arrondissement Leuven",
						weather_main = "Clouds",
						weather_description = "broken clouds",
						sunrise = "2021-11-14T08:00:00.000",
						sunset = "2021-11-14T19:00:00.000",
						latitude = 50.1234,
						longitude = 4.1234,
						pressure = 1020,
						wind_speed = 5.75,
						wind_degree = 140,
						humidity = 85,
						cloudiness = 77,
						rain_last_hour = NA,
						rain_last_3hours = NA,
						snow_last_hour = NA,
						snow_last_3hours = NA,
						temperature = 13.123456789012345,
						temp_min = 12.123456789012345,
						temp_max = 14.123456789012345)
	unit_test("weather",
						country = NA,
						area_name = NA,
						weather_main = NA,
						weather_description = NA,
						sunrise = NA,
						sunset = NA,
						latitude = NA,
						longitude = NA,
						pressure = NA,
						wind_speed = NA,
						wind_degree = NA,
						humidity = NA,
						cloudiness = NA,
						rain_last_hour = NA,
						rain_last_3hours = NA,
						snow_last_hour = NA,
						snow_last_3hours = NA,
						temperature = NA,
						temp_min = NA,
						temp_max = NA)
})

# Wifi ===========
test_that("wifi", {
	unit_test("wifi",
						ssid = "318e527d52bb2f775c79d84a5c888614ca772b30",
						bssid = "e412411ff32dcf879275b33882643ee2d328a56a",
						ip = "10.11.31.06")
	unit_test("wifi",
						ssid = NA,
						bssid = NA,
						ip = NA)
})
