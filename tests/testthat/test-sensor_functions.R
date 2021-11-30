# Tests for sensor_functions.R

## get_data ===============
test_that("get_data", {
	db <- open_db(system.file("testdata", package = "CARP"), "test.db")
	res <- get_data(db, "Activity", "12345", "2021-11-14", "2021-11-14") %>%
		dplyr::collect()
	expect_identical(
		res,
		tibble::tibble(
			measurement_id = c("fbf85cd7-6d37-53a8-5c44-ad8fe13ef7ac",
												 "ef96364c-d1f4-5f73-ce40-277f078e3d0f",
												 "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"),
			participant_id = "12345",
			date = "2021-11-14",
			time = c("13:59:59", "14:00:00", "14:00:01"),
			confidence = c(NA, 100L, 99L),
			type = c(NA, "WALKING", "STILL")
		)
	)

	# Only a start date
	res <- get_data(db, "Device", "12345", "2021-11-14") %>%
		dplyr::collect()
	expect_identical(
		res,
		tibble::tibble(
			measurement_id = c("ac1230a8-ed5f-4ded-7fca-7693a5ab4124",
												 "138b9204-a313-96f3-89de-42bc2ac9d1e9"),
			participant_id = "12345",
			date = "2021-11-14",
			time = c("13:00:00", "14:01:00"),
			device_id = c("QKQ1.200628.002", NA),
			hardware = c("qcom", NA),
			device_name = c("gauguin", NA),
			device_manufacturer = c("Xiaomi", NA),
			device_model = c("M2007J17G", NA),
			operating_system = c("REL", NA),
			platform = c("Android", NA)
		)
	)

	# Only an end date
	res <- get_data(db, "Device", "12345", end_date = "2021-11-13") %>%
		dplyr::collect()
	expect_identical(
		res,
		tibble::tibble(
			measurement_id = "bce3c272-3e06-4c84-f533-5bbbeaaac049",
			participant_id = "12345",
			date = "2021-11-13",
			time = "13:00:00",
			device_id = "QKQ1.200628.002",
			hardware = "qcom",
			device_name ="gauguin",
			device_manufacturer = "Xiaomi",
			device_model = "M2007J17G",
			operating_system = "REL",
			platform = "Android"
		)
	)

	DBI::dbDisconnect(db)
	expect_error(get_data(db, "Activity"), "Database connection is not valid")
})

## first_date ===============
test_that("first_date", {
	db <- open_db(system.file("testdata", package = "CARP"), "test.db")
	expect_identical(first_date(db, "Device"), "2021-11-13")
	expect_identical(first_date(db, "Device", "12345"), "2021-11-13")
	DBI::dbDisconnect(db)
})

## last_date ===============
test_that("last_date", {
	db <- open_db(system.file("testdata", package = "CARP"), "test.db")
	expect_identical(last_date(db, "Device"), "2021-11-14")
	expect_identical(last_date(db, "Device", "12345"), "2021-11-14")
	DBI::dbDisconnect(db)
})

## link ===============
test_that("link", {
	dat1 <- data.frame(
		time = rep(c(as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
						 as.POSIXct("2021-11-14 15:00:00")), 2),
		participant_id = c(rep("12345", 3), rep("23456", 3)),
		item_one = rep(c(40, 50, 60), 2)
	)

	dat2 <- data.frame(
		time = rep(seq.POSIXt(as.POSIXct("2021-11-14 12:50:00"), by = "5 min", length.out = 30), 2),
		participant_id = c(rep("12345", 30), rep("23456", 30)),
		x = rep(1:30, 2)
	)

	res <- link(dat1, dat2, "participant_id", -1800)
	true <- tibble::tibble(
		time = rep(c(as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
								 as.POSIXct("2021-11-14 15:00:00")), 2),
		participant_id = c(rep("12345", 3), rep("23456", 3)),
		item_one = rep(c(40, 50, 60), 2),
		data = rep(list(
			tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 12:50:00"),
																			 length.out = 3, by = "5 min"),
										 x = 1:3),
			tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 13:30:00"),
																			 length.out = 7, by = "5 min"),
										 x = 9:15),
			tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 14:30:00"),
																			 length.out = 7, by = "5 min"),
										 x = 21:27)
		), 2)
	)
	expect_equal(res, true)

	res <- link(dat1, dat2, "participant_id", 1800)
	true <- tibble::tibble(
		time = rep(c(as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
						 as.POSIXct("2021-11-14 15:00:00")), 2),
		participant_id = c(rep("12345", 3), rep("23456", 3)),
		item_one = rep(c(40, 50, 60), 2),
		data =  rep(list(
			tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 13:00:00"),
																			 length.out = 7, by = "5 min"),
										 x = 3:9),
			tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 14:00:00"),
																			 length.out = 7, by = "5 min"),
										 x = 15:21),
			tibble::tibble(time = seq.POSIXt(from = as.POSIXct("2021-11-14 15:00:00"),
																			 length.out = 4, by = "5 min"),
										 x = 27:30)
		), 2)
	)
	expect_equal(res, true)

	expect_error(link(1, dat2, "participant_id", -1800), "x must be a data frame")
	expect_error(link(dat1, 1, "participant_id", -1800), "y must be a data frame")
	expect_error(link(dat1, dat2, 12345, -1800),
							 "by must be a character vector of variables to join by")

	expect_error(link(dplyr::mutate(dat1, time = as.character(time)), dat2, offset = -1800),
							 "column 'time' in x must be a POSIXct")
	expect_error(link(dat1, dplyr::mutate(dat2, time = as.character(time)), offset = -1800),
							 "column 'time' in y must be a POSIXct")
	expect_error(link(dat1, dat2, offset = TRUE),
							 "offset must be a character vector, numeric vector, or a period")
	expect_error(link(dat1, dat2, offset = "1800"),
							 paste("Invalid offset specified\\. Try something like '30 minutes' or",
							 "lubridate::minutes\\(30\\)\\. Note that negative values do not work when",
							 "specifying character vectors, instead use minutes\\(-30\\) or -1800\\."))
	expect_error(link(dplyr::select(dat1, -time), dat2, offset = -1800),
							 "column 'time' must be present in both x and y")
	expect_error(link(dat1, dplyr::select(dat2, -time), offset = -1800),
							 "column 'time' must be present in both x and y")
})

## link2 ===============
test_that("link2", {
	path <- system.file("testdata", package = "CARP")
	db <- open_db(path, "test.db")
	dat1 <- data.frame(
		time = c(as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
						 as.POSIXct("2021-11-14 15:00:00")),
		participant_id = "12345",
		item_one = c(40, 50, 60)
	)

	# Check basic functionality
	res <- link2(db, "Activity", "Connectivity", offset = 1800)
	true <- tibble::tibble(
		measurement_id = c("fbf85cd7-6d37-53a8-5c44-ad8fe13ef7ac",
											 "ef96364c-d1f4-5f73-ce40-277f078e3d0f",
											 "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"),
		participant_id = "12345",
		time = as.POSIXct(c("2021-11-14 13:59:59", "2021-11-14 14:00:00", "2021-11-14 14:00:01")),
		confidence = c(NA, 100L, 99L),
		type = c(NA, "WALKING", "STILL"),
		data = list(
			tibble::tibble(
				measurement_id = c("27a5777a-ec41-80de-afa4-d2e7f6b02fcf",
													 "2d430c2a-5b16-1dce-0e2f-c049c44e3729"),
				time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:01:00")),
				connectivity_status = c("wifi", NA)
			),
			tibble::tibble(
				measurement_id = c("27a5777a-ec41-80de-afa4-d2e7f6b02fcf",
													 "2d430c2a-5b16-1dce-0e2f-c049c44e3729"),
				time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:01:00")),
				connectivity_status = c("wifi", NA)
			),
			tibble::tibble(
				measurement_id = "2d430c2a-5b16-1dce-0e2f-c049c44e3729",
				time = as.POSIXct("2021-11-14 14:01:00"),
				connectivity_status = NA_character_
			)
		)
	)
	expect_identical(res, true)

	# Check reverse
	res <- link2(db, "Activity", "Connectivity", offset = 1800, reverse = TRUE)
	true <- tibble::tibble(
		measurement_id = c("27a5777a-ec41-80de-afa4-d2e7f6b02fcf",
											 "2d430c2a-5b16-1dce-0e2f-c049c44e3729"),
		participant_id = "12345",
		time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:01:00")),
		connectivity_status = c("wifi", NA),
		data = list(
			tibble::tibble(
				measurement_id = c("ef96364c-d1f4-5f73-ce40-277f078e3d0f",
													 "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"),
				time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:00:01")),
				confidence = c(100L, 99L),
				type = c("WALKING", "STILL"),
			),
			tibble::tibble(
				measurement_id = character(0),
				time = structure(numeric(0), tzone = "", class = c("POSIXct", "POSIXt")),
				confidence = integer(0),
				type = character(0)
			)
		)
	)
	expect_identical(res, true)

	# Check with external data
	res <- link2(db, "Activity", external = dat1, offset = 1800)
	true <- tibble::tibble(
		dat1,
		data = list(
			tibble::tibble(measurement_id = character(0),
										 time = structure(numeric(0), tzone = "", class = c("POSIXct", "POSIXt")),
										 confidence = integer(0L),
										 type = character(0)),
			tibble::tibble(measurement_id = c("ef96364c-d1f4-5f73-ce40-277f078e3d0f",
																				"5ba54e77-4bcf-c8d1-17ff-71b9ed908897"),
										 time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:00:01")),
										 confidence = c(100L, 99L),
										 type = c("WALKING", "STILL")),
			tibble::tibble(measurement_id = character(0),
										 time = structure(numeric(0), tzone = "", class = c("POSIXct", "POSIXt")),
										 confidence = integer(0),
										 type = character(0))
			)
	)
	expect_identical(res, true)

	expect_error(link2(db, "Activity", 1:10, -1800), "sensor_two must be a character vector")
	expect_error(link2(db, "Activity", offset = -1800, external = "Bluetooth"),
										 "external must be a data frame")
	expect_error(link2(db, "Activity", "Bluetooth", -1800, external = dat1),
							 "only a second sensor or an external data frame can be supplied, but not both")
	expect_error(link2(db, "Activity", offset = -1800),
							 "either a second sensor or an external data frame must be supplied")
	DBI::dbDisconnect(db)
	expect_error(link2(db, "Activity", "Bluetooth", -1800), "Database connection is not valid")

	# Check if ignore_large works
	db <- create_db(path, "big.db")
	add_study(db, data.frame(study_id = "test-study", data_format = "CARP"))
	add_participant(db, data.frame(study_id = "test-study", participant_id = "12345"))

	sens_value <- seq.int(0, 10, length.out = 50001)
	time_value <- seq.POSIXt(as.POSIXct("2021-11-14 14:00:00.000", format = "%F %H:%M:%OS"), by = "sec", length.out = 50001)
	acc <- data.frame(
		measurement_id = paste0("id_", 1:50001),
		participant_id = "12345",
		date = "2021-11-14",
		time = strftime(time_value, format = "%H:%M:%OS3"),
		x = sens_value,
		y = sens_value,
		z = sens_value
	)

	DBI::dbWriteTable(db, "Accelerometer", acc, overwrite = TRUE)
	DBI::dbWriteTable(db, "Gyroscope", acc, overwrite = TRUE)
	expect_error(link2(db, "Accelerometer", "Gyroscope", offset = 30),
							 "the total number of rows is higher than 100000. Use ignore_large = TRUE to continue")
	expect_error(link2(db, "Accelerometer", "Gyroscope", offset = 30, ignore_large = TRUE),
							 "x and y are identical")
	DBI::dbDisconnect(db)
	file.remove(file.path(path, "big.db"))
})


## get_installed_apps ===============
test_that("get_installed_apps", {
	db <- open_db(system.file("testdata", package = "CARP"), "test.db")
	res <- get_installed_apps(db, "12345")
	true <- tibble::tibble(app = c("BBC News",
																 "Calculator",
																 "Clock",
																 "Google News",
																 "Google PDF Viewer",
																 "Google Play Books",
																 "Google Play Games",
																 "Google Play Movies & TV",
																 "Google Play Music",
																 "Google Play Services for AR",
																 "Google VR Services",
																 "Home",
																 "Mobile Device Information Provider",
																 "Photos",
																 "WhatsApp",
																 "m-Path Sense"))
	expect_identical(res, true)
	DBI::dbDisconnect(db)
})
