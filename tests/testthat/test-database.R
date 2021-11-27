# Tests for database.R

test_that("sensors-vec", {
	expect_vector(sensors, character(), size = 24)
})

test_that("create_db", {
	path <- system.file("testdata", package = "CARP")

	db <- create_db(path = path, "create.db")
	DBI::dbDisconnect(db)
	expect_true(file.exists(system.file("testdata", "create.db", package = "CARP")))

	# Test overwrite argument
	expect_error({
		db <- create_db(path = path, "create.db", overwrite = TRUE)
		DBI::dbDisconnect(db)
	}, NA)
	expect_error({
		db <- create_db(path = path, "create.db", overwrite = FALSE)
		DBI::dbDisconnect(db)
	}, "Database .+?(?=\\.db)\\.db already exists\\. Use overwrite = TRUE to overwrite\\.", perl = TRUE)

	file.remove(system.file("testdata", "create.db", package = "CARP"))
})

test_that("open_db", {
	expect_error(open_db(".", "foo.db"), "There is no such file")

	path <- system.file("testdata", package = "CARP")
	fake_db <- paste0(path, "/foo.db")

	# Create a new (non-CARP db)
	db <- DBI::dbConnect(RSQLite::SQLite(), fake_db)
	DBI::dbExecute(db, "CREATE TABLE foo(bar INTEGER, PRIMARY KEY(bar));")
	DBI::dbDisconnect(db)
	expect_error(open_db(NULL, fake_db), "Sorry, this does not appear to be a CARP database.")
	file.remove(system.file("testdata", "foo.db", package = "CARP"))

	db <- open_db(path, "test.db")
	expect_true(DBI::dbIsValid(db))
	DBI::dbDisconnect(db)
})

test_that("close_db", {
	db <- open_db(system.file("testdata", package = "CARP"), "test.db")
	expect_error(close_db(db), NA)
	expect_false(DBI::dbIsValid(db))
	expect_error(close_db(db) , NA) # Invalid db
	rm(db)
	expect_error(close_db(db), NA) # db does not exist
	db <- NULL
	expect_error(close_db(db), NA) # NULL db
})

test_that("index_db", {
	db <- create_db(system.file("testdata", package = "CARP"), "foo.db")
	expect_error(index_db(db), NA)
	expect_error(index_db(db), "index idx_accelerometer already exists")

	# Cleanup
	DBI::dbDisconnect(db)
	file.remove(system.file("testdata", "foo.db", package = "CARP"))
})

test_that("add_study", {
	db <- create_db(system.file("testdata", package = "CARP"), "foo.db")
	data <- data.frame(study_id = "12345", data_format = "CARP")
	expect_equal(add_study(db, data), 1)

	studies <- DBI::dbGetQuery(db, "SELECT * FROM Study")
	expect_equal(studies, data)
	expect_equal(add_study(db, data), 0)
	expect_equal(add_study(db, data.frame(foo = 1, bar = 2)), 0)

	# Cleanup
	DBI::dbDisconnect(db)
	file.remove(system.file("testdata", "foo.db", package = "CARP"))
})

test_that("add_participant", {
	db <- create_db(system.file("testdata", package = "CARP"), "foo.db")
	data <- data.frame(participant_id = "12345", study_id = "12345")
	DBI::dbExecute(db, "INSERT INTO Study VALUES('12345', 'CARP')")
	expect_equal(add_participant(db, data), 1)
	participants <- DBI::dbGetQuery(db, "SELECT * FROM Participant")
	expect_equal(participants, data)
	expect_equal(add_participant(db, data), 0)
	expect_equal(add_participant(db, data.frame(foo = 1, bar = 2)), 0)

	# Cleanup
	DBI::dbDisconnect(db)
	file.remove(system.file("testdata", "foo.db", package = "CARP"))
})

test_that("add_processed_file", {
	db <- create_db(system.file("testdata", package = "CARP"), "foo.db")
	data <- data.frame(file_name = "12345.json", participant_id = "12345", study_id = "12345")
	DBI::dbExecute(db, "INSERT INTO Study VALUES('12345', 'CARP')")
	DBI::dbExecute(db, "INSERT INTO Participant VALUES('12345', '12345')")
	expect_equal(add_processed_files(db, data), 1)
	files <- DBI::dbGetQuery(db, "SELECT * FROM ProcessedFiles")
	expect_equal(files, data)
	expect_equal(add_processed_files(db, data), 0)
	expect_equal(add_processed_files(db, data.frame(foo = 1, bar = 2)), 0)

	# Cleanup
	DBI::dbDisconnect(db)
	file.remove(system.file("testdata", "foo.db", package = "CARP"))
})

test_that("clear_sensors_db", {
	path <- system.file("testdata", package = "CARP")
	db <- import(path, dbname = "foo.db", recursive = FALSE)
	db <- open_db(path, "foo.db")
	original <- get_nrows(db)
	res <- clear_sensors_db(db)
	expect_type(res, "list")
	expect_length(res, length(sensors))
	expect_identical(Reduce(`+`, res), sum(original))
	expect_identical(sum(get_nrows(db)), 0L)
	close_db(db)
	file.remove(system.file("testdata", "foo.db", package = "CARP"))
})

test_that("get_processed_files", {
	db <- open_db(system.file("testdata", package = "CARP"), "test.db")
	res <- get_processed_files(db)
	true <- data.frame(
		file_name = c("empty.json", "test.json"),
		participant_id = c("empty.json", "12345"),
		study_id = c("-1", "test-study")
	)
	expect_identical(res, true)
	DBI::dbDisconnect(db)
})

test_that("get_participants", {
	db <- open_db(system.file("testdata", package = "CARP"), "test.db")
	res <- get_participants(db)
	res_lazy <- get_participants(db, lazy = TRUE)
	true <- data.frame(
		participant_id = c("empty.json", "12345"),
		study_id = c("-1", "test-study")
	)
	expect_identical(res, true)
	expect_s3_class(res_lazy, "tbl_SQLiteConnection")
	DBI::dbDisconnect(db)
})

test_that("get_study", {
	db <- open_db(system.file("testdata", package = "CARP"), "test.db")
	res <- get_studies(db)
	res_lazy <- get_studies(db, lazy = TRUE)
	true <- data.frame(
		study_id = c("-1", "test-study"),
		data_format = c(NA, "carp")
	)
	expect_identical(res, true)
	expect_s3_class(res_lazy, "tbl_SQLiteConnection")
	DBI::dbDisconnect(db)
})

test_that("get_nrows", {
	db <- open_db(system.file("testdata", package = "CARP"), "test.db")
	expect_vector(get_nrows(db), integer(), length(sensors))
	DBI::dbDisconnect(db)
})
