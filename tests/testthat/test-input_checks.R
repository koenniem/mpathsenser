test_that("ensure_suggested_package", {
  expect_error(
    ensure_suggested_package("base"),
    NA
  )

  expect_error(
    ensure_suggested_package("foo"),
    "Package `foo` is needed for this function to work."
  )
})

test_that("check_db", {
  expect_error(
    check_db(NULL, arg = "db"),
    "Database connection `db` must not be NULL."
  )
  expect_error(
    check_db(NULL, allow_null = TRUE),
    NA
  )
  expect_error(
    check_db(NULL, arg = "foo"),
    "Database connection `foo` must not be NULL."
  )

  expect_error(
    check_db("foo", arg = "db"),
    "Argument `db` is not a database connection."
  )
  expect_error(
    check_db("foo", allow_null = TRUE, arg = "foo"),
    "Argument `foo` is not a database connection."
  )

  db <- create_db(NULL, tempfile())
  expect_true(check_db(db))
  expect_true(check_db(db, arg = "foo"))
  expect_true(check_db(db, allow_null = TRUE, arg = "foo"))
  dbDisconnect(db)

  expect_error(
    check_db(db, arg = "db"),
    "Database connection `db` is not valid."
  )
  expect_error(
    check_db(db, allow_null = TRUE, arg = "db"),
    "Database connection `db` is not valid."
  )
  unlink(db@driver@dbdir)
  expect_error(
    check_db(db, arg = "foo"),
    "Database connection `foo` is not valid."
  )
})

test_that("check_arg", {
  # Test NULL behaviour
  expect_error(
    check_arg(NULL, type = "character", arg = "x"),
    "Argument `x` must be a character."
  )

  expect_error(
    check_arg(NULL, type = "character", n = 1, arg = "x"),
    "Argument `x` must be a character of length 1."
  )

  expect_error(
    check_arg(NULL, type = "character", allow_null = TRUE),
    NA
  )

  # Test whether cases work
  expect_true(check_arg("foo", type = "character"))
  expect_true(check_arg(1.12, type = "double"))
  expect_true(check_arg(1L, type = "integer"))
  expect_true(check_arg(1, type = "integerish"))
  expect_true(check_arg(TRUE, type = "logical"))
  expect_true(check_arg(1L, type = "numeric"))
  expect_true(check_arg(1, type = "numeric"))
  expect_true(check_arg(1.12, type = "numeric"))
  expect_true(check_arg(as.factor(iris$Species), type = "factor"))
  expect_true(check_arg(Sys.time(), type = "POSIXt"))
  expect_true(check_arg(data.frame(), type = "data.frame"))
  expect_true(check_arg(list(), type = "list"))

  # Test n
  expect_true(check_arg("foo", type = "character", n = 1))
  expect_true(check_arg(1.12, type = "double", n = 1))
  expect_true(check_arg(1L, type = "integer", n = 1))
  expect_true(check_arg(1, type = "integerish", n = 1))
  expect_true(check_arg(TRUE, type = "logical", n = 1))
  expect_true(check_arg(1L, type = "numeric", n = 1))
  expect_true(check_arg(1, type = "numeric", n = 1))
  expect_true(check_arg(1.12, type = "numeric", n = 1))
  expect_true(check_arg(as.factor(iris$Species)[1], type = "factor", n = 1))
  expect_true(check_arg(Sys.time(), type = "POSIXt", n = 1))
  expect_true(check_arg(list(foo = "bar"), type = "list", n = 1))

  # Test non-working cases
  expect_error(
    check_arg(TRUE, type = "character", arg = "x"),
    "Argument `x` must be a character."
  )
  expect_error(
    check_arg(TRUE, type = "double", arg = "x"),
    "Argument `x` must be a double."
  )
  expect_error(
    check_arg(TRUE, type = "integer", arg = "x"),
    "Argument `x` must be an integer."
  )
  expect_error(
    check_arg(TRUE, type = "integerish", arg = "x"),
    "Argument `x` must be an integerish."
  )
  expect_error(
    check_arg(1, type = "logical", arg = "x"),
    "Argument `x` must be a logical."
  )
  expect_error(
    check_arg(TRUE, type = "factor", arg = "x"),
    "Argument `x` must be a factor."
  )
  expect_error(
    check_arg(TRUE, type = "numeric", arg = "x"),
    "Argument `x` must be a numeric."
  )
  expect_error(
    check_arg(TRUE, type = "POSIXt", arg = "x"),
    "Argument `x` must be a POSIXt."
  )
  expect_error(
    check_arg(TRUE, type = "data.frame", arg = "x"),
    "Argument `x` must be a data.frame."
  )
  expect_error(
    check_arg(TRUE, type = "list", arg = "x"),
    "Argument `x` must be a list."
  )

  # Test non-working with n
  expect_error(
    check_arg(TRUE, type = "character", n = 2, arg = "x"),
    "Argument `x` must be a character of length 2."
  )
  expect_error(
    check_arg(TRUE, type = "double", n = 2, arg = "x"),
    "Argument `x` must be a double of length 2."
  )
  expect_error(
    check_arg(TRUE, type = "integer", n = 2, arg = "x"),
    "Argument `x` must be an integer of length 2."
  )
  expect_error(
    check_arg(TRUE, type = "integerish", n = 2, arg = "x"),
    "Argument `x` must be an integerish of length 2."
  )
  expect_error(
    check_arg(1, type = "logical", n = 2, arg = "x"),
    "Argument `x` must be a logical of length 2."
  )
  expect_error(
    check_arg(TRUE, type = "factor", n = 2, arg = "x"),
    "Argument `x` must be a factor of length 2."
  )
  expect_error(
    check_arg(TRUE, type = "numeric", n = 2, arg = "x"),
    "Argument `x` must be a numeric of length 2."
  )
  expect_error(
    check_arg(TRUE, type = "POSIXt", n = 2, arg = "x"),
    "Argument `x` must be a POSIXt of length 2."
  )
  expect_error(
    check_arg(TRUE, type = "list", n = 2, arg = "x"),
    "Argument `x` must be a list of length 2."
  )

  # Multiple tests
  expect_true(check_arg("foo", type = c("character", "double")))
  expect_true(check_arg(1, type = c("character", "double")))
  expect_error(
    check_arg(TRUE, type = c("character", "double"), arg = "x"),
    "Argument `x` must be a character or a double."
  )
})

test_that("check_sensors", {
  expect_true(check_sensors("Accelerometer"))
  expect_true(check_sensors("accelerometer"))
  expect_true(check_sensors("Accelerometer", n = 1, allow_null = TRUE))
  expect_true(check_sensors(NULL, allow_null = TRUE))
  expect_true(check_sensors(c("Accelerometer", "Activity"), n = 2))

  expect_error(
    check_sensors(NULL, arg = "sensors"),
    "Argument `sensors` must be a character."
  )
  expect_error(
    check_sensors("Foo", arg = "sensors"),
    "Sensor\\(s\\) \"Foo\" could not be found."
  )
  expect_error(
    check_sensors("foo", arg = "sensors"),
    "Sensor\\(s\\) \"foo\" could not be found."
  )
  expect_error(
    check_sensors(c("foo", "bar"), arg = "sensors"),
    "Sensor\\(s\\) \"foo\", \"bar\" could not be found."
  )

  expect_error(
    check_sensors(c("Accelerometer", "Gyroscope"), n = 1, arg = "sensors"),
    "Argument `sensors` must be a character of length 1."
  )
})

test_that("check_offset", {
  expect_error(
    check_offset(TRUE, 0),
    "`offset_before` must be a character vector, numeric vector, or a period."
  )
  expect_error(
    check_offset(0, offset_after = TRUE),
    "`offset_after` must be a character vector, numeric vector, or a period."
  )
  expect_error(
    check_offset(offset_before = "1800", 0),
    paste(
      "Invalid offset specified\\."
    )
  )
  expect_error(
    check_offset(0, 0),
    "`offset_before` and `offset_after` cannot be 0 or NULL at the same time."
  )
  expect_warning(
    check_offset(offset_before = -1800, 0),
    "`offset_before` must be a positive period \\(i.e. greater than 0\\)."
  )
  expect_warning(
    check_offset(0, offset_after = -1800),
    "`offset_after` must be a positive period \\(i.e. greater than 0\\)."
  )
})
