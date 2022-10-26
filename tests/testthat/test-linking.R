## link ===============
test_that("link", {
  dat1 <- data.frame(
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 3), 2),
    participant_id = c(rep("12345", 3), rep("23456", 3)),
    item_one = rep(c(40, 50, 60), 2)
  )

  dat2 <- data.frame(
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 12:50:00"), by = "5 min", length.out = 30), 2),
    participant_id = c(rep("12345", 30), rep("23456", 30)),
    x = rep(1:30, 2)
  )

  res <- link(dat1, dat2, "participant_id", offset_before = 1800, split = NULL)
  true <- tibble::tibble(
    time = rep(c(
      as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
      as.POSIXct("2021-11-14 15:00:00")
    ), 2),
    participant_id = c(rep("12345", 3), rep("23456", 3)),
    item_one = rep(c(40, 50, 60), 2),
    data = rep(list(
      tibble::tibble(
        time = seq.POSIXt(
          from = as.POSIXct("2021-11-14 12:50:00"),
          length.out = 3, by = "5 min"
        ),
        x = 1:3
      ),
      tibble::tibble(
        time = seq.POSIXt(
          from = as.POSIXct("2021-11-14 13:30:00"),
          length.out = 7, by = "5 min"
        ),
        x = 9:15
      ),
      tibble::tibble(
        time = seq.POSIXt(
          from = as.POSIXct("2021-11-14 14:30:00"),
          length.out = 7, by = "5 min"
        ),
        x = 21:27
      )
    ), 2)
  )
  expect_equal(res, true)

  # Test split argument
  res <- link(dat1, dat2, "participant_id", offset_before = 1800, split = 6)
  expect_equal(res, true)

  # Scrambled test
  scramble <- function(data) {
    idx <- sample(1:nrow(data), nrow(data))
    data[idx, ]
  }
  res <- link(scramble(dat1), scramble(dat2), "participant_id", offset_before = 1800) %>%
    arrange(participant_id, time)
  expect_equal(res, true)

  res <- link(dat1, dat2, "participant_id", offset_after = 1800)
  true <- tibble::tibble(
    time = rep(c(
      as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
      as.POSIXct("2021-11-14 15:00:00")
    ), 2),
    participant_id = c(rep("12345", 3), rep("23456", 3)),
    item_one = rep(c(40, 50, 60), 2),
    data = rep(list(
      tibble::tibble(
        time = seq.POSIXt(
          from = as.POSIXct("2021-11-14 13:00:00"),
          length.out = 7, by = "5 min"
        ),
        x = 3:9
      ),
      tibble::tibble(
        time = seq.POSIXt(
          from = as.POSIXct("2021-11-14 14:00:00"),
          length.out = 7, by = "5 min"
        ),
        x = 15:21
      ),
      tibble::tibble(
        time = seq.POSIXt(
          from = as.POSIXct("2021-11-14 15:00:00"),
          length.out = 4, by = "5 min"
        ),
        x = 27:30
      )
    ), 2)
  )
  expect_equal(res, true)

  res <- link(scramble(dat1), scramble(dat2), "participant_id", offset_after = 1800) %>%
    arrange(participant_id, time)
  expect_equal(res, true)

  # Test add_before and add_after
  res <- link(dat1, dat2, "participant_id",
    offset_before = 1800,
    add_before = TRUE, add_after = TRUE
  )
  true <- tibble::tibble(
    time = rep(c(
      as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 14:00:00"),
      as.POSIXct("2021-11-14 15:00:00")
    ), 2),
    participant_id = c(rep("12345", 3), rep("23456", 3)),
    item_one = rep(c(40, 50, 60), 2),
    data = rep(list(
      tibble::tibble(
        time = c(
          seq.POSIXt(
            from = as.POSIXct("2021-11-14 12:50:00"),
            length.out = 3, by = "5 min"
          ),
          as.POSIXct("2021-11-14 13:00:00")
        ),
        x = c(1:3, 4),
        original_time = c(
          rep(lubridate::`NA_POSIXct_`, 3),
          as.POSIXct("2021-11-14 13:05:00")
        )
      ),
      tibble::tibble(
        time = c(
          as.POSIXct("2021-11-14 13:30:00"),
          seq.POSIXt(
            from = as.POSIXct("2021-11-14 13:30:00"),
            length.out = 7, by = "5 min"
          ),
          as.POSIXct("2021-11-14 14:00:00")
        ),
        x = c(8, 9:15, 16),
        original_time = c(
          as.POSIXct("2021-11-14 13:25:00"),
          rep(lubridate::`NA_POSIXct_`, 7),
          as.POSIXct("2021-11-14 14:05:00")
        )
      ),
      tibble::tibble(
        time = c(
          as.POSIXct("2021-11-14 14:30:00"),
          seq.POSIXt(
            from = as.POSIXct("2021-11-14 14:30:00"),
            length.out = 7, by = "5 min"
          ),
          as.POSIXct("2021-11-14 15:00:00")
        ),
        x = c(20, 21:27, 28),
        original_time = c(
          as.POSIXct("2021-11-14 14:25:00"),
          rep(lubridate::`NA_POSIXct_`, 7),
          as.POSIXct("2021-11-14 15:05:00")
        )
      )
    ), 2)
  )
  expect_equal(res, true, ignore_attr = TRUE)

  # Test arguments
  expect_error(link(1, dat2, "participant_id", offset_before = 1800), "x must be a data frame")
  expect_error(link(dat1, 1, "participant_id", offset_before = 1800), "y must be a data frame")
  expect_error(
    link(dat1, dat2, 12345, offset_before = 1800),
    "by must be a character vector of variables to join by"
  )

  expect_error(
    link(mutate(dat1, time = as.character(time)), dat2, offset_before = 1800),
    "column 'time' in x must be a POSIXct"
  )
  expect_error(
    link(dat1, mutate(dat2, time = as.character(time)), offset_before = 1800),
    "column 'time' in y must be a POSIXct"
  )
  expect_error(
    link(dat1, dat2, offset_before = TRUE),
    "offset_before must be a character vector, numeric vector, or a period"
  )
  expect_error(
    link(dat1, dat2, offset_after = TRUE),
    "offset_after must be a character vector, numeric vector, or a period"
  )
  expect_error(
    link(dat1, dat2, offset_before = "1800"),
    paste(
      "Invalid offset specified\\."
    )
  )
  expect_error(
    link(dplyr::select(dat1, -time), dat2, offset_before = 1800),
    "column 'time' must be present in both x and y"
  )
  expect_error(
    link(dat1, dplyr::select(dat2, -time), offset_before = 1800),
    "column 'time' must be present in both x and y"
  )
  expect_error(
    link(dat1, dat2),
    "offset_before and offset_after cannot be 0 or NULL at the same time."
  )
  expect_warning(
    link(dat1, dat2, "participant_id", offset_before = -1800),
    "offset_before must be a positive period \\(i.e. greater than 0\\)."
  )
  expect_warning(
    link(dat1, dat2, "participant_id", offset_after = -1800),
    "offset_after must be a positive period \\(i.e. greater than 0\\)."
  )

  # Bug #6: Test whether original_time is present in all nested data columns
  # Create some data to use
  dat1 <- data.frame(
    time = c(
      rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 3), 2),
      as.POSIXct("2021-11-14 13:00:00"), as.POSIXct("2021-11-14 13:00:00")
    ),
    participant_id = c(rep("12345", 3), rep("23456", 3), "45678", "56789"),
    item_one = c(rep(c(40, 50, 60), 2), 40, 40)
  )

  dat2 <- data.frame(
    time = c(
      rep(seq.POSIXt(as.POSIXct("2021-11-14 12:50:00"), by = "5 min", length.out = 30), 2),
      seq.POSIXt(as.POSIXct("2021-11-14 12:30:00"), by = "5 min", length.out = 6)
    ),
    participant_id = c(rep("12345", 30), rep("23456", 30), rep("45678", 6)),
    x = c(rep(1:30, 2), 1:6)
  )

  # Link together, make sure to include rows before and after
  res <- link(
    x = dat1,
    y = dat2,
    by = "participant_id",
    offset_before = 1800,
    add_before = TRUE,
    add_after = TRUE
  )

  # Use the results, e.g. to filter out the extra added rows by offset_before and offset_after
  # (as you have to do for the number of screen unlocks).
  expect_true(all(purrr::map_lgl(res$data, ~ "original_time" %in% colnames(.x))))

  # Bug: add_before and add_after were ignored (or rather lost) if data_main is empty
  dat1 <- data.frame(
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 3), 2),
    participant_id = c(rep("12345", 3), rep("23456", 3)),
    item_one = rep(c(40, 50, 60), 2)
  )
  dat2 <- data.frame(
    time = rep(c(as.POSIXct("2021-11-14 11:50:00"), as.POSIXct("2021-11-14 16:50:00")), 2),
    participant_id = c(rep("12345", 2), rep("23456", 2)),
    x = rep(1:2, 2)
  )

  res <- link(
    x = dat1,
    y = dat2,
    by = "participant_id",
    offset_before = 1800L,
    add_before = TRUE,
    add_after = TRUE
  )

  expect_true(all(purrr::map_int(res$data, nrow) != 0))
})

## link_db ===============
test_that("link_db", {
  path <- system.file("testdata", package = "mpathsenser")
  db <- open_db(path, "test.db")
  dat1 <- data.frame(
    time = as.POSIXct(c("2021-11-14 13:00:00", "2021-11-14 14:00:00", "2021-11-14 15:00:00"),
      tz = "UTC"
    ),
    participant_id = "12345",
    item_one = c(40, 50, 60)
  )

  # Check basic functionality
  res <- link_db(db, "Activity", "Connectivity", offset_after = 1800)
  true <- tibble::tibble(
    measurement_id = c(
      "fbf85cd7-6d37-53a8-5c44-ad8fe13ef7ac",
      "ef96364c-d1f4-5f73-ce40-277f078e3d0f",
      "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"
    ),
    participant_id = "12345",
    time = as.POSIXct(c("2021-11-14 13:59:59", "2021-11-14 14:00:00", "2021-11-14 14:00:01"),
      tz = "UTC"
    ),
    confidence = c(NA, 100L, 99L),
    type = c(NA, "WALKING", "STILL"),
    data = list(
      tibble::tibble(
        measurement_id = c(
          "27a5777a-ec41-80de-afa4-d2e7f6b02fcf",
          "2d430c2a-5b16-1dce-0e2f-c049c44e3729"
        ),
        time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:01:00"), tz = "UTC"),
        connectivity_status = c("wifi", NA)
      ),
      tibble::tibble(
        measurement_id = c(
          "27a5777a-ec41-80de-afa4-d2e7f6b02fcf",
          "2d430c2a-5b16-1dce-0e2f-c049c44e3729"
        ),
        time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:01:00"), tz = "UTC"),
        connectivity_status = c("wifi", NA)
      ),
      tibble::tibble(
        measurement_id = "2d430c2a-5b16-1dce-0e2f-c049c44e3729",
        time = as.POSIXct("2021-11-14 14:01:00", tz = "UTC"),
        connectivity_status = NA_character_
      )
    )
  )
  expect_equal(res, true)

  # Check reverse
  res <- link_db(db, "Activity", "Connectivity", offset_after = 1800, reverse = TRUE)
  true <- tibble::tibble(
    measurement_id = c(
      "27a5777a-ec41-80de-afa4-d2e7f6b02fcf",
      "2d430c2a-5b16-1dce-0e2f-c049c44e3729"
    ),
    participant_id = "12345",
    time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:01:00"), tz = "UTC"),
    connectivity_status = c("wifi", NA),
    data = list(
      tibble::tibble(
        measurement_id = c(
          "ef96364c-d1f4-5f73-ce40-277f078e3d0f",
          "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"
        ),
        time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:00:01"), tz = "UTC"),
        confidence = c(100L, 99L),
        type = c("WALKING", "STILL"),
      ),
      tibble::tibble(
        measurement_id = character(0),
        time = structure(numeric(0), tzone = "UTC", class = c("POSIXct", "POSIXt")),
        confidence = integer(0),
        type = character(0)
      )
    )
  )
  expect_equal(res, true)

  # Check with external data
  res <- link_db(db, "Activity", external = dat1, offset_after = 1800)
  true <- tibble::tibble(
    dat1,
    data = list(
      tibble::tibble(
        measurement_id = character(0),
        time = structure(numeric(0), tzone = "UTC", class = c("POSIXct", "POSIXt")),
        confidence = integer(0L),
        type = character(0)
      ),
      tibble::tibble(
        measurement_id = c(
          "ef96364c-d1f4-5f73-ce40-277f078e3d0f",
          "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"
        ),
        time = as.POSIXct(c("2021-11-14 14:00:00", "2021-11-14 14:00:01"), tz = "UTC"),
        confidence = c(100L, 99L),
        type = c("WALKING", "STILL")
      ),
      tibble::tibble(
        measurement_id = character(0),
        time = structure(numeric(0), tzone = "UTC", class = c("POSIXct", "POSIXt")),
        confidence = integer(0),
        type = character(0)
      )
    )
  )
  expect_equal(res, true)

  # Argument checks
  expect_error(
    link_db(db, "Activity", 1:10, offset_before = 1800),
    "sensor_two must be a character vector"
  )
  expect_error(
    link_db(db, "Activity", offset_before = 1800, external = "Bluetooth"),
    "external must be a data frame"
  )
  expect_error(
    link_db(db, "Activity", "Bluetooth", offset_before = 1800, external = dat1),
    "only a second sensor or an external data frame can be supplied, but not both"
  )
  expect_error(
    link_db(db, "Activity", offset_before = 1800),
    "either a second sensor or an external data frame must be supplied"
  )
  dbDisconnect(db)
  expect_error(
    link_db(db, "Activity", "Bluetooth", offset_before = 1800),
    "Database connection is not valid"
  )
  db <- open_db(NULL, db@dbname)

  # Check time zone differences
  dat1$time <- format(dat1$time, tz = "Europe/Brussels", usetz = TRUE)
  expect_warning(
    link_db(db, "Activity", external = dat1, offset_after = 1800),
    "`external` is not using UTC as a time zone, unlike the data in the database."
  )
  dat1$time <- lubridate::force_tz(dat1$time, "UTC")
  dbDisconnect(db)

  # Check if ignore_large works
  filename <- tempfile("big", fileext = ".db")
  db <- create_db(NULL, filename)

  # Populate database
  add_study(db, data.frame(study_id = "test-study", data_format = "CARP"))
  add_participant(db, data.frame(study_id = "test-study", participant_id = "12345"))

  sens_value <- seq.int(0, 10, length.out = 50001)
  time_value <- seq.POSIXt(as.POSIXct("2021-11-14 14:00:00.000", format = "%F %H:%M:%OS"),
    by = "sec",
    length.out = 50001
  )
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

  expect_error(
    link_db(db, "Accelerometer", "Gyroscope", offset_after = 30),
    "the total number of rows is higher than 100000. Use ignore_large = TRUE to continue"
  )
  expect_error(
    link_db(db, "Accelerometer", "Gyroscope", offset_after = 30, ignore_large = TRUE),
    "x and y are identical"
  )

  # Cleanup
  dbDisconnect(db)
  file.remove(filename)
})

## link_gaps =================
test_that("link_gaps", {
  dat1 <- data.frame(
    participant_id = c(rep("12345", 6), rep("23456", 6)),
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 6), 2),
    item_one = rep(seq.int(10, by = 10, length.out = 6), 2)
  )

  # Test with two participants to ensure link takes different groups into account
  # Test both before and after each beep
  # 1. the gap falls completely inside the beep interval
  # 2. the start of the gap falls inside the beep interval, but the end does not
  # 3. the start of the gap falls outside of the beep interval, but the end of the gap falls inside
  # 4. the gap spans over the entire interval
  # 5. the gap occurs entirely before the interval
  # 6. the gap occurs entirely after the interval
  dat2 <- data.frame(
    participant_id = c(rep("12345", 46), rep("23456", 46)),
    from = rep(c(
      seq.POSIXt(as.POSIXct("2021-11-14 12:40:00"), by = "1 min", length.out = 20), # 1 before
      seq.POSIXt(as.POSIXct("2021-11-14 13:10:00"), by = "1 min", length.out = 20), # 1 after
      as.POSIXct(c(
        "2021-11-14 13:55:00", # 2 before, 3 after
        "2021-11-14 14:25:00", # 3 before, 2 after
        "2021-11-14 15:25:00", # 2 after
        "2021-11-14 15:30:00", # 4 before, after
        "2021-11-14 12:15:00", # 5 before, after
        "2021-11-14 18:35:00" # 6 before, after
      ))), 2),
    to = rep(c(
      seq.POSIXt(as.POSIXct("2021-11-14 12:41:00"), by = "1 min", length.out = 20), # 1 before
      seq.POSIXt(as.POSIXct("2021-11-14 13:11:00"), by = "1 min", length.out = 20), # 1 after
      as.POSIXct(c(
        "2021-11-14 14:05:00", # 2 before, 3 after
        "2021-11-14 14:40:00", # 3 before
        "2021-11-14 15:30:00", # 2 after
        "2021-11-14 16:30:00", # 4 before, after
        "2021-11-14 12:25:00", # 5 before, after
        "2021-11-14 18:40:00" # 6 before, after
      ))), 2)
  )

  # Test difference types of input for offset_before
  # Integer vs double
  expect_equal(
    link_gaps(dat1, dat2, by = "participant_id", offset_before = 1800L),
    link_gaps(dat1, dat2, by = "participant_id", offset_before = 1800)
  )

  expect_equal(
    link_gaps(dat1, dat2, by = "participant_id", offset_before = 1800L),
    link_gaps(dat1, dat2, by = "participant_id", offset_before = lubridate::minutes(30))
  )

  expect_equal(
    link_gaps(dat1, dat2, by = "participant_id", offset_before = 1800L),
    link_gaps(dat1, dat2, by = "participant_id", offset_before = "30 minutes")
  )

  expect_equal(
    link_gaps(dat1, dat2, by = "participant_id", offset_before = 1800L),
    link_gaps(dat1, dat2, by = "participant_id", offset_before = "1800 seconds")
  )

  # Offset after
  expect_equal(
    link_gaps(dat1, dat2, by = "participant_id", offset_after = 1800L),
    link_gaps(dat1, dat2, by = "participant_id", offset_after = 1800)
  )

  expect_equal(
    link_gaps(dat1, dat2, by = "participant_id", offset_after = 1800L),
    link_gaps(dat1, dat2, by = "participant_id", offset_after = lubridate::minutes(30))
  )

  expect_equal(
    link_gaps(dat1, dat2, by = "participant_id", offset_after = 1800L),
    link_gaps(dat1, dat2, by = "participant_id", offset_after = "30 minutes")
  )

  expect_equal(
    link_gaps(dat1, dat2, by = "participant_id", offset_after = 1800L),
    link_gaps(dat1, dat2, by = "participant_id", offset_after = "1800 seconds")
  )

  # Offset_before, raw_data = TRUE
  res_raw <- link_gaps(
    data = dat1,
    gaps = dat2,
    by = "participant_id",
    offset_before = 1800L,
    offset_after = 0L,
    raw_data = TRUE
  )
  true <- tibble::tibble(
    participant_id = c(rep("12345", 6), rep("23456", 6)),
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 6), 2),
    item_one = rep(seq.int(10, by = 10, length.out = 6), 2),
    gap_data = rep(list(
      tibble::tibble(
        from = seq.POSIXt(as.POSIXct("2021-11-14 12:40:00"), by = "1 min", length.out = 20),
        to = seq.POSIXt(as.POSIXct("2021-11-14 12:41:00"), by = "1 min", length.out = 20),
        gap = rep(60, 20)
      ), # 1
      tibble::tibble(
        from = as.POSIXct("2021-11-14 13:55:00"),
        to = as.POSIXct("2021-11-14 14:00:00"),
        gap = 300
      ), # 3
      tibble::tibble(
        from = as.POSIXct("2021-11-14 14:30:00"),
        to = as.POSIXct("2021-11-14 14:40:00"),
        gap = 600
      ), # 3
      tibble::tibble(
        from = as.POSIXct("2021-11-14 15:30:00"),
        to = as.POSIXct("2021-11-14 16:00:00"),
        gap = 1800
      ), # 4
      tibble::tibble(
        from = as.POSIXct(double(0), tz = ""),
        to = as.POSIXct(double(0), tz = ""),
        gap = integer(0)
      ), # 5
      tibble::tibble(
        from = as.POSIXct(double(0), tz = ""),
        to = as.POSIXct(double(0), tz = ""),
        gap = integer(0)
      ) # 6
    ), 2),
    gap = rep(c(1200, 300, 600, 1800, 0, 0), 2)
  )
  expect_equal(res_raw, true)

  # Check accidental double link
  expect_error(
    link_gaps(
      data = res_raw,
      gaps = dat2,
      by = "participant_id",
      offset_before = 1800L,
      raw_data = TRUE
    ),
    "column 'gap' should not already be present in data"
  )

  expect_error(
    link_gaps(
      data = res_raw %>% dplyr::select(-gap),
      gaps = dat2,
      by = "participant_id",
      offset_before = 1800L,
      raw_data = TRUE
    ),
    "column 'gap_data' should not already be present in data"
  )

  expect_error(
    link_gaps(
      data = res_raw %>% dplyr::select(-gap),
      gaps = dat2,
      by = "participant_id",
      offset_before = 1800L,
      raw_data = FALSE
    ),
    NA
  )

  # Scrambled test
  scramble <- function(data) {
    idx <- sample(1:nrow(data), nrow(data))
    data[idx, ]
  }
  res <- link_gaps(
    data = scramble(dat1),
    gaps = scramble(dat2),
    by = "participant_id",
    offset_before = 1800,
    raw_data = TRUE
  ) %>%
    arrange(participant_id, time)
  expect_equal(res_raw, true)

  # offset_before, raw_data = FALSE
  res <- link_gaps(
    data = dat1,
    gaps = dat2,
    by = "participant_id",
    offset_before = 1800L,
    offset_after = 0L
  )
  true <- tibble::tibble(
    participant_id = c(rep("12345", 6), rep("23456", 6)),
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 6), 2),
    item_one = rep(seq.int(10, by = 10, length.out = 6), 2),
    gap = rep(c(1200, 300, 600, 1800, 0, 0), 2)
  )
  expect_equal(res, true)

  # Test whether results from raw_data = FALSE and TRUE are the same
  expect_equal(
    res_raw %>%
      mutate(gap = purrr::map_dbl(gap_data, ~ sum(.x$gap))) %>%
      dplyr::select(-gap_data),
    res
  )

  # Offset_after, raw_data = TRUE
  res_raw <- link_gaps(
    data = dat1,
    gaps = dat2,
    by = "participant_id",
    offset_before = 0L,
    offset_after = 1800L,
    raw_data = TRUE
  )
  true <- tibble::tibble(
    participant_id = c(rep("12345", 6), rep("23456", 6)),
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 6), 2),
    item_one = rep(seq.int(10, by = 10, length.out = 6), 2),
    gap_data = rep(list(
      tibble::tibble(
        from = seq.POSIXt(as.POSIXct("2021-11-14 13:10:00"), by = "1 min", length.out = 20),
        to = seq.POSIXt(as.POSIXct("2021-11-14 13:11:00"), by = "1 min", length.out = 20),
        gap = rep(60, 20)
      ), # 1
      tibble::tibble(
        from = c(as.POSIXct("2021-11-14 14:00:00"), as.POSIXct("2021-11-14 14:25:00")),
        to = c(as.POSIXct("2021-11-14 14:05:00"), as.POSIXct("2021-11-14 14:30:00")),
        gap = 300
      ), # 2
      tibble::tibble(
        from = as.POSIXct("2021-11-14 15:25:00"),
        to = as.POSIXct("2021-11-14 15:30:00"),
        gap = 300
      ), # 2
      tibble::tibble(
        from = as.POSIXct("2021-11-14 16:00:00"),
        to = as.POSIXct("2021-11-14 16:30:00"),
        gap = 1800
      ), # 4
      tibble::tibble(
        from = as.POSIXct(double(0), tz = ""),
        to = as.POSIXct(double(0), tz = ""),
        gap = integer(0)
      ), # 5
      tibble::tibble(
        from = as.POSIXct(double(0), tz = ""),
        to = as.POSIXct(double(0), tz = ""),
        gap = integer(0)
      ) # 6
    ), 2),
    gap = rep(c(1200, 600, 300, 1800, 0, 0), 2)
  )
  expect_equal(res_raw, true)

  # offset_after, raw_data = FALSE
  res <- link_gaps(
    data = dat1,
    gaps = dat2,
    by = "participant_id",
    offset_before = 0L,
    offset_after = 1800L
  )
  true <- tibble::tibble(
    participant_id = c(rep("12345", 6), rep("23456", 6)),
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 6), 2),
    item_one = rep(seq.int(10, by = 10, length.out = 6), 2),
    gap = rep(c(1200, 600, 300, 1800, 0, 0), 2)
  )
  expect_equal(res, true)

  # Test whether results from raw_data = FALSE and TRUE are the same
  expect_equal(
    res_raw %>%
      mutate(gap = purrr::map_dbl(gap_data, ~ sum(.x$gap))) %>%
      dplyr::select(-gap_data),
    res
  )

  # Offset both
  res_raw <- link_gaps(
    data = dat1,
    gaps = dat2,
    by = "participant_id",
    offset_before = 1800L,
    offset_after = 1800L,
    raw_data = TRUE
  )
  true <- tibble::tibble(
    participant_id = c(rep("12345", 6), rep("23456", 6)),
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 6), 2),
    item_one = rep(seq.int(10, by = 10, length.out = 6), 2),
    gap_data = rep(list(
      tibble::tibble(
        from = c(
          seq.POSIXt(as.POSIXct("2021-11-14 12:40:00"), by = "1 min", length.out = 20),
          seq.POSIXt(as.POSIXct("2021-11-14 13:10:00"), by = "1 min", length.out = 20)
        ),
        to = c(
          seq.POSIXt(as.POSIXct("2021-11-14 12:41:00"), by = "1 min", length.out = 20),
          seq.POSIXt(as.POSIXct("2021-11-14 13:11:00"), by = "1 min", length.out = 20)
        ),
        gap = rep(60, 40)
      ), # 1
      tibble::tibble(
        from = c(as.POSIXct("2021-11-14 13:55:00"), as.POSIXct("2021-11-14 14:25:00")),
        to = c(as.POSIXct("2021-11-14 14:05:00"), as.POSIXct("2021-11-14 14:30:00")),
        gap = c(600, 300)
      ), # 2
      tibble::tibble(
        from = c(as.POSIXct("2021-11-14 14:30:00"), as.POSIXct("2021-11-14 15:25:00")),
        to = c(as.POSIXct("2021-11-14 14:40:00"), as.POSIXct("2021-11-14 15:30:00")),
        gap = c(600, 300)
      ), # 2
      tibble::tibble(
        from = as.POSIXct("2021-11-14 15:30:00"),
        to = as.POSIXct("2021-11-14 16:30:00"),
        gap = 3600
      ), # 4
      tibble::tibble(
        from = as.POSIXct(double(0), tz = ""),
        to = as.POSIXct(double(0), tz = ""),
        gap = integer(0)
      ), # 5
      tibble::tibble(
        from = as.POSIXct(double(0), tz = ""),
        to = as.POSIXct(double(0), tz = ""),
        gap = integer(0)
      ) # 6
    ), 2),
    gap = rep(c(2400, 900, 900, 3600, 0, 0), 2)
  )
  expect_equal(res_raw, true)

  # offset_both
  res <- link_gaps(
    data = dat1,
    gaps = dat2,
    by = "participant_id",
    offset_before = 1800L,
    offset_after = 1800L
  )
  true <- tibble::tibble(
    participant_id = c(rep("12345", 6), rep("23456", 6)),
    time = rep(seq.POSIXt(as.POSIXct("2021-11-14 13:00:00"), by = "1 hour", length.out = 6), 2),
    item_one = rep(seq.int(10, by = 10, length.out = 6), 2),
    gap = rep(c(2400, 900, 900, 3600, 0, 0), 2)
  )
  expect_equal(res, true)

  # Test whether results from raw_data = FALSE and TRUE are the same
  expect_equal(
    res_raw %>%
      mutate(gap = purrr::map_dbl(gap_data, ~ sum(.x$gap))) %>%
      dplyr::select(-gap_data),
    res
  )

  # Argument checks
  expect_error(
    link_gaps(
      data = dat1$participant_id,
      gaps = dat2,
      by = "participant_id",
      offset_before = 1800L
    ),
    "`data` must be a data frame"
  )
  expect_error(
    link_gaps(
      data = dat1,
      gaps = dat2$participant_id,
      by = "participant_id",
      offset_before = 1800L
    ),
    "`gaps` must be a data frame"
  )
  expect_error(
    link_gaps(
      data = dat1,
      gaps = dat2,
      by = as.integer(dat1$participant_id),
      offset_before = 1800L
    ),
    "`by` must be a character vector of variables to join by"
  )
  expect_error(
    link_gaps(
      data = dat1,
      gaps = dat2,
      by = "participant_id"
    ),
    "`offset_before` and `offset_after` cannot be 0 or NULL at the same time."
  )
  expect_error(
    link_gaps(
      data = dat1,
      gaps = dat2,
      by = "participant_id",
      offset_before = TRUE
    ),
    "`offset_before` must be a character vector, numeric vector, or a period."
  )
  expect_error(
    link_gaps(
      data = dat1,
      gaps = dat2,
      by = "participant_id",
      offset_after = TRUE
    ),
    "`offset_after` must be a character vector, numeric vector, or a period."
  )
  expect_error(
    link_gaps(
      data = dat1,
      gaps = dat2,
      by = "participant_id",
      offset_before = "20 foos",
      offset_after = "20 bars"
    ),
    "Invalid offset specified."
  )
  expect_warning(
    link_gaps(
      data = dat1,
      gaps = dat2,
      by = "participant_id",
      offset_before = -1800
    ),
    "`offset_before` must be a positive period \\(i.e. greater than 0\\)."
  )
  expect_warning(
    link_gaps(
      data = dat1,
      gaps = dat2,
      by = "participant_id",
      offset_after = -1800
    ),
    "`offset_after` must be a positive period \\(i.e. greater than 0\\)."
  )
  expect_error(
    link_gaps(
      data = dat1[,-2],
      gaps = dat2,
      by = "participant_id",
      offset_after = 1800
    ),
    "Column `time` must be present in `data`"
  )
  expect_error(
    link_gaps(
      data = dat1,
      gaps = dat2[,-2],
      by = "participant_id",
      offset_after = 1800
    ),
    "Column `from` and `to` must be present in `gaps`."
  )
  expect_error(
    link_gaps(
      data = mutate(dat1, time = as.character(time)),
      gaps = dat2,
      by = "participant_id",
      offset_after = 1800
    ),
    "Column `time` in `data` must be a POSIXct."
  )

})

## bin_data =================
test_that("bin_data", {
  data <- tibble::tibble(
    participant_id = 1,
    datetime = c(
      "2022-06-21 15:00:00", "2022-06-21 15:55:00",
      "2022-06-21 17:05:00", "2022-06-21 17:10:00"
    ),
    confidence = 100,
    type = "WALKING"
  )

  # get bins per hour, even if the interval is longer than one hour
  res <- data %>%
    mutate(datetime = as.POSIXct(datetime)) %>%
    mutate(lead = lead(datetime)) %>%
    bin_data(
      start_time = datetime,
      end_time = lead,
      by = "hour"
    )

  true <- tibble::tibble(
    bin = as.POSIXct(c("2022-06-21 15:00:00", "2022-06-21 16:00:00", "2022-06-21 17:00:00")),
    bin_data = list(
      tibble::tibble(
        participant_id = 1,
        datetime = as.POSIXct(c("2022-06-21 15:00:00", "2022-06-21 15:55:00")),
        confidence = 100,
        type = "WALKING",
        lead = as.POSIXct(c("2022-06-21 15:55:00", "2022-06-21 16:00:00"))
      ),
      tibble::tibble(
        participant_id = 1,
        datetime = as.POSIXct(c("2022-06-21 16:00:00")),
        confidence = 100,
        type = "WALKING",
        lead = as.POSIXct("2022-06-21 17:00:00")
      ),
      tibble::tibble(
        participant_id = 1,
        datetime = as.POSIXct(c("2022-06-21 17:00:00", "2022-06-21 17:05:00", "2022-06-21 17:10:00")),
        confidence = 100,
        type = "WALKING",
        lead = as.POSIXct(c("2022-06-21 17:05:00", "2022-06-21 17:10:00", NA))
      )
    )
  )
  expect_equal(res, true)

  # Alternatively, you can give an integer value to by to create custom-sized
  # bins, but only if fixed = FALSE. Not that these bins are not rounded to,
  # as in this example 30 minutes, but rather depends on the earliest time
  # in the group.
  res <- data %>%
    mutate(datetime = as.POSIXct(datetime)) %>%
      mutate(lead = lead(datetime)) %>%
      bin_data(
        start_time = datetime,
        end_time = lead,
        by = 1800L,
        fixed = FALSE
      )
  true <- tibble::tibble(
    bin = as.POSIXct(c("2022-06-21 15:00:00", "2022-06-21 15:30:00", "2022-06-21 16:00:00",
                       "2022-06-21 16:30:00", "2022-06-21 17:00:00")),
    bin_data = list(
      tibble::tibble(
        participant_id = 1,
        datetime = as.POSIXct(c("2022-06-21 15:00:00")),
        confidence = 100,
        type = "WALKING",
        lead = as.POSIXct(c("2022-06-21 15:30:00"))
      ),
      tibble::tibble(
        participant_id = 1,
        datetime = as.POSIXct(c("2022-06-21 15:30:00", "2022-06-21 15:55:00")),
        confidence = 100,
        type = "WALKING",
        lead = as.POSIXct(c("2022-06-21 15:55:00", "2022-06-21 16:00:00"))
      ),
      tibble::tibble(
        participant_id = 1,
        datetime = as.POSIXct(c("2022-06-21 16:00:00")),
        confidence = 100,
        type = "WALKING",
        lead = as.POSIXct(c("2022-06-21 16:30:00"))
      ),
      tibble::tibble(
        participant_id = 1,
        datetime = as.POSIXct(c("2022-06-21 16:30:00")),
        confidence = 100,
        type = "WALKING",
        lead = as.POSIXct(c("2022-06-21 17:00:00"))
      ),
      tibble::tibble(
        participant_id = 1,
        datetime = as.POSIXct(c("2022-06-21 17:00:00", "2022-06-21 17:05:00", "2022-06-21 17:10:00")),
        confidence = 100,
        type = "WALKING",
        lead = as.POSIXct(c("2022-06-21 17:05:00", "2022-06-21 17:10:00", NA))
      )
    )
  )
  expect_equal(res, true)

  # More complicated data for showcasing grouping:
  data <- tibble::tibble(
    participant_id = c(rep(1, 4), rep(2, 4)),
    datetime = rep(c(
      "2022-06-21 15:00:00", "2022-06-21 15:55:00",
      "2022-06-21 17:05:00", "2022-06-21 17:10:00"
    ), 2),
    confidence = 100,
    type = rep(c("STILL", "WALKING", "STILL", "WALKING"), 2)
  )

  # binned_intervals also takes into account the prior grouping structure
  res <- data %>%
    mutate(datetime = as.POSIXct(datetime)) %>%
    group_by(participant_id) %>%
    mutate(lead = lead(datetime)) %>%
    group_by(participant_id, type) %>%
    bin_data(
      start_time = datetime,
      end_time = lead,
      by = "hour"
    )
  true <- tibble::tibble(
    participant_id = c(rep(1, 6), rep(2, 6)),
    type = rep(c("STILL", "STILL", "STILL", "WALKING", "WALKING", "WALKING"), 2),
    bin = rep(as.POSIXct(c("2022-06-21 15:00:00", "2022-06-21 16:00:00",
                           "2022-06-21 17:00:00")), 4),
    bin_data = rep(list(
      # STILL
      tibble::tibble(
        datetime = as.POSIXct(c("2022-06-21 15:00:00")),
        confidence = 100,
        lead = as.POSIXct(c("2022-06-21 15:55:00"))
      ),
      tibble::tibble(
        datetime = as.POSIXct(double(0)),
        confidence = double(0),
        lead = as.POSIXct(double(0))
      ),
      tibble::tibble(
        datetime = as.POSIXct(c("2022-06-21 17:05:00")),
        confidence = 100,
        lead = as.POSIXct(c("2022-06-21 17:10:00"))
      ),
      # WALKING
      tibble::tibble(
        datetime = as.POSIXct(c("2022-06-21 15:55:00")),
        confidence = 100,
        lead = as.POSIXct(c("2022-06-21 16:00:00"))
      ),
      tibble::tibble(
        datetime = as.POSIXct(c("2022-06-21 16:00:00")),
        confidence = 100,
        lead = as.POSIXct(c("2022-06-21 17:00:00"))
      ),
      tibble::tibble(
        datetime = as.POSIXct(c("2022-06-21 17:00:00", "2022-06-21 17:10:00")),
        confidence = 100,
        lead = as.POSIXct(c("2022-06-21 17:05:00", NA))
      )
    ), 2)
  ) %>%
    group_by(participant_id, type)
  expect_equal(res, true)

  # To get the duration for each bin (note to change the variable names in sum):
  duration <- purrr::map_dbl(.x = res$bin_data,
                             .f = ~ sum(as.double(.x$lead) - as.double(.x$datetime),
                                        na.rm = TRUE) / 60)

  # Or:
  duration2 <- res %>%
    unnest(bin_data, keep_empty = TRUE) %>%
    mutate(duration = .data$lead - .data$datetime) %>%
    group_by(bin, .add = TRUE) %>%
    summarise(duration = sum(.data$duration, na.rm = TRUE), .groups = "drop")

  true <-c(55, 0, 5, 5, 60, 5, 55, 0, 5, 5, 60, 5)
  expect_equal(duration, as.double(duration2$duration))
  expect_equal(duration, true)
  expect_equal(as.double(duration2$duration), true)

  # Argument checks
  data <- data %>%
    mutate(datetime = as.POSIXct(datetime)) %>%
    mutate(lead = lead(datetime))

  expect_error(
      bin_data(
        data = data,
        start_time = datetime,
        end_time = lead,
        by = TRUE
      ),
      "`by` must be one of 'sec', 'min', 'hour', or 'day', or a numeric value if `fixed = FALSE`."
  )
  expect_error(
    data %>%
      mutate(datetime = as.character(datetime)) %>%
      bin_data(
        start_time = datetime,
        end_time = lead,
        by = "hour"
      ),
    NA
  )
})
