test_that("the example ESM fixture matches the example capture", {
  skip_if_not_installed("mpathr")

  esm_file <- system.file(
    "extdata",
    "example-esm",
    "example_esm.csv",
    package = "mpathsenser"
  )
  meta_file <- system.file("extdata", "example_meta.csv", package = "mpathr")
  expect_no_warning(esm <- mpathr::read_mpath(esm_file, meta_file))

  expect_equal(nrow(esm), 16)
  expect_equal(unique(esm$connectionId), 372780)
  expect_equal(sum(is.na(esm$timeStampStart)), 3)
  expect_equal(
    is.na(esm$slider_anxious_sliderNegPos),
    is.na(esm$timeStampStart)
  )

  sent <- mpathr::timestamps_to_datetime(
    esm$timeStampSent,
    tz_offset = esm$timeZoneOffset
  )

  db <- example_db()
  on.exit(cleanup_test_db(db), add = TRUE)
  expect_equal(unique(esm$connectionId), get_participants(db)$participant_id)

  pedometer <- dplyr::collect(get_data(db, "Pedometer", participant_id = 372780))
  expect_true(all(sent >= min(pedometer$time) & sent <= max(pedometer$time)))
})
