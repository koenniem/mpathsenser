test_that("compress works correctly", {
  # Create some test data
  data <- tibble(
    row_id = 1:18,
    x = c(1, 1, 1, NA, NA, NA, 2, 3, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7),
    y = c(1, 2, 2, NA, NA, NA, 2, 3, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7),
    z = c(1, 2, 3, NA, NA, NA, 4, 4, 4, 5, 5, 5, NA, NA, NA, NA, NA, NA)
  )

  # Surrounding
  expect_equal(
    compress(.data = data,
             .cols = !"row_id",
             .before = TRUE,
             .after = TRUE),
    data[c(1, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 15, 16, 18), ]
  )

  # Preceding
  expect_equal(
    compress(.data = data,
             .cols = !"row_id",
             .before = TRUE,
             .after = FALSE),
    data[c(1, 2, 3, 4, 7, 8, 9, 10, 13, 16), ]
  )

  # Succeeding
  expect_equal(
    compress(.data = data,
             .cols = !"row_id",
             .before = FALSE,
             .after = TRUE),
    data[c(1, 2, 3, 6, 7, 8, 9, 12, 15, 18), ]
  )
})

test_that("equals compares values correctly", {
  expect_true(equals(1, 1))
  expect_true(equals("foo", "foo"))
  expect_true(equals(1, 1+1e-10))
  expect_true(equals(1, 1L))

  expect_true(all(equals(letters, letters)))
  expect_true(all(!equals(letters, LETTERS)))

  # POSIXt
  time <- Sys.time()
  date <- Sys.Date()
  expect_true(equals(time, time))
  expect_true(equals(date, date))
})

test_that("equals handles NA, NaN, and NULL", {
  expect_true(equals(NA, NA))
  expect_true(equals(NA, NaN))
  expect_false(equals(NA, NULL))
})

test_that("equals handles lists", {
  expect_true(equals(list(foo = 1), list(foo = 1)))
  expect_false(equals(list(foo = 1), list(bar = 1)))
  expect_false(all(equals(list(foo = 1, bar = 2), list(foo = 1, list(bar = 2)))))
  expect_false(all(equals(list(foo = 1, bar = 2), list(foo = 1))))
})
