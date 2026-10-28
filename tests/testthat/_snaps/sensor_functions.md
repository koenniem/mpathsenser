# get_data rejects invalid date strings

    Code
      get_data(db, "Activity", start_date = "2021-02-30")
    Condition
      Error in `.normalize_get_data_date()`:
      ! `start_date` must be a valid date in "YYYY-MM-DD" format.
      x You supplied "2021-02-30".

---

    Code
      get_data(db, "Activity", end_date = "2021-11-14 trailing")
    Condition
      Error in `.normalize_get_data_date()`:
      ! `end_date` must be a valid date in "YYYY-MM-DD" format.
      x You supplied "2021-11-14 trailing".

# moving_average validates windows, columns, and date filters

    Code
      moving_average(db, "Accelerometer", "x_mean")
    Condition
      Error in `.moving_average_window_seconds()`:
      ! `window` is absent but must be supplied.

---

    Code
      moving_average(db, "Accelerometer", "x_mean", window = 0)
    Condition
      Error in `.moving_average_window_seconds()`:
      ! `window` must be a finite, positive duration.

---

    Code
      moving_average(db, "Accelerometer", "x_mean", window = -1)
    Condition
      Error in `.moving_average_window_seconds()`:
      ! `window` must be a finite, positive duration.

---

    Code
      moving_average(db, "Accelerometer", "x_mean", window = Inf)
    Condition
      Error in `.moving_average_window_seconds()`:
      ! `window` must be a finite, positive duration.

---

    Code
      moving_average(db, "Accelerometer", "x_mean", window = c(1, 2))
    Condition
      Error in `.moving_average_window_seconds()`:
      ! `window` must have length one.

---

    Code
      moving_average(db, "Accelerometer", "x_mean", window = "nonsense")
    Condition
      Error in `.moving_average_window_seconds()`:
      ! `window` must be a valid duration.

---

    Code
      moving_average(db, "Accelerometer", "x_mean", window = lubridate::period(
        months = 1))
    Condition
      Error in `.moving_average_window_seconds()`:
      ! `window` cannot include calendar-dependent years or months.

---

    Code
      moving_average(db, "Accelerometer", "x_mean", window = lubridate::period(years = 1))
    Condition
      Error in `.moving_average_window_seconds()`:
      ! `window` cannot include calendar-dependent years or months.

---

    Code
      moving_average(db, "Accelerometer", character(), window = 60)
    Condition
      Error in `moving_average()`:
      ! `cols` must be a non-empty vector of unique, non-missing column names.

---

    Code
      moving_average(db, "Accelerometer", c("x_mean", "x_mean"), window = 60)
    Condition
      Error in `moving_average()`:
      ! `cols` must be a non-empty vector of unique, non-missing column names.

---

    Code
      moving_average(db, "Accelerometer", "absent", window = 60)
    Condition
      Error in `moving_average()`:
      ! `cols` must name existing numeric measurement columns, excluding `participant_id` and `time`.
      x Invalid column: absent.

---

    Code
      moving_average(db, "Accelerometer", c("timezone", "participant_id", "time"),
      window = 60)
    Condition
      Error in `moving_average()`:
      ! `cols` must name existing numeric measurement columns, excluding `participant_id` and `time`.
      x Invalid columns: timezone, participant_id, and time.

---

    Code
      moving_average(db, "Accelerometer", "x_mean", window = 60, start_date = "foo")
    Condition
      Error in `.normalize_get_data_date()`:
      ! `start_date` must be a valid date in "YYYY-MM-DD" format.
      x You supplied "foo".

---

    Code
      moving_average(db, "Accelerometer", "x_mean", window = 60, end_date = "foo")
    Condition
      Error in `.normalize_get_data_date()`:
      ! `end_date` must be a valid date in "YYYY-MM-DD" format.
      x You supplied "foo".

