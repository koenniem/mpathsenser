# Helper functions for setting up test databases

# Write a small set of m-Path Sense JSON files (CARP format) to `dir`
write_test_json_files <- function(dir) {
  t_us <- 1765889440388567

  p1 <- list(
    list(
      sensorStartTime = t_us,
      data = list(
        `__type` = "dk.cachet.carp.mpathinfo",
        connectionId = "12345",
        studyName = "test_study",
        senseVersion = 5
      )
    ),
    list(
      sensorStartTime = t_us - 3600 * 1e6,
      data = list(`__type` = "dk.cachet.carp.timezone", timezone = "Europe/Brussels")
    ),
    list(
      sensorStartTime = t_us,
      data = list(`__type` = "dk.cachet.carp.activity", confidence = 100, type = "WALKING")
    ),
    list(
      sensorStartTime = t_us,
      data = list(
        `__type` = "dk.cachet.carp.batterystate",
        batteryLevel = 87,
        batteryStatus = "CHARGING"
      )
    ),
    list(
      sensorStartTime = t_us,
      data = list(`__type` = "dk.cachet.carp.stepcount", steps = 42)
    )
  )
  jsonlite::write_json(p1, file.path(dir, "test1.json"), auto_unbox = TRUE)

  p2 <- list(
    list(
      sensorStartTime = t_us + 1e6,
      data = list(
        `__type` = "dk.cachet.carp.mpathinfo",
        connectionId = "12345",
        studyName = "test_study",
        senseVersion = 5
      )
    ),
    list(
      sensorStartTime = t_us + 1e6,
      data = list(
        `__type` = "dk.cachet.carp.wifi",
        ssid = "test",
        bssid = "AA:BB",
        ip = "127.0.0.1"
      )
    )
  )
  jsonlite::write_json(p2, file.path(dir, "test2.json"), auto_unbox = TRUE)

  invisible(NULL)
}

# Create a fresh test database with sample data for testing
create_test_db <- function(path = ":memory:") {
  db <- create_db(NULL, path)

  dir <- tempfile("mpathsenser_test")
  dir.create(dir)
  write_test_json_files(dir)
  suppressMessages(read_mpath_sense(
    path = dir,
    db = db,
    recursive = FALSE,
    .progress = FALSE
  ))
  unlink(dir, recursive = TRUE)

  db
}

# Clean up a test database
cleanup_test_db <- function(db) {
  if (is.null(db) || !inherits(db, "duckdb_connection")) {
    return(invisible(NULL))
  }

  db_path <- tryCatch(
    db@driver@dbdir,
    error = function(e) NULL
  )

  if (dbIsValid(db)) {
    dbDisconnect(db)
  }

  if (!is.null(db_path) && file.exists(db_path)) {
    file.remove(db_path)
  }

  invisible(NULL)
}

# Write a richer set of m-Path Sense JSON files, mimicking the data the
# sensor_functions tests were written against (Device, Activity, AppUsage,
# Accelerometer) with measurements spanning 2021-11-13 and 2021-11-14.
write_sensor_test_json_files <- function(dir) {
  us <- function(t) format(as.numeric(as.POSIXct(t, tz = "UTC")) * 1e6, scientific = FALSE)
  device_full <- list(
    `__type` = "dk.cachet.carp.deviceinformation",
    deviceId = "QKQ1.200628.002",
    hardware = "qcom",
    deviceName = "gauguin",
    deviceManufacturer = "Xiaomi",
    deviceModel = "M2007J17G",
    operatingSystem = "REL",
    platform = "Android"
  )
  apps <- list(
    "com.bbc.news" = list(name = "BBC News", packageName = "com.bbc.news"),
    "com.calculator" = list(name = "Calculator", packageName = "com.calculator"),
    "com.clock" = list(name = "Clock", packageName = "com.clock"),
    "com.google.news" = list(name = "Google News", packageName = "com.google.news"),
    "com.google.pdfviewer" = list(name = "Google PDF Viewer", packageName = "com.google.pdfviewer"),
    "com.google.play.books" = list(
      name = "Google Play Books",
      packageName = "com.google.play.books"
    ),
    "com.google.play.games" = list(
      name = "Google Play Games",
      packageName = "com.google.play.games"
    ),
    "com.google.play.movies" = list(
      name = "Google Play Movies & TV",
      packageName = "com.google.play.movies"
    ),
    "com.google.play.music" = list(
      name = "Google Play Music",
      packageName = "com.google.play.music"
    ),
    "com.google.ar.core" = list(
      name = "Google Play Services for AR",
      packageName = "com.google.ar.core"
    ),
    "com.google.vr" = list(name = "Google VR Services", packageName = "com.google.vr"),
    "com.android.launcher" = list(name = "Home", packageName = "com.android.launcher"),
    "com.miui.providers" = list(
      name = "Mobile Device Information Provider",
      packageName = "com.miui.providers"
    ),
    "com.google.android.apps.photos" = list(
      name = "Photos",
      packageName = "com.google.android.apps.photos"
    ),
    "com.whatsapp" = list(name = "WhatsApp", packageName = "com.whatsapp"),
    "nl.mps.mpathsense" = list(name = "m-Path Sense", packageName = "nl.mps.mpathsense")
  )
  entries <- list(
    # Device info
    list(sensorStartTime = us("2021-11-13 13:00:00"), data = device_full),
    list(sensorStartTime = us("2021-11-14 13:00:00"), data = device_full),
    list(
      sensorStartTime = us("2021-11-14 14:01:00"),
      data = list(`__type` = "dk.cachet.carp.deviceinformation")
    ),
    # Activity
    list(
      sensorStartTime = us("2021-11-14 13:59:59"),
      data = list(`__type` = "dk.cachet.carp.activity")
    ),
    list(
      sensorStartTime = us("2021-11-14 14:00:00"),
      data = list(`__type` = "dk.cachet.carp.activity", confidence = 100, type = "WALKING")
    ),
    list(
      sensorStartTime = us("2021-11-14 14:00:01"),
      data = list(`__type` = "dk.cachet.carp.activity", confidence = 99, type = "STILL")
    ),
    # App usage (16 apps at the same time)
    list(
      sensorStartTime = us("2021-11-14 14:00:10"),
      data = list(`__type` = "dk.cachet.carp.appusage", usage = apps)
    ),
    # Accelerometer (for moving_average)
    list(
      sensorStartTime = us("2021-11-14 14:00:02"),
      data = list(
        `__type` = "dk.cachet.carp.accelerationfeatures",
        count = 1,
        xMean = 0.1,
        yMean = 0.2,
        zMean = 9.8,
        xStd = 0.01,
        yStd = 0.02,
        zStd = 0.03,
        xMin = 0,
        yMin = 0,
        zMin = 9.7,
        xMax = 0.2,
        yMax = 0.4,
        zMax = 9.9
      )
    ),
    # A couple of other sensors at 14:02:00
    list(
      sensorStartTime = us("2021-11-14 14:02:00"),
      data = list(
        `__type` = "dk.cachet.carp.batterystate",
        batteryLevel = 87,
        batteryStatus = "CHARGING"
      )
    ),
    list(
      sensorStartTime = us("2021-11-14 14:02:00"),
      data = list(`__type` = "dk.cachet.carp.stepcount", steps = 42)
    )
  )
  jsonlite::write_json(
    c(
      list(list(
        sensorStartTime = us("2021-11-13 13:00:00"),
        data = list(
          `__type` = "dk.cachet.carp.mpathinfo",
          connectionId = "12345",
          studyName = "test_study",
          senseVersion = 5
        )
      )),
      entries
    ),
    file.path(dir, "sensor_test.json"),
    auto_unbox = TRUE
  )
  invisible(NULL)
}

# Create a test database with the richer sensor data
create_sensor_test_db <- function(path = ":memory:") {
  db <- create_db(NULL, path)
  dir <- tempfile("mpathsenser_sensor_test")
  dir.create(dir)
  write_sensor_test_json_files(dir)
  suppressMessages(read_mpath_sense(
    path = dir,
    db = db,
    recursive = FALSE,
    .progress = FALSE
  ))
  unlink(dir, recursive = TRUE)
  db
}
