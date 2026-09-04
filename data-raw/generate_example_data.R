# Generate the small example dataset used by the README and the "Get started"
# vignette. Run from the package root: source("data-raw/generate_example_data.R")
#
# The dataset mimics the output of the m-Path Sense app: JSON files with an
# mpathinfo entry followed by sensor entries, named after the m-Path Sense
# convention. Two participants, three days of data for the main participant.

library(jsonlite)

dir <- "inst/extdata/example"
unlink(dir, recursive = TRUE)
dir.create(dir, recursive = TRUE)

set.seed(20250814)

# Timestamp helpers: m-Path Sense stores microseconds since the epoch
us <- function(ymd_hms) {
  sprintf("%.0f", as.numeric(as.POSIXct(ymd_hms, tz = "UTC")) * 1e6)
}

mac <- "1C:2B:9B:12:6E:23"

garmin_entry <- function(day, sensor, n) {
  t0 <- as.numeric(as.POSIXct(paste0("2025-12-", day, " 14:00:00"), tz = "UTC")) * 1e6
  if (sensor == "heartRate") {
    values <- lapply(seq_len(n), function(i) {
      list(
        timestamp = t0 + i * 500000,
        beatsPerMinute = sample(55:95, 1),
        macAddress = mac
      )
    })
  } else {
    # stress
    values <- lapply(seq_len(n), function(i) {
      list(
        timestamp = t0 + i * 3000000,
        stressScore = sample(1:99, 1),
        status = "measuring",
        macAddress = mac
      )
    })
  }
  list(
    sensorStartTime = t0,
    data = c(
      list(`__type` = "dk.cachet.carp.garminalllogsdata"),
      setNames(list(values), sensor)
    )
  )
}

mk_block <- function(pid, day, hour, minutes, apps = NULL) {
  t <- sprintf("2025-12-%02d %02d:%02d:00", day, hour, minutes)
  entries <- list(
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.mpathinfo",
        connectionId = pid,
        studyName = "cravings_study",
        senseVersion = 5
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.activity",
        confidence = sample(c(100, 100, 90, 70), 1),
        type = sample(c("STILL", "WALKING", "ON_FOOT", "IN_VEHICLE"), 1)
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.batterystate",
        batteryLevel = sample(40:95, 1),
        batteryStatus = sample(c("CHARGING", "DISCHARGING"), 1)
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.stepcount",
        steps = sample(50:1500, 1)
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.wifi",
        ssid = sample(c("KULeuven-Guest", "HomeWiFi", "CafeNet"), 1),
        bssid = paste(sample(c("AA", "BB", "CC", "DD", "EE", "FF"), 6, TRUE), collapse = ":"),
        ip = paste(sample(1:255, 4, TRUE), collapse = ".")
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.screenevent",
        screenEvent = sample(c("SCREEN_ON", "SCREEN_OFF"), 1)
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.location",
        latitude = 50.8772 + rnorm(1, 0, 0.005),
        longitude = 4.7006 + rnorm(1, 0, 0.005),
        altitude = sample(20:60, 1),
        accuracy = sample(5:30, 1)
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.ambientlight",
        meanLux = sample(c(5, 50, 200, 1000), 1),
        stdLux = sample(1:50, 1),
        minLux = 0,
        maxLux = sample(10:1500, 1)
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.connectivity",
        connectivityStatus = sample(c("wifi", "mobile", "none"), 1)
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.freememory",
        freePhysicalMemory = sample(2:8, 1) * 1e9,
        freeVirtualMemory = sample(2:8, 1) * 1e9
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.timezone",
        timezone = "Europe/Brussels"
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.weather",
        country = "BE",
        areaName = "Leuven",
        weatherMain = "Clouds",
        weatherDescription = "overcast clouds",
        pressure = sample(1000:1025, 1),
        windSpeed = sample(1:15, 1),
        windDegree = sample(0:359, 1),
        humidity = sample(40:95, 1),
        cloudiness = sample(0:100, 1),
        temperature = sample(2:10, 1),
        tempMin = 1,
        tempMax = 12
      )
    ),
    list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.heartbeat",
        period = 15L,
        deviceType = "android",
        deviceRoleName = "phone"
      )
    )
  )
  if (!is.null(apps)) {
    entries[[length(entries) + 1]] <- list(
      sensorStartTime = us(t),
      data = list(
        `__type` = "dk.cachet.carp.appusage",
        usage = setNames(
          lapply(apps, function(a) {
            list(
              startDate = paste0(
                "2025-12-",
                day,
                "T",
                sprintf("%02d", hour),
                ":",
                sprintf("%02d", minutes),
                ":00.000Z"
              ),
              endDate = paste0(
                "2025-12-",
                day,
                "T",
                sprintf("%02d", hour + 1),
                ":",
                sprintf("%02d", minutes),
                ":00.000Z"
              ),
              usage = sample(60000:3600000, 1),
              name = a,
              packageName = gsub(" ", ".", tolower(a), fixed = TRUE)
            )
          }),
          apps
        )
      )
    )
  }
  # A couple of accelerometer feature rows (roughly every 30 minutes)
  for (k in 1:2) {
    t2 <- sprintf("2025-12-%02d %02d:%02d:00", day, hour, minutes + k * 30)
    entries[[length(entries) + 1]] <- list(
      sensorStartTime = us(t2),
      data = list(
        `__type` = "dk.cachet.carp.accelerationfeatures",
        count = 300,
        xMean = rnorm(1, 0, 0.5),
        yMean = rnorm(1, 0, 0.5),
        zMean = rnorm(1, 9.8, 0.1),
        xStd = runif(1, 0.5, 3),
        yStd = runif(1, 0.5, 3),
        zStd = runif(1, 0.5, 3),
        xMin = -3,
        yMin = -3,
        zMin = 5,
        xMax = 3,
        yMax = 3,
        zMax = 14,
        xMaxMinDiff = 6,
        yMaxMinDiff = 6,
        zMaxMinDiff = 9,
        xMedian = 0,
        yMedian = 0,
        zMedian = 9.8,
        xMad = 1,
        yMad = 1,
        zMad = 1,
        xIqr = 1.5,
        yIqr = 1.5,
        zIqr = 1.5,
        xNegCount = 150,
        yNegCount = 150,
        zNegCount = 0,
        xPosCount = 150,
        yPosCount = 150,
        zPosCount = 300,
        xAboveMean = 150,
        yAboveMean = 150,
        zAboveMean = 300,
        xEnergy = 5,
        yEnergy = 5,
        zEnergy = 10,
        avgResultAcceleration = 3,
        signalMagnitudeArea = 6
      )
    )
  }
  entries
}

# Participant 12345: three days, three blocks per day
apps <- c("WhatsApp", "Gmail", "Spotify", "Google Maps")
for (day in 13:15) {
  for (hm in list(c(8, 0), c(14, 0), c(20, 0))) {
    entries <- mk_block("12345", day, hm[1], hm[2], apps = apps)
    if (hm[2] == 0) {
      # A daily device entry and a Garmin log entry
      entries <- c(
        entries,
        list(list(
          sensorStartTime = us(sprintf("2025-12-%02d %02d:%02d:00", day, hm[1], hm[2])),
          data = list(
            `__type` = "dk.cachet.carp.deviceinformation",
            deviceId = "QKQ1.200628.002",
            hardware = "qcom",
            deviceName = "gauguin",
            deviceManufacturer = "Xiaomi",
            deviceModel = "M2007J17G",
            operatingSystem = "REL",
            platform = "Android"
          )
        ))
      )
      entries <- c(
        entries,
        list(garmin_entry(day, "heartRate", 30), garmin_entry(day, "stress", 10))
      )
    }
    write_json(
      entries,
      file.path(
        dir,
        sprintf(
          "1_example_study_12345_m_Path_sense_2025-12-%02d_%02d-%02d-00.000000.json",
          day,
          hm[1],
          hm[2]
        )
      ),
      auto_unbox = TRUE
    )
  }
}

# Participant 67890: one block on one day
write_json(
  mk_block("67890", 14, 12, 0, apps = c("WhatsApp", "Gmail")),
  file.path(dir, "1_example_study_67890_m_Path_sense_2025-12-14_12-00-00.000000.json"),
  auto_unbox = TRUE
)

cat("Wrote", length(list.files(dir)), "files to", dir, "\n")
cat("Total size:", sum(file.size(list.files(dir, full.names = TRUE))) / 1024, "KB\n")
