# Test that the output of test.db is equal to test.json

rand <- function(n, chars = TRUE, numbers = TRUE, uppercase = FALSE) {
  data <- NULL

  if (!chars && !numbers) abort("Select either letters, numbers, or both.")

  if (chars) {
    if (uppercase) {
      data <- c(data, LETTERS[1:6])
    } else {
      data <- c(data, letters[1:6])
    }
  }

  if (numbers) {
    data <- c(data, 0:9)
  }

  paste(sample(data, n, TRUE), collapse = "")
}

gen_id <- function(uppercase = FALSE) {
  if (uppercase) {
    paste0(
      rand(8, uppercase = TRUE), "-", rand(4, uppercase = TRUE), "-",
      rand(4, uppercase = TRUE), "-", rand(4, uppercase = TRUE), "-",
      rand(12, uppercase = TRUE)
    )
  } else {
    paste0(rand(8), "-", rand(4), "-", rand(4), "-", rand(4), "-", rand(12))
  }
}

### db_test ============
db_test <- function(sensor, true_data) {
  path <- system.file("testdata", package = "mpathsenser")
  tempfile <- tempfile()
  db <- create_db(NULL, tempfile)
  suppressMessages(import(path, db = db, sensors = sensor, batch_size = 1, recursive = FALSE))

  data <- get_data(db, sensor, "12345", "2021-11-13", "2021-11-14") %>%
    collect()
  true <- true_data

  testthat::expect_equal(data, true)

  close_db(db)
  unlink(tempfile)
}

### Accelerometer ============
test_that("Accelerometer", {
  db_test("Accelerometer", true_data = tibble::tibble(
    measurement_id = c(
      "15d06c62-3b69-5a1d-21b1-bf82ddc573bf_1",
      paste0("268f6135-70ac-f96a-f800-0d8f6bb6ffa6_", 1:5),
      "15d06c62-3b69-5a1d-21b1-bf82ddc573bg",
      "268f6135-70ac-f96a-f800-0d8f6bb6ffa6",
      "9da62087-feb7-cd4d-2964-f33c96587863"

    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c(
      "13:59:59.000", "14:00:00.000", "14:00:00.200",
      "14:00:00.400", "14:00:00.600", "14:00:00.800",
      "13:59:59.000", "14:00:00.000", "14:00:01.000"
    ),
    timezone = c(rep(NA, 7), "CET", "CET"),
    x = c(
      NA, -0.45947299565606, -0.31261359042355, -0.92950401127219,
      -0.57727191720589, -0.29951507561905, NA, -0.45947299565606,
      NA
    ),
    y = c(
      NA, 0.66402153964424, 0.37969748447006, 0.40856299984702,
      0.91639749921748, 0.362559743517898, NA, 0.66402153964424,
      NA
    ),
    z = c(
      NA, 9.34531187528098, 9.19530456721872, 9.55071386678994,
      9.861390738568486, 9.38065402300287, NA, 9.34531187528098,
      NA
    ),
    x_mean = c(rep(NA, 8), -0.0022081288080),
    y_mean = c(rep(NA, 8), -0.0004500896919),
    z_mean = c(rep(NA, 8), -0.023477349265),
    x_mean_sq = c(rep(NA, 8), 0.00002054091549),
    y_mean_sq = c(rep(NA, 8), 0.00001809346463),
    z_mean_sq = c(rep(NA, 8), 0.000582901524),
    n = c(rep(NA, 8), 298)
  ))
})

### Activity ============
test_that("Activity", {
  db_test("Activity", true_data = tibble::tibble(
    measurement_id = c(
      "fbf85cd7-6d37-53a8-5c44-ad8fe13ef7ac",
      "ef96364c-d1f4-5f73-ce40-277f078e3d0f",
      "5ba54e77-4bcf-c8d1-17ff-71b9ed908897"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("13:59:59", "14:00:00", "14:00:01"),
    timezone = c(NA, "CET", "CET"),
    confidence = c(NA, 100L, 99L),
    type = c(NA, "WALKING", "STILL")
  ))
})

### AirQuality ============
test_that("AirQuality", {
  db_test("AirQuality", true_data = tibble::tibble(
    measurement_id = c(
      "d8dc5bea-6184-bafb-d65c-0a180aa21eb0",
      "c68f94a8-1992-8ce5-0d29-8290c75679c7"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("13:59:59", "14:00:00"),
    timezone = c(NA, "CET"),
    air_quality_index = c(NA, 31L),
    air_quality_level = c(NA, "MODERATE"),
    source = c(NA, "IRCEL-CELINE - Belgian Interregional Environment Agency"),
    place = c(NA, "Aarschot, Belgium"),
    latitude = c(NA, 50.43555),
    longitude = c(NA, 4.146731)
  ))
})

### InstalledApps ============
test_that("InstalledApps", {
  db_test("InstalledApps", true_data = tibble::tibble(
    measurement_id = c(
      "b7eacaef-a4ff-90c0-c5a2-7e293dee7721_1",
      paste0("c48c62ac-a738-0ee0-586d-29ab0acff25a_", 1:16)
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("13:59:59", rep("14:00:00", 16)),
    timezone = c(NA, rep("CET", 16)),
    app = c(
      NA, "WhatsApp", "Google Play Services for AR", "BBC News", "Clock", "Google Play Music",
      "Mobile Device Information Provider", "Calculator", "Google Play Movies & TV",
      "Photos", "Google Play Books", "Home", "Google PDF Viewer", "m-Path Sense",
      "Google VR Services", "Google Play Games", "Google News"
    )
  ))
})

### AppUsage ============
test_that("AppUsage", {
  db_test("AppUsage", true_data = tibble::tibble(
    measurement_id = c(
      "2c3de6bf-e63d-4c47-8d14-36b9505d8968_1",
      paste0("2e3f8f0a-9de1-f328-6a2e-29b168925179_", 1:7)
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("13:59:59", rep("14:00:00", 7)),
    timezone = c(NA, rep("CET", 7)),
    start = c(NA, rep("2021-11-14T14:00:00.000000Z", 7)),
    end = c(NA, rep("2021-11-14T14:30:00.000000Z", 7)),
    usage = c(NA, 525L, 18L, 230L, 3L, 10L, 2L, 33L),
    app = c(
      "", "kuleuven", "securitycenter", "home", "systemui",
      "permissioncontroller", "powerkeeper", "settings"
    )
  ))
})

### Battery ============
test_that("Battery", {
  db_test("Battery", true_data = tibble::tibble(
    measurement_id = c(
      "1cc3b19c-a56a-ebc0-cd55-d5faf928c6c8",
      "8a224e06-6587-6be6-b7e8-a85fea1d974f",
      "ebd712d8-eb84-dea9-5b98-e9f16b2e0533"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:01:00", "14:02:00"),
    timezone = c("CET", "CET", NA),
    battery_level = c(100L, 99L, NA),
    battery_status = c("charging", "discharging", NA)
  ))
})

### Bluetooth ============
test_that("Bluetooth", {
  db_test("Bluetooth", true_data = tibble::tibble(
    measurement_id = c(
      paste0("fff452f8-f927-86ca-b853-5236f25ec7dd_", 1:2),
      "88cf67a2-16d0-e7cc-2be5-6296d7a0f0b3_1"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:00:00", "14:01:00"),
    timezone = c("CET", "CET", NA),
    advertisement_name = c(
      "ace4ec2999d033ac6b76def024d53507dc6eafbb",
      "3dbd32cf6327dd01fd0fab96d4e202686d5e0e96", NA
    ),
    bluetooth_device_id = c(
      "a94074e7b729cf573aa9e34218c4318b04b62560",
      "9b0cbfcbee534e034b1dec3b2057f7c310bf01d9", NA
    ),
    bluetooth_device_name = c(
      "915e4ac54400e2304392f1a2d123561853295e50",
      "761dd8aa1f57e662a05d0b601b508b2694ed4275", NA
    ),
    bluetooth_device_type = c("unknown", "le", NA),
    connectable = c(0L, 1L, NA),
    rssi = c(-84L, -91L, NA),
    tx_power_level = c(100L, NA, NA)
  ))
})

### Calendar ============
test_that("Calendar", {
  db_test("Calendar", true_data = tibble::tibble(
    measurement_id = c(
      paste0("7b050885-ad2f-3cd3-9217-5eb876e3c5c1_", 1:2),
      "5bafebd2-3d01-2355-0ad4-5e1f697b690f_1"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:00:00", "14:01:00"),
    timezone = c("CET", "CET", NA),
    event_id = c("279", "262", NA),
    calendar_id = c("3", "8", NA),
    title = c(
      "f4984b61b560744d74f08ea659bb3a2275537f31",
      "7fb8bab16fd45110007e30e1d6dd4a7f6811bc8d", NA
    ),
    description = c(
      "28a74980d7bebc37dbc9cfa949871ce83edc2b26",
      "3d4e4a2d7dac35a3264e7026b798c7dde2b39675", NA
    ),
    start = c("2021-11-14T14:00:00.000Z", "2021-11-14T14:30:00.000Z", NA),
    end = c("2021-11-14T14:30:00.000Z", "2021-11-14T15:00:00.000Z", NA),
    all_day = c(0L, 0L, NA),
    location = c("PSI", "02. Leuven", NA),
    attendees = c("c87ec7481c056ea2c88541cd41b966a9f8114d51", NA, NA)
  ))
})

### Connectivity ============
test_that("Connectivity", {
  db_test("Connectivity", true_data = tibble::tibble(
    measurement_id = c(
      "27a5777a-ec41-80de-afa4-d2e7f6b02fcf",
      "2d430c2a-5b16-1dce-0e2f-c049c44e3729"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:01:00"),
    timezone = c("CET", NA),
    connectivity_status = c("wifi", NA)
  ))
})

### Device ============
test_that("Device", {
  db_test("Device", true_data = tibble::tibble(
    measurement_id = c(
      "bce3c272-3e06-4c84-f533-5bbbeaaac049",
      "ac1230a8-ed5f-4ded-7fca-7693a5ab4124",
      "138b9204-a313-96f3-89de-42bc2ac9d1e9"
    ),
    participant_id = "12345",
    date = c("2021-11-13", "2021-11-14", "2021-11-14"),
    time = c(rep("13:00:00", 2), "14:01:00"),
    timezone = c("CET", "CET", NA),
    device_id = c(rep("QKQ1.200628.002", 2), NA),
    hardware = c(rep("qcom", 2), NA),
    device_name = c(rep("gauguin", 2), NA),
    device_manufacturer = c(rep("Xiaomi", 2), NA),
    device_model = c(rep("M2007J17G", 2), NA),
    operating_system = c(rep("REL", 2), NA),
    platform = c(rep("Android", 2), NA)
  ))
})

### Error ============
test_that("Error", {
  db_test("Error", true_data = tibble::tibble(
    measurement_id = c(
      "df74ca23-3d0f-fcb1-70b9-12b2051c1115",
      "b1c326ff-e96b-059f-7f1b-72d3384b7222"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:01:00"),
    timezone = c("CET", NA),
    message = c(paste(
      "AirQuality Probe Exception: SocketException: Failed host lookup:",
      "'api.waqi.info' (OS Error: nodename nor servname provided, or not known,",
      "errno = 8)"
    ), NA)
  ))
})

### Gyroscope ============
test_that("Gyroscope", {
  db_test("Gyroscope", true_data = tibble::tibble(
    measurement_id = c(
      "06bce1d4-b4a9-d1f1-2f06-cc76d872a61d_1",
      paste0("ce0cc1bf-f071-91b5-19c8-4b3b789e64a2_", 1:5),
      "ce0cc1bf-f071-91b5-19c8-4b3b789e64a3",
      "06bce1d4-b4a9-d1f1-2f06-cc76d872a61g",
      "6d8aa5b8-cd1b-c482-f678-5267c85b393b"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c(
      "14:00:01.000", "14:00:00.000", "14:00:00.200", "14:00:00.400",
      "14:00:00.600", "14:00:00.800", "14:00:00.000", "14:00:01.000",
      "14:00:02.000"
    ),
    timezone = c(rep(NA, 6), "CET", NA, "CET"),
    x = c(
      NA, -0.0003741947002708912, -0.000054615666158497336, 0.0001584370620548725,
      0.0001584370620548725, 0.0001584370620548725, -0.0003741947002708912,
      NA, NA
    ),
    y = c(
      NA, -0.003209952265024185, -0.0006000570137985051, -0.0001739516155794263,
      -0.00038700434379279613, -0.00038700434379279613, -0.003209952265024185,
      NA, NA
    ),
    z = c(
      NA, 0.021946871653199197, 0.00010896776802837849, 0.00010896776802837849,
      0.00010896776802837849, 0.00010896776802837849,  0.021946871653199197,
      NA, NA
    ),
    x_mean = c(rep(NA, 8), -1.0022081288080912509),
    y_mean = c(rep(NA, 8), -2.0004500896919943741),
    z_mean = c(rep(NA, 8), -3.02347734926530979),
    x_mean_sq = c(rep(NA, 8), 4.000020540915494408226),
    y_mean_sq = c(rep(NA, 8), 5.000018093464634687579),
    z_mean_sq = c(rep(NA, 8), 6.0005829015244919054),
    n = c(rep(NA, 8), 299)
  ))
})

### Light ============
test_that("Light", {
  db_test("Light", true_data = tibble::tibble(
    measurement_id = c(
      "23d93a4e-7ff8-f057-828d-c728c9466f8a",
      "9199095d-acc1-737d-3029-5089738c6079"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:00:10"),
    timezone = c("CET", NA),
    mean_lux = c(353, NA),
    std_lux = c(80.2, NA),
    min_lux = c(20.1, NA),
    max_lux = c(985, NA)
  ))
})

### Location ============
test_that("Location", {
  db_test("Location", true_data = tibble::tibble(
    measurement_id = c(
      "24d0a408-a87c-2ea4-80c0-23a964a16675",
      "62db94d7-2155-f867-b7c4-f878681ec736"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:01:00"),
    timezone = c("CET", NA),
    latitude = c(paste0(
      "beb3005c45cb250b9841b9aae0637fa5610c45ce206ca38494f7f74a6cbdf8566cdf0d8967e",
      "9422bb857a4d87c2e6c08d85f162525a8f9d6a72a8"
    ), NA),
    longitude = c(paste0(
      "8a4f368c36ab8e6e9dcbca9d7ef88a313f48e841e77746c54f1ca5b1887cd56f123d8ba36e",
      "92465e5483ebbd335b242ff8fade5b31a43e20dd851"
    ), NA),
    altitude = c(71.7784264646543, NA),
    accuracy = c(22.1168141708386, NA),
    speed = c(0.467787307347347, NA),
    speed_accuracy = c(1.9150350848860985, NA),
    heading = c(35.5130489970395, NA)
  ))
})

### Memory  ============
test_that("Memory", {
  db_test("Memory", true_data = tibble::tibble(
    measurement_id = c(
      "7f2fbe8b-af33-0078-137e-ed21c0f97b10",
      "7e3eb79e-56b2-441d-c7ab-890ba478b923"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:01:00"),
    timezone = c("CET", NA),
    free_physical_memory = c(2027336609L, NA),
    free_virtual_memory = c(233377575L, NA)
  ))
})

### Mobility ============
test_that("Mobility", {
  db_test("Mobility", true_data = tibble::tibble(
    measurement_id = c(
      "95d7c9e8-bf65-1d89-264a-6d43e9c98e25",
      "6b789772-18ae-ce5f-b522-a38793023e1d"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:01:00"),
    timezone = c("CET", NA),
    number_of_places = c(6L, NA),
    location_variance = c(0.232154350507205752, NA),
    entropy = c(1.6472347857577631, NA),
    normalized_entropy = c(0.3990261427129516, NA),
    home_stay = c(-1, NA),
    distance_travelled = c(0L, NA)
  ))
})

### Noise ============
test_that("Noise", {
  db_test("Noise", true_data = tibble::tibble(
    measurement_id = c(
      "5fc9ecdf-c62c-955c-e59d-d6c4a2ee9a2a",
      "6ab8de47-fdfd-b2bd-bba1-6b6641906835"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:01:00"),
    timezone = c("CET", NA),
    mean_decibel = c(42.96355291627242, NA),
    std_decibel = c(4.971839655603458, NA),
    min_decibel = c(34.289534207582256, NA),
    max_decibel = c(71.893975307503923, NA)
  ))
})

### PhoneLog ============

### Pedometer ============
test_that("Pedometer", {
  db_test("Pedometer", true_data = tibble::tibble(
    measurement_id = c(
      "5ea89a50-777c-11eb-8c91-3f093a6e6e87",
      "50dc55c0-356c-11ec-b60d-ddab48d96f4a",
      "af026c22-1d7f-6bb0-b492-9d705c5bdda8"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:00:01", "14:00:02"),
    timezone = c("CET", "CET", NA),
    step_count = c(1000L, 1001L, NA)
  ))
})

### Screen ============
test_that("Screen", {
  db_test("Screen", true_data = tibble::tibble(
    measurement_id = c(
      "c40b2459-e598-8fdb-7925-e55ed3048baf",
      "28560428-b12d-e753-f5d4-61c1d51fcd58"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:00:01"),
    timezone = c(NA, "CET"),
    screen_event = c(NA, "SCREEN_ON")
  ))
})

### TextMessage ============

### Weather ============
test_that("Weather", {
  db_test("Weather", true_data = tibble::tibble(
    measurement_id = c(
      "d42140e5-d91b-8ec5-87fd-f003e4fdcdf3",
      "84f92625-1c56-fdd8-18ed-5fffa8f69e6c"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:01:00"),
    timezone = c("CET", NA),
    country = c("BE", NA),
    area_name = c("Arrondissement Leuven", NA),
    weather_main = c("Clouds", NA),
    weather_description = c("overcast clouds", NA),
    sunrise = c("2021-11-14T07:30:00.000", NA),
    sunset = c("2021-11-14T18:00:00.000", NA),
    latitude = c(50.1234, NA),
    longitude = c(4.1234, NA),
    pressure = c(1000L, NA),
    wind_speed = c(3.12, NA),
    wind_degree = c(250, NA),
    humidity = c(50L, NA),
    cloudiness = c(90L, NA),
    rain_last_hour = c(0, NA),
    rain_last_3hours = c(0, NA),
    snow_last_hour = c(0, NA),
    snow_last_3hours = c(0, NA),
    temperature = c(17.123456789, NA),
    temp_min = c(17, NA),
    temp_max = c(17.123456789, NA)
  ))
})

### WifI ============
test_that("Wifi", {
  db_test("Wifi", true_data = tibble::tibble(
    measurement_id = c(
      "59ce179b-3a3c-42d8-6e5c-6a1030545e92",
      "0392ed98-a2d5-fcd1-2ec5-ac13dd865f9a"
    ),
    participant_id = "12345",
    date = "2021-11-14",
    time = c("14:00:00", "14:01:00"),
    timezone = c(NA, "CET"),
    ssid = c(NA, "878779215c977eefd2c434d71a0e172aa19b66e1"),
    bssid = c(NA, "2f054f4cbb48bb589f40e82ac0433912d1db3931"),
    ip = c(NA, "10.415.918.389")
  ))
})
