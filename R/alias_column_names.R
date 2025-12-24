# This file exists to provide a single place to remap the column names of the output files to the
# ones used in the database schema of mpathsenser. As the column names have already changed several
# times over the year, we use these functions so we can easily add new aliases of the existing
# columns.

#' Alias column names of a data frame containing sensor data
#'
#' This function is used to remap the column names of a data frame containing sensor data to the
#' column names used in the database schema of mpathsenser. As the column names have already
#' changed several times over the year, we use these functions so we can easily add new aliases of
#' the existing columns. The function is generic and dispatches to the specific method for the
#' class of `data`.
#'
#' Aliasing of the column names depends on the specific method, but column names are only changed
#' if they have an alias available. Otherwise, they are kept unchanged.
#'
#' @param data A data frame for which the column names are remapped according to the specific method
#' for this data frame. As such, `data` should have a class attribute that corresponds to one of
#' the methods defined in this file.
#' @inheritParams rlang::args_dots_empty
#'
#' @return A data frame with the column names remapped according to the specific method for this
#' data frame.
#' @keywords internal
#'
#' @examples
#' x <- data.frame(
#'   id = 1:3,
#'   timestamp = as.POSIXct(c("2021-01-01 00:00:00", "2021-01-01 00:00:01", "2021-01-01 00:00:02")),
#'   xm = c(1, 2, 3),
#'   ym = c(4, 5, 6),
#'   zm = c(7, 8, 9)
#' )
#' class(x) <- c("accelerometer", class(x))
#' mpathsenser:::alias_column_names(x)
alias_column_names <- function(data, ...) {
  rlang::check_dots_empty()
  UseMethod("alias_column_names")
}

#' @export
#' @keywords internal
alias_column_names.accelerometer <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("xm", "xMean") ~ "x_mean",
          c("ym", "yMean") ~ "y_mean",
          c("zm", "zMean") ~ "z_mean",
          c("xStd") ~ "x_std",
          c("yStd") ~ "y_std",
          c("zStd") ~ "z_std",
          c("xAad") ~ "x_aad",
          c("yAad") ~ "y_aad",
          c("zAad") ~ "z_aad",
          c("xMin") ~ "x_min",
          c("yMin") ~ "y_min",
          c("zMin") ~ "z_min",
          c("xMax") ~ "x_max",
          c("yMax") ~ "y_max",
          c("zMax") ~ "z_max",
          c("xMaxMinDiff") ~ "x_max_min_diff",
          c("yMaxMinDiff") ~ "y_max_min_diff",
          c("zMaxMinDiff") ~ "z_max_min_diff",
          c("xMedian") ~ "x_median",
          c("yMedian") ~ "y_median",
          c("zMedian") ~ "z_median",
          c("xMad") ~ "x_mad",
          c("yMad") ~ "y_mad",
          c("zMad") ~ "z_mad",
          c("xIqr") ~ "x_iqr",
          c("yIqr") ~ "y_iqr",
          c("zIqr") ~ "z_iqr",
          c("xNegCount") ~ "x_neg_n",
          c("yNegCount") ~ "y_neg_n",
          c("zNegCount") ~ "z_neg_n",
          c("xPosCount") ~ "x_pos_n",
          c("yPosCount") ~ "y_pos_n",
          c("zPosCount") ~ "z_pos_n",
          c("xAboveMean") ~ "x_above_mean",
          c("yAboveMean") ~ "y_above_mean",
          c("zAboveMean") ~ "z_above_mean",
          c("xms", "xEnergy") ~ "x_energy",
          c("yms", "yEnergy") ~ "y_energy",
          c("zms", "zEnergy") ~ "z_energy",
          c("avgResultAcceleration") ~ "avg_res_acc",
          c("signalMagnitudeArea") ~ "sma",
          "count" ~ "n",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.activity <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.airquality <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("airQualityIndex", "air_quality_index") ~ "air_quality_index",
          c("airQualityLevel", "air_quality_level") ~ "air_quality_level",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.appusage <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("startDate", "start_date") ~ "start",
          c("endDate", "end_date") ~ "end",
          c("appName", "app_name", "name") ~ "app",
          c("packageName") ~ "package_name",
          c("lastForeground") ~ "last_foreground",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.battery <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("batteryLevel") ~ "battery_level",
          c("batteryStatus") ~ "battery_status",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.bluetooth <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("scanResult", "scanResults") ~ "scan_result",
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("startScan") ~ "start_scan",
          c("endScan") ~ "end_scan",
          c("advertisementName") ~ "advertisement_name",
          c("bluetoothDeviceId") ~ "bluetooth_device_id",
          c("bluetoothDeviceName") ~ "bluetooth_device_name",
          c("bluetoothDeviceType") ~ "bluetooth_device_type",
          c("connectable") ~ "connectable",
          c("rssi") ~ "rssi",
          c("txPowerLevel") ~ "tx_power_level",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.connectivity <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("connectivityStatus") ~ "connectivity_status",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.device <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("deviceData") ~ "device_data",
          c("deviceId") ~ "device_id",
          c("deviceName") ~ "device_name",
          c("deviceManufacturer") ~ "device_manufacturer",
          c("deviceModel") ~ "device_model",
          c("operatingSystem") ~ "operating_system",
          c("operatingSystemVersion") ~ "operating_system_version",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.error <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminaccelerometer <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp") ~ "time",
          c("xValue") ~ "x",
          c("yValue") ~ "y",
          c("zValue") ~ "z",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminactigraphy <- function(data, ...) {
  data |>
    select(-dplyr::any_of("end_time")) |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("startTimestamp") ~ "time",
          c("endTimestamp") ~ "end_time",
          c("instance") ~ "instance",
          c("totalEnergy") ~ "total_energy",
          c("zeroCrossingCount") ~ "n_zero_crossing",
          c("timeAboveThreshold") ~ "time_above_threshold",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminbbi <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp") ~ "time",
          c("bbi") ~ "bbi",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminenhancedbbi <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp") ~ "time",
          c("bbi") ~ "bbi",
          c("status") ~ "status",
          c("gapDuration") ~ "gap_duration",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garmingyroscope <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp") ~ "time",
          c("xValue") ~ "x",
          c("yValue") ~ "y",
          c("zValue") ~ "z",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminheartrate <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp") ~ "time",
          c("beatsPerMinute") ~ "bpm",
          c("status") ~ "status",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminmeta <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("fromTime") ~ "time_from",
          c("toTime") ~ "time_to",
          c("accelerometer") ~ "n_accelerometer",
          c("actigraphy1") ~ "n_actigraphy_1",
          c("actigraphy2") ~ "n_actigraphy_2",
          c("actigraphy3") ~ "n_actigraphy_3",
          c("bbi") ~ "n_bbi",
          c("enhancedBbi") ~ "n_enhanced_bbi",
          c("gyroscope") ~ "n_gyroscope",
          c("heartRate") ~ "n_heartrate",
          c("respiration") ~ "n_respiration",
          c("skinTemperature") ~ "n_skin_temperature",
          c("spo2") ~ "n_spo2",
          c("steps") ~ "n_steps",
          c("stress") ~ "n_stress",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminrespiration <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp") ~ "time",
          c("breathsPerMinute") ~ "bpm",
          c("status") ~ "status",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminskintemperature <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp") ~ "time",
          c("temperature") ~ "temperature",
          c("status") ~ "status",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminspo2 <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp") ~ "time",
          c("spo2Reading") ~ "spo2",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminsteps <- function(data, ...) {
  data |>
    select(-dplyr::any_of("end_time")) |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("startTimestamp") ~ "time",
          c("endTimestamp") ~ "end_time",
          c("stepCount") ~ "step_count",
          c("totalSteps") ~ "total_steps",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminstress <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp") ~ "time",
          c("endTimestamp") ~ "end_time",
          c("stressScore") ~ "stress",
          c("status") ~ "status",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminwriststatus <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp") ~ "time",
          c("status") ~ "status",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.garminzerocrossing <- function(data, ...) {
  data |>
    select(-dplyr::any_of("end_time")) |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("startTimestamp") ~ "time",
          c("endTimestamp") ~ "end_time",
          c("zeroCrossingCount") ~ "n_zero_crossing",
          c("totalEnergy") ~ "total_energy",
          c("deadband") ~ "deadband",
          c("macAddress") ~ "mac_address",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.geofence <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.gyroscope <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.heartbeat <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("deviceType") ~ "device_type",
          c("deviceRoleName") ~ "device_role_name",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.keyboard <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.light <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("meanLux") ~ "mean_lux",
          c("stdLux") ~ "std_lux",
          c("minLux") ~ "min_lux",
          c("maxLux") ~ "max_lux",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.location <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("verticalAccuracy") ~ "vertical_accuracy",
          c("headingAccuracy") ~ "heading_accuracy",
          c("speedAccuracy") ~ "speed_accuracy",
          c("isMock") ~ "is_mock",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.memory <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("freePhysicalMemory") ~ "free_physical_memory",
          c("freeVirtualMemory") ~ "free_virtual_memory",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.mpathinfo <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("connectionId") ~ "connection_id",
          c("accountCode") ~ "accountCode",
          c("studyName") ~ "study_name",
          c("senseVersion") ~ "sense_version",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.noise <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("meanDecibel") ~ "mean_decibel",
          c("stdDecibel") ~ "std_decibel",
          c("minDecibel") ~ "min_decibel",
          c("maxDecibel") ~ "max_decibel",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.pedometer <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("stepCount", "steps") ~ "step_count",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.screen <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("screenEvent") ~ "screen_event",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.timezone <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.weather <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          c("areaName") ~ "area_name",
          c("weatherMain") ~ "weather_main",
          c("weatherDescription") ~ "weather_description",
          c("windSpeed") ~ "wind_speed",
          c("windDegree") ~ "wind_degree",
          c("rainLastHour") ~ "rain_last_hour",
          c("rainLast3Hours", "rain_last_3_hours") ~ "rain_last_3hours",
          c("snowLastHour") ~ "snow_last_hour",
          c("snowLast3Hours", "snow_last_3_hours") ~ "snow_last_3hours",
          c("tempMin") ~ "temp_min",
          c("tempMax") ~ "temp_max",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}

#' @export
#' @keywords internal
alias_column_names.wifi <- function(data, ...) {
  data |>
    dplyr::rename_with(
      .fn = \(colnames) {
        dplyr::case_match(
          colnames,
          c("id") ~ "measurement_id",
          c("timestamp", "start_time") ~ "time",
          .default = colnames,
          .ptype = character()
        )
      }
    )
}
