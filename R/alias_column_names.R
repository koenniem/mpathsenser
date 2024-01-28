# This file exists to provide a single place to remap the column names of the output files to the
# ones used in the database schema of mpathsenser. As the column names have already changed several
# times over the year, we use these functions so we can easily add new aliases of the existing
# columns.

#' @keywords internal
alias_column_names <- function(names, sensor) {
  UseMethod("alias_column_names", object = structure(list(), class = sensor))
}

#' @export
#' @keywords internal
alias_column_names.accelerometer <- function(names, sensor) {
  dplyr::case_match(
    names,
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
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.activity <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.airquality <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("airQualityIndex", "air_quality_index") ~ air_quality_index,
    c("airQualityLevel", "air_quality_level") ~ air_quality_level,
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.appusage <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("startDate", "start_date") ~ "start",
    c("endDate", "end_date") ~ "end",
    c("appName", "app_name") ~ "app",
    c("packageName") ~ "package_name",
    c("lastForeground") ~ "last_foreground",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.battery <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("batteryLevel") ~ "battery_level",
    c("batteryStatus") ~ "battery_status",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.bluetooth <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("scanResult") ~ "scan_result",
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
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.connectivity <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("connectivityStatus") ~ "connectivity_status",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.device <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("deviceId") ~ "device_id",
    c("deviceName") ~ "device_name",
    c("deviceManufacturer") ~ "device_manufacturer",
    c("device_model") ~ "device_model",
    c("operatingSystem") ~ "operating_system",
    c("operatingSystemVersion") ~ "operating_system_version",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.error <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.geofence <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.gyroscope <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.heartbeat <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("deviceType") ~ "device_type",
    c("deviceRoleName") ~ "device_role_name",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.keyboard <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.light <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("meanLux") ~ "mean_lux",
    c("stdLux") ~ "std_lux",
    c("minLux") ~ "min_lux",
    c("maxLux") ~ "max_lux",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.location <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("verticalAccuracy") ~ "vertical_accuracy",
    c("headingAccuracy") ~ "heading_accuracy",
    c("speedAccuracy") ~ "speed_accuracy",
    c("isMock") ~ "is_mock",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.memory <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("freePhysicalMemory") ~ "free_physical_memory",
    c("freeVirtualMemory") ~ "free_virtual_memory",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.noise <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("meanDecibel") ~ "mean_decibel",
    c("stdDecibel") ~ "std_decibel",
    c("minDecibel") ~ "min_decibel",
    c("maxDecibel") ~ "max_decibel",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.pedometer <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("stepCount", "steps") ~ "step_count",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.screen <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    c("screenEvent") ~ "screen_event",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.timezone <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.weather <- function(names, sensor) {
  dplyr::case_match(
    names,
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
    .default = names,
    .ptype = character()
  )
}

#' @export
#' @keywords internal
alias_column_names.wifi <- function(names, sensor) {
  dplyr::case_match(
    names,
    c("id") ~ "measurement_id",
    c("timestamp", "start_time") ~ "time",
    .default = names,
    .ptype = character()
  )
}
