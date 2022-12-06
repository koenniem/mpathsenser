#' Create a default specification list
#'
#' @param time_window_before The time period before each beep, as in [link()].
#' @param time_window_after The time period after each beep, as in [link()].
#' @param min_gap he minimum time (in seconds) passed between two subsequent measurements for it to
#'   be considered a gap, as in [identify_gaps()].
#' @param gap_sensors One or multiple \code{\link[mpathsenser]{sensors}}, as in [identify_gaps()].
#'   See sensors for a list of available sensors.
#' @param tz The time zone to be used in processing the data. See [OlsonNames()] for a full list of
#'   the available time zones.
#' @param ... Arguments passed on to [add_sensor_spec()].
#'
#' @return A list containing sensor-specific arguments and time_window arguments. See the details
#'   section for a more precise description.
#' @export
#'
#' @examples
#' create_specs(1800, 0)
#' #'
#' # To modify a single parameter in a list of specifications,
#' # use purrr's assign_in:
#' specs <- list(create_specs(1800L, 0), create_specs(0, 1800))
#' purrr::map(.x = specs,
#'            .f = purrr::assign_in,
#'            where = c("gyroscope", "gaps", "gaps"),
#'            value = TRUE)
#'
#' # Alternatively, it may be added to replace the sensor entirely
#' # if many values need to be changed. This is possible using
#' # add_sensor_spec() (note it replaces all values for that sensor):
#' purrr::map(.x = specs,
#'            .f = add_sensor_spec,
#'            sensor = "gyroscope",
#'            preprocess_bin = FALSE,
#'            preprocess_moving_average = FALSE,
#'            gaps = TRUE,
#'            gaps_continue = TRUE,
#'            link_add_after = TRUE,
#'            link_add_before = TRUE)
create_specs <- function(time_window_before,
                         time_window_after,
                         min_gap = 60L,
                         gap_sensors = sensors,
                         tz = Sys.timezone(),
                         defaults = NULL,
                         sensors = NULL,
                         ...) {

  check_arg(time_window_before, "integerish", n = 1)
  check_arg(time_window_after, "integerish", n = 1)
  check_arg(min_gap, "integerish", n = 1)
  check_arg(gap_sensors, "character")
  check_arg(tz, "character", n = 1)
  check_arg(defaults, "data.frame", allow_null = TRUE)

  sensor_names <- c("accelerometer", "activity", "appusage", "battery", "bluetooth",
                    "calendar", "connectivity", "gyroscope", "light", "location", "noise",
                    "pedometer", "screen", "weather", "wifi")


  # Create defaults if none provided
  if (is.null(defaults)) {
    defaults <- default_specs(tz = tz)
  }

  # Only provide specifications for certain sensors
  if (!is.null(sensors)) {
    defaults <- defaults[defaults$sensor %in% tolower(sensors), ]
  }

  sensor_list <-  vector("list", nrow(defaults))
  names(sensor_list) <- defaults$sensor

  out <- list(
    time_windows = list(
      time_window_before = time_window_before,
      time_window_after = time_window_after
    ),
    gaps = list(
      min_gap = min_gap,
      gap_sensors = gap_sensors,
      tz = tz
    )
  )
  out <- c(out, sensor_list)
  class(out) <- c("specifications", class(out))

  # Loop over each row in the defaults table and add sensor specifications cumulatively
  add_spec <- function(spec, sensor_row) {
    args <- c(list(specifications = spec), sensor_row)
    do.call(add_sensor_spec, args)
  }

  out <- Reduce(f = add_spec,
                x = asplit(defaults, 1),
                init = out)

  out
}

default_specs <- function(sensors = NULL, tz = Sys.timezone()) {
  sensor_names <- c("accelerometer", "activity", "appusage", "battery", "bluetooth",
                    "calendar", "connectivity", "gyroscope", "light", "location", "noise",
                    "pedometer", "screen", "weather", "wifi")

  defaults <- tibble(
    sensor = sensor_names,
    preprocess_bin = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                       FALSE, FALSE, FALSE, FALSE),
    preprocess_moving_average = c(60L, 0, 0, 0, 0, 0, 0, 60L, 0, 0, 0 , 0, 0, 0, 0),
    preprocess_tz = tz,
    gaps = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
             FALSE, TRUE),
    gaps_fill = list(
      NULL,
      list(confidence = 100, type = "GAP"),
      list(usage = NA, app = "GAP"),
      list(battery_level = NA, battery_status = "GAP"),
      NULL,
      NULL,
      list(connectivity = "GAP"),
      NULL,
      NULL,
      list(latitude = NA, longitude = NA),
      NULL,
      NULL,
      list(screen_event = "GAP"),
      NULL,
      list(ssid = "GAP", bssid = "GAP", ip = "GAP")
    ),
    link_add_before = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE,
                        TRUE, TRUE, TRUE),
    link_add_after = link_add_before
  )

  if (!is.null(sensors)) {
    defaults <- defaults[defaults$sensor %in% tolower(sensors), ]
  }

  defaults
}

#' Check that object is a valid specifications list
#'
#' @param specifications A specifications object, as created by [create_specs()].
#' @param arg An argument name as a string. This argument will be mentioned in error messages as the
#'   input that is at the origin of a problem.
#' @param call The execution environment of a currently running function.
#'
#' @return Returns `TRUE` invisibly.
validate_specifications <- function(specifications,
                                    arg = rlang::caller_arg(specifications),
                                    call = rlang::caller_env()) {
  abort_msg <- c(paste0("Argument `", arg, "` is not in a valid format."),
                 i = "Use `create_specs()` to create a new specification list.")

  if (!inherits(specifications, "specifications")) {
    abort(abort_msg, call = call, arg = arg)
  }

  spec_names <- c("time_windows", "gaps")
  sensor_names <- c("accelerometer", "activity", "appusage", "battery",
                    "bluetooth", "calendar", "connectivity", "gyroscope", "light", "location",
                    "noise", "pedometer", "screen", "weather", "wifi")
  if (!all(names(specifications) %in% spec_names) &&
      !any(names(specifications) %in% sensor_names)) {
    abort(abort_msg, call = call, arg = arg)
  }
  return(invisible(TRUE))
}

#' Add sensor-specific arguments to a specification list
#'
#' @param specifications A specific list as created by [create_specs()].
#' @param sensor The name of a sensor. See \link[mpathsenser]{sensors} for a list of available sensors.
#' @param preprocess_bin Whether raw sensor data should be binned to 1-minute intervals before
#'   processing. Corresponds to the `bin` argument in [feature_data()].
#' @param preprocess_moving_average Whether a moving average should be calculated for the raw sensor
#'   data before processing. Corresponds to the `moving_average` argument in [feature_data()].
#' @param preprocess_tz The time zone to be used in processing the data. See [OlsonNames()] for a
#'   full list of the available time zones.
#' @param gaps Should gaps be included?
#' @param gaps_continue Whether to continue the measurement(s) prior to the gap once the gap ends.
#'   Corresponds to the `continue` argument in [add_gaps()].
#' @param gaps_fill A named list of the columns to fill with default values for the extra
#'   measurements that are added because of the gaps. Corresponds to the `fill` argument in
#'   [add_gaps()].`
#' @param link_add_before Whether to add the last measurement before the start of each interval.
#'   Corresponds to the `add_before` argument in [link()].
#' @param link_add_after Whether to add the first measurement after the end of each interval.
#'   Corresponds to the `add_after` argument in [link()].
#' @param ...
#'
#' @return A list of specifications
#' @export
add_sensor_spec <- function(specifications,
                            sensor,
                            preprocess_bin = FALSE,
                            preprocess_moving_average = FALSE,
                            preprocess_tz = Sys.timezone(),
                            gaps = TRUE,
                            gaps_continue = FALSE,
                            gaps_fill = NULL,
                            link_add_before = TRUE,
                            link_add_after = TRUE,
                            ...) {

  preprocess_tz <- force(preprocess_tz)
  validate_specifications(specifications)
  check_sensors(sensor, n = 1)
  check_arg(preprocess_bin, "logical", n = 1)
  check_arg(preprocess_moving_average, c("logical", "integerish"), n = 1)
  check_arg(preprocess_tz, "character", n = 1)
  check_arg(gaps, "logical", n = 1)
  check_arg(gaps_continue, "logical", n = 1)
  check_arg(gaps_fill, "list", allow_null = TRUE)
  check_arg(link_add_before, "logical", n = 1)
  check_arg(link_add_after, "logical", n = 1)

  feature <- NULL
  if (rlang::dots_n(...) != 0) {
    feature <- rlang::list2(...)
  }

  # Check if sensor is supported (not all in mpathsenser::sensors are)
  if (!(tolower(sensor) %in% names(specifications))) {
    abort(paste0("Sensor ", sensor, " is not supported."))
  }

  specifications[[tolower(sensor)]] <- list(
    preprocess = list(
      bin = preprocess_bin,
      moving_average = preprocess_moving_average,
      tz = preprocess_tz
    ),
    gaps = list(
      gaps = gaps,
      continue = gaps_continue,
      fill = rlang::enexpr(gaps_fill)
    ),
    link = list(
      add_before = link_add_before,
      add_after = link_add_after
    ),
    feature = feature
  )

  specifications
}

