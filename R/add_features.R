# Features =================

source(
  file = file.path(path, "feature implementations.R"),
  echo = FALSE, verbose = FALSE
)

# make_specifications <- function(...) {
#   dots <- rlang::list2(...)
#
#   specs_internal <- function(prev_value, next_value, name) {
#     next_value %>%
#       purrr::transpose(.) %>%
#       enframe(name = NULL, value = name) %>%
#       bind_cols(prev_value, .)
#   }
#
#   if (length(dots) >= 2) {
#     out <- purrr::reduce2(dots, names(dots), specs_internal, .init = NULL)
#   } else {
#     out <- dots
#   }
#
#   out <- out %>%
#     purrr::transpose(.) %>%
#     enframe(name = NULL, value = "specifications")
#
#   out
# }

#' Add features to ESM data
#'
#' @param esm
#' @param specifications
#' @param db
#'
#' @details
#' 1. [identify_gaps()]: Identifying gaps in the data (if needed).
#' 2. [link_gaps()]: Linking gaps to the ESM data (if needed).
#' 3. [feature_data()]: Extracting feature-specific data for each sensor.
#' 4. [add_gaps()]: Adding gaps to the feature data (if needed).
#' 5. [link()]: Linking the feature data to the ESM data within a time window.
#' 6. Various calls to sensor-specific functions, such as [activity_duration()],
#' [noise_intensity()], or [step_count()].
#'
#' @return
#' @export
add_features <- function(esm, specifications, db) {
  # Argument checks
  ensure_suggested_package("memoise")
  check_arg(esm, c("list", "data.frame"))
  check_arg(specifications, "list")
  check_db(db)

  # Create memoised functions
  mem_add_features_funs(env = environment())

  # The sensor specifications
  sensor_specs <-  map(.x = specifications,
                       .f = ~keep(.x, names(.x) %in% tolower(sensors)))

  # Extract gaps
  # TODO: Check if any of the specifications use gaps
  # TODO: Only extract gaps for specifications where gaps = TRUE
  gaps <- map(.x = specifications,
              .f = function(.x) {
                mem_identify_gaps(db = db,
                                  sensor = .x$gaps$gap_sensors,
                                  min_gap = .x$gaps$min_gap) %>%
                  mutate(dplyr::across(c(to, from), lubridate::ymd_hms)) %>%
                  mutate(dplyr::across(c(to, from), lubridate::with_tz, tzone = .x$gaps$tz))
              })
  memoise::forget(mem_identify_gaps)

  # Link gaps to ESM data
  # TODO: Only link gaps for specifications where gaps = TRUE
  esm <- pmap(.l = list(esm, gaps, specifications),
              .f = ~mem_link_gaps(data = ..1,
                                  gaps = ..2,
                                  by = "participant_id",
                                  offset_before = ..3$time_windows$time_window_before,
                                  offset_after = ..3$time_windows$time_window_after,
                                  raw_data = FALSE)
  ) %>%
    map2(.x = .,
         .y = specifications,
         ~mutate(.x, gap = gap / (.y$time_windows$time_window_before + .y$time_windows$time_window_after)))
  memoise::forget(mem_link_gaps)

  # Get the data
  feature_data <- sensor_specs %>%
    map(.f = ~imap(.x = .x,
                   .f = ~mem_feature_data(
                     db = db,
                     sensor = .y,
                     cols = NULL,
                     .x$preprocess,
                     cache = memoise::cache_memory()
                   )))

  memoise::forget(mem_feature_data)

  # Only retain participants that occur in the sensor data
  # In the second part: Loop over both esm and feature data, then again loop over each sensor to
  # replicate the ESM and apply a semi_join.
  features <- esm %>%
    map(.f = ~select(.x, all_of(c("participant_id", "time")), any_of("gap"))) %>%
    map2(.y = feature_data,
         .f = ~map(.x = .y,
                   .f = ~semi_join(.y, .x, by = "participant_id"),
                   .x))

  # Add gaps, only if gaps$gaps is TRUE
  feature_data <- pmap(.l = list(feature_data, sensor_specs, gaps),
                       .f = ~map2(.x = ..1,
                                  .y = ..2,
                                  .f = function(data, spec, gaps) {
                                    if (spec$gaps$gaps) {
                                      mem_add_gaps(data = data,
                                                   gaps = gaps,
                                                   by = "participant_id",
                                                   continue = spec$gaps$continue,
                                                   fill = spec$gaps$fill)
                                    } else data
                                  }, ..3))

  memoise::forget(mem_add_gaps)
  rm(gaps)

  # Link to ESM
  features <- specifications %>%
    map(.f = ~keep(.x, !(names(.x) %in% tolower(sensors)))) %>%
    pmap(.l = list(features, feature_data, sensor_specs, .),
         .f = ~pmap(.l = list(..1, ..2, ..3),
                    .f = ~link(x = ..1,
                               y = ..2,
                               by = "participant_id",
                               offset_before = ..4$time_windows$time_window_before,
                               offset_after = ..4$time_windows$time_window_after,
                               add_before = ..3$link$add_before,
                               add_after = ..3$link$add_after),
                    ..4))
  memoise::forget(mem_link)
  rm(feature_data)

  # Calculate the features
  features <- map(.x = features,
                  .f = ~imap(.x = .x,
                             .f = ~`class<-`(.x, c(tolower(.y), class(.x)))))
  features <- map2(.x = features,
                   .y = sensor_specs,
                   .f = ~map2(.x = .x,
                              .y = .y,
                              .f = ~map_feature(.x, .y$feature)))

  memoise::forget(mem_map_feature)


  # For discarding non-implemented functions
  features <- map(features, ~purrr::discard(.x, .p = ~all(is.na(.x))))

  # Merge features for different sensors
  features <- map(.x = features,
                  .f = ~purrr::reduce(
                    .x = .x,
                    .f = ~left_join(.x, .y, by = intersect(colnames(.x), colnames(.y)))))


  # Finally, add the features to the ESM data
  esm <- map2(.x = esm,
              .y = features,
              .f = ~left_join(.x, .y, by = intersect(colnames(.x), colnames(.y))))

  esm
}

mem_add_features_funs <- function(env = rlang::caller_env()) {
  cm <- memoise::cache_filesystem(tempdir())

  if (!exists("mem_identify_gaps") || !memoise::is.memoised(mem_identify_gaps)) {
    assign("mem_identify_gaps", memoise::memoise(identify_gaps, cache = cm), envir = env)
  }
  if (!exists("mem_link_gaps") || !memoise::is.memoised(mem_link_gaps)) {
    assign("mem_link_gaps", memoise::memoise(link_gaps), envir = env)
  }
  if (!exists("mem_feature_data") || !memoise::is.memoised(mem_feature_data)) {
    assign("mem_feature_data", memoise::memoise(feature_data), envir = env)
  }
  if (!exists("mem_add_gaps") || !memoise::is.memoised(mem_add_gaps)) {
    assign("mem_add_gaps",  memoise::memoise(add_gaps), envir= env)
  }
  if (!exists("mem_link") || !memoise::is.memoised(mem_link)) {
    assign("mem_link", memoise::memoise(link), envir = env)
  }
  if (!exists("mem_map_feature") || !memoise::is.memoised(mem_map_feature)) {
    assign("mem_map_feature", memoise::memoise(map_feature), envir = env)
  }

  return(invisible(TRUE))
}

## Activity
add_activity_features <- function(specifications, gaps) {
  # Get the data
  activity_data <- get_activity_data()

  # Add the gaps
  activity_data <- add_gaps(data = activity_data,
                            gaps = gaps,
                            by = participant_id,
                            fill = list(confidence = 100, type = "GAP"))

  # Link to ESM
  specifications <- specifications %>%
    mutate(activity_duration = map(.x = .data$esm,
                                   .f = ~select(.x, participant_id, time))) %>%
    mutate(activity_duration = pmap(.l = list(activity_duration,
                                              time_window_before,
                                              time_window_after),
                                    .f = ~link(x = ..1,
                                               y = activity_data,
                                               by = "participant_id",
                                               offset_before = ..2,
                                               offset_after = ..3,
                                               add_before = TRUE,
                                               add_after = TRUE)
    ))

  # Calculate which beeps have more than 50% UNKNOWN
  # Always fill in 0 when activity$UNKNOWN is false
  # Cull beeps with more than 50% UNKNNOWN
  specifications <- specifications %>%
    mutate(activity_duration = map2(.x = activity_duration,
                                    .y = map_lgl(.x = specifications$activity,
                                                 .f = ~pull(.x, UNKNOWN)),
                                    .f = map_activity_over_unknown)) %>%
    mutate(activity_duration = map(.x = activity_duration,
                                   .f = ~filter(.x, activity_unknown <= 50)))

  # Now, re-link without UNKNOWNs (if necessary)
  activity_data <- activity_data %>%
    filter(type != "UNKNOWN")

  specifications <- specifications %>%
    mutate(activity_duration = map(.x = activity_duration,
                                   .f = ~select(.x, -c(data, activity_unknown)))) %>%
    mutate(activity_duration = pmap(.l = list(activity_duration,
                                              time_window_before,
                                              time_window_after),
                                    .f = ~link(x = ..1,
                                               y = activity_data,
                                               by = "participant_id",
                                               offset_before = ..2,
                                               offset_after = ..3,
                                               add_before = TRUE,
                                               add_after = TRUE)
    ))

  # And recalculate duration
  specifications <- specifications %>%
    mutate(activity_duration = map(.x = activity_duration,
                                   .f = map_activity_duration))

  # Finally, add the features to the full data set
  specifications <- specifications %>%
    mutate(data = map2(.x = data,
                       .y = activity_duration,
                       .f = ~left_join(.x, .y, by = c("participant_id", "time")))) %>%
    select(-activity_duration)

  # Clean-up
  rm(activity_data)
}

## Screen ============
# TODO: Normalise screen count with gaps duration
add_screen_features <- function(specifications, gaps) {
  # Get the data
  screen_data <- get_screen_data()

  # Add the gaps
  screen_data <- add_gaps(data = screen_data,
                          gaps = gaps,
                          by = "participant_id",
                          fill = list(screen_event = "GAP"))

  # Only retain participants that occur in the screen data
  specifications <- specifications %>%
    mutate(screen_duration = map(.x = esm,
                                 .f = ~select(.x, participant_id, time, gap))) %>%
    mutate(screen_duration = map(.x = screen_duration,
                                 .f = ~semi_join(.x, screen_data, by = "participant_id")))

  # Link to ESM
  specifications <- specifications %>%
    mutate(screen_duration = pmap(.l = list(screen_duration,
                                            time_window_before,
                                            time_window_after),
                                  .f = ~link(x = ..1,
                                             y = screen_data,
                                             by = "participant_id",
                                             offset_before = ..2,
                                             offset_after = ..3,
                                             add_before = TRUE,
                                             add_after = TRUE)
    ))

  # Screen unlocks and duration
  specifications <- specifications %>%
    mutate(screen_unlocks = map(.x = screen_duration,
                                .y = time_window,
                                .f = map_screen_unlocks)) %>%
    mutate(screen_duration = map(.x = screen_duration,
                                 .f = map_screen_duration))

  # Finally, add the features to the full data set
  specifications <- specifications %>%
    mutate(data = map2(.x = data,
                       .y = screen_unlocks,
                       .f = ~left_join(.x, .y, by = c("participant_id", "time")))) %>%
    mutate(data = map2(.x = data,
                       .y = screen_duration,
                       .f = ~left_join(.x, .y, by = c("participant_id", "time")))) %>%
    select(-c(screen_unlocks, screen_duration))

  # Clean-up
  rm(screen_data)
}


