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
  sensor_specs <- map(.x = specifications,
                       .f = ~keep(.x, names(.x) %in% tolower(sensors)))

  # 1. Identify gaps
  # Is there at least one sensor in a specification where gaps are used? If not, we skip all steps
  # with gaps
  specs_with_gaps <- map_lgl(.x = sensor_specs,
                             .f = \(spec){
                               map_lgl(.x = spec,
                                       .f = ~pluck(.x$gaps$gaps)) |>
                                 some(.x = _, .p = isTRUE)
                             })

  if (any(specs_with_gaps)) {
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_progress_step("Finding gaps", spinner = TRUE)
    }

    gaps <- map2(.x = specifications,
                 .y = specs_with_gaps,
                 .f = \(spec, has_gaps) {
                   if (requireNamespace("cli", quietly = TRUE)) {
                     cli::cli_progress_update()
                   }

                   if (!has_gaps) {
                     return(NULL)
                   }

                   mem_identify_gaps(db = db,
                                     sensor = spec$gaps$gap_sensors,
                                     min_gap = spec$gaps$min_gap) |>
                     mutate(dplyr::across(c(to, from), lubridate::ymd_hms)) |>
                     mutate(dplyr::across(c(to, from), lubridate::with_tz, tzone = spec$gaps$tz))
                 })
    memoise::forget(mem_identify_gaps)

    # 2. Link gaps to ESM data
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_progress_step("Linking gaps to ESM", spinner = TRUE)
    }
    esm <- pmap(.l = list(esm, gaps, specifications, specs_with_gaps),
                .f = \(data, gap_data, specs, has_gaps) {
                  if (requireNamespace("cli", quietly = TRUE)) {
                    cli::cli_progress_update()
                  }

                  if (!has_gaps) {
                    return(data)
                  }

                  data |>
                    mem_link_gaps(gaps = gap_data,
                                  by = "participant_id",
                                  offset_before = specs$time_windows$time_window_before,
                                  offset_after = specs$time_windows$time_window_after,
                                  raw_data = FALSE) |>
                    mutate(gap = gap / (specs$time_windows$time_window_before +
                                          specs$time_windows$time_window_after))
                })
    memoise::forget(mem_link_gaps)
  }

  # Step 3 to 6: Get the feature data, add the gaps, link to the ESM data, and calculate the feature
  # Do this per sensor instead of per specification, as everything at once is often too large to fit
  # in memory.

  # The sensors across all specifications
  snsrs <- map(sensor_specs, \(spec) names(spec)) |>
    unlist(recursive = FALSE, use.names = FALSE) |>
    unique()

  # Transpose the list so we can loop over the sensors
  trans_sensor_specs <- purrr::list_transpose(sensor_specs, template = snsrs)
  features <- pmap(.l = list(names(trans_sensor_specs),
                             trans_sensor_specs),
                   .f = \(sensor_name, sensor_spec) {
                     add_features_by_sensor(sensor = sensor_name,
                                            sensor_spec = sensor_spec,
                                            esm = esm,
                                            specifications = specifications,
                                            db = db,
                                            gaps = gaps)
                   })


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

add_features_by_sensor <- function(sensor, sensor_spec, esm, specifications, db, gaps) {
  if (requireNamespace("cli", quietly = TRUE)) {
    step_id <- cli::cli_progress_step("Adding {tolower(sensor)} features...",
                                      msg_done = "Successfully added {tolower(sensor)} features!",
                                      current = FALSE,
                                      .auto_close = TRUE)
    cli::cli_progress_bar(
      total = 4,
      status = "",
      format = paste("{cli::pb_bar} {cli::pb_current}/{cli::pb_total}",
                     "{cli::pb_status} | {cli::pb_elapsed}"),
    )
  }

  # 3. Get the feature data
  browser()
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_progress_update(status = "Retrieving data")
  }

  # Cache for GPS coordinates
  cache <- memoise::cache_memory()
  feature_data <- map(.x = sensor_spec,
                      .f = ~mem_feature_data(
                        db = db,
                        sensor = sensor,
                        cols = NULL,
                        .x$preprocess,
                        cache = cache
                      ))
  memoise::forget(mem_feature_data)
  cache$reset()

  # Only retain participants that occur in the sensor data
  # In the second part: Loop over both esm and feature data, then again loop over each sensor to
  # replicate the ESM and apply a semi_join.

  features <- esm |>
    map(.f = ~select(.x, all_of(c("participant_id", "time")), any_of("gap"))) |>
    map2(.x = _,
         .y = feature_data,
         .f = ~semi_join(.x, .y, by = "participant_id"),
         .x)

  # 4. Add gaps to the sensor data, only if gaps$gaps is TRUE
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_progress_update(status = "Adding gaps")
  }
  feature_data <- pmap(.l = list(feature_data, sensor_spec, gaps),
                       .f = function(data, spec, gaps) {
                         if (!spec$gaps$gaps) {
                           return(data)
                         }
                         mem_add_gaps(data = data,
                                      gaps = gaps,
                                      by = "participant_id",
                                      continue = spec$gaps$continue,
                                      fill = spec$gaps$fill)
                       })

  memoise::forget(mem_add_gaps)

  # 5. Link sensor data to ESM
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_progress_update(status = "Linking to ESM")
  }
  features <- specifications %>%
    map(.f = ~keep(.x, !(names(.x) %in% tolower(sensors)))) %>%
    pmap(.l = list(features, feature_data, sensor_spec, .),
         .f = ~mem_link(x = ..1,
                        y = ..2,
                        by = "participant_id",
                        time = time,
                        y_time = time,
                        offset_before = ..4$time_windows$time_window_before,
                        offset_after = ..4$time_windows$time_window_after,
                        add_before = ..3$link$add_before,
                        add_after = ..3$link$add_after))
  memoise::forget(mem_link)
  rm(feature_data)

  # 6. Calculate the features
  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_progress_update(status = "Calculating features")
  }
  features <- map(.x = features,
                  .f = ~`class<-`(.x, c(tolower(sensor), class(.x))))
  features <- pmap(.l = list(features, sensor_specs, specifications),
                   .f = ~mem_map_feature(..1,
                                         ..2,
                                         start = ..3$time_windows$time_window_before,
                                         end = ..3$time_windows$time_window_before))

  # features2 <- features
  # for(i in seq_along(features[[1]])) {
  #   sensor <- names(features[[1]][i])
  #   print(sensor)
  #
  #   for (j in seq_along(features)) {
  #     print(j)
  #     features2[[j]][[sensor]] <- map_feature(
  #       features[[j]][[sensor]],
  #       sensor_specs[[j]][[sensor]]$feature,
  #       start = specifications[[j]]$time_windows$time_window_before,
  #       end = specifications[[j]]$time_windows$time_window_after
  #     )
  #   }
  # }

  memoise::forget(mem_map_feature)

  if (requireNamespace("cli", quietly = TRUE)) {
    cli::cli_progress_done(id = step_id)
  }
  return(features)
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
    assign("mem_add_gaps",  memoise::memoise(add_gaps), envir = env)
  }
  if (!exists("mem_link") || !memoise::is.memoised(mem_link)) {
    assign("mem_link", memoise::memoise(link), envir = env)
  }
  if (!exists("mem_map_feature") || !memoise::is.memoised(mem_map_feature)) {
    assign("mem_map_feature", memoise::memoise(map_feature), envir = env)
  }

  return(invisible(TRUE))
}
