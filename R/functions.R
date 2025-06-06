# Create a custom JSON reader function that handles NaN values
my_read_json <- function(file_path) {
  # First attempt to read with standard jsonlite
  tryCatch({
    jsonlite::fromJSON(file_path)
  }, error = function(e) {
    # If that fails, try a more robust approach:
    # Read the file as text
    json_text <- readLines(file_path, warn = FALSE)

    # Replace NaN with null (which is valid in JSON)
    json_text <- gsub(":\\s*NaN", ": null", json_text)

    # Parse the modified JSON text
    jsonlite::fromJSON(paste(json_text, collapse = ""))
  })
}

# Example usage in your targets pipeline:
# tar_target(hr_json,
#            read_json(hr_json_paths),
#            pattern = map(hr_json_paths),
#            iteration = 'list')


# Data processing functions -----------------------------------------------

processCGM <- function(json_dat) {
  json_dat |>
    tibble() |>
    unnest_wider(json_dat) |>
    select(uuid, patient_id, cgm) |>
    mutate(uuid = unique(uuid[!is.na(uuid)])) |>
    unnest_longer(cgm) |>
    hoist(
      cgm,
      bg = c('blood_glucose', 'value'),
      timestamp = c('effective_time_frame', 'time_interval', 'start_date_time'),
      time_s = c('transmitter_time', 'value')
    ) |>
    select(-patient_id, -cgm) |>
    mutate(device = 'cgm') |>
    mutate(bg = map_dbl(bg, function(x) {
      if (is.null(x) || length(x) == 0) {
        return(NA_real_)
      } else {
        # Handle multiple elements by taking first
        first_element <- x[1]
        # Suppress warnings for character to numeric conversion
        result <- suppressWarnings(as.numeric(first_element))
        return(result)
      }
    }))

}

processHR <- function(json_dat) {
  json_dat |>
    tibble() |>
    unnest_wider(json_dat) |>
    select(uuid, heart_rate) |>
    mutate(uuid = unique(uuid[!is.na(uuid)])) |>
    unnest_longer(heart_rate) |>
    unnest_wider(heart_rate) |>
    unnest_wider(heart_rate) |>
    unnest_wider(effective_time_frame) |>
    rename(hr = value, hr_units = unit) |>
    mutate(timestamp = ymd_hms(date_time),
           time_s = as.numeric(difftime(timestamp, first(timestamp), units = "secs")))
}

processOxSat <- function(json_dat) {
  json_dat |>
    tibble() |>
    unnest_wider(json_dat) |>
    select(uuid, user_id, breathing) |>
    mutate(uuid = unique(uuid[!is.na(uuid)])) |>
    unnest_longer(breathing) |>
    unnest_wider(breathing) |>
    unnest_wider(oxygen_saturation) |>
    unnest_wider(effective_time_frame) |>
    mutate(timestamp = ymd_hms(date_time)) |>
    select(-user_id, -unit, -date_time) |>
    rename(device = measurement_method,
           ox_sat_percent = value)
}

processPhysicalActivity <- function(json_dat) {
  json_dat |>
    tibble() |>
    unnest_wider(json_dat) |>
    select(uuid, user_id, activity) |>
    mutate(uuid = unique(uuid[!is.na(uuid)])) |>
    unnest_longer(activity) |>
    unnest_wider(activity) |>
    unnest_wider(base_movement_quantity) |>
    unnest_wider(effective_time_frame) |>
    unnest_wider(time_interval) |>
    select(-user_id) |>
    mutate(start_date_time = ymd_hms(start_date_time),
           timestamp = ymd_hms(end_date_time),
           steps = as.numeric(value)) |>
    select(-value)
}

processCalories <- function(json_dat) {
  # First check if json_dat is empty or NULL
  if (is.null(json_dat) || length(json_dat) == 0) {
    warning("Empty or NULL input data")
    return(tibble())
  }

  # Convert to tibble and handle potential issues
  tbl_data <- tibble(json_dat) |>
    unnest_wider(json_dat)

  # Early return if no activity data is present
  if (!"activity" %in% names(tbl_data) || all(is.na(tbl_data$activity))) {
    warning("No activity data found")
    return(tibble(
      uuid = unique(tbl_data$uuid[!is.na(tbl_data$uuid)]),
      timestamp = as.POSIXct(NA),
      kcal = NA_real_
    ))
  }

  # Process the data for non-NA activities
  tbl_data |>
    select(uuid, user_id, activity) |>
    mutate(uuid = unique(uuid[!is.na(uuid)])) |>
    # Filter out NA activity before unnesting
    filter(!is.na(activity)) |>
    unnest_longer(activity) |>
    unnest_wider(activity) |>
    # Check if calories_value exists before unnesting
    mutate(
      has_calories = !is.na(calories_value)
    ) |>
    filter(has_calories) |>
    select(-has_calories) |>
    unnest_wider(calories_value) |>
    unnest_wider(effective_time_frame) |>
    select(-user_id, -unit) |>
    rename(kcal = value) |>
    mutate(timestamp = ymd_hms(date_time)) |>
    select(-date_time)
}

processRespRate <- function(json_dat) {
  json_dat |>
    tibble() |>
    unnest_wider(json_dat) |>
    select(uuid, user_id, breathing) |>
    mutate(uuid = unique(uuid[!is.na(uuid)])) |>
    unnest_longer(breathing) |>
    unnest_wider(breathing) |>
    unnest_wider(respiratory_rate) |>
    unnest_wider(effective_time_frame) |>
    select(-user_id) |>
    mutate(timestamp = ymd_hms(date_time)) |>
    rename(resp_rate = value)
}

processStress <- function(json_dat) {
  json_dat |>
    tibble() |>
    unnest_wider(json_dat) |>
    select(uuid, user_id, stress) |>
    mutate(uuid = unique(uuid[!is.na(uuid)])) |>
    unnest_longer(stress) |>
    unnest_wider(stress) |>
    unnest_wider(stress) |>
    unnest_wider(effective_time_frame) |>
    mutate(timestamp = ymd_hms(date_time)) |>
    select(-user_id, -date_time) |>
    rename(stress = value)

}

processEnvironment <- function(csv_path) {
  dat <- read_csv(csv_path, skip = 45)
  basename_file <- basename(csv_path)
  dat |>
    mutate(uuid = paste0('AIREADI-', str_remove(basename_file, "_ENV\\.csv")))
}

processSleep <- function(json_dat) {
  # Check if json_dat is empty or NULL
  if (is.null(json_dat) || length(json_dat) == 0) {
    warning("Empty or NULL input data")
    return(tibble())
  }

  # Convert to tibble and check column existence
  tryCatch({
    tbl_data <- tibble(json_dat) |>
      unnest_wider(json_dat)

    # Create an empty result with the expected structure if required columns are missing
    required_cols <- c("uuid", "sleep")
    missing_cols <- required_cols[!required_cols %in% names(tbl_data)]

    if (length(missing_cols) > 0) {
      warning(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))

      # Return empty tibble with expected structure
      return(tibble(
        uuid = character(0),
        sleep_type = character(0),
        start_date_time = as.POSIXct(character(0)),
        end_date_time = as.POSIXct(character(0))
      ))
    }

    # Handle missing uuid by generating a random one if needed
    if (all(is.na(tbl_data$uuid))) {
      random_uuid <- paste0("generated-", format(Sys.time(), "%Y%m%d%H%M%S"))
      tbl_data$uuid <- random_uuid
      warning("All UUID values were NA. Generated a placeholder UUID.")
    } else {
      tbl_data <- tbl_data |>
        mutate(uuid = unique(uuid[!is.na(uuid)]))
    }

    # Early return if no sleep data is present
    if (!"sleep" %in% names(tbl_data) || all(is.na(tbl_data$sleep))) {
      warning("No sleep data found")
      return(tibble(
        uuid = unique(tbl_data$uuid),
        sleep_type = character(0),
        start_date_time = as.POSIXct(character(0)),
        end_date_time = as.POSIXct(character(0))
      ))
    }

    # Process the data
    result <- tbl_data |>
      select(uuid, user_id, sleep) |>
      filter(!is.na(sleep)) |>
      unnest_longer(sleep) |>
      unnest_wider(sleep)

    # Check if sleep_stage_time_frame exists
    if (!"sleep_stage_time_frame" %in% names(result) ||
        all(is.na(result$sleep_stage_time_frame))) {
      warning("No sleep_stage_time_frame data found")
      return(tibble(
        uuid = unique(tbl_data$uuid),
        sleep_type = character(0),
        start_date_time = as.POSIXct(character(0)),
        end_date_time = as.POSIXct(character(0))
      ))
    }

    result <- result |>
      unnest_wider(sleep_stage_time_frame) |>
      unnest_wider(time_interval) |>
      select(-user_id) |>
      mutate(
        start_date_time = ymd_hms(start_date_time),
        end_date_time = ymd_hms(end_date_time)
      )

    return(result)

  }, error = function(e) {
    warning(paste("Error processing sleep data:", e$message))
    return(tibble(
      uuid = character(0),
      sleep_type = character(0),
      start_date_time = as.POSIXct(character(0)),
      end_date_time = as.POSIXct(character(0))
    ))
  })
}

extract_time_s <- function(df) {
  if ("time_s" %in% names(df)) {
    return(df$time_s)
  } else if ("timestamp" %in% names(df)) {
    return(as.numeric(df$timestamp))
  } else if ("date_time" %in% names(df)) {
    return(as.numeric(as.POSIXct(df$date_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
  } else if ("start_date_time" %in% names(df)) {
    return(as.numeric(df$start_date_time))
  } else {
    stop("No recognizable time column")
  }
}


extract_one_per_list <- function(list_of_dfs, target_uuid) {
  # Find first df where uuid matches
  matched_df <- keep(list_of_dfs, ~ target_uuid %in% .x$uuid)
  if (length(matched_df) > 0) {
    return(matched_df[[1]])
  } else {
    return(NULL)
  }
}


align_timeseries <- function(df_list, time_interval = 60, max_gap_linear = 3600) {

  # Extract all unique timestamps and create common time grid
  all_timestamps <- df_list %>%
    map(~ {
      ts <- .x$timestamp
      # Handle both character and POSIXct timestamps
      if (is.character(ts)) {
        as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
      } else {
        as.POSIXct(ts, tz = "UTC")
      }
    }) %>%
    unlist() %>%
    as.POSIXct(origin = "1970-01-01", tz = "UTC")

  time_range <- range(all_timestamps, na.rm = TRUE)
  common_grid <- seq(from = time_range[1],
                     to = time_range[2],
                     by = time_interval)

  # Identify value columns (exclude timestamp, uuid, and other metadata)
  get_value_cols <- function(df) {
    exclude_cols <- c("timestamp", "uuid", "device", "activity_name",
                      "hr_units", "date_time", "time_s", "unit",
                      "start_date_time", "value")
    names(df)[!names(df) %in% exclude_cols & sapply(df, is.numeric)]
  }

  # Process each dataframe
  aligned_list <- imap(df_list, function(df, idx) {
    value_cols <- get_value_cols(df)

    if (length(value_cols) == 0) return(NULL)

    # Convert timestamps to POSIXct consistently
    if (is.character(df$timestamp)) {
      df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    } else {
      df$timestamp <- as.POSIXct(df$timestamp, tz = "UTC")
    }

    # Convert cumulative variables to incremental
    cumulative_vars <- c("kcal", "steps")
    df_processed <- df

    for (cum_var in cumulative_vars) {
      if (cum_var %in% names(df_processed)) {
        df_processed <- df_processed %>%
          arrange(timestamp) %>%
          mutate(!!sym(cum_var) := {
            diffs <- c(0, diff(!!sym(cum_var)))
            # Set negative values (resets) to the current value instead
            ifelse(diffs < 0, !!sym(cum_var), diffs)
          })
      }
    }

    # Create base dataframe with common time grid
    base_df <- data.frame(
      timestamp = common_grid,
      uuid = df$uuid[1]
    )

    # For each value column, interpolate to common grid
    for (col in value_cols) {
      # Remove rows with missing timestamps or values
      clean_data <- df_processed[!is.na(df_processed$timestamp) & !is.na(df_processed[[col]]), ]

      if (nrow(clean_data) >= 2) {  # Changed from > 1 to >= 2 for clarity
        # Interpolate using approx
        interpolated <- approx(x = as.numeric(clean_data$timestamp),
                               y = clean_data[[col]],
                               xout = as.numeric(common_grid),
                               method = "linear",
                               rule = 1)$y

        base_df[[col]] <- interpolated
      } else if (nrow(clean_data) == 1) {
        # If only one data point, use constant interpolation
        base_df[[col]] <- clean_data[[col]][1]
      } else {
        base_df[[col]] <- NA
      }
    }

    # Add source identifier
    base_df$source <- paste0("df_", idx)

    return(base_df)
  })

  # Remove NULL entries and combine
  aligned_list <- aligned_list[!sapply(aligned_list, is.null)]

  # Full join all dataframes on timestamp and uuid
  result <- reduce(aligned_list, function(x, y) {
    full_join(x, y, by = c("timestamp", "uuid"))
  })

  return(result)
}

