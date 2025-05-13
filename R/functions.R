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
    mutate(device = 'cgm')

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
           end_date_time = ymd_hms(end_date_time),
           steps = as.numeric(value))
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

align_to_ref_time_safe <- function(df) {
  if ("bg" %in% names(df)) {
    return(df %>% select(time_s, everything()))
  }

  time_s <- extract_time_s(df)

  # Data range limits
  time_min <- min(time_s, na.rm = TRUE)
  time_max <- max(time_s, na.rm = TRUE)

  data_cols <- setdiff(names(df), c("time_s", "timestamp", "date_time", "start_date_time", "end_date_time", "uuid", "device", "unit", "activity_name"))

  aligned_df <- map_dfc(data_cols, function(col) {
    if (is.numeric(df[[col]])) {
      # Interpolate only inside range, NA outside
      approx_y <- approx(
        x = time_s,
        y = df[[col]],
        xout = ref_time_s,
        rule = 1  # 1 = NA outside range
      )$y
      tibble(!!col := approx_y)
    } else {
      # Nearest only inside range, NA outside
      nearest_values <- sapply(ref_time_s, function(rt) {
        if (rt < time_min || rt > time_max) {
          return(NA)
        } else {
          idx <- which.min(abs(time_s - rt))
          df[[col]][idx]
        }
      })
      tibble(!!col := nearest_values)
    }
  })

  bind_cols(tibble(time_s = ref_time_s), aligned_df)
}



