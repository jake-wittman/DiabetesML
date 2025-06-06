library(targets)
library(dplyr)
library(purrr)
library(lubridate)
tar_load(c(cgm_processed, hr_processed, physical_activity_calorie_processed, physical_activity_processed, ox_sat_processed))
all_lists <- list(cgm_processed, hr_processed, physical_activity_calorie_processed, physical_activity_processed, ox_sat_processed)

cgm <- bind_rows(cgm_processed)
hr <- bind_rows(hr_processed)
kcal <- bind_rows(physical_activity_calorie_processed)
steps <- bind_rows(physical_activity_processed)
ox2 <- bind_rows(ox_sat_processed)


# Step 1: Extract all uuids in each list
uuids_per_list <- map(all_lists, function(l) {
  map_chr(l, ~ unique(.x$uuid)[1])  # Assuming 1 uuid per data.frame
})

# Step 2: Find the intersection of uuids across all lists
common_uuids <- reduce(uuids_per_list, intersect)

# Check result
print(common_uuids)

# Step 3: Extract one example from each list that has the common uuid
extract_one_per_list <- function(list_of_dfs, target_uuid) {
  # Find first df where uuid matches
  matched_df <- keep(list_of_dfs, ~ target_uuid %in% .x$uuid)
  if (length(matched_df) > 0) {
    return(matched_df[[1]])
  } else {
    return(NULL)
  }
}

# Pick first common uuid (or loop through later if multiple)
target_uuid <- common_uuids[1]

# Extract one per list
example_per_list <- map(all_lists, extract_one_per_list, target_uuid = target_uuid)
example_per_list <- map(example_per_list, function(.x) {
  if ('timestamp' %in% names(.x)) {
    .x <- .x |>
      select(timestamp, everything())
  } else {
    .x <- .x |>
      rename(timestamp = end_date_time) |>
      select(timestamp, everything())
  }
})



ggplot() +
  geom_point(data = example_per_list[[1]],
             aes(x = ymd_hms(timestamp),
                 y = bg),
             color = 'red') +
  geom_point(data = example_per_list[[2]],
             aes(x = timestamp,
                 y = hr,
                 color = 'blue')) +
  geom_point(data = example_per_list[[3]],
             aes(y = kcal,
                 x = timestamp,
                  color = 'green')) +
  geom_point(data = example_per_list[[4]],
             aes(x = start_date_time,
                 y = steps,
                 color = 'purple')) +
  geom_point(data = example_per_list[[5]],
             aes(y = ox_sat_percent,
                 x = timestamp,
                 color = 'black'))

# Define your target day (e.g., based on the earliest time in one data.frame)
target_day_start <- floor_date(min(example_per_list[[5]]$timestamp), unit = "day")
target_day_end <- target_day_start + days(1)

ggplot() +
  geom_point(data = example_per_list[[1]],
             aes(x = ymd_hms(timestamp),
                 y = bg,
                 color = "BG")) +  # Named in aes
  geom_point(data = example_per_list[[2]],
             aes(x = timestamp,
                 y = hr,
                 color = "HR")) +
  geom_point(data = example_per_list[[3]],
             aes(x = timestamp,
                 y = kcal,
                 color = "kCal")) +
  geom_point(data = example_per_list[[4]],
             aes(x = start_date_time,
                 y = steps,
                 color = "Steps")) +
  geom_point(data = example_per_list[[5]],
             aes(x = timestamp,
                 y = ox_sat_percent,
                 color = "Oxygen Saturation")) +
  coord_cartesian(xlim = c(target_day_start, target_day_end)) +
  scale_color_manual(
    name = "Variable",  # Legend title
    values = c(
      "BG" = "red",
      "HR" = "blue",
      "kCal" = "green",
      "Steps" = "purple",
      "Oxygen Saturation" = "black"
    )
  ) +
  theme_minimal(base_size = 30) +  # Set large base size (applies to most text)
  theme(
    plot.title = element_text(size = 48, face = "bold"),
    axis.title = element_text(size = 40),
    axis.text = element_text(size = 32),
    legend.title = element_text(size = 40),
    legend.text = element_text(size = 32)
  ) +
  labs(title = "Single day",
       x = "Time",
       y = "Values") +
  scale_y_continuous(limits = c(0, 750))

library(dplyr)
library(purrr)
library(zoo)
library(splines)

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

# Usage with your data
aligned_data <- align_timeseries(example_per_list, time_interval = 300)


library(ggplot2)
library(dplyr)
library(tidyr)

# Check alignment results
check_alignment <- function(aligned_data) {
  cat("Aligned data dimensions:", dim(aligned_data), "\n")
  cat("Time range:", range(aligned_data$timestamp), "\n")
  cat("Number of time points:", nrow(aligned_data), "\n")
  cat("Variables:", names(aligned_data)[!names(aligned_data) %in% c("timestamp", "uuid")], "\n")
  cat("Missing values per variable:\n")
  print(aligned_data %>% summarise(across(where(is.numeric), ~sum(is.na(.x)))))
}

# Plot all time series
plot_timeseries <- function(aligned_data) {

  # Get numeric columns (exclude timestamp, uuid, source)
  value_cols <- names(aligned_data)[sapply(aligned_data, is.numeric) &
                                      !names(aligned_data) %in% c("timestamp")]

  # Reshape to long format
  plot_data <- aligned_data %>%
    select(timestamp, all_of(value_cols)) %>%
    pivot_longer(cols = -timestamp, names_to = "variable", values_to = "value") %>%
    filter(!is.na(value))

  # Create faceted plot
  ggplot(plot_data, aes(x = timestamp, y = value)) +
    geom_line(alpha = 0.7) +
    geom_point(size = 0.5, alpha = 0.6) +
    facet_wrap(~variable, scales = "free_y", ncol = 2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Aligned Time Series",
         x = "Time",
         y = "Value")
}

# Compare original vs aligned data
compare_original_aligned <- function(original_list, aligned_data) {
  # Extract all numeric variables from original data
  original_data <- map_dfr(original_list, function(df) {
    # Convert timestamps consistently first
    if (is.character(df$timestamp)) {
      df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    } else {
      df$timestamp <- as.POSIXct(df$timestamp, tz = "UTC")
    }

    value_cols <- names(df)[sapply(df, is.numeric) &
                              !names(df) %in% c("time_s")]
    if (length(value_cols) > 0) {
      df %>%
        select(timestamp, all_of(value_cols)) %>%
        pivot_longer(cols = -timestamp, names_to = "variable", values_to = "value") %>%
        filter(!is.na(value)) %>%
        mutate(type = "original")
    }
  })

  # Extract corresponding variables from aligned data
  aligned_long <- aligned_data %>%
    select(timestamp, where(is.numeric)) %>%
    pivot_longer(cols = -timestamp, names_to = "variable", values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(type = "aligned")

  # Combine and filter to variables present in both
  common_vars <- intersect(original_data$variable, aligned_long$variable)
  combined <- bind_rows(original_data, aligned_long) %>%
    filter(variable %in% common_vars)

  ggplot(combined, aes(x = timestamp, y = value, color = type)) +
    geom_line(alpha = 0.7) +
    geom_point(size = 0.8, alpha = 0.7) +
    facet_wrap(~variable, scales = "free_y", ncol = 2) +
    scale_color_manual(values = c("original" = "blue", "aligned" = "red")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Original vs Aligned: All Variables",
         x = "Time", y = "Value")
}

# Usage
aligned_data <- align_timeseries(example_per_list, time_interval = 300)

check_alignment(b)
plot_timeseries(b)
compare_original_aligned(new_list[[123]], b)



# Method 1: Create nested list for all common UUIDs
create_uuid_nested_lists <- function(all_lists, common_uuids) {
  # Function to extract data for a specific UUID from all lists
  extract_uuid_data <- function(target_uuid) {
    uuid_data <- map(all_lists, function(list_of_dfs) {
      # Find dataframes that contain this UUID
      matched_dfs <- keep(list_of_dfs, ~ target_uuid %in% .x$uuid)
      if (length(matched_dfs) > 0) {
        return(matched_dfs[[1]])  # Return first match
      } else {
        return(NULL)
      }
    })

    # Remove NULL entries (data types that don't have this UUID)
    uuid_data <- uuid_data[!sapply(uuid_data, is.null)]
    return(uuid_data)
  }

  # Create nested list for all common UUIDs
  nested_lists <- map(common_uuids, extract_uuid_data)
  names(nested_lists) <- common_uuids

  return(nested_lists)
}

# Create the nested structure
uuid_nested_data <- create_uuid_nested_lists(all_lists, common_uuids)

# Alternative Method 2: More explicit approach with named data types
create_uuid_nested_lists_named <- function(cgm_list, hr_list, cal_list, activity_list, oxsat_list, common_uuids) {

  extract_uuid_data_named <- function(target_uuid) {
    data_types <- list(
      cgm = cgm_list,
      hr = hr_list,
      calories = cal_list,
      activity = activity_list,
      oxsat = oxsat_list
    )

    uuid_data <- map(data_types, function(list_of_dfs) {
      matched_dfs <- keep(list_of_dfs, ~ target_uuid %in% .x$uuid)
      if (length(matched_dfs) > 0) {
        return(matched_dfs[[1]])
      } else {
        return(NULL)
      }
    })

    # Remove NULL entries
    uuid_data <- uuid_data[!sapply(uuid_data, is.null)]
    return(uuid_data)
  }

  nested_lists <- map(common_uuids, extract_uuid_data_named)
  names(nested_lists) <- common_uuids

  return(nested_lists)
}

# Using the named approach
uuid_nested_data_named <- create_uuid_nested_lists_named(
  cgm_processed,
  hr_processed,
  physical_activity_calorie_processed,
  physical_activity_processed,
  ox_sat_processed,
  common_uuids
)

# Example usage:
# Access data for first UUID
first_uuid <- names(uuid_nested_data)[1]
first_uuid_data <- uuid_nested_data[[first_uuid]]

# Or access by UUID name directly
specific_uuid_data <- uuid_nested_data[["AIREADI-1023"]]  # Replace with actual UUID

# Check structure
cat("Number of UUIDs:", length(uuid_nested_data), "\n")
cat("Data types available for first UUID:", length(uuid_nested_data[[1]]), "\n")
cat("Names of data types for first UUID:", names(uuid_nested_data[[1]]), "\n")

# You can now process each UUID's data
process_all_uuids <- function(uuid_nested_data, time_interval = 300) {
  results <- map(uuid_nested_data, function(uuid_data) {
    tryCatch({
      align_timeseries(uuid_data, time_interval = time_interval)
    }, error = function(e) {
      cat("Error processing UUID:", names(uuid_data)[1], "-", e$message, "\n")
      return(NULL)
    })
  })

  # Remove failed attempts
  results[!sapply(results, is.null)]
}

# Process all UUIDs
all_aligned_data <- process_all_uuids(uuid_nested_data)
