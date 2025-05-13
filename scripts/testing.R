tar_load(c(cgm_processed, hr_processed, physical_activity_calorie_processed, physical_activity_processed, ox_sat_processed))
all_lists <- list(cgm_processed, hr_processed, physical_activity_calorie_processed, physical_activity_processed, ox_sat_processed)

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
                 x = start_date_time,
                  color = 'green')) +
  geom_point(data = example_per_list[[4]],
             aes(x = timestamp,
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
















dat_list <- ls()
dat_list <- dat_list[str_detect(dat_list, 'processed')]
dat_list <- dat_list[-c(2)]
dat <- mget(dat_list)
map(dat, ~head(.x) |> dput())


library(tidyverse)
library(lubridate)

# Let's first convert all the data frames into a list for easier handling
# I'm assuming these data frames are named based on their content:
df_bg <- dat[[1]]
df_hr <- dat[[2]]
df_ox <- dat[[3]]
df_kcal <- dat[[4]]
df_activity <- dat[[5]]
df_resp <- dat[[6]]
df_sleep <- dat[[7]]
df_stress <- dat[[8]]
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

# List your data.frames
df_list <- list(df_bg, df_hr, df_ox, df_kcal, df_steps, df_resp, df_sleep, df_stress)

# Step 1: Identify reference df and its time vector
ref_df <- df_list %>%
  keep(~ "bg" %in% names(.)) %>%
  .[[1]]

ref_time_s <- ref_df$time_s

# Step 2: Universal function to extract time in seconds from any df
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


# Step 4: Apply to all and combine into one wide df
aligned_list <- map(df_list, align_to_ref_time_safe)
final_combined_df <- reduce(aligned_list, left_join, by = "time_s")

print(final_combined_df)
library(ggplot2)
library(tidyr)
library(dplyr)

# Step 1: Pivot to long format (ignore time_s duplicates)
plot_df <- final_combined_df %>%
  pivot_longer(
    -time_s,
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value))  # Optionally filter NAs if you want to focus on available data

# Step 2: Plot with facets
ggplot(plot_df, aes(x = time_s, y = value)) +
  geom_line(color = "blue", na.rm = TRUE) +
  geom_point(color = "red", size = 2, na.rm = TRUE) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(
    title = "Diagnostic plot: Aligned time series",
    x = "Reference Time (s)",
    y = "Value"
  )

