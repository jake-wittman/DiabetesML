
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
