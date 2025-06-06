library(tidyverse)
library(jsonlite)
data_path <- '//cdc.gov/locker/CCHP_NCCD_DDT_Data1/epistat/Jake Wittman/Workstation/DiabetesML/data'

gcm_manifest_path <- file.path(data_path, 'wearable_blood_glucose/manifest.tsv')
gcm_manifest <- read_tsv(gcm_manifest_path)
wearable_manifest_path <- file.path(data_path, 'wearable_activity_monitor/manifest.tsv')
wearable_manifest <- read_tsv(wearable_manifest_path)


cgm_path <- 'wearable_blood_glucose/continuous_glucose_monitoring/dexcom_g6/'
first_participant <- file.path(data_path, cgm_path, '1023/1023_DEX.json')
first_part <- read_json(first_participant)
df <- tibble(first_part)
df

cgm <- first_part$body$cgm
cgm$effective_time_frame$time_interval <- cgm$effective_time_frame$time_interval |>
  mutate(start_date_time = ymd_hms(start_date_time))
ggplot(cgm, aes(x = effective_time_frame$time_interval$start_date_time, y = blood_glucose$value)) +
  geom_point() +
  geom_line() +
  scale_x_datetime(date_breaks = '1 day', date_labels = '%d')

hr_path <- 'wearable_activity_monitor/heart_rate/garmin_vivosmart5/'
random_participant <- file.path(data_path, hr_path, '1023/1023_heartrate.json')
random_part <- read_json(random_participant, simplifyVector = TRUE)
json_dat <- random_part
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



hr <- random_part$body$heart_rate
hr$effective_time_frame <- hr$effective_time_frame |>
  mutate(date_time = ymd_hms(date_time))
ggplot() +
  geom_point(data = hr,
             aes(x = effective_time_frame$date_time, y = heart_rate$value),
             color = 'blue') +
  geom_point(data = cgm,
             aes(x = effective_time_frame$time_interval$start_date_time, y = blood_glucose$value),
             color = 'red') +
  scale_x_datetime(date_breaks = '1 day', date_labels = '%d',
                   limits = c(min(cgm$effective_time_frame$time_interval$start_date_time),
                              min(cgm$effective_time_frame$time_interval$start_date_time) + 60*60*24))
