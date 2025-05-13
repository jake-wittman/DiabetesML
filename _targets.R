# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(jsonlite)
# library(crew)
# Set target options:
tar_option_set(
  packages = c("tibble",
               'tarchetypes',
               'dplyr',
               'lubridate',
               'readr',
               'dplyr',
               'purrr',
               'tidyr'), # Packages that your targets need for their tasks.
  error = 'null'
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)
# tar_option_set(
#   controller = crew_controller_local(workers = 8)
# )
# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # Base data paths
  tar_target(cgm_path,
             file.path(here::here(), 'data/wearable_blood_glucose/continuous_glucose_monitoring/dexcom_g6/')),
  tar_target(wearable_path,
             file.path(here::here(), 'data/wearable_activity_monitor/')),
  tar_target(clinical_dat_path,
             file.path(here::here(), 'data/clinical_data')),
  tar_target(env_paths,
             file.path(here::here(), 'data/environment/environmental_sensor/leelab_anura/')),
  # Device-specific paths
  tar_target(hr_paths,
             file.path(wearable_path, 'heart_rate/garmin_vivosmart5')),
  tar_target(ox_sat_paths,
             file.path(wearable_path, 'oxygen_saturation/garmin_vivosmart5')),
  tar_target(physical_activity_paths,
             file.path(wearable_path, 'physical_activity/garmin_vivosmart5')),
  tar_target(physical_activity_calorie_paths,
             file.path(wearable_path, 'physical_activity_calorie/garmin_vivosmart5')),
  tar_target(respiratory_rate_paths,
             file.path(wearable_path, 'respiratory_rate/garmin_vivosmart5')),
  tar_target(sleep_paths,
             file.path(wearable_path, 'sleep/garmin_vivosmart5')),
  tar_target(stress_paths,
             file.path(wearable_path, 'stress/garmin_vivosmart5')),


  # Clinical data targets
  tar_target(clinical_csv_paths,
             list.files(clinical_dat_path, full.names = TRUE, recursive = TRUE, pattern = 'csv')),

  # JSON file paths for each data type
  tar_files(cgm_json_paths,
            list.files(cgm_path, full.names = TRUE, recursive = TRUE)),
  tar_files(hr_json_paths,
            list.files(hr_paths, full.names = TRUE, recursive = TRUE)),
  tar_files(ox_sat_json_paths,
            list.files(ox_sat_paths, full.names = TRUE, recursive = TRUE)),
  tar_files(physical_activity_json_paths,
            list.files(physical_activity_paths, full.names = TRUE, recursive = TRUE)),
  tar_files(physical_activity_calorie_json_paths,
            list.files(physical_activity_calorie_paths, full.names = TRUE, recursive = TRUE)),
  tar_files(respiratory_rate_json_paths,
            list.files(respiratory_rate_paths, full.names = TRUE, recursive = TRUE)),
  tar_files(sleep_json_paths,
            list.files(sleep_paths, full.names = TRUE, recursive = TRUE)),
  tar_files(stress_json_paths,
            list.files(stress_paths, full.names = TRUE, recursive = TRUE)),
  tar_files(env_csv_paths,
            list.files(env_paths, full.names = TRUE, recursive = TRUE)),

  # Reading JSON data
  tar_target(cgm_json,
             read_json(cgm_json_paths),
             pattern = map(cgm_json_paths),
             iteration = 'list'),
  tar_target(hr_json,
             read_json(hr_json_paths),
             pattern = map(hr_json_paths),
             iteration = 'list'),
  tar_target(ox_sat_json,
             my_read_json(ox_sat_json_paths),
             pattern = map(ox_sat_json_paths),
             iteration = 'list'),
  tar_target(physical_activity_json,
             read_json(physical_activity_json_paths),
             pattern = map(physical_activity_json_paths),
             iteration = 'list'),
  tar_target(physical_activity_calorie_json,
             read_json(physical_activity_calorie_json_paths),
             pattern = map(physical_activity_calorie_json_paths),
             iteration = 'list'),
  tar_target(respiratory_rate_json,
             read_json(respiratory_rate_json_paths),
             pattern = map(respiratory_rate_json_paths),
             iteration = 'list'),
  tar_target(sleep_json,
             read_json(sleep_json_paths),
             pattern = map(sleep_json_paths),
             iteration = 'list'),
  tar_target(stress_json,
             read_json(stress_json_paths),
             pattern = map(stress_json_paths),
             iteration = 'list'),
  tar_target(env_csv,
             read_csv(env_csv_paths),
             pattern = map(env_csv_paths),
             iteration = 'list'),
  tar_target(clinical_csv,
             read_csv(clinical_csv_paths),
             pattern = map(clinical_csv_paths),
             iteration = 'list'),

  # Processing data
  tar_target(cgm_processed,
             processCGM(cgm_json),
             pattern = map(cgm_json),
             iteration = 'vector'),
  tar_target(hr_processed,
             processHR(hr_json),
             pattern = map(hr_json),
             iteration = 'vector'),
  tar_target(ox_sat_processed,
             processOxSat(ox_sat_json),
             pattern = map(ox_sat_json),
             iteration = 'vector'),
  tar_target(physical_activity_calorie_processed,
             processCalories(physical_activity_calorie_json),
             pattern = map(physical_activity_calorie_json),
             iteration = 'vector'),
  tar_target(physical_activity_processed,
             processPhysicalActivity(physical_activity_json),
             pattern = map(physical_activity_json),
             iteration = 'vector'),
  tar_target(respiratory_rate_processed,
             processRespRate(respiratory_rate_json),
             pattern = map(respiratory_rate_json),
             iteration = 'vector'),
  tar_target(stress_processed,
             processStress(stress_json),
             pattern = map(stress_json),
             iteration = 'vector'),
  tar_target(sleep_processed,
             processSleep(sleep_json),
             pattern = map(sleep_json),
             iteration = 'vector'),


  tar_target(clinical_processed,
             {
               dat_list <- map(clinical_csv_paths, ~read_csv(.x))
               dat <- reduce(dat_list, left_join)
               dat <- mutate(dat, uuid = paste0('AIREAD-', person_id))
               dat
             }),

  tar_target(
    realigned_ts,
    {
      # List your data.frames
      df_list <- list(
        cgm_processed,
        hr_processed,
        ox_sat_processed,
        physical_activity_calorie_processed,
        physical_activity_processed,
        respiratory_rate_processed,
        stress_processed,
        sleep_processed
      )

      # Step 1: Identify reference df and its time vector
      ref_df <- df_list %>%
        keep( ~ "bg" %in% names(.)) %>%
        .[[1]]

      ref_time_s <- ref_df$time_s
      # Step 4: Apply to all and combine into one wide df
      aligned_list <- map(df_list, align_to_ref_time_safe)
      final_combined_df <- reduce(aligned_list, left_join, by = "time_s")
    },
    pattern = map(
      cgm_processed,
      hr_processed,
      ox_sat_processed,
      physical_activity_calorie_processed,
      physical_activity_processed,
      respiratory_rate_processed,
      stress_processed,
      sleep_processed
    ),
    iteration = 'list'
  )
)
