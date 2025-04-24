# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(jsonlite)
# Set target options:
tar_option_set(
  packages = c("tibble",
               'tarchetypes',
               'dplyr',
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

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(cgm_path,
             file.path('//cdc.gov/locker/CCHP_NCCD_DDT_Data1',
             'epistat/Jake Wittman/Workstation/DiabetesML',
             'data/wearable_blood_glucose/continuous_glucose_monitoring/dexcom_g6/') ),

  tar_target(wearable_path,
             file.path('//cdc.gov/locker/CCHP_NCCD_DDT_Data1',
                       'epistat/Jake Wittman/Workstation/DiabetesML',
                       'data/wearable_activity_monitor/')
             ),

  tar_target(hr_paths,
             file.path(wearable_path, 'heart_rate/garmin_vivosmart5')),

  tar_files(hr_json_paths,
            list.files(hr_paths, full.names = TRUE, recursive = TRUE)),

  tar_files(cgm_json_paths,
           list.files(cgm_path, full.names = TRUE, recursive = TRUE)),

  tar_target(cgm_json,
             read_json(cgm_json_paths),
             pattern = map(cgm_json_paths),
             iteration = 'list'),

  tar_target(cgm_processed,
             processCGM(cgm_json),
             pattern = map(cgm_json),
             iteration = 'list'),

  tar_target(hr_json,
             read_json(hr_json_paths),
             pattern = map(hr_json_paths),
             iteration = 'list'),

  tar_target(hr_processed,
             processHR(hr_json),
             pattern = map(hr_json),
             iteration = 'list')

)
