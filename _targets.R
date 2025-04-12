# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", "RSQLite", "DBI", "tidyverse", "CLSIEP15", "mcr", "parallel", "robslopes"),
  # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
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
tar_source("R")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # tar_target(db_connection, {
  #   # Create a SQLite database connection
  #   db <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2.db")}
  #  # name = data,
  #   #command = tibble(x = rnorm(100), y = rnorm(100))
  #   # format = "qs" # Efficient storage for general data objects.
  # ),
  tar_target(ri_pre_data, {
    # Create a SQLite database connection
    con <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2.db")
    on.exit(dbDisconnect(con))  # Ensure the connection is closed when done
    data <- fun_get_RI_SQL_data(con)
    data
  }),
  tar_target(ri_data, fun_prepare_RI_data(ri_pre_data)),
  tar_target(vk_data, {
    # Create a SQLite database connection
    con <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2.db")
    on.exit(dbDisconnect(con))  # Ensure the connection is closed when done
    data <- fun_get_VK_SQL_data(con)
    data
  }),
  tar_target(qc_pre_data, {
    # Create a SQLite database connection
    con <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2.db")
    on.exit(dbDisconnect(con))  # Ensure the connection is closed when done
    data <- fun_get_QC_SQL_data(con)
    data
  }),
  tar_target(qc_data, fun_filter_QC_data(qc_pre_data)),
  
  # analysing QC results accoridng to CLSI
  tar_target(qc_results, {
    bind_rows(Filter(
      Negate(is.null),
      lapply(unique(qc_data$DxI9000), fun_process_QC_data, data = qc_data)
    )) |> 
      arrange(Analyt)
  }),
  
  # retriev QC-measurements from the SQL database
  tar_target(val_pre_data, {
    # Create a SQLite database connection
    con <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2.db")
    on.exit(dbDisconnect(con))  # Ensure the connection is closed when done
    data <- fun_get_val_SQL_data(con)
    data
  }),
  
  # created the nested wide tables for the QC raw data
  tar_target(val_data, fun_nest_val_data(val_pre_data)),
  tar_target(val_data1, fun_add_wide_QC_data(val_data, qc_data)),
  
  # add scatterplots
  tar_target(val_data2, fun_add_scatterplot(val_data)),
  
  # add paba regression data
  tar_target(val_data3, fun_add_paba_regression(val_data2)),
  tar_target(val_data4, left_join(val_data3, val_data1, by = "Analyt"))
  # ,
  # tar_target(close_connection, {
  #   #con <- db_connection
  #   force(ri_data)  # Ensure the target is run before closing the connection
  #   dbDisconnect(con)  # Schließe die Verbindung
  #   NULL
  # })
)
