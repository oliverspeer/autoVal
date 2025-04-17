# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) 
# Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", 
               "RSQLite", 
               "DBI", 
               "tidyverse", 
               "CLSIEP15", 
               "mcr", 
               "parallel", 
               "robslopes", 
               "data.table", 
               "readxl",
               "openxlsx2",
               "fs",
               "rlang",
               "data.table",
               "units"),
  # error = "null",
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
  controller = crew::crew_controller_local(workers = 6, seconds_idle = 60)
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
  # Continue running the pipeline even if a target fails.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # tar_target(db_connection, {
  #   # Create a SQLite database connection
  #   db <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2_test.db")}
  #  # name = data,
  #   #command = tibble(x = rnorm(100), y = rnorm(100))
  #   # format = "qs" # Efficient storage for general data objects.
  # ),
  
  # überwachen ob neue csv-files mit Messdaten vorhanden sind
    tar_files(name = raw_data_csv,
              command = list.files(path = "C:/R_local/autoVal/2_Rohdaten",#"I:\\Institut-Haus 04\\Labor 2_Core Lab Klinische Chemie\\Evaluationen\\Geraete\\DxI9000\\Validation\\2_Rohdaten",
                                   pattern = "*.csv",
                                   full.names = TRUE,
                                   recursive = FALSE)
              ), # Use format = "file" for file targets.
  
    # get mol-mass xlsx file  
   tar_files(MolMass_xlsx, "Dev_changeUnitsDxI9000.xlsx"),
    # import and process mol-mass data
   tar_target(molmass_data, fun_load_molmass(MolMass_xlsx_files)),
    # import and process DxI9000 data
   tar_target(DxI9000_data, 
              fun_load_process_DxI9000_data(raw_data_csv_files, sum_dat = molmass_data)),
    # upload DxI9000 data to SQLite database
   tar_target(dxi9000_data_sql, {
      # Create a SQLite database connection
      con <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2_test.db")
      on.exit(dbDisconnect(con))  # Ensure the connection is closed when done
      fun_upload_DxI9000_data(con, DxI9000_data)
    }),
    
    #check for new DxI800 xlsx-files
  tar_files(name = raw_data_xlsx,
              command = list.files(path = "C:/R_local/autoVal/2_Rohdaten",#"I:\\Institut-Haus 04\\Labor 2_Core Lab Klinische Chemie\\Evaluationen\\Geraete\\DxI9000\\Validation\\2_Rohdaten",
                                   pattern = "*.xlsx",
                                   full.names = TRUE,
                                   recursive = FALSE)
              ),
    
    #tidy and upload DxI800 data
    #tar_target(dxi800_data, fun_tidy_and_upload_DxI800_data(raw_data_xlsx_files)),
  tar_target(dxi800_data_sql, {
      # Create a SQLite database connection
      con <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2_test.db")
      on.exit(dbDisconnect(con))  # Ensure the connection is closed when done
      fun_tidy_and_upload_DxI800_data(con, raw_data_files = raw_data_xlsx_files)
      # dbWriteTable(con, 
      #              "MeasurementData", 
      #              dxi800_data, 
      #              append = TRUE, 
      #              row.names = FALSE)
      
    }),
  
  tar_target(ri_pre_data, {
    # Create a SQLite database connection
    con <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2_test.db")
    on.exit(dbDisconnect(con))  # Ensure the connection is closed when done
    data <- fun_get_RI_SQL_data(con)
    data
  }),
  tar_target(ri_data, fun_prepare_RI_data(ri_pre_data)),
  tar_target(vk_data, {
    # Create a SQLite database connection
    con <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2_test.db")
    on.exit(dbDisconnect(con))  # Ensure the connection is closed when done
    data <- fun_get_VK_SQL_data(con)
    data
  }),
  tar_target(qc_pre_data, {
    # Create a SQLite database connection
    con <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2_test.db")
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
  tar_target(qc_summary, fun_process_qc_summary(ri_data, vk_data, qc_results)),
  
  # retriev QC-measurements from the SQL database
  tar_target(val_pre_data, {
    # Create a SQLite database connection
    con <- DBI::dbConnect(RSQLite::SQLite(), "ClinicalChemistry_2_test.db")
    on.exit(dbDisconnect(con))  # Ensure the connection is closed when done
    #MD_hash <- tar_read(dxi800_data_sql)
    data <- fun_get_val_SQL_data(con, MD_hash = dxi800_data_sql, DD_hash = dxi9000_data_sql)
    data
  }),
  
  # created the nested wide tables for the QC raw data
  tar_target(val_data, fun_nest_val_data(val_pre_data)),
  tar_target(val_data1, fun_add_wide_QC_data(val_data, qc_data)),
  
  # add scatterplots
  tar_target(val_data2, fun_add_scatterplot(val_data)),
  
  # add paba regression data
  tar_target(val_data3, fun_add_paba_regression(val_data2)),
  tar_target(val_data4, left_join(val_data3, val_data1, by = "Analyt")),
  tar_target(val_dat, fun_add_qc_summary(val_data4, qc_summary)),
  tar_quarto(
    name = DxI9000_Validations_Report,
    path = "Dev_targets_DxIautoVal.qmd"#,
    #execute_params = list(val_dat = val_dat)
    
  )
  
  
  # add VK summary data
  #tar_target(val_data5, fun_add_summary_stats(val_data4, qc_results))
  # ,
  # tar_target(close_connection, {
  #   #con <- db_connection
  #   force(ri_data)  # Ensure the target is run before closing the connection
  #   dbDisconnect(con)  # Schließe die Verbindung
  #   NULL
  # })
)
