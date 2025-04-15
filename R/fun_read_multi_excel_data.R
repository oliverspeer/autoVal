# function to read multiple files ---------------------
fun_read_multi_excel_data <- function(raw_data_files, dt_name = "dxi.data") {
  # project_directory <- rstudioapi::getActiveProject()
  #data.path <- "C:/R_local/autoVal/2_Rohdaten" #"I:\\Institut-Haus 04\\Labor 2_Core Lab Klinische Chemie\\Evaluationen\\Geraete\\DxI9000\\Validation\\2_Rohdaten"
  files <- raw_data_files
  assign("files", files, envir = .GlobalEnv)
  all.data <- list()
  
  # Loop through all files
  for (file in files) {
    #full.path <- file.path(data.path, file.name)
    #load full.path into the global environment
    #assign("full.path", full.path, envir = .GlobalEnv)
    
    # Read the header
    head <- read_xlsx(file, rows = 1:2, colNames = FALSE)
    
    
    # Detect NA cells in the second row  , replace them with letters 
    head[2, which(is.na(head[2, ]))] <- as.list(letters[1:length(which(is.na(head[2, ])))])
    
    # Combine the first and second row to create the column names
    head <- apply(head, 2, function(x) paste(rev(x), collapse = "_"))
    
    
    # Read the data while skipping the first two rows and using the combined column names
    dt <- read_xlsx(file, start_row = 2)
    colnames(dt) <- head
    
    
    
    # Removing "a_", "b_", ..., "z_" from the column names
    colnames(dt) <- gsub("^[a-z]_", "", colnames(dt))
    
    # Ensure the "Probennummer" column exists even if it was not in the source file,
    # filling with NA for rows if it was added manually
    if (!"Probennummer" %in% colnames(dt)) {
      dt <- mutate(dt, Probennummer = NA)
    }
    
    # Convert the data.frame to a data.table
    setDT(dt)  
    
    
    # Remove specific columns to anonymize the data
    dt[, c("Name", "Vorname") := NULL]
    
    # Define and exclude certain columns from conversion to numeric
    exclude.cols <- c("Tagesnummer", "Geb.datum", "Geschl.", "Auftragg.")
    include.cols <- setdiff(names(dt), exclude.cols)
    
    
    # Convert included columns to numeric
    for (col in include.cols) {
      dt[, (col) := as.numeric(get(col))]
    }
    
    dt[, Geb.datum := ymd(Geb.datum)]
    
    # Store the processed data table in a list
    all.data[[file]] <- dt
  }
  
  # Combine all processed data.tables
  dt_name <- rbindlist(all.data, use.names = TRUE, fill = TRUE)
  
  # new column `sex` with 0 if Dt.wide.pth$f_Geschl. == "F" and 1 if Dt.wide.pth$f_Geschl. == "M"
  dt_name$sex <- ifelse(dt_name$Geschl. == "F", 0, 1)
  
  # calculate the age from dt_name$Datum and dt_name$e_Geb.datum
  dt_name <- dt_name[
    !grepl("Tagesnummer", Tagesnummer) & !is.na(Tagesnummer)
  ][, Datum := ymd(substr(Tagesnummer, 1, 10))
  ]
  
  dt_name[, Datum := ymd(Datum)
  ][, Geb.datum := ymd(Geb.datum)
  ][, Alter := round(interval(start = Geb.datum, end = Datum) / years(1), 2)] 
  
  
  
  
  
  
  archive <- "C:/R_local/autoVal/2_Rohdaten/sql_importiert/" #"I:\\Institut-Haus 04\\Labor 2_Core Lab Klinische Chemie\\Evaluationen\\Geraete\\DxI9000\\Validation\\2_Rohdaten\\SQLdb_importiert\\"
  files.name <- basename(files)
  file_move(files,  file.path(archive, files.name))
  
  return(dt_name)
}
