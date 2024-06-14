# define functions --------------------------------------------------------
# function to read multiple files ---------------------
fun.read.multi.excel.data <- function(file.pattern, dt.name) {
  # project_directory <- rstudioapi::getActiveProject()
  data.path <- getActiveProject()
  files <- list.files(data.path, pattern = file.pattern)
  all.data <- list()
  
  # Loop through all files
  for (file.name in files) {
    full.path <- file.path(data.path, file.name)
    
    # Read the header
    head <- read_xlsx(full.path, rows = 1:2, colNames = FALSE)
    
    
    # Detect NA cells in the second row  , replace them with letters 
    head[2, which(is.na(head[2, ]))] <- as.list(letters[1:length(which(is.na(head[2, ])))])
    
    # Combine the first and second row to create the column names
    head <- apply(head, 2, function(x) paste(rev(x), collapse = "_"))
    
    
    # Read the data while skipping the first two rows and using the combined column names
    dt <- read_xlsx(full.path, start_row = 2)
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
    all.data[[file.name]] <- dt
  }
  
  # Combine all processed data.tables
  dt.name <- rbindlist(all.data, use.names = TRUE, fill = TRUE)
  
  # new column `sex` with 0 if Dt.wide.pth$f_Geschl. == "F" and 1 if Dt.wide.pth$f_Geschl. == "M"
  dt.name$sex <- ifelse(dt.name$Geschl. == "F", 0, 1)
  
  # calculate the age from dt.name$Datum and dt.name$e_Geb.datum
  dt.name <- dt.name[
    !grepl("Tagesnummer", Tagesnummer) & !is.na(Tagesnummer)
  ][, Datum := ymd(substr(Tagesnummer, 1, 10))
  ]
  
  dt.name[, Datum := ymd(Datum)
  ][, Geb.datum := ymd(Geb.datum)
  ][, Alter := round(interval(start = Geb.datum, end = Datum) / years(1), 2)] 
  
  
  
  
  return(dt.name)
}


# function to tidy up data --------------------------------------
fun.write.tidy.data<- function(data, dt_name) {
  
  # prepare  id.cols that are not to be melted
  id.cols <- names(data)[!grepl("_\\d+", names(data))]
  
  # Melt the data.table
  DT.m1 = melt(
    data,
    id.vars = id.cols,
    variable.name = "Bezeichnung_Methode",
    value.name = "Werte",
    na.rm = TRUE
  )
  
  # Split 'Bezeichnung_Methode' into two columns 'Bezeichnung' and 'Methode'
  DT.m1[, c("Bezeichnung", "Methode") := tstrsplit(Bezeichnung_Methode, "_", fixed = TRUE)
  ][, Bezeichnung_Methode := NULL][, Methode := as.numeric(Methode)]
  
  dt_name <- setDT(DT.m1)
  
  
  # change the column names to be SQL compatible
  setnames(dt_name, old = c("Geb.datum", "Geschl.", "Auftragg."), new = c("DOB", "Geschlecht", "KundenID"))
  
  # change DOB and Datum to numeric
  # dt_name[, DOB := as.numeric(DOB)]
  # dt_name[, Datum := as.numeric(Datum)]
  
  # extract year, quarter, month, week, day from Datum
  dt_name[, Jahr := year(Datum)
  ][, Quartal := quarter(Datum)
  ][, Monat := month(Datum)
  ][, Woche := week(Datum)
  ][, Tag := day(Datum)
  ][, DOB := as.character(DOB)
  ][, Datum := as.character(Datum)
  ]
  
  
  
  return(dt_name)
}


# DxI data (read excel, tidy up data)  -------------------------------------
dxi.data <- fun.read.multi.excel.data("BlutDxI", "dxi.data")
DT.tidy.dxi <- fun.write.tidy.data(dxi.data, 
                                   #DT.tarif, 
                                   "DT.tidy.dxi")



# Insert data from DT.tidy.dxi into the measurement.data table in the SQLite database
dbWriteTable(con, "MeasurementData", DT.tidy.dxi, append = TRUE, row.names = FALSE)

# close the connection
dbDisconnect(con)


