# prepare libraries and database connection--------------------------

setwd("H:/R/autoVal_H")
source("StartUp.R")
StartUpRoutine()



# read Raw Data------------------------------------------------------
# Define the raw data directory 
dir.rawdata <- "I:\\Institut-Haus 04\\Labor 2_Core Lab Klinische Chemie\\Evaluationen\\Geräte\\DxI9000\\Validation\\2_Rohdaten\\"

# List all raw data CSV files
csv.files <- list.files(path = dir.rawdata, pattern = "*.csv", full.names = TRUE)

# Check if there are any raw data files
if (length(csv.files) > 0) {
  # Identify the latest file based on modification time
  latest.csv <- csv.files[which.max(file.info(csv.files)$mtime)]

  val.dat <- read_csv(latest.csv)
  
  # val.dat <- read_csv("I:\\Institut-Haus 04\\Labor 2_Core Lab Klinische Chemie\\Evaluationen\\Geräte\\DxI9000\\Validation\\2_Rohdaten\\20240527 Messungen 12.csv")
  
   val.dat <- val.dat |>  
    rename(
      PackRevision = `PackRevision...77`,
      PackRevision2 = `PackRevision...89`
    )
  
  setDT(val.dat)


  val.dat$Probennummer <- as.numeric(substr(val.dat$SampleID, 1, nchar(val.dat$SampleID) - 2))
  
  # insert val.dat into the SQLite DB--------------------------------
  
  dbWriteTable(con, "DxIvalData", val.dat, append = TRUE, row.names = FALSE)

  # Disconnect from the database -----------------------------------
  dbDisconnect(con)
} else {
  print("No raw data files found.")
}








