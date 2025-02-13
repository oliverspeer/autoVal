setwd("C:/R_local/autoVal")

source("StartUp.R")
StartUpRoutine()


# Load the data
# function to load QC data from different file formats
fun.load.qc.file <- function(file_path, remove_cols, delim = NULL, clean_level = FALSE) {
  if (grepl(".xlsx", file_path, fixed = TRUE)) {
    df <- read_excel(file_path) |> select(-all_of(remove_cols))
  } else if (grepl(".csv", file_path, fixed = TRUE) && !is.null(delim)) {
    df <- read_delim(file_path, delim = delim, escape_double = FALSE, trim_ws = TRUE) |> 
      select(-all_of(remove_cols))
    if (clean_level && "Level" %in% names(df)) {
      df <- df |> mutate(Level = gsub("[A-Za-z]", "", Level) |> as.numeric(),
                         Date = as.POSIXct(Date, format = "%d.%m.%Y %H:%M", tz = "UTC"))
    }
  } else {
    stop(paste("Unsupported file format or missing delimiter for CSV: ", file_path))
  }
  return(df)
}

# load QC data
setwd("C:/R_local/autoVal")
dxi_1_immunoassay_qc <- fun.load.qc.file("DXI 1 Immunoassay QC.xlsx", 9:11)
dxi_1_stfr_qc <- fun.load.qc.file("DXI 1 sTfR QC.xlsx", 9:11)
dxi_1_speciality_qc <- fun.load.qc.file("DXI 1 Speciality QC.xlsx", 9:11)
dxi_1_shbg_qc <- fun.load.qc.file("DXI 1 SHBG QC.xlsx", 9:11)
dxi_2_amh_qc <- fun.load.qc.file("DXI 2 AMH QC.xlsx", 9:11)
dxi1_cardio_1_0 <- fun.load.qc.file("DXI1 Cardio 1.0.csv", 9:12, delim = ";", clean_level = TRUE)
dxi1_tumormarker_1_0 <- fun.load.qc.file("DXI1 Tumormarker 1.0.csv", 9:12, delim = ";", clean_level = TRUE)

dxi_1_immunoassay_qc25 <- fun.load.qc.file("DXI 1 Immunoassay QC25.xlsx", 6)
dxi_1_stfr_qc25 <- fun.load.qc.file("DXI 1 sTfR QC25.xlsx", 6)
dxi_1_speciality_qc25 <- fun.load.qc.file("DXI 1 Speciality QC25.xlsx", 6)
dxi_1_shbg_qc25 <- fun.load.qc.file("DXI 1 SHBG QC25.xlsx", 6)
dxi_2_amh_qc25 <- fun.load.qc.file("DXI 1 AMH QC25.xlsx", 6)
dxi1_cardio_1_025 <- fun.load.qc.file("DXI1 Cardio 25.csv", 6, delim = ";", clean_level = TRUE)
dxi1_tumormarker_1_025 <- fun.load.qc.file("DXI1 Tumormarker QC25.xlsx", 6)
# bind all data frames together
qc.dat <- bind_rows(
  dxi_1_immunoassay_qc, 
  dxi_1_stfr_qc, 
  dxi_1_speciality_qc, 
  dxi_1_shbg_qc, 
  dxi_2_amh_qc, 
  dxi1_cardio_1_0, 
  dxi1_tumormarker_1_0) |> 
  
  rename(LotNr = "Lot No.") |>
  select(Parameter, Level, Date, Zielwert, SD, LotNr)
  

  
  # group_by(Parameter, Level, Zielwert) |> 
  # reframe(
  #   Date = Date,
  #   Zielwert = first(Zielwert),
  #   Level = first(Level),
  #   SD = first(SD),
  #   LotNr = first(LotNr)
  # ) |> 
  # ungroup()

qc.dat1 <- bind_rows(
  dxi_1_immunoassay_qc25, 
  dxi_1_stfr_qc25, 
  dxi_1_speciality_qc25, 
  dxi_1_shbg_qc25, 
  dxi_2_amh_qc25, 
  dxi1_cardio_1_025, 
  dxi1_tumormarker_1_025) 

# |> 
#   
#   group_by(Parameter, Level, Zielwert) |> 
#   reframe(
#     Date = Date,
#     Zielwert = first(Zielwert),
#     Level = first(Level),
#     SD = first(SD),
#     LotNr = first(LotNr)
#   ) |> 
#   ungroup()


qc.dat <- bind_rows(qc.dat, qc.dat1) |> 
  
  group_by(Parameter, Level, Zielwert) |> 
  summarise(
    minDate = min(Date, na.rm = TRUE),
    maxDate = max(Date, na.rm = TRUE),
    Zielwert = first(Zielwert),
    Level = first(Level),
    SD = first(SD),
    LotNr = first(LotNr)
  ) |> 
  ungroup()

translate.df <- dbReadTable(con, "TranslationData")
write.xlsx(translate.df, "TranslationData.xlsx")


# automatically create a SQL TABLE containing QC data
# function to map R data types to SQLite data types
map.data.types <- function(df) {
  sapply(df, function(column) {
    dtype <- class(column)[1]  # Get the class of the column. If multiple, take the first.
    switch(dtype,
           "factor" = "TEXT",
           "character" = "TEXT",
           "integer" = "INTEGER",
           "numeric" = "REAL",
           "Date" = "TEXT",  # SQLite does not have a DATE type, so dates are often stored as TEXT
           "POSIXct" = "TEXT",  # or as INTEGER (Unix Time)
           "logical" = "INTEGER",  # TRUE/FALSE mapped to 1/0
           "UNKNOWN")  # Default case if type not matched
  })
}

# map data types from val.dat
col.types <- map.data.types(qc.dat)

# construct a CREATE TABLE statement for val.dat using the mapped data types
create.table.statement <- paste0("CREATE TABLE IF NOT EXISTS QCData (",
                                 paste(
                                   sapply(names(col.types), function(name) paste0('"', name, '"')),
                                   col.types, 
                                   sep=" ", 
                                   collapse=", "),
                                 ", QCID INTEGER PRIMARY KEY AUTOINCREMENT",
                                 ", FOREIGN KEY (Probennummer) REFERENCES MeasurementData (Probennummer)",
                                 ")") 

# connect to SQLite DB
# con <- dbConnect(SQLite(), dbname = "C:/R_local/labStat/ClinicalChemistry_2.db")

# create the table in the SQLite DB
# dbExecute(con, create.table.statement)

