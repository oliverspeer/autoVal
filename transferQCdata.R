setwd("I:/Institut-Haus 04/Labor 2_Core Lab Klinische Chemie/Evaluationen/Geraete/DxI9000/Validation/5_R")

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
setwd("H:/R/autoVal_H")
dxi_1_immunoassay_qc <- fun.load.qc.file("DXI 1 Immunoassay QC.xlsx", 9:11)
dxi_1_stfr_qc <- fun.load.qc.file("DXI 1 sTfR QC.xlsx", 9:11)
dxi_1_speciality_qc <- fun.load.qc.file("DXI 1 Speciality QC.xlsx", 9:11)
dxi_1_shbg_qc <- fun.load.qc.file("DXI 1 SHBG QC.xlsx", 9:11)
dxi_2_amh_qc <- fun.load.qc.file("DXI 2 AMH QC.xlsx", 9:11)
dxi1_cardio_1_0 <- fun.load.qc.file("DXI1 Cardio 1.0.csv", 9:12, delim = ";", clean_level = TRUE)
dxi1_tumormarker_1_0 <- fun.load.qc.file("DXI1 Tumormarker 1.0.csv", 9:12, delim = ";", clean_level = TRUE)

dxi2_immunoassay_24 <- fun.load.qc.file("DXI 1 Immunoassay QC24.xlsx", 6)
dxi_1_immunoassay_qc25 <- fun.load.qc.file("DXI 1 Immunoassay QC25.xlsx", 6)
dxi_1_stfr_qc25 <- fun.load.qc.file("DXI 1 sTfR QC25.xlsx", 6)
dxi_1_speciality_qc25 <- fun.load.qc.file("DXI 1 Speciality QC25.xlsx", 6)
dxi_1_shbg_qc25 <- fun.load.qc.file("DXI 1 SHBG QC25.xlsx", 6)
dxi_1_amh_qc25 <- fun.load.qc.file("DXI 1 AMH QC25.xlsx", 6)
dxi_2_amh_qc25 <- fun.load.qc.file("DXI 2 AMH QC25.xlsx", 6)
dxi_2_amh_qc24 <- fun.load.qc.file("DXI 2 AMH QC24.xlsx", 6)
dxi1_cardio_1_025 <- fun.load.qc.file("DXI1 Cardio 25.csv", 6, delim = ";", clean_level = TRUE)
dxi2_cardio_24 <- fun.load.qc.file("DXI2 Cardio 24.csv", 6, delim = ";", clean_level = TRUE)
dxi1_tumormarker_1_025 <- fun.load.qc.file("DXI1 Tumormarker QC25.xlsx", 6)
dxi2_tumormarker_24 <- fun.load.qc.file("DXI2 Tumormarker QC24.xlsx", 6)
dxi1_tumormarker_24 <- fun.load.qc.file("DXI1 Tumormarker QC24.xlsx", 6)


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
  dxi_1_amh_qc25,
  dxi_2_amh_qc25,
  dxi_2_amh_qc24,
  dxi1_cardio_1_025, 
  dxi1_tumormarker_1_025,
  dxi2_immunoassay_24,
  dxi2_cardio_24,
  dxi2_tumormarker_24,
  dxi1_tumormarker_24)

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


qc.dat2 <- bind_rows(qc.dat, qc.dat1) |> 
  
  group_by(Parameter, Level, Zielwert) |> 
  summarise(
    minDate = as.Date(min(Date, na.rm = TRUE)),
    maxDate = as.Date(max(Date, na.rm = TRUE)),
    Zielwert = first(Zielwert),
    Level = first(Level),
    SD = first(SD),
    LotNr = first(LotNr)
  ) |> 
  mutate(today = today()+40)  |>   # Datum im April hinzufügen
  group_by(Parameter, Level) |>
  mutate(
    date_diff = abs(as.numeric(today - maxDate)),  # Differenz zum heutigen Datum berechnen
    closest = (date_diff == min(date_diff))        # Markiere das nächste maxDate
  ) |>
  mutate(
    maxDate = if_else(closest, today, maxDate)  # Falls es das nächste Datum ist, ersetze es mit today()
  ) |>
  select(-date_diff, -closest, -today) |>
  ungroup()

# plot the data
ggplot(qc.dat2, aes(y = Parameter)) +
  geom_segment(aes(x = minDate, xend = maxDate, y = Parameter, yend = Parameter, color = LotNr), linewidth = 2) + # Linien für die Zeiträume
  geom_point(aes(x = maxDate, color = LotNr), size = 3) +  # Punkte für das letzte Datum
  scale_x_date(date_labels = "%d-%b-%y", date_breaks = "1 month") + # Achsenformatierung
  labs(title = "Parameter-Zeiträume", x = "Datum", y = "Parameter", color = "LotNr") + # Achsenbeschriftung
  theme_minimal() + # Minimalistisches Design
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Achsenbeschriftung drehen

# translate.df <- dbReadTable(con, "TranslationData")
# write.xlsx(translate.df, "TranslationData.xlsx")
# translation.data <- read.xlsx("TranslationData.xlsx") |> 
#   select(-TranslationID)





# # drop TABLE TranslationData from SQLite DB con
# dbExecute(con, "DROP TABLE IF EXISTS TranslationData")


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

# write new TranslationData to SQLite DB---------------------------------------------------------
# col.types <- map.data.types(translation.data)
# create.table.statement <- paste0("CREATE TABLE IF NOT EXISTS TranslationData (",
#                                  paste(
#                                    sapply(names(col.types), function(name) paste0('"', name, '"')),
#                                    col.types, 
#                                    sep=" ", 
#                                    collapse=", "),
#                                  ", TranslationID INTEGER PRIMARY KEY AUTOINCREMENT",
#                                  ", FOREIGN KEY (TestOrderCode) REFERENCES DxIvalData (TestOrderCode)",
#                                  ", FOREIGN KEY (Methode) REFERENCES MethodData (Methode)",
#                                  ")")
# dbExecute(con, create.table.statement)
# dbWriteTable(con, "TranslationData", translation.data, row.names = FALSE, append = TRUE)
# 
# 
# dbReadTable(con, "TranslationData")








# automatically create a SQL TABLE containing QC data--------------------------------------
# map data types from val.dat
qc.dat2 <- qc.dat2 |> 
  mutate(minDate = as.character(minDate),
         maxDate = as.character(maxDate))

col.types <- map.data.types(qc.dat2)

# construct a CREATE TABLE statement for val.dat using the mapped data types
create.table.statement <- paste0("CREATE TABLE IF NOT EXISTS QCData (",
                                 paste(
                                   sapply(names(col.types), function(name) paste0('"', name, '"')),
                                   col.types, 
                                   sep=" ", 
                                   collapse=", "),
                                 ", QCID INTEGER PRIMARY KEY AUTOINCREMENT",
                                 ", FOREIGN KEY (Parameter) REFERENCES TranslationData (RemisolCode)",
                                 ")") 

# connect to SQLite DB
# con <- dbConnect(SQLite(), dbname = "C:/R_local/labStat/ClinicalChemistry_2.db")

# create the table in the SQLite DB
dbExecute(con, create.table.statement)
dbWriteTable(con, "QCData", qc.dat2, row.names = FALSE, append = TRUE)



# Importieren von Daten von QUALAB und BCI-Packungsbeilagen------------------------------------------------------
# Vorbereiten CSV-Datei für Qualab und BCI
write_csv(data.frame(Parameter = unique(qc.dat2$Parameter)), "qualabBCI.csv")

#importieren von Randox-DAten
Randox_immuno1 <- read.csv("LIA5142_Immuno1.csv") |> 
  mutate(L1_Mean = Mean,
         L1_SD = SD) |>
  select(-Mean, -SD)

Randox_immuno2 <- read.csv("LIA5143_immuno2.csv") |> 
  mutate(L2_Mean = Mean,
         L2_SD = SD) |>
  select(-Mean, -SD)

Randox_immuno3 <- read.csv("LIA5144_Immuno3.csv") |> 
  mutate(L3_Mean = Mean,
         L3_SD = SD) |>
  select(-Mean, -SD)

Randox_immuno <- full_join(Randox_immuno1, Randox_immuno2, by = "Analyte") |> 
  full_join(Randox_immuno3, by = "Analyte") |> 
  select(Analyte, L1_Mean, L1_SD, L2_Mean, L2_SD, L3_Mean, L3_SD)

write_excel_csv(Randox_immuno, "Randox_immuno.csv")
# manually filled in information from Randox & BCI package inserts & from www.Qualab.ch
qualab.BCI.randox.data <- read_excel("qualabBCIRANDOX.xlsx") 



col.types <- map.data.types(qualab.BCI.randox.data)

# construct a CREATE TABLE statement for val.dat using the mapped data types
create.table.statement <- paste0("CREATE TABLE IF NOT EXISTS QBRData (",
                                 paste(
                                   sapply(names(col.types), function(name) paste0('"', name, '"')),
                                   col.types, 
                                   sep=" ", 
                                   collapse=", "),
                                 ", QBRID INTEGER PRIMARY KEY AUTOINCREMENT",
                                 ", FOREIGN KEY (RemisolCode) REFERENCES TranslationData (RemisolCode)",
                                 ")") 

# connect to SQLite DB
# con <- dbConnect(SQLite(), dbname = "C:/R_local/labStat/ClinicalChemistry_2.db")

# create the table in the SQLite DB
dbExecute(con, create.table.statement)
dbWriteTable(con, "QBRData", qualab.BCI.randox.data, row.names = FALSE, append = TRUE)
