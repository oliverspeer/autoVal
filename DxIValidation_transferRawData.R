# prepare libraries and database connection--------------------------

setwd("H:/R/autoVal_H")
source("StartUp.R")
StartUpRoutine()


# load mol masses table-----------------------------------------------------------------------------------------------------
# df.mol.mass <- read_excel("Dev_changeUnitsDxI9000.xlsx") # Dev_changeUnitsDxI9000.xlsx written manually
#save(df.mol.mass, file = "mol_mass.RData")
load("mol_mass.RData")


sum.dat <- setDT(df.mol.mass)
sum.dat$molar_mass <- as.numeric(sum.dat$`molar_mass(g/mol)`)

# read Raw Data------------------------------------------------------
# Define the raw data directory 
dir.rawdata <- "I:\\Institut-Haus 04\\Labor 2_Core Lab Klinische Chemie\\Evaluationen\\Geräte\\DxI9000\\Validation\\2_Rohdaten\\"

# List all raw data CSV files
csv.files <- list.files(path = dir.rawdata, pattern = "*.csv", full.names = TRUE)

# Check if there are any raw data files
if (length(csv.files) > 0) {
  # Identify the latest file based on modification time
  latest.csv <- csv.files[which.max(file.info(csv.files)$mtime)]

  
  
} else {
  print("No raw data files found.")
  stop()
}


# read csv------------------------------------------------
val.dat <- read_csv(latest.csv)

# val.dat <- read_csv("I:\\Institut-Haus 04\\Labor 2_Core Lab Klinische Chemie\\Evaluationen\\Geräte\\DxI9000\\Validation\\2_Rohdaten\\20240527 Messungen 12.csv")


val.dat <- val.dat |>  
  rename(
    PackRevision = `PackRevision...77`,
    PackRevision2 = `PackRevision...89`
  )

setDT(val.dat)


val.dat$Probennummer <- as.numeric(substr(val.dat$SampleID, 1, nchar(val.dat$SampleID) - 2))
val.dat.order <- names(val.dat) # for creation of table later on

# create data.frame to convert units ------------------------------------
df <- data.frame(cbind(val.dat$TestCompleteDT, val.dat$SampleID , val.dat$TestOrderCode, val.dat$TestName, val.dat$DoseResult, val.dat$DoseUnit))

# Rename columns
colnames(df) <- c("TestCompleteDT", "SampleID", "TestOrderCode", "TestName", "DoseResult", "DoseUnit")

# Merge data tables
merged.dat <- merge(df, sum.dat, by = "TestOrderCode", all.x = TRUE)
merged.dat$DoseResult <- as.numeric(merged.dat$DoseResult)



# Function to convert units to a standard form----------------------------
convert_to_standard <- function(unit) {
  install_unit("IU", "umol/min", "international unit")
  # Define a dictionary for common unit conversions
  unit_dict <- list(
    "ug/l" = set_units(1, "ug/l"),
    "ng/ml" = set_units(1, "ng/ml"),
    "nmol/l" = set_units(1, "nmol/l"),
    "umol/l" = set_units(1, "umol/l"),
    "pmol/l" = set_units(1, "pmol/l"),
    "ng/l" = set_units(1, "ng/l"),
    "pg/ml" = set_units(1, "pg/ml"),
    "ug/dl" = set_units(1, "ug/dl"),
    "ng/dl" = set_units(1, "ng/dl"),
    "mU/l" = set_units(1, "mIU/l"),
    "mIU/ml" = set_units(1, "mIU/ml"),
    "uIU/ml" = set_units(1, "uIU/ml"),
    "U/l" = set_units(1, "IU/l"),
    "nmol/l" = set_units(1, "nmol/l"),
    "µg/l" = set_units(1, ug/l),
    "ng/mL" = set_units(1, ng/ml),
    "ng/ml" = set_units(1, ng/ml),
    "nmol/l" = set_units(1, nmol/l),
    "µmol/l" = set_units(1, umol/l),
    "pmol/l" = set_units(1, pmol/l),
    "pmol/L" = set_units(1, pmol/l),
    "ng/l" = set_units(1, ng/l),
    "pg/mL" = set_units(1, pg/ml),
    "µg/dL" = set_units(1, ug/dl),
    "µg/l" = set_units(1, ug/l),
    "ng/dL" = set_units(1, ng/dl),
    "mU/l" = set_units(1, mIU/l),
    "mlU/l" = set_units(1, mIU/l), # Assumed conversion for demonstration
    "U/l" = set_units(1, IU/l),
    "µIU/mL" = set_units(1, uIU/ml),
    "mIU/mL" = set_units(1, mIU/ml),
    "nmol/L" = set_units(1, nmol/l), # assuming case insensitivity
    "nmol/l" = set_units(1, nmol/l),
    "g/mol" = set_units(1, g/mol)# assuming case insensitivity
  )
  
 
  
  # Check if unit exists in dictionary
  if (!unit #_lower 
      %in% names(unit_dict)) {
    message(paste("Unit", unit, "not found in dictionary."))
    return(NULL)
  }
  
  return(unit_dict[[unit]])
}


# convert units to ZLM INLAB  standard form-----------------------------------------
n <- length(merged.dat$DoseResult)
merged.dat$DoseResult_c <- numeric(n)
for (i in 1:n){
  if (is.na(merged.dat$DoseResult[i])) {
    merged.dat$DoseResult_c[i] <- NA
    next
  }
  from_unit <- convert_to_standard(merged.dat$DoseUnit[i])
  to_unit <- convert_to_standard(merged.dat$Einheit_800[i])
  # Compare the units
  is_same_unit <- tryCatch({
    isTRUE(all.equal(from_unit, to_unit))
  }, error = function(e) {
    FALSE
  })
  if (is_same_unit == FALSE && grepl("mol", units(to_unit)$numerator)) {
    merged.dat$DoseResult_c[i] <- set_units(merged.dat$DoseResult[i], from_unit, mode = "standard") / 
      set_units(merged.dat$molar_mass[i], "g/mol") / 
      set_units(1, merged.dat$Einheit_800[i], mode = "standard")
     } else {
    merged.dat$DoseResult_c[i] <- merged.dat$DoseResult[i]
  }
}


# merge converted results back to val.dat-------------------------------------
# Merge data tables
val.dat <- merge(val.dat, merged.dat[, c("TestCompleteDT", "SampleID", "DoseResult_c", "Einheit_800")], by = c('TestCompleteDT', "SampleID"),  all.x = TRUE)

new.col.order <- c("TestOrderCode", "TestName","SampleID", "Probennummer", "DoseResult", "DoseUnit", "DoseResult_c", "Einheit_800", setdiff(val.dat.order, c("TestOrderCode", "TestName", "SampleID", "Probennummer", "DoseResult", "DoseUnit", "DoseResult_c", "Einheit_800"))) 

setcolorder(val.dat, new.col.order)


# insert val.dat into the SQLite DB--------------------------------

dbWriteTable(con, "DxIvalData", val.dat, append = TRUE, row.names = FALSE)

# Disconnect from the database -----------------------------------
dbDisconnect(con)








