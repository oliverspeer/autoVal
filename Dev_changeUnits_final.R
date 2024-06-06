 # library(units)
 # library(data.table)
 # library(dplyr)
 
 source("startUp.R")
 StartUpRoutine()
 
 dir.rawdata <- "I:\\Institut-Haus 04\\Labor 2_Core Lab Klinische Chemie\\Evaluationen\\Geraete\\DxI9000\\Validation\\2_Rohdaten\\"
 
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
   val.dat$DoseResult <- as.numeric(val.dat$DoseResult)
 }
 
 
 
 load("mol_mass.RData")
 # Example sum.dat for demonstration
 sum.dat <- setDT(df.mol.mass)
 sum.dat$molar_mass <- as.numeric(sum.dat$`molar_mass(g/mol)`)
 
 
 # Function to convert units to a standard form
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
         
           # Convert to lowercase to handle case insensitivity
           unit_lower <- tolower(unit)
           
             # Check if unit exists in dictionary
             if (!unit_lower %in% names(unit_dict)) {
                 message(paste("Unit", unit, "not found in dictionary."))
                 return(NULL)
               }
           
             return(unit_dict[[unit_lower]])
         }
   # Function to compare units and convert if necessary
     compare_and_convert_single <- function(dose_results, dose_unit, einheit_800, molar_mass) {
         dose_results <- as.numeric(dose_results)  # Ensure dose_result is numeric
         
           # Convert units to standard form
           standard_from_unit <- tryCatch({
               convert_to_standard(dose_unit)
             }, error = function(e) {
                 return(NULL)
               })
           
             standard_to_unit <- tryCatch({
                 convert_to_standard(einheit_800)
               }, error = function(e) {
                   return(NULL)
                 })
             
               if (is.null(standard_from_unit) || is.null(standard_to_unit)) {
                   message(paste("Unit conversion failed for units:", dose_unit, "and", einheit_800))
                   return(NA)  # Return NA for unrecognized units
                 }
             
               # Compare the units
               is_same_unit <- tryCatch({
                   isTRUE(all.equal(standard_from_unit, standard_to_unit))
                 }, error = function(e) {
                     FALSE
                   })
               
                 # If units are not the same, convert the result to molar concentration
                 if (!is_same_unit) {
                     converted_result <- set_units(dose_results, convert_to_standard(dose_unit), mode = "standard") / 
                         set_units(molar_mass, "g/mol") / 
                         set_units(1, einheit_800, mode = "standard")
                     # converted_result <- convert_to_molar(dose_results, standard_from_unit, molar_mass)
                       return(as.numeric(converted_result))
                   } else {
                       return(dose_results)
                     }
             }
     # Function to iterate over each row and apply unit conversion
       compare_and_convert <- function(dose_results, dose_units, einheit_800s, molar_masses) {
           n <- length(dose_results)
           results <- numeric(n)
           
             for (i in 1:n) {
                 results[i] <- compare_and_convert_single(dose_results[i], dose_units[i], einheit_800s[i], molar_masses[i])
               }
           
             return(results)
         }
      # Example sum.dat for demonstration
         # sum.dat <- data.table(
         #     TestOrderCode = c(212, 213, 214), 
         #     Einheit_800 = c("µg/l", "nmol/l", "ng/mL"), 
         #     molar_mass = c(180.16, 300.29, 100.1)  # Example molecular masses
         #   )
         # Example val.dat for demonstration
           # val.dat <- data.table(
           #     TestOrderCode = c(212, 212, 213, 214),
           #     DoseUnit = c("ng/mL", "µg/l", "ng/mL", "ng/mL"),
           #     DoseResult = c(5, 10, 20, 15)
           #   )
       
       
       
           # Merge data tables
             merged.dat <- merge(val.dat, sum.dat, by = "TestOrderCode", all.x = TRUE)
             # Apply the comparison and conversion function row by row
               merged.dat <- merged.dat %>%
                   rowwise() %>%
                   mutate(
                       Result = compare_and_convert_single(DoseResult, DoseUnit, Einheit_800, molar_mass)
                     ) %>%
                   ungroup()
               # View the updated results
                 print(merged.dat$Result)
                 print(merged.dat$DoseResult)
                 