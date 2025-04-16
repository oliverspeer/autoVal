fun_load_process_DxI9000_data <- function(csv_path, sum_dat) {
  source("StartUp.R")
 StartUpRoutine()
 source("R/fun_convert_to_standard.R")
  library(targets)
 tar_load(molmass_data)  
 tar_load(raw_data_csv)
 
 sum.dat <- molmass_data  
 csv_path <- raw_data_csv
 
 # Lese die CSV-Datei ein und benenne die Spalten um
  val.dat <- read_csv(csv_path) |>
    rename(
      PatientID = `Patient ID`,
      SampleID = `Sample ID`,
      TestName = `Test Name`,
      TestCompleteDT = `Comp. Time`,
      TestOrderCode = `Test ID`,
      DoseResult = `Result`,
      DoseUnit = `Units`,
      SampleLoadDT = `Load Date/Time`
    ) |>
    mutate(Probennummer = as.numeric(substr(SampleID, 1, nchar(SampleID) - 2))) |>
    setDT()
  
  val.dat.order <- names(val.dat) # for creation of table later on
  
  # Erstelle DataFrame zur Konvertierung der Einheiten
  df <-
    val.dat[, .(TestCompleteDT,
                SampleID,
                TestOrderCode,
                TestName,
                DoseResult,
                DoseUnit)]
  
  # Merge data tables
  
  
  
  merged.dat <- df |>
    
    merge(sum.dat, by = "TestOrderCode", all.x = TRUE) |>
    
    mutate(
      Symbol = sub("([><]).*", "\\1", DoseResult),
      
      Symbol = ifelse(Symbol == DoseResult, NA, Symbol),
      
      DoseResult = sub("[><]", "", DoseResult) |> as.numeric()
      
    )
  
  
  
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
      
      "µg/l" = set_units(1, ug / l),
      
      "ng/mL" = set_units(1, ng / ml),
      
      "ng/ml" = set_units(1, ng / ml),
      
      "nmol/l" = set_units(1, nmol / l),
      
      "µmol/l" = set_units(1, umol / l),
      
      "pmol/l" = set_units(1, pmol / l),
      
      "pmol/L" = set_units(1, pmol / l),
      
      "ng/l" = set_units(1, ng / l),
      
      "pg/mL" = set_units(1, pg / ml),
      
      "µg/dL" = set_units(1, ug / dl),
      
      "µg/l" = set_units(1, ug / l),
      
      "ng/dL" = set_units(1, ng / dl),
      
      "mU/l" = set_units(1, mIU / l),
      
      "mlU/l" = set_units(1, mIU / l),
      # Assumed conversion for demonstration
      
      "U/l" = set_units(1, IU / l),
      
      "µIU/mL" = set_units(1, uIU / ml),
      
      "mIU/mL" = set_units(1, mIU / ml),
      
      "nmol/L" = set_units(1, nmol / l),
      # assuming case insensitivity
      
      "nmol/l" = set_units(1, nmol / l),
      
      "g/mol" = set_units(1, g / mol)# assuming case insensitivity
      
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
  
  for (i in 1:n) {
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
    
    if (is_same_unit == FALSE &&
        grepl("mol", units(to_unit)$numerator)) {
      merged.dat$DoseResult_c[i] <-
        set_units(merged.dat$DoseResult[i], from_unit, mode = "standard") /
        
        set_units(merged.dat$molar_mass[i], "g/mol") /
        
        set_units(1, merged.dat$Einheit_800[i], mode = "standard")
      
    } else {
      merged.dat$DoseResult_c[i] <- merged.dat$DoseResult[i]
      
    }
    
  }
  
  
  
  
  
  # merge converted results back to val.dat-------------------------------------
  
  # Merge data tables
  
  val.dat <- merge(
    val.dat,
    
    merged.dat[, c("TestCompleteDT",
                   "SampleID",
                   "Symbol",
                   "DoseResult_c",
                   "Einheit_800")],
    
    by = c('TestCompleteDT', "SampleID"),
    
    all.x = TRUE
    
  ) |>
    
    setcolorder(
      c(
        "TestOrderCode",
        "TestName",
        "SampleID",
        "Probennummer",
        "DoseResult",
        "DoseUnit",
        "Symbol",
        "DoseResult_c",
        "Einheit_800",
        
        setdiff(
          colnames(val.dat),
          c(
            "TestOrderCode",
            "TestName",
            "SampleID",
            "Probennummer",
            "DoseResult",
            "DoseUnit",
            "Symbol",
            "DoseResult_c",
            "Einheit_800"
          )
        )
        
      )
      
    ) |>
    
    mutate(
      TestCompleteDT = TestCompleteDT |>
        
        as.POSIXct(format = "%m.%d.%Y %H:%M:%S", tz = "UTC") |>
        
        format("%m-%d-%Y %I:%M:%OS3 %p") %>%
        
        gsub("vorm\\.", "AM", .) %>%
        
        gsub("nachm\\.", "PM", .),
      
      SampleLoadDT = SampleLoadDT |>
        
        as.POSIXct(format = "%m.%d.%Y %H:%M:%S", tz = "UTC") |>
        
        format("%m-%d-%Y %I:%M:%OS3 %p") %>%
        
        gsub("vorm\\.", "AM", .) %>%
        
        gsub("nachm\\.", "PM", .)
    )
  
  return(val.dat)
}
