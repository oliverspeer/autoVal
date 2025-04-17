fun_load_process_DxI9000_data <- function(csv_path, sum_dat) {
  #  source("StartUp.R")
  # StartUpRoutine()
  # source("R/fun_convert_to_standard.R")
  #  library(targets)
  # tar_load(molmass_data)
  # tar_load(raw_data_csv)
  #
  # sum_dat <- molmass_data
  # csv_path <- raw_data_csv
  
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
    
    merge(sum_dat, by = "TestOrderCode", all.x = TRUE) |>
    
    mutate(
      Symbol = sub("([><]).*", "\\1", DoseResult),
      
      Symbol = ifelse(Symbol == DoseResult, NA, Symbol),
      
      DoseResult = sub("[><]", NA, DoseResult) |> as.numeric()
      
    )
  
  
  
  # Function to convert units to a standard form----------------------------
  
  
  
  
  
  
  
  # convert units to ZLM INLAB  standard form-----------------------------------------
  
  n <- length(merged.dat$DoseResult)
  
  merged.dat$DoseResult_c <- numeric(n)
  
  for (i in 1:n) {
    if (is.na(merged.dat$DoseResult[i])) {
      merged.dat$DoseResult_c[i] <- NA
      
      next
      
    }
    
    from_unit <- fun_convert_to_standard(merged.dat$DoseUnit[i])
    
    to_unit <- fun_convert_to_standard(merged.dat$Einheit_800[i])
    
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
  
  # archive <- "C:/R_local/autoVal/2_Rohdaten/sql_importiert/"
  # files <- csv_path
  # files.name <- basename(files)
  # 
  # # Verschiebe alle Dateien außer "BlutDxI_dummy.xlsx"
  # for (file in files) {
  #   if (basename(file) != "250417dummy.csv") {
  #     file_move(file, file.path(archive, basename(file)))
  #   }
  # }
  
  return(val.dat)
}
