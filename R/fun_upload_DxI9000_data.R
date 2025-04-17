fun_upload_DxI9000_data <- function(con, data, table_name = "DxIvalData") {
 
  # Schreibe die verarbeiteten Daten in die Datenbank
  DBI::dbWriteTable(con, table_name, data, append = TRUE, row.names = FALSE)

  fun_hash_MeasurementData(con, table_name)  
  }
