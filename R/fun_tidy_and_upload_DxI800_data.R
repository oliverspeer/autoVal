fun_tidy_and_upload_DxI800_data <- function(con, 
                                            raw_data_files, 
                                            table_name = "MeasurementData") {
  # Lese und verarbeite die Excel-Daten
  dxi.data <- fun_read_multi_excel_data(raw_data_files, dt_name = "dxi.data")
  DT.tidy.dxi <- fun_write_tidy_data(data = dxi.data, dt_name = "DT.tidy.dxi")
  
  # Schreibe die verarbeiteten Daten in die Datenbank
  DBI::dbWriteTable(con, table_name, DT.tidy.dxi, append = TRUE, row.names = FALSE)

  fun_hash_MeasurementData(con, table_name)  
  }
