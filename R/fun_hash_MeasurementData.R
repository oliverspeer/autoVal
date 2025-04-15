fun_hash_MeasurementData <- function(con, table_name = "MeasurementData") {
  # this is still sub optimal, as the data needs to be returned to R
  data <- DBI::dbReadTable(con, table_name)
  # this could be optimized using heuristics to get a hash of the table in a better way
  rlang::hash(data)
}