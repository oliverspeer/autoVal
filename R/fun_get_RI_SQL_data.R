fun_get_RI_SQL_data <- function(con) {
  dbGetQuery(con, "
    SELECT DISTINCT
      d.TestName AS DxI9000,
      m.REF_L_M AS RI_L,
      m.REF_H_M AS RI_H
    FROM MethodData m
    JOIN TranslationData t USING (Methode)
    JOIN DxIvalData d USING (TestOrderCode);
  ")
}

#' @title fun_get_RI_SQL_data
#' @description This function retrieves reference interval data from the SQL database.
#' It uses a SQL query to select distinct test names and their corresponding reference intervals. 
#' The function connects to the database using the provided connection object and executes the query.
#' The result is returned as a data frame.
#' @param con A database connection object.
#' @return A data frame containing the reference interval data.
#' @examples
#' # con <- dbConnect(RSQLite::SQLite(), "path/to/database.sqlite")
#' #' # Retrieve reference interval data
#' #' ri_data <- fun_get_RI_SQL_data(con)
#' #' # Close the database connection
#' #' # dbDisconnect(con)
#' @export
#' fun_get_RI_SQL_data
#' @importFrom DBI dbGetQuery