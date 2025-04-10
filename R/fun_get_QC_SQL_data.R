fun_get_QC_SQL_data <- function(con) {
  dbGetQuery(con, "
    SELECT d.TestName AS DxI9000, 
           d.TestCompleteDT, 
           d.SampleID, 
           q.Level,
           CAST(d.DoseResult_c AS REAL) AS DoseResult, 
           d.DoseUnit, 
           d.Einheit_800, 
           q.Zielwert, 
           q.LotNr, 
           q.maxDate, 
           q.minDate
    FROM DxIvalData d
    JOIN TranslationData t ON d.TestOrderCode = t.TestOrderCode
    JOIN QCData q ON q.Parameter = t.RemisolCode
    WHERE d.DoseUnit IS NOT NULL;
")
}

#' @title fun_get_QC_SQL_data
#' @description This function retrieves QC data from the SQL database.
#' It uses a SQL query to select distinct test names and their corresponding QC values.
#' The function connects to the database using the provided connection object and executes the query.
#' The result is returned as a data frame.
#' #' @param con A database connection object.
#' #' #' @return A data frame containing the QC data.
#' #' #' @examples
#' # #' # con <- dbConnect(RSQLite::SQLite(), "path/to/database.sqlite")
#' # #' #' # Retrieve QC data
#' # #' # qc_data <- fun_get_QC_SQL_data(con)
#' # #' #' # Close the database connection
#' # #' # dbDisconnect(con)
#' #' #' @export
#' fun_get_QC_SQL_data
#' @importFrom DBI dbGetQuery
