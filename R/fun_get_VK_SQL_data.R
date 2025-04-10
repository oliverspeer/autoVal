fun_get_VK_SQL_data <- function(con) {
  dbGetQuery(con, "
  SELECT DISTINCT 
    d.TestName AS DxI9000, 
    --q.'QUALAB_3S[%]', 
    q.'BCI_VK[%]'
  FROM QBRData q
  JOIN DxIvalData d USING (TestName)
  WHERE d.DoseResult <> 'No result';")
}

#' @title fun_get_VK_SQL_data
#' @description This function retrieves VK data from the SQL database.
#' It uses a SQL query to select distinct test names and their corresponding VK values.
#' The function connects to the database using the provided connection object and executes the query.
#' The result is returned as a data frame.
#' #' @param con A database connection object.
#' #' @return A data frame containing the VK data.
#' #' @examples
#' #' # con <- dbConnect(RSQLite::SQLite(), "path/to/database.sqlite")
#' #' #' # Retrieve VK data
#' # #' vk_data <- fun_get_VK_SQL_data(con)
#' #' #' # Close the database connection
#' # #' # dbDisconnect(con)
#' #' @export
#' fun_get_VK_SQL_data
#' @importFrom DBI dbGetQuery