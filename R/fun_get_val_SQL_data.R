fun_get_val_SQL_data <- function(con, MD_hash) {
  
  
  if (is.null(MD_hash)) {
    stop("Die Tabelle 'MeasurementData' ist leer oder nicht vorhanden.")
  }
dbGetQuery(con, "
  SELECT DISTINCT
    d.TestName AS Analyt,
    CAST(md.Werte AS REAL) AS DxI800,
    MAX(d.DoseResult_c) AS DxI9000,
    m.EINHEIT AS Einheit,
    d.Probennummer
  FROM MeasurementData md
  JOIN MethodData m USING (Methode)
  JOIN TranslationData t USING (Methode)
  JOIN DxIvalData d ON d.TestOrderCode = t.TestOrderCode AND d.Probennummer = md.Probennummer
  WHERE d.DoseResult <> 'No result'
  GROUP BY d.TestName, md.Werte, d.Probennummer, m.EINHEIT;
  ")
}

#' @title function to extract double measurement data from SQLite database
#' @description 
#' @param con A database connection object.
#' @return A data frame containing the reference interval data.
#' @examples 