# Funktion zur Darstellung der QC-Rohdaten in das breite CLSI 5x5 Format
process_raw_data <- function(analyt, level, pattern) {
  result <- tryCatch({
    get_recent_data(all_data_qc |> filter(Level == level & DxI9000 == analyt), pattern)
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })
  
  if (!is.null(result)) {
    result |>
      mutate(Date = as.Date(Date)) |>  # Konvertiere Date in ein Date-Objekt
      arrange(Date) |>
      group_by(Date) |>
      mutate(row_id = row_number(),
             DoseResult = round(DoseResult, 2)) |>
      select(Date, DoseResult, row_id) |>
      pivot_wider(
        names_from = Date,
        values_from = DoseResult,
        id_cols = row_id
      ) |>
      select(-row_id)
  } else {
    return(NULL)
  }
}