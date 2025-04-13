fun_add_qc_summary <- function(val_data, qc_summary) {
  val_data |> 
    mutate(
      summary_details = map2(data, Analyt, ~ {
        # Statistiken aus den Daten
        data_summary <- data.frame(
          n = as.character(nrow(.x)),
          Messbereich = paste(
            as.character(round(min(.x$DxI9000, na.rm = TRUE), 2)),
            " - ", 
            as.character(round(max(.x$DxI9000, na.rm = TRUE), 2))
          )
        )
        
        # Ergänzende Statistiken aus qc_summary
        sum_details <- qc_summary |> 
          filter(Analyt == .y) |> 
          select(-Analyt) |> 
          pivot_longer(
            everything(), 
            names_to = "Statistik", 
            values_to = "Werte"
          )
        
        # Zusammenführen
        bind_rows(
          pivot_longer(data_summary, everything(), names_to = "Statistik", values_to = "Werte"), 
          sum_details
        )
      })
    )
}
