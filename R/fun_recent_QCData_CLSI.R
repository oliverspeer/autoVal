get_recent_data <- function(df, level_pattern) {
  # Filtere nach dem Pattern (z.B. "[a-zA-Z]+1" oder "[a-zA-Z]+2")
  df <- df %>% 
    filter(grepl(level_pattern, SampleID))
  
  # Gruppieren nach Datum und nur Gruppen behalten, die mindestens 5 Messungen haben
  df <- df %>%
    group_by(Date) %>%
    filter(n() >= 5) %>%
    ungroup()
  
  # Sortiere absteigend nach Datum (also aktuellste zuerst)
  df <- df %>% arrange(desc(Date))
  
  # Akkumulieren der Zeilen anhand der aktuellsten Datumsgruppen, bis mindestens 25 Zeilen erreicht sind
  dates_ordered <- unique(df$Date)
  selected <- data.frame()
  for (d in dates_ordered) {
    group_data <- df %>% filter(Date == d)
    selected <- bind_rows(selected, group_data)
    if (nrow(selected) >= 25) break
  }
  
  # Falls nach dem Akkumulieren weniger als 25 Zeilen vorhanden sind, wird NULL zurückgegeben
  if (nrow(selected) < 25) {
    msg <- paste("Zuwenig Messungen: Es wurden nur ", nrow(selected), " Zeilen gefunden, aber 25 benoetigt.")
    message(msg) # message() wird nicht unterdrückt
    warning(msg) # warning() wird unterdrückt
    return(NULL)
  }
  
  # Falls mehr als 25 Zeilen vorhanden sind: Versuche, aus der letzten (am wenigsten aktuellen) Gruppe
  # so viele Zeilen zu entfernen, dass am Ende genau 25 Zeilen vorliegen,
  # ohne dass aus diesem Datum weniger als 5 Zeilen verbleiben.
  if (nrow(selected) > 25) {
    last_date <- tail(unique(selected$Date), 1)
    group_last <- selected %>% filter(Date == last_date)
    excess <- nrow(selected) - 25
    if ((nrow(group_last) - excess) >= 5) {
      selected <- bind_rows(
        selected %>% filter(Date != last_date),
        group_last %>% slice(1:(nrow(group_last) - excess))
      )
    } else {
      # Falls ein Trimmen den letzten Tag unter 5 Messungen bringen würde,
      # werden einfach die ersten 25 Zeilen genommen.
      selected <- selected %>% slice(1:25)
    }
  }
  
  return(selected)
}
