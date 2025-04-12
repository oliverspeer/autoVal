# Funktion zum Filtern und Verarbeiten der Daten für einen bestimmten TestName
fun_process_QC_data <- function(test_name, data) {
  test_data <- filter(data, DxI9000 == test_name)
  
  # Daten für Level 1 (L) und Level 2 (H) filtern
  # prc.L.data <- filter(test_data, Level == 1 & grepl("[a-zA-Z]+1", SampleID)) |> 
  #   arrange(desc(Date))
  # prc.H.data <- filter(test_data, Level == 2 & grepl("[a-zA-Z]+2", SampleID)) |> 
  #   arrange(desc(Date))
  
  # Für Level 1 und Level 2: Daten zuerst nach Level filtern, dann die aktuellsten Daten gemäß get_recent_data auswählen
  prc.L.data <- get_recent_data(test_data %>% filter(Level == 1), "[a-zA-Z]+1")
  prc.H.data <- get_recent_data(test_data %>% filter(Level == 2), "[a-zA-Z]+2")
  
  # Prüfen, ob beide Levels gültige Daten (mindestens 25 Zeilen) liefern
  if (is.null(prc.L.data) || is.null(prc.H.data) ||
      nrow(prc.L.data) < 25 || nrow(prc.H.data) < 25) {
    warning(sprintf("Not enough data for %s. Skipping.", test_name))
    return(NULL)
  }
  
  # Berechnung von Präzisionsmetriken
  prc.data.L <- fun_calc_prc_metrics(prc.L.data)
  prc.data.H <- fun_calc_prc_metrics(prc.H.data)
  
  # Berechnung von Bias-Werten
  bias.L <- fun_calc_bias_data(prc.data.L, prc.L.data)
  bias.H <- fun_calc_bias_data(prc.data.H, prc.H.data)
  
  # Erstellung des Wide-Format DataFrames
  data.frame(
    Analyt = test_name,
    `Intra_Assay_VK_L` = round(prc.data.L$CVR, 2),
    `Inter_Assay_VK_L` = round(prc.data.L$CVWL, 2),
    `bias.L` = round(bias.L$bias, 2),
    `Intra_Assay_VK_H` = round(prc.data.H$CVR, 2),
    `Inter_Assay_VK_H` = round(prc.data.H$CVWL, 2),
    `bias.H` = round(bias.H$bias, 2)
  )
  
  
}