fun_process_qc_summary <- function(ri_data, vk_data, qc_results) {
  qc_summary <- ri_data |> 
    merge(vk_data, by = "Analyt", all = TRUE) |> 
    merge(qc_results, by = "Analyt", all = TRUE) |> 
    mutate(across(everything(), ~ replace_na(.x, 0))) |>     # fehlende Werte durch "0" ersetzen
    filter(!row_number() %in% 4) |>                           # 4. Zeile entfernen
    mutate(across(everything(), as.character))               # alle Spalten als character casten
  
  #return(qc_summary)
}

#' @title Funktion zum Erstellen der QC-Übersicht
#' @description Diese Funktion erstellt eine Übersicht der QC-Daten.
#' @param ri_data Datenrahmen mit RI-Daten
#' @param vk_data Datenrahmen mit VK-Daten
#' @param qc_results Datenrahmen mit QC-Ergebnissen
#' @return Datenrahmen mit QC-Übersicht
#' @export
#' @examples
#' ri_data <- data.frame(Analyt = c("A", "B", "C"), RI = c(1, 2, 3))
#' vk_data <- data.frame(Analyt = c("A", "B", "C"), VK = c(4, 5, 6))
#' qc_results <- data.frame(Analyt = c("A", "B", "C"), QC = c(7, 8, 9))
#' qc_summary <- fun_process_qc_summary(ri_data, vk_data, qc_results)
#' print(qc_summary)
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr replace_na
#' @importFrom stats setNames
#' @importFrom base merge
#' @importFrom base as.character
#' 