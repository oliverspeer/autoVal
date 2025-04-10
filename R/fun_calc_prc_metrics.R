fun_calc_prc_metrics <- function(data) {
  long_data <- data.frame(
    rep = rep(1:5, each = 5),
    name = paste(data$SampleID[1:25], rep(1:5, each = 5), sep = "_"),
    value = data$DoseResult[1:25],
    Zielwert = data$Zielwert[1:25]
  )
  calculate_aov_infos(long_data)
}

#' @title Calculate AOV Information
#' @description This function calculates the AOV information for the given data.
#' It performs ANOVA and imprecision estimates, and returns the results.
#' @param data A data frame containing the data to be analyzed.
#' @return A list containing the results of the ANOVA and imprecision estimates.
#' @details
#' This function performs the following steps:
#' 1. Reshapes the data into long format.
#' 2. Performs ANOVA on the reshaped data.
#' 3. Calculates imprecision estimates.
#' 4. Returns the results as a list.
#' @examples
#' 
#' 