fun_prepare_RI_data <- function(data) {
  data  |> 
    rowwise()  |> 
    mutate(
      Referenzintervall = case_when(
        is.na(RI_L) & is.na(RI_H) ~ "",
        RI_L == "<" ~ paste0(RI_L, RI_H),
        TRUE ~ paste0(RI_L, " - ", RI_H)
      )
    )  |> 
    ungroup()  |> 
    select(-RI_L, -RI_H)
}

#' @title fun_prepare_RI_data
#' @description This function prepares reference interval data by creating a new column
#' ' "Referenzintervall" based on the values of "RI_L" and "RI_H". It handles cases where
#' both "RI_L" and "RI_H" are NA, and formats the intervals correctly.
#' The function takes a data frame as input and returns a modified data frame with the new column.
#' #' @param data A data frame containing reference interval data with columns "RI_L" and "RI_H". 
#' Normaly produced by the function "fun_get_RI_SQL_data".
#' #' @return A data frame with a new column "Referenzintervall" containing the formatted reference intervals.
#' #' @examples
#' #' # Example data frame
#' #' df <- data.frame(
#' #'   RI_L = c(1, 2, NA, "<"),
#' #'   RI_H = c(5, 10, 20, 30)
#' #' #' )
#' #' #' # Prepare the data
#' #' #' prepared_data <- fun_prepare_RI_data(df)
#' #' #' # Print the result
#' #' #' print(prepared_data)
#' #' @export
#' fun_prepare_RI_data
#' @importFrom dplyr rowwise mutate case_when ungroup select
 