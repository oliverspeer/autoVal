fun_filter_QC_data <- function(qc_data) {
  qc_data  |> 
    mutate(
      Date = as.Date(TestCompleteDT, "%m-%d-%Y"),
      maxDate = as.Date(maxDate, "%Y-%m-%d"),
      minDate = as.Date(minDate, "%Y-%m-%d")
    )  |> 
    filter(Date <= maxDate & Date >= minDate) |>  
    filter(grepl("^QC", SampleID)) |> 
    mutate(Date = as.character(Date)) 
  
}

#' @title filter_QC_data_by_date
#' @description This function filters QC data based on the date range specified in the data.
#' It converts the "TestCompleteDT", "maxDate", and "minDate" columns to Date format and filters the data
#' to include only rows where the "Date" falls within the range defined by "maxDate" and "minDate".
#' The function takes a data frame as input and returns a modified data frame with the filtered data.
#' 