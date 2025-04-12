fun_nest_val_data <- function(val_dat) {
  # Check if val_dat is a data frame
  if (!is.data.frame(val_dat)) {
    stop("val_dat must be a data frame")
  }
  
  # Check for required columns
  required_columns <- c("Analyt", "DxI800", "DxI9000", "Einheit", "Probennummer")
  missing_columns <- setdiff(required_columns, names(val_dat))
  
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Process the data
 val_dat |> 
  mutate(
    DxI800 = as.numeric(DxI800), 
    DxI9000 = as.numeric(DxI9000),
    Delta = round((100-DxI800*100/DxI9000), 1)
  ) |> 
  select(Analyt, DxI800, DxI9000, Einheit, Delta, Probennummer) |>
  na.omit()|> 
  group_by(Analyt) |> 
  nest() |> 
  arrange(Analyt) |>
  ungroup()
 
}