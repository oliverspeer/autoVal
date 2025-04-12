fun_add_wide_QC_data <- function(df, data) {
  df |> 
    select(Analyt) |> 
    rowwise() |>
    mutate(
      prc.L.raw.data = list(fun_wide_QCData_CLSI(Analyt, 1, "[a-zA-Z]+1", data)),
      prc.H.raw.data = list(fun_wide_QCData_CLSI(Analyt, 2, "[a-zA-Z]+2", data))
    ) |> 
     ungroup()
}