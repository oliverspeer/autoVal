fun_add_paba_regression <- function(df) {
  df |> 
    mutate(
      PBreg = map(data, \(data) {
        if (nrow(data) < 7 || sd(data$DxI800) == 0 || sd(data$DxI9000) == 0) {
          return(NULL)
        } else {
          PBreg <- mcreg(data$DxI800, data$DxI9000, method.reg = "PaBa")
          PBreg@mnames[1:2] <- c("DxI800", "DxI9000")
          return(PBreg)
        }
      })
    )
}
