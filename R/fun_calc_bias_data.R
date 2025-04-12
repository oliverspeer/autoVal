fun_calc_bias_data <- function(prc_data, data) {
  calculate_bias_interval(
    'E',
    nrun = prc_data$N / prc_data$k,
    nrep = prc_data$k,
    SWL = prc_data$SWL,
    SR = prc_data$SR,
    nsamples = 2,
    expected_mean = data$Zielwert[25],
    user_mean = prc_data$mean
  )
}

