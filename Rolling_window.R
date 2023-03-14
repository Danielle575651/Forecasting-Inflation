# rolling_window: Uses a rolling window approach to construct (pseudo)-out-of-sample forecasts
  # f = function
  # data = data
  # nForecast = n of out-of-sample forecast
  # horizon = forecasting horizon
  # variable = target variable
# Based on paper: Forecasting inflation in a data-rich environment

rolling_window = function(f, data, nForecast, horizon, variable,...){
  ind = 1:nrow(data) # size of the data set
  window_size = nrow(data) - nForecast # size of the window
  indmat = matrix(NA, window_size, nForecast) # matrix to store the forecasts
  indmat[1,] = 1:ncol(indmat)
  
  for (i in 2:nrow(indmat)) {
    indmat[i,] = indmat[i-1,]+1
  }
  # Apply the model (f) to get the forecasts
  rw = apply(indmat, 2, f, data = data, horizon = horizon, variable = variable, ...)
  forecast = unlist(lapply(rw, function(x)x$forecast))
  outputs = lapply(rw, function(x)x$outputs)
  return(list(forecast = forecast, outputs = outputs))
}