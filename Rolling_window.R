#fn = function, df = data, nwindow = window size, horizon = forecasting horizon, variable = target variable
rolling_window=function(fn,df,nwindow,horizon,variable,...){
  # nrow(df) = number of observations full sample
  ind=1:nrow(df)
  # window_size = number of observations full sample - the window we want to move over
  window_size=nrow(df)-nwindow
  indmat=matrix(NA,window_size,nwindow)
  indmat[1,]=1:ncol(indmat)
  
  for (i in 2:nrow(indmat)) {
    indmat[i,]=indmat[i-1,]+1
  }
  
  # Apply is a substitute to a loop
  # Margin = 2, manipulation is performed over the columns
  # Fun = fn, the function to apply
  rw = apply(indmat,2,fn,df=df,horizon=horizon,variable=variable,...)
  forecast = unlist(lapply(rw,function(x)x$forecast))
  
  # lapply: performs operations on list objects and returns a list of objects of
  # same length as the original set
  outputs = lapply(rw,function(x)x$outputs)
  
  shapley_values = unlist(lapply(rw, function(x)x$shapley_values))
  Xout = unlist(lapply(rw, function(x)x$Xout))
  return (list(forecast=forecast, outputs=outputs, shapley_values = shapley_values, Xout = Xout))
  
}