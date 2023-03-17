#install.packages("Metrics")

library(tidyverse)
library(Metrics)
library(kernelshap)
library(shapviz)

# Extract forecasts from this model:
model_name = "Ridge"
doc_type = ".rda"  
doc_name = paste(model_name,doc_type, sep = "")

# Load the forecasts and actual data
load("dataset.rda")
dates = dataset$date
data = dataset%>%select(-date)%>%as.matrix()
CPI_out = tail(data[, "data.CPIAUCSL"],277)
load(doc_name)
load("yout.rda")

# Columns: t+1,...,t+12, acc 3, acc 6, acc 12
# Time: 277 obs (out-of-sample period)
# For t+12, January 2023 (last data point) should be compared with January 2022
# Compare for period: start sample period: total obs - window until total obs - horizon
nwindows = nrow(forecasts)
rmse_vector = vector( "double" , 15 )
mae_vector = vector( "double" , 15 )
mad_vector = vector( "double" , 15 )


for (i in 1:12) {
  inf_out = tail(CPI_out, nwindows - i)
  forecast_vector = head(forecasts[,i], nwindows - i)
  rmse_vector[i] = rmse(inf_out, forecast_vector)
  mae_vector[i] = mae(inf_out, forecast_vector)
  mad_vector[i] = mad(inf_out, forecast_vector)
}

# Evaluate accumulated forecast
acc3 = yout[,2]
acc3 = tail(acc3, nwindows - 2)
acc6 = yout[,3]
acc6 = tail(acc6, nwindows - 5)
acc12 = yout[,4]
acc12 = tail(acc12, nwindows - 11)

acc3_for = forecasts[,13]
acc3_for = acc3_for[!is.na(acc3_for)]
acc6_for = forecasts[,14]
acc6_for = acc6_for[!is.na(acc6_for)]
acc12_for = forecasts[,15]
acc12_for = acc12_for[!is.na(acc12_for)]


rmse_vector[13] = rmse(acc3, acc3_for)
rmse_vector[14] = rmse(acc6, acc6_for)
rmse_vector[15] = rmse(acc12, acc12_for)

mae_vector[13] = mae(acc3, acc3_for)
mae_vector[14] = mae(acc6, acc6_for)
mae_vector[15] = mae(acc12, acc12_for)

mad_vector[13] = mad(acc3, acc3_for)
mad_vector[14] = mad(acc6, acc6_for)
mad_vector[15] = mad(acc12, acc12_for)

# Store rmse, mae and mad in matrix
metrics = matrix(NA,3,15)
metrics[1,] = rmse_vector
metrics[2,] = mae_vector
metrics[3,] = mad_vector
save(metrics, file = paste("Metrics",model_name,".rda",sep = ""))





