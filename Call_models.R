
### must add package for specific models ###
#install.packages("devtools")
#library(devtools)
#install_github("gabrielrvsc/HDeconometrics")
#install.packages("randomForest")
#install.packages("e1071")
library(HDeconometrics)
library(glmnet)
library(randomForest)
library(e1071)



source('C:/Users/Gebruiker/Documents/Forecasting-Inflation/Analysis/Functions/Rolling_window.R')
source('C:/Users/Gebruiker/Documents/Forecasting-Inflation/Analysis/Functions/Models.R')

#####
## The file with the forecasts will be saved with model_name
model_name = "SVR"
## The function called to run models is model_function, which is a function from functions.R
model_function = runsvr
#####


load("dataset.rda")
dates = dataset$date
data = dataset%>%select(-date)%>%as.matrix()
rownames(data) = as.character(dates)

####### run rolling window ##########
nwindows = 277
model_list = list()
for(i in 1:12){ 
  model = rolling_window(model_function,data,nwindows+i-1,i,"data.CPIAUCSL")
  model_list[[i]] = model
  cat(i,"\n")
}

forecasts = Reduce(cbind,lapply(model_list, function(x)head(x$forecast,nwindows)))

forecasts = accumulate_model(forecasts)

save(forecasts,file = paste("",model_name,".rda",sep = ""))


plot(tail(data[,"data.CPIAUCSL"],277),type = "l")
lines(forecasts[,1],col = 3)

forecast_data = data.frame(tail(dates,277), tail(data[,"data.CPIAUCSL"],277), forecasts[,1])
colnames(forecast_data) = c("date", "CPI", "forecast_CPI")
ggplot(forecast_data, aes(x=date)) + 
  geom_line(aes(y = CPI), color = "darkred") + 
  geom_line(aes(y = forecast_CPI), color="steelblue", linetype="twodash")
