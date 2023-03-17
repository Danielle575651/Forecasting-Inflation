
### must add package for specific models ###
#install.packages("devtools")
#install_github("gabrielrvsc/HDeconometrics")
#install.packages("randomForest")
#install.packages("e1071")
#install.packages("grf")
#install_github("ebprado/MOTR-BART/MOTRbart")
#install.packages("kernelshap")
#install.packages("shapviz")
#install_github("philgoucou/macrorf")
library(devtools)
library(HDeconometrics)
library(glmnet)
library(randomForest)
library(e1071)
library(grf)
library(MOTRbart)
library(MacroRF)
library(kernelshap)
library(shapviz)

source('C:/Users/Gebruiker/Documents/Forecasting-Inflation/Analysis/Functions/Rolling_window.R')
source('C:/Users/Gebruiker/Documents/Forecasting-Inflation/Analysis/Functions/Models.R')


#####
## The file with the forecasts will be saved with model_name
model_name = "Elastic net"
## The function called to run models is model_function, which is a function from functions.R
model_function = runadaelasticnet
#####


load("dataset.rda")
dates = dataset$date
data = dataset%>%select(-date)%>%as.matrix()
rownames(data) = as.character(dates)

####### run rolling window ##########
nwindows = 277
model_list = list()
#for(i in 1:12){ 
i=3
  model = rolling_window(model_function,data,nwindows+i-1,i,"data.CPIAUCSL")
  model_list[[i]] = model
  cat(i,"\n")
#}

forecasts = Reduce(cbind,lapply(model_list, function(x)head(x$forecast,nwindows)))

# Get the shapley values of all observations and combine them in a matrix,
# each column represents a regressor
shapley = unlist(lapply(model_list, function(x)x$shapley))
shapley = matrix(shapley, ncol = 10, byrow = TRUE)
avg_shapley = colMeans(shapley)

barplot(height=avg_shapley, names=c("RPI", "INDPRO", "UNRATE", "HOUST", "DPCERA", "M2SL", "TB3MS", "SP500", "Slope yield", "CPI lag"), 
        col="#69b3a2",
        horiz=T
)

Xout = unlist(lapply(model_list, function(x)x$Xout))
Xout = matrix(Xout, ncol = 10, byrow = TRUE)

for (i in 1:ncol(Xout)) {
  plot(Xout[,i], shapley[,i]) # Functional form  
}

# forecasts = accumulate_model(forecasts)

save(forecasts,file = paste("",model_name,".rda",sep = ""))

plot(tail(data[,"data.CPIAUCSL"],277),type = "l")
lines(forecasts[,1],col = 3)

forecast_data = data.frame(tail(dates,277), tail(data[,"data.CPIAUCSL"],277), forecasts[,1])
colnames(forecast_data) = c("date", "CPI", "forecast_CPI")
g = ggplot(forecast_data, aes(x=date)) + 
  geom_line(aes(y = CPI), color = "darkred") + 
  geom_line(aes(y = forecast_CPI), color="steelblue", linetype="twodash")
save(g, file = paste("",model_name,".png",sep = ""))