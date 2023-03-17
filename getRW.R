
#### gets out of sample y and computes random walk forecasts ###
library(roll)
library(dplyr)
load("dataset.Rda")
dates = dataset$date
data = dataset%>%select(-date)%>%as.matrix()
rownames(dataset) = as.character(dates)

nwindows = 277

y = data[,"data.CPIAUCSL"]
y = cbind(y,roll_prod(1+y,3)-1,roll_prod(1+y,6)-1,roll_prod(1+y,12)-1)
yout = tail(y,nwindows)

rw = matrix(NA,nwindows,12)
for(i in 1:12){
  aux=data[(nrow(data)-nwindows-i+1):(nrow(data)-i),"data.CPIAUCSL"]
  rw[,i]=aux;
}

rw3 = tail(embed(y[,2],4)[,4],nwindows)
rw6 = tail(embed(y[,3],7)[,7],nwindows)
rw12 = tail(embed(y[,4],13)[,13],nwindows)
rw = cbind(rw,rw3,rw6,rw12)
colnames(rw) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")

save(yout,file = "yout.rda")
save(rw,file = "rw.rda")

rw_data = data.frame(tail(dates,277), tail(data[,"data.CPIAUCSL"],277), rw[,1])
colnames(rw_data) = c("date", "CPI", "forecast_CPI")
ggplot(forecast_data, aes(x=date)) + 
  geom_line(aes(y = CPI), color = "darkred") + 
  geom_line(aes(y = forecast_CPI), color="steelblue", linetype="twodash")


