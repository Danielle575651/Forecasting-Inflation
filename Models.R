dataprep = function(ind,df,variable, horizon, univar = FALSE)
{
  df=df[ind,]
  y=df[,variable]
  
  if (univar==FALSE){
      x = df
      x = x[, colnames(x) != variable] # Exclude CPI from potential predictors
      max_lag = 1
    } else{
      x = as.matrix(df[,variable])
      x = head(x,-1)
      x = na.omit(x)
      max_lag = 12
  }
  
  # Each row of the resulting matrix consists of x[t], x[t-1],..., x[t-dimension+1]
  # 4 indicates: include the regressors at time t, t-1, t-2 and t-3
  # We only indicate the regressors at time t, but we could extend it here
   X=embed(as.matrix(x), max_lag)
   
   # In case not arp model, we include lag of CPI as regressor
  if (univar == FALSE) {
    y_lag = head(y, -1)
    X = tail(X, (nrow(X) - 1))
    X = cbind(X, y_lag)
  }
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]
  Xout=X[nrow(X),]
  Xout=t(as.vector(Xout))
  yin=tail(y,nrow(Xin))
  
  return(list(Xin = Xin, Xout = Xout, yin = yin))
  
}





# Benchmark models
# 1: Random walk

# run getRW.R

# 2: ARX(p) model

runarx=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon, univar = TRUE)
  ar_terms_in = prep_data$Xin
  yin = prep_data$yin
  ar_terms_out = prep_data$Xout
  
  # First we determine the optimal lag order
  bb=Inf
  best = 1
  for(i in seq(1,ncol(ar_terms_in),1)){
    m=lm(yin~ar_terms_in[,1:i])
    crit=BIC(m)
    if(crit<bb){
      bb=crit
      modelest=m
      best = i
    }
  }
  
  # Second, include regressor terms in regression
  prep_data = dataprep(ind,df,variable,horizon, univar = FALSE)
  x_terms_in = prep_data$Xin
  x_terms_out = prep_data$Xout
  
  ar_n_obs = nrow(ar_terms_in)
  x_n_obs = nrow(x_terms_in)
  dif_obs = x_n_obs - ar_n_obs
  x_terms_in = tail(x_terms_in, x_n_obs - dif_obs)
  
  modelest = lm(yin~ar_terms_in[,1:best]+x_terms_in)
  
  coef=coef(modelest)
  coef[is.na(coef)] = 0
  
  forecast=c(1,ar_terms_out[,1:best], x_terms_out)%*%coef
  
  return(list(forecast=forecast))
}

# Also interesting to compare with "normal" AR(p) model based on BIC:
runar=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon, univar = TRUE)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  bb=Inf
  best = 1
  for(i in seq(1,ncol(Xin),1)){
    m=lm(yin~Xin[,1:i])
    crit=BIC(m)
    if(crit<bb){
      bb=crit
      modelest=m
      best = i
    }
  }

  coef=coef(modelest)
  coef[is.na(coef)] = 0
  forecast=c(1,Xout[,1:best])%*%coef
  
  return(list(forecast=forecast))
}

# Machine learning models
# Linear Machine Learning models:

# 1: Ridge regression: alpha = 0 is the ridge penalty
runridge=function(ind,df,variable,horizon, alpha = 0, alpha2 = 0, adaptive = FALSE){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest = ic.glmnet(Xin,yin,alpha = alpha)
  
  # Adaptive ridge (Could be an extension)
  if (adaptive==TRUE){
    cridge = coef(modelest)
    penalty = (abs(cridge[-1])+1/sqrt(length(yin)))^(-1)
    modelest = ic.glmnet(Xin,yin, penalty.factor = penalty, alpha = alpha2)
  }
  
  forecast=predict(modelest,Xout)
  
  if (horizon == 3) {
    shapley_values = ic.getShapleyValues(modelest, Xout, Xin, yin)
  }
  
  ### outputs ###
  coeflvl=coef(modelest)[-1]
  coefpar=coeflvl*apply(Xin,2,sd)
  lambda=modelest$lambda
  outputs=list(coeflvl=coeflvl,coefpar=coefpar,lambda=lambda)
  
  return(list(forecast=forecast, outputs=outputs, shapley_values = shapley_values, Xout = Xout))
}

# 2: Adaptive Elastic Net: alpha = 0.5
runadaelasticnet=function(ind,df,variable,horizon, alpha = 0.5, alpha2 = 0.5, adaptive = TRUE){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest = ic.glmnet(Xin,yin,alpha = alpha)
  
  # Adaptive elastic net
  if (adaptive==TRUE){
    celasticnet = coef(modelest)
    penalty = (abs(celasticnet[-1])+1/sqrt(length(yin)))^(-1)
    modelest = ic.glmnet(Xin,yin, penalty.factor = penalty, alpha = alpha2)
  }
  
  forecast=predict(modelest,Xout)
  if (horizon == 3) {
    shapley_values = ic.getShapleyValues(modelest, Xout, Xin)
  }
  
  ### outputs ###
  coeflvl=coef(modelest)[-1]
  coefpar=coeflvl*apply(Xin,2,sd)
  lambda=modelest$lambda
  outputs=list(coeflvl=coeflvl,coefpar=coefpar,lambda=lambda)
  
  return(list(forecast=forecast, outputs=outputs, shapley_values = shapley_values, Xout = Xout))
}

# Nonlinear ML models
# 1: Random forest
runrf=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest=randomForest::randomForest(Xin,yin, importance = TRUE)
  forecast=predict(modelest,Xout)
  
  ## outputs
  importance = randomForest::importance(modelest)
  outputs = list(importance = importance)
  
  return(list(forecast=forecast, outputs = outputs))
}


# 2: SVR
runsvr =function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest=e1071::svm(Xin,yin, importance = TRUE)
  forecast=predict(modelest,Xout)
  
  return(list(forecast=forecast))
}


# 3: LLF
runllf=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest=grf::ll_regression_forest(Xin,yin, num.trees = 10)
  forecast=predict(modelest,Xout)
  
  return(list(forecast=forecast))
}



# 4: MOTR-BART
runmotrbart=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  modelest=MOTRbart::motr_bart(Xin,yin, ntrees = 10, nburn = 10, npost = 10)
  forecast = predict_motr_bart(modelest, Xout, 'mean')

  return(list(forecast=forecast))
}

# 5: MRF
runmrf = function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  yout = prep_data$yout
  data = cbind(yin, yout, Xin, Xout)
  
  
  return(list(forecast=forecast))
}


accumulate_model = function(forecasts){
  
  acc3 = c(rep(NA,2),sapply(1:(nrow(forecasts)-2), function(x){
    prod(1+diag(forecasts[x:(x+2),1:3]))-1
  })) 
  acc6 = c(rep(NA,5),sapply(1:(nrow(forecasts)-5), function(x){
    prod(1+diag(forecasts[x:(x+5),1:6]))-1
  }))
  acc12 = c(rep(NA,11),sapply(1:(nrow(forecasts)-11), function(x){
    prod(1+diag(forecasts[x:(x+11),1:12]))-1
  }))
  
  forecasts = cbind(forecasts,acc3,acc6,acc12)
  colnames(forecasts) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")
  
  return(forecasts)
  
}

ic.glmnet = function (x, y, crit = c("bic", "aic", "aicc", 
                                     "hqc"), alpha = 1, ...) 
{
  if (is.matrix(x) == FALSE) {
    x = as.matrix(x)
  }
  if (is.vector(y) == FALSE) {
    y = as.vector(y)
  }
  crit = match.arg(crit)
  n = length(y)
  model = glmnet(x = x, y = y, alpha = alpha,...)
  coef = coef(model)
  lambda = model$lambda
  df = model$df
  yhat = cbind(1, x) %*% coef
  residuals = (y - yhat)
  mse = colMeans(residuals^2)
  sse = colSums(residuals^2)
  nvar = df + 1
  bic = n * log(mse) + nvar * log(n)
  aic = n * log(mse) + 2 * nvar
  aicc = aic + (2 * nvar * (nvar + 1))/(n - nvar - 1)
  hqc = n * log(mse) + 2 * nvar * log(log(n))
  sst = (n - 1) * var(y)
  r2 = 1 - (sse/sst)
  adjr2 = (1 - (1 - r2) * (n - 1)/(nrow(x) - nvar - 1))
  crit = switch(crit, bic = bic, aic = aic, aicc = aicc, hqc = hqc)
  selected = best.model = which(crit == min(crit))
  ic = c(bic = bic[selected], aic = aic[selected], aicc = aicc[selected], 
         hqc = hqc[selected])
  result = list(coefficients = coef[, selected], ic = ic, lambda = lambda[selected], 
                nvar = nvar[selected], glmnet = model, residuals = residuals[, 
                                                                             selected], fitted.values = yhat[, selected], ic.range = crit, 
                df = df, call = match.call())
  class(result) = "ic.glmnet"
  return(result)
}

ic.getShapleyValues = function(modelest, Xout, Xin) {
  colnames(Xout) =  c("RPI", "INDPRO", "UNRATE", "HOUST", "DPCERA", "M2SL", "TB3MS", "SP500", "Slope yield", "CPI lag")
  colnames(Xin) = c("RPI", "INDPRO", "UNRATE", "HOUST", "DPCERA", "M2SL", "TB3MS", "SP500", "Slope yield","CPI lag")
  
  system.time(
    shap_lm <- kernelshap(modelest, Xout, Xin)
  )
  shapley_values = shap_lm$S # 1 x 10 vector
  return(shap_lm$S)
}
