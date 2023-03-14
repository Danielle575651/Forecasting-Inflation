dataprep = function(ind,df,variable,horizon,add_dummy = TRUE, univar = FALSE, factonly = FALSE, nofact = FALSE)
{
  df=df[ind,]
  y=df[,variable]
  
  if(nofact==TRUE){
    if(univar==FALSE){
      x=df
    }else{
      x = as.matrix(df[,variable])
    }
  }else{
    if(univar==FALSE){
      factors=princomp(scale(df))$scores[,1:4]
      if(factonly == TRUE){
        x = cbind(df[,variable],factors)
      }else{
        x=cbind(df,factors)
      }
    }else{
      x = as.matrix(df[,variable])
    }
  }
  
  # Each row of the resulting matrix consists of x[t], x[t-1],..., x[t-dimension+1]
  # Here they include the regressors at time t, t-1, t-2 and t-3
  X=embed(as.matrix(x),4)
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]
  Xout=X[nrow(X),]
  Xout=t(as.vector(Xout))
  yin=tail(y,nrow(Xin))
  
  if("2008-11-01" %in% names(yin)){
    
    dummy=rep(0,length(yin))
    intervention=which(names(yin)=="2008-11-01")
    dummy[intervention]=1
    if(add_dummy == TRUE){
      Xin=cbind(Xin,dummy)
      Xout=cbind(Xout,0)
    }
    
  }else{
    dummy = rep(0,length(yin))
    if(add_dummy == TRUE){
      Xin=cbind(Xin,dummy)
      Xout=cbind(Xout,0)
    }
  }
  
  return(list(dummy = dummy, Xin = Xin, Xout = Xout, yin = yin))
  
}





# Benchmark models
# 1: Random walk



# 2: ARX(p) model

runarx=function(ind,df,variable,horizon, type = "bic"){
  prep_data = dataprep(ind,df,variable,horizon, univar = TRUE, add_dummy = FALSE)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  dummy = prep_data$dummy
  
  
  if(type=="bic"){
    bb=Inf
    best = 1
    for(i in seq(1,ncol(Xin),1)){
      m=lm(yin~Xin[,1:i]+dummy)
      crit=BIC(m)
      if(crit<bb){
        bb=crit
        modelest=m
        best = i
      }
    }
  }
  
  # When we selected the optimal lag order, we can run the ARX model
  ar_terms_in = Xin[,1:best]
  ar_terms_out = Xout[,1:best]
  
  # Collect the regressor values (X part of ARX)
  x_terms = dataprep(ind, data, variable, horizon, univar = FALSE, add_dummy = FALSE)
  x_terms_in = x_terms$Xin
  x_terms_out = x_terms$Xout
  
  
  coef_arx = coef(lm(yin~ x_terms_in + ar_terms_in + dummy))
  coef_arx[is.na(coef_arx)] = 0
  forecast = c(1, x_terms_out, ar_terms_out, 0)%*%coef_arx
  
  return(list(forecast = forecast))
}

# Also interesting to compare with "normal" AR(p) model based on BIC:
runar=function(ind,df,variable,horizon){
  prep_data = dataprep(ind,df,variable,horizon, univar = TRUE, add_dummy = FALSE)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  dummy = prep_data$dummy
  
  
  bb=Inf
  best = 1
  for(i in seq(1,ncol(Xin),1)){
    m=lm(yin~Xin[,1:i]+dummy)
    crit=BIC(m)
    if(crit<bb){
      bb=crit
      modelest=m
      best = i
    }
  }

  coef=coef(modelest)
  coef[is.na(coef)] = 0
  forecast=c(1,Xout[,1:best],0)%*%coef
  
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
  
  ### outputs ###
  coeflvl=coef(modelest)[-1]
  coefpar=coeflvl*apply(Xin,2,sd)
  lambda=modelest$lambda
  outputs=list(coeflvl=coeflvl,coefpar=coefpar,lambda=lambda)
  
  return(list(forecast=forecast, outputs=outputs))
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
  
  ### outputs ###
  coeflvl=coef(modelest)[-1]
  coefpar=coeflvl*apply(Xin,2,sd)
  lambda=modelest$lambda
  outputs=list(coeflvl=coeflvl,coefpar=coefpar,lambda=lambda)
  
  return(list(forecast=forecast, outputs=outputs))
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

# 4: MOTR-BART





