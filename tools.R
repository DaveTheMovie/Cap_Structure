# Model Tools ####


AdjRSq = function(yhat,y,n,p){
  r2 = cor(yhat,y)^2
  error = 1 - r2
  inflatedError = error * (n-1) / (n-p)
  adjRSq = 1 - inflatedError
  return(adjRSq)
}


PlotFit = function(formulaStr,d_iis,d_oos,targetName){
  
  myformula = as.formula(formulaStr)
  fit = lm(formula = myformula,data = d_iis,na.action = na.exclude)
  
  yhat_iis = predict(fit,newdata = d_iis)
  yhat_oos = predict(fit,newdata = d_oos)
  y_oos = d_oos$EL
  
  AdjRsq_iis = summary(fit)$adj.r.squared
  AdjRsq_oos = AdjRSq(yhat = predict(fit,d_oos),
                      y = y_oos,
                      n = dim(d_oos)[1],
                      p = length(coefficients(fit)))
  
  plot(y_oos,yhat_oos, ylim = c(min(y_oos),max(y_oos)))
  abline(a = 0,b = 1,col = 'green')
  
  title1 = paste('AdjRSq ISS: ',round(AdjRsq_iis,2))
  title2 = paste('AdjRSq OSS: ',round(AdjRsq_oos,2))
  title3 = formula(fit)
  
  title(paste0(formulaStr,'\n',title1,'\n',title2))
  
  print(summary(fit))
}
