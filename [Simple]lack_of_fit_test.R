#[Simple regression] lack of fit test

#H0 : the model fits well
#H1 : the model does not fit well

lack_of_fit_test = function(X, Y, a){
  print("HO: the model is the good fit model   H1: the model is not a good fit model")
  t = lm(Y~X)
  
  estimator_B0 = t$coefficients[[1]]
  estimator_B1 = t$coefficients[[2]]
  
  SSE = anova(t)[2,2]

  print(paste("estimator for B0 : ", estimator_B0 , "estimator for B1 : ", estimator_B1))
  print(paste("estimator for Yi : ", estimator_B0,"+",estimator_B1,"x" ))
  print(paste("SSE : " , SSE))
  
  fit_test = lm(Y ~ factor (X))
  
  SSpe = anova(t, fit_test)[2,2]
  SSlof = SSE - SSpe

  print(paste("SSpe: ", SSpe ))
  print(paste("SSlof : ", SSlof ))
  
  estimator_var = anova(t ,fit_test)[2,2]/anova(t, fit_test)[2,1]
  print(paste("estimator for variance : ", estimator_var ))
  
  p_value = anova(t, fit_test)[[6]][2]
  
  if (p_value >= a){
    print("the model is a good fit model")
    
  }else{
    print("the model is not a good fit model")
  }
}

#[Steps]
lm(Y~X)
anova(lm(Y~X))   #--> SSE (needed to find SSlof)

lm(Y~factor(X))
anova(lm(Y~X), lm(Y~factor(X)))   #--> SSpe, estimate for variance, p-value



#[example with the code]

X = c(1,1, 3,3,3, 4, 5,5, 7,7,7,7, 8)
Y = c(2,4, 2,5,3, 6, 8,7, 11,13,10,10, 15)

lack_of_fit_test(X, Y, 0.05)
