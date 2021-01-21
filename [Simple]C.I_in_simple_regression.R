#[Simple regression] 

##---------------------confidence interval for expected value of Y when X is given

conf_int_exp_Y = function(X,Y,x,a){
  estimated_B0 = lm(Y~X)$coefficients[[1]]
  estimated_B1 = lm(Y~X)$coefficients[[2]]
  
  
  y_estimate = estimated_B1*x + estimated_B0
  t_critical = abs(qt(a/2, length(X)-2))
  MSE = anova(lm(Y~X))[2,3]
  sample_mean = mean(X)
  Sxx = (sum((X - sample_mean)^2))
  
  lower_bound = y_estimate - t_critical * sqrt(MSE) * (1/length(X)+ (x-sample_mean)^2/Sxx)^0.5
  upper_bound = y_estimate + t_critical * sqrt(MSE) * (1/length(X)+ (x-sample_mean)^2/Sxx)^0.5
  return(c(lower_bound, upper_bound))
}

#[example with the code]
X = c(1,1, 3,3,3, 4, 5,5, 7,7,7,7, 8)
Y = c(2,4, 2,5,3, 6, 8,7, 11,13,10,10, 15)

conf_int_exp_Y(X, Y, 6, 0.05)



##---------------------confidence interval for Y when X is given

conf_int_Y = function(X,Y,x,a){
  estimated_B0 = lm(Y~X)$coefficients[[1]]
  estimated_B1 = lm(Y~X)$coefficients[[2]]
  
  y_estimate = estimated_B1*x + estimated_B0
  t_critical = abs(qt(a/2, length(X)-2))
  MSE = anova(lm(Y~X))[2,3]
  sample_mean = mean(X)
  Sxx = (sum((X - sample_mean)^2))
  
  lower_bound = y_estimate - t_critical * sqrt(MSE) * (1 + 1/length(X)+ (x-sample_mean)^2/Sxx)^0.5
  upper_bound = y_estimate + t_critical * sqrt(MSE) * (1+ 1/length(X)+ (x-sample_mean)^2/Sxx)^0.5
  return(c(lower_bound, upper_bound))
}


#[example with the code]
conf_int_Y(X, Y, 6, 0.05)
#[simpler code in r]

predict(lm(Y~X), newdata = data.frame(X= ), interval =  c("none", "confidence", "prediction"))

#[EX]
X1 = c(3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 8, 9, 10) 
Y = c(1, 2, 3, 3, 6, 4, 5, 7, 3, 7, 6, 9, 10)

predict(lm(Y~X1), newdata = data.frmae(X1 = 3), interval = "confidence")


##---------------------confidence interval for variance

conf_int_var = function(X,Y,a){
  
  #주의해야할 구간: chisquare(a/2, n-2) = qchisq(1-a/2, length(X)-2)
  chi_square_critical_upper = qchisq(1-a/2, length(X)-2)   
  
  #주의해야할 구간: chisquare(1-a/2, n-2) = qchisq(a/2, length(X)-2)
  chi_square_critical_lower = qchisq(a/2, length(X)-2)        
  
  MSE = anova(lm(Y~X))[3][[1]][2]
  
  lower_bound = (length(X)-2)*MSE/chi_square_critical_upper
  upper_bound = (length(X)-2)*MSE/chi_square_critical_lower
  return(c(lower_bound, upper_bound))
}

#[example with the code]
conf_int_var(X, Y, 0.05)
