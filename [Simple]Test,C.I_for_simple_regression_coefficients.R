#[Simple regression] Test, C.I on Regression coefficient 
# https://jangpiano-science.tistory.com/107?category=875433

##------------------------code for test on coefficients(B0 and B1)

#a is the significance level 
Test_Regression_Coefficient = function(X, Y, a){
  
  print("H0 : B0 = 0,  H1 : B1=0 ")
  
  options("scipen" = 100)   #for the case having p-value as exponent
  
  test = summary(lm(Y ~X))

  p_value_B0 = summary(lm(Y~X))$coefficients[,4][1]
  p_value_B1 = summary(lm(Y~X))$coefficients[,4][2]
  
  if (p_value_B0<a){
    print("reject null hypothesis on B0. That is, B0 is not 0.")
  } else {
    print("do not reject null hypothesis on B0. That is, B0 is 0.")
  }
  
  if (p_value_B1<a){
    print("reject null hypothesis on B1. That is, B1 is not 0.")
  }else{
    print("do not reject null hypothesis on B1. That is, B1 is 0.")
  }
} 


#[STEPS]
#hypothesis of B0 
#H0 : B0 = 0
#H1 : B0 !=0

#hypothesis of B1 
#H0 : B1 = 0
#H1 : B1 !=0

test = lm(Y~X)
summary(test)
summary(lm(Y~X))$coefficients[,4]

p_value_B0 = summary(lm(Y~X))$coefficients[,4][1]
p_value_B1 = summary(lm(Y~X))$coefficients[,4][2]

#[example with the code]
set.seed(2)

X=sort(sample(x=0:10, size=15, replace=TRUE))
Y=sort(sample(x=0:10, size=15, replace=TRUE))

Test_Regression_Coefficient(X, Y, 0.05)

'''
[1] "H0 : B0 = 0,  H1 : B1=0 "
[1] "do not reject null hypothesis on B0. That is, B0 is 0."
[1] "reject null hypothesis on B1. That is, B1 is not 0." 
'''



#CONFIDENCE INTERVAL OF REGRESSION COEFFICIENT

confint(lm(Y~X1), level = 0.95) 

#[example]

X1 = c(3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 8, 9, 10)  
Y = c(1, 2, 3, 3, 6, 4, 5, 7, 3, 7, 6, 9, 10)

'''
>confint(lm(Y~X1), level = 0.95)
                 2.5 %   97.5 %
(Intercept) -4.3406016 1.234159
X1           0.6565352 1.525538
'''

##--------------------------code for finding confidence interval for B0 and B1

conf_int= function(X, Y, a, B){
  
  sample_mean = mean(X)
  Sxx = (sum((X - sample_mean)^2))
  t_critical = abs(qt(a/2, length(X)-2))
  estimate_var = sqrt(deviance(lm(Y~X))/(length(X)-2))
    
  if (B=="B0"){
    
    estimate_B0 = coef(lm(Y~X))[1]
    
    lower_bound = estimate_B0 - t_critical * estimate_var * sqrt(1/length(X) + sample_mean^2/Sxx)
    upper_bound = estimate_B0 + t_critical * estimate_var * sqrt(1/length(X) + sample_mean^2/Sxx)
    
    return(c(lower_bound, upper_bound)) 
  }
  else if (B=="B1"){
    
    estimate_B1 = coef(lm(Y~X))[2]
    
    lower_bound = estimate_B1 - t_critical * estimate_var * sqrt(1/Sxx)
    upper_bound = estimate_B1 + t_critical * estimate_var * sqrt(1/Sxx)
    
    return(c(lower_bound, upper_bound)) 
  }
}

#[example with the code]
'''
>conf_int(X1, Y, 0.05, "B0")
(Intercept) (Intercept) 
  -4.340602    1.234159 
>conf_int(X1, Y, 0.05, "B1")
        X         X 
0.6565352 1.5255376 
'''
