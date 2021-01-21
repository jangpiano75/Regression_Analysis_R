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

confint(lm(Y~X1)) 

#[example]

X1 = c(3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 8, 9, 10)  
Y = c(1, 2, 3, 3, 6, 4, 5, 7, 3, 7, 6, 9, 10)

confint(lm(Y~X1))


##--------------------------code for finding confidence interval for B0 and B1
conf_int= function(X, Y, a, B){
  
  sample_mean = mean(X)
  Sxx = (sum((X - sample_mean)^2))
  t_critical = abs(qt(a/2, length(X)-2))
  
  if (B=="B0"){
    
    estimate_B0 = summary( lm(Y~X) )$coefficients[1]
    s_e_B0 = summary( lm(Y~X) )$coefficients[3]
    
    lower_bound = estimate_B0 - t_critical * (s_e_B0) * (1/length(X) + sample_mean^2/Sxx)^0.5
    upper_bound = estimate_B0 + t_critical * (s_e_B0) * (1/length(X) + sample_mean^2/Sxx)^0.5
    
    return(c(lower_bound, upper_bound)) 
  }
  else if (B=="B1"){
    
    estimate_B1 = summary( lm(Y~X) )$coefficient[2]
    s_e_B1 = summary( lm(Y~X) )$coefficients[4]
  
    lower_bound = estimate_B1 - t_critical * (s_e_B1) * (Sxx)^-0.5
    upper_bound = estimate_B1 + t_critical * (s_e_B1) * (Sxx)^-0.5
    
    return(c(lower_bound, upper_bound)) 
  }
}

#[example with the code]

conf_int (X, Y, 0.05, "B0") 
#-0.1631919  0.5813159

conf_int (X, Y, 0.05, "B1") 
#1.015865 1.039779


