#https://jangpiano-science.tistory.com/110?category=875432

#
#code for adjusted multiple linear regression with 2 variables 
multiple_regre_v_2 = function(X1, X2, Y, x1, x2){
  
  options("scipen" = 100)  #지수로 표기될 경우를 대비해 (지수표기 -->숫자표기)
  
  y_1<-lm(Y~X1)
  e1 = resid(y_1) 
  
  x_1<-lm(X2~X1)
  e2 = resid(x_1)
  
  estimate_B2 = lm(e1 ~e2)$coefficients[2]
  
  y_2<-lm(Y~X2)
  e11 = resid(y_2)
  
  x_2<-lm(X1~X2)
  e22 = resid(x_2)
  
  estimate_B1 = lm(e11 ~ e22)$coefficients[2]
  
  estimate_B0 = mean(Y) - estimate_B1*mean(X1) - estimate_B2*mean(X2)
  
  y_estimate = estimate_B0 + estimate_B1*x1 + estimate_B2*x2
  y_estimate
}

#[STEPS]

X1<-c(3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 8, 9, 10)
X2<-c(2, 4, 6, 5, 8, 4, 6, 9, 7, 9, 7, 9, 10)
Y<-c(1, 2, 3, 3, 6, 4, 5, 7, 3, 7, 6, 9, 10)

e1 = resid(lm(Y~X1))
e2 = resid(lm(X2~X1))
estimate_B1 = lm(e1 ~e2)$coefficients[2]

e11 = resid(lm(Y~X2))
e22 = resid(lm(X1~X2))
estimate_B2 = lm(e11~e22)$coefficients[2]


estimate_B0 = mean(Y) - estimate_B1*mean(X1) - estimate_B2*mean(X2)


#[EXAMPLE]
multiple_regre_v_2(X1, X2, Y, 3, 2)

#
#code for adjusted multiple linear regression with more than 2 variables (generalized code) 

estimate_B = list()

adjusted_multi_regre = function(data_frame, Y){
  
  estimate_B0 = mean(Y)
  
  for (i in 2:length(data_frame)){
    
    e1 = resid(lm(Y~., data = data_frame[-i]))
    e2 = resid(lm(data_frame[[i]]~., data = data_frame[c(-1, -i)]))
    
    estimate_B = c(estimate_B, lm(e1~e2)$coefficients[2])
    print(paste("estimate of B",i-1,"is",estimate_B[[i-1]], collapse = " "))
    
    estimate_B0 = estimate_B0 - estimate_B[[i-1]]*mean(data_frame[[i]])
  }
  print(paste("estimate of B 0 is", estimate_B0, collapse = " "))
}


#[STEPS]
X1 = c(3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 8, 9, 10)  #age of children
X2 = c(2, 4, 6, 5, 8, 4, 6, 9, 7, 9, 7, 9, 10)  #income level of parents
X3 = c(1, 5, 2, 6, 7, 2, 3, 6, 9, 10, 9, 4, 7) #children's academic interest
Y = c(1, 2, 3, 3, 6, 4, 5, 7, 3, 7, 6, 9, 10)  #expenditure in child-education

data_2 = data.frame(Y, X1, X2, X3)
data_2

e1 = resid(lm(Y~X2+X3))    #e(Y|X2,X3)
e2 = resid(lm(X1~X2+X3))    #e(X1|X2,X3)
estimate_B1 = lm(e1 ~e2)$coefficients[2]

e21 = resid(lm(Y~X1+X3))    #e(Y|X1,X3)
e22 = resid(lm(X2~X1+X3))    #e(X2|X1,X3)
estimate_B2 = lm(e21 ~ e22)$coefficients[2]

e31 = resid(lm(Y~X1+X2))     #e(Y|X1,X2)
e32 = resid(lm(X3~X1+X2))     #e(X3|X1,X2)
estimate_B3 = lm(e31 ~ e32)$coefficients[2]

estimate_B0 = mean(Y) - estimate_B1*mean(X1) - estimate_B2*mean(X2) - estimate_B3*mean(X3)

#[EXAMPLE]

adjusted_multi_regre(data_2, Y)
