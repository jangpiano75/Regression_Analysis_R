###code for adjusted multiple linear regression with 2 variables (변수가 두개인 조정된 다중 선형 회귀 모형) 
multiple_regre_v_2 = function(X1, X2, Y, x1, x2){
  
  options("scipen" = 100)  #지수로 표기될 경우를 대비해 (지수표기 -->숫자표기)
  
  y_1<-lm(Y~X1)
  e1 = resid(y_1) 
  
  x_1<-lm(X2~X1, data = data_2)
  e2 = resid(x_1)
  
  estimate_B2 = lm(e1 ~e2)$coefficients[2]
  
  y_2<-lm(Y~X2, data = data_2)
  e11 = resid(y_2)
  
  x_2<-lm(X1~X2)
  e22 = resid(x_2)
  
  estimate_B1 = lm(e11 ~ e22)$coefficients[2]
  
  estimate_B0 = mean(Y) - estimate_B1*mean(X1) - estimate_B2*mean(X2)
  
  y_estimate = estimate_B0 + estimate_B1*x1 + estimate_B2*x2
  y_estimate
}

#[steps with an example]

#Multiple regression_with 2 variables X1, X2

X1<-c(3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 8, 9, 10)
X2<-c(2, 4, 6, 5, 8, 4, 6, 9, 7, 9, 7, 9, 10)
Y<-c(1, 2, 3, 3, 6, 4, 5, 7, 3, 7, 6, 9, 10)

plot(X1, Y)
lm(Y~X1)     

##------------ estimator of adjusted B2 by X1 (X1에 대해 조정된 회귀계수 B2의 추정값 )------------

data_2<-data.frame(Y, X1, X2)
y_1<-lm(Y~X1, data = data_2 )
y_1

#residuals after regression fit of Y with respect to X1 (Y의 X1에 관한 회귀적합후 나오는 잔차) 
e1 = resid(y_1)   #residuals 


x_1<-lm(X2~X1, data = data_2)
x_1

#residuals after regression fit of X2 with respect to X1 (X2의 X1에 관한 회귀적합후 나오는 잔차)
e2 = resid(x_1)

# Scatter plot with X-axis:e2(X2|X1), Y-axis:e1(Y|X1) 
#(x축을 e2(X2|X1), y축을 e1(Y|X1)로 하는 산점도)
plot(e2, e1, xlim=c(-2, 3), ylim=c(-3, 3))

#regression fit of e1 with respect to e2
options("scipen" = 100)   #exponential notation to numerical notation 

lm(e1 ~ e2)
estimate_B2 = lm(e1 ~e2)$coefficients[2]

##------------ estimate of adjusted B1 by X2 (X2에 대해 조정된 회귀계수 B1의 추정값 )------------

#residuals after regression fit of Y with respect to X2 (Y의 X2에 관한 회귀적합후 나오는 잔차)
y_2<-lm(Y~X2, data = data_2)
y_2
e11 = resid(y_2)

#residuals after regression fit of X1 with respect to X2 (X1의 X2에 관한 회귀적합후 나오는 잔차)
x_2<-lm(X1~X2, data = data_2)
x_2

e22 = resid(x_2)

#regression fit of e11 with respect to e22

lm(e11 ~ e22)
estimate_B1 = lm(e11 ~ e22)$coefficients[2]

##------------ estimate of adjusted B0 (B0의 추정값 )------------
# B0의 추정값 = y의 표본평균 - B1의 추정값 *x1의 표본평균 - B2의 추정값 *x2의 표본평균 
estimate_B0 = mean(Y) - estimate_B1*mean(X1) - estimate_B2*mean(X2)
estimate_B0 


##---------------ultimate estimated regression equation ------------
#y_estimate = estimate_B0 + estimate_B1*x1 + estimate_B2*x2
