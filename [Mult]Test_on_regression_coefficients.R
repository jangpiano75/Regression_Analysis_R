#[Multiple regression] Test on Regression coefficient 

#1. 
#H0 : B1 = B2 =... = Bp 0
#H1 : at least a Bi !=0 (i = 1,2, ..., p)

#data_frame should be the data frame including Y and Xs
Test_Regression_Coefficients = function(data_frame, Y, a){
  
  print("H0 : B0 = B1 = B2 = ...= Bp = 0,  H1 : at least a Bi!=0 ")
  
  options("scipen" = 100)   #for the case having p-value as exponent
  print(summary(lm(Y ~., data = data_frame)))
  
  f = summary(lm(Y ~., data = data_frame))$fstatistic
  f_statistics = summary(lm(Y~., data = data_frame))$fstatistic[1]
  df1 = summary(lm(Y ~., data = data_frame))$fstatistic[2]
  df2 = summary(lm(Y ~., data = data_frame))$fstatistic[3]
  print(paste("fstatistics:", f_statistics, "df1:", df1, "df2:", df2, collapse = " "))
  
  p_value = pf(f[1],f[2],f[3], lower.tail=F)
  print(paste("p_value is", p_value, collapse = " "))
  if (p_value<a){
    print("reject null hypothesis, accept alternative hypothesive")
    print("at least a Bi!=0")
  }else{
    print("do not reject null hypothesis")
    print("B0 = B1 = B2 = ...= Bp = 0")
  }
} 


#2.

#H0 : Bi = 0
#H1 : Bi != 0

p_value = list()
Test_Regression_Coefficient = function(data_frame, Y, a){
  
  print("H0 : Bi = 0,  H1 : Bi!=0 ")
  
  options("scipen" = 100)   #for the case having p-value as exponent
  test = summary(lm(Y ~., data = data_frame))
  
  for (i in 2:length(data_1)){
    
    p_value = c(p_value, test$coefficients[,4][i])
    print (paste ("p_value for B", i-1, "is", p_value[i-1], collapse = " " ))
    
    if (p_value[[i-1]]<a){
      print(paste("B", i-1, "is not zero", collapse = " "))
    }
    else{
      print(paste("B",i-1, "should be zero", collapse = " "))
    }
  }
} 


  