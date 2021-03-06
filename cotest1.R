#using model.matrix to generate the dummy variables D1 and D2 
# D1=0 between 20150416 and 20150704
#D1=1 between 20150705 and 20151124

cotest1 <- function(s,f,n=1){
  # 20121128~20150416
  #y1 <- 137761
  # 20150416~20150709
  y2 <- 13921
  #20150709~20151125
  y3 <- 36720
  #1.test cointegrate between spot and futures and get the residuals as
  #the error correction term
  lmresult <- lm(s ~ f)
  ect <- lmresult$residuals
  #2. use the ect(erro correction term) and lag term of spot and futures
  # to run regression about the diff(spot or futures)
  ##2.1 generate the dummy variable to indicator the launch of new futures
  ##and the shorting ban 
  
  dum1 <- c(rep(0, y2), rep(1, (y3-y2)))
  #dum2 <- c(rep(0, y2) , rep(1, (y3-y2)))
  # regresssion on log(spot) ~ ect + dum1*ect + dum2*ect +diff(lag(s)) + diff(lag(f))
  s1 <- diff(s)[-c(1:2)]
  f1 <- diff(f)[-c(1:2)]
  s2 <- diff(s)[2:(length(s)-2)]
  f2 <- diff(f)[2:(length(s)-2)]
  s3 <- diff(s)[1:(length(s)-3)]
  f3 <- diff(f)[1:(length(s)-3)]
  cotest1 <- lm(s1 ~ ect[-c(1:3)] + dum1[-c(1:3)]:ect[-c(1:3)] +  
                  s2 + s3 + f2 + f3)
  cotest2 <- lm(f1 ~ ect[-c(1:3)] + dum1[-c(1:3)]:ect[-c(1:3)] +  
                  s2 + s3 + f2 + f3)
  # regression on log(futures) ~~ ect + dum1*ect + dum2*ect +diff(lag(s)) + diff(lag(f))
  if (n ==1){return(summary(cotest1))}
  if (n ==2){return(summary(cotest2))}
}

cotest1(sz50c,ih00c)
cotest1(sz50c,ih00c,2)
