
# IC,IH VECM with 2 dummy variables
tidc <- rep(20:1, rep(240,20))
Nc <- ((length(ic00c)-length(tidc))%/%240)
Mc <- Nc%/%20
tidt <- rep(20:8, rep(240,13))
tidz <- rep(tidc, Mc)
tidc <- c(tidc,tidz)
tidc <- c(tidc,tidt)


cotest3 <- function(s,f,n=1){
  # 20121128~20150416
  #y1 <- 137761
  # 20150416~20150709
  y2 <- 13921
  #20150416~20151125
  y3 <- 36720
  #1.test cointegrate between spot and futures and get the residuals as
  #the error correction term
  lmresult <- lm(f ~ s + tidc)
  ect <- lmresult$residuals
  #2. use the ect(erro correction term) and lag term of spot and futures
  # to run regression about the diff(spot or futures)
  ##2.1 generate the dummy variable to indicator the launch of new futures
  ##and the shorting ban 
  
  dum1 <- c(rep(1, y2), rep(0, (y3-y2)))
  dum2 <- c(rep(0, y2) , rep(1, (y3-y2)))
  dum3 <- c(rep(1, y2), rep(0,y3-y2))
  dum4 <- c(rep(0, y2), rep(1,y3-y2))
  # regresssion on log(spot) ~ ect + dum1*ect + dum2*ect +diff(lag(s)) + diff(lag(f))
  s1 <- diff(s)[-c(1:2)]
  f1 <- diff(f)[-c(1:2)]
  s2 <- diff(s)[2:(length(s)-2)]
  f2 <- diff(f)[2:(length(s)-2)]
  s3 <- diff(s)[1:(length(s)-3)]
  f3 <- diff(f)[1:(length(s)-3)]
  cotest1 <- lm(s1 ~ dum1[-c(1:3)]:ect[-c(1:3)] + dum2[-c(1:3)]:ect[-c(1:3)] + 
                  s2 + s3 + f2 + f3)
  cotestspe <- lm(s1 ~ ect[-c(1:3)]  +  
                    s2 + s3 + f2 + f3)
  cotest3 <- lm(s1 ~ dum3[-c(1:3)]:ect[-c(1:3)] + dum4[-c(1:3)]:ect[-c(1:3)] + 
                  s2 + s3 + f2 + f3)
  cotest2 <- lm(f1 ~ dum1[-c(1:3)]:ect[-c(1:3)] + dum2[-c(1:3)]:ect[-c(1:3)] +
                  s2 + s3 + f2 + f3)
  e1 <- cotest1$residuals
  e2 <- cotest2$residuals
  v1 <- sd(e1)
  v2 <- sd(e2)
  rho <- cor(e1,e2)
  
  d11 <- cotest1$coefficients[6]
  names(d11) <- NULL
  d12 <- cotest1$coefficients[7]
  names(d12) <- NULL

  d21 <- cotest2$coefficients[6]
  names(d21) <- NULL
  d22 <- cotest2$coefficients[7]
  names(d22) <- NULL
  
  #GG measure
  g1 <- abs(d11)/(abs(d11)+abs(d21))
  g2 <- abs(d12)/(abs(d12)+abs(d22))
  
  
  #Hasbrouck measure
  H11 <- (-d21*v1 + d11*rho*v2)^2/(((-d21*v1 + d11*rho*v2)^2)+(d11*v2*sqrt(1-rho^2))^2)
  H12 <- (d11*v2*sqrt(1-rho^2))^2/(((-d21*v1 + d11*rho*v2)^2)+(d11*v2*sqrt(1-rho^2))^2)
  
  
  H21 <- (-d22*v1 + d12*rho*v2)^2/((((-d22*v1 + d12*rho*v2)^2)+(d12*v2*sqrt(1-rho^2))^2))
  H22 <- (d12*v2*sqrt(1-rho^2))^2/((((-d22*v1 + d12*rho*v2)^2)+(d12*v2*sqrt(1-rho^2))^2))
  
  
  
  # regression on log(futures) ~~ ect + dum1*ect + dum2*ect +diff(lag(s)) + diff(lag(f))
  if (n ==1){return(summary(cotest1))}
  if (n ==2){return(summary(cotest2))}
  #if (n ==3){return(c((H11+H12)/2,(H21+H22)/2))}
  if (n ==3){return(c(H11,H12,H21,H22))}
  if (n ==4){return(c(g1,g2))}
  #if (n ==4){return(summary(cotest3))}
}


  cotest3(zz500c,ic00c,1)
  cotest3(zz500c,ic00c,2)
  cotest3(zz500c,ic00c,3)
  cotest3(zz500c,ic00c,4) 
  
if (1<0){ 
  
  cotest3(sz50c,ih00c,1)
  cotest3(sz50c,ih00c,2)
  cotest3(sz50c,ih00c,3)
  cotest3(sz50c,ih00c,4)
}


