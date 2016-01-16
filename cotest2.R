
#specially for IF with 3 dummy variables 


tid <- rep(18:1, rep(240,18))
N <- ((length(if00c)-length(tid))%/%240)
M <- N%/%20
tid0 <- rep(20:1, rep(240,20))
tidt <- rep(20:12, rep(240,9))
tidz <- rep(tid0, M)
tid <- c(tid,tidz)
tid <- c(tid,tidt)

cotest2 <- function(s,f,n=1){
  # 20121128~20150416
  y1 <- 137761
  # 20121128~20150709
  y2 <- 151681
  #20150709~20151125
  y3 <- 174480
  #1.test cointegrate between spot and futures and get the residuals as
  #the error correction term
  lmresult <- lm(f ~ s)
  #lmresult <- lm(f ~ s)
  ect <- lmresult$residuals
  #2. use the ect(erro correction term) and lag term of spot and futures
  # to run regression about the diff(spot or futures)
  ##2.1 generate the dummy variable to indicator the launch of new futures
  ##and the shorting ban 
  
  dum1 <- c(rep(1, y1),  rep(0, (y3-y1)))
  dum2 <- c(rep(0, y1), rep(1, (y2-y1)) , rep(0, (y3-y2)))
  dum3 <- c(rep(0, y2), rep(1,(y3-y2)))
  dum4 <- c(rep(1,y2),rep(0,(y3-y2)))
  dum5 <- c(rep(0,y2),rep(1,(y3-y2)))
  # regresssion on log(spot) ~ ect + dum1*ect + dum2*ect +diff(lag(s)) + diff(lag(f))
  s1 <- diff(s)[-c(1:2)]
  f1 <- diff(f)[-c(1:2)]
  s2 <- diff(s)[2:(length(s)-2)]
  f2 <- diff(f)[2:(length(s)-2)]
  s3 <- diff(s)[1:(length(s)-3)]
  f3 <- diff(f)[1:(length(s)-3)]
  cotest1 <- lm(s1 ~  dum1[-c(1:3)]:ect[-c(1:3)] + dum2[-c(1:3)]:ect[-c(1:3)]+dum3[-c(1:3)]:ect[-c(1:3)]   
                +  s2 + s3 + f2 + f3 )
  cotestspec <- lm(s1 ~ ect[-c(1:3)] +  s2 + s3 + f2 + f3)
  cotest4 <- lm(s1 ~ dum4[-c(1:3)]:ect[-c(1:3)] +dum5[-c(1:3)]:ect[-c(1:3)]+  s2 + s3 + f2 + f3)
  cotest2 <- lm(f1 ~  dum1[-c(1:3)]:ect[-c(1:3)] + dum2[-c(1:3)]:ect[-c(1:3)]+dum3[-c(1:3)]:ect[-c(1:3)]  
                +  s2 + s3 + f2 + f3)
  e1 <- cotest1$residuals
  e2 <- cotest2$residuals
  v1 <- sd(e1)
  v2 <- sd(e2)
  rho <- cor(e1,e2)
  d11 <- cotest1$coefficients[6]
  names(d11) <- NULL
  d12 <- cotest1$coefficients[7]
  names(d12) <- NULL
  d13 <- cotest1$coefficients[8]
  names(d13) <- NULL
  d21 <- cotest2$coefficients[6]
  names(d21) <- NULL
  d22 <- cotest2$coefficients[7]
  names(d22) <- NULL
  d23 <- cotest2$coefficients[8]
  names(d23) <- NULL
  
  #H11 <- (d11*v1 + d21*rho*v2)^2/(((d11*v1 + d21*rho*v2)^2)+(d21*v2*sqrt(1-rho^2))^2)
  #H12 <- (d21*v2*sqrt(1-rho^2))^2/(((d11*v1 + d21*rho*v2)^2)+(d21*v2*sqrt(1-rho^2))^2)
  H11 <- (-d21*v1 + d11*rho*v2)^2/(((d21*v1 + d11*rho*v2)^2)+(d11*v2*sqrt(1-rho^2))^2)
  H12 <- (d11*v2*sqrt(1-rho^2))^2/(((d21*v1 + d11*rho*v2)^2)+(d11*v2*sqrt(1-rho^2))^2)
  
  
  #H21 <- (d12*v1 + d22*rho*v2)^2/(((d12*v1 + d22*rho*v2)^2)+(d22*v2*sqrt(1-rho^2))^2)
  #H22 <- (d22*v2*sqrt(1-rho^2))^2/(((d12*v1 + d22*rho*v2)^2)+(d22*v2*sqrt(1-rho^2))^2)
  
  H21 <- (-d22*v1 + d12*rho*v2)^2/((((-d22*v1 + d12*rho*v2)^2)+(d12*v2*sqrt(1-rho^2))^2))
  H22 <- (d12*v2*sqrt(1-rho^2))^2/((((-d22*v1 + d12*rho*v2)^2)+(d12*v2*sqrt(1-rho^2))^2))
  
  H31 <- (-d23*v1 + d13*rho*v2)^2/(((d23*v1 + d13*rho*v2)^2)+(d13*v2*sqrt(1-rho^2))^2)
  H32 <- (d13*v2*sqrt(1-rho^2))^2/((((-d23*v1 + d13*rho*v2)^2)+(d13*v2*sqrt(1-rho^2))^2))
  
  
  
  # regression on log(futures) ~~ ect + dum1*ect + dum2*ect +diff(lag(s)) + diff(lag(f))
  #cotest2 <- lm(diff(f) ~ ect[-1] + dum1:ect[-1] + dum2:ect[-1]  
  #              +  diff(lag(s)) + diff(lag(f)) )
  #if (n ==1){return(summary(cotest1))}
  return(c((H11+H12)/2,(H21+H22)/2,(H31+H32)/2))
  if (n ==2){return(summary(cotest2))}
  if (n ==3){return(summary(cotestspec))}
  if (n ==4){return(summary(cotest4))}
}

cotest2(hs300c,if00c,1)
#cotest2(hs300c,if00c,2)
#cotest2(hs300c,if00c,3)
#cotest2(hs300c,if00c,4)
