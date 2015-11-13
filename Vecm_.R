###############################
#I want to build up my first
#paper program in RStudio with Reproducible
#Research (RR) method,i.e gathering data
#analysing data, writing paper,and 
#preseting beamer all together by different
#modules.
###############################
#Main Module
#Purpose: Using VECM to analyse Index futures  and
#stock liqudity, consider the shorting ban in China
#examine the relationship between liquidity and 3 types of
#index futures basis(IC/IF/IH) 
#Using Method: VECM
#Begin Date: 20151112
#Coder: YOU WANG
#EMAIL: power15wy@gmail.com
###############################
Vecm_ <- function(x,y)
{
  #x is spot, y is futures
  library("tseries")
  library("urca")
  library("zoo")
  s <- x
  f <- y
  # exclude the NA
  s <- na.approx(s)
  f <- na.approx(f)
  
  ts <- cbind(s,f)
  vecm <- ca.jo(ts)
  summary(vecm)
  cajorls(vecm, r = 1)
}












