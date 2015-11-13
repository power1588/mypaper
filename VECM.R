###############################
#I want to build up my first
#paper program in RStudio with Reproducible
#Research (RR) method,i.e gathering data
#analysing data, writing paper,and 
#preseting beamer all together by different
#modules.
###############################
#Analysing Module
#Purpose: Using VECM to analyse Index futures  and
#stock liqudity, consider the shorting ban in China
#examine the relationship between liquidity and 3 types of
#index futures basis(IC/IF/IH) 
#Using Method: VECM
#Begin Date: 20151112
#Coder: YOU WANG
#EMAIL: power15wy@gmail.com
###############################

#first use VECM model====================== 
#ADF test for IF300 index futures and spot
library("tseries")
library("urca")
library("zoo")
spot2013 <- hs3002013$V1
#make them length equal
spot2013 <- na.approx(spot2013)
futures2013 <- fhs3002013$V1


adf.test(spot2013)
adf.test(futures2013)
s300 <- spot2013[1:length(spot2013)]
f300 <- futures2013[1:length(futures2013)]

#combine the tow time series
#test the stationary of residuals
tsif <- cbind(s300,f300)
reg300 <- lm(s300 ~ f300)
r <- reg300$residuals
adf.test(r)
po.coint <- po.test(tsif, demean = TRUE, lshort = TRUE)


#Johansen procedure
yJoTest <- ca.jo(tsif, type = c("trace"), ecdet = c("none"), K=2)
summary(yJoTest)

#vecm
vecm <- ca.jo(tsif)
summary(vecm)
cajorls(vecm, r = 1)








