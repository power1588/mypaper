###############################
#I want to build up my first
#paper program in RStudio with Reproducible
#Research (RR) method,i.e gathering data
#analysing data, writing paper,and 
#preseting beamer all together by different
#modules.
###############################
#Analysing Module
#Purpose: Using VAR to analyse Index futures basis and
#stock liqudity, consider the shorting ban in China
#examine the relationship between liquidity and 3 types of
#index futures basis(IC/IF/IH) 
#Using Method: VAR VECM
#Begin Date: 20151013
#Coder: YOU WANG
#EMAIL: power15wy@gmail.com
###############################

####loading data
hs300 = read.csv('E:\\Rtrial\\winddata\\hs300.csv')
zz500 = read.csv('E:\\Rtrial\\winddata\\zz500.csv')
sz50 = read.csv('E:\\Rtrial\\winddata\\sz50.csv')
if00 = read.csv('E:\\Rtrial\\winddata\\if00.csv')
ic00 = read.csv('E:\\Rtrial\\winddata\\ic00.csv')
ih00 = read.csv('E:\\Rtrial\\winddata\\ih00.csv')

####clean the data
if (1<0){
library(chron)
dtimes <- t$X000300.SH
dtparts <- as.data.frame(as.character.factor(dtimes))
t <- as.character(dtparts[,1])
tt <- strptime(t,"%Y-%m-%d %H:%M")
t1 <- substr(t,1,10)
t2 <- substr(t,12,16)
thetime <- chron(dates. =dp[,1],times. =dp[,2], format = c('y-m-d','h:m:s') )
}

#hs300 index close price
hs300clo <- hs300[3:45015,3]
hs300clo <- as.numeric(as.character(hs300clo))
hs300clo <- hs300clo[!is.na(hs300clo)]
#sz50 index close price
sz50clo <- sz50[3:45015,2]
sz50clo <- as.numeric(as.character(sz50clo))
sz50clo <- sz50clo[!is.na(sz50clo)]
#zz500 index close price
zz500clo <- zz500[3:45015,2]
zz500clo <- as.numeric(as.character(zz500clo ))
zz500clo <- zz500clo [!is.na(zz500clo )]
