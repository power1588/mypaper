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

####loading fine data
hs300 = read.csv('E:\\Rtrial\\winddata\\hs300.csv',header = FALSE)
#hs3002013 = read.csv('E:\\Rtrial\\winddata\\hs3002013.csv',header = FALSE)
#hs3002010 = read.csv('E:\\Rtrial\\winddata\\hs3002010.csv',header = FALSE)
zz500 = read.csv('E:\\Rtrial\\winddata\\zz500.csv',header = FALSE)
sz50 = read.csv('E:\\Rtrial\\winddata\\sz50.csv',header = FALSE)
if00 = read.csv('E:\\Rtrial\\winddata\\ifhs300.csv',header = FALSE)
#fhs3002013 = read.csv('E:\\Rtrial\\winddata\\fhs3002013.csv',header = FALSE)
#fhs3002010 = read.csv('E:\\Rtrial\\winddata\\fhs3002010.csv',header = FALSE)
ic00 = read.csv('E:\\Rtrial\\winddata\\ifzz500.csv',header = FALSE)
ih00 = read.csv('E:\\Rtrial\\winddata\\ifsz50.csv',header = FALSE)

#obtain the basis of if00 ic00 ih00
#index structural is close volume change
#index futures structural is close volume change position
basif <- na.approx(if00$V1) - na.approx(hs300$V1)
#fhs3002015<- na.approx(if00$V1)
#lfutures2010 <- log(fhs3002010$V1)
#lspot2010 <- log(na.approx(hs3002010$V1))
#lfutures2015 <- log(fhs3002015)
#hs3002015 <- na.approx(hs300$V1)
#lspot2015 <- log(hs3002015)
#basif2013 <- na.approx(fhs3002013$V1) -  na.approx(hs3002013$V1)
basih <- na.approx(ih00$V1) - na.approx(sz50$V1)
basic <- na.approx(ic00$V1) - na.approx(zz500$V1)
#let NA value in basic equal 0



####clean the data
#delete 5061  15665(15664) 21690(21688) 26028(26025)

if (1<0){
library(chron)
dtimes <- t$X000300.SH
dtparts <- as.data.frame(as.character.factor(dtimes))
t <- as.character(dtparts[,1])
tt <- strptime(t,"%Y-%m-%d %H:%M")
t1 <- substr(t,1,10)
t2 <- substr(t,12,16)
thetime <- chron(dates. =dp[,1],times. =dp[,2], format = c('y-m-d','h:m:s') )
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

}

