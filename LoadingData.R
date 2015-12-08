#loading data
library("zoo")
##loading hs300 index data 20100416~20151127
hs300 <- read.csv('E:\\Rtrial\\winddata\\hs300.csv',header = FALSE)
zz500 <- read.csv('E:\\Rtrial\\winddata\\zz500.csv',header = FALSE)
sz50 <- read.csv('E:\\Rtrial\\winddata\\sz50.csv',header = FALSE)

##loading index futures data
if00 <- read.csv('E:\\Rtrial\\winddata\\if00.csv',header = FALSE)
ic00 <- read.csv('E:\\Rtrial\\winddata\\ic00.csv',header = FALSE)
ih00 <- read.csv('E:\\Rtrial\\winddata\\ih00.csv',header = FALSE)

if01 <- read.csv('E:\\Rtrial\\winddata\\if01.csv',header = FALSE)
ic01 <- read.csv('E:\\Rtrial\\winddata\\ic01.csv',header = FALSE)
ih01 <- read.csv('E:\\Rtrial\\winddata\\ih01.csv',header = FALSE)

if02 <- read.csv('E:\\Rtrial\\winddata\\if02.csv',header = FALSE)
ic02 <- read.csv('E:\\Rtrial\\winddata\\ic02.csv',header = FALSE)
ih02 <- read.csv('E:\\Rtrial\\winddata\\ih02.csv',header = FALSE)

if03 <- read.csv('E:\\Rtrial\\winddata\\if03.csv',header = FALSE)
ic03 <- read.csv('E:\\Rtrial\\winddata\\ic03.csv',header = FALSE)
ih03 <- read.csv('E:\\Rtrial\\winddata\\ih03.csv',header = FALSE)


#test the data is.na()
hs300t <- summary(is.na(hs300))
if00t <- summary(is.na(if00))
if01t <- summary(is.na(if01))
if02t <- summary(is.na(if02))
if03t <- summary(is.na(if03))

#use zoo package na.approx() function to generate time series without NA
#take the logritham of Index Futures
if00c<- log(na.approx(if00$V1))
ic00c<- log(na.approx(ic00$V1))
ih00c<- log(na.approx(ih00$V1))

if01c<- log(na.approx(if01$V1))
ic01c<- log(na.approx(ic01$V1))
ih01c<- log(na.approx(ih01$V1))

if02c<- log(na.approx(if02$V1))
ic02c<- log(na.approx(ic02$V1))
ih02c<- log(na.approx(ih02$V1))

if03c<- log(na.approx(if03$V1))
ic03c<- log(na.approx(ic03$V1))
ih03c<- log(na.approx(ih03$V1))

#take logritham of index
hs300c <- log(na.approx(hs300$V1))
zz500c <- log(na.approx(zz500$V1))
sz50c <- log(na.approx(sz50$V1))

#get the basis 
basif <- if00c - hs300c
basic <- ic00c - zz500c
basih <- ih00c - sz50c




