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
#examine the relationships between liquidity and 3 types of
#index futures basis(IC/IF/IH) 
#Using Method: VAR VECM
#Begin Date: 20151013
#Coder: YOU WANG
#EMAIL: power15wy@gmail.com
###############################
library("vars")
library("fUnitRoots")
#20150416 TO THE 20151127
basif1 <- basif[(length(basif)-36719):length(basif)]
#20150709 SHORTING BAN
basif2 <- basif1[13921:length(basif1)]
#20150527
basif3 <- basif1[1:6960]
#20150826
basif4 <- basif1[6961:22320]
basic2 <- basic[13921:length(basic)]
basic3 <- basic[1:6960]
basic4 <- basic[6961:22320]
basih2 <- basih[13921:length(basih)]
basih3 <- basih[1:6960]
basih4 <- basih[6961:22320]
adftif1 <- adfTest(basif1,lags=1,type = "nc")@test
adftif3 <- adfTest(basif3,lags=1,type = "nc")@test
adftif4 <- adfTest(basif4,lags=1,type = "nc")@test

adftih1 <- adfTest(basih,lags=1,type = "nc")@test
adftih3 <- adfTest(basih3,lags=1,type = "nc")@test
adftih4 <- adfTest(basih4,lags=1,type = "nc")@test

adftic1<- adfTest(basic,lags=1,type = "nc")@test
adftic3<- adfTest(basic3,lags=1,type = "nc")@test
adftic4<- adfTest(basic4,lags=1,type = "nc")@test

adfdata <- rbind(basic,basif1,basih)

#combine the vector

adfdata1 <- timeSeries::cbind2(basic,basif1)
adfdata2 <- timeSeries::cbind2(basic2,basif2)
adfdata3 <- timeSeries::cbind2(basic3,basif3)
adfdata4 <- timeSeries::cbind2(basic4,basif4)

adfdata1 <- timeSeries::cbind2(adfdata1,basih)
adfdata2 <- timeSeries::cbind2(adfdata2,basih2)
adfdata3 <- timeSeries::cbind2(adfdata3,basih3)
adfdata4 <- timeSeries::cbind2(adfdata4,basih4)

infocrit <- VARselect(adfdata1,lag.max = 20,type = "const")
infocrit$selection
infocrit2 <- VARselect(adfdata2,lag.max = 20,type = "const")
infocrit2$selection
infocrit3 <- VARselect(adfdata3,lag.max = 20,type = "const")
infocrit3$selection
infocrit4 <- VARselect(adfdata4,lag.max = 20,type = "const")
infocrit4$selection

varest1 <- VAR(adfdata1,p=7,type = "const")
varest2 <- VAR(adfdata2,p=6,type = "const")
varest3 <- VAR(adfdata3,p=7,type = "const")
varest4 <- VAR(adfdata4,p=5,type = "const")



#Granger causality  y1=IC y2=IH y3=IF
varcausalic <- causality(varest1,cause = 'y1')
varcausalih <-  causality(varest1,cause = 'y2')
varcausalif <- causality(varest1,cause = 'y3')

#IRA of VAR y1=IC y2=IH y3=IF
iraif1 <- irf(varest1, impusle="y3",
             response = c("y1","y2","y3"),n.ahead = 10,
             ortho = TRUE, cumulative = TRUE,
             boot = FALSE, seed = 12345)
plot(iraif1)

#20150709 TO 20151127 SHORTING BAN
iraif2 <- irf(varest2, impusle="y3",
             response = c("y1","y2","y3"),n.ahead = 10,
             ortho = TRUE, cumulative = TRUE,
             boot = FALSE, seed = 12345)
plot(iraif2)

#20150416 TO 20150527  MANIA PERIOD
iraif3 <- irf(varest3, impusle="y3",
             response = c("y1","y2","y3"),n.ahead = 10,
             ortho = TRUE, cumulative = TRUE,
             boot = FALSE, seed = 12345)
plot(iraif3)

#20150527 TO 20150826 THE "BIG FALL"
iraif4 <- irf(varest4, impusle="y3",
             response = c("y1","y2","y3"),n.ahead = 10,
             ortho = TRUE, cumulative = TRUE,
             boot = FALSE, seed = 12345)
plot(iraif4)






#SVAR Model
Amat <- diag(3)
Amat[2,1] <- NA
Amat[1,2] <- NA
Amat[3,1] <- NA
Amat[3,2] <- NA
Amat[1,3] <- NA
Amat[2,3] <- NA
SVARest <- SVAR(varest1,estmethod = "direct",Amat = Amat,hessian=TRUE)
irasvaric <- irf(SVARest,impulse = "y1",response = c("y1","y2","y3"),boot = FALSE)
#ic impact ih
plot(irasvaric)

irasvarih <- irf(SVARest,impulse = "y2",response = c("y1","y2","y3"),boot = FALSE)
#ih impact on if
plot(irasvarih)

irasvarif <- irf(SVARest,impulse = "y3",response = c("y1","y2","y3"),boot = FALSE)
#if impact on ih
plot(irasvarif)

VARselect(adfdata,lag.max = 20,type = "both")

if (1<0)
{
Bmat <- diag(3)
Bmat[2,1] <- NA 
Bmat[3,1] <- NA
Bmat[3,2] <- NA
SVARest2 <- SVAR(varest,estmethod = "scoring",Bmat = Bmat,max.iter = 50000)
irasvaric2ih <- irf(SVARest2,impulse = "y1",response = "y2",boot = FALSE)
#ic impact on ih
plot(irasvaric2ih)
irasvaric2if <- irf(SVARest2,impulse = "y1",response = "y3",boot = FALSE)
#ic impact on if
plot(irasvaric2if)

irasvarif2 <- irf(SVARest2,impulse = "y3",response = "y1",boot = FALSE)
plot(irasvarif2)
irasvarih2 <- irf(SVARest2,impulse = "y2",response = "y3",boot = FALSE)
plot(irasvarih2)
}




