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
adftif <- adfTest(basif,lags=1,type = "nc")@test
adftih <- adfTest(basih,lags=1,type = "nc")@test
adftic <- adfTest(basic,lags=1,type = "nc")@test
adfdata <- rbind(basic,basif,basih)

#combine the vector
adfdata <- timeSeries::cbind2(basic,basif)
adfdata <- timeSeries::cbind2(adfdata,basih)
infocrit <- VARselect(adfdata,lag.max = 20,type = "const")
infocrit$selection

varest <- VAR(adfdata,p=8,type = "const")

if (1<0){
varest
varest2 <- VAR(adfdata,type = "const",lag.max = 4,ic="SC")
varest2
}

#Granger causality  y1=IC y2=IH y3=IF
varcausalic <- causality(varest,cause = 'y1')
varcausalih <- causality(varest,cause = 'y2')
varcausalif <- causality(varest,cause = 'y3')

#IRA of VAR
iraic <- irf(varest, impusle="y1",
             response = "y3",n.ahead = 10,
             ortho = TRUE, cumulative = TRUE,
             boot = FALSE, seed = 12345)
iraih <- irf(varest, impusle="y1",
             response = "y2",n.ahead = 10,
             ortho = TRUE, cumulative = TRUE,
             boot = FALSE, seed = 12345)
Amat <- diag(3)
Amat[2,1] <- NA
Amat[1,2] <- NA
Amat[3,1] <- NA
Amat[3,2] <- NA
Amat[1,3] <- NA
Amat[2,3] <- NA
SVARest <- SVAR(varest,estmethod = "direct",Amat = Amat,hessian=TRUE)
irasvaricih <- irf(SVARest,impulse = "y1",response = "y2",boot = FALSE)
#ic impact ih
plot(irasvaricih)
irasvaricif <- irf(SVARest,impulse = "y1",response = "y3",boot = FALSE)
#ic impact if
plot(irasvaricif)

irasvarific <- irf(SVARest,impulse = "y3",response = "y1",boot = FALSE)
#if impact on ic
plot(irasvarific)
irasvarifih <- irf(SVARest,impulse = "y3",response = "y2",boot = FALSE)
#if impact on ih
plot(irasvarifih)

irasvarihif <- irf(SVARest,impulse = "y2",response = "y3",boot = FALSE)
#ih impact on if
plot(irasvarihif)
irasvarihic <- irf(SVARest,impulse = "y2",response = "y1",boot = FALSE)
#ih impact on ic
plot(irasvarihic)

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




