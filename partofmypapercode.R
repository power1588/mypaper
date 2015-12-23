<<echo=FALSE, fig.width=4,  fig.height=2.5, message=FALSE, tidy=FALSE, warning=FALSE, fig.align="center">>=
  #use the same time interval of r2
  nod0 <- 128882
  nod1 <- 151681
  #the hs300c is the lagrithem of close price of hs300 index
  r1 <- hs300c[(nod0+1):nod1] - hs300c[nod0:(nod1-1)]
  acf(r1)
  @
    The above figure shows that the situation before 20150709.
  <<echo=FALSE>>=
    
    #A variance ratio test with holding 
    #period value chosen by a data dependent procedure
    av1 <- Auto.VR(r1)
  bt1 <- Box.test(r1,lag = 10, type = 'Ljung')
  @
    
    <<echo=FALSE>>=
    
    #A variance ratio test with holding 
    #period value chosen by a data dependent procedure
    av2 <- Auto.VR(r2)
  bt2 <- Box.test(r2,lag = 10, type = 'Ljung')
  @
    
    
    <<echo=FALSE, fig.width=4,  fig.height=2.5, message=FALSE, tidy=FALSE, warning=FALSE, fig.align="center">>=
    #from 20121128 to 20151125 the number of minutes
    nod2 <- 174480 
  #the hs300c is the lagrithem of close price of hs300 index
  r2 <- hs300c[(nod1+1):nod2] - hs300c[nod1:(nod2-1)]
  acf(r2)
  @
    However, as we can see that the lags level of the situation after the shorting ban still obvious even after 10 lags level which representS that the serial autocorrelation still exists over 10 minute on average. According to the classical theory of random walk, this would mean that the deterioration of spot market  efficiency.