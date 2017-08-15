## ----setup, comment = '', message=FALSE----------------------------------
library(stockTools)
if(!exists('E'))E<-new.env()
getHistoricalStocks(c('GE',  "AA" ), e = E , startDate = '2000-01-01', endDate = '2017-08-01')
eapply(E, function(x)tail(x,1))

## ------------------------------------------------------------------------
tickerEnvUpdate(E)
eapply(E, function(x)tail(x,1))

## ------------------------------------------------------------------------
## working here
ls(E)
head(E$AA)
x<-buildStockDataSetFromEnv(E)
dim(x)
tail(x,2)

