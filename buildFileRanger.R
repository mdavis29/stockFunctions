library(stockTools)
library(Matrix)
library(xgboost)
library(caret)

data("sp500Symbols")
## build new data
e<-new.env()
failList <- getHistoricalStocks(sp500Symbols, e)
stockDataEnv<-e
save(stockDataEnv, file = 'stockDataEnv.rda')

##if the data is already saved as an enviroment. l
load('stockDataEnv.rda')
rd<-buildStockDataSetFromEnv(stockDataEnv)
## get Varible Names
classCol<- "tradeFlag" 
predVars<-colnames(rd)[!colnames(rd) %in% c( "time","tradeInd","stockName",classCol)]

### create a cluster model
set.seed(2012)
findClusters(na.omit(rd[, predVars]))
clusterFit<-kmeans(na.omit(rd[,predVars]), 10, iter.max = 1000)
save(clusterFit, file = 'clusterFit.rda')
###
load('clusterFit.rda')



##split data into training and test sets
trainIndex<-rd$time < as.Date('2016-01-08')



trainData.raw<- na.omit(rd[trainIndex, ])
trainData.raw$tradeInd<-NULL
trainData.raw$time<-as.numeric(trainData.raw$time)
evalData.raw<- na.omit(rd[!trainIndex, ])
evalData.raw$time<-as.numeric(evalData.raw$time)
evalData.raw$tradeInd<-NULL


##fit an xgboost model 

library(xgboost)

set.seed(20123)              
## create cluster model
library(modelToolKit)
#fit.cluster<-kmeans(train, 12, iter.max = 1000)
fit.cluster<-clusterFit

### prepare training data
train<-as.matrix(trainData.raw[, predVars])
train.kmd <- predictKmeans(fit.cluster, train)
train.kmd$cluster<-as.numeric(as.character(train.kmd$cluster))
trainLabels<-as.numeric(trainData.raw[, classCol])-1
train.ready<-as.matrix(cbind(train, train.kmd))


## prepare test data    
test<-as.matrix(evalData.raw[, predVars])
test.kmd <- predictKmeans(fit.cluster, test)
test.kmd$cluster <-as.numeric(as.character(test.kmd$cluster))
testLabels<-as.numeric(evalData.raw[, classCol])-1
test.ready<-as.matrix(cbind(test, test.kmd))

w<-ifelse(trainLabels ==2,1,2 )
## create model
library(xgboost)
fit.xgb<-xgboost(data= train.ready, 
                 label = trainLabels,
                 nrounds = 1000,
                 objective =  'multi:softmax',
                 save_name = 'stocl.xgbModel',
                 weight = w,
                 params = list(subsample = .60,
                               eta = .1,
                               max_depth = 6,
                               num_class = 5,
                               colsample_bytree  = .9))
                               

preds<-as.factor(predict(fit.xgb, test.ready))
obs<-testLabels
  
caret::confusionMatrix(preds, obs )  
  
  
  xgb.ggplot.importance(xgb.importance(model = fit.xgb))
  
  
  