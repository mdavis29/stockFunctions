library(stockTools)
library(Matrix)
library(mxnet)
data("sp500Symbols")
## build new data
e<-new.env()
failList <- getHistoricalStocks(sp500Symbols, e)
stockDataEnv<-e
save(stockDataEnv, file = 'stockDataEnv.rda')

##if the data is already saved as an enviroment. l
load('stockDataEnv.rda')
x<-buildStockDataSetFromEnv(stockDataEnv)
x$tradeInd<-NULL
## relevel for mxnet
x$tradeFlag<-as.factor(x$tradeFlag)
levels(x$tradeFlag)<-c(0,1,2)
x$tradeFlag<-as.numeric(as.character(x$tradeFlag))

### pre Proc the training data
x.clean<-na.omit(x)

##dummy encode the data stes
dummys<-model.matrix(~stockName, data = x.clean) 
class<-"tradeFlag" 
featureCols<-colnames(x.clean)[!colnames(x.clean)%in% c(class, 'stockName', 'tradeInd')]

## create a index to split the training and evals sets up based on data
trainIndex<-as.Date(rownames(x.clean)) < as.Date('2016-01-01')

xTrain.raw<-x.clean[trainIndex, featureCols]
findOutliersByQuantile<-function(mydata, r = 1.8, verbose = FALSE){
  nc<-ncol(mydata)
  nr<-nrow(mydata)
  cores<-parallel::detectCores()
  cl<-makeCluster(cores)
  registerDoParallel(cl)
  outputMatrix<-foreach( i  =  1:nc, .combine = 'cbind') %dopar%{
    tempOut<-rep(FALSE, nr)
    tempData<-mydata[,i]
    if(length(unique(tempData))>2){
      quant<-quantile(tempData, na.rm = TRUE) 
      iqr<-IQR(tempData, na.rm = TRUE)
      lows<-ifelse(tempData < quant[3] - iqr*r, TRUE, FALSE)
      highs<-ifelse(tempData > quant[3] + iqr*r, TRUE, FALSE)
      tempOut<-apply(cbind(lows,highs), c(1), any)
      }
  }
  stopCluster(cl)
  if(verbose)print(summary(outputMatrix))
  output<-apply(outputMatrix,  c(1), any)
  return(output)
}
## get the index of any outliers
outlierIndex<-findOutliersByQuantile(xTrain.raw, 1.8, verbose = TRUE)  

xTrain.clean<-xTrain.raw[!outlierIndex,]
yTrain<-x.clean[trainIndex,class][!outlierIndex]

## create Pre Process<
preProc<-preProcess(xTrain.clean, methods = c('range', "medianImpute"))

xTrain.ready<-cbind(predict(preProc, xTrain.clean[, featureCols] ),dummys[trainIndex, ][!outlierIndex,]) 
xTest.ready<-cbind(predict(preProc,x.clean[!trainIndex, featureCols] ), dummys[!trainIndex,])
yTest<-x.clean[!trainIndex,class]

## create the data iterator
trainIter<-mx.io.arrayiter(t(xTrain.ready),(yTrain), batch.size =1250, shuffle = TRUE)
evalIter<-mx.io.arrayiter(t(xTest.ready),yTest, batch.size = 1250, shuffle = TRUE)

## define the network
data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=200)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=200)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=3)
softmax <- mx.symbol.SoftmaxOutput(fc3, name="sm")
## run on mulit gpus
devices <- list(mx.gpu(0), mx.gpu(1), mx.gpu(2))
mx.set.seed(0)

st<-Sys.time()
fit <- mx.model.FeedForward.create(softmax,
                                   X = trainIter, 
                                   eval.data = evalIter,
                                   ctx=devices, 
                                   num.round=20,
                                   learning.rate=0.03,
                                   wd=0.001,
                                   optimizer = 'adagrad',
                                   clip_gradient=1,
                                   eval.metric=mx.metric.accuracy, 
                                   initializer=mx.init.Xavier(rnd_type = "gaussian", factor_type = "avg", magnitude = 2),
                                   epoch.end.callback = mx.callback.log.train.metric(1))
cpuTime<-Sys.time()-st



preds<-predict(fit, xTest.ready)
preds.c<-as.factor(apply(preds, c(1), which.max))
levels(preds.c)<-c('sell', 'hold', 'buy')
obs.c<-as.factor(yTest)
levels(obs.c)<-c('sell', 'hold', 'buy')
caret::confusionMatrix(pred.c, obs.c)


