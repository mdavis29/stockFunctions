##model build section
library(caret)
library(vtreat)
library(stockTools)
data("stockData")
rd<-as.data.frame(stockData)
##uses v treat to handel NAs 
treat<-designTreatmentsZ(head(rd,3000), colnames(rd))
rd.t<-prepare(treat,rd)
##preProcess for the data, removing columns with little variance and preforms PCA
p<-preProcess(rd.t, c('pca', 'zv', 'nzv'))
rd.p<-predict(p, rd.t)
rownames(rd.p)<-as.Date(rownames(rd))

##creats the trad signal indicator
y<-getSymbolList('TRV')
y.t<-as.factor(T.flag(T.ind(y)))
levels(y.t)<-c('sell', 'hold', 'buy')
names(y.t)<-as.Date(index(y))
##find the intersection of the date on the target and the raw data. 
keep<-intersect(names(y.t), rownames(rd.p))
##split the training and test sets 
ntrain<-3000
xTrain<-rd.p[rownames(rd.p) %in% head(keep, ntrain),]
xTest<-rd.p[rownames(rd.p) %in% tail(keep,length(keep)- ntrain),]
yTrain<-y.t[names(y.t) %in% head(keep, ntrain)]
yTest<-y.t[names(y.t) %in% head(keep,length(keep) -  ntrain)]

require(parallel)
require(doParallel)
cl<-makeCluster(4)
registerDoParallel(cl)

gbmGrid<-expand.grid(n.trees = 1:4*200, 
                     shrinkage = c(0.1, 0.2),
                     interaction.depth = c(5,9),
                     n.minobsinnode = c(15, 25))

ctr<-trainControl(p = .8,
                  search = "random", 
                  classProbs = TRUE,
                  returnData = FALSE)

fit<-train(x = xTrain, 
           y = as.matrix(yTrain),
           method = 'rpart',
           tuneLength = 20, 
           trControl = ctr)
           
           
    
