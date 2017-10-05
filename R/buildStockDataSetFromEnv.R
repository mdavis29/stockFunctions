#' @title Build Stock Data Predictors from Enviroment
#' @param e enviroment where the xts stock data is stored
#' @param stockNameList a list of stockNames
#' @param verbose print debugging output
#' @description takes and enviroment with xts stock data, engineers features and merges it into one data frame 
#' @return a data frame of merge stock data
#' @export
#' 
buildStockDataSetFromEnv<-function(e, stockNameList = NULL, verbose = FALSE){
   haveList<-ls(e)
   buildList<-union(haveList,intersect(haveList, stockNameList))
   if(verbose)print(buildList)
   if(verbose)print(paste('have everything ?',all(buildList %in% ls(e)))) 
   cores<-parallel::detectCores()
   cores<-min(cores, length(haveList))
   cl<-makeCluster(cores)
   if(verbose)print(cl)
   registerDoParallel(cl)
   output <- foreach ( i = 1:(length(buildList)), 
                        .combine = 'rbind',
                        .packages = c('quantmod', 'stockTools')) %dopar% {
    tempName<-buildList[i]
    tempData<-get(tempName, envir = e)
    tempDf<-try(getXtsFeatures(tempData, stockName = tempName))
    if(class(tempDf)[1] == 'data.frame'){
      tempDf<-data.frame(tempDf, stockName = as.factor(rep(tempName, nrow(tempDf))))
      }
    if(class(tempDf) == "try-error" ){
      tempDf<-data.frame()
      }  
    data.frame(tempDf)
  }
  stopCluster(cl)
  return(output)
}

