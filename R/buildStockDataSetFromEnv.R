#' @title Build Stock Data Predictors from Enviroment
#' @param e enviroment where the xts stock data is stored
#' @param stockNameList a list of stockNames
#' @description takes and enviroment with xts stock data, engineers features and merges it into one data frame 
#' @return a data frame of merge stock data
#' @export
#' 
buildStockDataSetFromEnv<-function(e, stockNameList = NULL, verbose = FALSE){
  if(is.null(stockNameList)){stockNameList<-ls(e)}
  if(verbose)print(stockNameList)
  cores<-parallel::detectCores()
  cores<-min(cores, length(stockNameList))
   cl<-makeCluster(cores)
  if(verbose)print(cl)
  registerDoParallel(cl)
    output <- foreach ( i = 1:(length(stockNameList)), 
                        .combine = 'rbind',
                        .packages = c('quantmod', 'stockTools')) %dopar% {
    tempName<-stockNameList[i]
    tempDf<-NULL
    try(tempData<-get(tempName, envir = e))
    if(!is.xts(tempData)){print(paste('missing from envir : ', stockNameList[i]))}
    if(is.xts(tempData)){
        print(paste(stockNameList[i], 'successully found'))
        tempDf<-getXtsFeatures(tempData)
        tempDf
      }
  }
  stopCluster(cl)
  return(output)
}

