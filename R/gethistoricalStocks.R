#' @title get SP 500 Historical
#' @param sp500Symbols a list of the the SP 500 or any symbol list 
#' @param e enviroment to stor the data
#' @param startDate  text string date to start
#' @param endDate text string date to end
#' @param sleepTime time to sleep in betweenn
#' @description function that attemps to get the entire sp500 and store it in an enviroment
#' @export
getHistoricalStocks<-function(symboList, e, startDate = '2000-01-01', endDate = NULL, sleepTime = 3,cleanColNames = TRUE, verbose = FALSE){
  failList<-c()
  if(is.null(endDate)){endDate<-as.character(Sys.Date())}
  options(error = expression(NULL))
  for ( i in 1:(length(symboList))){
    if(verbose)print(paste('getting', symboList[i]))
    try(getSymbols(symboList[i], warnings=FALSE, env = e, from = startDate, to = endDate, silent=TRUE))
    Sys.sleep(sleepTime )
    tempData<-xts(get(symboList[i], e ))
    if(!is.xts(tempData)){failList<-append(failList, symboList[i])
      if(verbose)print(paste('failed', symboList[i]))
    }
    if(is.xts(tempData) & cleanColNames){
      colnames(tempData)<-stringr::str_replace_all(colnames(tempData),'.*[.]' , '')
      index<-index(tempData)
      assign(symboList[i], tempData, envir = e)
      if(verbose)print(c(symboList[i], as.character(min(index)), as.character(max(index))))
    }
  }
  return(failList)
}
