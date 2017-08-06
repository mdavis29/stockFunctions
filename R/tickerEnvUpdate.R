#' @title Ticker Enviroment Update
#' @param e enviroment to update
#' @param symbolList char vector of symbols to look up
#' @param startDate character start date
#' @param endDate character end date (optional)
#' @description updates and enviroment with ticker symbols
#' @return a list of stock tickers that failed to update 
#' @export
tickerEnvUpdate<-function(e,
                          symbolList = NULL,
                          startDate = '2000-01-03',
                          endDate = NULL){
  failList<-c()
  ## if there is no symbol list, get one from the envir e
  startDate<-as.Date(startDate)
  ## if there is no end Date, use the sys date
  if(is.null(endDate)){endDate<-Sys.Date() - 1}
  ## loop through the symbol list and pull the symbols out of the env
  for (i in 1:(length( symbolList))){
    output<-NULL
    tempName<-symbolList[i]
    oldData<-try(get(tempName, e ), silent = TRUE)
    ## if you can get the symbol from the env, find the start and end dates
    if(is.xts(oldData)){
      minDate<-as.Date(head(index(oldData),1))
      maxDate<-as.Date(tail(index(oldData),1))
      if(minDate <= startDate){
        startDate<-maxDate+1}
      endDate<-max(c(endDate, maxDate))
    }
    newData<-NULL
    ## if the data is including the start Date, try and get it using quantMod
    if(!is.xts(oldData)){oldData<-NULL}
    if(startDate <= endDate){
      print(paste('getting new data for ', tempName))
      newData <- try(getSymbols(tempName,
                                from = as.character(startDate),
                                to = as.character(endDate),
                                auto.assign = FALSE,
                                warnings = FALSE))
      
    }
    if(startDate == endDate){print(paste(paste(tempName, 'up to date'), endDate))}
    if(!is.xts(newData)){
      failList <-append(failList, tempName)
      print(paste(tempName, 'failed to fetch'))
    }
    if(is.xts(newData)){
      colnames(newData)<-unlist(lapply(colnames(newData), function(x)strsplit(x,'[.]')[[1]][2]))
      assign(tempName, newData, envir = e)
    }
    Sys.sleep(4)
  }
  return(failList)
}
