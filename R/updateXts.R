#' @title Update my merged List of stock tickers 
#' @param mydata merged xts object where the columns are stock OHLC Data 
#' @author Matthew Davis
#' @export
#' @description looks up a list of stock tickers and returns a merged xts of them
#' @return merged xts of stock tickers updated to the previous day.
updateXts<-function(mydata){
  getList<-c()
  tickerList <- unlist(unique(lapply(colnames(mydata), 
                             function(x)strsplit(x,'[.]')[[1]][[1]])))
  maxDate <- as.Date(tail(index(mydata),1))
  minDate <- as.Date(head(index(mydata),1))
  for ( i in 1:length(tickerList)){
    testHead<-anyNA(head(mydata[,grepl(tickerList[i], colnames(mydata))],1))
    testTail<-anyNA(tail(mydata[,grepl(tickerList[i], colnames(mydata))],1))
    if(all(c(testHead, testTail, maxDate < Sys.Date()-1 )) == FALSE){
      getList<-append(getList, tickerList[i])
        }
      }
    newData<-getSymbolList(getList,st = minDate, aa = FALSE )
    output.merged<-merge.xts(mydata, newData)
    return(output.merged)
  }