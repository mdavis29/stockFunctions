#' @title Get Market Indicators
#' @param symbol.list a vector of or list of symbols to look up
#' @param st Date class object start date of when to look up
#' @param aa Logical auto assign the stock symbol to the enviroment
#' @author Matthew Davis
#' @export
#' @description looks up a list of stock tickers and returns a merged xts of them
#' @return xts object of merged stock OHCL from the list provided
getSymbolList<-function(symbol.list = NULL ,
                        st = as.Date("1999-02-01"), 
                        aa = FALSE){
  
  if(is.null(symbol.list)){symbol.list<-c('^DJI',
                                          'XAX', 'GSPC',
                                          'NDAQ', 
                                          '^N225',
                                          '^GDAXI', 
                                          '^FTSE',
                                          'NQGM',
                                          '^RUI',
                                          'OIL',
                                          'UBG',
                                          'SOYB',
                                          'WEAT',
                                          'CORN',
                                          'COW',
                                          'VHT',
                                          'FDX',
                                          "AA",
                                          "CAT",
                                          'WMT')
                                          }
  
  output.merged<-xts()
    for (i in 1:(length(symbol.list))){
        temp<-xts()
        temp<-try(getSymbols(symbol.list[i], from = st, auto.assign = aa, src = "yahoo"))

          if(is.null(nrow(temp))){
             temp<-try(getSymbols(symbol.list[i], from = st, auto.assign = aa, src = "google"))
                if(is.null(nrow(temp))){print(paste(symbol.list[i], 'failed to fetch Data'))}
                                }
              output.merged<-merge.xts(output.merged, temp)
                                  }

        return(output.merged)
      }

