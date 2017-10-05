#' @title Trade Simulator
#' @param preds an txs object of buy sell hold probablities
#' @param e an enviroment with xts OHLC data 
#' @param params a list of parameters
#' 
tradeSimulator<-function(model, e, params, verbose = TRUE){
  params.default <- list(seedMoney = 10000, 
                         maxTradeAmount = 2000,
                         tradeCost = 10,
                         stopLossPercent = .5
                         )
  stockList<-ls(e)

  if(verbose)print(minDate)

  if(verbose(print(maxdDate)))
  while(minDate < maxDate){
    minDate <- minDate + 1 
    get()
    
    
  }  
}