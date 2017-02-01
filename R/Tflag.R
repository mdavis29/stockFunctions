#' @title Trade Signal Flag
#' @param x An xts object containing output from T.ind()
#' @param sig Signal strength that triggers a buy or sell
#' @author Matthew Davis
#' @export
#' @description flags when to buy sell and hold a stock, after inputing trade signal from T.ind()
#' @return xts object of trade signals,buy = 1, sell = -1, hold = 0
T.flag<-function(x, sig = .1){

  output<-ifelse(is.na(x), NA,
                 ifelse(x < -sig, -1,
                        ifelse (x > sig, 1, 0)))
          return(output)
      }
