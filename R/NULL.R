#' @title  Stock Functions
#' @description Basic Tools for predicting the stock makes:
#'  \itemize{
#'    \item T.ind takes and xts OHCL object and gets a trade signal from it, based on future preformance
#'    \item T.flag takes a T.ind signal and creates a buy, sell or hold flag
#'    \item buildPredictors takes and xts OHLCV object and builds a set of predictors based on TTR package
#'    \item getSymbol.list looks up a list of symbols and returns a merged xts of them
#'   }
#' @docType package
#' @name fpTools
NULL

