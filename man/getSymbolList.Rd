% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSymbolList.R
\name{getSymbolList}
\alias{getSymbolList}
\title{Get Market Indicators}
\usage{
getSymbolList(symbol.list = NULL, st = as.Date("1999-02-01"), aa = FALSE)
}
\arguments{
\item{symbol.list}{a vector of or list of symbols to look up}

\item{st}{Date class object start date of when to look up}

\item{aa}{Logical auto assign the stock symbol to the enviroment}
}
\value{
xts object of merged stock OHCL from the list provided
}
\description{
looks up a list of stock tickers and returns a merged xts of them
}
\author{
Matthew Davis
}
