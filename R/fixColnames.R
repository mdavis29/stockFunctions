#' @title fix column names
#' @param e and enviroment where stock xts data is stored
#' @param stockNameList list of stocks to correct names for, ls(e is used if null)
#' @description changes xts object column names from ex: GE.Open to Open
#' @export
#' 
fixColnames<-function(e,stockNameList =NULL ){
  if(is.null(stockNameList))stockNameList<-ls(e)
  for ( i  in 1:(length(stockNameList) )){
    x<-get(stockNameList[i], envir = e)
    colnames(x)<-unlist(lapply(colnames(x), function(x)strsplit(x,'[.]')[[1]][2]))
    assign(stockNameList[i],x, envir = e )
  }
  return(print('completed'))
}