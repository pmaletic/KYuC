#' winsorization
#'
#' Its purposes is to eliminate outliers in a following way. Values lower or higher than a certain percentile
#' will be replaced by the percentile. The function is taken from package DescTools.
#' @param x a input vector
#' @param perc_d lower percentile, which will be used for winsoriation
#' @param perc_u lower percentile, which will be used for winsoriation 
#' @return Winsorized  vector \code{x}
#' @examples
#' example_vector=c(-1000,1,2,3,4,5,6,7,8,9,1000)
#' winsorization(example_vector,0.05,0.95)
#' @export
#' @import DescTools
#' 
winsorization<-function(x,perc_d,perc_u){
y<-Winsorize(x, probs = c(perc_d,perc_u))
return(y)
}
